#' Choose M-spline knots for a flexible-baseline survival model
#'
#' Place boundary and internal knots for the M-spline baseline hazard used by
#' the flexible survival models (`distribution = "mspline"` or `"pexp"`). Knots
#' are chosen from the pooled event/censoring times of the index IPD and the
#' reconstructed comparator pseudo-IPD.
#'
#' @param data An `mlumr_data` object (survival family) from [combine_data()].
#' @param n_knots Number of internal knots (default 7; capped at 50). Use a
#'   smaller value when events are scarce; the recommended rule of thumb is to
#'   keep the number of spline coefficients (`n_knots + degree + 1`) below half
#'   the number of events.
#' @param type Internal-knot placement: `"quantile"` (default, at quantiles of
#'   the pooled event times) or `"equal"` (evenly spaced between the boundaries).
#'
#' @return A list with `internal` (internal knot locations), `boundary` (lower
#'   and upper boundary knots) and `n_knots` (the realized number of internal
#'   knots after dropping any that coincide with the boundaries).
#'
#' @details
#' The lower boundary knot is fixed at 0 (not the minimum delayed-entry time),
#' so the cumulative hazard is anchored at `H(0) = 0` and stays continuous with
#' the backward constant-hazard extrapolation; delayed-entry times (> 0) are
#' evaluated on the basis. The upper boundary knot is the maximum observed time
#' across both data sources. The M-spline basis is normalized so that the
#' baseline cumulative hazard equals 1 at the upper boundary; the hazard scale
#' is carried by the model intercepts.
#'
#' @examples
#' \dontrun{
#' # Build a survival network, then choose M-spline baseline knots:
#' dat <- combine_data(index_ipd, comparator_agd)
#' knots <- make_knots(dat, n_knots = 5)
#' knots$boundary  # lower (0) and upper boundary knots
#' knots$internal  # internal knot locations
#' }
#' @seealso [mlumr()] with `distribution = "mspline"`.
#' @export
make_knots <- function(data, n_knots = 7, type = c("quantile", "equal")) {
  type <- match.arg(type)
  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data()", call. = FALSE)
  }
  if ((data$family %||% "") != "survival") {
    stop("make_knots() requires a survival mlumr_data object", call. = FALSE)
  }
  if (!is.numeric(n_knots) || length(n_knots) != 1L || n_knots < 0 ||
        n_knots != floor(n_knots) || n_knots > 50) {
    stop("`n_knots` must be an integer in [0, 50]", call. = FALSE)
  }

  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  all_times <- c(ipd$.time, pseudo$.time)
  event_times <- c(ipd$.time[ipd$.status == 1], pseudo$.time[pseudo$.status == 1])
  .knots_from_times(all_times, event_times, n_knots, type)
}


#' Knot placement from a single set of survival times
#'
#' The computational core of [make_knots()], factored out so the same rule can
#' be applied either to the pooled times (one shared baseline) or to each
#' study's own times (`aux_by = ".study"`, one baseline per study). Per-study
#' boundary knots are what multinma's default `type = "quantile"` does, and they
#' are what keeps a stratified baseline identified: a basis function with no
#' support over a study's observed period leaves that study's spline scale free
#' to trade off against its intercept.
#'
#' @param all_times All observed times for the stratum (events and censorings).
#' @param event_times Event times only; falls back to `all_times` if empty.
#' @param n_knots Number of internal knots.
#' @param type `"quantile"` (event-time quantiles) or `"equal"` (equally spaced).
#' @return A list with `internal`, `boundary`, and `n_knots`.
#' @keywords internal
.knots_from_times <- function(all_times, event_times, n_knots,
                              type = c("quantile", "equal")) {
  type <- match.arg(type)
  if (length(event_times) == 0L) event_times <- all_times

  # Anchor the baseline at t = 0, not at the minimum entry time. With delayed
  # entry the smallest entry can be > 0; placing the lower boundary knot there
  # makes the I-spline cumulative hazard 0 at the lower knot while the backward
  # constant-hazard extrapolation gives H(lower-) > 0, a discontinuity that makes
  # S(t) non-monotonic and corrupts the RMST integral, and zeros the Stan
  # delayed-entry correction (the I-spline at delay_time = lower is 0). Anchoring
  # at 0 keeps H(0) = 0 and continuous, and evaluates delayed-entry times (> 0)
  # correctly on the basis.
  lower <- 0
  upper <- max(all_times, na.rm = TRUE)
  if (!is.finite(upper) || upper <= lower) {
    stop("Could not determine valid boundary knots from the survival times",
         call. = FALSE)
  }

  internal <- numeric(0)
  if (n_knots >= 1L) {
    probs <- seq_len(n_knots) / (n_knots + 1)
    internal <- if (type == "quantile") {
      unname(stats::quantile(event_times, probs = probs, names = FALSE))
    } else {
      lower + probs * (upper - lower)
    }
  }
  internal <- sort(unique(internal[internal > lower & internal < upper]))

  list(internal = internal, boundary = c(lower, upper), n_knots = length(internal))
}


#' Build an M-spline basis specification
#'
#' @param knots A list from [make_knots()] (`internal`, `boundary`).
#' @param degree Spline degree: 3 (cubic M-spline) or 0 (piecewise exponential).
#' @return A basis spec list with `internal`, `boundary`, `degree`, `n_scoef`.
#' @keywords internal
.build_mspline_basis <- function(knots, degree) {
  if (!requireNamespace("splines2", quietly = TRUE)) {
    stop("Package 'splines2' is required for flexible-baseline survival models.",
         call. = FALSE)
  }
  spec <- list(
    internal = knots$internal,
    boundary = knots$boundary,
    degree = as.integer(degree)
  )
  probe <- splines2::mSpline(
    mean(knots$boundary),
    knots = spec$internal, degree = spec$degree,
    Boundary.knots = spec$boundary, intercept = TRUE
  )
  spec$n_scoef <- ncol(probe)
  spec
}


#' Evaluate an M-spline (or integrated I-spline) basis at given times
#'
#' Values outside the boundary knots are extrapolated with constant boundary
#' hazards. For the integrated basis, this adds a linear tail beyond the
#' boundary so cumulative hazards continue increasing. Times at or before zero
#' contribute no cumulative hazard.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param times Numeric vector of evaluation times.
#' @param integral If `TRUE`, return the integrated (I-spline) basis; otherwise
#'   the M-spline basis.
#' @return A numeric matrix with `length(times)` rows and `spec$n_scoef` columns.
#' @keywords internal
.eval_basis <- function(spec, times, integral = FALSE) {
  if (length(times) == 0L) {
    return(matrix(numeric(0), nrow = 0L, ncol = spec$n_scoef))
  }
  lower <- spec$boundary[1]
  upper <- spec$boundary[2]
  times_clamped <- pmin(pmax(times, lower), upper)
  basis <- splines2::mSpline(
    times_clamped,
    knots = spec$internal, degree = spec$degree,
    Boundary.knots = spec$boundary, intercept = TRUE, integral = integral
  )
  basis <- matrix(as.numeric(basis), nrow = length(times), ncol = spec$n_scoef)
  if (integral) {
    before_zero <- times <= 0
    before_lower <- times > 0 & times < lower
    after_upper <- times > upper

    if (any(before_lower)) {
      lower_haz <- splines2::mSpline(
        rep(lower, sum(before_lower)),
        knots = spec$internal, degree = spec$degree,
        Boundary.knots = spec$boundary, intercept = TRUE, integral = FALSE
      )
      lower_haz <- matrix(as.numeric(lower_haz), nrow = sum(before_lower),
                          ncol = spec$n_scoef)
      basis[before_lower, ] <- times[before_lower] * lower_haz
    }

    if (any(after_upper)) {
      upper_haz <- splines2::mSpline(
        rep(upper, sum(after_upper)),
        knots = spec$internal, degree = spec$degree,
        Boundary.knots = spec$boundary, intercept = TRUE, integral = FALSE
      )
      upper_haz <- matrix(as.numeric(upper_haz), nrow = sum(after_upper),
                          ncol = spec$n_scoef)
      basis[after_upper, ] <- basis[after_upper, ] +
        (times[after_upper] - upper) * upper_haz
    }

    if (any(before_zero)) {
      basis[before_zero, ] <- 0
    }
  }
  basis
}


#' RW1 anchor for the log-ratio spline coefficients (centers on a flat baseline)
#'
#' Returns the inverse-softmax (length `n_scoef - 1`) of the M-spline coefficient
#' vector that produces a **constant baseline hazard**, so the RW1 smoothing
#' prior is centered on a flat baseline even when the knots are unevenly spaced.
#' Combined in Stan as `softmax(append_row(0, lscoef_prior_mean))`, this recovers
#' the constant-hazard simplex exactly. Uses the knot-spacing construction of
#' Jackson (arXiv:2306.03957); reimplemented from `multinma`
#' (GPL-3, `multinma:::mspline_constant_hazard`).
#' @keywords internal
.mspline_constant_hazard <- function(spec) {
  ord <- spec$degree + 1L
  n <- spec$n_scoef
  knots <- c(rep(spec$boundary[1], ord), spec$internal, rep(spec$boundary[2], ord))
  coefs <- (knots[(1:n) + ord] - knots[1:n]) / (ord * diff(spec$boundary))
  log(coefs[-1]) - log(coefs[1])           # inverse softmax (reference = coef 1)
}


#' Knot-spacing-aware RW1 step weights for the spline coefficients
#'
#' Returns `sqrt` of the normalized knot gaps (length `n_scoef - 1`) so the RW1
#' increments are scaled by interval width under unevenly spaced knots.
#' Reimplemented from `multinma` (GPL-3, `multinma:::rw1_prior_weights`).
#' @keywords internal
.rw1_prior_weights <- function(spec) {
  ord <- spec$degree + 1L
  n <- spec$n_scoef
  knots <- c(rep(spec$boundary[1], ord), spec$internal, rep(spec$boundary[2], ord))
  wts <- if (ord == 1L) {
    (knots[2:n] - knots[1:(n - 1)]) / (knots[n] - spec$boundary[1])
  } else {
    (knots[(ord + 1):(n + ord - 1)] - knots[2:n]) / ((ord - 1) * diff(spec$boundary))
  }
  sqrt(wts)
}


#' Softmax of a vector (numerically stable); used to map log-ratios to a simplex
#' @keywords internal
.softmax <- function(x) {
  z <- x - max(x)
  exp(z) / sum(exp(z))
}


#' Per-study M-spline bases of matching dimension
#'
#' Builds one basis per baseline stratum from that study's OWN observed times,
#' which is what keeps a stratified flexible baseline identified: a basis
#' function with no support over a study's observed period leaves that study's
#' spline scale free to trade off against its intercept, an exact likelihood
#' ridge (see `tests/testthat/test-mspline-identification.R`).
#'
#' The Stan models carry one simplex dimension shared across strata, so the two
#' bases must agree in size. Tied event times, which are the norm in pseudo-IPD
#' reconstructed from a digitized Kaplan-Meier curve, can collapse duplicated
#' quantile knots in one study and not the other. Falling back to a single
#' pooled basis when that happens would route valid user data straight back into
#' the nonidentified configuration the per-study knots exist to prevent, so
#' instead the realized internal-knot count is reduced until BOTH studies agree,
#' and if no workable count exists the fit stops rather than silently returning
#' a ridge.
#'
#' @param ipd The index study's individual data (`.time`, `.status`).
#' @param pseudo The comparator study's reconstructed pseudo-IPD.
#' @param n_knots Requested number of internal knots.
#' @param degree Spline degree (3 = cubic M-spline, 0 = piecewise exponential).
#' @return A list with `index` and `comparator` basis specs of equal
#'   `n_scoef`, and `n_knots` (the realized count actually used).
#' @keywords internal
.matched_per_study_bases <- function(ipd, pseudo, n_knots, degree) {
  build <- function(nk) {
    k_idx <- .knots_from_times(ipd$.time, ipd$.time[ipd$.status == 1], nk)
    k_cmp <- .knots_from_times(pseudo$.time, pseudo$.time[pseudo$.status == 1], nk)
    list(index = .build_mspline_basis(k_idx, degree),
         comparator = .build_mspline_basis(k_cmp, degree))
  }

  # How far the reduction may go is a property of the DEGREE, not a constant. A
  # degree-3 basis with no internal knots still has degree + 1 = 4 coefficients,
  # so zero is a valid, and always-matching, last resort. A degree-0
  # (piecewise-exponential) basis with no internal knots collapses to a single
  # constant hazard, which is an exponential model with no shape to smooth, so
  # it must keep at least one internal knot. Stopping at 1 for every degree hid
  # the valid cubic fallback behind an error.
  min_nk <- if (degree >= 1L) 0L else 1L

  nk <- as.integer(n_knots)
  repeat {
    specs <- build(nk)
    if (specs$index$n_scoef == specs$comparator$n_scoef) break
    nk <- nk - 1L
    if (nk < min_nk) {
      stop("Could not place per-study M-spline knots of equal dimension: the ",
           "index and comparator studies realize different numbers of internal ",
           "knots at every `n_knots` down to ", min_nk, ", which happens when ",
           "tied event times collapse quantile knots in one study only. A ",
           "single pooled basis is not used as a fallback, because with ",
           "unequal follow-up it leaves the shorter study's late basis columns ",
           "unsupported and the likelihood exactly flat along a ",
           "scale/intercept direction. Reduce the number of distinct tied ",
           "times in the reconstructed comparator curve, or fit a parametric ",
           "`distribution` instead.",
           call. = FALSE)
    }
  }

  if (nk < as.integer(n_knots)) {
    # Reducing the count keeps every column supported in BOTH studies, which is
    # the property that matters; report it rather than let the fitted model
    # quietly disagree with the requested `n_knots`.
    message("Reduced `n_knots` from ", n_knots, " to ", nk,
            " so the index and comparator M-spline bases have the same ",
            "dimension (tied event times collapsed quantile knots in one ",
            "study). Each study still gets knots over its own observed times.")
  }

  specs$n_knots <- nk
  # Belt and braces: with per-study boundaries every column is supported by
  # construction, but the identification guarantee is worth asserting rather
  # than assuming, since it is the whole reason this function exists.
  # Support has to be judged over the period each study was actually AT RISK.
  # With delayed entry nobody is under observation before the earliest entry
  # time, so a column living only there enters no event hazard and no exposure
  # increment, and is exactly as unidentified as one past the end of follow-up.
  .assert_basis_support(specs$index, max(ipd$.time), "index",
                        ipd$.delay_time, ipd$.time,
                        ipd$.time[ipd$.status == 1])
  .assert_basis_support(specs$comparator, max(pseudo$.time), "comparator",
                        pseudo$.delay_time, pseudo$.time,
                        pseudo$.time[pseudo$.status == 1])
  specs
}


#' Stop if any basis column has no support over a study's observed period
#'
#' An unsupported column is exactly the nonidentification condition: its
#' coefficient cannot be moved by the likelihood, so simplex mass can be parked
#' there and traded against the study intercept at no cost in fit.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param observed_max The largest time that study actually observed.
#' @param label Study label used in the error message.
#' @param entry,exit The study's per-subject entry and exit times, whose merged
#'   union is the period it had someone under observation. Omit both for data
#'   with no delayed entry, which is treated as one interval from zero.
#' @param event The study's event times, or `NULL`. The cumulative hazard
#'   integrates over the risk intervals and cannot see an isolated instant, but
#'   the event term evaluates the hazard AT each event time, so a column
#'   positive only there is supported after all.
#' @return `TRUE`, invisibly.
#' @keywords internal
.assert_basis_support <- function(spec, observed_max, label,
                                  entry = NULL, exit = NULL, event = NULL) {
  # Evaluate at STRUCTURAL points, not a fixed uniform grid. A degree-0
  # (piecewise exponential) basis column is supported on exactly one inter-knot
  # interval, and a narrow interval can fall entirely between the points of a
  # uniform grid; the column would then be reported dead although it is fine.
  # Every knot, every inter-knot midpoint, and the boundaries are enough: each
  # column of an M-spline basis of any degree is positive somewhere on the
  # interior of its own support, and its support always contains at least one
  # full inter-knot interval, hence at least one of these midpoints.
  risk <- .risk_intervals(entry, exit, observed_max)
  at_risk_start <- risk[[1L]][["lo"]]
  knots <- sort(unique(c(spec$boundary, spec$internal)))
  # Structural points WITHIN each covered stretch. Sampling the whole span
  # instead would put points in the gaps between risk intervals, where no
  # subject is under observation and a basis column therefore enters no
  # likelihood term.
  grid <- unlist(lapply(risk, function(iv) {
    b <- sort(unique(c(iv[["lo"]], iv[["hi"]],
                       knots[knots > iv[["lo"]] & knots < iv[["hi"]]])))
    if (length(b) < 2L) {
      return(numeric(0))
    }
    mids <- (utils::head(b, -1L) + b[-1L]) / 2
    inner <- seq(iv[["lo"]], iv[["hi"]], length.out = 66L)
    # STRICTLY inside. A piecewise-constant column is positive at the closed
    # left end of its own interval, so evaluating at a bare endpoint made a
    # column live off a single instant: with exposure on [1, 2] and [8, 9], the
    # column on [2, 8) is positive at t = 2 alone, contributes zero integrated
    # hazard, and would have passed. A point carries no likelihood.
    c(mids, inner[-c(1L, length(inner))])
  }))
  # The interiors above cover the CUMULATIVE hazard, which integrates over the
  # risk intervals and so cannot see an isolated instant. The event term is the
  # other half of the likelihood: it evaluates the hazard AT each event time,
  # so a column positive only there does carry likelihood after all. With
  # exposure on [1, 2] and [8, 9] and a degree-0 basis, the column on [2, 8) is
  # positive at t = 2 alone; that is dead when nobody fails at 2 and live when
  # somebody does, and only the event times distinguish the two.
  grid <- c(grid, event[is.finite(event)])
  grid <- sort(unique(grid[is.finite(grid)]))
  if (!length(grid)) {
    return(invisible(TRUE))
  }
  b <- .eval_basis(spec, grid, integral = FALSE)
  # M-spline values have units of inverse time. An absolute cutoff therefore
  # changes the answer when the same follow-up is expressed in days rather
  # than seconds. Support is structural: a column is live if it is positive at
  # any structural point, regardless of its numerical scale.
  live <- apply(is.finite(b) & b > 0, 2, any)
  dead <- which(!live)
  if (length(dead) > 0L) {
    where <- if (length(risk) > 1L || at_risk_start > 0) {
      paste0("its observed risk set (",
             paste(vapply(risk, function(iv) {
               sprintf("[%s, %s]", format(iv[["lo"]], digits = 4),
                       format(iv[["hi"]], digits = 4))
             }, character(1)), collapse = " and "), ")")
    } else {
      "its observed follow-up"
    }
    stop("The ", label, " study's M-spline basis has ", length(dead),
         " column(s) with no support over ", where, " (columns ",
         paste(dead, collapse = ", "), "). That is an exact likelihood ridge: ",
         "the spline scale is unidentified against the study intercept. ",
         if (length(risk) > 1L || at_risk_start > 0) {
           paste0("Nobody is under observation outside that risk set, so a ",
                  "column supported only there enters no event hazard and no ",
                  "exposure increment. ")
         } else {
           ""
         },
         "Reduce `n_knots`.", call. = FALSE)
  }
  if (at_risk_start > 0) {
    # Even with every column supported, the hazard BELOW the earliest entry
    # time is not informed by these data at all. That does not affect a
    # conditional quantity, but S(t) and RMST integrate the hazard from 0, so
    # those carry whatever the prior says about a stretch nobody was observed
    # in. A reader comparing absolute survival across arms deserves to know
    # which part of the curve that is.
    message("The ", label, " study enters at ",
            format(at_risk_start, digits = 4),
            ", so no observation informs its hazard below that time. ",
            "Conditional quantities are unaffected, but absolute survival and ",
            "RMST integrate from 0 and are therefore prior-dependent over ",
            "[0, ", format(at_risk_start, digits = 4), "]. Compare survival ",
            "conditional on reaching entry, or report RMST from a landmark at ",
            "or after it.")
  }
  invisible(TRUE)
}


#' The stretches of time a study actually had someone under observation
#'
#' The union of each subject's `[entry, exit]`, merged. Reducing this to a
#' single span from the earliest entry to the last exit would treat a gap with
#' an empty risk set as observed: subjects seen on `[1, 2]` and `[8, 9]` leave
#' `(2, 8)` contributing no event hazard and no cumulative-hazard exposure, and
#' a basis column living only there is as unidentified as one before the first
#' entry. Without entry times this is a single interval from zero, which is
#' what keeps the check unchanged for ordinary data.
#'
#' @param entry Entry times, or `NULL`.
#' @param exit Exit times, or `NULL`.
#' @param observed_max Last observed time, used when `exit` is absent.
#' @return A list of `c(lo, hi)` intervals, in increasing order.
#' @keywords internal
.risk_intervals <- function(entry, exit, observed_max) {
  whole <- list(c(lo = 0, hi = observed_max))
  if (is.null(entry) || !is.numeric(entry) || !length(entry)) {
    return(whole)
  }
  if (is.null(exit) || !is.numeric(exit) || length(exit) != length(entry)) {
    # Entry times without matching exits still say where observation STARTS,
    # which is the larger of the two errors; fall back to one interval from it.
    lo <- suppressWarnings(min(entry[is.finite(entry)]))
    if (!is.finite(lo)) {
      return(whole)
    }
    return(list(c(lo = max(0, lo), hi = observed_max)))
  }
  # Compare against the CLAMPED lower bound, which is what `lo` below uses.
  # Testing the raw entry instead let an interval lying entirely before zero
  # through: (-2, -1) satisfies exit > entry, and the clamp then turned it into
  # (0, -1), an inverted interval that broke this function's own increasing
  # order contract and would have handed `.assert_basis_support()` a grid to
  # build over negative time.
  keep <- is.finite(entry) & is.finite(exit) & exit > pmax(0, entry)
  if (!any(keep)) {
    return(whole)
  }
  lo <- pmax(0, entry[keep])
  hi <- exit[keep]
  ord <- order(lo)
  lo <- lo[ord]
  hi <- hi[ord]
  out <- list()
  cur <- c(lo = lo[[1L]], hi = hi[[1L]])
  for (i in seq_along(lo)[-1L]) {
    if (lo[[i]] <= cur[["hi"]]) {
      cur[["hi"]] <- max(cur[["hi"]], hi[[i]])
    } else {
      out[[length(out) + 1L]] <- cur
      cur <- c(lo = lo[[i]], hi = hi[[i]])
    }
  }
  out[[length(out) + 1L]] <- cur
  out
}
