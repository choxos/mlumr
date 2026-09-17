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
#' The core of [make_knots()], applied either to the pooled times (one shared
#' baseline) or to each study's own times (`aux_by = ".study"`). Per-study
#' boundary knots keep a stratified baseline identified: a basis function with
#' no support over a study's observed period leaves its spline scale free to
#' trade off against the intercept.
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

  # Anchor at t = 0, not at the minimum entry time, so H(0) = 0 stays
  # continuous with the backward extrapolation under delayed entry.
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



#' Per-study M-spline bases of matching dimension
#'
#' One basis per stratum from that study's own observed times, which is what
#' keeps a stratified flexible baseline identified. The Stan models share one
#' simplex dimension across strata, so when tied event times collapse
#' quantile knots in one study only, the internal-knot count is reduced until
#' both studies agree; a pooled fallback would restore the nonidentified
#' configuration the per-study knots exist to prevent.
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

  # A degree-3 basis with no internal knots still has four coefficients; a
  # degree-0 one collapses to a constant hazard, so it keeps one knot.
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
           "tied event times collapse quantile knots in one study only. Reduce ",
           "the number of distinct tied times in the reconstructed comparator ",
           "curve, or fit a parametric `distribution` instead.",
           call. = FALSE)
    }
  }

  if (nk < as.integer(n_knots)) {
    message("Reduced `n_knots` from ", n_knots, " to ", nk,
            " so the index and comparator M-spline bases have the same ",
            "dimension (tied event times collapsed quantile knots in one ",
            "study). Each study still gets knots over its own observed times.")
  }

  specs$n_knots <- nk
  # Support is judged over the period each study was at risk, so a column
  # living only before the earliest entry counts as unsupported.
  .assert_basis_support(specs$index, max(ipd$.time), "index",
                        ipd$.delay_time, ipd$.time,
                        ipd$.time[ipd$.status == 1])
  .assert_basis_support(specs$comparator, max(pseudo$.time), "comparator",
                        pseudo$.delay_time, pseudo$.time,
                        pseudo$.time[pseudo$.status == 1])
  specs
}


#' Which basis columns carry likelihood over one study's observed risk set
#'
#' Shared by [.assert_basis_support()] and
#' [.assert_shared_basis_identified()], so what counts as support is decided
#' once.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param observed_max Largest observed time; used when `exit` is absent.
#' @param entry,exit,event Delayed-entry, exit and event times.
#' @return A logical vector with one entry per basis column. All `TRUE` when
#'   there is no risk period to evaluate over, which is not this function's to
#'   refuse.
#' @keywords internal
.live_basis_columns <- function(spec, observed_max, entry = NULL, exit = NULL,
                                event = NULL) {
  # Evaluate at structural points (knots, inter-knot midpoints, boundaries),
  # not a uniform grid that can miss a narrow degree-0 interval.
  risk <- .risk_intervals(entry, exit, observed_max)
  knots <- sort(unique(c(spec$boundary, spec$internal)))
  # Only within each covered stretch: nobody is at risk in the gaps.
  grid <- unlist(lapply(risk, function(iv) {
    b <- sort(unique(c(iv[["lo"]], iv[["hi"]],
                       knots[knots > iv[["lo"]] & knots < iv[["hi"]]])))
    if (length(b) < 2L) {
      return(numeric(0))
    }
    mids <- (utils::head(b, -1L) + b[-1L]) / 2
    inner <- seq(iv[["lo"]], iv[["hi"]], length.out = 66L)
    # Strictly inside: an endpoint carries no exposure.
    c(mids, inner[-c(1L, length(inner))])
  }))
  # The event term evaluates the hazard at each event time, so a column
  # positive only there still carries likelihood.
  grid <- c(grid, event[is.finite(event)])
  grid <- sort(unique(grid[is.finite(grid)]))
  if (!length(grid)) {
    return(rep(TRUE, spec$n_scoef))
  }
  b <- .eval_basis(spec, grid, integral = FALSE)
  # Support is structural: positive somewhere, whatever the time unit.
  apply(is.finite(b) & b > 0, 2, any)
}


#' Refuse a shared baseline whose weights float against the study intercepts
#'
#' With `aux_by = "none"` the model carries one weight simplex and a
#' separate intercept per study, so if the studies' exposure falls on
#' disjoint sets of basis columns, mass can be moved between the sets and
#' absorbed exactly by the intercepts while the treatment contrast moves
#' freely. What rules it out is that the studies and the columns they touch
#' form one connected component. Connectivity is exact for a degree-0 basis
#' and necessary rather than sufficient above it.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param studies A named list; each element a list with `observed_max`,
#'   `entry`, `exit` and `event`.
#' @return `TRUE`, invisibly.
#' @keywords internal
.assert_shared_basis_identified <- function(spec, studies) {
  inc <- vapply(studies, function(st) {
    .live_basis_columns(spec, st$observed_max, st$entry, st$exit, st$event)
  }, logical(spec$n_scoef))
  inc <- matrix(inc, nrow = spec$n_scoef, ncol = length(studies),
                dimnames = list(NULL, names(studies)))
  if (ncol(inc) < 2L) {
    return(invisible(TRUE))
  }
  # Grow one component out from the first study.
  seen_study <- c(TRUE, rep(FALSE, ncol(inc) - 1L))
  seen_col <- rep(FALSE, nrow(inc))
  repeat {
    next_col <- seen_col | apply(inc[, seen_study, drop = FALSE], 1, any)
    next_study <- seen_study | apply(inc[next_col, , drop = FALSE], 2, any)
    if (identical(next_col, seen_col) && identical(next_study, seen_study)) {
      break
    }
    seen_col <- next_col
    seen_study <- next_study
  }
  if (all(seen_study)) {
    return(invisible(TRUE))
  }
  reached <- names(studies)[seen_study]
  cut_off <- names(studies)[!seen_study]
  stop("The shared baseline is unidentified: ",
       paste(reached, collapse = ", "), " and ",
       paste(cut_off, collapse = ", "),
       " are observed over disjoint sets of spline columns, so the weights on ",
       "one set can be rescaled and absorbed exactly by the study intercepts. ",
       "Give each study its own baseline with `aux_by = \".study\"`, or ",
       "reduce `n_knots` until the studies share a column.", call. = FALSE)
}


#' Stop if any basis column has no support over a study's observed period
#'
#' An unsupported column is the nonidentification condition: simplex mass can
#' be parked on it and traded against the study intercept at no cost in fit.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param observed_max The largest time that study actually observed.
#' @param label Study label used in the error message.
#' @param entry,exit The study's per-subject entry and exit times; omit both
#'   for data with no delayed entry.
#' @param event The study's event times, or `NULL`.
#' @return `TRUE`, invisibly.
#' @keywords internal
.assert_basis_support <- function(spec, observed_max, label,
                                  entry = NULL, exit = NULL, event = NULL) {
  risk <- .risk_intervals(entry, exit, observed_max)
  at_risk_start <- risk[[1L]][["lo"]]
  live <- .live_basis_columns(spec, observed_max, entry, exit, event)
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
    # Below the earliest entry the hazard is extrapolated under the spline
    # restrictions; conditioning on survival to a landmark cancels it.
    message("The ", label, " study enters at ",
            format(at_risk_start, digits = 4),
            ", so nobody was at risk below that time and its hazard there is ",
            "extrapolated under the spline restrictions rather than observed. ",
            "Absolute survival and RMST integrate from 0 and depend on that ",
            "stretch; conditioning on survival to a landmark cancels it, so ",
            "compare survival conditional on reaching entry, or report RMST ",
            "from a landmark at or after it.")
  }
  invisible(TRUE)
}


#' The stretches of time a study actually had someone under observation
#'
#' The union of each subject's `[entry, exit]`, merged, so a gap with an
#' empty risk set is not treated as observed. Without entry times this is
#' one interval from zero.
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
    # Entry times without exits still say where observation starts.
    lo <- suppressWarnings(min(entry[is.finite(entry)]))
    if (!is.finite(lo)) {
      return(whole)
    }
    return(list(c(lo = max(0, lo), hi = observed_max)))
  }
  # Compare against the clamped lower bound, so an interval entirely before
  # zero is dropped rather than inverted.
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
