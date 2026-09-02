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
  .assert_basis_support(specs$index, max(ipd$.time), "index")
  .assert_basis_support(specs$comparator, max(pseudo$.time), "comparator")
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
#' @return `TRUE`, invisibly.
#' @keywords internal
.assert_basis_support <- function(spec, observed_max, label) {
  # Evaluate at STRUCTURAL points, not a fixed uniform grid. A degree-0
  # (piecewise exponential) basis column is supported on exactly one inter-knot
  # interval, and a narrow interval can fall entirely between the points of a
  # uniform grid; the column would then be reported dead although it is fine.
  # Every knot, every inter-knot midpoint, and the boundaries are enough: each
  # column of an M-spline basis of any degree is positive somewhere on the
  # interior of its own support, and its support always contains at least one
  # full inter-knot interval, hence at least one of these midpoints.
  breaks <- sort(unique(c(spec$boundary, spec$internal)))
  breaks <- breaks[breaks <= observed_max]
  if (length(breaks) < 2L) breaks <- c(0, observed_max)
  mids <- (utils::head(breaks, -1L) + breaks[-1L]) / 2
  grid <- sort(unique(c(breaks, mids,
                        seq(0, observed_max, length.out = 256L))))
  grid <- grid[is.finite(grid) & grid >= 0 & grid <= observed_max]
  b <- .eval_basis(spec, grid, integral = FALSE)
  # M-spline values have units of inverse time. An absolute cutoff therefore
  # changes the answer when the same follow-up is expressed in days rather
  # than seconds. Support is structural: a column is live if it is positive at
  # any structural point, regardless of its numerical scale.
  live <- apply(is.finite(b) & b > 0, 2, any)
  dead <- which(!live)
  if (length(dead) > 0L) {
    stop("The ", label, " study's M-spline basis has ", length(dead),
         " column(s) with no support over its observed follow-up (columns ",
         paste(dead, collapse = ", "), "). That is an exact likelihood ridge: ",
         "the spline scale is unidentified against the study intercept. ",
         "Reduce `n_knots`.", call. = FALSE)
  }
  invisible(TRUE)
}
