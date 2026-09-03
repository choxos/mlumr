#' Validate and resolve link function for a given family
#'
#' Checks that `link` is valid for `family` and returns the resolved link name
#' plus an integer code for Stan. Accepts canonical family names (`"binomial"`,
#' `"normal"`, `"poisson"`) and data-type aliases (`"binary"`, `"count"`,
#' `"rate"`, `"continuous"`).
#'
#' The V1 likelihood/link matrix is:
#'
#' \tabular{llll}{
#'   \strong{Data type} \tab \strong{Family} \tab \strong{Likelihoods} \tab \strong{Link functions} \cr
#'   Binary  \tab binomial \tab bernoulli (IPD), binomial (AgD)  \tab logit, probit, cloglog \cr
#'   Count*  \tab binomial \tab bernoulli (IPD), binomial (AgD)  \tab logit, probit, cloglog \cr
#'   Rate    \tab poisson  \tab poisson                          \tab log                    \cr
#'   Continuous \tab normal \tab normal                           \tab identity, log          \cr
#' }
#'
#' *`"count"` refers to count/total (binomial denominator) data, not Poisson
#' event counts. For Poisson rate or count outcomes, use `"poisson"` or `"rate"`.
#'
#' @param family Character: canonical (`"binomial"`, `"normal"`, `"poisson"`)
#'   or alias (`"binary"`, `"count"`, `"rate"`, `"continuous"`).
#' @param link Character or `NULL`. If `NULL`, uses default for family.
#' @return List with components:
#' \describe{
#'   \item{family}{Canonical family name (e.g. `"binomial"`)}
#'   \item{link}{Resolved link name (e.g. `"probit"`)}
#'   \item{code}{Integer code for Stan data block}
#' }
#' @keywords internal
check_link <- function(family, link = NULL) {

  family <- .validate_link_string(family, "family")
  family <- tolower(family)

  # Resolve data-type aliases to canonical family names (user-facing
  # ergonomics; valid links / defaults come from family_config)
  family_aliases <- c(
    binary     = "binomial",
    count      = "binomial",
    rate       = "poisson",
    continuous = "normal",
    tte        = "survival",
    surv       = "survival"
  )
  canonical <- unname(family_aliases[family])
  if (!is.na(canonical)) {
    family <- canonical
  }

  if (!family %in% names(family_config)) {
    stop(sprintf("Unknown family '%s'. Valid: %s",
                 family,
                 paste(c(names(family_config), names(family_aliases)),
                       collapse = ", ")),
         call. = FALSE)
  }

  cfg <- family_config[[family]]

  if (is.null(link)) {
    link <- cfg$link_default
  } else {
    link <- .validate_link_string(link, "link")
  }
  link <- tolower(link)

  if (!link %in% cfg$links) {
    stop(sprintf("Link '%s' is not valid for family '%s'. Valid: %s",
                 link, family, paste(cfg$links, collapse = ", ")),
         call. = FALSE)
  }

  list(
    family = family,
    link = link,
    code = as.integer(match(link, cfg$links))
  )
}

#' Inverse link function (linear predictor -> response scale)
#' @param x Numeric vector
#' @param link Character: link function name
#' @return Numeric vector on response scale
#' @importFrom stats pnorm
#' @keywords internal
inverse_link <- function(x, link = c("identity", "log", "logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(x, "x")
  switch(link,
    identity = x,
    log      = exp(x),
    logit    = plogis(x),
    probit   = pnorm(x),
    cloglog  = -expm1(-exp(x))
  )
}


#' Stable log probabilities for binary inverse links
#'
#' Returns `log(P(Y = 1))` and `log(P(Y = 0))` without first rounding either
#' probability to zero or one. This is used when a marginal contrast remains
#' finite even though its natural-scale probabilities are outside double
#' precision.
#' @keywords internal
.binary_log_probs <- function(eta, link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(eta, "eta")

  if (link == "logit") {
    return(list(
      event = -(pmax(-eta, 0) + log1p(exp(-abs(eta)))),
      nonevent = -(pmax(eta, 0) + log1p(exp(-abs(eta))))
    ))
  }
  if (link == "probit") {
    return(list(
      event = stats::pnorm(eta, log.p = TRUE),
      nonevent = stats::pnorm(eta, lower.tail = FALSE, log.p = TRUE)
    ))
  }

  exp_eta <- exp(eta)
  log_event <- log(-expm1(-exp_eta))
  # `small` indexes both a test and an assignment, so an NA in `eta` would make
  # `any()` return NA (an error in `if`) and the assignment illegal. Missing
  # input must propagate as missing output, the way the logit and probit
  # branches above already do.
  small <- !is.na(eta) & eta < -18
  if (any(small)) {
    # 1 - exp(-x) = x (1 - x/2 + x^2/6 - ...), so log(1 - exp(-x)) is
    # eta + log1p(-x/2 + x^2/6) with x = exp(eta). Below eta = -18 the direct
    # form loses the leading digits, and below eta = -745 exp(eta) underflows to
    # zero and it returns -Inf instead of eta.
    x <- exp_eta[small]
    log_event[small] <- eta[small] + log1p(-x / 2 + x^2 / 6)
  }
  list(event = log_event, nonevent = -exp_eta)
}


#' Binary link of a marginal probability represented on both log tails
#' @keywords internal
.binary_link_from_logs <- function(log_event, log_nonevent,
                                   link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  if (link == "logit") return(log_event - log_nonevent)
  if (link == "probit") {
    # Invert through whichever tail is the smaller one, which is where the
    # log-scale input carries its digits. An NA subscript is illegal in an
    # assignment, so missing values are carried through explicitly rather than
    # silently taking the `numeric()` zero fill.
    out <- rep(NA_real_, length(log_event))
    known <- !is.na(log_event) & !is.na(log_nonevent)
    lower <- known & log_event <= log(0.5)
    upper <- known & !lower
    out[lower] <- stats::qnorm(log_event[lower], log.p = TRUE)
    out[upper] <- stats::qnorm(log_nonevent[upper], lower.tail = FALSE,
                               log.p = TRUE)
    return(out)
  }

  # For a very small event probability, log(1 - p) rounds to zero even though
  # the complementary-log-log link is the ordinary finite value log(p) plus a
  # negligible correction. Use that equivalent tail representation directly.
  out <- log(-log_nonevent)
  # For a tiny event probability, -log(1 - p) is p to within double precision,
  # so cloglog(p) = log(-log(1 - p)) is log(p). Use that when log(1 - p) has
  # rounded to zero and `log(-log_nonevent)` would return -Inf for a value that
  # is finite.
  small <- !is.na(log_event) & log_event < -18
  out[small] <- log_event[small]
  out
}


#' Elementwise log(exp(x) + exp(y))
#' @keywords internal
.logspace_add <- function(x, y) {
  out <- pmax(x, y)
  finite <- is.finite(out)
  out[finite] <- out[finite] +
    log1p(exp(-abs(x[finite] - y[finite])))
  both_neg_inf <- is.infinite(x) & x < 0 & is.infinite(y) & y < 0
  out[both_neg_inf] <- -Inf
  out
}


#' Weighted log mean of exponentiated values
#' @keywords internal
.weighted_log_mean_exp <- function(x, weights = rep(1, length(x))) {
  if (length(x) != length(weights) || any(!is.finite(weights)) ||
        any(weights < 0) || !any(weights > 0)) {
    stop("`weights` must be finite, non-negative, and match `x`.", call. = FALSE)
  }
  # A zero weight contributes nothing, but log(0) is -Inf and x + -Inf is NaN
  # for an infinite x, which would poison the maximum. Drop them first.
  keep <- weights > 0
  x <- x[keep]
  weights <- weights[keep]
  log_weights <- log(weights)
  z <- x + log_weights
  m_num <- max(z)
  if (is.infinite(m_num)) return(m_num)
  m_den <- max(log_weights)
  log_num <- m_num + log(sum(exp(z - m_num)))
  log_den <- m_den + log(sum(exp(log_weights - m_den)))
  log_num - log_den
}


#' Normalize non-negative weights without overflowing their sum
#' @keywords internal
.normalize_weights <- function(weights) {
  if (!length(weights) || any(!is.finite(weights)) || any(weights < 0) ||
        !any(weights > 0)) {
    stop("`weights` must be finite, non-negative, and include a positive value.",
         call. = FALSE)
  }
  scaled <- weights / max(weights)
  scaled / sum(scaled)
}


#' Stable difference exp(log_x) - exp(log_y)
#'
#' Evaluates the difference from the logarithms so that cancellation happens
#' before the return to the natural scale. Equal logarithms return exactly `0`,
#' including `-Inf - -Inf` (both quantities are zero) and `Inf - Inf`, which is
#' mathematically indeterminate and is reported as no difference rather than
#' `NaN`. Arguments are recycled to a common length; `NA` propagates.
#' @keywords internal
.exp_difference_logs <- function(log_x, log_y) {
  # Recycle to a common length up front. Comparing vectors of different lengths
  # recycles inside each test but not in `out`, which silently produced NA tails
  # instead of either an answer or an error.
  n <- max(length(log_x), length(log_y))
  if (length(log_x) != n) log_x <- rep_len(log_x, n)
  if (length(log_y) != n) log_y <- rep_len(log_y, n)
  out <- rep(NaN, n)
  known <- !is.na(log_x) & !is.na(log_y)
  # Two positive infinities mean both quantities are unbounded, so their
  # difference is indeterminate. Without this guard the equality branch below
  # would report an exact null effect. Equal finite logs, and two -Inf logs
  # (both quantities zero), do legitimately give a difference of zero.
  both_unbounded <- known & log_x == Inf & log_y == Inf
  same <- known & !both_unbounded & log_x == log_y
  x_larger <- known & !same & log_x > log_y
  y_larger <- known & !same & log_y > log_x

  out[same] <- 0
  out[x_larger] <- exp(
    log_x[x_larger] + log(-expm1(log_y[x_larger] - log_x[x_larger]))
  )
  out[y_larger] <- -exp(
    log_y[y_larger] + log(-expm1(log_x[y_larger] - log_y[y_larger]))
  )
  out
}

#' Link function (response scale -> linear predictor)
#' @param x Numeric vector on response scale
#' @param link Character: link function name
#' @return Numeric vector on linear predictor scale
#' @keywords internal
link_fun <- function(x, link = c("identity", "log", "logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(x, "x")
  eps <- .Machine$double.eps
  p <- pmin(pmax(x, eps), 1 - eps)
  switch(link,
    identity = x,
    log      = log(pmax(x, eps)),
    logit    = qlogis(p),
    probit   = qnorm(p),
    cloglog  = log(-log1p(-p))
  )
}

#' Apply a boundary-only binomial continuity correction
#'
#' At zero or all events, uses the pseudo-count estimate
#' `(r + min_count) / (n + 2 * min_count)`. Interior probabilities are
#' unchanged.
#' @keywords internal
bound_probability <- function(p, n, min_count = 0.5) {
  .validate_numeric_vector(p, "p")
  .validate_positive_numeric(n, "n")
  .validate_positive_numeric(min_count, "min_count")
  if (any(2 * min_count > n)) {
    stop("`min_count` must be no larger than n / 2.", call. = FALSE)
  }
  # A probability outside [0, 1] is an upstream construction error, not a
  # boundary arm. Correcting it would return a plausible-looking number and
  # hide the bug that produced it.
  if (any(!is.na(p) & (p < 0 | p > 1))) {
    stop("`p` must lie in [0, 1].", call. = FALSE)
  }
  # ifelse() sizes its result by the length of the test, so recycle first:
  # a scalar `p` with a vector `n` would otherwise collapse to one value,
  # where the previous pmin/pmax form returned one result per `n`.
  len <- max(length(p), length(n))
  p <- rep_len(p, len)
  n <- rep_len(n, len)
  lower <- min_count / (n + 2 * min_count)
  upper <- (n + min_count) / (n + 2 * min_count)
  ifelse(p == 0, lower, ifelse(p == 1, upper, p))
}

#' Derivative of a binomial link with respect to probability
#' @keywords internal
link_derivative_response <- function(p, link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(p, "p")
  p <- .bound_unit_interval(p)

  switch(link,
    logit = 1 / (p * (1 - p)),
    probit = 1 / dnorm(qnorm(p)),
    cloglog = 1 / ((1 - p) * (-log1p(-p)))
  )
}

#' Derivative of inverse link with respect to linear predictor
#' @keywords internal
inverse_link_derivative <- function(eta,
                                    p = inverse_link(eta, link),
                                    link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(eta, "eta")
  .validate_numeric_vector(p, "p")
  p <- .bound_unit_interval(p)

  switch(link,
    logit = p * (1 - p),
    probit = dnorm(eta),
    cloglog = {
      exp_eta <- exp(eta)
      deriv <- exp_eta * exp(-exp_eta)
      deriv[is.infinite(exp_eta) & exp_eta > 0] <- 0
      deriv
    }
  )
}

#' Delta-method variance for a transformed binomial proportion
#' @keywords internal
binomial_link_variance <- function(p, n, link = c("logit", "probit", "cloglog")) {
  .validate_numeric_vector(p, "p")
  .validate_positive_numeric(n, "n")
  p <- .bound_unit_interval(p)
  p * (1 - p) * link_derivative_response(p, link)^2 / n
}

#' Emit package progress messages when enabled
#' @keywords internal
mlumr_message <- function(..., verbose = TRUE) {
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(verbose)) {
    message(...)
  }
  invisible(NULL)
}


#' Validate a scalar link/family string
#' @keywords internal
.validate_link_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a single non-missing string.", name),
         call. = FALSE)
  }
  x
}


#' Validate a numeric vector
#' @keywords internal
.validate_numeric_vector <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric.", name), call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate positive finite numeric input
#' @keywords internal
.validate_positive_numeric <- function(x, name) {
  if (!is.numeric(x) || length(x) == 0L ||
        any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf("`%s` must contain positive finite values.", name),
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Bound probabilities to the open unit interval
#' @keywords internal
.bound_unit_interval <- function(p) {
  eps <- .Machine$double.eps
  pmin(pmax(p, eps), 1 - eps)
}
