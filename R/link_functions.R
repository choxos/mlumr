#' Validate and resolve link function for a given family
#'
#' Checks that `link` is valid for `family` and returns the resolved link name
#' plus an integer code for Stan. `family` is the canonical name the data
#' setup records: `"binomial"`, `"normal"` or `"poisson"` from [set_ipd()] and
#' [set_agd()], or `"survival"` from [set_ipd()] with [set_agd_surv()].
#'
#' The likelihood/link matrix is:
#'
#' \tabular{lll}{
#'   \strong{Family} \tab \strong{Likelihoods} \tab \strong{Link functions} \cr
#'   binomial \tab bernoulli (IPD), binomial (AgD) \tab logit, probit, cloglog \cr
#'   poisson  \tab poisson                         \tab log                    \cr
#'   normal   \tab normal                          \tab identity, log          \cr
#'   survival \tab parametric or M-spline hazard   \tab log (the only one)     \cr
#' }
#'
#' @param family Character: `"binomial"`, `"normal"`, `"poisson"` or
#'   `"survival"`.
#' @param link Character or `NULL`. If `NULL`, uses default for family.
#' @return List with components:
#' \describe{
#'   \item{family}{Canonical family name (e.g. `"binomial"`)}
#'   \item{link}{Resolved link name (e.g. `"probit"`)}
#'   \item{code}{Integer code for Stan data block}
#' }
#' @noRd
check_link <- function(family, link = NULL) {

  family <- .validate_link_string(family, "family")
  family <- tolower(family)

  if (!family %in% names(family_config)) {
    stop(sprintf("Unknown family '%s'. Valid: %s",
                 family, paste(names(family_config), collapse = ", ")),
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
#' @noRd
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
#' @noRd
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
  # Missing input propagates as missing output.
  small <- !is.na(eta) & eta < -18
  if (any(small)) {
    # Series for log(1 - exp(-x)) with x = exp(eta), exact where the direct
    # form loses its leading digits.
    x <- exp_eta[small]
    log_event[small] <- eta[small] + log1p(-x / 2 + x^2 / 6)
  }
  list(event = log_event, nonevent = -exp_eta)
}


#' Binary link of a marginal probability represented on both log tails
#' @noRd
.binary_link_from_logs <- function(log_event, log_nonevent,
                                   link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  if (link == "logit") return(log_event - log_nonevent)
  if (link == "probit") {
    # Invert through the smaller tail, which carries the digits.
    out <- rep(NA_real_, length(log_event))
    known <- !is.na(log_event) & !is.na(log_nonevent)
    lower <- known & log_event <= log(0.5)
    upper <- known & !lower
    out[lower] <- stats::qnorm(log_event[lower], log.p = TRUE)
    out[upper] <- stats::qnorm(log_nonevent[upper], lower.tail = FALSE,
                               log.p = TRUE)
    return(out)
  }

  out <- log(-log_nonevent)
  # For a tiny event probability cloglog(p) is log(p) to double precision,
  # where log(1 - p) has rounded to zero.
  small <- !is.na(log_event) & log_event < -18
  out[small] <- log_event[small]
  out
}


#' Elementwise log(exp(x) + exp(y))
#' @noRd
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
#' @noRd
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
  # Shift by the largest `x` before adding the weights: a log probability can
  # be so large that adding log(w) rounds it away.
  m_x <- max(x)
  if (is.infinite(m_x)) return(m_x)
  # Identical values have the maximum as their mean, exactly.
  if (all(x == m_x)) return(m_x)
  # Near a probability of one the shift throws away the correction that is
  # the answer; `log1p(mean(expm1(x)))` keeps it and is used where it is
  # also safe, `m_x` in (-1, 0].
  if (m_x <= 0 && m_x > -1) {
    # Normalized by the largest weight so the sum neither overflows nor
    # underflows.
    scaled <- weights / max(weights)
    mean_expm1 <- sum(scaled * expm1(x)) / sum(scaled)
    # Only near one; past -0.5 the shifted form is the accurate one.
    if (mean_expm1 > -0.5) return(log1p(mean_expm1))
  }
  z <- (x - m_x) + log_weights
  m_num <- max(z)
  m_den <- max(log_weights)
  log_num <- m_num + log(sum(exp(z - m_num)))
  log_den <- m_den + log(sum(exp(log_weights - m_den)))
  m_x + (log_num - log_den)
}


#' Normalize non-negative weights without overflowing their sum
#' @noRd
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
#' Cancellation happens before the return to the natural scale. Equal logs
#' return exactly `0`, two `+Inf` logs return `NaN`, arguments recycle and
#' `NA` propagates.
#' @noRd
.exp_difference_logs <- function(log_x, log_y) {
  n <- max(length(log_x), length(log_y))
  if (length(log_x) != n) log_x <- rep_len(log_x, n)
  if (length(log_y) != n) log_y <- rep_len(log_y, n)
  out <- rep(NaN, n)
  known <- !is.na(log_x) & !is.na(log_y)
  # Two positive infinities have no difference; equal finite or -Inf logs do.
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
#' @noRd
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
#' @noRd
bound_probability <- function(p, n, min_count = 0.5) {
  .validate_numeric_vector(p, "p")
  .validate_positive_numeric(n, "n")
  .validate_positive_numeric(min_count, "min_count")
  if (any(2 * min_count > n)) {
    stop("`min_count` must be no larger than n / 2.", call. = FALSE)
  }
  # A probability outside [0, 1] is an upstream error, not a boundary arm.
  if (any(!is.na(p) & (p < 0 | p > 1))) {
    stop("`p` must lie in [0, 1].", call. = FALSE)
  }
  # ifelse() sizes its result by the test, so recycle first.
  len <- max(length(p), length(n))
  p <- rep_len(p, len)
  n <- rep_len(n, len)
  lower <- min_count / (n + 2 * min_count)
  upper <- (n + min_count) / (n + 2 * min_count)
  ifelse(p == 0, lower, ifelse(p == 1, upper, p))
}

#' Exact interval for a directly observed binomial proportion
#'
#' Clopper and Pearson's interval, as `stats::binom.test()` reports it but
#' without the integer check: beta quantiles, with the lower bound at 0 when
#' the count is 0 and the upper bound at 1 when the count equals `n`. For
#' integer counts its coverage is at least nominal for every true
#' probability, which the bounded Wald interval it replaced lacked; a
#' fractional count has no such guarantee.
#'
#' @param r Event count, in `[0, n]`.
#' @param n Number of trials, positive.
#' @param conf_level Confidence level.
#' @return List with `lower` and `upper`.
#' @noRd
.clopper_pearson_interval <- function(r, n, conf_level) {
  .validate_numeric_vector(r, "r")
  .validate_positive_numeric(n, "n")
  if (any(r < 0 | r > n)) {
    stop("`r` must lie in [0, n].", call. = FALSE)
  }
  alpha <- 1 - conf_level
  len <- max(length(r), length(n))
  r <- rep_len(r, len)
  n <- rep_len(n, len)
  # The boundary bound is set outright, never asked of a shape-zero beta.
  lower <- numeric(len)
  upper <- rep(1, len)
  inner <- r > 0
  lower[inner] <- stats::qbeta(alpha / 2, r[inner], n[inner] - r[inner] + 1)
  inner <- r < n
  upper[inner] <- stats::qbeta(1 - alpha / 2, r[inner] + 1, n[inner] - r[inner])
  list(lower = lower, upper = upper)
}

#' Exact interval for a directly observed Poisson rate
#'
#' Garwood's interval, as `stats::poisson.test()` reports it: gamma quantiles
#' over the exposure, with the lower bound at 0 when the count is 0. Coverage
#' is at least nominal for every true rate.
#'
#' @param x Event count, non-negative.
#' @param exposure Total exposure, positive.
#' @param conf_level Confidence level.
#' @return List with `lower` and `upper`.
#' @noRd
.garwood_interval <- function(x, exposure, conf_level) {
  .validate_numeric_vector(x, "x")
  .validate_positive_numeric(exposure, "exposure")
  if (any(x < 0)) {
    stop("`x` must be non-negative.", call. = FALSE)
  }
  alpha <- 1 - conf_level
  len <- max(length(x), length(exposure))
  x <- rep_len(x, len)
  exposure <- rep_len(exposure, len)
  lower <- numeric(len)
  inner <- x > 0
  lower[inner] <- stats::qgamma(alpha / 2, x[inner]) / exposure[inner]
  list(lower = lower, upper = stats::qgamma(1 - alpha / 2, x + 1) / exposure)
}

#' Derivative of a binomial link with respect to probability
#' @noRd
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

#' Delta-method variance for a transformed binomial proportion
#' @noRd
binomial_link_variance <- function(p, n, link = c("logit", "probit", "cloglog")) {
  .validate_numeric_vector(p, "p")
  .validate_positive_numeric(n, "n")
  p <- .bound_unit_interval(p)
  p * (1 - p) * link_derivative_response(p, link)^2 / n
}

#' Emit package progress messages when enabled
#' @noRd
mlumr_message <- function(..., verbose = TRUE) {
  .validate_flag(verbose, "verbose")
  if (isTRUE(verbose)) {
    message(...)
  }
  invisible(NULL)
}


#' Validate a scalar link/family string
#' @noRd
.validate_link_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a single non-missing string.", name),
         call. = FALSE)
  }
  x
}


#' Validate a numeric vector
#' @noRd
.validate_numeric_vector <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric.", name), call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate positive finite numeric input
#' @noRd
.validate_positive_numeric <- function(x, name) {
  if (!is.numeric(x) || length(x) == 0L ||
        any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf("`%s` must contain positive finite values.", name),
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Bound probabilities to the open unit interval
#' @noRd
.bound_unit_interval <- function(p) {
  eps <- .Machine$double.eps
  pmin(pmax(p, eps), 1 - eps)
}
