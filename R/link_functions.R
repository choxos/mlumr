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
    continuous = "normal"
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

#' Bound probabilities away from 0 and 1
#' @keywords internal
bound_probability <- function(p, n, min_count = 0.5) {
  .validate_numeric_vector(p, "p")
  .validate_positive_numeric(n, "n")
  .validate_positive_numeric(min_count, "min_count")
  if (any(2 * min_count > n)) {
    stop("`min_count` must be no larger than n / 2.", call. = FALSE)
  }
  pmin(pmax(p, min_count / n), 1 - min_count / n)
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
