# Moment-parameterized marginal distributions for add_integration().
#
# Published baseline tables report a mean and a standard deviation, not a shape
# and a rate. multinma solves this by shadowing the relevant stats:: quantile
# functions with versions that also accept `mean` / `sd` and reparameterize
# internally, so `distr(qgamma, mean = age_mean, sd = age_sd)` works directly
# off the numbers in the paper. mlumr mirrors that, both because the public API
# deliberately tracks multinma's and because the alternative is asking users to
# convert moments by hand at every call site.
#
# When both `mean` and `sd` are supplied they override the native parameters.
# Otherwise these forward to stats:: unchanged, so they are drop-in safe.

#' The Gamma distribution, parameterized by mean and standard deviation
#'
#' Density, distribution, and quantile functions for the gamma distribution,
#' accepting either the native `shape` / `rate` (or `scale`) parameters or a
#' `mean` and `sd`, which override them. Useful with [distr()] and
#' [add_integration()], where covariate moments come straight from a published
#' baseline table. The moment parameterization uses
#' `shape = (mean / sd)^2` and `rate = mean / sd^2`.
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param shape,rate,scale See [stats::GammaDist].
#' @param lower.tail,log.p,log See [stats::GammaDist].
#' @param mean,sd Mean and standard deviation, overriding `shape` and
#'   `rate` / `scale` when both are supplied.
#'
#' @return A numeric vector, as the corresponding \pkg{stats} function.
#' @seealso [distr()], [add_integration()], [qbern()]
#' @name GammaDist
#' @examples
#' # Equivalent specifications
#' qgamma(0.5, mean = 65, sd = 8)
#' qgamma(0.5, shape = (65 / 8)^2, rate = 65 / 8^2)
NULL

#' @rdname GammaDist
#' @export
qgamma <- function(p, shape, rate = 1, scale = 1 / rate, lower.tail = TRUE,
                   log.p = FALSE, mean, sd) {
  if (!missing(mean) && !missing(sd)) {
    return(stats::qgamma(p, shape = (mean / sd)^2, rate = mean / sd^2,
                         lower.tail = lower.tail, log.p = log.p))
  }
  stats::qgamma(p, shape = shape, scale = scale,
                lower.tail = lower.tail, log.p = log.p)
}

#' @rdname GammaDist
#' @export
pgamma <- function(q, shape, rate = 1, scale = 1 / rate, lower.tail = TRUE,
                   log.p = FALSE, mean, sd) {
  if (!missing(mean) && !missing(sd)) {
    return(stats::pgamma(q, shape = (mean / sd)^2, rate = mean / sd^2,
                         lower.tail = lower.tail, log.p = log.p))
  }
  stats::pgamma(q, shape = shape, scale = scale,
                lower.tail = lower.tail, log.p = log.p)
}

#' @rdname GammaDist
#' @export
dgamma <- function(x, shape, rate = 1, scale = 1 / rate, log = FALSE,
                   mean, sd) {
  if (!missing(mean) && !missing(sd)) {
    return(stats::dgamma(x, shape = (mean / sd)^2, rate = mean / sd^2,
                         log = log))
  }
  stats::dgamma(x, shape = shape, scale = scale, log = log)
}


#' The logit-Normal distribution
#'
#' Density, distribution, and quantile functions for the logit-normal
#' distribution: the distribution of `plogis(z)` where `z` is normal with mean
#' `mu` and standard deviation `sigma`. It is the natural marginal for a
#' covariate reported as a proportion on `(0, 1)`, such as percent body surface
#' area.
#'
#' For convenience the distribution may be given by its `mean` and `sd` on the
#' natural `(0, 1)` scale instead of `mu` and `sigma` on the logit scale. There
#' is no closed form for that reparameterization, so `mu` and `sigma` are found
#' numerically; supply `mu` / `sigma` directly if you have them.
#'
#' @param x,q Vector of quantiles, in `(0, 1)`.
#' @param p Vector of probabilities.
#' @param mu,sigma Location and scale, on the logit scale.
#' @param ... Passed to the underlying \pkg{stats} normal function.
#' @param mean,sd Mean and standard deviation on the `(0, 1)` scale,
#'   overriding `mu` and `sigma` when both are supplied.
#'
#' @return A numeric vector.
#' @seealso [distr()], [add_integration()], [GammaDist]
#' @name logitNormal
#' @examples
#' qlogitnorm(0.5, mean = 0.34, sd = 0.19)
NULL

#' @rdname logitNormal
#' @export
dlogitnorm <- function(x, mu = 0, sigma = 1, ..., mean, sd) {
  is_log <- isTRUE(list(...)$log)
  pars <- .logitnorm_pars(mu, sigma, if (missing(mean)) NULL else mean,
                          if (missing(sd)) NULL else sd,
                          !missing(mean), !missing(sd))
  # The Jacobian form dnorm(qlogis(x)) / (x (1 - x)) is 0/0 at the support
  # boundaries, and on the log scale -Inf - (-Inf); both evaluate to NaN in
  # floating point although the density there is simply zero. Outside [0, 1]
  # qlogis() is NaN with a warning. Evaluate the formula only on the open
  # interval and fill the rest in with the value the density actually takes.
  x <- as.numeric(x)
  out <- rep(if (is_log) -Inf else 0, length(x))
  inside <- !is.na(x) & x > 0 & x < 1
  if (any(inside)) {
    ld <- stats::dnorm(stats::qlogis(x[inside]), mean = pars[["mu"]],
                       sd = pars[["sigma"]], log = TRUE) -
      log(x[inside]) - log1p(-x[inside])
    out[inside] <- if (is_log) ld else exp(ld)
  }
  out[is.na(x)] <- x[is.na(x)]   # keeps NA as NA and NaN as NaN
  out
}

#' @rdname logitNormal
#' @export
plogitnorm <- function(q, mu = 0, sigma = 1, ..., mean, sd) {
  pars <- .logitnorm_pars(mu, sigma, if (missing(mean)) NULL else mean,
                          if (missing(sd)) NULL else sd,
                          !missing(mean), !missing(sd))
  # qlogis() is NaN with a warning outside [0, 1], but the distribution function
  # is defined everywhere: 0 below the support and 1 above it. Clamping to the
  # boundary gives exactly that, because qlogis(0) / qlogis(1) are -Inf / Inf
  # and pnorm() maps them to 0 / 1 under every `lower.tail` and `log.p`
  # combination, so the `...` semantics are preserved rather than special-cased.
  q <- as.numeric(q)
  finite <- !is.na(q)
  q[finite] <- pmin(pmax(q[finite], 0), 1)
  stats::pnorm(stats::qlogis(q), mean = pars[["mu"]], sd = pars[["sigma"]], ...)
}

#' @rdname logitNormal
#' @export
qlogitnorm <- function(p, mu = 0, sigma = 1, ..., mean, sd) {
  pars <- .logitnorm_pars(mu, sigma, if (missing(mean)) NULL else mean,
                          if (missing(sd)) NULL else sd,
                          !missing(mean), !missing(sd))
  stats::plogis(stats::qnorm(p, mean = pars[["mu"]], sd = pars[["sigma"]], ...))
}

#' Moments of a logit-normal, by numerical integration
#'
#' Returns `NULL` rather than erroring when the quadrature fails, so the
#' objective below can penalize an infeasible region instead of aborting the
#' search.
#' @keywords internal
.ln_moments <- function(mu, sigma) {
  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) return(NULL)
  dens <- function(x) dlogitnorm(x, mu = mu, sigma = sigma)
  out <- tryCatch({
    m <- stats::integrate(function(x) x * dens(x), 0, 1)$value
    v <- stats::integrate(function(x) (x - m)^2 * dens(x), 0, 1)$value
    if (!is.finite(m) || !is.finite(v) || v < 0) NULL else c(mean = m, sd = sqrt(v))
  }, error = function(e) NULL)
  out
}

#' Squared distance between a logit-normal's moments and a target
#'
#' `est` is `(mu, log sigma)`. Optimizing the log keeps `sigma` strictly
#' positive without a constrained optimizer; an unconstrained search over
#' `sigma` itself can step to a negative scale, where the density is `NaN` and
#' the objective is meaningless.
#' @keywords internal
.lndiff <- function(est, m, s) {
  mom <- .ln_moments(est[[1L]], exp(est[[2L]]))
  if (is.null(mom)) return(.Machine$double.xmax^0.5)
  (mom[["mean"]] - m)^2 + (mom[["sd"]] - s)^2
}

#' Solve for one logit-normal (mu, sigma) from a mean and SD
#'
#' Starts from the delta-method approximation on the logit scale rather than
#' from the target moments themselves, which live on a different scale and make
#' a poor starting point, and verifies that the recovered moments actually
#' reproduce the target before returning.
#' @keywords internal
.lnopt <- function(m, s, tol = 1e-3) {
  start <- c(stats::qlogis(m), log(s / (m * (1 - m))))
  opt <- stats::optim(start, .lndiff, m = m, s = s,
                      control = list(reltol = 1e-10, maxit = 1000L))
  pars <- c(mu = opt$par[[1L]], sigma = exp(opt$par[[2L]]))
  bad <- opt$convergence != 0
  if (!bad) {
    mom <- .ln_moments(pars[["mu"]], pars[["sigma"]])
    # Convergence of the optimizer is not the same as having hit the target: a
    # flat or penalized region can converge far from it. Check the moments the
    # solution actually implies.
    bad <- is.null(mom) ||
      abs(mom[["mean"]] - m) > tol || abs(mom[["sd"]] - s) > tol
  }
  if (bad) {
    warning(sprintf(paste0("logit-normal moment matching failed for mean = %g, ",
                           "sd = %g; NAs produced. Supply `mu` and `sigma` on ",
                           "the logit scale instead."), m, s), call. = FALSE)
    return(c(mu = NA_real_, sigma = NA_real_))
  }
  pars
}

#' Estimate logit-normal mu / sigma from a mean and SD on (0, 1)
#'
#' The feasibility check is not decoration. A variable supported on `(0, 1)`
#' has `Var(X) <= mean * (1 - mean)`, with equality only for a two-point
#' distribution on the boundaries, which no logit-normal can represent. Given an
#' impossible pair the optimizer still returns something, so without this the
#' caller would silently integrate over a distribution that has neither the
#' requested mean nor the requested SD.
#' @keywords internal
.pars_logitnorm <- function(m, s) {
  if (length(m) != length(s) && length(m) > 1 && length(s) > 1) {
    stop("`mean` and `sd` must be the same length.", call. = FALSE)
  }
  if (!is.numeric(m) || !is.numeric(s) || any(!is.finite(m)) ||
        any(!is.finite(s))) {
    stop("logit-normal `mean` and `sd` must be finite numbers.", call. = FALSE)
  }
  # Open interval: the logit of 0 or 1 is infinite, so a boundary mean has no
  # logit-normal representation at all.
  if (any(m <= 0 | m >= 1)) {
    stop("logit-normal `mean` must be strictly inside (0, 1). Have you ",
         "rescaled a percentage?", call. = FALSE)
  }
  if (any(s <= 0)) {
    stop("logit-normal `sd` must be strictly positive.", call. = FALSE)
  }
  infeasible <- s^2 >= m * (1 - m)
  if (any(infeasible)) {
    i <- which(infeasible)[[1L]]
    stop(sprintf(paste0("logit-normal `sd` = %g is impossible for `mean` = %g: ",
                        "a variable on (0, 1) has variance below ",
                        "mean * (1 - mean) = %g, so sd must be under %g. ",
                        "Check whether the reported spread is a standard ",
                        "deviation or a standard error."),
                 s[[i]], m[[i]], m[[i]] * (1 - m[[i]]),
                 sqrt(m[[i]] * (1 - m[[i]]))), call. = FALSE)
  }
  as.data.frame(do.call(rbind, mapply(.lnopt, m, s, SIMPLIFY = FALSE)))
}

#' Resolve logit-normal parameters from either parameterization
#'
#' Supplying only one of `mean` / `sd` used to fall through to the `mu` / `sigma`
#' defaults, so `distr(qlogitnorm, mean = bsa_mean)` silently integrated over a
#' standard logit-normal instead of the requested distribution. Half a moment
#' specification is a mistake, not a parameterization.
#' @keywords internal
.logitnorm_pars <- function(mu, sigma, mean, sd, has_mean, has_sd) {
  if (has_mean && has_sd) return(.pars_logitnorm(mean, sd))
  if (has_mean || has_sd) {
    stop("The logit-normal moment parameterization needs both `mean` and ",
         "`sd`. Supply the other one, or give `mu` and `sigma` on the logit ",
         "scale.", call. = FALSE)
  }
  list(mu = mu, sigma = sigma)
}
