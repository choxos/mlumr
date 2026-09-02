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
                   log.p = FALSE, ..., mean, sd) {
  .reject_gamma_dots(...)
  gp <- .gamma_moment_pars(missing(mean), missing(sd),
                           if (missing(mean)) NULL else mean,
                           if (missing(sd)) NULL else sd)
  if (!is.null(gp)) {
    return(stats::qgamma(p, shape = gp$shape, rate = gp$rate,
                         lower.tail = lower.tail, log.p = log.p))
  }
  .reject_rate_and_scale(missing(rate), missing(scale))
  stats::qgamma(p, shape = shape, scale = scale,
                lower.tail = lower.tail, log.p = log.p)
}

#' @rdname GammaDist
#' @export
pgamma <- function(q, shape, rate = 1, scale = 1 / rate, lower.tail = TRUE,
                   log.p = FALSE, ..., mean, sd) {
  .reject_gamma_dots(...)
  gp <- .gamma_moment_pars(missing(mean), missing(sd),
                           if (missing(mean)) NULL else mean,
                           if (missing(sd)) NULL else sd)
  if (!is.null(gp)) {
    return(stats::pgamma(q, shape = gp$shape, rate = gp$rate,
                         lower.tail = lower.tail, log.p = log.p))
  }
  .reject_rate_and_scale(missing(rate), missing(scale))
  stats::pgamma(q, shape = shape, scale = scale,
                lower.tail = lower.tail, log.p = log.p)
}

#' @rdname GammaDist
#' @export
dgamma <- function(x, shape, rate = 1, scale = 1 / rate, log = FALSE,
                   ..., mean, sd) {
  .reject_gamma_dots(...)
  gp <- .gamma_moment_pars(missing(mean), missing(sd),
                           if (missing(mean)) NULL else mean,
                           if (missing(sd)) NULL else sd)
  if (!is.null(gp)) {
    return(stats::dgamma(x, shape = gp$shape, rate = gp$rate, log = log))
  }
  .reject_rate_and_scale(missing(rate), missing(scale))
  stats::dgamma(x, shape = shape, scale = scale, log = log)
}

#' Gamma shape and rate from a mean and a standard deviation
#'
#' Returns `NULL` when neither moment was supplied, so the caller falls through
#' to the native parameterization.
#'
#' @param no_mean,no_sd Whether the caller's `mean` / `sd` were missing.
#' @param mean,sd The supplied moments, or `NULL`.
#' @return A list with `shape` and `rate`, or `NULL`.
#' @keywords internal
.gamma_moment_pars <- function(no_mean, no_sd, mean, sd) {
  if (no_mean && no_sd) return(NULL)
  # Half a moment specification is a mistake, not a parameterization. Without
  # this, `dgamma(x, shape = 2, mean = 5)` silently returned the shape-2
  # distribution and ignored the mean, and `qgamma(p, mean = 5)` failed with
  # R's "argument \"shape\" is missing" rather than saying what was wrong.
  if (no_mean || no_sd) {
    stop("The gamma moment parameterization needs both `mean` and `sd`. ",
         "Supply the other one, or give `shape` and `rate` / `scale`.",
         call. = FALSE)
  }
  if (!is.numeric(mean) || !is.numeric(sd)) {
    stop("Gamma `mean` and `sd` must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(mean) | mean <= 0)) {
    stop("Gamma `mean` must be finite and strictly positive.", call. = FALSE)
  }
  # Both conversions square the SD, so a negative one used to pass silently:
  # `sd = -2` returned exactly the `sd = 2` distribution.
  if (any(!is.finite(sd) | sd <= 0)) {
    stop("Gamma `sd` must be finite and strictly positive.", call. = FALSE)
  }
  # `mean / sd^2` overflows as soon as `sd^2` does, which happens from
  # sd = 1.4e154 even where the rate itself is perfectly representable:
  # mean = sd = 1e200 gave rate 0 and a median of Inf instead of 6.93e199.
  # Dividing twice keeps every intermediate on the scale of the answer.
  ratio <- mean / sd
  list(shape = ratio^2, rate = ratio / sd)
}

#' Refuse an argument these wrappers do not have
#'
#' `mean` and `sd` sit behind `...` so that they cannot take part in partial
#' matching. Without that, adding an `sd` formal made `s` ambiguous between
#' `scale` and `sd`, and `qgamma(p, shape = 2, s = 4)`, which \pkg{stats}
#' accepts as `scale`, failed with "argument 3 matches multiple formal
#' arguments". Only formals declared before `...` are matched partially, so
#' moving the pair behind it restores the abbreviation and costs only this
#' check, which keeps `...` from silently swallowing a typo the way
#' \pkg{stats} would not.
#' @param ... Must be empty.
#' @keywords internal
.reject_gamma_dots <- function(...) {
  nm <- names(list(...))
  if (length(nm) || ...length() > 0L) {
    lbl <- if (length(nm)) paste(nm[nzchar(nm)], collapse = ", ") else ""
    stop(sprintf(paste0("unused argument%s%s. `mean` and `sd` must be given ",
                        "by their full names."),
                 if (...length() > 1L) "s" else "",
                 if (nzchar(lbl)) paste0(" (", lbl, ")") else ""),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Refuse a conflicting `rate` and `scale`
#'
#' The `scale = 1 / rate` default makes both arguments look supplied to
#' \pkg{stats}, which would otherwise reject the pair. Forwarding only `scale`
#' silently resolved the conflict in its favor.
#' @param no_rate,no_scale Whether the caller's `rate` / `scale` were missing.
#' @keywords internal
.reject_rate_and_scale <- function(no_rate, no_scale) {
  if (!no_rate && !no_scale) {
    stop("specify 'rate' or 'scale' but not both", call. = FALSE)
  }
  invisible(TRUE)
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
#' @param log Return the log density. Positional, as in [stats::dnorm()].
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
dlogitnorm <- function(x, mu = 0, sigma = 1, log = FALSE, ..., mean, sd) {
  pars <- .logitnorm_pars(mu, sigma, if (missing(mean)) NULL else mean,
                          if (missing(sd)) NULL else sd,
                          !missing(mean), !missing(sd))
  # The Jacobian form dnorm(qlogis(x)) / (x (1 - x)) is 0/0 at the support
  # boundaries, and on the log scale -Inf - (-Inf); both evaluate to NaN in
  # floating point although the density there is simply zero. Outside [0, 1]
  # qlogis() is NaN with a warning. Evaluate the formula only on the open
  # interval and fill the rest in with the value the density actually takes.
  #
  # Recycle first. Subsetting `x` to the interior while leaving `mu` and
  # `sigma` at full length silently paired each interior point with the wrong
  # parameter: dlogitnorm(c(0, 0.5), mu = c(0, 10), sigma = 1) evaluated the
  # single interior point against mu = 0 and returned 1.596 for a density of
  # 3.08e-22.
  x <- as.numeric(x)
  mu_v <- as.numeric(pars[["mu"]])
  sigma_v <- as.numeric(pars[["sigma"]])
  n <- max(length(x), length(mu_v), length(sigma_v))
  if (n == 0L || !length(x) || !length(mu_v) || !length(sigma_v)) {
    return(numeric(0))
  }
  x <- rep_len(x, n)
  mu_v <- rep_len(mu_v, n)
  sigma_v <- rep_len(sigma_v, n)

  is_log <- isTRUE(log)
  out <- rep(if (is_log) -Inf else 0, n)
  # An unusable parameter is not a point outside the support: the density is
  # unknown there, not zero. dlogitnorm(0, mu = NA) used to return 0.
  bad <- !is.finite(mu_v) | !is.finite(sigma_v) | sigma_v < 0
  inside <- !is.na(x) & x > 0 & x < 1 & !bad
  if (any(inside)) {
    ld <- stats::dnorm(stats::qlogis(x[inside]), mean = mu_v[inside],
                       sd = sigma_v[inside], log = TRUE) -
      base::log(x[inside]) - log1p(-x[inside])
    out[inside] <- if (is_log) ld else exp(ld)
  }
  out[bad] <- NA_real_
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
#' Integrates over the latent normal variable rather than over `x` on `(0, 1)`.
#' On the `x` scale a concentrated margin is a narrow spike inside the unit
#' interval and adaptive quadrature steps over it: across a sweep of
#' `(mu, sigma)` the `x`-scale rule was wrong by up to 100 percent and failed
#' outright on several, while for `mean = 0.01` and `sd = 0.0011` it reported an
#' SD of 0.00110 where the true value is 0.00156. Because the objective below
#' and its acceptance check both used that rule, the solver certified a
#' distribution whose SD it had never matched, and [add_integration()] then drew
#' points from it.
#'
#' Two details make the latent form exact rather than merely better. The
#' integrand's only sharp feature is the logistic transition, so the range is
#' split at `z0 = -mu / sigma`, which puts a panel boundary exactly on it; `z0`
#' is clamped to the range where the normal weight has any mass, since beyond
#' that the split would leave the mass in one huge panel. And `abs.tol` is set
#' to zero: the variance integral is around 1e-11 for a concentrated margin,
#' far under the default absolute tolerance of 1.2e-4, so the default rule
#' returned its first crude estimate and declared success.
#'
#' Returns `NULL` rather than erroring when the quadrature fails, so the
#' objective below can penalize an infeasible region instead of aborting the
#' search.
#' @keywords internal
.ln_moments <- function(mu, sigma) {
  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) return(NULL)
  z0 <- max(-8, min(8, -mu / sigma))
  int <- function(f) {
    stats::integrate(f, -Inf, z0, rel.tol = 1e-11, abs.tol = 0)$value +
      stats::integrate(f, z0, Inf, rel.tol = 1e-11, abs.tol = 0)$value
  }
  g <- function(z) stats::plogis(mu + sigma * z)
  out <- tryCatch({
    m <- int(function(z) g(z) * stats::dnorm(z))
    v <- int(function(z) (g(z) - m)^2 * stats::dnorm(z))
    if (!is.finite(m) || !is.finite(v) || v < 0) {
      NULL
    } else {
      c(mean = m, sd = sqrt(v))
    }
  }, error = function(e) NULL)
  out
}

#' Squared relative distance between a logit-normal's moments and a target
#'
#' `est` is `(mu, log sigma)`. Optimizing the log keeps `sigma` strictly
#' positive without a constrained optimizer; an unconstrained search over
#' `sigma` itself can step to a negative scale, where the density is `NaN` and
#' the objective is meaningless.
#'
#' The residuals are divided by their targets. An absolute objective is
#' meaningless for a small margin: at `mean = 0.0005` a solution three times
#' too large scores 1e-6, which any convergence rule reads as a fit.
#' @keywords internal
.lndiff <- function(est, m, s) {
  mom <- .ln_moments(est[[1L]], exp(est[[2L]]))
  if (is.null(mom)) return(.Machine$double.xmax^0.5)
  ((mom[["mean"]] - m) / m)^2 + ((mom[["sd"]] - s) / s)^2
}

#' Solve for one logit-normal (mu, sigma) from a mean and SD
#'
#' Starts from the delta-method approximation on the logit scale rather than
#' from the target moments themselves, which live on a different scale and make
#' a poor starting point, and verifies that the recovered moments actually
#' reproduce the target before returning.
#'
#' Nelder-Mead reports convergence when its simplex has collapsed, which on
#' this objective happens well short of the target: from a single pass, 27 of
#' 77 mean and SD pairs spanning the feasible region were still off by more
#' than 1e-6, the worst by 6e-3. Restarting rebuilds the simplex around the
#' current point, so the search is repeated until a restart no longer improves
#' the objective. That leaves every one of the 77 within 2e-12, for a mean of
#' under four passes.
#'
#' `tol` is relative to each target moment. It was absolute at 1e-3, which for
#' `mean = 0.0005, sd = 0.0004` is larger than either target, so the check
#' accepted a solution with mean 0.00147 and SD 0.00139 in silence.
#' @keywords internal
.lnopt <- function(m, s, tol = 1e-4) {
  par <- c(stats::qlogis(m), log(s / (m * (1 - m))))
  prev <- Inf
  bad <- TRUE
  for (k in seq_len(8L)) {
    opt <- stats::optim(par, .lndiff, m = m, s = s,
                        control = list(reltol = 1e-12, maxit = 2000L))
    par <- opt$par
    bad <- opt$convergence != 0
    if (opt$value >= prev * (1 - 1e-6)) break
    prev <- opt$value
  }
  pars <- c(mu = par[[1L]], sigma = exp(par[[2L]]))
  if (!bad) {
    mom <- .ln_moments(pars[["mu"]], pars[["sigma"]])
    # Convergence of the optimizer is not the same as having hit the target: a
    # flat or penalized region can converge far from it. Check the moments the
    # solution actually implies.
    bad <- is.null(mom) ||
      abs(mom[["mean"]] - m) > tol * m ||
      abs(mom[["sd"]] - s) > tol * s
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
  if (!length(m) || !length(s)) return(as.data.frame(list(mu = numeric(0),
                                                          sigma = numeric(0))))
  # Recycle before validating rather than after. The feasibility test below
  # compares s^2 against m * (1 - m) elementwise, which recycles a scalar `sd`
  # on its own, and then reported the offending pair with the index that test
  # produced: qlogitnorm(0.5, mean = c(0.5, 0.01), sd = 0.3) flagged pair 2 and
  # died on s[[2]] with "subscript out of bounds" instead of saying which mean
  # and SD were impossible together.
  n <- max(length(m), length(s))
  m <- rep_len(m, n)
  s <- rep_len(s, n)
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
