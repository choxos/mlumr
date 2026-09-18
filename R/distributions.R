# Moment-parameterized marginal distributions for add_integration(), as in
# multinma: the stats:: functions are shadowed by versions that also accept
# `mean` and `sd`, which override the native parameters when both are given.

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
#'   `rate` / `scale` when both are supplied. Both must be named in full.
#' @param ... Must be empty; it keeps `mean` and `sd` out of partial matching.
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
  if (any(!is.finite(sd) | sd <= 0)) {
    stop("Gamma `sd` must be finite and strictly positive.", call. = FALSE)
  }
  # Dividing twice avoids overflow in sd^2.
  ratio <- mean / sd
  list(shape = ratio^2, rate = ratio / sd)
}

#' Refuse an argument these wrappers do not have
#'
#' `mean` and `sd` sit behind `...` so they cannot take part in partial
#' matching; this check keeps `...` from swallowing a typo.
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

#' Refuse a conflicting `rate` and `scale`, as \pkg{stats} does
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
#' @param ... For `plogitnorm()` and `qlogitnorm()`, passed to the underlying
#'   \pkg{stats} normal function ([stats::pnorm()], [stats::qnorm()]), so
#'   `lower.tail` and `log.p` work as usual. `dlogitnorm()` builds its density
#'   from [stats::dnorm()] and a Jacobian rather than delegating, so it has
#'   nothing to forward and refuses anything passed here; in its signature
#'   `...` serves only to keep `mean` and `sd` from matching positionally.
#' @param mean,sd Mean and standard deviation on the `(0, 1)` scale,
#'   overriding `mu` and `sigma` when both are supplied.
#'
#' @return A numeric vector.
#' @seealso [distr()], [add_integration()], [GammaDist]
#' @name logitNormal
#' @examples
#' qlogitnorm(0.5, mean = 0.34, sd = 0.19)
NULL

#' Refuse arguments a function has no use for
#'
#' A function that computes its own answer would otherwise discard whatever
#' `...` collected, so a misspelled argument would read as a default.
#' @keywords internal
.reject_unused_dots <- function(...) {
  n <- ...length()
  if (n == 0L) {
    return(invisible(NULL))
  }
  nms <- ...names()
  labels <- vapply(seq_len(n), function(i) {
    if (!is.null(nms) && !is.na(nms[[i]]) && nzchar(nms[[i]])) {
      nms[[i]]
    } else {
      paste0("[[", i, "]] (unnamed)")
    }
  }, character(1))
  stop(sprintf("unused argument%s: %s", if (n > 1L) "s" else "",
               paste(labels, collapse = ", ")), call. = FALSE)
}

#' @rdname logitNormal
#' @export
dlogitnorm <- function(x, mu = 0, sigma = 1, log = FALSE, ..., mean, sd) {
  pars <- .logitnorm_pars(mu, sigma, if (missing(mean)) NULL else mean,
                          if (missing(sd)) NULL else sd,
                          !missing(mean), !missing(sd))
  # Nothing is delegated here, so `...` must be empty.
  .reject_unused_dots(...)
  # The Jacobian form is 0/0 at the support boundaries, so evaluate it on the
  # open interval only and fill in the zero density elsewhere. Recycle before
  # subsetting so each point keeps its own parameters.
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

  # Coerce `log` as the stats functions do.
  if (length(log) != 1L || is.na(as.logical(log))) {
    stop("`log` must be a single non-missing value coercible to TRUE or FALSE.",
         call. = FALSE)
  }
  is_log <- as.logical(log)
  out <- rep(if (is_log) -Inf else 0, n)
  # An unusable parameter gives NA, not zero.
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
  # Clamping to [0, 1] gives 0 below the support and 1 above it under every
  # `lower.tail` and `log.p` combination.
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
#' Integrates over the latent normal variable, splitting the range at
#' `z0 = -mu / sigma`, clamped between -8 and 8, where the logistic transition sits, with `abs.tol = 0`
#' because the variance of a concentrated margin is far below the default
#' absolute tolerance. Returns `NULL` when the quadrature fails.
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
#' `est` is `(mu, log sigma)`; the residuals are relative to their targets so
#' a small margin is judged on its own scale.
#' @keywords internal
.lndiff <- function(est, m, s) {
  mom <- .ln_moments(est[[1L]], exp(est[[2L]]))
  if (is.null(mom)) return(.Machine$double.xmax^0.5)
  ((mom[["mean"]] - m) / m)^2 + ((mom[["sd"]] - s) / s)^2
}

#' Solve for one logit-normal (mu, sigma) from a mean and SD
#'
#' Starts from the delta-method approximation on the logit scale, restarts
#' Nelder-Mead, at most eight attempts, until a restart no longer improves the objective, and checks
#' that the recovered moments reproduce the target to within `tol` (relative).
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
    # Convergence is not the same as having hit the target.
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
#' A variable on `(0, 1)` has `Var(X) < mean * (1 - mean)`, so an impossible
#' pair is refused before the optimizer is asked.
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
  # Recycle before validating, so the offending pair can be named.
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
                        "sd must be under sqrt(mean * (1 - mean)) = %g."),
                 s[[i]], m[[i]], sqrt(m[[i]] * (1 - m[[i]]))), call. = FALSE)
  }
  as.data.frame(do.call(rbind, mapply(.lnopt, m, s, SIMPLIFY = FALSE)))
}

#' Resolve logit-normal parameters from either parameterization
#' @keywords internal
.logitnorm_pars <- function(mu, sigma, mean, sd, has_mean, has_sd) {
  if (has_mean && has_sd) return(.pars_logitnorm(mean, sd))
  if (has_mean || has_sd) {
    stop("The logit-normal moment parameterization needs both `mean` and ",
         "`sd`. Supply the other one, or give `mu` and `sigma` on the logit ",
         "scale.", call. = FALSE)
  }
  .validate_logitnorm_native(mu, sigma)
}

#' Validate native logit-normal `mu` / `sigma`
#' @keywords internal
.validate_logitnorm_native <- function(mu, sigma) {
  # Test missingness before type, since a bare NA is logical.
  if (anyNA(mu) || anyNA(sigma)) {
    stop("logit-normal `mu` and `sigma` must not be missing.", call. = FALSE)
  }
  if (!is.numeric(mu) || !is.numeric(sigma)) {
    stop("logit-normal `mu` and `sigma` must be numeric.", call. = FALSE)
  }
  if (!length(mu) || !length(sigma)) return(list(mu = mu, sigma = sigma))
  if (any(!is.finite(mu))) {
    stop("logit-normal `mu` must be finite.", call. = FALSE)
  }
  if (any(!is.finite(sigma))) {
    stop("logit-normal `sigma` must be finite.", call. = FALSE)
  }
  if (any(sigma <= 0)) {
    stop("logit-normal `sigma` must be strictly positive.", call. = FALSE)
  }
  # Partial recycling pairs values with the wrong parameter.
  if (length(mu) != length(sigma) && length(mu) > 1L && length(sigma) > 1L) {
    stop("logit-normal `mu` and `sigma` must be the same length, or one of ",
         "them a single value.", call. = FALSE)
  }
  list(mu = mu, sigma = sigma)
}
