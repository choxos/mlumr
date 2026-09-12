#' Residual and numerical-zero ratios, on a scale where squares cannot overflow
#'
#' Everything is divided by one common magnitude before being squared, so the
#' ratios are unchanged while the sums stay in range.
#'
#' `zero_ratio` is what the computed residual can be when the true one is zero.
#' For `r = y - fl(X b)` the elementwise rounding is bounded by
#' `p * eps * (|X| |b|)`, which is small when the fitted coefficients are small
#' and large when they are not. That is an UPPER bound on rounding, and it is
#' used only in that direction: a residual above it is certainly real, while a
#' residual at or below it is undecided, not proven zero. Coefficients dropped
#' as redundant are `NA` and contribute nothing, so a constant covariate needs
#' no special handling.
#'
#' @param X Design matrix.
#' @param y Outcome.
#' @param mu_hat Fitted values.
#' @param b Fitted coefficients; `NA` for columns dropped as redundant.
#' @param rank Fitted rank.
#' @param mu Fitted values for a nonlinear link, or `NULL`.
#' @return List with `ratio` (residual sum of squares over the total sum of
#'   squares), `zero_ratio` (the rounding bound on the same scale) and `rank`.
#' @keywords internal
.fit_ratios <- function(X, y, mu_hat, b, rank, mu = NULL) {
  m <- max(abs(y), abs(mu_hat))
  if (!is.finite(m) || m == 0) {
    m <- 1
  }
  tss <- sum(((y - mean(y)) / m)^2)
  b[is.na(b)] <- 0
  bound <- ncol(X) * .Machine$double.eps * (abs(X) %*% abs(b))
  if (!is.null(mu)) {
    bound <- bound * mu
  }
  list(ratio = sum(((y - mu_hat) / m)^2) / tss,
       zero_ratio = sum((bound / m)^2) / tss,
       rank = rank)
}


#' Least-squares residual ratios of a linear fit
#'
#' `lm.fit()`'s default pivot tolerance of `1e-7` can drop a column that is
#' nearly but not exactly collinear with another, measuring the residual
#' against a design smaller than the one that will be fitted, so it is lowered
#' to the floor. A linear fit of finite data always returns, so this cannot
#' fail to give a verdict.
#'
#' @param X Design matrix, intercept included.
#' @param y Outcome vector.
#' @return See [.fit_ratios()].
#' @keywords internal
.linear_fit_ratios <- function(X, y) {
  fit <- stats::lm.fit(X, y, tol = .Machine$double.eps)
  .fit_ratios(X, y, y - fit$residuals, fit$coefficients, fit$rank)
}


#' Response-scale residual ratio of a log-link normal fit
#'
#' The likelihood under `link = "log"` is `normal(exp(theta), sigma)`, so the
#' residual that informs sigma is `y - exp(theta)` on the RESPONSE scale, and
#' that is the one to measure when asking whether it is nearly zero. Whether
#' it is EXACTLY zero is a different question, answered on the log scale by
#' [.check_normal_residual_variation()], so this fit is only a screen and a
#' failure to converge costs nothing but the screen.
#'
#' `glm.fit()` takes its QR pivot tolerance as `min(1e-7, epsilon / 1000)`, so
#' the only way to stop it discarding a nearly collinear column is through
#' `epsilon`. At `1e-13` the pivot tolerance is `1e-16`, matching the linear
#' fit. Whether the iteration then reports convergence is not required: near
#' an exact fit the deviance changes at rounding level from one iterate to the
#' next, and whether that clears the tolerance differs between platforms. The
#' residual of ANY iterate bounds the least-squares minimum from above, so a
#' small one justifies the warning and a large one only withholds it, which is
#' the safe direction for a screen.
#'
#' The fit is a Gaussian log-link IRLS, whose deviance is a sum of squared
#' outcomes, so it cannot run on an outcome spanning more than about 700 log
#' units however it is centered: the squares overflow. The caller centers
#' `log(y)` on its midrange, which keeps both ends finite whenever the span
#' fits at all. Beyond that there is no screen, and no verdict depends on
#' one.
#'
#' @param X Design matrix, intercept included.
#' @param y Non-negative outcome vector. Zeros are allowed when `start` is
#'   given, since the default starting values take `log(y)`.
#' @param start Starting coefficients, or `NULL` for `glm.fit()`'s own.
#' @return The residual ratio, or `NA` when the fit could not run.
#' @keywords internal
.log_link_response_ratio <- function(X, y, start = NULL) {
  fit <- tryCatch(
    suppressWarnings(stats::glm.fit(
      X, y, start = start, family = stats::gaussian("log"),
      control = list(epsilon = 1e-13, maxit = 100, trace = FALSE)
    )),
    error = function(e) NULL
  )
  if (is.null(fit) || !all(is.finite(fit$fitted.values))) {
    return(NA_real_)
  }
  # A perturbation d in the linear predictor moves the fitted value by mu * d,
  # so the bound carries that factor onto the response scale.
  .fit_ratios(X, y, fit$fitted.values, fit$coefficients, fit$rank,
              mu = fit$fitted.values)$ratio
}


#' Classify how much residual variation a normal IPD outcome has
#'
#' The decision core behind [.check_normal_residual_variation()], which turns
#' the status into a refusal, a warning or silence. Kept separate so the
#' mixed-zero log-link case can classify the positive rows on their own.
#'
#' Everything here is about the design the model fits, `X`, whose exact rank
#' and exactly independent pivot columns come from [.exact_rank()]. A
#' factorization at machine precision can find a lower rank, when a column
#' differs from a combination of the others by less than rounding; the model
#' still carries that column, and a fit through it can be exact where the
#' reduced fit shows a residual. So the numerical fit is taken on the exact
#' pivot columns, and if even those cannot be resolved the question is left
#' open rather than answered from a design the model does not fit.
#'
#' Statuses, in the order they are decided:
#'
#' * `"saturated"`: `n <= rank`, the design reproduces any outcome.
#' * `"constant"`: the outcome is constant with residual degrees of freedom,
#'   so the intercept alone fits it exactly.
#' * `"exact"`: every replicate group agrees and there are exactly `rank`
#'   distinct design rows, so the design reaches every observed value.
#' * `"unresolved"`: the exact pivot columns are dependent to within
#'   rounding, so no numerical fit spans the design the model fits.
#' * `"unresolved_log"`: `log(y)` is constant although `y` is not, so the
#'   log-scale total sum of squares is zero and the ratio undefined.
#' * `"undecidable"`: the computed residual is at or below the rounding an
#'   exact fit can leave.
#' * `"near_exact"`: a real residual at most `1e-6` of the total.
#' * `"positive"`: a real residual.
#'
#' @param X Design matrix as the model fits it, intercept included: raw, or
#'   centered by the model's own centers.
#' @param y Outcome vector; positive under `link = "log"`.
#' @param link `"identity"` or `"log"`.
#' @return List with `status` and, where they apply, `n`, `rank`,
#'   `numerical_rank`, `ratio` and `zero_ratio`.
#' @keywords internal
.residual_variation_status <- function(X, y, link) {
  n <- length(y)
  geometry <- .exact_rank(X)
  rank <- geometry$rank
  if (n <= rank) {
    return(list(status = "saturated", n = n, rank = rank))
  }
  if (all(y == y[1])) {
    return(list(status = "constant", n = n, rank = rank))
  }
  replicates <- .design_replicates(X, y)
  if (replicates$consistent && replicates$n_distinct == rank) {
    return(list(status = "exact", n = n, rank = rank))
  }

  # The numerical fit runs on the exact pivot columns, scaled by powers of
  # two, which is exact: the rank and pivot decisions then do not hang on
  # the predictors' units, and a column dropped there is one the model
  # cannot resolve either, not one a units choice hid.
  Xs <- .scale_design(X)[, geometry$pivots, drop = FALSE]

  # Both sums are squares, so an outcome in extreme units breaks the units
  # invariance this test is built on: below about 1e-162 they both underflow to
  # zero and a varying outcome reads as constant, and above about 1e154 they
  # both overflow to Inf and the ratio is NaN. The ratio is what matters, so
  # take it on a normalized outcome, where the largest square is 1.
  #
  # Normalize the VARIATION, not the level. `y = 1e15 + 3 * x` is stored
  # exactly and fitted exactly, but dividing it by its own maximum leaves the
  # whole 147-wide spread inside the last few digits and turns it into noise.
  # Subtracting one of the data values first is exact whenever they share a
  # scale, and with an intercept in the design it shifts only the intercept
  # coefficient, so the residual is untouched.
  if (identical(link, "log")) {
    # Existence of an exact fit is a linear question in log(y). Centering
    # log(y) shifts only the intercept, and keeps the response-scale screen
    # below from underflowing the small end of a wide outcome to zero, which
    # is not a value a log link can start from. The center is the midrange,
    # not the mean: five outcomes near 1e-260 and one near 1e260 have a mean
    # log far below the middle, and centering on it overflows the large one.
    # The midrange keeps both ends finite whenever the span itself fits.
    log_y <- log(y)
    log_y <- log_y - (max(log_y) + min(log_y)) / 2
    fit <- .linear_fit_ratios(Xs, log_y)
    screen <- function() .log_link_response_ratio(Xs, exp(log_y))
  } else {
    # The span itself can overflow: y = c(-1e308, 1e308) makes y - min(y) Inf
    # and sends a non-finite response into the fit. Halving is exact in binary,
    # and the scaling below makes the factor irrelevant.
    y <- if (is.finite(max(y) - min(y))) {
      y - min(y)
    } else {
      y / 2 - min(y) / 2
    }
    scale <- max(abs(y))
    if (is.finite(scale) && scale > 0) {
      y <- y / scale
    }
    fit <- .linear_fit_ratios(Xs, y)
    screen <- function() fit$ratio
  }
  if (fit$rank < rank) {
    # The pivot columns are exactly independent and the factorization still
    # dropped one: it differs from a combination of the others by less than
    # rounding. The residual of the reduced fit says nothing about the fit
    # the model makes through that direction.
    return(list(status = "unresolved", n = n, rank = rank,
                numerical_rank = fit$rank))
  }

  # Only an exactly zero residual is improper. For any positive residual the
  # exp(-RSS / (2 sigma^2)) factor drives the density to zero as sigma does,
  # and the integral converges however small that residual is. The bound is
  # used only as a bound: above it the residual is certainly real, at or
  # below it nothing at double precision decides.
  positive <- !replicates$consistent || isTRUE(fit$ratio > fit$zero_ratio)
  if (!positive && !is.finite(fit$ratio)) {
    # Outcomes near 1e300 that differ by a few units in the last place have
    # identical logarithms, so the log-scale total sum of squares is zero and
    # the ratio is undefined. The variation is real on the response scale, but
    # it is at the resolution of double precision.
    return(list(status = "unresolved_log", n = n, rank = rank))
  }
  if (!positive) {
    return(list(status = "undecidable", n = n, rank = rank,
                ratio = fit$ratio, zero_ratio = fit$zero_ratio))
  }
  ratio <- screen()
  if (is.finite(ratio) && ratio <= 1e-6) {
    return(list(status = "near_exact", n = n, rank = rank, ratio = ratio))
  }
  list(status = "positive", n = n, rank = rank, ratio = ratio)
}


#' Refuse normal IPD that its own covariates fit exactly
#'
#' With `n` IPD rows and a design of rank `r`, integrating out the
#' coefficients leaves a marginal density for sigma proportional to
#' `sigma^(r - n) * exp(-RSS / (2 * sigma^2))`. When `RSS` is zero the
#' likelihood is singular at sigma = 0 rather than flat: the density is
#' `sigma^(r - n)` all the way down, whose integral to zero diverges for
#' every `n > r`, and a proper prior on sigma does not repair it: a prior
#' with positive density at zero leaves the divergence exactly where it was.
#' Proper priors on the coefficients do not either; they scale the density
#' by the prior at the exact solution and leave its shape in sigma. The
#' posterior is improper and nothing reports it. The sampler drifts toward
#' zero and returns whatever it reached, with ordinary-looking diagnostics.
#'
#' The question is whether an exact fit exists in the design the model
#' fits, and [.residual_variation_status()] settles it in this order,
#' stopping at the first that decides:
#'
#' * A saturated design (`n <= rank`) reproduces any outcome, and its posterior
#'   is proper: the marginal density for sigma is bounded at zero and falls as
#'   `sigma^(-n)` far out. Nothing in the data separates the residual SD from
#'   the coefficients there, so what is reported for sigma is potentially
#'   strongly sensitive to the coefficient priors. That warns.
#' * A constant outcome with `n > rank` is reproduced by the intercept alone.
#'   The exact fit is certain, and so is the impropriety. Refused.
#' * Replicate design rows carrying different outcomes prove the residual
#'   positive whatever the fit, since identical rows get identical fitted
#'   values. The posterior is proper.
#' * When every replicate group agrees and there are exactly `rank` distinct
#'   rows, the design reaches every outcome on those rows, so the fit is exact
#'   and the posterior improper. Refused.
#' * Otherwise the computed residual is compared with the rounding an exact fit
#'   can leave, `p * eps * |X||b|` elementwise. Above it the residual is
#'   certainly real and the posterior proper. At or below it nothing at double
#'   precision tells an exact fit from one this close, and Stan computes the
#'   same likelihood at the same precision, so the model is refused as
#'   undecidable rather than passed as proper.
#'
#' The rank throughout is the EXACT rank of the design, from
#' [.exact_rank()], and the structural rules read the rows the model fits.
#' A factorization at machine precision can find fewer independent columns
#' than the design has, when one differs from a combination of the others
#' by less than rounding; the model still carries that column, and an
#' outcome can be fitted exactly through it with enormous coefficients
#' where the reduced fit shows an ordinary residual. Such a design is
#' refused as unresolved: nothing at double precision decides whether the
#' fit through it is exact, and the guard does not answer from a design the
#' model does not fit.
#'
#' A proper posterior whose residual is at most `1e-6` of the outcome's total
#' sum of squares is warned about: the residual SD will concentrate near zero
#' and the sampler has to work there. That is a screen on the input, not a
#' verdict on the fit; the sampler's own diagnostics say how it went.
#'
#' Under `link = "log"` an exact fit `y = exp(X b)` exists exactly when
#' `log(y)` lies in the column space of `X`, so existence is decided by a
#' linear fit of `log(y)`, which cannot overflow however wide `y` is. The
#' near-exact screen is then taken on the response scale, where the likelihood
#' measures its residual, because an outcome spanning many orders of magnitude
#' can have an ordinary residual in `log(y)` while the response-scale fit
#' reproduces every large observation and leaves almost nothing.
#'
#' A non-positive observation is a valid one under a log-link normal, which
#' constrains the mean and not the data, and a negative one cannot be matched
#' by a positive mean at all, so its residual bounds the total away from zero
#' and the posterior is proper. Zero is different: the mean can approach it at
#' the boundary where the linear predictor goes to `-Inf`. If every row is
#' zero, or the positive rows are fitted exactly while some direction of the
#' coefficients leaves them fixed and drives the zero rows' predictors down,
#' the likelihood grows as `sigma^(-n)` along that ray and whether a posterior
#' exists depends on how fast the coefficient priors decay: a normal prior
#' tames it, a Student-t or Cauchy prior does not. The guard does not see the
#' prior, so it refuses those cases. It passes the mixed case when the positive
#' rows leave a real residual, or when no such direction exists, which
#' [.zero_boundary()] decides exactly for up to two free directions and leaves
#' unknown, and therefore refused, beyond. A zero row is pinned only when it
#' lies exactly in the row space of the positive rows; one merely close to
#' it, within `1e-8` of its size, is neither pinned nor safely free, and the
#' model is refused as undecided.
#'
#' The test is the residual sum of squares against the outcome's own total
#' sum of squares, so it is invariant to the units of the outcome.
#'
#' @param data An `mlumr_data` object.
#' @param link The resolved link, `"identity"` or `"log"`.
#' @param center The centers the model subtracts from the covariates: the
#'   numeric vector `mlumr()` computes (zeros when it does not center), or
#'   `TRUE` for the IPD columns' midranges as a stand-in, or `FALSE` for the
#'   raw design. The guard judges the design the model fits, so the model's
#'   own centers are what `mlumr()` passes. A design the centering sends
#'   out of the double range is left to the validators that own it.
#' @return `TRUE` invisibly if the data were warned about.
#' @keywords internal
.check_normal_residual_variation <- function(data, link = "identity",
                                             center = TRUE) {
  ipd <- data$ipd$data
  y <- suppressWarnings(as.numeric(ipd$.outcome))
  covariates <- as.matrix(ipd[, data$covariates, drop = FALSE])
  # Not the place to diagnose non-finite inputs: the validators that own that
  # question run their own checks and give their own messages.
  if (length(y) == 0L || !all(is.finite(y)) || !all(is.finite(covariates))) {
    return(invisible(FALSE))
  }
  if (isTRUE(center)) {
    # A stand-in for the model's own centers, formed from halves so that a
    # column holding values near both 1e308 and -1e308 does not overflow.
    center <- apply(covariates, 2, function(v) max(v) / 2 + min(v) / 2)
  }
  if (is.numeric(center)) {
    covariates <- sweep(covariates, 2, as.numeric(center))
    if (!all(is.finite(covariates))) {
      return(invisible(FALSE))
    }
  }
  X <- cbind(1, covariates)
  advice <- paste(
    "This is a property of the data, not a setting: the outcome needs",
    "variation the covariates do not explain, or the model needs an",
    "observation process (a measurement error or rounding scale) that",
    "supplies one."
  )
  improper <- paste(
    "The posterior for the residual SD is improper: its density behaves as",
    "sigma^(rank - n) near zero and does not integrate, and the sampler",
    "would drift toward zero and report where it stopped."
  )
  boundary <- paste(
    "There the likelihood grows without bound as the residual SD shrinks,",
    "and whether any posterior exists depends on the tails of the",
    "coefficient priors, so the model is refused. The outcome needs",
    "variation the covariates do not explain, or a link whose mean can",
    "reach it."
  )
  unresolved <- function(s) {
    fmt <- paste0(
      "The IPD design has %d exactly independent columns, but a ",
      "factorization at machine precision resolves only %d: a covariate ",
      "differs from a combination of the others by less than rounding. The ",
      "model fits that column, and whether the outcome is reproduced ",
      "exactly through it cannot be decided at double precision, so the ",
      "model is refused rather than passed as proper. Drop the covariate ",
      "that is nearly a combination of the others, or rescale the design so ",
      "the difference is visible."
    )
    stop(sprintf(fmt, s$rank, s$numerical_rank), call. = FALSE)
  }

  if (identical(link, "log") && any(y <= 0)) {
    if (any(y < 0)) {
      # No positive mean matches a negative outcome, so the residual is
      # bounded away from zero and the posterior proper. The fit can still
      # be near exact: the negative rows' means can be driven toward zero
      # while the rest are fitted, and then the residual SD sits near the
      # size of the negatives. The screen says so where it can run.
      if (any(y > 0)) {
        return(invisible(.screen_mixed_zero(X, y, y > 0)))
      }
      return(invisible(FALSE))
    }
    if (all(y == 0)) {
      stop("The IPD outcome is identically zero, which a positive mean under ",
           "link = \"log\" can only approach as the intercept goes to -Inf. ",
           boundary, call. = FALSE)
    }
    # Mixed zeros and positives. The boundary ray needs the positive rows
    # fitted exactly AND a direction that leaves them fixed while driving
    # every zero row's predictor to -Inf. The positive rows' own status
    # settles the first, and .zero_boundary() the second.
    pos <- y > 0
    sub <- .residual_variation_status(X[pos, , drop = FALSE], y[pos], "log")
    if (identical(sub$status, "unresolved")) {
      unresolved(sub)
    }
    passes <- sub$status %in% c("positive", "near_exact")
    if (!passes) {
      Xs <- .scale_design(X)
      reach <- .zero_boundary(Xs[pos, , drop = FALSE],
                              Xs[!pos, , drop = FALSE],
                              X[pos, , drop = FALSE],
                              X[!pos, , drop = FALSE])
      passes <- identical(reach, "unreachable")
    }
    if (passes) {
      # The near-exact screen has to see the whole outcome. The positive rows
      # alone can have an ordinary residual relative to their own spread and
      # a tiny one relative to the total, once the zero rows put the total
      # sum of squares at the level squared: replicates at 1e6 - 1 and
      # 1e6 + 1 beside zeros have a residual of 2 against a total near 1e12.
      # The response-scale fit runs on everything, started from the positive
      # rows' log-scale fit since the default start takes log(0).
      return(invisible(.screen_mixed_zero(X, y, pos)))
    }
    lead <- paste0("The IPD outcome has zeros, which a positive mean under ",
                   "link = \"log\" can only approach as their linear ",
                   "predictor goes to -Inf, and the positive rows are fitted ",
                   "exactly. ")
    if (identical(reach, "reachable")) {
      stop(lead, "A direction of the coefficients leaves them fitted while ",
           "taking every zero row there. ", boundary, call. = FALSE)
    }
    stop(lead, "Whether a direction of the coefficients leaves them fitted ",
         "while taking every zero row there could not be decided: the ",
         "question has more than two free directions, which this check does ",
         "not attempt, or a zero row within rounding of the positive rows' ",
         "span without lying in it, or zero rows opposite to within ",
         "rounding, or a design whose rank at machine precision differs ",
         "from its exact rank, so it refuses rather than pass a possibly ",
         "unbounded likelihood. ", boundary, call. = FALSE)
  }

  s <- .residual_variation_status(X, y, link)
  switch(
    s$status,
    saturated = {
      warning("The IPD design has as many free columns as rows (", s$n,
              " rows, rank ", s$rank, "), so it reproduces the outcome ",
              "exactly and leaves no residual degrees of freedom. The ",
              "posterior is proper, but nothing in the data separates the ",
              "residual SD from the coefficients, so what is reported for ",
              "sigma is potentially strongly sensitive to the coefficient ",
              "priors.", call. = FALSE)
      invisible(TRUE)
    },
    constant = stop("The IPD outcome is constant, so the intercept alone ",
                    "reproduces it exactly and the normal model has no ",
                    "residual variation. ", improper, " ", advice,
                    call. = FALSE),
    exact = stop("The IPD covariates fit the outcome exactly: there are only ",
                 s$rank, " distinct covariate profiles for a design of rank ",
                 s$rank, ", and every replicate of a profile carries the same ",
                 "outcome, so the design reaches every observed value. ",
                 improper, " ", advice, call. = FALSE),
    unresolved = unresolved(s),
    unresolved_log = stop("The IPD outcome varies by less than the ",
                          "resolution of its logarithm, so whether the ",
                          "covariates fit it exactly under link = \"log\" ",
                          "cannot be decided at double precision, and the ",
                          "model is refused rather than passed as proper. ",
                          advice, call. = FALSE),
    undecidable = {
      fmt <- paste0(
        "The IPD covariates fit the outcome to within rounding: the residual ",
        "sum of squares is %.3g of the total, at or below the %.3g that ",
        "rounding alone can leave when the fit is exact. At double precision ",
        "nothing tells an exact fit from one this close, and the likelihood ",
        "is computed at the same precision, so the model is refused rather ",
        "than passed as proper. An exact fit makes the posterior for the ",
        "residual SD improper: its density behaves as sigma^(rank - n) near ",
        "zero and does not integrate. "
      )
      stop(sprintf(fmt, s$ratio, s$zero_ratio), advice, call. = FALSE)
    },
    near_exact = {
      .warn_near_exact(s$ratio)
      invisible(TRUE)
    },
    invisible(FALSE)
  )
}


#' Refuse a log-normal survival fit whose exact events collapse its scale
#'
#' A log-normal AFT is a normal model for `log(t)` with a positive scale
#' `sdlog`, and its exact-event density has exactly the singularity that
#' [.check_normal_residual_variation()] refuses. With `n` uncensored index
#' rows, a design of rank `r` that reproduces every `log(t)` exactly, and the
#' coefficients integrated out, the marginal density of the scale behaves as
#' `sdlog^(r - n)` near zero. Its integral diverges for every `n > r`, and no
#' prior with positive density at zero repairs it: `prior_aux` defaults to a
#' half-normal, and the half-t and exponential alternatives all have positive
#' density there. The sampler would drift toward zero and report where it
#' stopped.
#'
#' Four scopes, each a deliberate limit rather than a certificate.
#'
#' **The distribution.** Two groups, told apart by what the auxiliary `aux`
#' is rather than by the family's name. `"lognormal"` and `"gengamma"` are
#' refused, because for both of them `aux` *is* a scale: the log-scale
#' standard deviation for the one, and `sigma` for the other, which enters
#' the Lawless density `gengamma_lpdf(y, mu, sigma, k)` as a `-log(sigma)`
#' term and as the divisor of the log residual. An exact fit sends either to
#' zero, where the density with the coefficients integrated out behaves as
#' `aux^(rank - n)` and does not integrate. The generalized gamma's *second*
#' auxiliary is its shape, and it is not what diverges: at an exact fit the
#' density's dependence on it is bounded.
#'
#' The Weibull, log-logistic and gamma are log-location-scale families too,
#' but their auxiliary is a shape, the reciprocal of a scale, so the same
#' exact fit sends it to `+Inf` rather than to zero. The likelihood there
#' grows polynomially in the shape and a half-normal or exponential prior's
#' tail integrates it, while a half-t's may not: propriety is a property of
#' the prior, not of the data, and refusing the data would refuse well-posed
#' default fits. A warning says so instead, and a censored row that bounds
#' the shape suppresses it, as it does for a scale.
#'
#' **The auxiliary stratification.** Decided only when the index study holds
#' the scale alone, which `aux_by = ".study"` (the default) and `NULL` both
#' give it. Under `aux_by = "none"` the comparator rows enter the same
#' parameter, and sharing does not bound it by itself: a comparator of
#' right-censored rows whose fitted times sit above their censoring times
#' contributes a likelihood tending to one as the scale goes to zero, which
#' repairs nothing. Whether it bounds the parameter is a question about the
#' marginalized aggregate likelihood, which this geometry does not see, so
#' that case is reported rather than decided, and warned about rather than
#' refused.
#'
#' **Censoring.** A right-censored row at `c` whose fitted `eta` is below
#' `log(c)` has survival going to zero faster than any power of the scale, and
#' it makes the posterior proper on its own. One at or above `log(c)` has
#' survival going to one half or one and does nothing. So censored rows are
#' consulted, and only when the fit on the event rows determines their linear
#' predictors. That is estimability, not identification: a censored row's
#' predictor is determined whenever its covariate vector lies in the ROW
#' SPACE of the event design, which is weaker than that design having full
#' column rank, and the difference is not a corner case, since a censored
#' row at a covariate profile the events already occupy is always in it.
#' A gap between the fitted predictor and the nearer end of the row's region
#' that is no larger than the rounding in computing it does not count
#' either, since its sign is not information. Otherwise
#' the question is left undecided and said to be. They are consulted BEFORE
#' any message is issued, including the near-exact and saturated ones: a row
#' that bounds the parameter makes every one of those messages untrue, not
#' just the refusal.
#'
#' **Which scale the fit is read on.** The log-time one for the
#' location-scale families, whose `eta` sits at `log t`. Gompertz is the
#' exception: its hazard is `exp(eta + shape * t)`, so profiling a row's
#' density over `eta` puts the maximum at `log(shape) - log(expm1(shape * t))`,
#' about `log(shape) - shape * t`. That ridge needs the event TIMES in the
#' column space, not their logarithms, with the intercept absorbing the
#' `log(shape)`, so Gompertz is read on the time scale throughout.
#'
#' **Delayed entry, left and interval censoring.** All examined, through the
#' OBSERVATION REGION each row is known to lie in, on whichever of those two
#' scales the family is read on. A right-censored row at `c` runs from `c`
#' upwards with no upper end; a left-censored one at `u` runs up to `u` with
#' no lower end; an interval one runs from `l` to `u`. As the auxiliary goes
#' to its boundary
#' the fitted distribution concentrates at the fitted value, so a row's
#' contribution tends to one when that value is strictly inside its region
#' and to zero when it is strictly outside, and only the second bounds the
#' auxiliary.
#'
#' A delayed entry is not an ordinary lower end, because the contribution is
#' conditional on survival to it: with the fitted value BELOW the entry the
#' conditional law piles up just above it and the probability tends to one,
#' not to zero. So an entry never closes a region from below, while an
#' interval that opens strictly above its entry still does.
#'
#' Skipping a censoring type outright was not the safe choice it looked
#' like: it let a row that suppresses nothing stand in for one that does,
#' and sent an improper posterior to the sampler in silence.
#'
#' The comparator side is not examined either: its reconstructed rows enter a
#' likelihood marginalized over the integration grid, which is not this
#' geometry.
#'
#' @param data An `mlumr_data` object with `family = "survival"`.
#' @param distribution The resolved survival distribution.
#' @param aux_by The auxiliary stratification, as passed to [mlumr()].
#' @param center The centers the model subtracts from the covariates, as for
#'   [.check_normal_residual_variation()].
#' @return `TRUE` invisibly if the data were warned about, `FALSE` otherwise.
#' @keywords internal
.check_survival_scale_collapse <- function(data, distribution,
                                           aux_by = ".study",
                                           center = TRUE) {
  # `gengamma` belongs with `lognormal`, not with the shapes: its first
  # auxiliary is the Lawless `sigma`, a scale, and its shape is the second.
  scale_families <- c("lognormal", "gengamma")
  # Gompertz belongs here too. Left out, it got no diagnosis at all, not
  # even the prior-sensitivity warning the others get. Three repeated events
  # at t = 1 on an intercept-only design drive the intercept to
  # `log(a) - log(expm1(a))`, about -a, and with the coefficient integrated
  # out against a Cauchy `prior_intercept` the marginal slope
  # `d log M / d log a` is 1.000 at a = 1e6. A Cauchy `prior_aux`
  # contributes a^-2, which leaves a^-1 and does not integrate.
  #
  # It is read on the TIME scale, not the log-time one: the ridge above is
  # `eta = log(a) - log(expm1(a t))`, about `log(a) - a t`, so it needs the
  # event times themselves in the column space. Reading it on the log scale
  # was wrong in both directions. Five events at `t = 1:5` over `x = 0:4` fit
  # exactly on the time scale and not on the log one (residual sum of squares
  # 0.085 of the total), and their marginal slope is 1.000, so a Cauchy
  # `prior_aux` leaves `a^-1` and no posterior: that passed in silence. Three
  # events at `t = exp(0:2)` fit exactly on the log scale and not on the time
  # one, where the marginal FALLS by 577,014 per decade of shape: that was
  # warned about as a collapse it does not have.
  shape_families <- c("weibull", "weibull-aft", "loglogistic", "gamma",
                      "gompertz")
  if (!distribution %in% c(scale_families, shape_families)) {
    return(invisible(FALSE))
  }
  shared_aux <- identical(aux_by, "none")
  if (!is.null(aux_by) && !identical(aux_by, ".study") && !shared_aux) {
    return(invisible(FALSE))
  }
  ipd <- data$ipd$data
  status <- ipd$.status
  delay <- ipd$.delay_time %||% rep(0, nrow(ipd))
  time <- suppressWarnings(as.numeric(ipd$.time))
  if (is.null(status) || !length(time)) return(invisible(FALSE))
  start <- suppressWarnings(as.numeric(ipd$.start_time %||% rep(0, nrow(ipd))))
  # Every censoring type is examined now, through the observation region
  # each one puts its row in. Skipping a type outright was not the safe
  # choice it looked like: a left-censored row whose upper bound sits ABOVE
  # the fitted time contributes a probability tending to one, so it
  # suppresses nothing and the event singularity is exactly the one the
  # undelayed right-censored case is refused for. Three repeated events at
  # t = 1 on an intercept-only design with one left-censored row at upper
  # bound 2 give `d log M / d log(1/sdlog)` of 2.000, identical to the same
  # data with the row removed; at upper bound 0.5 the marginal collapses to
  # -1.8e11 instead, which is the row genuinely bounding.
  if (any(!status %in% c(0L, 1L, 2L, 3L))) return(invisible(FALSE))
  # Delayed entry does not rescue an exact fit, so skipping the whole
  # question for it admitted the collapse in silence. Each row contributes
  # `f(t) / S(entry)` or `S(c) / S(entry)`, and the entry time is strictly
  # below its own row's time. As the scale goes to zero the fitted
  # distribution concentrates at the fitted time, so `S(entry)` tends to one
  # and every term is the undelayed one. Measured on three exact events over
  # a rank-2 design, `d log M / d log(1/sdlog)` is 1.000 with no delayed
  # entry, 1.000 with entry at half the event time and 1.000 with entry at
  # 99% of it, and from `sdlog = 1e-3` down the three marginals agree to
  # every printed digit. A censored row whose entry sits ABOVE its own
  # fitted time does not change either: both survivals go to zero and their
  # ratio still goes to zero, so it still bounds.
  #
  # An entry at or above its own row's time is the one configuration this
  # argument does not cover. The validators do not admit one, so it is
  # refused rather than analyzed.
  if (any(!is.finite(delay), na.rm = TRUE)) return(invisible(FALSE))
  if (any(!is.finite(start), na.rm = TRUE)) return(invisible(FALSE))
  if (any(delay >= time, na.rm = TRUE)) return(invisible(FALSE))
  # An interval that does not open before it closes, or a lower end at or
  # above the row's own upper end, leaves no region to be inside or outside
  # of. The validators do not admit one.
  if (any(status == 3L & !(start < time), na.rm = TRUE)) {
    return(invisible(FALSE))
  }
  events <- status == 1L
  if (!any(events)) return(invisible(FALSE))
  covariates <- as.matrix(ipd[, data$covariates, drop = FALSE])
  # The guard stays on the LOG time whichever scale the fit is read on: a
  # time of zero is finite on the time scale, but its profile maximizer is
  # not, and a negative one has no ridge to speak of either.
  log_time <- suppressWarnings(log(time))
  # Not the place to diagnose non-finite or non-positive inputs: the
  # validators that own that question run their own checks.
  if (!all(is.finite(log_time)) || !all(is.finite(covariates))) {
    return(invisible(FALSE))
  }
  time_scale <- identical(distribution, "gompertz")
  y <- if (time_scale) time else log_time
  # How the messages below name the scale the fit was read on.
  scale_phrase <- if (time_scale) "on the time scale" else "on the log scale"
  if (isTRUE(center)) {
    center <- apply(covariates, 2, function(v) max(v) / 2 + min(v) / 2)
  }
  if (is.numeric(center)) {
    covariates <- sweep(covariates, 2, as.numeric(center))
    if (!all(is.finite(covariates))) return(invisible(FALSE))
  }
  X <- cbind(1, covariates)
  s <- .residual_variation_status(X[events, , drop = FALSE], y[events],
                                  "identity")
  if (identical(s$status, "positive")) return(invisible(FALSE))
  # A right-censored row can bound the auxiliary away from its boundary, but
  # only one whose fitted time falls below its censoring time. The argument
  # is the same whichever boundary it is: S(c) = 1 - Phi((log c - eta) / sigma)
  # goes to zero as sigma does, and exp(-(c e^-eta)^k) goes to zero as k
  # grows, both faster than the likelihood's polynomial growth.
  #
  # This is consulted BEFORE any message below, not between them. Such a row
  # makes every one of these untrue, not only the refusal: an auxiliary held
  # away from its boundary does not concentrate against it, and a design that
  # leaves no residual degree of freedom is not the only thing informing the
  # auxiliary once a censored row does.
  bound <- if (any(!events)) {
    # Only these three interpolate: `exact` is decided structurally (as many
    # distinct event profiles as the rank, replicates agreeing), `saturated`
    # has full row rank, and `constant` is reproduced by the intercept. A
    # `near_exact` or `unresolved` fit leaves a residual, so the structural
    # shortcut inside is not available to it.
    # The region each censored row is observed to lie in, on the same scale
    # the fit is read on.
    # A right-censored row runs from its own time upwards with no upper end;
    # the other two close at their own upper bound.
    #
    # A delayed entry is NOT an ordinary lower end. The contribution is
    # conditional on survival to it, and when the fitted value sits BELOW
    # the entry the conditional law piles up just above it, so the
    # probability of the region tends to one rather than to zero. Three
    # events at t = 1 with a left-censored row entering at 1.5 and closing
    # at 2: the row's own contribution is 1.000000 at every scale and the
    # marginal slope is 2.000, identical to the same data with the row
    # removed. Reading the entry as a lower end called that "bounded" and
    # passed the improper fit.
    #
    # An interval that opens STRICTLY above its entry still bounds from
    # below, since the pile at the entry is then outside it. So the lower
    # end is the interval's own opening when it has one above the entry,
    # and no lower end at all otherwise.
    # `start > delay` is the only way an interval opens above its entry, and
    # it forces `start > 0`, so this is never `log(0)`.
    open <- if (time_scale) {
      ifelse(start > delay, start, -Inf)
    } else {
      suppressWarnings(ifelse(start > delay, log(start), -Inf))
    }
    right <- status[!events] == 0L
    .censoring_bounds_aux(
      X, y, events,
      exact_fit = s$status %in% c("exact", "constant", "saturated"),
      lower = ifelse(right, y[!events], open[!events]),
      upper = ifelse(right, Inf, y[!events])
    )
  } else {
    "unbounded"
  }
  if (identical(bound, "bounded")) return(invisible(FALSE))

  # Near-exact is excluded so it falls through to its own warning below,
  # which is the accurate message for it: its residual is real.
  if (shared_aux && !identical(s$status, "near_exact")) {
    # `aux_by = "none"` puts the comparator rows in the same parameter.
    # Sharing does not bound it by itself: a comparator of right-censored
    # rows whose fitted times sit above their censoring times contributes a
    # likelihood tending to one as the scale goes to zero, and repairs
    # nothing. Whether it does bound it is a property of the marginalized
    # aggregate likelihood, which this geometry does not see, so the case is
    # reported rather than decided, and is not refused on a guess.
    #
    # A near-exact fit is proper whatever the comparator does, so it is not
    # this branch's business; but it still gets its own warning below rather
    # than silence. Sharing multiplies the index's near-boundary likelihood
    # by whatever the comparator contributes there, and when that is a
    # nonzero limit, which is exactly the configuration this function admits
    # it cannot analyze, the concentration is unchanged. Staying silent
    # would hide the sampler diagnostic in the one case that motivates this
    # branch existing.
    warning("The index event rows leave nothing to bound ",
            .aux_name(distribution), " away from its boundary, and no ",
            "censored index row bounds it either. Under `aux_by = \"none\"` ",
            "the comparator rows share that parameter, but sharing does not ",
            "bound it on its own: a comparator of right-censored rows whose ",
            "fitted times sit above their censoring times contributes a ",
            "likelihood tending to one at the boundary. Whether the ",
            "comparator bounds it is a property of the marginalized ",
            "aggregate likelihood, which this check does not examine, so ",
            "the fit is neither refused nor passed as proper: check the ",
            "sampler's behavior near the boundary and the sensitivity to ",
            "`prior_aux`.", call. = FALSE)
    return(invisible(TRUE))
  }

  if (identical(s$status, "near_exact")) {
    # The same reasoning as [.warn_near_exact()], about this parameter: the
    # residual is real and the posterior proper, and the auxiliary still
    # concentrates against its boundary.
    #
    # For the two whose ridge moves the coefficients, that is conditional
    # rather than certain: ordinary normal coefficient priors can stop the
    # shape well before a small event residual does, and the defaults are
    # not wide, `normal(0, 10)` on the intercept and `normal(0, 2.5)` on the
    # rest. Saying it will concentrate would be a sampler diagnosis the data
    # do not support on their own.
    conditional <- switch(
      distribution,
      gamma = paste0(
        " Whether it does is conditional on `prior_intercept` here: the ",
        "ridge shifts the intercept by about -log(shape), so an ordinary ",
        "normal intercept prior can stop the shape before this residual ",
        "does."
      ),
      weibull = paste0(
        " Whether it does is conditional on `prior_beta` and ",
        "`prior_intercept` here: this is the proportional-hazards Weibull, ",
        "whose ridge scales the linear predictor with the shape, so ",
        "ordinary normal coefficient priors can stop it before this ",
        "residual does."
      ),
      gompertz = paste0(
        " Whether it does is conditional on `prior_intercept` and ",
        "`prior_beta` here, and more so than for the others: the ridge ",
        "drives the linear predictor to about `log(shape) - shape * t`, ",
        "which runs away with the shape itself rather than with its ",
        "logarithm, so the default `normal(0, 10)` intercept prior stops the ",
        "shape long before a residual this small does."
      ),
      ""
    )
    fmt <- paste0(
      "The index covariates very nearly fit the event times exactly %s ",
      "(residual sum of squares is %.3g of the total). The ",
      "residual is real, so the posterior is proper, but %s %s ",
      "concentrate against its boundary and the sampler has to work there: ",
      "check its diagnostics before reading the estimate.%s"
    )
    warning(sprintf(fmt, scale_phrase, s$ratio, .aux_name(distribution),
                    if (nzchar(conditional)) "may" else "will",
                    conditional),
            call. = FALSE)
    return(invisible(TRUE))
  }
  # Saturated is an exact fit, but it does not diverge for every family:
  # `n == rank` is exactly the case where integrating the coefficients out
  # cancels the auxiliary's growth. The AFT location-scale parameterizations
  # put each event density's peak at the shape and its width in the location
  # at one over the shape, so the `shape^n` growth meets a `shape^-n` from
  # the coefficient integral and the marginal is CONSTANT in the shape.
  # Measured as `d log M / d log shape` from 10 to 1e6, integrating the
  # coefficients against their default priors: 0.000 for `weibull-aft`,
  # 0.002 for `loglogistic`, and negative throughout for `gamma`, whose
  # exact fit also drags the intercept to `-log(shape)` and into a proper
  # prior's tail.
  #
  # The proportional-hazards Weibull is the exception. Its cumulative hazard
  # is `t^shape e^eta`, so the width in the location stays of order one and
  # nothing cancels: two rank-2 rows both at `t = 1` give a slope of exactly
  # 2.000 over eight decades, which is `shape^n`. The coefficients stay at
  # eta = 0 rather than moving into their prior tails, and a
  # `prior_cauchy()` auxiliary contributes only `shape^-2`, so the tail is
  # constant and does not integrate. That one keeps the divergence warning.
  #
  # None of this survives `n > rank`, where the cancellation is partial and
  # the growth returns: the same measurement on three rows over a rank-2
  # design gives 1.000 for `weibull-aft` and `loglogistic` and 0.487 for
  # `gamma`, which are `n - rank` and `(n - rank) / 2`. So the exemption is
  # a property of `saturated` alone, and `exact` keeps the warning for every
  # shape family.
  # Gompertz is not exempt either, and for a different reason than the
  # proportional-hazards Weibull. Integrating a row's density over its own
  # `eta` gives `shape * e^(shape t) / expm1(shape t)`, which tends to the
  # SHAPE rather than to a constant, so a saturated design contributes
  # `shape^n`; against that the coefficients supply `shape^-2` apiece, but
  # only for the ones the ridge actually moves. The marginal goes as
  # `shape^(n - 2k)` for `k` moved coefficients, so propriety fails once
  # `n >= 2k + 1` and is not a property of `n == rank` at all. Measured
  # slopes at `n == rank`: 1.000 for three events all at t = 1 on a rank-3
  # design, where the times are the intercept alone and `k` is one, and
  # 2.000 for six rows whose times need two of six columns. A Cauchy
  # `prior_aux` takes off 2, leaving `shape^-1` and `shape^0`, neither of
  # which integrates. The generic saturated design does have every
  # coefficient moving, `k == n`, and is proper at `shape^-n`, but that is
  # the common case and not the guarantee this branch was making.
  saturated_divergent <- c("weibull", "gompertz")
  if (identical(s$status, "saturated") &&
      !distribution %in% saturated_divergent) {
    exponent_clause <- if (distribution %in% shape_families) {
      paste0("the growth of ", .aux_name(distribution),
             " carries the exponent `n - rank`")
    } else {
      paste0("the density of ", .aux_name(distribution),
             " carries the exponent `rank - n`")
    }
    warning("The uncensored index rows are as many as the free columns of ",
            "their design (", s$n, " rows, rank ", s$rank, "), so it ",
            "reproduces every event time exactly and leaves no residual ",
            "degrees of freedom. The posterior is proper, since with the ",
            "coefficients integrated out ", exponent_clause, ", which is ",
            "zero here, but nothing in the index data separates it from the ",
            "coefficients, so what is reported for it is ",
            "potentially strongly sensitive to `prior_intercept` and ",
            "`prior_beta`. The exact-fit coefficient vector includes the ",
            "intercept, and their defaults are not the same width, ",
            "`normal(0, 10)` against `normal(0, 2.5)`, so a sensitivity ",
            "analysis on one of them is not one on both.", call. = FALSE)
    return(invisible(TRUE))
  }

  # Only `exact` and `constant` are exact fits. `constant` is one because the
  # design carries an intercept, so identical event times are reproduced by
  # it alone. The other three statuses mean the question could not be
  # ANSWERED at double precision, and a message that asserts an exact fit
  # would be a false diagnosis of a design that is merely near-collinear or
  # rounded.
  # Why the question could not be answered, shared by the shape warning and
  # the scale refusal so the two describe the same state the same way.
  undecided_reason <- function(s) {
    if (identical(s$status, "unresolved")) {
      return(sprintf(paste0("the index design has %d exactly independent ",
                            "columns and a factorization at machine ",
                            "precision resolves only %d, so whether the ",
                            "event times are reproduced through that column ",
                            "cannot be told at double precision"),
                     s$rank, s$numerical_rank))
    }
    paste("the index covariates fit the event times", scale_phrase,
          "to within rounding, and at double precision nothing tells that",
          "from an exact fit")
  }
  # `saturated` reaches here only for a shape family, and it is an exact fit:
  # a design with as many free columns as event rows reproduces every one of
  # them.
  resolved_exact <- s$status %in% c("exact", "constant", "saturated")
  undetermined_bound <- if (identical(bound, "undetermined")) {
    paste0(", and whether a censored row bounds it could not be told, since ",
           "the fit on the event rows does not determine those rows' linear ",
           "predictors")
  } else {
    ""
  }
  # Which prior settles it is not the same for all of them, because the
  # exact-fit ridge does not move the same way. Under the AFT
  # parameterizations (`weibull-aft`, `loglogistic`) an exact fit pins
  # `eta` at `log(t)` and the ridge leaves the coefficients where they are,
  # so `prior_aux` carries it alone. It does not under the other two, and
  # pointing only at `prior_aux` there sends the reader to the wrong
  # sensitivity analysis.
  ridge_clause <- switch(
    distribution,
    gamma = paste0(
      " The ridge here does not hold the coefficients fixed: `t e^-eta` is ",
      "Gamma(shape, 1), whose mode is at `shape - 1`, so an exact fit needs ",
      "the intercept to fall like `-log(shape)` as the shape grows. That ",
      "makes `prior_intercept` bear on propriety as well, and a normal one ",
      "integrates the ridge even where `prior_aux` alone would not."
    ),
    gompertz = paste0(
      " The ridge here does not hold the coefficients fixed: the hazard is ",
      "`exp(eta + shape * t)`, so an exact fit drives the linear predictor ",
      "to `log(shape) - log(expm1(shape * t))`, about ",
      "`log(shape) - shape * t`, which runs away with the shape itself ",
      "rather than with its logarithm. With the coefficients integrated out ",
      "against Cauchy priors the marginal goes as `shape^(n - 2k)`, where ",
      "`k` counts the coefficients that ridge moves, which is how many ",
      "columns the event times need: measured slopes are -1.000, 0.000 and ",
      "1.000 for one, two and three events on an intercept-only design, ",
      "where `k` is one, and 1.000 for five events whose times are exactly ",
      "linear in one centered covariate, where `k` is two. ",
      "`prior_intercept` and `prior_beta` therefore bear on propriety as ",
      "much as `prior_aux` does, and ordinary normal ones integrate the ",
      "ridge where Cauchy ones need not."
    ),
    weibull = paste0(
      " The ridge here does not hold the coefficients fixed: this is the ",
      "proportional-hazards Weibull, with cumulative hazard ",
      "`t^shape exp(eta)`, so an exact fit scales the whole linear ",
      "predictor with the shape. `prior_beta` and `prior_intercept` bear on ",
      "propriety alongside `prior_aux`, and a sensitivity analysis on the ",
      "auxiliary prior alone would not see it."
    ),
    ""
  )
  prior_clause <- paste0(
    "Whether a posterior exists then depends on the tail of `prior_aux`: a ",
    "half-normal or an exponential integrates it and a half-t need not, so ",
    "what is reported for the shape is a property of that prior rather than ",
    "of the data.", ridge_clause, " The event times need variation the ",
    "covariates do not explain, or an observation process (a measurement ",
    "error or a rounding scale) that supplies one."
  )
  if (distribution %in% shape_families) {
    if (resolved_exact) {
      geometry_clause <- if (identical(s$status, "saturated")) {
        paste0(" (the ", s$n, " uncensored rows are as many as the free ",
               "columns of their design, of rank ", s$rank, ", so it ",
               "reproduces every one of them and leaves no residual degree ",
               "of freedom)")
      } else {
        ""
      }
      warning("The index covariates fit every event time exactly ",
              scale_phrase, geometry_clause, ", so the likelihood grows ",
              "without ",
              "limit as the ", distribution, " shape does",
              undetermined_bound, ". ", prior_clause, call. = FALSE)
    } else {
      warning("Whether the index covariates fit every event time exactly ",
              scale_phrase, " could not be told at double precision (",
              undecided_reason(s), "), so neither could whether the ",
              "likelihood grows without limit as the ", distribution,
              " shape does", undetermined_bound, ". If it does: ",
              prior_clause, " Until that is settled, treat the shape as ",
              "prior-driven and check its sensitivity.", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  advice <- paste(
    "This is a property of the data, not a setting: the event times need",
    "variation the covariates do not explain, or the model needs an",
    "observation process (a rounding or measurement scale) that supplies",
    "one. Rounded times may need an interval-censored representation;",
    "jitter and a hidden floor on the scale are not honest substitutes."
  )
  # Both of these families reach here, and the argument is the same for
  # both: it is the parameter's name that differs.
  scale_label <- if (identical(distribution, "lognormal")) {
    "the log-normal `sdlog`"
  } else {
    "the generalized-gamma `sigma`"
  }
  scale_symbol <- if (identical(distribution, "lognormal")) "sdlog" else "sigma"
  improper <- paste0(
    "The posterior for ", scale_label, " is improper: with the coefficients ",
    "integrated out its density behaves as ", scale_symbol, "^(rank - n) ",
    "near zero and does not integrate, and the sampler would drift toward ",
    "zero and report where it stopped."
  )
  undecided <- function(why) {
    stop("Whether ", scale_label, " has a proper posterior could not be ",
         "decided: ", why, ". A possibly improper posterior is not one to ",
         "sample, so the model is refused rather than passed as proper. ",
         advice, call. = FALSE)
  }
  if (!resolved_exact) {
    undecided(undecided_reason(s))
  }
  if (identical(bound, "undetermined")) {
    undecided(paste("the index covariates fit every event time exactly, and",
                    "the censored rows' linear predictors are not determined",
                    "by that fit, so whether one of them falls below its",
                    "censoring time could not be told"))
  }
  if (any(!events)) {
    stop("The index covariates fit every event time exactly on the log ",
         "scale, and no censored row is predicted to fail before it was ",
         "censored, so none of them bounds the scale away from zero. ",
         improper, " ", advice, call. = FALSE)
  }
  stop("The index covariates fit every event time exactly on the log scale: ",
       s$n, " uncensored rows against a design of rank ", s$rank,
       ", with no censored row to bound the scale. ", improper, " ", advice,
       call. = FALSE)
}


#' Whether a censored row bounds the auxiliary away from its boundary
#'
#' The event rows are fitted exactly, so the auxiliary runs to its boundary
#' unless some censored row's survival goes to zero with it. That happens
#' when the row's linear predictor is strictly below the log of its
#' censoring time, and the condition is the same whichever boundary it is:
#' the log-normal survival `1 - Phi((log c - eta) / sigma)` goes to zero as
#' `sigma` does, and `exp(-(c e^-eta)^k)` goes to zero as `k` grows.
#'
#' It is a question about a censored row's fitted value, not about the
#' coefficients. Without full column rank the exact solutions form an affine
#' family, but a row's fitted value is the same across the whole family
#' whenever its covariate vector lies in the ROW SPACE of the event design,
#' which is the usual estimability condition. That is strictly weaker than
#' identifying every coefficient, and the difference is not a corner case:
#' exact events and a censored row at the same covariate profile fix that
#' row's predictor exactly, however deficient the design is, because the row
#' is one of the event rows. Requiring the stronger condition refused fits
#' that were proper.
#'
#' A row that is not estimable is not evidence in either direction, and is
#' consulted for neither answer: it cannot produce `"bounded"`, and it blocks
#' `"unbounded"`, which needs every row to be strictly inside its region. All
#' it can do is leave the answer `"undetermined"`.
#'
#' A censored row that repeats an event row's covariate profile needs no
#' solve at all. Where the event fit interpolates, its predictor IS that
#' event's own `y`, for every exact solution and however deficient or
#' ill-conditioned the design is, so the question reduces to comparing two
#' stored data values. That is worth asking first: it is exact where the
#' numerical path is not, and it answers cases the numerical path refuses,
#' including a design whose numerical rank falls below its exact one.
#'
#' @param X The full centered design, intercept first.
#' @param y The fitted response for every row, on the scale the family's
#'   collapse happens on: `log(time)` for the location-scale families, whose
#'   `eta` sits at `log t`, and `time` itself for Gompertz, whose ridge is
#'   `log(shape) - log(expm1(shape * t))` and so needs the times rather than
#'   their logarithms. This function does not transform it and does not know
#'   the family; it only requires that `lower` and `upper` arrive on the same
#'   scale.
#' @param events Logical, which rows are events.
#' @param exact_fit Whether the event rows are known to be fitted exactly.
#'   The structural shortcut above holds only then, and the caller knows it
#'   from the geometry it already measured; a near-exact fit puts the
#'   duplicated row at the fitted value rather than at the event's time, so
#'   the shortcut is not taken for one.
#' @param lower,upper The ends of each non-event row's observation region, on
#'   the same scale as `y`, one value per non-event row, in the order those
#'   rows appear in `X`. `lower` may be `-Inf` and `upper` may be `Inf`, which
#'   is
#'   what an open end means; `lower <= upper` is required and a row that
#'   violates it leaves the answer `"undetermined"`. The defaults are the
#'   right-censored region, `y[!events]` and `Inf`, so a caller that knows
#'   only times gets the behavior it had before the other two censoring
#'   types were admitted.
#' @return `"bounded"`, `"unbounded"`, or `"undetermined"`.
#' @keywords internal
.censoring_bounds_aux <- function(X, y, events, exact_fit = FALSE,
                                  lower = NULL, upper = NULL) {
  Xe <- X[events, , drop = FALSE]
  Xc <- X[!events, , drop = FALSE]
  if (!nrow(Xc)) return("unbounded")
  # Every censoring type asks the same question of the same object: the
  # OBSERVATION REGION the row is known to lie in, on the same scale as the
  # fit. A
  # right-censored row runs from its own time upwards with no upper end, a
  # left-censored one up to its own time with no lower end, an interval one
  # between its two. As the auxiliary goes to its boundary the fitted
  # distribution concentrates at the fitted value, so the row's contribution
  # tends to one when that value is strictly INSIDE its region and to zero
  # when it is strictly outside. Only the second bounds the auxiliary.
  #
  # The caller decides where a row's ends come from; a delayed entry is NOT
  # one of them, since it conditions the observation rather than bounding
  # it. Defaulting to the right-censored region keeps the caller that
  # passes only times.
  yc <- y[!events]
  if (is.null(lower)) lower <- yc
  if (is.null(upper)) upper <- rep(Inf, length(yc))
  if (length(lower) != length(yc) || length(upper) != length(yc)) {
    return("undetermined")
  }
  if (any(is.na(lower)) || any(is.na(upper)) || any(lower > upper)) {
    return("undetermined")
  }
  if (isTRUE(exact_fit)) {
    # Exact keys, not rounded ones: `x` and `x + 1e-13` are different
    # profiles, and a 15-digit character conversion would merge them and
    # pin a censored row to an event time that is not its own. `%a` is the
    # binary value itself, and the `+ 0` normalizes a negative zero, which
    # compares equal but prints differently.
    row_keys <- function(M) {
      apply(matrix(sprintf("%a", M + 0), nrow = nrow(M)), 1L, paste,
            collapse = "|")
    }
    twin <- match(row_keys(Xc), row_keys(Xe))
    repeated <- which(!is.na(twin))
    if (length(repeated)) {
      pinned <- y[events][twin[repeated]]
      if (any(pinned < lower[repeated] | pinned > upper[repeated])) {
        return("bounded")
      }
    }
  }
  rank_e <- .exact_rank(Xe)$rank
  fit <- tryCatch(stats::lm.fit(Xe, y[events]), error = function(e) NULL)
  if (is.null(fit)) return("undetermined")
  # `lm.fit()` decides its own rank at a numerical tolerance, and it can drop
  # a column that `.exact_rank()` keeps. The estimability test below runs on
  # the exact rank, so the two would be answering about different models: the
  # fitted values would come from the reduced one while the rows were judged
  # against the full one. When the exact fit depends on the dropped
  # direction that is not a rounding difference. With events at
  # `x = (-1, 0, 1, 2)`, a second column `x + 1e-13`, and event times equal
  # to that second column, the exact solution is `(0, 0, 1)` and the reduced
  # one is `(1e-13, 1, 0)`; a censored row at `(1, 0, 10)` has a true
  # predictor of 10 and a reduced one of 1e-13, so a censoring time of 5
  # came back "bounded" when the true answer is "unbounded", and an improper
  # fit went to Stan in silence. Refuse to answer instead.
  if (!is.numeric(fit$rank) || fit$rank < rank_e) return("undetermined")
  beta <- fit$coefficients
  # A rank-deficient event design leaves some coefficients aliased, and
  # `lm.fit()` returns NA for them. Any one solution will do here: the fitted
  # value of an ESTIMABLE row is the same for every solution, which is what
  # estimability means, and setting the aliased entries to zero picks the
  # solution that uses only the pivot columns.
  beta[is.na(beta)] <- 0
  if (!all(is.finite(beta))) return("undetermined")
  eta <- as.vector(Xc %*% beta)
  if (!all(is.finite(eta))) return("undetermined")
  # A censored row's predictor is determined whenever its covariate vector
  # lies in the ROW SPACE of the event design. That is weaker than every
  # coefficient being identified, and requiring the stronger condition
  # refused fits that are proper: exact events and a censored row at the
  # same covariate profile determine that row's predictor exactly, however
  # deficient the design is, because the row is one of the event rows.
  #
  # Ask the cheap question first. If adding every censored row at once does
  # not raise the rank, all of them are in the row space, which is the usual
  # case; only otherwise is it worth asking row by row.
  all_in_span <- .exact_rank(rbind(Xe, Xc))$rank == rank_e
  estimable <- if (all_in_span) {
    rep(TRUE, nrow(Xc))
  } else {
    vapply(
      seq_len(nrow(Xc)),
      function(i) {
        .exact_rank(rbind(Xe, Xc[i, , drop = FALSE]))$rank == rank_e
      },
      logical(1L)
    )
  }
  # A row that is not estimable is not evidence either way, so it cannot
  # give "unbounded": it leaves the question undetermined instead.
  #
  # `eta` is a computed least-squares value, not an exact one, so the sign of
  # a rounding-sized gap is not information. A censored row sitting ON the
  # fitted boundary has survival tending to 1/2 and bounds nothing, but over
  # 4000 randomly generated boundary rows (a censored row duplicating an
  # event's covariate profile and its time) a third came out strictly below
  # and would have been read as bounding, which admits an improper fit in
  # silence. Require the gap to exceed the rounding before calling it one,
  # and where it does not, say the question is undetermined rather than
  # guess the sign.
  #
  # The rounding in `Xc %*% beta` is governed by the size of the terms that
  # went into it, not by the size of what came out. An ill-conditioned design
  # with full numerical rank reaches an exact fit through large cancelling
  # coefficients, and then a predictor near zero carries an absolute error
  # many orders above its own magnitude. Over 2000 ill-conditioned boundary
  # rows a tolerance built from `abs(eta)` was beaten 39 times, by up to a
  # factor of 4, each one a rounding artifact read as a bound. This is the
  # bound [.fit_ratios()] already uses for the same reason.
  # And `beta` itself carries the error of the least-squares solve, which a
  # bound on the dot product alone does not see. That error is amplified by the
  # conditioning of the design, and a censored row in the row space can be an
  # EXTRAPOLATION of the event rows rather than one of them, which amplifies
  # it again: over 1024 such rows at condition numbers up to 9e7, all of them
  # sitting exactly on the boundary, 16 came back "bounded" without this
  # factor. A censored row that merely duplicates an event row does not show
  # it, because the fitted value there is accurate to the backward error.
  #
  # The condition number is taken on the columns the fit actually used, not
  # on `Xe`: a rank-deficient design is singular, and its `kappa()` would be
  # infinite and turn every answer into "undetermined", including the
  # deficient-but-estimable rows this function exists to answer.
  used <- fit$qr$pivot[seq_len(fit$rank)]
  # On the COLUMN-SCALED design, as [.residual_variation_status()] does. An
  # unscaled `kappa()` counts a units choice as ill-conditioning: the same
  # data with a covariate multiplied by 2^50 has kappa 1.1e15 where the
  # scaled design has exactly 1, and the tolerance that came out of it
  # swallowed a real censoring gap and returned "undetermined", refusing a
  # log-normal the censored row makes proper. Propriety is not a property of
  # the units.
  #
  # The censored rows and the coefficients are transformed the same way, so
  # `eta` is untouched (the divisors are powers of two, so this is exact)
  # and the norms below are the scaled ones. The whole tolerance is then
  # invariant under a change of predictor units, which is the point.
  divisor <- rep(1, ncol(X))
  if (ncol(X) > 1L) {
    for (j in 2:ncol(X)) {
      size <- max(abs(Xe[, j]))
      if (is.finite(size) && size > 0) {
        divisor[j] <- 2^min(max(floor(log2(size)), -1022), 1023)
      }
    }
  }
  Xe_s <- sweep(Xe, 2L, divisor, "/")
  Xc_s <- sweep(Xc, 2L, divisor, "/")
  beta_s <- beta * divisor
  if (!all(is.finite(Xe_s)) || !all(is.finite(Xc_s)) ||
      !all(is.finite(beta_s))) {
    return("undetermined")
  }
  cond <- tryCatch(kappa(Xe_s[, used, drop = FALSE], exact = FALSE),
                   error = function(e) Inf)
  if (!isTRUE(is.finite(cond))) cond <- Inf
  # The solve error is NORM-wise, and `|Xc| |beta|` is not a bound on it.
  # A coefficient's own error is set by the size of the whole solution, not
  # by its own size, so the coordinatewise product silently assumes every
  # coordinate carries at most `cond * eps` of RELATIVE error. A small
  # coefficient against a large covariate breaks that assumption, and the
  # product understates the term that dominates the predictor's error.
  # Events at `t = 1000:1004` on `(1, t, t^2)` with `beta = (3, 2, 2^-38)`
  # keep full numerical rank at a condition number of 6e11; a censored row
  # at `(1, 0, -2^38)` sitting EXACTLY on its fitted boundary had a
  # computed gap 7 times the coordinatewise tolerance, and 389 times it at
  # `2^-44`. Each one returned "bounded" and passed a possibly improper fit
  # in silence. `||Xc|| ||beta||` dominates the coordinatewise product by
  # Cauchy-Schwarz, so this is a widening: it can turn a "bounded" into an
  # "undetermined" and never the other way.
  xc_norm <- sqrt(rowSums(Xc_s^2))
  beta_norm <- sqrt(sum(beta_s^2))
  if (!all(is.finite(xc_norm)) || !is.finite(beta_norm)) {
    return("undetermined")
  }
  edge <- pmax(ifelse(is.finite(lower), abs(lower), 0),
               ifelse(is.finite(upper), abs(upper), 0))
  tol <- max(8, nrow(Xe)) * .Machine$double.eps * max(1, cond) *
    pmax(xc_norm * beta_norm, edge, 1)
  # Outside its region by more than the rounding, in either direction, is a
  # bound; strictly inside it by more than the rounding is no bound at all.
  # An infinite end never decides anything: `Inf - eta` is the whole real
  # line of slack, which is what a right-censored row's upper end means.
  outside <- pmax(lower - eta, eta - upper)
  inside <- pmin(eta - lower, upper - eta)
  if (any(estimable & outside > tol)) return("bounded")
  if (all(estimable) && all(inside > tol)) return("unbounded")
  "undetermined"
}


#' The auxiliary parameter a survival distribution calls its own
#' @keywords internal
.aux_name <- function(distribution) {
  switch(distribution,
         lognormal = "`sdlog`",
         loglogistic = "the log-logistic shape",
         gamma = "the gamma shape",
         gengamma = "the generalized-gamma scale `sigma`",
         gompertz = "the Gompertz shape",
         "the Weibull shape")
}


#' The near-exact screen for a log-link outcome with non-positive values
#'
#' The response-scale fit on the whole outcome, started from the positive
#' rows' log-scale fit. The outcome is scaled by a power of two, which is
#' exact and only shifts the intercept under a log link. Any iterate's
#' residual bounds the least-squares minimum from above, so a small one
#' justifies the warning and a large one only withholds it.
#'
#' @param X Design matrix as the model fits it, intercept included.
#' @param y Outcome vector with at least one positive value and at least one
#'   zero or negative one.
#' @param pos Logical, which rows are positive.
#' @return `TRUE` invisibly if the warning was issued.
#' @keywords internal
.screen_mixed_zero <- function(X, y, pos) {
  Xs <- .scale_design(X)
  # Scale by a power of two at the midrange of the positive outcomes' log
  # range, so both ends stay representable whenever the span fits at all:
  # scaling to the maximum alone underflowed the small end of a wide outcome
  # to zero, and the fit below then died on log(0) instead of standing aside.
  # Zeros are zeros at any scale.
  log2_pos <- log2(y[pos])
  power <- floor((max(log2_pos) + min(log2_pos)) / 2)
  power <- min(max(power, -1022), 1023)
  y <- y / 2^power
  # The screen is optional: an outcome it cannot represent, or a fit that
  # cannot start, leaves the verdict where the structural checks put it.
  ratio <- tryCatch({
    start <- stats::lm.fit(Xs[pos, , drop = FALSE], log(y[pos]),
                           tol = .Machine$double.eps)$coefficients
    start[is.na(start)] <- 0
    .log_link_response_ratio(Xs, y, start = start)
  }, error = function(e) NA_real_)
  if (is.finite(ratio) && ratio <= 1e-6) {
    .warn_near_exact(ratio)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}


#' The near-exact warning, shared by the plain and the mixed-zero paths
#' @param ratio Residual sum of squares over the total.
#' @return `NULL`, invisibly; called for the warning.
#' @keywords internal
.warn_near_exact <- function(ratio) {
  fmt <- paste0(
    "The IPD covariates very nearly fit the outcome exactly (residual sum ",
    "of squares is %.3g of the total). The residual is real, so the ",
    "posterior is proper, but the residual SD will concentrate near zero ",
    "and the sampler has to work there: check its diagnostics before ",
    "reading the estimate of sigma."
  )
  warning(sprintf(fmt, ratio), call. = FALSE)
  invisible(NULL)
}


#' Fit ML-UMR Model
#'
#' Fit a Bayesian multilevel unanchored meta-regression model using individual
#' patient data (IPD) and aggregate data (AgD). Supports binary, continuous,
#' count, and time-to-event outcomes.
#'
#' @param data An `mlumr_data` object with integration points (from
#'   [add_integration()])
#' @param model Model type: `"spfa"` (shared prognostic factor assumption) or
#'   `"relaxed"` (treatment-specific coefficients). Default `"spfa"`.
#' @section Normal outcomes with no residual variation:
#' A normal fit whose IPD covariates reproduce the outcome exactly has an
#' improper posterior for the residual SD, so `mlumr()` refuses it before any
#' sampling. Whether an exact fit exists is settled structurally where it can
#' be: a constant outcome, or one where every replicate of a covariate profile
#' agrees and there are only as many distinct profiles as the design has rank,
#' is fitted exactly and refused; replicate profiles carrying different
#' outcomes prove the residual positive and the posterior proper. Otherwise
#' the residual sum of squares is compared with the rounding an exact fit can
#' leave, which grows with the fitted coefficients. Above it the residual is
#' real and the posterior proper; at or below it nothing at double precision
#' tells an exact fit from one this close, and the model is refused as
#' undecidable rather than passed. A proper posterior whose residual is at
#' most `1e-6` of the outcome's total sum of squares is warned about, since
#' the residual SD will concentrate near zero and the sampler has to work
#' there; that is a screen on the input, and the sampler's own diagnostics say
#' how the fit went. Under `link = "log"` existence of an exact fit is decided
#' on `log(y)`, where it is a linear question, and the near-exact screen on
#' the response scale the likelihood uses. Zeros are the boundary case there:
#' an outcome identically zero is refused, and so is one whose positive rows
#' are fitted exactly while a free direction of the coefficients can take the
#' zero rows' predictors to `-Inf`, since along that ray the likelihood is
#' unbounded and only the coefficient priors' tails decide whether a
#' posterior exists. A zero row is pinned, and the ray blocked, only when it
#' lies exactly in the span of the positive rows; a row merely within
#' rounding of that span is refused as undecided.
#'
#' The check judges the design the model fits, with the model's own
#' centering (`center = TRUE`) or none, and its rank is the design's exact
#' rank, computed in exact arithmetic rather than by a factorization at
#' machine precision. A covariate that differs from a combination of the
#' others by less than rounding is still a column of the model, and an
#' outcome can be reproduced exactly through it with enormous coefficients
#' where a fit without it shows an ordinary residual. The structural rules
#' see that with the exact rank, and refuse it as an exact fit when the
#' distinct profiles are as few as the rank; otherwise such a design is
#' refused as unresolved, since nothing at double precision decides the
#' question, rather than passed on the strength of the reduced fit. With
#' `center = FALSE` the rounding bound carries the cancellation of the raw
#' predictor offsets, as the likelihood then does, so a residual below that
#' rounding is refused as undecidable where the centered fit would only
#' warn. A saturated design, with as many free columns as rows, is warned
#' about rather than refused: its posterior is proper, but nothing in the
#' data separates the residual SD from the coefficients, so what is reported
#' for sigma is potentially strongly sensitive to the coefficient priors.
#'
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
#'   poisson and survival: `"log"` (default, only option). If `NULL`, uses the
#'   canonical default for the family.
#' @param prior_intercept Prior for treatment intercepts. Default from
#'   [default_prior_intercept()] (`prior_normal(0, 10)`). This is a generic
#'   starting value on the linear-predictor scale, not a calibrated choice for
#'   every family or outcome scale. See [prior_normal()] for guidance.
#' @param prior_beta Prior for regression coefficients. May be a single
#'   prior broadcast to all covariates, or a `list` of priors of length
#'   `n_cov` for per-coefficient specification. All per-coefficient priors
#'   must share the same family and (for Student-t) df. Default from
#'   [default_prior_beta()] (`prior_normal(0, 2.5)`). Gelman et al. (2008)
#'   motivate a Cauchy prior after a particular predictor scaling, not this
#'   normal prior as a universal default. Set `autoscale = TRUE` on the
#'   prior to divide the scale by each covariate's empirical SD: useful
#'   when predictors are on very different scales. For `model = "spfa"`
#'   the single coefficient vector `beta` uses this prior; for
#'   `model = "relaxed"` the index-arm coefficients `beta_index` use it
#'   while `beta_comparator` uses `prior_beta_comparator` (see below).
#' @param prior_beta_comparator (Relaxed model only.) Prior for the
#'   comparator-arm regression coefficients `beta_comparator`. Same
#'   specification rules as `prior_beta` (single prior or per-coefficient
#'   list, any supported family); a different family from `prior_beta` is
#'   allowed (for example a heavy-tailed Student-t). If `NULL` (the default)
#'   `prior_beta` is used (matching the default symmetric behavior). This is
#'   a secondary, targeted regularization tool: for reliable relaxed-model
#'   estimates first ensure adequate integration points
#'   ([add_integration()] `n_int`) and post-warmup iterations. The
#'   AgD likelihood informs the comparator-population *outcome* directly,
#'   but that does not by itself identify `beta_comparator` or the
#'   comparator-population treatment contrast: how well either is determined
#'   depends on the number and geometry of independent aggregate summaries,
#'   the link, the covariate distribution, the outcome precision, and this
#'   prior. A handful of aggregate rows can leave whole coefficient
#'   directions informed only by the prior while the posterior still looks
#'   narrow. [check_identification()] reports the geometry of the aggregate
#'   rows, exactly for a normal identity-link model and descriptively for a
#'   nonlinear mean (it does not accept survival fits); [prior_sensitivity()]
#'   shows how much the posterior moves with the prior scale. Neither is a
#'   sufficient test on its own. The index-population effect
#'   additionally averages `beta_comparator` over the IPD covariate
#'   distribution (an extrapolation, since `beta_comparator` is informed
#'   only by the AgD likelihood), so its residual width is
#'   identification-driven. Tightening this prior (for example a smaller
#'   `prior_normal(0, 1)`) regularizes that residual width. Ignored for
#'   `model = "spfa"` (which has a single shared `beta`).
#' @param prior_sigma Prior for residual SD (normal family only). Default
#'   from [default_prior_sigma()] (`prior_normal(0, 2.5)`, half-normal via
#'   the Stan `<lower=0>` constraint). [prior_exponential()] is also
#'   supported for sigma.
#' @details
#' The model assumes that all AgD rows come from the same comparator treatment
#' and that, conditional on covariates, there is no between-study heterogeneity.
#' If AgD rows come from multiple studies with different designs or unmeasured
#' confounders, this assumption may not hold. No random effects for study-level
#' heterogeneity are included.
#'
#' **AgD scale assumptions (family = `"normal"`).** The AgD likelihood is
#' `y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
#' `y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`. In both
#' cases `set_agd()` expects `outcome_mean` and `outcome_se` on the
#' **arithmetic (original, untransformed) scale**, not log-scale or
#' geometric. Passing log-scale summaries silently misspecifies the
#' likelihood. See [set_agd()] for details.
#'
#' **The comparator population is the size-weighted mixture of its aggregate
#' rows.** Integrated marginal predictions in the comparator population
#' (`*_comparator` generated quantities) weight each row by the population it
#' represents:
#' \itemize{
#'   \item **binomial**: `n_agd[k]`, the AgD sample size.
#'   \item **normal**: `agd_weight[k]`, from `outcome_n`. This is required for
#'     more than one aggregate row, and is `1` for a single row where the
#'     weighting is irrelevant.
#'   \item **poisson**: `E_agd[k]`, the AgD exposure.
#' }
#' The weights say which population the estimand refers to, and are deliberately
#' separate from the likelihood's own precision weighting, which says how much
#' each row constrains the parameters. Because the parts of a split subgroup sum
#' to the whole, the estimand does not change with how the aggregate evidence
#' happens to be tabulated.
#'
#' **Weakly-identified coefficients in the relaxed model**:
#' `beta_comparator` is identified only through AgD, so the relaxed
#' model needs informative priors (or many AgD rows) to estimate
#' effect modification reliably. [prior_sensitivity()] is the
#' recommended diagnostic.
#'
#' @seealso [prior_sensitivity()] for sensitivity of the posterior
#'   to `prior_beta`; [set_agd()] for AgD scale requirements;
#'   [prior_summary()] for introspection of the priors actually used.
#'
#' @param distribution For `family = "survival"` only: the survival
#'   distribution. One of the parametric forms `"exponential"`, `"weibull"`
#'   (default), `"gompertz"` (proportional hazards), `"exponential-aft"`,
#'   `"weibull-aft"`, `"lognormal"`, `"loglogistic"`, `"gamma"`, `"gengamma"`
#'   (accelerated failure time), or the flexible-baseline forms `"mspline"`
#'   and `"pexp"` (piecewise exponential). Must be `NULL` for other families.
#'   Note: `"gengamma"` is the generalized gamma restricted to positive Lawless
#'   shape `Q` (`Q = 1 / sqrt(aux2) > 0`), which nests the Weibull, gamma, and
#'   (as the limit) log-normal; it does not represent negative-`Q` shapes. Use a
#'   flexible `"mspline"` baseline if the data need a hazard shape outside the
#'   positive-`Q` family. `"gengamma"` is also the least numerically robust
#'   option: its likelihood uses Stan's regularized incomplete gamma function,
#'   whose gradient can fail to converge (`grad_reg_lower_inc_gamma: n
#'   (internal counter) exceeded 100000 iterations`). Isolated messages of that
#'   kind are rejected proposals and are harmless, but frequent ones, divergent
#'   transitions, or a chain that fails outright mean the fit should not be
#'   trusted. Always inspect the MCMC diagnostics reported by `summary()` on a
#'   `gengamma` fit, and prefer `"weibull"`, `"gamma"`, `"lognormal"`, or
#'   `"mspline"` when they fit comparably.
#'   Note: `"gompertz"` has a positive shape only (the shape carries a
#'   `<lower=0>` constraint, so the hazard `exp(eta + shape * t)` is
#'   monotonically increasing). Decreasing-hazard Gompertz (negative shape),
#'   available in some survival software, is not supported; use `"mspline"` /
#'   `"pexp"` for a decreasing or non-monotone baseline hazard.
#' @param prior_aux For `family = "survival"` parametric distributions: prior
#'   for the shape/scale parameter(s) (half-normal/half-t/exponential via the
#'   `<lower=0>` constraint). Default [default_prior_aux()]. One default is
#'   reused across distributions whose auxiliary parameters do not share a
#'   scale, so check it against your own time unit rather than assuming it is
#'   weakly informative. The Weibull and gamma shapes and the log-normal
#'   `sdlog` are dimensionless, but the Gompertz shape has units of 1 / time:
#'   the same trial expressed in days, months, or years gives that parameter
#'   values three orders of magnitude apart, and a half-normal(0, 2) is
#'   near-flat on one scale and strongly informative on another. Set it
#'   explicitly for a Gompertz baseline, and use [prior_sensitivity()] or a
#'   prior-predictive check to see what hazard shapes it implies.
#' @param prior_aux2 For `family = "survival"` with
#'   `distribution = "gengamma"`: prior for the SECOND generalized-gamma
#'   auxiliary parameter. `NULL` (the default) reuses `prior_aux`, which is the
#'   previous behavior. The two auxiliaries control different features of the
#'   hazard, so they can need different regularization; supply this when one of
#'   them is poorly identified. Every other distribution has at most one
#'   auxiliary parameter: supplying this for one of them warns and has no
#'   effect on the fit, and the value is discarded WITHOUT being validated, so
#'   a malformed prior in that position warns like any other ignored one rather
#'   than aborting the fit. Non-survival families behave the same way.
#' @param prior_smooth For `family = "survival"` flexible baselines
#'   (`"mspline"`/`"pexp"`): prior for the random-walk smoothing SD. Default
#'   [default_prior_smooth()].
#' @param n_knots For `family = "survival"` flexible baselines: number of
#'   internal spline knots (default 7). See [make_knots()].
#' @param knots Optional custom knots for a flexible survival baseline. With a
#'   shared baseline (`aux_by = "none"`), supply one [make_knots()] result. With
#'   study-specific baselines, supply `list(index = ..., comparator = ...)`,
#'   where each element has the same structure and coefficient count.
#' @param aux_by For `family = "survival"`: how the baseline hazard is shared
#'   between the two studies, the unanchored analogue of `multinma::nma()`'s
#'   `aux_by`. `".study"` (the default) gives each study its **own** baseline
#'   shape, so the M-spline coefficients (or the parametric shape parameters)
#'   are estimated separately for the index and comparator studies. This matches
#'   `multinma`, where `.study` is always part of the stratification, and it is
#'   the right default: two single-arm trials rarely share a hazard shape, and
#'   assuming they do imposes proportional hazards *across studies*, which no
#'   randomization supports.
#'
#'   `NULL` is accepted and means the same as `".study"`, matching multinma,
#'   where a `NULL` `aux_by` is resolved to `".study"` and `.study` is always
#'   part of the stratification.
#'
#'   `"none"` gives both studies **one** shared shape. multinma has no spelling
#'   for this because it cannot do it; in an unanchored comparison it is a
#'   stronger assumption that buys precision, so it is worth fitting as a
#'   sensitivity analysis when the two Kaplan-Meier curves plainly have the same
#'   shape, but it should be a deliberate choice rather than a default.
#'
#'   **What stratifying assumes, and what it cannot test.** The parity with
#'   `multinma` is a parity of spelling, not of meaning. In an anchored
#'   randomized network each study contributes several arms, so a study-specific
#'   baseline shape is a nuisance parameter and within-study randomization still
#'   identifies the treatment effect. Here each study contributes exactly **one**
#'   arm, so a study-specific baseline shape and a treatment-specific baseline
#'   shape are perfectly aliased: nothing in the data can separate them. Under
#'   `".study"` the fitted shape therefore travels with the treatment when the
#'   effect is transported, which is an additional structural assumption the
#'   data cannot check, not merely the unanchored analogue of stratifying by
#'   study. `"none"` makes the opposite assumption, that the shape belongs to
#'   the disease rather than to the arm, and that one is at least testable
#'   against the two observed curves. Neither is assumption-free; fit both and
#'   report the difference.
#'
#'   With the stratified default the marginal hazard ratio varies with time, so
#'   the scalar `delta_*` reported by [marginal_effects()] is its value at one
#'   time, not a constant; pass `at_time` to choose which. This applies only
#'   where the shapes genuinely differ: the exponential has no shape, so
#'   `aux_by` leaves its closed-form contrast exact. The collapsible RMST
#'   difference does not have this problem and is the better headline estimand.
#'
#'   Identification differs by baseline. For `"mspline"` / `"pexp"` each stratum
#'   gets **its own knots over its own observed support** (as in multinma's
#'   default `type = "quantile"`), and its coefficients are a simplex, which
#'   pins that study's cumulative hazard to 1 at a boundary the study actually
#'   observed. Both parts matter. A single pooled basis spanning the longest
#'   study would leave the shorter study with basis functions it never observes;
#'   scaling its observed coefficients by `c`, moving the surplus simplex mass
#'   into an unobserved column, and replacing its intercept by `mu - log(c)`
#'   would then leave the likelihood exactly unchanged, so the intercept would
#'   be set by the prior rather than by data. Per-study boundaries remove that
#'   flat direction. For parametric baselines there is no such normalization and
#'   none is needed, because shape and scale enter the hazard as different
#'   functions of time; but the comparator shape is then informed only by the
#'   reconstructed comparator curve, so stratifying spends information that a
#'   short or heavily censored aggregate curve may not have. The exponential has
#'   no shape at all, so `aux_by` does not change it.
#'
#'   Reach for `"none"` only when the two arms' Kaplan-Meier curves plainly have
#'   the same shape, and report it as a sensitivity analysis rather than as the
#'   primary result: one shared shape is the stronger assumption and buys
#'   precision, but nothing in an unanchored design justifies it.
#'
#'   **An assumption worth naming.** When a stratified fit predicts the index
#'   treatment in the comparator population, it carries the *index study's*
#'   baseline shape with it, and vice versa. That is coherent only if the
#'   residual time pattern is a property of the treatment that travels across
#'   populations. In an anchored `multinma` network a study-stratified baseline
#'   is a study nuisance, not something attached to a treatment; here each study
#'   contributes exactly one arm, so the data cannot separate a
#'   treatment-specific hazard shape from a study, design, or calendar-time
#'   shape. Stratifying is the safer default for the *contrast*, but absolute
#'   predictions transported across populations rest on this extra assumption.
#'   Where it is doubtful, prefer the RMST estimands, compare against
#'   `aux_by = "none"`, and say which was used.
#' @param mspline_degree For `family = "survival"` flexible baselines: spline
#'   degree override (default derived from `distribution`: 3 for `"mspline"`,
#'   0 for `"pexp"`).
#' @param pred_times For `family = "survival"`: times at which survival,
#'   hazard and cumulative-hazard predictions are produced. If `NULL`, a grid
#'   up to the maximum observed time is used.
#' @param rmst_horizon For `family = "survival"`: the upper time limit for the
#'   restricted mean survival time. If `NULL`, the maximum observed time, except
#'   for a flexible baseline (`"mspline"` / `"pexp"`) stratified by study, where
#'   it defaults to the COMMON follow-up
#'   `min(max(index times), max(comparator times))`. Each study's flexible
#'   baseline is extrapolated as a constant hazard past its own last observed
#'   time, so a pooled-maximum default would make the headline RMST extrapolate
#'   the shorter study by construction. Pass a longer horizon explicitly to
#'   accept that extrapolation; doing so still warns.
#' @param n_rmst_grid For `family = "survival"`: number of equally spaced nodes
#'   (default `100`) on `[0, rmst_horizon]` for the trapezoidal RMST integral.
#'   Increase for sharp early hazards, long horizons, or high-curvature
#'   flexible-baseline tails where 100 points may be too coarse; refit at a
#'   higher value and compare RMST to check convergence.
#' @param center Logical (default `TRUE`). Center the covariates about the
#'   pooled IPD and population-weighted declared AgD means before fitting.
#'   The likelihood is unchanged after the intercept is transformed with the
#'   slopes, and centering often improves sampling geometry. Priors specified
#'   independently on the numerical intercept and slopes are not generally
#'   invariant to that transformation, so `center = TRUE` and `FALSE` can imply
#'   different joint priors even when their likelihoods represent the same
#'   regression model. Set `FALSE` to fit on the raw covariate scale.
#' @param qr Logical (default `FALSE`). Apply a thin-QR
#'   reparameterization to the combined (intercepts + covariates) design matrix.
#'   This decorrelates the design columns for more efficient HMC. The Stan model
#'   maps the requested priors to the original regression coefficients before
#'   the QR transform, so this option is intended as a computational
#'   reparameterization. Useful with many correlated or ill-scaled covariates;
#'   for the common few-covariate case the default fused-GLM path (with
#'   `center = TRUE`) is usually faster.
#' @details
#' The model assumes that all AgD rows come from the same comparator treatment
#' and that, conditional on covariates, there is no between-study heterogeneity.
#' If AgD rows come from multiple studies with different designs or unmeasured
#' confounders, this assumption may not hold. No random effects for study-level
#' heterogeneity are included.
#'
#' **AgD scale assumptions (family = `"normal"`).** The AgD likelihood is
#' `y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
#' `y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`. In both
#' cases `set_agd()` expects `outcome_mean` and `outcome_se` on the
#' **arithmetic (original, untransformed) scale**, not log-scale or
#' geometric. Passing log-scale summaries silently misspecifies the
#' likelihood. See [set_agd()] for details.
#'
#' **Comparator-population weighting is family-dependent.** Integrated
#' marginal predictions in the comparator population (`*_comparator`
#' generated quantities) are weighted by:
#' \itemize{
#'   \item **binomial**: `n_agd[k]` (AgD sample size), so larger
#'     AgD rows contribute more to the marginal mean.
#'   \item **normal**: `outcome_n[k]` (AgD sample size), which is required for
#'     multiple rows; a single row has weight one when `outcome_n` is omitted.
#'     These are the estimand's
#'     mixing weights, not the likelihood's `1 / se^2` precision weights, so
#'     splitting one comparator population into subgroup rows does not change
#'     the target population.
#'   \item **poisson**: `E_agd[k]` (AgD exposure), matching the
#'     rate-based likelihood.
#' }
#' Each weighting is natural for the corresponding likelihood; users
#' comparing marginal effects across families should be aware they are
#' not identically weighted.
#'
#' **Weakly-identified coefficients in the relaxed model.**
#' `beta_comparator` is identified only through AgD, so the relaxed
#' model needs informative priors (or many AgD rows) to estimate
#' effect modification reliably. [prior_sensitivity()] is the
#' recommended diagnostic.
#'
#' **Identifying the relaxed model with subgroup AgD.** The strongest way to
#' identify `beta_comparator` from data is to supply the comparator AgD as
#' **joint subgroups**: mutually exclusive, collectively exhaustive strata of
#' the comparator population, one [set_agd()] row per subgroup, each with its
#' own covariate summaries and outcome. Each subgroup contributes a separate
#' marginal likelihood term (`L_AgD = prod_s L_{AgD,s}`), and the variation in
#' covariate means across subgroups is what can separate the
#' treatment-specific covariate effects `beta_comparator` from the comparator
#' intercept (the primary relaxed-SPFA strategy of Chandler & Ishak,
#' Section 2.2.1). A single aggregate outcome summary generally cannot
#' separately identify all comparator coefficients and the comparator
#' intercept. Its likelihood term constrains one combination of them, the
#' comparator intercept plus the coefficients weighted by that row's
#' covariate means on the model's scale, which under `center = TRUE` are the
#' declared means minus the pooled center; the directions the row does not
#' constrain remain prior-driven, although their marginal posteriors can
#' still move through the constrained combination. For one covariate with an
#' identity link, independent normal priors of variance `a^2` on the
#' intercept and `b^2` on the coefficient, a centered mean `m` and an outcome
#' SE `s`, the posterior variance of the coefficient is
#' `1 / (1 / b^2 + m^2 / (a^2 + s^2))`: `b^2` when `m = 0`, which is a single
#' row whose mean sits at the pooled center, and smaller the further the
#' row's mean sits from it. Joint, nonoverlapping subgroup summaries each add
#' a likelihood term; how many directions those terms identify depends on
#' their number, their covariate-distribution geometry (rows with the same
#' mean profile tighten one combination and add no direction), the outcome
#' precision and the model, and for a nonlinear link differences in the
#' distributions beyond the mean profiles matter too. Remaining directions
#' require explicit prior sensitivity analysis. (Marginal, overlapping
#' subgroups would double-count patients and understate uncertainty; supply
#' jointly-defined subgroups.)
#'
#' @seealso [prior_sensitivity()] for sensitivity of the posterior
#'   to `prior_beta`; [set_agd()] for AgD scale requirements;
#'   [prior_summary()] for introspection of the priors actually used.
#'
#' @param chains Number of MCMC chains (default 4)
#' @param iter Total iterations per chain (default 2000)
#' @param warmup Number of warmup iterations (default 1000)
#' @param seed Random seed for reproducibility. If `NULL` (default), the fixed
#'   seed 2026 is used and a warning says so, so an unseeded fit still
#'   reproduces. The seed actually used is reported in the fitting messages.
#' @param adapt_delta Target acceptance rate (default 0.95)
#' @param max_treedepth Maximum tree depth for NUTS (default 15)
#' @param refresh How often to print progress (0 = silent, default 200)
#' @param engine Stan backend: `"rstan"` (default) or `"cmdstanr"`. If `NULL`,
#'   uses the engine set by [mlumr_engine()]. See [mlumr_engine()] for setup.
#' @param verbose Logical; if `FALSE`, suppresses mlumr progress messages.
#'   Stan sampler progress is still controlled by `refresh`.
#' @param ... Additional arguments passed to the Stan sampling function
#'   ([rstan::sampling()] or cmdstanr's `$sample()` method)
#'
#' @return An object of class `mlumr_fit`
#' @export
#'
#' @examples
#' \dontrun{
#' # Binary SPFA model
#' fit_spfa <- mlumr(dat, model = "spfa")
#'
#' # Relaxed SPFA (allows effect modification)
#' fit_relaxed <- mlumr(dat, model = "relaxed")
#' }
mlumr <- function(data,
                  model = c("spfa", "relaxed"),
                  link = NULL,
                  prior_intercept = default_prior_intercept(),
                  prior_beta = default_prior_beta(),
                  prior_sigma = default_prior_sigma(),
                  distribution = NULL,
                  prior_aux = NULL,
                  prior_smooth = NULL,
                  n_knots = 7L,
                  knots = NULL,
                  mspline_degree = NULL,
                  aux_by = ".study",
                  pred_times = NULL,
                  rmst_horizon = NULL,
                  n_rmst_grid = 100L,
                  center = TRUE,
                  qr = FALSE,
                  chains = 4,
                  iter = 2000,
                  warmup = 1000,
                  seed = NULL,
                  adapt_delta = 0.95,
                  max_treedepth = 15,
                  refresh = 200,
                  engine = NULL,
                  verbose = TRUE,
                  # Appended rather than placed next to `prior_beta`, where it
                  # reads better: inserting a formal in the middle silently
                  # rebinds every positional argument after it, so a 0.1.0 call
                  # passing prior_sigma positionally would have applied it to
                  # the comparator coefficients instead.
                  prior_beta_comparator = NULL,
                  # Appended for the same reason, and it is the reason: placed
                  # next to `prior_aux` where it reads better, it rebound every
                  # positional argument from `prior_smooth` onward, so a call
                  # passing `n_knots` positionally would have set the smoothing
                  # prior instead.
                  prior_aux2 = NULL,
                  ...) {

  model <- match.arg(model)

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(center) || length(center) != 1L || is.na(center)) {
    stop("`center` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(qr) || length(qr) != 1L || is.na(qr)) {
    stop("`qr` must be TRUE or FALSE.", call. = FALSE)
  }
  .validate_mlumr_sampling_args(
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh
  )
  engine <- .resolve_mlumr_engine(engine)
  seed_info <- .resolve_mlumr_seed(seed)
  seed <- seed_info$value

  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data()", call. = FALSE)
  }
  if (!data$has_integration) {
    stop("Integration points not found. Use add_integration() first.", call. = FALSE)
  }

  validate_prior(prior_intercept, "intercept")
  if (prior_intercept$distribution == "exponential") {
    stop("prior_intercept does not support exponential priors ",
         "(treatment intercepts are unconstrained on the link scale). ",
         "Use prior_normal(), prior_student_t(), or prior_cauchy().",
         call. = FALSE)
  }
  # prior_beta may be a single prior or a list of per-coefficient priors;
  # per-coefficient validation happens inside stan_prior_fields_beta().
  if (is_single_prior(prior_beta)) {
    validate_prior(prior_beta, "beta")
    if (prior_beta$distribution == "exponential") {
      stop("prior_beta does not support exponential priors ",
           "(coefficients are unconstrained on the link scale). ",
           "Use prior_normal(), prior_student_t(), or prior_cauchy().",
           call. = FALSE)
    }
  } else if (!is.list(prior_beta)) {
    stop("`prior_beta` must be a prior list or a list of priors.", call. = FALSE)
  }

  # C.3: autoscale is only consumed by the regression-coefficient priors. Warn
  # if the user set it
  # on prior_intercept or prior_sigma, because it will be silently ignored.
  if (isTRUE(prior_intercept$autoscale)) {
    warning("`autoscale = TRUE` on prior_intercept is ignored; ",
            "autoscaling is only applied to prior_beta and ",
            "prior_beta_comparator.", call. = FALSE)
  }

  family <- data$family %||% "binomial"
  link_info <- check_link(family, link)

  if (!is.null(prior_beta_comparator)) {
    if (model == "spfa") {
      # Ignored means ignored: validating first made a malformed value an error
      # on a model that never reads it, so the user got a hard failure instead
      # of the warning telling them the argument does not apply.
      warning("`prior_beta_comparator` is ignored for the SPFA model ",
              "(which has a single shared `beta`); only the relaxed model ",
              "has a comparator-specific coefficient vector.",
              call. = FALSE)
      prior_beta_comparator <- NULL
    } else if (is_single_prior(prior_beta_comparator)) {
      validate_prior(prior_beta_comparator, "beta_comparator")
      if (prior_beta_comparator$distribution == "exponential") {
        stop("prior_beta_comparator does not support exponential priors ",
             "(coefficients are unconstrained on the link scale). ",
             "Use prior_normal(), prior_student_t(), or prior_cauchy().",
             call. = FALSE)
      }
    } else if (!is.list(prior_beta_comparator)) {
      stop("`prior_beta_comparator` must be a prior list or a list of priors.",
           call. = FALSE)
    }
  }

  # Survival distribution + auxiliary/smoothing priors
  surv_info <- NULL
  if (family == "survival") {
    surv_info <- .survival_distribution_info(distribution)
    if (!is.null(knots) && surv_info$kind != "flexible") {
      stop("`knots` can only be supplied with `distribution = \"mspline\"` ",
           "or `distribution = \"pexp\"`.", call. = FALSE)
    }
    if (!is.null(mspline_degree)) {
      # `distribution` names the baseline shape, and the degree defines it:
      # "pexp" IS degree 0 and "mspline" IS degree 3. Silently honoring a
      # contradicting override would fit one model and report the other, so a
      # mismatch is rejected rather than resolved in either direction.
      requested <- as.integer(mspline_degree)
      canonical <- surv_info$mspline_degree
      if (is.na(canonical)) {
        stop("`mspline_degree` applies only to the flexible baselines, ",
             "`distribution = \"mspline\"` (degree 3) or \"pexp\" ",
             "(degree 0). Got distribution = \"", distribution, "\".",
             call. = FALSE)
      }
      if (!identical(requested, canonical)) {
        stop("`mspline_degree = ", requested, "` contradicts `distribution = \"",
             distribution, "\"`, which is degree ", canonical, ". Use ",
             "`distribution = \"pexp\"` for a degree-0 piecewise-exponential ",
             "baseline or `distribution = \"mspline\"` for the degree-3 ",
             "M-spline; the fit would otherwise be reported under the wrong ",
             "name.", call. = FALSE)
      }
      surv_info$mspline_degree <- requested
    }
    prior_aux <- prior_aux %||% default_prior_aux()
    # Only the generalized gamma has a second auxiliary parameter. Everywhere
    # else a supplied `prior_aux2` has nothing to apply to, and defaulting it
    # silently would let a user believe they had regularized something.
    if (!is.null(prior_aux2) && (surv_info$n_aux %||% 0L) < 2L) {
      warning("`prior_aux2` applies to the second auxiliary parameter of ",
              "`distribution = \"gengamma\"`; `distribution = \"",
              surv_info$distribution, "\"` has ",
              if ((surv_info$n_aux %||% 0L) == 0L) "no" else "one",
              " auxiliary parameter, so it is ignored. Use `prior_aux`.",
              call. = FALSE)
      # Drop it here rather than letting it reach `validate_prior()` below.
      # Announcing that a value is ignored and then erroring on its contents
      # is two contracts for one argument: a well-formed ignored prior was
      # discarded quietly while a malformed ignored prior aborted the fit, and
      # which of the two happened depended on a distribution the argument does
      # not even apply to. The non-survival branch already warns without
      # validating; this makes the survival branch agree with it.
      prior_aux2 <- NULL
    }
    # Falling back to `prior_aux` keeps the previous behavior exactly for every
    # fit that does not name the second auxiliary.
    prior_aux2 <- prior_aux2 %||% prior_aux
    prior_smooth <- prior_smooth %||% default_prior_smooth()
    validate_prior(prior_aux, "prior_aux")
    validate_prior(prior_aux2, "prior_aux2")
    validate_prior(prior_smooth, "prior_smooth")
    .validate_survival_controls(pred_times, rmst_horizon, mspline_degree,
                                n_knots, n_rmst_grid,
                                distribution = distribution,
                                knots = knots)
    invisible(.resolve_aux_strata(aux_by))   # fail on a bad value before fitting
    .validate_survival_studies(data, aux_by)
  } else {
    if (!is.null(knots)) {
      stop("`knots` is only used for flexible survival models.", call. = FALSE)
    }
    if (!is.null(distribution)) {
      stop("`distribution` is only used for family = 'survival'.", call. = FALSE)
    }
    # `aux_by` now defaults to ".study", so a non-NULL value is not evidence the
    # user asked for it. Only object when they supplied it explicitly.
    if (!missing(aux_by)) {
      stop("`aux_by` is only used for family = 'survival': it stratifies the ",
           "baseline hazard, which the other families do not have.",
           call. = FALSE)
    }
    # The rest of the survival controls were accepted and silently discarded, so
    # a caller could hand a non-survival fit a prediction grid and get a fit
    # back that never used it. Those with a NULL default are evidence on their
    # own; n_knots and n_rmst_grid carry real defaults, so use missing().
    unused <- c(
      if (!is.null(mspline_degree)) "mspline_degree",
      if (!is.null(pred_times)) "pred_times",
      if (!is.null(rmst_horizon)) "rmst_horizon",
      if (!missing(n_knots)) "n_knots",
      if (!missing(n_rmst_grid)) "n_rmst_grid"
    )
    if (length(unused)) {
      stop("`", paste(unused, collapse = "`, `"), "` ",
           if (length(unused) == 1L) "describes" else "describe",
           " a survival baseline hazard or its prediction grid, which ",
           "family = '", family, "' does not have.", call. = FALSE)
    }
    if (!is.null(prior_aux) || !is.null(prior_aux2) || !is.null(prior_smooth)) {
      warning("`prior_aux` / `prior_aux2` / `prior_smooth` are ignored for ",
              "non-survival families.", call. = FALSE)
    }
  }

  if (family == "normal") {
    validate_prior(prior_sigma, "sigma")
    if (isTRUE(prior_sigma$autoscale)) {
      warning("`autoscale = TRUE` on prior_sigma is ignored; ",
              "autoscaling is only applied to prior_beta and ",
              "prior_beta_comparator.", call. = FALSE)
    }
  } else if (!is.null(prior_sigma) && !isTRUE(prior_sigma$default)) {
    warning("`prior_sigma` is ignored for non-normal families.",
            call. = FALSE)
  }

  # Weak identifiability of `beta_comparator` in relaxed models. It is informed
  # only by the aggregate likelihood, so the note differs by what that
  # likelihood actually is.
  if (model == "relaxed") {
    n_cov_check <- data$n_covariates
    # What to tell the user depends on whether they have already regularized.
    # Pointing at `prior_beta` was wrong either way: it shrinks beta_index too,
    # where the IPD are informative, which is precisely the coupling
    # `prior_beta_comparator` exists to remove. And once a comparator prior IS
    # set, advising the user to set one says nothing; what matters then is that
    # the posterior for those coefficients is a statement about the prior.
    remedy <- if (is.null(prior_beta_comparator)) {
      paste0("Add jointly-defined subgroup rows, regularize with an ",
             "informative `prior_beta_comparator`, or use model = \"spfa\".")
    } else {
      paste0("You have supplied `prior_beta_comparator`, so the comparator ",
             "coefficients are estimable. The aggregate data still cannot ",
             "separate every direction of `beta_comparator`, and it is those ",
             "directions that the prior determines; combinations the ",
             "likelihood does constrain remain data-driven. Refit with ",
             "different `prior_beta_comparator` scales to see how far the ",
             "index-population estimand moves, or add jointly-defined ",
             "subgroup rows.")
    }
    # The spread warning below is a different claim: every direction IS
    # separated by the likelihood, some of them with little leverage. Telling
    # that user the data "cannot separate every direction" contradicted the
    # sentence before it, and whether the prior or the data ends up
    # determining those coefficients depends on the outcome precision along
    # them, which the profiles cannot show.
    remedy_spread <- if (is.null(prior_beta_comparator)) {
      remedy
    } else {
      paste0("You have supplied `prior_beta_comparator`, which regularizes the ",
             "coefficients along those directions; whether it or the data ",
             "ends up determining them depends on how precisely the rows' ",
             "outcomes are reported. Refit with different ",
             "`prior_beta_comparator` scales to see how far the ",
             "index-population estimand moves, or add jointly-defined ",
             "subgroup rows.")
    }
    if (family == "survival") {
      # A reconstructed comparator curve is NOT one scalar constraint. It
      # contributes a likelihood term at every event and censoring time, so how
      # much of (mu_c, beta_c) it pins down is model- and
      # covariate-distribution-dependent, not a matter of counting rows. One
      # binary covariate under exponential proportional hazards gives a
      # known-weight two-component mixture whose two rates the curve shape can
      # separate; several continuous covariates can leave the curve nearly
      # invariant to rotations of beta_c that hold its norm fixed, identifying
      # summaries but not the direction. Say what is true in both cases, and do
      # not gate on a row count or an event count: neither bounds nor certifies
      # identification here. See check_identification(), which refuses survival
      # for the same reason.
      warning(sprintf(
        paste0("Relaxed survival model with %d covariate(s): the ",
               "treatment-specific comparator coefficients are informed only ",
               "through the marginal reconstructed comparator curve. Depending ",
               "on the survival model and the covariate distribution that ",
               "curve may identify some combinations of them while leaving ",
               "other directions weakly determined. Inspect the coefficient ",
               "posterior and run prior_sensitivity(), including ",
               "`prior_beta_comparator_scales`, rather than assuming either ",
               "outcome. Index-population effects are the most exposed, since ",
               "they transport these coefficients to the IPD covariate ",
               "distribution; `prior_beta_comparator` regularizes them, and ",
               "model = \"spfa\" avoids them entirely."),
        n_cov_check
      ), call. = FALSE)
    } else if (family == "normal" && link_info$link == "identity") {
      n_agd_rows_check <- nrow(data$agd$data)
      agd_rank <- .agd_covariate_rank(data)
      # Two different claims, and only one of them is about the likelihood.
      # `.profile_rank()` counts directions whose spread reaches a practical
      # threshold; the numerical rank counts directions that exist at all.
      # Profiles at -0.01 and +0.01 have spread 0.01 and numerical rank 2, and
      # with aggregate standard errors of 1e-6 the slope is pinned to about
      # 7e-5. Saying the likelihood does not separate those parameters is
      # simply false, and precision cannot be judged from the profiles alone
      # because it also depends on the reported standard errors and row sizes.
      agd_numeric_rank <- .agd_covariate_numeric_rank(data)
      if (agd_numeric_rank < n_cov_check + 1L) {
        warning(sprintf(
          paste0("Relaxed model with %d AgD row(s) and %d covariate(s): the ",
                 "aggregate mean profiles span only %d independent ",
                 "direction(s) including the intercept, and the identity-link ",
                 "design needs %d. Some comparator-parameter combinations are ",
                 "therefore not separated by the likelihood at all. The most ",
                 "effective fix is to supply the comparator as jointly-defined ",
                 "subgroup rows, one set_agd() row per stratum with its own ",
                 "covariate summaries. %s"),
          n_agd_rows_check, n_cov_check, agd_numeric_rank, n_cov_check + 1L,
          remedy
        ), call. = FALSE)
      } else if (agd_rank < n_cov_check + 1L) {
        warning(sprintf(
          paste0("Relaxed model with %d AgD row(s) and %d covariate(s): the ",
                 "aggregate mean profiles span the %d direction(s) the ",
                 "identity-link design needs, but %d of them move less than ",
                 "the exploratory 0.05 IPD-SD screening threshold. Along those ",
                 "directions the aggregate rows differ very little, so the ",
                 "corresponding comparator coefficients lean on how precisely ",
                 "each row's outcome is reported: with large standard errors ",
                 "or small rows they will be wide and prior-sensitive, with ",
                 "small ones they can still be estimated well. This is a ",
                 "screening heuristic about SPREAD, not a statement about the ",
                 "posterior; read it off the fitted intervals. %s"),
          n_agd_rows_check, n_cov_check, n_cov_check + 1L,
          n_cov_check + 1L - agd_rank, remedy_spread
        ), call. = FALSE)
      }
    } else {
      n_agd_rows_check <- nrow(data$agd$data)
      # Not the row count. Mean-profile rank is not available here either,
      # because a nonlinear mean depends on each row's whole covariate
      # distribution and two rows with equal means but different spreads do
      # carry different constraints. Rows built from an identical integration
      # grid are a different matter: they are the identical function of the
      # comparator parameters whatever the link, so the second repeats the
      # first's likelihood term. Counting distinct grids is the bound the raw
      # row count is not, and it is what stops a duplicated set_agd() row from
      # suppressing this warning for the nonlinear families as well.
      n_distinct_check <- .agd_distinct_profiles(data)
      if (n_distinct_check < n_cov_check + 1L) {
        warning(sprintf(
          paste0("Relaxed model with %d AgD row(s), %d of them distinct, and ",
                 "%d covariate(s): the comparator side has %d parameters but ",
                 "only %d independent scalar aggregate outcome summaries. A ",
                 "row repeating another's integration grid contributes an ",
                 "identical likelihood term rather than a new constraint. The ",
                 "comparator coefficients cannot all be separated without ",
                 "prior information. %s"),
          n_agd_rows_check, n_distinct_check, n_cov_check, n_cov_check + 1L,
          n_distinct_check, remedy
        ), call. = FALSE)
      }
    }
  }

  prepared <- .mlumr_build_stan_data(
    data = data,
    family = family,
    link_info = link_info,
    prior_intercept = prior_intercept,
    prior_beta = prior_beta,
    prior_beta_comparator = prior_beta_comparator,
    prior_sigma = prior_sigma,
    surv_info = surv_info,
    prior_aux = prior_aux,
    prior_aux2 = prior_aux2,
    prior_smooth = prior_smooth,
    n_knots = n_knots,
    knots = knots,
    pred_times = pred_times,
    rmst_horizon = rmst_horizon,
    n_rmst_grid = n_rmst_grid,
    aux_by = aux_by,
    model = model,
    center = center,
    qr = qr
  )
  stan_data <- prepared$stan_data
  # The exact-fit guard judges the design the model fits, so it runs once
  # the covariates carry the model's own centers (zeros when it does not
  # center). Still before any backend is chosen or a model compiled.
  if (family == "normal") {
    .check_normal_residual_variation(data, link_info$link,
                                     center = stan_data$cov_center)
  }
  # A log-normal AFT is a normal model for log(t), so the same exact fit
  # makes the same improper posterior. The other log-location-scale
  # distributions carry a shape rather than a scale and are warned about.
  if (family == "survival") {
    .check_survival_scale_collapse(data, surv_info$distribution,
                                   aux_by = aux_by,
                                   center = stan_data$cov_center)
  }

  # Select Stan model. family_config gives the default prefix; the survival
  # family overrides it for the flexible-baseline distributions.
  stan_prefix <- if (family == "survival") {
    surv_info$stan_prefix
  } else {
    get_family_config(family)$stan_prefix
  }
  model_name <- paste0(stan_prefix, "_", model)

  .mlumr_log_fit_start(model_name, family, link_info$link, stan_data,
                       engine, seed_info, verbose)

  # Fit model
  result <- .mlumr_fit_backend(
    engine = engine,
    model_name = model_name,
    stan_data = stan_data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh,
    verbose = verbose,
    ...
  )

  fit <- result$native_fit
  draws <- result$draws
  chain_ids <- result$chain_ids
  summary_df <- result$summary_df
  n_divergent <- result$n_divergent
  n_max_td <- result$n_max_td

  # Resolved prior info for prior_summary(): carries both the user-specified
  # priors and the Stan-scale values actually used (post-autoscale), plus the
  # covariate SDs used for autoscaling, so the summary can reconstruct the
  # effective prior that was sampled.
  priors <- .mlumr_prior_metadata(
    data = data,
    family = family,
    model = model,
    prior_intercept = prior_intercept,
    prior_beta = prior_beta,
    prior_beta_comparator = prior_beta_comparator,
    prior_sigma = prior_sigma,
    prior_aux = prior_aux,
    prior_aux2 = prior_aux2,
    prior_smooth = prior_smooth,
    surv_info = surv_info,
    beta_fields = prepared$beta_fields,
    beta_comparator_fields = prepared$beta_comparator_fields,
    sd_x = prepared$sd_x
  )

  out <- list(
    stanfit = fit,
    draws = draws,
    # Real per-draw chain labels from the backend (NULL if unavailable); used by
    # diagnostics instead of reconstructing chain ids from row ordering.
    chain_ids = chain_ids,
    summary = summary_df,
    diagnostics = list(
      n_divergent = n_divergent,
      n_max_treedepth = n_max_td,
      n_chains_requested = result$n_chains_requested %||% as.integer(chains),
      n_chains_returned = result$n_chains_returned %||% as.integer(chains)
    ),
    data = data,
    family = family,
    link = link_info$link,
    link_code = link_info$code,
    model = model,
    model_name = model_name,
    # What was fitted, kept on the fit rather than passed to the sampler.
    # These describe the model, so they belong here: prior_sensitivity() reads
    # `distribution` and `surv_controls` to reproduce a fit, and predict() reads
    # `pred_times`. They were previously handed to .mlumr_fit_backend(), whose
    # signature ends in `...`, so instead of being stored they were forwarded
    # to the sampler: rstan::sampling() rejects unknown argument names outright
    # and CmdStanModel$sample() has no `...` to absorb them, which meant no
    # survival model could be fitted by either engine.
    distribution = if (!is.null(surv_info)) surv_info$distribution else NULL,
    surv_info = surv_info,
    pred_times = stan_data$pred_times,
    # Survival controls needed to faithfully reproduce this fit (e.g. in
    # prior_sensitivity refits); harmless/NULL for non-survival families.
    surv_controls = list(
      n_knots = n_knots,
      knots = if (!is.null(surv_info) && surv_info$kind == "flexible") knots else NULL,
      # Survival-only, like rmst_horizon/n_rmst_grid below: mlumr() rejects
      # `aux_by` for other families, so storing the formal default here would
      # make prior_sensitivity() replay it into a refit that then errors.
      aux_by = if (family == "survival") aux_by else NULL,
      # NA for a parametric baseline, and a refit rejects mspline_degree unless
      # the baseline is flexible. Store the absence as NULL, like `knots` above,
      # so every reader of surv_controls gets it right rather than only the one
      # that happens to normalize NA.
      mspline_degree = if (!is.null(surv_info) &&
                             surv_info$kind == "flexible") {
        surv_info$mspline_degree
      } else {
        NULL
      },
      pred_times = stan_data$pred_times,
      rmst_horizon = if (family == "survival")
        max(stan_data$rmst_grid_times) else NULL,
      n_rmst_grid = if (family == "survival")
        length(stan_data$rmst_grid_times) else NULL
    ),
    stan_data = stan_data,
    # Design-matrix controls that change the fitted parameterization (the
    # centered intercept, the QR-rotated coefficients). They have to travel
    # with the fit so a refit such as prior_sensitivity() reproduces the same
    # model rather than silently reverting to the defaults.
    model_controls = list(
      center = center,
      qr = qr
    ),
    engine = engine,
    priors = priors,
    sampling_args = list(
      chains = chains,
      iter = iter,
      warmup = warmup,
      seed = seed,
      # The effective settings when the backend reports them: a caller can
      # reach both through rstan's `control`, and a diagnostic that told them
      # to raise a limit they had already raised would be quoting the wrong
      # number. The cmdstanr backend takes them as arguments and cannot
      # differ, so it reports nothing and these fall back.
      adapt_delta = result$adapt_delta_used %||% adapt_delta,
      max_treedepth = result$max_treedepth_used %||% max_treedepth,
      # The full merged sampler control, so a replay reproduces every setting
      # and not just the two this list names. NULL for cmdstanr, which has no
      # `control` argument to reproduce.
      control = result$control_used,
      # Anything else the caller passed straight through to the backend, such
      # as `thin` or `init`. The NAMES only: an `init` can be a function or a
      # list of matrices, and a fit is not the place to keep a copy of one.
      # Recording that they existed is what lets a replay say so instead of
      # quietly running under different sampler settings; see
      # `prior_sensitivity()`, which cannot reproduce what it was never told.
      #
      # `control` arrives through `...` on the rstan path and IS stored, three
      # lines above, so listing it here would have a refit announce that it
      # was falling back to defaults for the one setting it reproduces in
      # full. Record what this list does not already carry.
      extra_backend_args = setdiff(
        names(list(...)),
        c("chains", "iter", "warmup", "seed", "adapt_delta", "max_treedepth",
          "control")
      )
    )
  )

  class(out) <- c("mlumr_fit", "list")

  check_diagnostics(out)

  mlumr_message("Fitting complete!", verbose = verbose)
  out
}

#' Build the Stan data list for mlumr()
#' @keywords internal
.mlumr_build_stan_data <- function(data, family, link_info, prior_intercept,
                                   prior_beta, prior_beta_comparator = NULL,
                                   prior_sigma,
                                   surv_info = NULL, prior_aux = NULL,
                                   prior_aux2 = NULL,
                                   prior_smooth = NULL, n_knots = 7L,
                                   knots = NULL,
                                   pred_times = NULL, rmst_horizon = NULL,
                                   n_rmst_grid = 100L, aux_by = NULL,
                                   model = "spfa", center = TRUE, qr = FALSE) {
  ipd_data <- data$ipd$data
  agd_data <- data$agd$data
  X_ipd <- as.matrix(ipd_data[, data$covariates])

  # Both coefficient blocks use the IPD SD as a common reference scale. For a
  # relaxed comparator block this is a convention, not a claim that its
  # coefficients are estimated from individual comparator observations.
  sd_x <- apply(X_ipd, 2, stats::sd)
  # A single IPD row makes stats::sd() undefined rather than zero. That is the
  # same situation as a constant covariate (no empirical scale to divide by),
  # and .warn_constant_ipd_covariates() has already said so, so record it as
  # zero variation. Leaving NA here aborted autoscaling with "missing value
  # where TRUE/FALSE needed" and made the stored prior metadata unreadable.
  sd_x[!is.finite(sd_x)] <- 0
  intercept_fields <- stan_prior_fields(prior_intercept)
  beta_fields <- stan_prior_fields_beta(
    prior_beta,
    data$n_covariates,
    sd_x = sd_x,
    covariate_names = data$covariates
  )
  # beta_comparator carries its own family, df, location and scale into Stan
  # (every relaxed model calls log_prior_vector() with the comparator-specific
  # *_dist and *_df, not the index ones), so a heavy-tailed comparator prior on
  # top of a normal index prior is fitted as requested. When the user leaves
  # prior_beta_comparator NULL we reuse the prior_beta values, which preserves
  # backward compatibility with pre-existing relaxed fits.
  beta_comparator_fields <- if (is.null(prior_beta_comparator)) {
    beta_fields
  } else {
    stan_prior_fields_beta(
      prior_beta_comparator,
      data$n_covariates,
      sd_x = sd_x,
      covariate_names = data$covariates
    )
  }

  stan_data <- list(
    n_ipd = nrow(ipd_data),
    n_cov = data$n_covariates,
    X_ipd = X_ipd,
    n_agd_rows = nrow(agd_data),
    n_int = data$n_int,
    X_int = data$integration_points,
    prior_intercept_mean = intercept_fields$mean,
    prior_intercept_sd = intercept_fields$sd,
    prior_intercept_dist = intercept_fields$dist,
    prior_intercept_df = intercept_fields$df,
    prior_beta_mean = as.array(beta_fields$mean),
    prior_beta_sd = as.array(beta_fields$sd),
    prior_beta_dist = beta_fields$dist,
    prior_beta_df = beta_fields$df,
    # Comparator-specific prior (location/scale + family/df), always passed;
    # SPFA Stan models silently ignore unused data entries, so this is a no-op
    # for SPFA. The family/df let a relaxed fit use a different prior family
    # (e.g. heavy-tailed Student-t) on the comparator coefficients than on the
    # index coefficients.
    prior_beta_comparator_mean = as.array(beta_comparator_fields$mean),
    prior_beta_comparator_sd = as.array(beta_comparator_fields$sd),
    prior_beta_comparator_dist = beta_comparator_fields$dist,
    prior_beta_comparator_df = beta_comparator_fields$df,
    link = link_info$code
  )

  if (family == "binomial") {
    stan_data$y_ipd <- .as_count_integer(ipd_data$.outcome)
    stan_data$n_agd <- array(.as_count_integer(agd_data$.n))
    stan_data$r_agd <- array(.as_count_integer(agd_data$.r))
  } else if (family == "normal") {
    bad_n <- is.null(agd_data$.n) || any(!is.finite(agd_data$.n)) ||
      any(agd_data$.n <= 0)
    if (nrow(agd_data) > 1L && bad_n) {
      stop("`outcome_n` is required when normal aggregate data contain ",
           "multiple rows, because those rows are population strata and must ",
           "be combined using their sample sizes.", call. = FALSE)
    }
    sigma_fields <- stan_prior_fields(prior_sigma)
    stan_data$y_ipd <- as.numeric(ipd_data$.outcome)
    stan_data$y_agd <- array(as.numeric(agd_data$.y))
    stan_data$se_agd <- array(as.numeric(agd_data$.se))
    # Target-population weights for the comparator estimand: sample-size weights
    # for multiple subgroup rows (so splitting one comparator population does
    # not change the standardized effect), or weight one for a single row when
    # outcome_n is omitted. These are mixing weights, not the likelihood's
    # 1/se^2 precision weights.
    n_agd_rows <- length(stan_data$y_agd)
    agd_n <- agd_data$.n
    stan_data$agd_weight <- if (!is.null(agd_n) &&
                                  all(is.finite(agd_n)) && all(agd_n > 0)) {
      as.array(as.numeric(agd_n))
    } else {
      as.array(rep(1, n_agd_rows))
    }
    stan_data$prior_sigma_location <- sigma_fields$mean
    stan_data$prior_sigma_scale <- sigma_fields$sd
    stan_data$prior_sigma_dist <- sigma_fields$dist
    stan_data$prior_sigma_df <- sigma_fields$df
  } else if (family == "poisson") {
    stan_data$y_ipd <- .as_count_integer(ipd_data$.outcome)
    stan_data$E_ipd <- as.numeric(ipd_data$.exposure)
    stan_data$r_agd <- array(.as_count_integer(agd_data$.r))
    stan_data$E_agd <- array(as.numeric(agd_data$.E))
  } else {
    stan_data <- .build_stan_data_survival(
      stan_data = stan_data, data = data, surv_info = surv_info,
      pred_times = pred_times, n_knots = n_knots,
      knots = knots,
      rmst_horizon = rmst_horizon, n_rmst_grid = n_rmst_grid,
      prior_aux = prior_aux, prior_aux2 = prior_aux2,
      prior_smooth = prior_smooth,
      n_strata = .resolve_aux_strata(aux_by)
    )
  }

  # Shared, all-family covariate centering and combined-design QR
  # reparameterization, mirroring `center = TRUE` / `QR` machinery.
  # Centering is likelihood-invariant (the intercept absorbs the shift), so the
  # estimands are unchanged. It is NOT prior-invariant: the intercept prior is
  # placed on the centered intercept, so the center must not depend on tuning
  # controls like `n_int` (see .mlumr_center_covariates(), which weights by AgD
  # rows, not row*point, precisely so the induced prior does not move). QR is an
  # affine reparameterization of the (intercepts + covariates) design that
  # decorrelates the sampling geometry. These leave the likelihood family and
  # response-scale estimands unchanged, but a fixed numerical prior need not
  # represent the same prior after a change of parameterization.
  agd_means <- as.matrix(agd_data[, paste0(data$covariates, "_mean"),
                                  drop = FALSE])
  stan_data <- .mlumr_center_covariates(
    stan_data, center = center, family = family, agd_means = agd_means
  )
  stan_data <- .mlumr_qr_design(stan_data, model = model, qr = qr)

  list(stan_data = stan_data,
       beta_fields = beta_fields,
       beta_comparator_fields = beta_comparator_fields,
       sd_x = sd_x)
}

#' Population weights for the AgD rows used in covariate centering
#'
#' Returns one weight per aggregate row, taken from the family's comparator
#' weight field (`n_agd`, `agd_weight`, `E_agd`), or the pseudo-individual count
#' for survival. Falls back to equal weights when no usable field is present.
#' Weights must be positive and finite, and must sum over a split subgroup to
#' the same total as the unsplit one, which is what makes the center invariant
#' to how the aggregate evidence is tabulated.
#'
#' @param stan_data The assembled Stan data list.
#' @param family Outcome family name.
#' @param n_agd_rows Number of aggregate rows.
#' @return Numeric vector of length `n_agd_rows`.
#' @keywords internal
.agd_center_weights <- function(stan_data, family, n_agd_rows) {
  fallback <- rep(1, n_agd_rows)
  cfg <- tryCatch(get_family_config(family), error = function(e) NULL)
  field <- if (is.null(cfg)) NULL else cfg$comp_weight_field
  w <- if (!is.null(field)) stan_data[[field]] else NULL
  # Survival has no comparator weight field; the pseudo-IPD count is the
  # equivalent population size. `stan_data$n_agd` is the TOTAL number of
  # pseudo-individuals, so falling through to the equal-weight fallback would
  # give every AgD row the same weight regardless of how many pseudo-individuals
  # it actually contributes. That is exactly the tabulation dependence this
  # function exists to remove: two comparator arms of 300 and 60 would be
  # centered as if they were 180 each. `agd_arm` maps each pseudo-individual to
  # its row, so tabulating it recovers the true per-row population.
  if (is.null(w) && identical(family, "survival")) {
    arm <- stan_data$agd_arm
    # `agd_count` is the per-row multiplicity used by tie aggregation, which
    # keeps one row per distinct (arm, time, start, delay, status) key. The
    # population a row represents is then the number of pseudo-individuals it
    # stands for, not the number of retained rows, so tabulate the arm map
    # expanded by its multiplicities. Absent tie aggregation every count is one
    # and this is exactly `tabulate(arm)`. Reading the counts here rather than
    # requiring the collapse to run after centering is what makes the weights
    # independent of that ordering: getting it wrong would silently change the
    # center, and with it the induced raw-scale intercept prior.
    cnt <- stan_data$agd_count
    w <- if (!is.null(arm) && n_agd_rows >= 1L) {
      arm_int <- as.integer(arm)
      if (!is.null(cnt)) {
        if (length(cnt) != length(arm_int) || !all(is.finite(cnt)) ||
              any(cnt < 1)) {
          stop("`stan_data$agd_count` must hold one positive multiplicity per ",
               "retained AgD row.", call. = FALSE)
        }
        arm_int <- rep(arm_int, times = .as_count_integer(cnt))
      }
      counts <- tabulate(arm_int, nbins = n_agd_rows)
      # A zero means an arm carries no reconstructed pseudo-individuals, which
      # is a data problem rather than a weighting choice. The guard below would
      # quietly revert to equal weights and change the center; say so instead.
      if (any(counts <= 0L)) {
        warning("Some aggregate survival arm(s) have no reconstructed ",
                "pseudo-individuals, so covariate centering falls back to ",
                "equal row weights. Check the arm labels on the pseudo-IPD.",
                call. = FALSE)
      }
      counts
    } else {
      stan_data$n_agd
    }
  }
  if (is.null(w)) return(fallback)
  w <- as.numeric(w)
  if (length(w) == 1L && n_agd_rows > 1L) w <- rep(w / n_agd_rows, n_agd_rows)
  if (length(w) != n_agd_rows || !all(is.finite(w)) || any(w <= 0)) {
    return(fallback)
  }
  w
}


#' Center IPD + integration covariates about their pooled mean (all families)
#'
#' Matches `center = TRUE` default. The intercept then represents the
#' baseline at the average covariate rather than at covariate = 0, removing the
#' intercept<->slope collinearity that forces deep NUTS trajectories on
#' real-scale covariates. The likelihood is invariant because `X_ipd` and the
#' integration grid are shifted by the same `xbar` and the intercept absorbs the
#' shift. A fixed numerical intercept prior is placed on the centered intercept,
#' however, so centering need not leave the posterior unchanged. `cov_center`
#' is always stored (zeros when `center = FALSE`) so predict()/conditional_effects()
#' can map raw-scale covariate values onto the (possibly centered) model scale.
#' @keywords internal
.mlumr_center_covariates <- function(stan_data, center = TRUE,
                                     family = "binomial", agd_means = NULL) {
  if (is.null(stan_data$X_ipd) || is.null(stan_data$X_int)) {
    return(stan_data)
  }
  X_ipd <- as.matrix(stan_data$X_ipd)              # [n_ipd, n_cov]
  X_int <- stan_data$X_int                         # [n_agd_rows, n_int, n_cov]
  n_cov <- ncol(X_ipd)
  if (center) {
    n_ipd_rows <- nrow(X_ipd)
    n_agd_rows <- dim(X_int)[1]
    # Use the declared AgD covariate means when the model builder supplies them.
    # Falling back to realized grid means keeps this helper usable in isolation.
    # The production center therefore does not move with the QMC resolution,
    # which would otherwise move the induced raw-scale intercept prior.
    if (is.null(agd_means)) {
      agd_means <- apply(X_int, c(1, 3), mean)
    }
    agd_row_means <- matrix(as.numeric(agd_means), nrow = n_agd_rows,
                            ncol = n_cov)
    # Weight each AgD row by the population it represents, not by 1. Row counts
    # are a property of how the aggregate evidence happens to be TABULATED:
    # splitting one comparator subgroup into two statistically equivalent rows
    # would otherwise change `n_agd_rows`, move `xbar`, and therefore change the
    # induced raw-scale intercept prior even though the likelihood and the
    # target estimand are unchanged. Sample-size weights are invariant under any
    # such split or merge, because the parts sum to the whole.
    w_agd <- .agd_center_weights(stan_data, family, n_agd_rows)
    xbar <- (n_ipd_rows * colMeans(X_ipd) +
               colSums(agd_row_means * w_agd)) /
      (n_ipd_rows + sum(w_agd))
    stan_data$X_ipd <- sweep(X_ipd, 2, xbar)
    stan_data$X_int <- sweep(X_int, 3, xbar)
    stan_data$cov_center <- xbar
  } else {
    stan_data$cov_center <- rep(0, n_cov)
  }
  stan_data
}

#' Build the combined (intercepts + covariates) design and optional thin-QR
#'
#' Mirrors QR machinery: the design matrix `D` stacks the IPD rows and
#' all AgD integration rows, with leading dummy columns for the index and
#' comparator intercepts followed by the (centered) covariate columns. SPFA uses
#' one shared covariate block (`nB = 2 + n_cov`); the relaxed model uses
#' treatment-specific blocks (`nB = 2 + 2 * n_cov`). When `qr = TRUE` the design
#' is replaced by the scaled thin-QR factor `Q` (`Q = qr.Q(D) * sqrt(N - 1)`) and
#' `R_inv = solve(qr.R(D) / sqrt(N - 1))` is returned so Stan can recover the
#' original-scale coefficients via `allbeta = R_inv * beta_tilde`. When
#' `qr = FALSE`, `Xq_*` is the raw design `D` and `R_inv` is the identity, so
#' `allbeta = beta_tilde` and the linear predictor is unchanged. The original
#' (centered) `X_ipd` / `X_int` are kept for the generated-quantities block.
#' @keywords internal
.mlumr_qr_design <- function(stan_data, model = "spfa", qr = FALSE) {
  if (is.null(stan_data$X_ipd) || is.null(stan_data$X_int)) {
    return(stan_data)
  }
  X_ipd <- as.matrix(stan_data$X_ipd)              # [n_ipd, n_cov], centered
  X_int <- stan_data$X_int                         # [n_agd_rows, n_int, n_cov]
  n_ipd <- nrow(X_ipd)
  n_cov <- ncol(X_ipd)
  n_agd <- dim(X_int)[1]
  n_int <- dim(X_int)[2]
  # Arm-major flatten of the integration grid: row (k-1)*n_int + m = X_int[k, m, ].
  X_int_flat <- matrix(aperm(X_int, c(2, 1, 3)), nrow = n_agd * n_int, ncol = n_cov)

  zero_ipd <- matrix(0, n_ipd, n_cov)
  zero_int <- matrix(0, n_agd * n_int, n_cov)
  if (identical(model, "relaxed")) {
    # [I_index, I_comparator, beta_index cols, beta_comparator cols]
    nB <- 2L + 2L * n_cov
    d_ipd <- cbind(1, 0, X_ipd, zero_ipd)
    d_int <- cbind(0, 1, zero_int, X_int_flat)
  } else {
    # SPFA: [I_index, I_comparator, shared beta cols]
    nB <- 2L + n_cov
    d_ipd <- cbind(1, 0, X_ipd)
    d_int <- cbind(0, 1, X_int_flat)
  }
  design <- rbind(d_ipd, d_int)
  n_rows <- nrow(design)

  if (qr) {
    # A thin QR needs full column rank: R is inverted to recover the
    # original-scale coefficients. set_ipd() permits a constant covariate with a
    # warning, and in a relaxed model that column is exactly collinear with its
    # own intercept, so the combined design can be rank deficient even though
    # the model is still estimable from the coefficient priors when qr = FALSE.
    # Fail here with something the user can act on rather than inside solve().
    if (n_rows < nB) {
      stop(sprintf(paste0("`qr = TRUE` needs at least as many rows as design ",
                          "columns, but the combined design has %d row(s) and ",
                          "%d column(s). Use `qr = FALSE`."),
                   n_rows, nB), call. = FALSE)
    }
    design_rank <- qr(design)$rank
    if (design_rank < nB) {
      stop(sprintf(paste0("`qr = TRUE` needs a full-rank design, but the ",
                          "combined (intercepts + covariates) design has rank ",
                          "%d of %d columns. A constant or collinear covariate ",
                          "is the usual cause; a constant covariate is exactly ",
                          "collinear with the intercept. Drop it, or use ",
                          "`qr = FALSE`, which does not invert the design."),
                   design_rank, nB), call. = FALSE)
    }
    qr_decomp <- qr(design)
    scale_factor <- sqrt(n_rows - 1)
    q_mat <- qr.Q(qr_decomp) * scale_factor
    r_mat <- qr.R(qr_decomp) / scale_factor
    r_inv <- solve(r_mat)
    xq_ipd <- q_mat[seq_len(n_ipd), , drop = FALSE]
    xq_int_flat <- q_mat[(n_ipd + 1L):n_rows, , drop = FALSE]
  } else {
    xq_ipd <- d_ipd
    xq_int_flat <- d_int
    r_inv <- diag(nB)
  }
  # Reshape the AgD design rows back to [n_agd, n_int, nB] (inverse of the
  # arm-major flatten above) for Stan's `array[n_agd_rows] matrix[n_int, nB]`.
  xq_int <- aperm(array(xq_int_flat, dim = c(n_int, n_agd, nB)), c(2, 1, 3))

  stan_data$qr <- as.integer(qr)
  stan_data$nB <- nB
  stan_data$Xq_ipd <- xq_ipd
  stan_data$Xq_int <- xq_int
  stan_data$R_inv <- r_inv
  stan_data
}

#' Rank of the AgD comparator covariate design
#'
#' The relaxed model's `mu_comparator` and `beta_comparator` are informed only
#' by the AgD likelihood, which contributes one term per AgD row evaluated at
#' that row's integration grid. The number of comparator parameters those terms
#' can separate is therefore the rank of the per-row mean covariate profiles
#' augmented with an intercept column, NOT the number of rows: rows that repeat
#' the same covariate summaries add likelihood terms but no new direction.
#'
#' Uses the declared aggregate covariate means, which define the identity-link
#' design exactly and do not vary with integration resolution.
#'
#' @param data An `mlumr_data` object with integration points.
#' @return Integer rank, at least 1.
#' @keywords internal
.agd_covariate_rank <- function(data) {
  covs <- data$covariates
  ipd_cov <- data$ipd$data[, covs, drop = FALSE]
  ref_sd <- apply(as.matrix(ipd_cov), 2L, stats::sd)
  .profile_rank(.agd_mean_profiles(data), ref_sd)
}

#' Numerical rank of the aggregate mean profiles
#'
#' The companion to [.agd_covariate_rank()] that answers the DIFFERENT question
#' of whether the directions exist at all, rather than whether they are spread
#' widely enough to be informative in practice.
#' @param data An `mlumr_data` object.
#' @return Integer rank including the intercept.
#' @keywords internal
.agd_covariate_numeric_rank <- function(data) {
  covs <- data$covariates
  ipd_cov <- data$ipd$data[, covs, drop = FALSE]
  ref_sd <- apply(as.matrix(ipd_cov), 2L, stats::sd)
  .profile_numeric_rank(.agd_mean_profiles(data), ref_sd)
}

#' Map `aux_by` onto the Stan `n_strata` switch
#'
#' There are only ever two studies in an unanchored comparison, so `".study"`
#' means 2 and `"none"` means 1. `NULL` resolves to the `".study"` default and
#' therefore also gives 2; only `"none"` asks for a single shared stratum. Named after multinma's argument so the
#' concept transfers, but deliberately not accepting `".trt"`: each study
#' contributes a single arm here, so stratifying by treatment and by study are
#' the same thing.
#' @param aux_by `NULL`, `".study"`, or `"none"`.
#' @return Integer number of baseline strata (1 or 2).
#' @keywords internal
.resolve_aux_strata <- function(aux_by) {
  # NULL means ".study", NOT "share". This follows multinma exactly
  # (multinma/R/nma.R: `if (quo_is_null(aux_by)) aux_by <- ".study"`, and
  # get_aux_id(add_study = TRUE) forces .study into the grouping regardless), so
  # a multinma user writing aux_by = NULL gets the model they expect rather than
  # its opposite. Sharing one baseline across studies has no multinma spelling
  # because multinma cannot do it, so it gets its own explicit name here.
  if (is.null(aux_by)) return(2L)
  if (!is.character(aux_by) || length(aux_by) != 1L) {
    stop("`aux_by` must be NULL or \".study\" (a baseline per study), or ",
         "\"none\" (one shared baseline).", call. = FALSE)
  }
  if (identical(aux_by, ".study")) return(2L)
  if (identical(aux_by, "none")) return(1L)
  if (identical(aux_by, ".trt")) {
    stop("`aux_by = \".trt\"` is the same as \".study\" here: each study ",
         "contributes a single arm, so stratifying by treatment and by study ",
         "give the same two strata. Use \".study\".", call. = FALSE)
  }
  stop("`aux_by` must be NULL or \".study\" (a baseline per study, the ",
       "default, as in multinma), or \"none\" (one shared baseline). Got \"",
       aux_by, "\".", call. = FALSE)
}


#' Validate the two-source study contract of the survival Stan models
#' @keywords internal
.validate_survival_studies <- function(data, aux_by) {
  if (identical(aux_by, "none")) return(invisible(TRUE))
  ipd_studies <- unique(as.character(data$ipd$data$.study))
  agd_studies <- unique(as.character(data$agd$pseudo_ipd$.study))
  if (length(ipd_studies) != 1L || length(agd_studies) != 1L) {
    stop(
      "Survival fits with the default `aux_by = \".study\"` contract require ",
      "exactly one index study and one comparator study. The current Stan ",
      "models have two source-specific baselines and cannot represent multiple ",
      "studies within either source. Use `aux_by = \"none\"` only if complete ",
      "baseline pooling across those studies is scientifically intended.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Augment the Stan data list with survival-specific arrays
#'
#' @param stan_data The partially built Stan data list to add to.
#' @param data The combined `mlumr_data` object holding the IPD and pseudo-IPD.
#' @param surv_info Distribution metadata from `.survival_distribution_info()`.
#' @param pred_times Prediction grid, or `NULL` for the default grid.
#' @param n_knots Number of internal knots for a flexible baseline.
#' @param knots Explicit [make_knots()] result, or `NULL` to derive them.
#' @param rmst_horizon RMST restriction time, or `NULL` for the default.
#' @param n_rmst_grid Number of RMST integration points.
#' @param prior_aux Prior on the parametric auxiliary shape parameters.
#' @param prior_aux2 Prior on the second generalized-gamma auxiliary parameter,
#'   or `NULL` to reuse `prior_aux`.
#' @param prior_smooth Prior on the flexible-baseline smoothing SD.
#' @param n_strata Number of baseline strata (1 or 2), from `.resolve_aux_strata()`.
#' @return `stan_data` with the survival arrays, bases and grids added.
#' @keywords internal
.build_stan_data_survival <- function(stan_data, data, surv_info, pred_times,
                                      n_knots, knots = NULL, rmst_horizon,
                                      n_rmst_grid = 100L,
                                      prior_aux, prior_aux2 = NULL,
                                      prior_smooth, n_strata = 1L) {
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  arm_summary <- data$agd$data
  # 1 = one baseline shared by both studies; 2 = one per study (see `aux_by`).
  stan_data$n_strata <- as.integer(n_strata)

  # Covariate centering is now applied for every family by the shared
  # .mlumr_center_covariates() step in .mlumr_build_stan_data() (matching
  # `center = TRUE` default), so it is no longer done here.

  # IPD survival arrays (index treatment)
  stan_data$ipd_time <- as.numeric(ipd$.time)
  stan_data$ipd_start_time <- as.numeric(ipd$.start_time)
  stan_data$ipd_delay_time <- as.numeric(ipd$.delay_time)
  stan_data$ipd_status <- as.integer(ipd$.status)

  # Comparator pseudo-IPD arrays + map each pseudo-individual to its arm
  stan_data$n_agd <- nrow(pseudo)
  stan_data$agd_time <- as.numeric(pseudo$.time)
  stan_data$agd_start_time <- as.numeric(pseudo$.start_time)
  stan_data$agd_delay_time <- as.numeric(pseudo$.delay_time)
  stan_data$agd_status <- as.integer(pseudo$.status)
  stan_data$agd_arm <- as.integer(match(pseudo$.arm, arm_summary$.arm))

  # Prediction + RMST grids (avoid t = 0 for the hazard-bearing pred grid).
  # Sort + unique so curve/median interpolation sees an increasing grid.
  max_time <- max(c(ipd$.time, pseudo$.time))
  if (is.null(pred_times)) {
    pred_times <- seq(max_time / 50, max_time, length.out = 50)
  }
  pred_times <- sort(unique(as.numeric(pred_times)))
  stan_data$n_pred_times <- length(pred_times)
  stan_data$pred_times <- pred_times

  # A flexible baseline is extrapolated as a constant hazard past each study's
  # own upper boundary knot, so with unequal follow-up a pooled-maximum default
  # horizon would make the HEADLINE RMST extrapolate the shorter study by
  # construction. A warning is not enough for a default estimand: default
  # instead to the common empirical support, and let a longer horizon be an
  # explicit choice (which still warns).
  horizon <- rmst_horizon
  if (is.null(horizon)) {
    horizon <- if (surv_info$kind == "flexible" && n_strata > 1L) {
      min(max(ipd$.time), max(pseudo$.time))
    } else {
      max_time
    }
  }
  # RMST is the trapezoidal integral of the survival curve on this grid; more
  # points give a finer (more accurate) approximation. Default 100 is adequate
  # for smooth curves; raise `n_rmst_grid` for sharp early hazards, long
  # horizons, or high-curvature flexible-baseline tails.
  rmst_grid <- seq(0, horizon, length.out = n_rmst_grid)
  stan_data$n_rmst_grid <- length(rmst_grid)
  stan_data$rmst_grid_times <- rmst_grid

  if (surv_info$kind == "parametric") {
    stan_data$dist <- surv_info$dist_code
    aux_fields <- stan_prior_fields(prior_aux)
    stan_data$prior_aux_location <- aux_fields$mean
    stan_data$prior_aux_scale <- aux_fields$sd
    stan_data$prior_aux_dist <- aux_fields$dist
    stan_data$prior_aux_df <- aux_fields$df
    # The second generalized-gamma shape has its own specification, defaulted to
    # `prior_aux` by the caller. Stan has always read these four slots
    # separately; they were simply fed the same numbers.
    aux2_fields <- stan_prior_fields(prior_aux2 %||% prior_aux)
    stan_data$prior_aux2_location <- aux2_fields$mean
    stan_data$prior_aux2_scale <- aux2_fields$sd
    stan_data$prior_aux2_dist <- aux2_fields$dist
    stan_data$prior_aux2_df <- aux2_fields$df
  } else {
    # One basis per baseline stratum. With `aux_by = ".study"` each study gets
    # its own boundary and internal knots over its OWN observed support, which
    # is what multinma's default `type = "quantile"` does. This is an
    # identification requirement, not a nicety: with a single pooled basis whose
    # upper boundary is the longest study's last time, a shorter study can have
    # basis functions with no support over its observed period. Scaling that
    # study's observed coefficients by c, moving the surplus simplex mass into
    # an unsupported column, and replacing mu by mu - log(c) leaves both
    # h0(t)exp(eta) and H0(t)exp(eta) unchanged at every observed time, so the
    # likelihood is exactly flat along that direction and the intercept is set
    # by the prior rather than the data. Per-study boundaries remove it: every
    # column is supported and H0_s is pinned at a time the study actually
    # observed.
    degree <- surv_info$mspline_degree
    if (n_strata > 1L) {
      if (is.null(knots)) {
        specs <- .matched_per_study_bases(ipd, pseudo, n_knots, degree)
        spec_idx <- specs$index
        spec_cmp <- specs$comparator
      } else {
        if (!is.list(knots) ||
            !all(c("index", "comparator") %in% names(knots))) {
          stop("With `aux_by = \".study\"`, `knots` must be a named list ",
               "with `index` and `comparator` knot specifications.",
               call. = FALSE)
        }
        k_idx <- .validate_user_knots(knots$index, max(ipd$.time), "index")
        k_cmp <- .validate_user_knots(knots$comparator, max(pseudo$.time),
                                      "comparator")
        spec_idx <- .build_mspline_basis(k_idx, degree)
        spec_cmp <- .build_mspline_basis(k_cmp, degree)
        if (spec_idx$n_scoef != spec_cmp$n_scoef) {
          stop("The index and comparator knot specifications must produce ",
               "the same number of spline coefficients.", call. = FALSE)
        }
        .assert_basis_support(spec_idx, max(ipd$.time), "index",
                              ipd$.delay_time, ipd$.time,
                              ipd$.time[ipd$.status == 1])
        .assert_basis_support(spec_cmp, max(pseudo$.time), "comparator",
                              pseudo$.delay_time, pseudo$.time,
                              pseudo$.time[pseudo$.status == 1])
      }
    } else {
      observed_max <- max(c(ipd$.time, pseudo$.time))
      knots_i <- if (is.null(knots)) {
        make_knots(data, n_knots = n_knots)
      } else {
        .validate_user_knots(knots, observed_max, "shared")
      }
      spec_idx <- spec_cmp <- .build_mspline_basis(knots_i, degree)
      # The stratified branch above asserts basis support and this one did not,
      # which left the shared baseline able to recreate the very ridge the
      # comment above describes. .validate_user_knots() only requires the upper
      # boundary to reach the last observed time, not to stop near it, so
      # boundary = c(0, 100) with all times under 10 gives columns supported
      # only on (10, 100]. Simplex mass can move onto those and trade one for
      # one against the intercept, leaving the likelihood exactly flat along
      # that direction. Per-study boundaries are what remove it when strata
      # differ; when they do not, this assertion is what remains.
      .assert_basis_support(spec_idx, observed_max, "shared",
                            c(ipd$.delay_time, pseudo$.delay_time),
                            c(ipd$.time, pseudo$.time),
                            c(ipd$.time[ipd$.status == 1],
                              pseudo$.time[pseudo$.status == 1]))
      # Pooled support says every column carries likelihood for SOMEBODY. It
      # cannot say the two studies are tied to each other, and with one shared
      # simplex and an intercept each they have to be: exposure on disjoint
      # column sets leaves the weights free to be rescaled against the
      # intercepts with the likelihood exactly flat along that direction.
      .assert_shared_basis_identified(
        spec_idx,
        list(index = list(observed_max = max(ipd$.time),
                          entry = ipd$.delay_time,
                          exit = ipd$.time,
                          event = ipd$.time[ipd$.status == 1]),
             comparator = list(observed_max = max(pseudo$.time),
                               entry = pseudo$.delay_time,
                               exit = pseudo$.time,
                               event = pseudo$.time[pseudo$.status == 1])))
    }
    spec <- spec_idx
    if (spec$n_scoef < 2L) {
      # Only reachable at degree 0 (`distribution = "pexp"`), where n_scoef is
      # the number of intervals: with no internal knot that is a single constant
      # hazard, and the random-walk smoothing prior has no increment to be
      # defined on. A cubic M-spline with no internal knots still has
      # degree + 1 = 4 coefficients and is fine.
      stop("Baseline basis has < 2 coefficients, so the random-walk smoothing ",
           "prior has no increments to smooth. A degree-", spec$degree,
           " basis needs at least one internal knot for that; use ",
           "`n_knots >= 1`, or `distribution = \"exponential\"` if a single ",
           "constant hazard is what you want. Got n_scoef = ", spec$n_scoef,
           ".", call. = FALSE)
    }
    # The M-spline baseline is extrapolated with a constant hazard past the
    # upper boundary knot. Warn if predictions/RMST extend past follow-up.
    upper <- min(spec_idx$boundary[2], spec_cmp$boundary[2])
    if (max(pred_times) > upper || max(rmst_grid) > upper) {
      which_arm <- if (n_strata > 1L && spec_cmp$boundary[2] < spec_idx$boundary[2]) {
        "the comparator study's baseline (its follow-up is the shorter one)"
      } else if (n_strata > 1L && spec_idx$boundary[2] < spec_cmp$boundary[2]) {
        "the index study's baseline (its follow-up is the shorter one)"
      } else {
        "the M-spline baseline"
      }
      warning("Flexible-baseline prediction/RMST times extend beyond the ",
              "largest time that study observed (",
              format(upper, digits = 4L), "); ", which_arm,
              " is extrapolated as constant there, so extrapolated estimates ",
              "are unreliable. Reduce `pred_times` / `rmst_horizon`.",
              call. = FALSE)
    }
    stan_data$n_scoef <- spec$n_scoef
    # The IPD is entirely the index study and the pseudo-IPD entirely the
    # comparator, so each likelihood block already uses only its own stratum's
    # basis. The prediction/RMST grids are evaluated on both.
    stan_data$b_ipd <- .eval_basis(spec_idx, ipd$.time, integral = FALSE)
    stan_data$ib_ipd <- .eval_basis(spec_idx, ipd$.time, integral = TRUE)
    stan_data$ib_ipd_start <- .eval_basis(spec_idx, ipd$.start_time, integral = TRUE)
    stan_data$ib_ipd_delay <- .eval_basis(spec_idx, ipd$.delay_time, integral = TRUE)
    stan_data$b_agd <- .eval_basis(spec_cmp, pseudo$.time, integral = FALSE)
    stan_data$ib_agd <- .eval_basis(spec_cmp, pseudo$.time, integral = TRUE)
    stan_data$ib_agd_start <- .eval_basis(spec_cmp, pseudo$.start_time, integral = TRUE)
    stan_data$ib_agd_delay <- .eval_basis(spec_cmp, pseudo$.delay_time, integral = TRUE)
    stan_data$pred_basis <- .eval_basis(spec_idx, pred_times, integral = FALSE)
    stan_data$pred_ibasis <- .eval_basis(spec_idx, pred_times, integral = TRUE)
    stan_data$rmst_ibasis <- .eval_basis(spec_idx, rmst_grid, integral = TRUE)
    stan_data$pred_basis_cmp <- .eval_basis(spec_cmp, pred_times, integral = FALSE)
    stan_data$pred_ibasis_cmp <- .eval_basis(spec_cmp, pred_times, integral = TRUE)
    stan_data$rmst_ibasis_cmp <- .eval_basis(spec_cmp, rmst_grid, integral = TRUE)
    # The constant-hazard centering and RW1 weights are properties of a basis,
    # so they are per-stratum once the bases differ.
    stan_data$lscoef_prior_mean <- cbind(.mspline_constant_hazard(spec_idx),
                                         .mspline_constant_hazard(spec_cmp))[, seq_len(n_strata),
                                                                             drop = FALSE]
    stan_data$lscoef_weights <- cbind(.rw1_prior_weights(spec_idx),
                                      .rw1_prior_weights(spec_cmp))[, seq_len(n_strata),
                                                                    drop = FALSE]
    smooth_fields <- stan_prior_fields(prior_smooth)
    stan_data$prior_sigma_smooth_location <- smooth_fields$mean
    stan_data$prior_sigma_smooth_scale <- smooth_fields$sd
    stan_data$prior_sigma_smooth_dist <- smooth_fields$dist
    stan_data$prior_sigma_smooth_df <- smooth_fields$df
  }
  stan_data
}


#' Validate user-supplied flexible-baseline knots
#' @keywords internal
.validate_user_knots <- function(knots, max_time, label) {
  if (!is.list(knots) ||
      !all(c("internal", "boundary") %in% names(knots))) {
    stop("The ", label, " knot specification must contain `internal` and ",
         "`boundary`.", call. = FALSE)
  }
  internal <- knots$internal
  boundary <- knots$boundary
  valid_internal <- is.numeric(internal) && all(is.finite(internal)) &&
    (length(internal) < 2L || all(diff(internal) > 0))
  valid_boundary <- is.numeric(boundary) && length(boundary) == 2L &&
    all(is.finite(boundary)) && boundary[1] == 0 && boundary[2] >= max_time
  if (!valid_internal || !valid_boundary ||
      any(internal <= boundary[1] | internal >= boundary[2])) {
    stop("The ", label, " knots must have strictly increasing finite internal ",
         "knots inside `boundary = c(0, upper)`, with `upper` covering all ",
         "observed times for that baseline.", call. = FALSE)
  }
  list(internal = internal, boundary = boundary,
       n_knots = length(internal))
}

#' Validate `adapt_delta`, wherever it arrived from
#'
#' Shared by the argument validator and the rstan control merge, so a setting
#' is checked the same way whether it came in as an argument or inside
#' `control`. It reached the sampler unchecked through the second door.
#'
#' @param adapt_delta The value to check.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
.validate_mlumr_adapt_delta <- function(adapt_delta) {
  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L ||
        !is.finite(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) {
    stop("`adapt_delta` must be a single finite number between 0 and 1.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Validate mlumr() sampler controls before backend dispatch
#' @keywords internal
.validate_mlumr_sampling_args <- function(chains, iter, warmup, seed,
                                          adapt_delta, max_treedepth,
                                          refresh) {
  .validate_mlumr_integer(chains, "chains", lower = 1L)
  .validate_mlumr_integer(iter, "iter", lower = 1L)
  .validate_mlumr_integer(warmup, "warmup", lower = 0L)
  if (warmup >= iter) {
    stop("`warmup` must be smaller than `iter` so post-warmup draws remain.",
         call. = FALSE)
  }
  if (!is.null(seed)) {
    .validate_mlumr_integer(seed, "seed", lower = 0L)
  }
  .validate_mlumr_adapt_delta(adapt_delta)
  .validate_mlumr_integer(max_treedepth, "max_treedepth", lower = 1L)
  .validate_mlumr_integer(refresh, "refresh", lower = 0L)
  invisible(TRUE)
}


#' Validate an integer-like mlumr() argument
#' @keywords internal
.validate_mlumr_integer <- function(x, name, lower) {
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    is.finite(x) &&
    x == floor(x) &&
    x >= lower &&
    x <= .Machine$integer.max
  if (!valid) {
    # Enforce the upper bound too: without it a value such as seed = 2^31 passes
    # every finite/floor check here and only becomes NA_integer_ later at
    # as.integer(), silently discarding the seed.
    stop(sprintf("`%s` must be a single integer >= %d and <= %.0f.",
                 name, lower, .Machine$integer.max), call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate survival-specific prediction grids and spline controls
#' @keywords internal
.validate_survival_controls <- function(pred_times, rmst_horizon,
                                        mspline_degree, n_knots,
                                        n_rmst_grid = 100L,
                                        distribution = NULL,
                                        knots = NULL) {
  valid_grid <- is.numeric(n_rmst_grid) && length(n_rmst_grid) == 1L &&
    is.finite(n_rmst_grid) && n_rmst_grid >= 2 &&
    n_rmst_grid == floor(n_rmst_grid)
  if (!valid_grid) {
    stop("`n_rmst_grid` must be a single integer >= 2 (RMST trapezoid nodes).",
         call. = FALSE)
  }
  if (!is.null(pred_times)) {
    valid <- is.numeric(pred_times) && length(pred_times) >= 1L &&
      all(is.finite(pred_times)) && all(pred_times > 0)
    if (!valid) {
      stop("`pred_times` must be finite, positive numbers.", call. = FALSE)
    }
  }
  if (!is.null(rmst_horizon)) {
    valid <- is.numeric(rmst_horizon) && length(rmst_horizon) == 1L &&
      is.finite(rmst_horizon) && rmst_horizon > 0
    if (!valid) {
      stop("`rmst_horizon` must be a single finite, positive number.",
           call. = FALSE)
    }
  }
  if (!is.null(mspline_degree)) {
    valid <- is.numeric(mspline_degree) && length(mspline_degree) == 1L &&
      is.finite(mspline_degree) && mspline_degree >= 0 &&
      mspline_degree == floor(mspline_degree)
    if (!valid) {
      stop("`mspline_degree` must be a non-negative integer.", call. = FALSE)
    }
  }
  # `n_knots` only ever reaches make_knots(), and only when no custom `knots`
  # were supplied. Checking it regardless rejected a perfectly good custom knot
  # specification because an argument the fit never reads was out of range.
  # The custom basis is validated on its own terms by .validate_user_knots()
  # and the resolved `n_scoef < 2` check.
  if (!is.null(knots)) {
    return(invisible(TRUE))
  }
  valid_knots <- is.numeric(n_knots) && length(n_knots) == 1L &&
    is.finite(n_knots) && n_knots >= 0 && n_knots == floor(n_knots) &&
    n_knots <= 50
  if (!valid_knots) {
    stop("`n_knots` must be an integer in [0, 50]. (Flexible baselines rarely ",
         "need more than a handful of internal knots; the cap also blocks ",
         "accidental enormous allocations.)", call. = FALSE)
  }
  # `n_knots = 0` is in range for make_knots(), which is also called directly.
  # Whether it yields a fittable baseline depends on the DEGREE, because
  # `n_scoef = length(internal) + degree + 1`:
  #   pexp    (degree 0) -> 0 + 0 + 1 = 1 coefficient. The random-walk smoothing
  #                         prior is defined on the DIFFERENCES between
  #                         coefficients, so one coefficient has no increments
  #                         and the model cannot be built.
  #   mspline (degree 3) -> 0 + 3 + 1 = 4 coefficients, with three RW1
  #                         increments. That is a perfectly ordinary single-
  #                         interval cubic M-spline and must NOT be rejected.
  # Reject only the case that genuinely cannot work, and leave the general
  # `n_scoef < 2` check to catch anything else.
  if (!is.null(distribution) && identical(distribution, "pexp") && n_knots < 1) {
    stop("`n_knots = 0` cannot be used with `distribution = \"pexp\"`: a ",
         "degree-0 basis with no internal knots leaves a single spline ",
         "coefficient, and the random-walk smoothing prior is defined on ",
         "differences between coefficients, so it needs at least two. Use ",
         "`n_knots >= 1`, or `distribution = \"exponential\"` for a constant ",
         "baseline hazard.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve and validate mlumr() backend engine
#'
#' The engine also arrives here through the option set in a profile or the
#' per-fit `engine` argument, and neither passes through `mlumr_engine()`. So
#' the Windows toolchain guard that `mlumr_engine()` applies runs here as
#' well; otherwise the first fit reaches compilation and fails there, without
#' the upgrade advice.
#' @keywords internal
.resolve_mlumr_engine <- function(engine) {
  engine <- .validate_engine_name(engine %||% get_engine())
  if (engine == "cmdstanr" && .cmdstanr_too_old_for_windows()) {
    stop(.cmdstanr_upgrade_advice(), call. = FALSE)
  }
  engine
}

#' Resolve the sampling seed
#'
#' An explicit `seed` argument wins; otherwise the fixed default 2026 is used
#' and a warning says so. Returns `list(value, source)`.
#'
#' The seed is deliberately not derived from R's RNG state. R initializes
#' `.Random.seed` on first use from the clock and the process id, so its
#' presence does not establish that the user called set.seed(): drawing from it
#' would make an unseeded fit silently irreproducible, and would advance the
#' caller's RNG stream as a side effect.
#' @keywords internal
.resolve_mlumr_seed <- function(seed) {
  if (!is.null(seed)) {
    return(list(value = as.integer(seed), source = "user"))
  }
  warning("No `seed` supplied; using the default seed 2026 so the fit is ",
          "reproducible. Pass `seed = ` to control it.", call. = FALSE)
  list(value = 2026L, source = "default")
}

#' Log mlumr() fit metadata
#' @keywords internal
.mlumr_log_fit_start <- function(model_name, family, link, stan_data,
                                 engine, seed_info, verbose) {
  mlumr_message(sprintf("Fitting ML-UMR (%s, %s, link=%s)...",
                        model_name, family, link),
                verbose = verbose)
  mlumr_message(sprintf("  IPD: n = %d", stan_data$n_ipd),
                verbose = verbose)

  if (family == "binomial") {
    mlumr_message(sprintf("  AgD: %d rows, total n = %d",
                          stan_data$n_agd_rows, sum(stan_data$n_agd)),
                  verbose = verbose)
  } else if (family == "normal") {
    mlumr_message(sprintf("  AgD: %d rows", stan_data$n_agd_rows),
                  verbose = verbose)
  } else if (family == "survival") {
    # Survival fell through to the Poisson line, which reads E_agd. That field
    # is unset here, and sum(NULL) is 0, so a 300-row reconstructed curve was
    # logged as "1 rows, total exposure = 0.0": a count that is not missing but
    # wrong, for a quantity a Kaplan-Meier reconstruction does not have.
    mlumr_message(sprintf("  AgD: %d rows, %d reconstructed pseudo-individuals",
                          stan_data$n_agd_rows, stan_data$n_agd),
                  verbose = verbose)
  } else {
    mlumr_message(sprintf("  AgD: %d rows, total exposure = %.1f",
                          stan_data$n_agd_rows, sum(stan_data$E_agd)),
                  verbose = verbose)
  }

  mlumr_message(sprintf("  Covariates: %d, Integration points: %d",
                        stan_data$n_cov, stan_data$n_int),
                verbose = verbose)
  mlumr_message(sprintf("  Engine: %s", engine), verbose = verbose)
  seed_note <- if (identical(seed_info$source, "default")) " (default)" else ""
  mlumr_message(sprintf("  Seed: %d%s", seed_info$value, seed_note),
                verbose = verbose)
}

#' Dispatch mlumr() sampling to the selected backend
#' @keywords internal
.mlumr_fit_backend <- function(engine, model_name, stan_data, chains, iter,
                               warmup, seed, adapt_delta, max_treedepth,
                               refresh, verbose = TRUE, ...) {
  if (engine == "cmdstanr") {
    fit_cmdstanr(model_name, stan_data, chains, iter, warmup,
                 seed, adapt_delta, max_treedepth, refresh,
                 verbose = verbose, ...)
  } else {
    fit_rstan(model_name, stan_data, chains, iter, warmup,
              seed, adapt_delta, max_treedepth, refresh, ...)
  }
}

#' Store user priors plus the resolved Stan-scale beta prior
#' @keywords internal
.mlumr_prior_metadata <- function(data, family, model = "spfa",
                                  prior_intercept, prior_beta,
                                  prior_beta_comparator = NULL,
                                  prior_sigma,
                                  beta_fields,
                                  beta_comparator_fields = NULL,
                                  sd_x,
                                  prior_aux = NULL, prior_aux2 = NULL,
                                  prior_smooth = NULL,
                                  surv_info = NULL) {
  priors <- list(
    intercept = prior_intercept,
    beta = prior_beta,
    beta_resolved = list(
      covariate_names = data$covariates,
      mean = beta_fields$mean,
      sd = beta_fields$sd,
      dist = beta_fields$dist,
      df = beta_fields$df,
      autoscale = beta_fields$autoscale,
      sd_x = sd_x
    )
  )

  # Surface the comparator coefficient prior only for relaxed fits, where it
  # is actually consumed. user-NULL still resolves to prior_beta inside the
  # Stan data list, so the resolved struct reflects what the sampler saw.
  if (identical(model, "relaxed")) {
    priors$beta_comparator <- prior_beta_comparator %||% prior_beta
    if (!is.null(beta_comparator_fields)) {
      priors$beta_comparator_resolved <- list(
        covariate_names = data$covariates,
        mean = beta_comparator_fields$mean,
        sd = beta_comparator_fields$sd,
        dist = beta_comparator_fields$dist,
        df = beta_comparator_fields$df,
        autoscale = beta_comparator_fields$autoscale,
        sd_x = sd_x,
        user_specified = !is.null(prior_beta_comparator)
      )
    }
  }

  if (family == "normal") {
    priors$sigma <- prior_sigma
  }

  if (family == "survival") {
    if (!is.null(surv_info) && surv_info$n_aux > 0) {
      priors$aux <- prior_aux
      # Only gengamma has a second auxiliary. Recording it separately is what
      # lets prior_summary() show a user who set `prior_aux2` that the model
      # used it, rather than printing one prior for two parameters.
      if (surv_info$n_aux > 1L) priors$aux2 <- prior_aux2 %||% prior_aux
    }
    if (!is.null(surv_info) && surv_info$kind == "flexible") {
      priors$smooth <- prior_smooth
    }
  }

  priors
}
