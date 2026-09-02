#' Specify a normal prior
#'
#' Construct a normal prior for passing to [mlumr()] via `prior_intercept`,
#' `prior_beta`, or `prior_sigma`. The resulting list is consumed by the
#' Stan models.
#'
#' @section Choosing a scale:
#'
#' The Stan community's prior-choice wiki (Vehtari et al., 2025) describes
#' five broad categories, from least to most informative:
#'
#' \enumerate{
#'   \item Flat prior — not recommended.
#'   \item Super-vague proper prior, e.g., `normal(0, 1e6)` — not
#'     recommended.
#'   \item Weakly informative, **very weak**, e.g., `normal(0, 10)`.
#'   \item Generic weakly informative, e.g., `normal(0, 1)`.
#'   \item Specific informative, e.g., `normal(0.4, 0.2)`.
#' }
#'
#' Those scales assume parameters are on roughly unit scale. In ML-UMR
#' models the natural scales are:
#'
#' \describe{
#'   \item{Treatment intercepts}{On the linear-predictor (link) scale. For
#'     a binary outcome with logit link, the intercept is a baseline log
#'     odds; `normal(0, 10)` spans ±20 log-odds at 95 percent and is
#'     "very weak". It is the default because the data usually constrain
#'     the intercept strongly. Tightening to `normal(0, 5)` is reasonable
#'     when the expected event rate is far from the extremes.}
#'   \item{Regression coefficients (`beta`)}{On the link scale, per unit
#'     of covariate. For logistic regression with predictors on unit
#'     scale, Gelman et al. (2008) and the Stan wiki recommend
#'     `student_t(df, 0, 2.5)` with `df` in `3:7`, or — as a practical
#'     approximation — `normal(0, 2.5)`. That is the default used by
#'     [mlumr()]. Use `normal(0, 1)` if you expect small effects (e.g.,
#'     standardized predictors in a normal-outcome model). If predictors
#'     are on very different scales, set `autoscale = TRUE` so the scale
#'     is divided by each covariate's SD.}
#'   \item{Residual SD (`sigma`, normal family only)}{`prior_sigma` is
#'     interpreted as a half-normal via the Stan `<lower=0>` constraint.
#'     The default `normal(0, 2.5)` (i.e., `half-normal(0, 2.5)`) is
#'     weakly informative for residual SDs on the scale of the outcome.
#'     Scale to the outcome if it is far from unit scale, or use
#'     [prior_exponential()].}
#' }
#'
#' Prior sensitivity is especially important for the relaxed model,
#' where `beta_comparator` is identified only by the AgD likelihood.
#' Run [prior_sensitivity()] to quantify how much conclusions move under
#' alternative scales; see `vignette("mlumr-models")`.
#'
#' @param mean Prior mean (default 0).
#' @param sd Prior standard deviation (default 10). The default matches
#'   the historical "very weak" scale; explicit tighter values are
#'   recommended for regression coefficients (see Details).
#' @param autoscale If `TRUE` and this prior is passed as `prior_beta`,
#'   the scale is divided by each covariate's empirical SD so the prior
#'   is weakly-informative regardless of predictor scaling. Default
#'   `FALSE` to preserve backward-compatible behavior; set to `TRUE`
#'   explicitly when passing unstandardized predictors. Ignored for
#'   `prior_intercept` and `prior_sigma`.
#'
#' @return A list with components `distribution`, `mean`, `sd`, `df`,
#'   `autoscale`.
#' @export
#'
#' @references
#' Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly
#' informative default prior distribution for logistic and other
#' regression models. *Annals of Applied Statistics*, 2(4), 1360–1383.
#'
#' Vehtari, A. et al. Prior Choice Recommendations (Stan wiki):
#' <https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations>.
#'
#' @examples
#' # Default weakly-very-weak intercept prior
#' prior_normal(mean = 0, sd = 10)
#'
#' # Gelman 2008 default for logistic-regression coefficients
#' prior_normal(mean = 0, sd = 2.5)
#'
#' # Autoscaled coefficient prior (dividing 2.5 by each covariate's SD)
#' prior_normal(mean = 0, sd = 2.5, autoscale = TRUE)
prior_normal <- function(mean = 0, sd = 10, autoscale = FALSE) {
  .validate_prior_number(mean, "mean")
  .validate_prior_number(sd, "sd", positive = TRUE)
  autoscale <- .validate_prior_autoscale(autoscale)
  list(
    distribution = "normal",
    mean = mean,
    sd = sd,
    df = NA_real_,
    autoscale = isTRUE(autoscale)
  )
}

#' Specify a Student-t prior
#'
#' Heavier-tailed alternative to [prior_normal()]. For logistic-regression
#' coefficients with unit-scale predictors, the Stan community prior-choice
#' recommendations suggest Student-t with `df` between 3 and 7 as a robust
#' weakly informative prior (Gelman et al., 2008).
#'
#' @param df Degrees of freedom (must be positive). Values in `3:7` are
#'   recommended for logistic-regression coefficients.
#' @param mean Prior location (default 0).
#' @param sd Prior scale (default 2.5).
#' @param autoscale See [prior_normal()]. Default `FALSE`.
#'
#' @return A list with components `distribution = "student_t"`, `df`,
#'   `mean`, `sd`, `autoscale`.
#' @export
#'
#' @examples
#' # Gelman et al. 2008 recommendation for logistic-regression coefficients
#' prior_student_t(df = 5, mean = 0, sd = 2.5)
prior_student_t <- function(df = 5, mean = 0, sd = 2.5, autoscale = FALSE) {
  .validate_prior_number(df, "df", positive = TRUE)
  .validate_prior_number(mean, "mean")
  .validate_prior_number(sd, "sd", positive = TRUE)
  autoscale <- .validate_prior_autoscale(autoscale)
  list(
    distribution = "student_t",
    mean = mean,
    sd = sd,
    df = df,
    autoscale = isTRUE(autoscale)
  )
}

#' Specify a Cauchy prior
#'
#' Cauchy is Student-t with `df = 1`; this constructor is a convenience
#' wrapper around [prior_student_t()]. It has very heavy tails and should
#' be used with care — modern recommendations generally prefer
#' `prior_student_t(df in 3:7, ...)` over Cauchy for regression
#' coefficients to keep sampling well-behaved (see Piironen & Vehtari on
#' the horseshoe; Ghosh et al. 2015).
#'
#' @param mean Prior location (default 0).
#' @param sd Prior scale (default 2.5).
#' @param autoscale See [prior_normal()]. Default `FALSE`.
#'
#' @return A list with components `distribution = "student_t"`, `df = 1`,
#'   `mean`, `sd`, `autoscale`.
#' @export
#'
#' @examples
#' prior_cauchy(mean = 0, sd = 2.5)
prior_cauchy <- function(mean = 0, sd = 2.5, autoscale = FALSE) {
  prior_student_t(df = 1, mean = mean, sd = sd, autoscale = autoscale)
}

#' Specify an exponential prior
#'
#' Exponential prior on a positive scalar. Currently supported for
#' [`prior_sigma`][mlumr] (normal-family residual SD) only; rejected for
#' unconstrained intercepts and regression coefficients.
#' `prior_exponential(rate = 1)` has mean 1 and is a reasonable
#' weakly-informative choice when the outcome is standardized.
#'
#' @param rate Rate parameter (default 1). Larger `rate` = tighter prior
#'   concentrated near zero.
#' @return A list with components `distribution = "exponential"`, `rate`,
#'   and placeholders (`mean = 0`, `sd = 1 / rate`) so the same
#'   Stan-field translation works.
#' @export
#' @examples
#' prior_exponential(rate = 1)
prior_exponential <- function(rate = 1) {
  .validate_prior_number(rate, "rate", positive = TRUE)
  list(
    distribution = "exponential",
    rate = rate,
    mean = 0,
    sd = 1 / rate,
    df = NA_real_,
    autoscale = FALSE
  )
}


# ---- Default priors (carry $default = TRUE and $version) -------------------

#' Default priors used by [mlumr()]
#'
#' These accessors return the current default priors used by [mlumr()],
#' tagged with `$default = TRUE` and the package version. [prior_summary()]
#' prints the version so cross-release reproducibility is diagnosable: if a
#' later release changes a default, fits produced with an older version
#' will still carry the correct `$version` tag.
#'
#' @return A prior list (see [prior_normal()]).
#' @name default_priors
#' @examples
#' default_prior_intercept()
#' default_prior_beta()
#' default_prior_sigma()
NULL

#' @rdname default_priors
#' @export
default_prior_intercept <- function() {
  .tag_default(prior_normal(mean = 0, sd = 10))
}

#' @rdname default_priors
#' @export
default_prior_beta <- function() {
  .tag_default(prior_normal(mean = 0, sd = 2.5))
}

#' @rdname default_priors
#' @export
default_prior_sigma <- function() {
  .tag_default(prior_normal(mean = 0, sd = 2.5))
}

#' @keywords internal
.tag_default <- function(prior) {
  prior$default <- TRUE
  prior$version <- as.character(utils::packageVersion("mlumr"))
  prior
}


# ---- Validation ------------------------------------------------------------

#' Validate prior specification
#' @param prior Prior specification list
#' @param param_name Parameter name for error messages
#' @keywords internal
validate_prior <- function(prior, param_name = "parameter") {
  if (!is.list(prior)) {
    stop(sprintf("Prior for %s must be a list", param_name), call. = FALSE)
  }
  valid_distribution <- is.character(prior$distribution) &&
    length(prior$distribution) == 1L &&
    prior$distribution %in% c("normal", "student_t", "exponential")
  if (!valid_distribution) {
    msg <- paste0(
      "Unsupported prior distribution '%s' for %s. ",
      "Use prior_normal(), prior_student_t(), prior_cauchy(), ",
      "or prior_exponential()."
    )
    stop(sprintf(
      msg,
      .prior_distribution_label(prior$distribution),
      param_name
    ), call. = FALSE)
  }
  if (prior$distribution == "exponential") {
    .validate_prior_number(prior$rate, "rate", positive = TRUE,
                           param_name = param_name)
    return(invisible(TRUE))
  }
  .validate_prior_number(prior$mean, "mean", param_name = param_name)
  .validate_prior_number(prior$sd, "sd", positive = TRUE,
                         param_name = param_name)
  if (prior$distribution == "student_t") {
    .validate_prior_number(prior$df, "df", positive = TRUE,
                           param_name = param_name)
  }
  invisible(TRUE)
}

.validate_prior_number <- function(x, field, positive = FALSE,
                                   param_name = NULL) {
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    is.finite(x) &&
    (!positive || x > 0)
  if (!valid) {
    qualifier <- if (positive) "positive finite" else "finite"
    if (is.null(param_name)) {
      stop(sprintf("`%s` must be a single %s number", field, qualifier),
           call. = FALSE)
    }
    stop(sprintf("Prior %s must be a single %s number for %s",
                 field, qualifier, param_name),
         call. = FALSE)
  }
  invisible(TRUE)
}

.validate_prior_autoscale <- function(autoscale) {
  if (!is.logical(autoscale) || length(autoscale) != 1L || is.na(autoscale)) {
    stop("`autoscale` must be TRUE or FALSE", call. = FALSE)
  }
  autoscale
}

.prior_distribution_label <- function(distribution) {
  if (is.null(distribution)) {
    return("<missing>")
  }
  paste(distribution, collapse = ", ")
}

#' Is this object a single prior (vs a list of priors)?
#' @keywords internal
is_single_prior <- function(x) {
  is.list(x) && !is.null(x$distribution)
}


# ---- Stan-data translation -------------------------------------------------

#' Translate a scalar prior spec to the Stan data fields
#'
#' Stan scalar-prior fields: `prior_*_mean`, `prior_*_sd`,
#' `prior_*_dist` (0 = normal, 1 = student_t, 2 = exponential for sigma
#' only), `prior_*_df` (used only when dist == 1; positive placeholder
#' otherwise).
#'
#' @param prior A prior list.
#' @return A list with `mean`, `sd`, `dist`, `df`.
#' @keywords internal
stan_prior_fields <- function(prior) {
  validate_prior(prior)
  dist_code <- switch(prior$distribution,
    normal      = 0L,
    student_t   = 1L,
    exponential = 2L,
    stop("Unsupported distribution: ", prior$distribution)
  )
  df_value <- if (prior$distribution == "student_t") prior$df else 3
  # For exponential, Stan interprets `scale` as 1 / rate (see priors_functions.stan).
  location <- if (prior$distribution == "exponential") 0 else prior$mean
  scale    <- if (prior$distribution == "exponential") 1 / prior$rate else prior$sd
  list(mean = location, sd = scale, dist = dist_code, df = df_value)
}


#' Translate a `prior_beta` spec to vector-valued Stan data fields
#'
#' The Stan models declare `prior_beta_mean` and `prior_beta_sd` as
#' `vector[n_cov]` so the same data contract supports:
#'
#' \itemize{
#'   \item (i) a single prior broadcast to all covariates (scalar user input),
#'   \item (ii) per-coefficient priors (a list of prior lists, length `n_cov`),
#'   \item (iii) autoscaling: each covariate's scale is divided by `sd(x_j)`
#'     so the prior is weakly-informative on the standardized scale.
#' }
#'
#' For a list of per-coefficient priors, all elements must use the same
#' `distribution` family and `df` (Stan branches on a single dist code).
#'
#' @param prior A prior list from [prior_normal()] / [prior_student_t()] /
#'   [prior_cauchy()], OR a list of such priors of length `n_cov`.
#' @param n_cov Number of covariates.
#' @param sd_x Optional numeric vector of covariate SDs (length `n_cov`).
#'   Required when any prior has `autoscale = TRUE`; ignored otherwise.
#' @param covariate_names Optional character vector of covariate names
#'   (length `n_cov`). Used only to produce informative warnings when
#'   `autoscale = TRUE` meets a zero-SD covariate.
#' @return A list with numeric vectors `mean` and `sd` (length `n_cov`)
#'   and scalars `dist`, `df`, and a logical vector `autoscale` recording
#'   which elements were autoscaled (for `prior_summary()`).
#' @keywords internal
stan_prior_fields_beta <- function(prior, n_cov, sd_x = NULL,
                                   covariate_names = NULL) {

  # Expand to a list of n_cov single-priors
  if (is_single_prior(prior)) {
    validate_prior(prior, "beta")
    prior_list <- rep(list(prior), n_cov)
  } else if (is.list(prior)) {
    if (length(prior) != n_cov) {
      stop(sprintf(
        "Per-coefficient prior list has length %d but n_cov = %d.",
        length(prior), n_cov
      ), call. = FALSE)
    }
    # Validate each
    for (i in seq_along(prior)) {
      validate_prior(prior[[i]], sprintf("beta[%d]", i))
    }
    prior_list <- prior
  } else {
    stop("`prior_beta` must be a single prior or a list of priors", call. = FALSE)
  }

  # Check all elements share a distribution family and df
  dists <- vapply(prior_list, function(p) p$distribution, character(1))
  if (length(unique(dists)) > 1L) {
    stop("All per-coefficient priors must use the same distribution family ",
         "(mixed normal / student_t is not supported by the Stan dispatch).",
         call. = FALSE)
  }
  if (dists[[1L]] == "exponential") {
    stop("Exponential priors are not supported for regression coefficients.",
         call. = FALSE)
  }
  dfs <- vapply(prior_list, function(p) if (p$distribution == "student_t") p$df else NA_real_,
                numeric(1))
  if (dists[[1L]] == "student_t" && length(unique(dfs)) > 1L) {
    stop("All per-coefficient Student-t priors must share the same df.",
         call. = FALSE)
  }

  means <- vapply(prior_list, function(p) as.numeric(p$mean)[[1L]], numeric(1))
  sds   <- vapply(prior_list, function(p) as.numeric(p$sd)[[1L]],   numeric(1))
  autos <- vapply(prior_list, function(p) isTRUE(p$autoscale),      logical(1))

  if (any(autos)) {
    if (is.null(sd_x)) {
      stop("`autoscale = TRUE` requires covariate SDs; did you call ",
           "stan_prior_fields_beta() without sd_x?", call. = FALSE)
    }
    if (length(sd_x) != n_cov) {
      stop("`sd_x` must have length n_cov", call. = FALSE)
    }
    # Protect against sd_x == 0 (constant covariate): fall back to unscaled
    # and warn, so the user knows the autoscale contract has been broken
    # for those columns. Naming the offending covariates makes the warning
    # actionable.
    zero_var <- autos & sd_x <= 0
    if (any(zero_var)) {
      if (is.null(covariate_names) || length(covariate_names) != n_cov) {
        bad <- sprintf("column %d", which(zero_var))
      } else {
        bad <- covariate_names[zero_var]
      }
      warning(sprintf(
        paste0("`autoscale = TRUE` is requested for covariate(s) with zero ",
               "empirical SD in the IPD: %s. The prior on these coefficients ",
               "falls back to the un-autoscaled scale (sd as supplied). ",
               "Consider removing the constant covariate or turning ",
               "autoscale off for it."),
        paste(bad, collapse = ", ")
      ), call. = FALSE)
    }
    # Autoscaling states a prior on the coefficient of a covariate measured in
    # SD units, so recovering it on the original scale divides both the location
    # and the scale by that covariate's SD. Dividing only the scale would leave
    # a nonzero prior mean attached to the wrong covariate scale. The default
    # mean = 0 is unaffected, since 0 / sd is 0.
    sd_x_safe <- ifelse(sd_x > 0, sd_x, 1)
    means <- ifelse(autos, means / sd_x_safe, means)
    sds <- ifelse(autos, sds / sd_x_safe, sds)
  }

  dist_code <- switch(dists[[1L]], normal = 0L, student_t = 1L)
  df_value <- if (dists[[1L]] == "student_t") dfs[[1L]] else 3

  list(
    mean = means,
    sd = sds,
    dist = dist_code,
    df = df_value,
    autoscale = autos
  )
}


#' @rdname default_priors
#' @export
#' @details
#' `default_prior_aux()` and `default_prior_smooth()` apply to the survival
#' family only. `prior_aux` is a half-normal(0, 2) on the shape/scale
#' parameter(s) of parametric survival distributions (Weibull/Gompertz/gamma
#' shape, log-normal sdlog, generalized-gamma shapes). `prior_smooth` is a
#' half-normal(0, 1) on the random-walk smoothing SD of the M-spline /
#' piecewise-exponential baseline hazard.
default_prior_aux <- function() {
  .tag_default(prior_normal(mean = 0, sd = 2))
}

#' @rdname default_priors
#' @export
default_prior_smooth <- function() {
  .tag_default(prior_normal(mean = 0, sd = 1))
}
