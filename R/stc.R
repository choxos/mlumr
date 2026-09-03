#' Simulated treatment comparison via G-computation
#'
#' Perform an unanchored simulated treatment comparison (STC) using parametric
#' G-computation. Fits a regression on IPD, predicts counterfactual
#' outcomes in both the index and comparator populations, and computes
#' marginal treatment effects with delta-method standard errors.
#'
#' For binomial outcomes, returns the treatment effect on the link scale plus
#' event probabilities, risk difference, and log risk ratio with SEs and CIs
#' for both populations. Event-probability intervals use Wald standard errors
#' and are bounded to `[0, 1]`. For Poisson outcomes, the comparator log rate
#' uses a 0.5 continuity correction when the observed event count is zero.
#'
#' @param data An `mlumr_data` object from [combine_data()]. Integration points
#'   are not required for STC but covariate information from the AgD is used.
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
#'   poisson: `"log"` (default). If `NULL`, uses the canonical default.
#' @param conf_level Confidence level for the interval (default 0.95)
#'
#' @param distribution For `family = "survival"`: the parametric distribution
#'   used for the package-specific survival G-computation (default
#'   `"weibull"`). Requires the `flexsurv`
#'   package. The STC estimand is the restricted-mean-survival-time difference.
#'   The flexible baselines `"mspline"` and `"pexp"` have no parametric
#'   `flexsurv` analogue: requesting either fits a Weibull G-computation as an
#'   approximate benchmark, emits a warning, and records
#'   `distribution_fit = "weibull"` and `approximated = TRUE` in the result
#'   (`$distribution` keeps the requested value). Survival STC currently
#'   supports right-censored data without delayed entry; use [mlumr()] for
#'   left-censored, interval-censored, or delayed-entry survival data.
#' @param n_boot For `family = "survival"` only: number of nonparametric
#'   bootstrap resamples used for the RMST-difference standard error (default
#'   `200`). Set `n_boot = 0` for a fast point estimate with no interval
#'   (`se`/CI returned as `NA`). Must be 0 or at least 2, since a single
#'   resample has no standard error; several hundred are needed before the
#'   interval is usable, so treat anything below the default as exploratory.
#'   Ignored for other families, which use the delta method.
#' @param seed For `family = "survival"` only: optional integer seed for the
#'   bootstrap, making the standard error reproducible. The global random
#'   number stream is restored on exit. Ignored for other families.
#' @param rmst_horizon For `family = "survival"` only: the restriction time the
#'   RMST difference is integrated to. Defaults to the largest observed time
#'   across both arms. RMST at a different horizon is a different estimand, so
#'   set this explicitly whenever the result is to be compared with an
#'   [mlumr()] fit, whose own default can be the follow-up both studies
#'   observed rather than the pooled maximum; read that fit's horizon from the
#'   `horizon` column of `predict(type = "rmst")`. A value beyond the observed
#'   range extrapolates the fitted parametric survival function and warns.
#'   Ignored for other families.
#'
#' @return An object of class `mlumr_stc`
#' @importFrom stats gaussian poisson dnorm
#' @export
#'
#' @details
#' The STC procedure is:
#' 1. Fit a GLM on IPD (binomial/gaussian/poisson as appropriate).
#' 2. Predict on comparator-population covariates (from integration points or
#'    AgD covariate means).
#' 3. Marginalize predictions over the comparator population.
#' 4. Predict on index-population covariates (IPD).
#' 5. Compute treatment effects and SEs via the delta method.
#'
#' STC is a parametric G-computation benchmark. It relies on the IPD outcome
#' model being correctly specified and transportable to the comparator
#' population. It does not model posterior uncertainty in population
#' covariate distributions or relax treatment-specific covariate effects.
#' When clinically meaningful effect modification is plausible, prefer
#' `mlumr(..., model = "relaxed")` as the primary analysis and use STC as a
#' sensitivity or benchmarking analysis.
#'
#' The estimand is the comparator-population contrast, and only that. Earlier
#' versions also reported an index-population effect, obtained by assuming the
#' treatment difference is constant on the link scale. That constancy is an
#' extra assumption which is not part of the STC estimand, is not testable from
#' the available data, and does not hold under effect modification, which is the
#' situation population adjustment exists to handle. Use [mlumr()], which
#' standardizes both treatment models and reports both populations without it,
#' when the index population is the decision target.
#'
#' @examples
#' \dontrun{
#' result <- stc(dat)
#' print(result)
#' }
stc <- function(data, link = NULL, conf_level = 0.95, distribution = "weibull",
                n_boot = 200L, seed = NULL, rmst_horizon = NULL) {

  .validate_mlumr_data_object(data)

  family <- data$family %||% "binomial"
  z <- .z_from_conf_level(conf_level)

  if (family == "survival") {
    if (!isTRUE(data$has_integration)) {
      stop("Survival STC requires comparator-population integration points. ",
           "Call add_integration() with a joint covariate distribution; ",
           "substituting aggregate means is not marginal standardization.",
           call. = FALSE)
    }
    .validate_mlumr_integer(n_boot, "n_boot", lower = 0L)
    # A single resample gives sd() = NA, which is indistinguishable downstream
    # from "every resample failed" and was reported as such. Either the
    # bootstrap is off (0) or it has enough replicates to have a variance.
    if (n_boot == 1L) {
      stop("`n_boot` must be 0 (no bootstrap) or at least 2: the standard ",
           "error of a single resample is undefined. Several hundred ",
           "resamples are needed for a usable interval; the default is 200.",
           call. = FALSE)
    }
    if (!is.null(seed)) {
      .validate_mlumr_integer(seed, "seed", lower = 0L)
    }
    out <- .stc_survival(data, conf_level, z, distribution,
                         n_boot = as.integer(n_boot), seed = seed,
                         rmst_horizon = rmst_horizon)
    class(out) <- c("mlumr_stc", "list")
    return(out)
  }

  ipd <- data$ipd$data
  agd <- data$agd$data
  cov_names <- data$covariates

  link_info <- check_link(family, link)
  link_resolved <- link_info$link

  if (family == "binomial") {
    glm_family <- binomial(link = link_resolved)
  } else if (family == "normal") {
    glm_family <- gaussian(link = link_resolved)
  } else {
    glm_family <- poisson(link = link_resolved)
  }

  fit <- glm(.stc_formula(cov_names, family), family = glm_family, data = ipd)
  glm_params <- .stc_glm_parameters(fit)

  comparator <- .stc_comparator_data(data, cov_names, family)
  newdata <- comparator$newdata
  n_int <- comparator$n_int

  beta_hat <- glm_params$beta_hat
  V <- glm_params$V

  out <- switch(family,
    binomial = .stc_binomial(data, fit, ipd, agd, newdata, link_resolved,
                             conf_level, z, beta_hat, V, n_int),
    normal = .stc_normal(data, fit, ipd, agd, newdata, link_resolved,
                         conf_level, z, beta_hat, V, n_int),
    poisson = .stc_poisson(data, fit, ipd, agd, newdata, link_resolved,
                           conf_level, z, beta_hat, V, n_int)
  )

  class(out) <- c("mlumr_stc", "list")
  out
}

#' Build the STC GLM formula without assuming syntactic covariate names
#' @keywords internal
.stc_formula <- function(cov_names, family) {
  terms <- lapply(cov_names, as.name)
  if (family == "poisson") {
    terms <- c(terms, list(call("offset", call("log", as.name(".exposure")))))
  }
  rhs <- Reduce(function(left, right) call("+", left, right), terms)
  stats::as.formula(call("~", as.name(".outcome"), rhs),
                    env = parent.frame())
}

#' Validate fitted STC GLM parameters before delta-method calculations
#' @keywords internal
.stc_glm_parameters <- function(fit) {
  if (!isTRUE(fit$converged)) {
    stop("STC GLM did not converge; check the IPD model or use mlumr().",
         call. = FALSE)
  }
  beta_hat <- coef(fit)
  V <- vcov(fit)
  if (anyNA(beta_hat) || anyNA(V)) {
    stop(
      paste(
        "STC GLM produced aliased coefficients or covariance terms.",
        "Check for collinear covariates or insufficient variation."
      ),
      call. = FALSE
    )
  }
  if (any(!is.finite(beta_hat)) || any(!is.finite(V))) {
    stop(
      paste(
        "STC GLM produced non-finite coefficients or covariance terms.",
        "This can occur with separation, collinearity, or insufficient",
        "outcome variation; consider mlumr() for a Bayesian fit."
      ),
      call. = FALSE
    )
  }
  list(beta_hat = beta_hat, V = V)
}

#' Build a model matrix aligned with fitted GLM coefficients
#' @keywords internal
.stc_model_matrix <- function(fit, newdata) {
  X <- stats::model.matrix(stats::delete.response(stats::terms(fit)), newdata)
  beta_names <- names(coef(fit))
  missing_cols <- setdiff(beta_names, colnames(X))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Cannot build STC model matrix columns: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  X[, beta_names, drop = FALSE]
}

#' Binomial STC estimator
#' @keywords internal
.stc_binomial <- function(data, fit, ipd, agd, newdata, link_resolved,
                          conf_level, z, beta_hat, V, n_int) {
  pred_probs <- predict(fit, newdata = newdata, type = "response")
  weights <- if (data$has_integration) rep(agd$.n, each = n_int) else agd$.n

  p_hat_A_comp <- weighted.mean(pred_probs, weights)
  n_B <- sum(agd$.n)
  p_B <- sum(agd$.r) / n_B

  p_hat_A_comp <- bound_probability(p_hat_A_comp, n_B)
  p_B <- bound_probability(p_B, n_B)

  comp_delta <- .stc_binomial_comparator_delta(
    fit, newdata, weights, beta_hat, V, link_resolved,
    p_hat_A_comp, p_B, n_B
  )

  estimate <- comp_delta$link_effect
  se <- comp_delta$link_effect_se
  var_p_B <- p_B * (1 - p_B) / n_B
  p_hat_A_comp_se <- .sqrt_variance(comp_delta$var_p_A,
                                    "comparator probability variance")
  p_B_se <- sqrt(var_p_B)
  p_hat_A_comp_ci <- .bounded_wald_interval(p_hat_A_comp, p_hat_A_comp_se, z,
                                            lower = 0, upper = 1)
  p_B_ci <- .bounded_wald_interval(p_B, p_B_se, z, lower = 0, upper = 1)

  rd <- p_hat_A_comp - p_B
  se_rd <- .sqrt_variance(comp_delta$var_p_A + var_p_B,
                          "risk-difference variance")

  log_rr <- log(p_hat_A_comp) - log(p_B)
  se_log_rr <- .sqrt_variance(
    comp_delta$var_p_A / (p_hat_A_comp^2) +
      (1 - p_B) / (n_B * p_B),
    "log-risk-ratio variance"
  )

  list(
    estimate = estimate,
    link_effect = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "binomial",
    link = link_resolved,
    population = "comparator",
    p_hat_index = p_hat_A_comp,
    p_hat_index_se = p_hat_A_comp_se,
    p_hat_index_lower = p_hat_A_comp_ci$lower,
    p_hat_index_upper = p_hat_A_comp_ci$upper,
    p_comparator = p_B,
    p_comparator_se = p_B_se,
    p_comparator_lower = p_B_ci$lower,
    p_comparator_upper = p_B_ci$upper,
    rd = rd,
    rd_se = se_rd,
    rd_lower = rd - z * se_rd,
    rd_upper = rd + z * se_rd,
    log_rr = log_rr,
    log_rr_se = se_log_rr,
    log_rr_lower = log_rr - z * se_log_rr,
    log_rr_upper = log_rr + z * se_log_rr,
    glm_fit = fit,
    data = data
  )
}

#' Comparator-population delta-method terms for binomial STC
#' @keywords internal
.stc_binomial_comparator_delta <- function(fit, newdata, weights,
                                           beta_hat, V, link_resolved,
                                           p_A, p_B, n_B) {
  X_comp_design <- .stc_model_matrix(fit, newdata)
  eta_comp <- as.vector(X_comp_design %*% beta_hat)
  p_comp <- inverse_link(eta_comp, link_resolved)
  w_norm <- weights / sum(weights)

  dp_dbeta <- inverse_link_derivative(eta_comp, p_comp, link_resolved)
  grad_mean <- colSums(w_norm * dp_dbeta * X_comp_design)
  grad_link <- grad_mean * link_derivative_response(
    sum(w_norm * p_comp), link_resolved
  )

  var_link_A <- as.numeric(t(grad_link) %*% V %*% grad_link)
  var_link_B <- binomial_link_variance(p_B, n_B, link_resolved)
  var_link_effect <- .nonnegative_variance(var_link_A + var_link_B,
                                           "comparator link-effect variance")
  var_p_A <- .nonnegative_variance(as.numeric(t(grad_mean) %*% V %*% grad_mean),
                                   "comparator probability variance")

  list(
    link_effect = link_fun(p_A, link_resolved) - link_fun(p_B, link_resolved),
    link_effect_se = sqrt(var_link_effect),
    var_link_effect = var_link_effect,
    var_p_A = var_p_A
  )
}

#' Normal-outcome STC estimator
#' @keywords internal
.stc_normal <- function(data, fit, ipd, agd, newdata, link_resolved,
                        conf_level, z, beta_hat, V, n_int) {
  if (nrow(agd) > 1L && is.null(agd$.n)) {
    stop("`outcome_n` is required for multiple normal AgD rows.", call. = FALSE)
  }
  # Population weights, not precision weights. The estimand is the comparator
  # population's mean, which is the size-weighted mixture of its strata; an
  # inverse-variance average estimates a common mean instead and is a different
  # quantity when the strata differ.
  agd_weights <- agd$.n %||% 1
  pred_y <- predict(fit, newdata = newdata, type = "response")
  weights <- if (data$has_integration) {
    rep(agd_weights, each = n_int)
  } else {
    agd_weights
  }
  y_hat_A <- weighted.mean(pred_y, weights)
  y_B <- sum(.normalize_weights(agd_weights) * agd$.y)
  estimate <- y_hat_A - y_B

  X_comp_design <- .stc_model_matrix(fit, newdata)
  w_norm <- weights / sum(weights)
  grad <- if (link_resolved == "identity") {
    colSums(w_norm * X_comp_design)
  } else {
    eta_comp <- as.vector(X_comp_design %*% beta_hat)
    colSums(w_norm * exp(eta_comp) * X_comp_design)
  }

  var_A <- .nonnegative_variance(as.numeric(t(grad) %*% V %*% grad),
                                 "normal STC index variance")
  var_B <- sum(.normalize_weights(agd_weights)^2 * agd$.se^2)
  se <- .sqrt_variance(var_A + var_B, "normal STC contrast variance")

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "normal",
    link = link_resolved,
    population = "comparator",
    y_hat_index = y_hat_A,
    y_hat_index_se = sqrt(var_A),
    y_comparator = y_B,
    y_comparator_se = sqrt(var_B),
    glm_fit = fit,
    data = data
  )
}

#' Poisson-outcome STC estimator
#' @keywords internal
.stc_poisson <- function(data, fit, ipd, agd, newdata, link_resolved,
                         conf_level, z, beta_hat, V, n_int) {
  pred_rate <- predict(fit, newdata = newdata, type = "response")
  weights <- if (data$has_integration) rep(agd$.E, each = n_int) else agd$.E
  rate_hat_A <- weighted.mean(pred_rate, weights)

  events_B <- sum(agd$.r)
  exposure_B <- sum(agd$.E)
  rate_B <- events_B / exposure_B
  events_B_adjusted <- max(events_B, 0.5)
  rate_B_for_log <- events_B_adjusted / exposure_B
  estimate <- log(rate_hat_A) - log(rate_B_for_log)

  X_comp_design <- .stc_model_matrix(fit, newdata)
  eta_comp <- as.vector(X_comp_design %*% beta_hat)
  lambda_comp <- exp(eta_comp)
  w_norm <- weights / sum(weights)

  grad_log_rate <- colSums(w_norm * lambda_comp * X_comp_design) /
    sum(w_norm * lambda_comp)
  var_lrr_A <- .nonnegative_variance(
    as.numeric(t(grad_log_rate) %*% V %*% grad_log_rate),
    "poisson STC log-rate variance"
  )
  var_lrr_B <- 1 / events_B_adjusted
  se <- .sqrt_variance(var_lrr_A + var_lrr_B,
                       "poisson STC contrast variance")

  grad_rate <- colSums(w_norm * lambda_comp * X_comp_design)
  var_rate_A <- .nonnegative_variance(
    as.numeric(t(grad_rate) %*% V %*% grad_rate),
    "poisson STC rate variance"
  )
  # With no events at all in the IPD the fitted rate is numerically 0, so the
  # gradient lambda * X vanishes and the delta method reports the standardized
  # rate as known to within ~1e-7. It is not: zero events over the observed
  # exposure is consistent with a clearly positive rate, which is why the
  # log-rate contrast on the same fit carries an SE in the tens of thousands.
  # Floor the variance at the continuity-corrected plug-in the naive estimator
  # would report from the same IPD, so the index arm still contributes to the
  # rate difference. Only reached at the boundary; a fit with any event keeps
  # its delta-method variance untouched.
  events_index_total <- sum(ipd$.outcome)
  if (events_index_total == 0) {
    exposure_index_total <- sum(ipd$.exposure)
    var_rate_A <- max(var_rate_A, 0.5 / exposure_index_total^2)
  }
  # Rate difference on the natural per-unit-exposure scale: the standardized
  # index rate minus the observed comparator rate. The standardized rate's
  # variance is the delta-method one already computed for it, and the two arms
  # are independent, so the variances add.
  rd <- rate_hat_A - rate_B
  # The comparator variance uses the continuity-corrected count, so a
  # zero-event comparator arm still contributes uncertainty rather than
  # collapsing the interval; the log-rate contrast already corrects the same
  # way.
  var_rd <- var_rate_A + events_B_adjusted / exposure_B^2
  se_rd <- .sqrt_variance(var_rd, "poisson STC rate-difference variance")

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "poisson",
    link = link_resolved,
    population = "comparator",
    rd = rd,
    rd_se = se_rd,
    rd_lower = rd - z * se_rd,
    rd_upper = rd + z * se_rd,
    rate_hat_index = rate_hat_A,
    rate_hat_index_se = sqrt(var_rate_A),
    rate_comparator = rate_B,
    rate_comparator_se = sqrt(events_B_adjusted) / exposure_B,
    events_comparator = events_B,
    exposure_comparator = exposure_B,
    events_comparator_adjusted = events_B_adjusted,
    glm_fit = fit,
    data = data
  )
}

#' Build comparator-population covariates for STC prediction
#' @keywords internal
.stc_comparator_data <- function(data, cov_names, family) {
  agd <- data$agd$data
  n_int <- NULL

  if (data$has_integration) {
    X_int <- data$integration_points
    n_agd_rows <- dim(X_int)[1]
    n_int <- dim(X_int)[2]

    X_comp <- matrix(NA_real_,
                     nrow = n_agd_rows * n_int,
                     ncol = length(cov_names))
    colnames(X_comp) <- cov_names
    for (k in seq_len(n_agd_rows)) {
      rows <- ((k - 1) * n_int + 1):(k * n_int)
      X_comp[rows, ] <- X_int[k, , ]
    }
    newdata <- as.data.frame(X_comp)
  } else {
    newdata <- .stc_agd_mean_newdata(agd, cov_names)
  }

  if (family == "poisson") {
    newdata$.exposure <- 1
  }

  list(newdata = newdata, n_int = n_int)
}

#' Build comparator-population covariates from AgD means
#' @keywords internal
.stc_agd_mean_newdata <- function(agd, cov_names) {
  newdata <- data.frame(row.names = seq_len(nrow(agd)))
  for (cov in cov_names) {
    mean_col <- paste0(cov, "_mean")
    if (!mean_col %in% names(agd)) {
      stop(sprintf(
        paste0(
          "Cannot find mean for covariate '%s' in AgD. ",
          "Either add integration points or ensure AgD has '%s' column."
        ),
        cov, mean_col
      ), call. = FALSE)
    }
    newdata[[cov]] <- agd[[mean_col]]
  }
  newdata
}


#' Stable Euclidean norm of two standard errors
#' @keywords internal
.stc_hypot <- function(x, y) {
  if (any(is.infinite(c(x, y)))) return(Inf)
  scale <- max(abs(c(x, y)))
  if (scale == 0) return(0)
  scale * sqrt((x / scale)^2 + (y / scale)^2)
}

#' Package-specific parametric survival G-computation (RMST difference)
#'
#' Fits a parametric survival model to the index IPD (adjusting for
#' covariates), G-computes the marginal restricted mean survival time (RMST) in
#' the comparator population, and contrasts it with the comparator RMST from the
#' reconstructed pseudo-IPD. Standard errors come from a nonparametric
#' bootstrap. This survival extension is a package benchmark, not the
#' established binary/continuous/count STC procedure. Requires the `flexsurv`
#' package.
#' @keywords internal
.stc_survival <- function(data, conf_level, z, distribution, n_boot = 200L,
                          seed = NULL, rmst_horizon = NULL) {
  if (!requireNamespace("flexsurv", quietly = TRUE)) {
    stop("Package 'flexsurv' is required for survival STC. ",
         "Install it or use mlumr() / naive().", call. = FALSE)
  }
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  cov_names <- data$covariates
  # Flexible baselines ("mspline"/"pexp") have no parametric flexsurv analogue.
  # Rather than silently report the requested distribution while actually
  # fitting a Weibull, flag the approximation: warn, and record both the
  # requested `distribution` and the `distribution_fit` actually used.
  # switch() in .stc_flexsurv_dist() used to end in an unnamed default, so any
  # unrecognized name (a typo such as "weibul") fell through to a Weibull fit
  # while the returned object still reported the name the user typed and
  # approximated = FALSE. Wrong model, wrong label, no warning. Validate first.
  valid_distributions <- c("exponential", "weibull", "gompertz",
                           "exponential-aft", "weibull-aft", "lognormal",
                           "loglogistic", "gamma", "gengamma",
                           "mspline", "pexp")
  if (!is.character(distribution) || length(distribution) != 1L ||
        is.na(distribution) || !distribution %in% valid_distributions) {
    stop("`distribution` must be one of: ",
         paste(valid_distributions, collapse = ", "), ".", call. = FALSE)
  }
  approximated <- distribution %in% c("mspline", "pexp")
  dist_fit <- if (approximated) "weibull" else distribution
  if (approximated) {
    warning(
      sprintf(
        paste0(
          "Survival STC has no parametric analogue for a '%s' baseline; ",
          "fitting a Weibull G-computation as an approximate RMST benchmark ",
          "(the result reports distribution_fit = \"weibull\"). Request ",
          "distribution = \"weibull\" to silence this, or use mlumr() for ",
          "the flexible-baseline Bayesian fit."
        ),
        distribution
      ),
      call. = FALSE
    )
  }
  # Map the actually-fitted family (dist_fit) to its flexsurv name. dist_fit is
  # the single normalized representation (Weibull for flexible-baseline requests),
  # so the flexsurv lookup never depends on the mspline/pexp fallback entries.
  dist_fs <- .stc_flexsurv_dist(dist_fit)
  # RMST is an integral to a restriction time, so an STC estimate is only
  # comparable with a Bayesian one when both use the same horizon. mlumr() can
  # narrow its default to the follow-up both studies observed, which differs
  # from the pooled maximum used here, so the horizon has to be settable.
  horizon <- if (is.null(rmst_horizon)) {
    max(c(ipd$.time, pseudo$.time))
  } else {
    if (!is.numeric(rmst_horizon) || length(rmst_horizon) != 1L ||
          !is.finite(rmst_horizon) || rmst_horizon <= 0) {
      stop("`rmst_horizon` must be a single positive finite time.",
           call. = FALSE)
    }
    obs_max <- max(c(ipd$.time, pseudo$.time))
    if (rmst_horizon > obs_max) {
      warning(sprintf(paste0("`rmst_horizon` = %.4g is beyond the largest ",
                             "observed time (%.4g); the fitted parametric ",
                             "survival function is extrapolated past the data ",
                             "there."), rmst_horizon, obs_max), call. = FALSE)
    }
    rmst_horizon
  }
  comp_cov <- .stc_comparator_data(data, cov_names, "survival")$newdata

  .validate_stc_survival_right_censored(ipd, pseudo)
  .validate_stc_survival_events(ipd, pseudo)

  point <- .stc_survival_point(ipd, pseudo, cov_names, comp_cov, dist_fs, horizon)

  # See .stc_survival_point(): a negative fitted shape / Q is outside the
  # parameter space of the Bayesian model carrying the same name.
  out_of_family <- NULL
  if (!is.null(point$family_par_name) && any(point$family_par < 0)) {
    out_of_family <- point$family_par_name
    approximated <- TRUE
    dist_fit <- sprintf("flexsurv %s, unrestricted %s", distribution,
                        point$family_par_name)
    warning(sprintf(
      paste0("The STC '%s' fit has %s = %s, outside the parameter space of ",
             "mlumr()'s '%s' model, which constrains %s > 0. flexsurv admits ",
             "the negative branch, so this benchmark and the Bayesian fit of ",
             "the same name are different distributional families here: a ",
             "difference between them need not be a Bayesian-versus-",
             "frequentist difference. Compare the collapsible RMST estimands, ",
             "or choose a distribution whose parameter spaces agree."),
      distribution, point$family_par_name,
      paste(sprintf("%.4g", point$family_par), collapse = " / "),
      distribution, point$family_par_name
    ), call. = FALSE)
  }

  if (n_boot > 0L) {
    # Seed the bootstrap reproducibly without perturbing the user's global RNG
    # stream: snapshot .Random.seed and restore it when this function returns.
    if (!is.null(seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        saved_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
        on.exit(assign(".Random.seed", saved_seed, envir = globalenv()), # nolint: object_name_linter.
                add = TRUE)
      } else {
        on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())),
                add = TRUE)
      }
      set.seed(seed)
    }
    # Each replicate carries its own family parameter as well as the two
    # estimates: the point-fit check above cannot see a resample that leaves
    # mlumr()'s parameter space while the point estimate stays inside it, and
    # those refits still enter the SE.
    boot <- vapply(seq_len(n_boot), function(b) {
      ib <- ipd[sample(nrow(ipd), replace = TRUE), , drop = FALSE]
      pb <- pseudo[sample(nrow(pseudo), replace = TRUE), , drop = FALSE]
      # A resample can lose every event in an arm. flexsurvreg() then returns
      # optimizer-boundary parameters with a warning rather than an error, so
      # the replicate would be counted as a success and its number would enter
      # the standard error. Treat it as the failed fit it is.
      if (sum(ib$.status == 1L) == 0L || sum(pb$.status == 1L) == 0L) {
        return(rep(NA_real_, 4L))
      }
      tryCatch({
        pt <- .stc_survival_point(ib, pb, cov_names, comp_cov, dist_fs, horizon)
        par_b <- c(NA_real_, NA_real_)
        if (!is.null(pt$family_par)) par_b <- unname(pt$family_par)
        c(pt$rmst_diff, pt$log_chr, par_b)
      }, error = function(e) rep(NA_real_, 4L))
    }, numeric(4))
    se <- stats::sd(boot[1, ], na.rm = TRUE)
    log_chr_se <- stats::sd(boot[2, ], na.rm = TRUE)
    # Count the two quantities separately: a resample can return a finite RMST
    # difference while the cumulative-hazard ratio is undefined at the horizon
    # (a boundary survival), and one shared count would hide that.
    n_boot_ok <- sum(!is.na(boot[1, ]))
    n_boot_ok_chr <- sum(!is.na(boot[2, ]))
    n_boot_failed <- n_boot - n_boot_ok
    n_boot_failed_chr <- n_boot - n_boot_ok_chr
    # Warn on EITHER shortfall. Gating on the RMST count alone left a run in
    # which every RMST difference was finite but several cumulative-hazard
    # ratios were not silently reporting a log-CHR interval built from fewer
    # replicates than the RMST one.
    if (n_boot_failed > 0L || n_boot_failed_chr > 0L) {
      warning(sprintf(
        paste0("Bootstrap successes: RMST difference %d/%d; log cumulative-",
               "hazard ratio %d/%d. Each standard error is based only on its ",
               "own successful resamples. A high failure rate gives an ",
               "over-narrow SE; consider a different `distribution` or a ",
               "larger `n_boot`."),
        n_boot_ok, n_boot, n_boot_ok_chr, n_boot
      ), call. = FALSE)
    }
    # How many resamples left the Bayesian model's parameter space. NA_integer_
    # when the distribution has no such parameter, which is not the same as
    # zero and must not print as though it had been checked.
    n_boot_out_of_family <- if (is.null(point$family_par_name)) {
      NA_integer_
    } else {
      as.integer(sum(apply(boot[3:4, , drop = FALSE], 2L,
                           function(v) any(!is.na(v) & v < 0))))
    }
    if (!is.na(n_boot_out_of_family) && n_boot_out_of_family > 0L) {
      warning(sprintf(
        paste0("%d of %d bootstrap resample(s) fitted %s < 0, outside the ",
               "parameter space of mlumr()'s '%s' model, and those refits are ",
               "included in the standard error. The interval is therefore a ",
               "broader-family flexsurv benchmark rather than a like-for-like ",
               "comparison with the Bayesian fit of the same name."),
        n_boot_out_of_family, n_boot, point$family_par_name, distribution
      ), call. = FALSE)
    }
    # Fewer than two successes leaves sd() undefined; make that explicit rather
    # than letting an NA propagate as though the bootstrap had simply failed.
    if (n_boot_ok < 2L) se <- NA_real_
    if (n_boot_ok_chr < 2L) log_chr_se <- NA_real_
  } else {
    se <- NA_real_
    log_chr_se <- NA_real_
    n_boot_ok <- 0L
    n_boot_ok_chr <- 0L
    n_boot_out_of_family <- NA_integer_
  }

  list(
    estimate = point$rmst_diff,
    rmst_diff = point$rmst_diff,
    se = se,
    ci_lower = if (is.na(se)) NA_real_ else point$rmst_diff - z * se,
    ci_upper = if (is.na(se)) NA_real_ else point$rmst_diff + z * se,
    conf_level = conf_level,
    family = "survival",
    population = "comparator",
    method = "package-specific parametric survival G-computation",
    distribution = distribution,
    distribution_fit = dist_fit,
    approximated = approximated,
    # Non-NULL when the fitted shape/Q left the Bayesian model's parameter
    # space; names the parameter that did so.
    out_of_family = out_of_family,
    family_par = point$family_par,
    # Name of the parameter whose sign decides family membership ("shape" for
    # Gompertz, "Q" for the generalized gamma), NULL when the distribution has
    # none. Reported separately from `out_of_family`, which is set only when the
    # POINT fit left the space.
    family_par_name = point$family_par_name,
    horizon = horizon,
    rmst_index_comparator = point$rmst_index,
    rmst_index = point$rmst_index,
    rmst_comparator = point$rmst_comparator,
    # Cumulative-hazard ratio at the horizon (ratio of cumulative hazards
    # H(horizon) = -log S(horizon)), with a bootstrap SE/CI on the log scale.
    # This is not a hazard ratio in general; see .stc_survival_point().
    log_chr = point$log_chr,
    chr = exp(point$log_chr),
    log_chr_se = log_chr_se,
    log_chr_lower = if (is.na(log_chr_se)) NA_real_ else point$log_chr - z * log_chr_se,
    log_chr_upper = if (is.na(log_chr_se)) NA_real_ else point$log_chr + z * log_chr_se,
    n_index = nrow(ipd),
    n_comparator = nrow(pseudo),
    n_boot = n_boot_ok,
    n_boot_requested = as.integer(n_boot),
    n_boot_ok = n_boot_ok,
    n_boot_ok_log_chr = n_boot_ok_chr,
    # Resamples whose fitted shape / Q left mlumr()'s parameter space but whose
    # RMST still entered the SE. NA_integer_ when the distribution has no such
    # parameter to leave.
    n_boot_out_of_family = n_boot_out_of_family,
    data = data
  )
}

#' Require events in both arms before a parametric survival STC
#'
#' With no events in an arm the likelihood for its event-time distribution has
#' no finite interior maximum. `flexsurvreg()` returns optimizer-boundary
#' parameters with a warning rather than failing, after which an RMST
#' difference and its interval look ordinary.
#' @keywords internal
.validate_stc_survival_events <- function(ipd, pseudo) {
  n_idx <- sum(ipd$.status == 1L)
  n_cmp <- sum(pseudo$.status == 1L)
  if (n_idx == 0L || n_cmp == 0L) {
    stop("Survival STC needs at least one event in each arm: observed ",
         n_idx, " in the index arm and ", n_cmp, " in the comparator arm. ",
         "With an event-free arm the parametric survival fit has no finite ",
         "interior estimate, so the RMST difference it produces is an ",
         "artifact of where the optimizer stopped.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate survival STC input supported by flexsurv formula construction
#' @keywords internal
.validate_stc_survival_right_censored <- function(ipd, pseudo) {
  status <- c(ipd$.status, pseudo$.status)
  start_time <- c(ipd$.start_time, pseudo$.start_time)
  delay_time <- c(ipd$.delay_time, pseudo$.delay_time)

  if (any(!status %in% c(0L, 1L)) ||
        any(start_time > 0, na.rm = TRUE) ||
        any(delay_time > 0, na.rm = TRUE)) {
    stop(
      "Survival STC currently supports only right-censored data without delayed entry. ",
      "Use mlumr() for left-censored, interval-censored, or delayed-entry survival data.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Require a flexsurv fit that actually converged
#'
#' A sparse or nearly separated sample can leave events in both arms and still
#' send the optimizer to a boundary. `flexsurvreg()` warns in that case rather
#' than failing, so the estimates were summarized as an ordinary RMST, and the
#' bootstrap counted such refits among its successes because `tryCatch()` sees
#' only errors. Raising an error here makes a non-converged replicate a failed
#' one, which is what it is.
#' @keywords internal
.validate_flexsurv_fit <- function(fit, arm) {
  conv <- fit$opt$convergence
  if (!is.null(conv) && !identical(as.integer(conv), 0L)) {
    stop("The survival STC fit for the ", arm, " arm did not converge ",
         "(optimizer code ", as.integer(conv), "), so its restricted mean is ",
         "an artifact of where the optimizer stopped.", call. = FALSE)
  }
  est <- tryCatch(fit$res[, "est"], error = function(e) NULL)
  if (is.null(est) || anyNA(est) || any(!is.finite(est))) {
    stop("The survival STC fit for the ", arm, " arm returned non-finite ",
         "parameter estimates.", call. = FALSE)
  }
  if (!is.null(fit$cov) && (anyNA(fit$cov) || any(!is.finite(fit$cov)))) {
    stop("The survival STC fit for the ", arm, " arm returned a non-finite ",
         "covariance matrix, so its uncertainty cannot be quantified.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' One STC survival point estimate (RMST_index, RMST_comparator, difference)
#' @keywords internal
.stc_survival_point <- function(ipd, pseudo, cov_names, comp_cov, dist_fs, horizon) {
  ipd$.stc_event <- as.integer(ipd$.status == 1L)
  pseudo$.stc_event <- as.integer(pseudo$.status == 1L)

  # Build the formula from symbols rather than pasting names into a string, as
  # `.stc_formula()` already does for the other families. Backtick-quoting a
  # name that itself contains a backtick produces a formula that does not parse,
  # so a column this function has already accepted as a valid numeric covariate
  # would fail here instead.
  rhs <- Reduce(function(left, right) call("+", left, right),
                lapply(cov_names, as.name))
  form_a <- stats::as.formula(
    call("~", quote(survival::Surv(.time, .stc_event)), rhs),
    env = parent.frame()
  )
  fit_a <- flexsurv::flexsurvreg(form_a, data = ipd, dist = dist_fs)
  .validate_flexsurv_fit(fit_a, "index")
  rmst_a_rows <- summary(fit_a, newdata = comp_cov, type = "rmst",
                         t = horizon, ci = FALSE, tidy = TRUE)
  # An equal-weight mean IS the comparator-population average here: survival
  # AgD carries exactly one arm-summary row (set_agd_surv() rejects multi-arm
  # comparators), so `comp_cov` is that single row's integration grid and the
  # points are equally weighted by construction. The other families average
  # over several AgD rows and must weight by `agd$.n` / `agd$.E`, which
  # survival AgD does not carry. Weighting has to arrive with multi-row
  # support, not before it.
  rmst_index <- mean(rmst_a_rows$est)

  fit_b <- flexsurv::flexsurvreg(survival::Surv(.time, .stc_event) ~ 1,
                                 data = pseudo, dist = dist_fs)
  .validate_flexsurv_fit(fit_b, "comparator")
  rmst_b <- summary(fit_b, type = "rmst", t = horizon, ci = FALSE,
                    tidy = TRUE)$est[1]

  # Cumulative-hazard ratio at the horizon: the ratio of cumulative hazards
  # H(t) = -log S(t) at t = horizon, for the G-computed index survival
  # (standardized to the comparator covariates) versus the comparator. This is
  # NOT in general a hazard ratio: only when the two separately-fitted survival
  # models happen to be proportional with a common baseline shape does it equal
  # the constant HR. NA if either survival is at a boundary (no events / certain
  # survival), where the log ratio is undefined.
  cumhaz_a_rows <- summary(fit_a, newdata = comp_cov, type = "cumhaz",
                           t = horizon, ci = FALSE, tidy = TRUE)$est
  log_surv_a <- .weighted_log_mean_exp(-cumhaz_a_rows)
  cumhaz_a <- -log_surv_a
  cumhaz_b <- summary(fit_b, type = "cumhaz", t = horizon, ci = FALSE,
                      tidy = TRUE)$est[1]
  log_chr <- if (is.finite(cumhaz_a) && is.finite(cumhaz_b) &&
                   cumhaz_a > 0 && cumhaz_b > 0) {
    log(cumhaz_a) - log(cumhaz_b)
  } else {
    NA_real_
  }

  # mlumr's Bayesian Gompertz constrains the shape to be positive, and its
  # generalized gamma is the positive-Q (Lawless k > 0) subfamily. flexsurv
  # admits the negative branch of both, so an STC benchmark can land outside the
  # family its label denotes and would then not be a like-for-like comparison
  # with the Bayesian fit of the same name. Report the parameter so stc() can
  # say so rather than leaving the reader to assume the spaces match.
  par_name <- switch(dist_fs, gompertz = "shape", gengamma = "Q", NULL)
  family_par <- NULL
  if (!is.null(par_name)) {
    family_par <- c(index = unname(fit_a$res[par_name, "est"]),
                    comparator = unname(fit_b$res[par_name, "est"]))
  }

  list(rmst_index = rmst_index, rmst_comparator = rmst_b,
       rmst_diff = rmst_index - rmst_b, log_chr = log_chr,
       family_par = family_par, family_par_name = par_name)
}

#' Map an mlumr survival distribution to a flexsurv distribution name
#' @keywords internal
.stc_flexsurv_dist <- function(distribution) {
  switch(distribution,
    exponential = "exp", "exponential-aft" = "exp",
    weibull = "weibull", "weibull-aft" = "weibull",
    gompertz = "gompertz", lognormal = "lnorm", loglogistic = "llogis",
    gamma = "gamma", gengamma = "gengamma",
    # Flexible baselines have no parametric STC analogue; approximate with
    # a Weibull G-computation.
    mspline = "weibull", pexp = "weibull",
    # No unnamed default: an unrecognized name must not fall through to a
    # Weibull fit that the result would then mislabel. stc() validates the name
    # before this point, so reaching here at all is a bug.
    stop("Unsupported survival distribution: ", distribution, call. = FALSE)
  )
}
