#' Simulated treatment comparison via G-computation
#'
#' Perform an unanchored simulated treatment comparison (STC) with marginal
#' standardization. Fits an outcome regression to the single IPD treatment arm,
#' standardizes its predictions over the comparator-population covariate
#' distribution, and contrasts that outcome with the reported comparator-arm
#' outcome in the same population. It does not standardize either treatment to
#' the index population.
#'
#' For binomial outcomes, returns the treatment effect on the link scale plus
#' event probabilities, risk difference, and log risk ratio with SEs and CIs
#' in the comparator population. Event-probability intervals use Wald standard
#' errors and are bounded to `[0, 1]`. When an observed arm has zero or all
#' events, transformed effect measures use the boundary-only pseudo-count
#' `(r + 0.5) / (n + 1)`; model predictions are never corrected. For Poisson
#' outcomes, the comparator log rate uses a 0.5 continuity correction when the
#' observed event count is zero.
#'
#' Scale note: `$estimate` (and the binomial `$log_rr`) is on the link / log
#' scale, where the null is 0. To compare against the natural-scale risk ratio
#' or rate ratio from [marginal_effects()] (where the null is 1), exponentiate
#' it (e.g. `exp(result$estimate)`).
#'
#' Normal-family weighting note: across multiple AgD rows, the normal STC
#' comparator-population prediction and observed mean use sample-size
#' (`outcome_n`) weights, matching the Bayesian ML-UMR comparator-population
#' estimand. `outcome_n` is required when there is more than one row; a single
#' row has weight one. The observed comparator-mean variance combines
#' independent, mutually exclusive strata as `sum(w^2 * se^2)` using normalized
#' population weights.
#'
#' @param data An `mlumr_data` object from [combine_data()]. Integration points
#'   from [add_integration()] are required whenever the outcome model uses a
#'   nonlinear link (binomial, Poisson, normal-log, or survival). Substitution
#'   of aggregate means is exact only for a normal identity-link model.
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
#'   poisson: `"log"` (default). Ignored for survival. If `NULL`, uses the
#'   canonical default.
#' @param conf_level Confidence level for the interval (default 0.95)
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
#'    AgD covariate means for the identity-link normal special case).
#' 3. Marginalize predictions over the comparator population.
#' 4. Contrast with the reported comparator outcome in that population.
#' 5. Compute first-order, fixed-integration-grid delta-method standard errors.
#'
#' The response-scale standardization follows the marginalization order used by
#' Ren et al.'s unanchored STC and by parametric G-computation: predict each
#' target profile, average the natural-scale outcomes, then transform that
#' average. This is a one-arm standardization benchmark: only the index-treatment
#' outcome model is fitted because comparator IPD are unavailable. Remiro-Azocar
#' et al. implement two-arm G-computation, where both potential outcomes are
#' predicted from an IPD study; that is a different data design even though the
#' response-scale marginalization step is shared.
#'
#' The non-survival standard error is conditional on the supplied integration
#' grid and reported comparator covariate summaries. It propagates fitted
#' regression-coefficient uncertainty and observed comparator-outcome
#' uncertainty, but not uncertainty from reconstructing the comparator
#' covariate distribution. Ren et al. instead resample the IPD, reconstruct the
#' target distribution, and use a nonparametric bootstrap. Use the present
#' delta-method result as a fast benchmark and use sensitivity analyses when
#' reconstruction uncertainty may matter.
#'
#' The estimator relies on correct specification of the index-treatment outcome
#' model and its applicability to the comparator population. It does not model
#' posterior uncertainty in population covariate distributions or relax
#' treatment-specific covariate effects.
#' When clinically meaningful effect modification is plausible, prefer
#' `mlumr(..., model = "relaxed")` as the primary analysis and use STC as a
#' sensitivity or benchmarking analysis.
#'
#' The returned effect is defined in the comparator population. Applying that
#' same effect to the index or another decision population is a separate
#' effect-equality assumption. `stc()` does not standardize to, perform, or
#' validate transport to the index population. This differs from two-arm
#' parametric G-computation, which fits treatment-specific outcome regressions
#' and can standardize both potential outcomes to a chosen target population.
#'
#' @references
#' Ren S, Ren S, Welton NJ, Strong M (2024). Advancing unanchored simulated
#' treatment comparisons: A novel implementation and simulation study.
#' *Research Synthesis Methods*, 15(4), 657-670.
#' \doi{10.1002/jrsm.1718}
#'
#' Remiro-Azocar A, Heath A, Baio G (2022). Parametric G-computation for
#' compatible indirect treatment comparisons with limited individual patient
#' data. *Research Synthesis Methods*, 13(6), 716-744.
#' \doi{10.1002/jrsm.1565}
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

  if (!isTRUE(data$has_integration) &&
        !(family == "normal" && link_resolved == "identity")) {
    stop("STC with a nonlinear link requires comparator-population integration ",
         "points. Call add_integration(); substituting aggregate covariate ",
         "means is generally biased.", call. = FALSE)
  }

  if (family == "binomial") {
    glm_family <- binomial(link = link_resolved)
  } else if (family == "normal") {
    glm_family <- gaussian(link = link_resolved)
  } else {
    glm_family <- poisson(link = link_resolved)
  }

  fit <- glm(.stc_formula(cov_names, family), family = glm_family, data = ipd,
             start = .stc_start_values(ipd, cov_names, glm_family))
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
  .stc_refuse_separation(fit)
  list(beta_hat = beta_hat, V = V)
}

#' Refuse a fit whose likelihood has no finite maximum
#'
#' The checks above look for a failure the fitting reports, and separation is
#' not one. Iterative reweighting stops when the deviance stops changing, and a
#' separated fit has no maximum for it to stop at: the deviance falls to about
#' 6e-10 while the intercept is still drifting, so the criterion fires anyway
#' and a binomial arm with no events comes back with `converged = TRUE`, finite
#' coefficients and a finite covariance. 100 rows with the outcome always zero
#' produce a coefficient of about -26.6 and a largest fitted probability of
#' 3e-12, with a confidence interval to match. Raising `maxit` changes none of
#' those numbers, which is what shows the iteration limit is not what stopped
#' it. Every number there is a property of where the iteration stopped, not of
#' the data, and reporting it as an estimate is worse than reporting nothing,
#' because nothing about it looks wrong.
#'
#' The symptom is the one thing separation always leaves: every fitted
#' probability pinned against 0 or 1, whether they all sit at one boundary
#' (an arm with no events) or split between the two (a covariate that
#' separates the outcome). A rate can legitimately be small, so the test is on
#' the boundary rather than on smallness, and it applies only where a boundary
#' exists.
#'
#' What it does not catch is quasi-complete separation, where rows sit on the
#' separating hyperplane: `y = c(0, 0, 1, 1)` on `x = c(-1, 0, 0, 1)` has no
#' finite slope, yet the two tied rows keep fitted probabilities of exactly
#' 0.5, so not every probability has reached a boundary. Telling that apart
#' from a strong but identified fit takes more than the fitted values, since a
#' legitimate signal here reaches a linear predictor of 20.1 while this case
#' reaches 19.6. The exact test is a linear program rather than a threshold,
#' so covering it is a dependency decision and not a correction to this test.
#' @param fit A fitted `glm`.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
.stc_refuse_separation <- function(fit) {
  fam <- tryCatch(stats::family(fit)$family, error = function(e) NA_character_)
  if (!identical(fam, "binomial")) {
    return(invisible(NULL))
  }
  mu <- stats::fitted(fit)
  mu <- mu[is.finite(mu)]
  if (!length(mu)) {
    return(invisible(NULL))
  }
  eps <- .Machine$double.eps^0.5
  # Every fitted probability at *a* boundary, not all at the same one. A
  # covariate that perfectly separates the outcome sends its two groups to
  # opposite boundaries, which is the ordinary presentation of separation and
  # the one an arm-level test misses: `y ~ x` with the two equal gives fitted
  # probabilities of 2e-11 and 1, `converged = TRUE`, and a slope of 49.
  if (all(mu < eps | mu > 1 - eps)) {
    stop(
      paste(
        "The STC outcome model is separated: every fitted probability sits at",
        "0 or 1, which happens when an arm has no events or no non-events.",
        "The likelihood has no finite maximum there, so the coefficients and",
        "the interval would describe where the fitting stopped rather than",
        "the data. Use mlumr(), whose prior makes the posterior proper."
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Starting values for the links that cannot find their own
#'
#' A Gaussian model with a log link has mean `exp(eta)`, which is defined for
#' any outcome, but R's initialization takes `log(y)` and stops on any value
#' at or below zero: an outcome of `c(-1, 1, 2, 2, 3, 4)`, whose group means
#' are both positive and whose fit exists, was refused with "cannot find valid
#' starting values". The intercept is put at the log of the mean outcome and
#' the slopes at zero, which is inside the parameter space whenever the mean
#' is positive, and the fitting proceeds from there.
#'
#' `NULL` everywhere else, so every other family and link keeps the
#' initialization it already had.
#' @param ipd The individual data being fitted.
#' @param cov_names The covariates in the model.
#' @param glm_family The resolved family object.
#' @return A numeric vector of starting values, or `NULL`.
#' @keywords internal
.stc_start_values <- function(ipd, cov_names, glm_family) {
  gaussian_log <- identical(glm_family$family, "gaussian") &&
    identical(glm_family$link, "log")
  if (!gaussian_log) {
    return(NULL)
  }
  y <- ipd$.outcome
  mu <- mean(y, na.rm = TRUE)
  if (!is.finite(mu) || mu <= 0) {
    return(NULL)
  }
  c(log(mu), rep(0, length(cov_names)))
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
  weights <- if (data$has_integration) rep(agd$.n, each = n_int) else agd$.n
  eta_comp <- as.numeric(predict(fit, newdata = newdata, type = "link"))
  lp_comp <- .binary_log_probs(eta_comp, link_resolved)
  log_p_A_comp <- .weighted_log_mean_exp(lp_comp$event, weights)
  log_q_A_comp <- .weighted_log_mean_exp(lp_comp$nonevent, weights)
  p_hat_A_comp <- exp(log_p_A_comp)
  n_B <- sum(agd$.n)
  p_B <- sum(agd$.r) / n_B
  p_B_effect <- bound_probability(p_B, n_B)
  row_p <- agd$.r / agd$.n
  row_w <- .normalize_weights(agd$.n)
  # Take the boundary correction from the POOLED n, as `.naive_binomial()`
  # does. With each row's own n the answer depends on how one comparator arm
  # was tabulated: 0/100 corrects to 0.5/101, while 0/50 + 0/50 corrects to
  # 0.5/51 twice, so two descriptions of the same data give different standard
  # errors. Interior rows are untouched either way.
  row_p_effect <- bound_probability(row_p, n_B)
  var_p_B_effect <- sum(
    row_w^2 * row_p_effect * (1 - row_p_effect) / agd$.n
  )

  comp_delta <- .stc_binomial_comparator_delta(
    fit, newdata, weights, beta_hat, V, link_resolved,
    log_p_A_comp, log_q_A_comp, p_B_effect, var_p_B_effect
  )

  estimate <- comp_delta$link_effect
  se <- comp_delta$link_effect_se
  p_hat_A_comp_se <- .sqrt_variance(comp_delta$var_p_A,
                                    "comparator probability variance")
  # Use the boundary-corrected variance on the absolute scale too. With raw
  # `row_p` a zero-event or all-event comparator arm has p(1 - p) = 0 and
  # contributes no uncertainty: 0/100 gave p_B_se = 0, a degenerate [0, 0]
  # interval, and a risk difference whose SE ignored the comparator entirely,
  # although 0/100 alone is consistent with p up to roughly 0.03. The
  # link-scale effect and the log risk ratio already used the corrected
  # variance; these did not. This mirrors `.naive_binomial()`.
  p_B_se <- .sqrt_variance(var_p_B_effect, "comparator probability variance")
  p_hat_A_comp_ci <- .bounded_wald_interval(p_hat_A_comp, p_hat_A_comp_se, z,
                                            lower = 0, upper = 1)
  p_B_ci <- .bounded_wald_interval(p_B, p_B_se, z, lower = 0, upper = 1)

  rd <- p_hat_A_comp - p_B
  se_rd <- .sqrt_variance(comp_delta$var_p_A + var_p_B_effect,
                          "risk-difference variance")

  log_rr <- log_p_A_comp - log(p_B_effect)
  se_log_rr <- .sqrt_variance(
    comp_delta$var_log_p_A + var_p_B_effect / p_B_effect^2,
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
    p_index_comparator = p_hat_A_comp,
    p_hat_index = p_hat_A_comp,
    p_hat_index_se = p_hat_A_comp_se,
    p_hat_index_lower = p_hat_A_comp_ci$lower,
    p_hat_index_upper = p_hat_A_comp_ci$upper,
    p_comparator_comparator = p_B,
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
                                           log_p_A, log_q_A, p_B,
                                           var_p_B) {
  X_comp_design <- .stc_model_matrix(fit, newdata)
  log_means <- function(beta) {
    lp <- .binary_log_probs(as.vector(X_comp_design %*% beta), link_resolved)
    c(event = .weighted_log_mean_exp(lp$event, weights),
      nonevent = .weighted_log_mean_exp(lp$nonevent, weights))
  }
  link_A <- function(beta) {
    lm <- log_means(beta)
    .binary_link_from_logs(lm["event"], lm["nonevent"], link_resolved)
  }
  p_A <- function(beta) exp(log_means(beta)["event"])
  log_p <- function(beta) log_means(beta)["event"]
  grad_link <- .stc_numeric_gradient(link_A, beta_hat)
  grad_mean <- .stc_numeric_gradient(p_A, beta_hat)
  grad_log_p <- .stc_numeric_gradient(log_p, beta_hat)

  var_link_A <- as.numeric(t(grad_link) %*% V %*% grad_link)
  var_link_B <- link_derivative_response(p_B, link_resolved)^2 * var_p_B
  var_link_effect <- .nonnegative_variance(var_link_A + var_link_B,
                                           "comparator link-effect variance")
  var_p_A <- .nonnegative_variance(as.numeric(t(grad_mean) %*% V %*% grad_mean),
                                   "comparator probability variance")
  var_log_p_A <- .nonnegative_variance(
    as.numeric(t(grad_log_p) %*% V %*% grad_log_p),
    "comparator log-probability variance"
  )

  list(
    link_effect = .binary_link_from_logs(log_p_A, log_q_A, link_resolved) -
      link_fun(p_B, link_resolved),
    link_effect_se = sqrt(var_link_effect),
    var_link_effect = var_link_effect,
    var_p_A = var_p_A,
    var_log_p_A = var_log_p_A
  )
}

#' Numerical gradient for fixed-grid STC delta-method summaries
#' @keywords internal
.stc_numeric_gradient <- function(fn, beta) {
  step <- .Machine$double.eps^(1 / 3) * pmax(1, abs(beta))
  vapply(seq_along(beta), function(j) {
    upper <- lower <- beta
    upper[j] <- upper[j] + step[j]
    lower[j] <- lower[j] - step[j]
    (fn(upper) - fn(lower)) / (2 * step[j])
  }, numeric(1))
}

#' Stable Euclidean norm of two standard errors
#' @keywords internal
.stc_hypot <- function(x, y) {
  if (any(is.infinite(c(x, y)))) return(Inf)
  scale <- max(abs(c(x, y)))
  if (scale == 0) return(0)
  scale * sqrt((x / scale)^2 + (y / scale)^2)
}

#' Normal-outcome STC estimator
#' @keywords internal
.stc_normal <- function(data, fit, ipd, agd, newdata, link_resolved,
                        conf_level, z, beta_hat, V, n_int) {
  if (nrow(agd) > 1L && is.null(agd$.n)) {
    stop("`outcome_n` is required for multiple normal AgD rows.", call. = FALSE)
  }
  agd_weights <- agd$.n %||% 1
  weights <- if (data$has_integration) {
    rep(agd_weights, each = n_int)
  } else {
    agd_weights
  }
  y_B <- sum(.normalize_weights(agd_weights) * agd$.y)
  X_comp_design <- .stc_model_matrix(fit, newdata)
  w_norm <- .normalize_weights(weights)
  eta_comp <- as.vector(X_comp_design %*% beta_hat)

  if (link_resolved == "identity") {
    y_hat_A <- sum(w_norm * eta_comp)
    grad <- colSums(w_norm * X_comp_design)
    var_A <- .nonnegative_variance(as.numeric(t(grad) %*% V %*% grad),
                                   "normal STC standardized-mean variance")
    se_A <- sqrt(var_A)
  } else {
    log_y_hat_A <- .weighted_log_mean_exp(eta_comp, weights)
    y_hat_A <- exp(log_y_hat_A)
    log_contribution <- log(weights) + eta_comp
    contribution_max <- max(log_contribution)
    centered_weights <- exp(log_contribution - contribution_max)
    centered_weights <- centered_weights / sum(centered_weights)
    grad_log <- colSums(centered_weights * X_comp_design)
    var_log_A <- .nonnegative_variance(
      as.numeric(t(grad_log) %*% V %*% grad_log),
      "normal STC standardized log-mean variance"
    )
    se_A <- if (var_log_A == 0) 0 else
      exp(log_y_hat_A + 0.5 * log(var_log_A))
  }
  w_B <- .normalize_weights(agd_weights)
  var_B <- sum(w_B^2 * agd$.se^2)
  se_B <- sqrt(var_B)
  md <- if (link_resolved == "log" && y_B > 0) {
    .exp_difference_logs(log_y_hat_A, log(y_B))
  } else {
    y_hat_A - y_B
  }
  md_se <- .stc_hypot(se_A, se_B)
  if (link_resolved == "log") {
    if (!is.finite(y_B) || y_B <= 0) {
      stop("Normal-log STC requires a positive comparator aggregate mean.",
           call. = FALSE)
    }
    estimate <- log_y_hat_A - log(y_B)
    se <- .sqrt_variance(var_log_A + var_B / y_B^2,
                         "normal STC log-mean-ratio variance")
  } else {
    estimate <- md
    se <- md_se
  }
  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "normal",
    link = link_resolved,
    population = "comparator",
    md = md,
    md_se = md_se,
    md_lower = md - z * md_se,
    md_upper = md + z * md_se,
    y_index_comparator = y_hat_A,
    y_hat_index = y_hat_A,
    y_hat_index_se = se_A,
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
  weights <- if (data$has_integration) rep(agd$.E, each = n_int) else agd$.E
  events_B <- sum(agd$.r)
  exposure_B <- sum(agd$.E)
  rate_B <- events_B / exposure_B
  events_B_adjusted <- max(events_B, 0.5)
  rate_B_for_log <- events_B_adjusted / exposure_B
  X_comp_design <- .stc_model_matrix(fit, newdata)
  eta_comp <- as.vector(X_comp_design %*% beta_hat)
  log_rate_hat_A <- .weighted_log_mean_exp(eta_comp, weights)
  rate_hat_A <- exp(log_rate_hat_A)
  estimate <- log_rate_hat_A - log(rate_B_for_log)
  log_contribution <- log(weights) + eta_comp
  contribution_max <- max(log_contribution)
  contribution <- exp(log_contribution - contribution_max)
  contribution <- contribution / sum(contribution)
  grad_log_rate <- colSums(contribution * X_comp_design)
  var_lrr_A <- .nonnegative_variance(
    as.numeric(t(grad_log_rate) %*% V %*% grad_log_rate),
    "poisson STC standardized log-rate variance"
  )
  var_lrr_B <- 1 / events_B_adjusted
  se <- .sqrt_variance(var_lrr_A + var_lrr_B,
                       "poisson STC contrast variance")

  # Gradient of the standardized RATE itself, not of its logarithm:
  # d/dbeta of sum_i w_norm_i * exp(eta_i) is sum_i w_norm_i * lambda_i * X_i.
  # The log-rate gradient above normalizes by the exponentially weighted
  # `contribution` instead, which is a different weighting, so both are needed.
  w_norm <- weights / sum(weights)
  lambda_comp <- exp(eta_comp)
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
    # Reached only where the mean profile IS the standardized quantity: stc()
    # rejects a nonlinear link without integration points up front, because
    # g^-1(mu + Xbar'beta) is not E_X[g^-1(mu + X'beta)] there.
    newdata <- .stc_agd_mean_newdata(agd, cov_names)
  }

  if (family == "poisson") {
    newdata$.exposure <- 1
  }

  list(newdata = newdata, n_int = n_int)
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
  # A finite covariance is not yet a usable one. The optimizer can report
  # convergence while the Hessian is not positive definite, which is a saddle
  # or boundary point rather than a maximum; flexsurv then returns finite
  # variances that can be zero or negative. Checking only for NA / Inf accepts
  # that fit, and the bootstrap counts it among its successes.
  if (!is.null(fit$cov) && length(fit$cov) > 0L) {
    v <- diag(as.matrix(fit$cov))
    ev <- tryCatch(
      eigen(as.matrix(fit$cov), symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) NA_real_
    )
    if (any(v <= 0) || anyNA(ev) || min(ev) <= 0) {
      stop("The survival STC fit for the ", arm, " arm returned a covariance ",
           "matrix that is not positive definite, so the optimizer stopped at ",
           "a saddle or boundary point rather than a maximum and its ",
           "uncertainty is not usable.", call. = FALSE)
    }
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
