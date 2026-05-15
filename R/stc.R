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
#' For binomial outcomes, the comparator-population treatment contrast is
#' transported to the index population **assuming the treatment contrast is
#' constant on the fitted link scale** (i.e., no effect modification on that
#' scale). Under this assumption, the index-population comparator
#' probability is computed as
#' `inv_link(link(p_A_index) - (link(p_A_comp) - link(p_B)))`, and its
#' uncertainty is propagated through the delta method. If effect modification
#' is expected, fit a Bayesian relaxed model with
#' `mlumr(..., model = "relaxed")` and use `predict(..., population =
#' "index")` instead, which does not require this assumption.
#'
#' @examples
#' \dontrun{
#' result <- stc(dat)
#' print(result)
#' }
stc <- function(data, link = NULL, conf_level = 0.95) {

  .validate_mlumr_data_object(data)

  family <- data$family %||% "binomial"
  ipd <- data$ipd$data
  agd <- data$agd$data
  cov_names <- data$covariates
  z <- .z_from_conf_level(conf_level)

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
  index_delta <- .stc_binomial_index_delta(
    fit, ipd, beta_hat, V, link_resolved,
    p_hat_A_comp, p_B, comp_delta
  )

  estimate <- comp_delta$link_effect
  se <- comp_delta$link_effect_se
  var_p_B <- p_B * (1 - p_B) / n_B
  p_hat_A_comp_se <- .sqrt_variance(comp_delta$var_p_A,
                                    "comparator probability variance")
  p_B_se <- sqrt(var_p_B)
  p_A_index_se <- .sqrt_variance(index_delta$var_p_A,
                                 "index probability variance")
  p_B_index_se <- .sqrt_variance(index_delta$var_p_B,
                                 "index comparator probability variance")
  p_hat_A_comp_ci <- .bounded_wald_interval(p_hat_A_comp, p_hat_A_comp_se, z,
                                            lower = 0, upper = 1)
  p_B_ci <- .bounded_wald_interval(p_B, p_B_se, z, lower = 0, upper = 1)
  p_A_index_ci <- .bounded_wald_interval(index_delta$p_A, p_A_index_se, z,
                                         lower = 0, upper = 1)
  p_B_index_ci <- .bounded_wald_interval(index_delta$p_B, p_B_index_se, z,
                                         lower = 0, upper = 1)

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
    p_hat_index = p_hat_A_comp,
    p_hat_index_se = p_hat_A_comp_se,
    p_hat_index_lower = p_hat_A_comp_ci$lower,
    p_hat_index_upper = p_hat_A_comp_ci$upper,
    p_comparator = p_B,
    p_comparator_se = p_B_se,
    p_comparator_lower = p_B_ci$lower,
    p_comparator_upper = p_B_ci$upper,
    p_index_index = index_delta$p_A,
    p_index_index_se = p_A_index_se,
    p_index_index_lower = p_A_index_ci$lower,
    p_index_index_upper = p_A_index_ci$upper,
    p_comparator_index = index_delta$p_B,
    p_comparator_index_se = p_B_index_se,
    p_comparator_index_lower = p_B_index_ci$lower,
    p_comparator_index_upper = p_B_index_ci$upper,
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

#' Index-population delta-method terms for binomial STC
#' @keywords internal
.stc_binomial_index_delta <- function(fit, ipd, beta_hat, V,
                                      link_resolved, p_A_comp, p_B_comp,
                                      comp_delta) {
  n_ipd <- nrow(ipd)
  p_A_index <- mean(predict(fit, newdata = ipd, type = "response"))
  p_A_index <- bound_probability(p_A_index, n_ipd)

  link_effect_comp <- link_fun(p_A_comp, link_resolved) -
    link_fun(p_B_comp, link_resolved)
  eta_B_index <- link_fun(p_A_index, link_resolved) - link_effect_comp
  p_B_index <- inverse_link(eta_B_index, link_resolved)
  p_B_index <- bound_probability(p_B_index, n_ipd)

  X_ipd_design <- .stc_model_matrix(fit, ipd)
  eta_ipd <- as.vector(X_ipd_design %*% beta_hat)
  p_ipd <- inverse_link(eta_ipd, link_resolved)
  dp_dbeta_ipd <- inverse_link_derivative(eta_ipd, p_ipd, link_resolved)
  grad_mean <- colMeans(dp_dbeta_ipd * X_ipd_design)
  var_p_A_index <- .nonnegative_variance(
    as.numeric(t(grad_mean) %*% V %*% grad_mean),
    "index probability variance"
  )

  dg_dp_A <- link_derivative_response(p_A_index, link_resolved)
  d_inv_link <- inverse_link_derivative(eta_B_index, p_B_index, link_resolved)
  var_p_B_index <- .nonnegative_variance(
    (d_inv_link * dg_dp_A)^2 * var_p_A_index +
      d_inv_link^2 * comp_delta$var_link_effect,
    "index comparator probability variance"
  )

  list(
    p_A = p_A_index,
    p_B = p_B_index,
    var_p_A = var_p_A_index,
    var_p_B = var_p_B_index
  )
}

#' Normal-outcome STC estimator
#' @keywords internal
.stc_normal <- function(data, fit, ipd, agd, newdata, link_resolved,
                        conf_level, z, beta_hat, V, n_int) {
  pred_y <- predict(fit, newdata = newdata, type = "response")
  weights <- if (data$has_integration) {
    rep(1 / agd$.se^2, each = n_int)
  } else {
    1 / (agd$.se^2)
  }
  y_hat_A <- weighted.mean(pred_y, weights)
  y_B <- weighted.mean(agd$.y, 1 / agd$.se^2)
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
  var_B <- 1 / sum(1 / agd$.se^2)
  se <- .sqrt_variance(var_A + var_B, "normal STC contrast variance")
  y_hat_A_index <- mean(predict(fit, newdata = ipd, type = "response"))

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "normal",
    link = link_resolved,
    y_hat_index = y_hat_A,
    y_hat_index_se = sqrt(var_A),
    y_comparator = y_B,
    y_comparator_se = sqrt(var_B),
    y_hat_index_index = y_hat_A_index,
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
  rate_hat_A_index <- mean(predict(fit, newdata = ipd, type = "response"))

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "poisson",
    link = link_resolved,
    rate_hat_index = rate_hat_A,
    rate_hat_index_se = sqrt(var_rate_A),
    rate_comparator = rate_B,
    rate_comparator_se = sqrt(rate_B / exposure_B),
    rate_hat_index_index = rate_hat_A_index,
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
