#' Predictions from ML-UMR model
#'
#' Generate population-average absolute-outcome predictions in the index and
#' comparator populations.
#'
#' @param object An `mlumr_fit` object
#' @param population Which population: `"both"`, `"index"`, or `"comparator"`
#' @param type Prediction type: `"response"` or `"link"`. For `"response"`:
#'   probabilities (binomial), means (normal), or rates (poisson). For
#'   `"link"`: mean linear predictor on the fitted link scale (logit, probit,
#'   cloglog, log, or identity). The link-scale values are computed directly
#'   from parameter draws as `E[eta]`, not as `link(E[g^{-1}(eta)])`, to
#'   avoid Jensen's inequality bias.
#' @param summary Return summary statistics (`TRUE`) or full posterior draws (`FALSE`)
#' @param probs Quantiles for summary (default `c(0.025, 0.5, 0.975)`)
#' @param ... Additional arguments (unused)
#'
#' @details
#' **Marginalization on non-identity links.** For `type = "response"` the
#' reported values are `E[g^{-1}(eta)]` — the posterior expectation of the
#' inverse-link-transformed linear predictor — *not* `g^{-1}(E[eta])`. The
#' two differ whenever `g` is non-linear (logit, probit, cloglog, log) by
#' Jensen's inequality. In the index population the expectation is taken
#' over IPD individuals; in the comparator population it is taken over the
#' integration points constructed by [add_integration()] from the AgD
#' moments. This is the correct population-average prediction for an
#' individual randomly drawn from that population, and it matches what the
#' Stan `generated quantities` block computes. For the link scale
#' (`type = "link"`) the reported value is `E[eta]`, a linear functional,
#' and the two interpretations coincide.
#'
#' @return A data frame with predictions. When `type = "link"`, values are
#'   mean linear predictors computed directly from parameter draws (avoiding
#'   Jensen's inequality bias).
#' @seealso [marginal_effects()] for treatment-effect summaries;
#'   [conditional_predict()] and [conditional_effects()] for predictions
#'   at specific covariate profiles.
#' @export
predict.mlumr_fit <- function(object,
                              population = c("both", "index", "comparator"),
                              type = c("response", "link"),
                              summary = TRUE,
                              probs = c(0.025, 0.5, 0.975),
                              ...) {

  .validate_mlumr_fit_object(object)
  population <- .validate_predict_choice(population, c("both", "index", "comparator"),
                                         "population")
  type <- .validate_predict_choice(type, c("response", "link"), "type")
  summary <- .validate_summary_flag(summary)
  .validate_probs(probs)

  family <- object$family %||% "binomial"

  cfg <- get_family_config(family)
  prefix <- cfg$predict_prefix

  all_vars <- paste0(prefix, "_",
                     c("index_index", "comparator_index",
                       "index_comparator", "comparator_comparator"))

  if (population == "index") {
    pred_cols <- all_vars[1:2]
    labels <- data.frame(
      treatment = c(object$data$index_treatment, object$data$comparator_treatment),
      population = "Index",
      stringsAsFactors = FALSE
    )
  } else if (population == "comparator") {
    pred_cols <- all_vars[3:4]
    labels <- data.frame(
      treatment = c(object$data$index_treatment, object$data$comparator_treatment),
      population = "Comparator",
      stringsAsFactors = FALSE
    )
  } else {
    pred_cols <- all_vars
    labels <- data.frame(
      treatment = rep(c(object$data$index_treatment, object$data$comparator_treatment), 2),
      population = rep(c("Index", "Comparator"), each = 2),
      stringsAsFactors = FALSE
    )
  }

  .require_draw_columns(object$draws, pred_cols, "prediction")
  pred_draws <- object$draws[, pred_cols, drop = FALSE]

  if (type == "link") {
    lnk <- object$link %||% cfg$link_default
    if (lnk != "identity") {
      # Compute mean linear predictors directly from parameter draws to avoid
      # Jensen's bias from link(E[inv_link(eta)]) != E[eta].
      pred_draws <- .compute_mean_lp(object, pred_cols)
    }
  }

  if (!summary) return(as.data.frame(pred_draws))

  summary_df <- .summarize_draw_matrix(pred_draws, probs)
  cbind(labels, summary_df, row.names = NULL)
}


#' Marginal treatment effects
#'
#' Extract marginal treatment effects from a fitted ML-UMR model. For
#' binomial: log odds ratio, risk difference, risk ratio. For normal:
#' mean difference. For poisson: rate ratio.
#'
#' @param object An `mlumr_fit` object
#' @param population Which population: `"both"`, `"index"`, or `"comparator"`
#' @param effect Which effect measure. For binomial: `"all"`, `"lor"`, `"rd"`,
#'   or `"rr"`. For normal: `"all"` or `"md"` (mean difference). For poisson:
#'   `"all"` or `"rr"` (rate ratio).
#' @param summary Return summary (`TRUE`) or full draws (`FALSE`)
#' @param probs Quantiles for summary
#'
#' @return A data frame
#' @seealso [predict.mlumr_fit()] for absolute predictions;
#'   [conditional_effects()] for covariate-conditional effects at specific
#'   profiles; [prior_sensitivity()] to check how strongly the marginal
#'   effect depends on `prior_beta`.
#' @export
#' @examples
#' \dontrun{
#' # All effect measures for both populations
#' marginal_effects(fit)
#'
#' # Only the log odds ratio in the index population
#' marginal_effects(fit, population = "index", effect = "lor")
#'
#' # Full posterior draws rather than summary statistics
#' marginal_effects(fit, summary = FALSE)
#' }
marginal_effects <- function(object,
                             population = c("both", "index", "comparator"),
                             effect = "all",
                             summary = TRUE,
                             probs = c(0.025, 0.5, 0.975)) {

  .validate_mlumr_fit_object(object)
  population <- .validate_predict_choice(population, c("both", "index", "comparator"),
                                         "population")
  effect <- .validate_effect_choice(effect)
  summary <- .validate_summary_flag(summary)
  .validate_probs(probs)

  family <- object$family %||% "binomial"
  cfg <- get_family_config(family)

  valid_effects <- c("all", cfg$effect_measures)
  if (!effect %in% valid_effects) {
    stop(sprintf("For %s family, `effect` must be one of: %s",
                 family, paste(valid_effects, collapse = ", ")), call. = FALSE)
  }
  if (effect == "all") {
    vars <- unlist(cfg$marginal_effect_vars, use.names = FALSE)
  } else {
    vars <- cfg$marginal_effect_vars[[effect]]
  }

  if (population == "index") {
    vars <- grep("_index$", vars, value = TRUE)
  } else if (population == "comparator") {
    vars <- grep("_comparator$", vars, value = TRUE)
  }

  .require_draw_columns(object$draws, vars, "marginal effect")
  effect_draws <- object$draws[, vars, drop = FALSE]

  if (!summary) return(as.data.frame(effect_draws))

  summary_df <- .summarize_draw_matrix(effect_draws, probs)

  # Invert marginal_effect_vars to map Stan variable name -> canonical
  # effect label. This is family-aware, so normal yields "MD" and poisson
  # yields "RR" (both use delta_* columns under different effect names)
  # rather than a regex that would collapse both to "DELTA".
  vmap <- cfg$marginal_effect_vars
  var_to_effect <- setNames(
    rep(names(vmap), lengths(vmap)),
    unlist(vmap, use.names = FALSE)
  )
  vars_row <- rownames(summary_df)
  pop_raw <- sub("^.*_(index|comparator)$", "\\1", vars_row)
  labels <- data.frame(
    variable = vars_row,
    effect = toupper(unname(var_to_effect[vars_row])),
    population = paste0(toupper(substring(pop_raw, 1, 1)),
                        substring(pop_raw, 2)),
    stringsAsFactors = FALSE
  )

  cbind(labels, summary_df, row.names = NULL)
}


#' Compute mean linear predictors from parameter draws
#'
#' Internal helper for predict.mlumr_fit with type="link". Computes
#' mean(eta_i) directly from parameter draws instead of applying the link
#' to marginalized response-scale draws, avoiding Jensen's inequality bias.
#'
#' @param object An mlumr_fit object
#' @param pred_cols Character vector of column names to compute
#' @return Data frame with the same columns as pred_cols, on the link scale
#' @keywords internal
.compute_mean_lp <- function(object, pred_cols) {
  draws <- object$draws
  family <- object$family %||% "binomial"
  covariates <- object$data$covariates
  n_cov <- length(covariates)
  is_relaxed <- object$model == "relaxed"
  n_draws <- nrow(draws)

  .require_draw_columns(draws, c("mu_index", "mu_comparator"), "linear predictor")
  mu_idx <- draws$mu_index
  mu_cmp <- draws$mu_comparator

  if (is_relaxed) {
    beta_idx_cols <- paste0("beta_index[", seq_len(n_cov), "]")
    beta_cmp_cols <- paste0("beta_comparator[", seq_len(n_cov), "]")
    .require_draw_columns(draws, c(beta_idx_cols, beta_cmp_cols), "linear predictor")
    beta_idx <- as.matrix(draws[, beta_idx_cols, drop = FALSE])
    beta_cmp <- as.matrix(draws[, beta_cmp_cols, drop = FALSE])
  } else {
    beta_cols <- paste0("beta[", seq_len(n_cov), "]")
    .require_draw_columns(draws, beta_cols, "linear predictor")
    beta_shared <- as.matrix(draws[, beta_cols, drop = FALSE])
    beta_idx <- beta_shared
    beta_cmp <- beta_shared
  }

  # Index population: mean LP over IPD covariates
  X_ipd <- as.matrix(object$data$ipd$data[, covariates, drop = FALSE])
  mean_X_ipd <- colMeans(X_ipd)

  lp_idx_index <- mu_idx + as.vector(beta_idx %*% mean_X_ipd)
  lp_cmp_index <- mu_cmp + as.vector(beta_cmp %*% mean_X_ipd)

  # Comparator population: weighted mean LP over AgD integration points
  X_int <- object$data$integration_points  # [n_agd_rows, n_int, n_cov]
  n_agd_rows <- dim(X_int)[1]

  # Family-appropriate weights (see family_config$comp_weight_field)
  stan_data <- object$stan_data
  wfield <- get_family_config(family)$comp_weight_field
  if (is.null(wfield)) {
    w <- rep(1, n_agd_rows)
  } else {
    w <- as.numeric(stan_data[[wfield]])
  }
  total_w <- sum(w)

  lp_idx_comp <- rep(0, n_draws)
  lp_cmp_comp <- rep(0, n_draws)
  for (k in seq_len(n_agd_rows)) {
    mean_X_k <- colMeans(matrix(X_int[k, , ], ncol = n_cov))
    lp_idx_comp <- lp_idx_comp + (mu_idx + as.vector(beta_idx %*% mean_X_k)) * w[k]
    lp_cmp_comp <- lp_cmp_comp + (mu_cmp + as.vector(beta_cmp %*% mean_X_k)) * w[k]
  }
  lp_idx_comp <- lp_idx_comp / total_w
  lp_cmp_comp <- lp_cmp_comp / total_w

  prefix <- get_family_config(family)$predict_prefix
  all_lp <- data.frame(
    lp_idx_index, lp_cmp_index, lp_idx_comp, lp_cmp_comp
  )
  names(all_lp) <- paste0(
    prefix, "_",
    c("index_index", "comparator_index",
      "index_comparator", "comparator_comparator")
  )

  all_lp[, pred_cols, drop = FALSE]
}


#' Validate an mlumr fit object
#' @keywords internal
.validate_mlumr_fit_object <- function(object) {
  if (!inherits(object, "mlumr_fit")) {
    stop("`object` must be an mlumr_fit object.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate an exact scalar prediction argument
#' @keywords internal
.validate_predict_choice <- function(x, choices, name) {
  if (identical(x, choices)) {
    return(choices[[1L]])
  }

  valid <- is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    nzchar(x) &&
    x %in% choices

  if (!valid) {
    stop(sprintf("`%s` must be one of: %s.",
                 name, paste(sprintf("'%s'", choices), collapse = ", ")),
         call. = FALSE)
  }

  x
}


#' Validate a scalar summary flag
#' @keywords internal
.validate_summary_flag <- function(summary) {
  valid <- is.logical(summary) && length(summary) == 1L && !is.na(summary)
  if (!valid) {
    stop("`summary` must be TRUE or FALSE.", call. = FALSE)
  }
  summary
}


#' Validate quantile probabilities
#' @keywords internal
.validate_probs <- function(probs) {
  valid <- is.numeric(probs) &&
    length(probs) > 0L &&
    all(is.finite(probs)) &&
    all(probs >= 0 & probs <= 1) &&
    !anyDuplicated(probs)

  if (!valid) {
    stop("`probs` must be unique finite numeric values between 0 and 1.",
         call. = FALSE)
  }

  invisible(TRUE)
}


#' Validate a scalar marginal-effect choice
#' @keywords internal
.validate_effect_choice <- function(effect) {
  valid <- is.character(effect) &&
    length(effect) == 1L &&
    !is.na(effect) &&
    nzchar(effect)

  if (!valid) {
    stop("`effect` must be a single non-missing string.", call. = FALSE)
  }

  effect
}


#' Require posterior draw columns
#' @keywords internal
.require_draw_columns <- function(draws, columns, context) {
  missing <- setdiff(columns, names(draws))
  if (length(missing) > 0L) {
    stop(sprintf("Missing %s draw column(s): %s.",
                 context, paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}
