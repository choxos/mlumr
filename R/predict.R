#' Predictions from ML-UMR model
#'
#' Generate population-average absolute-outcome predictions in the index and
#' comparator populations.
#'
#' @param object An `mlumr_fit` object
#' @param population Which population: `"both"`, `"index"`, or `"comparator"`
#' @param type Prediction type: `"response"` or `"link"`. For `"response"`:
#'   probabilities (binomial), means (normal), or rates (poisson). For
#'   `"link"`: the fitted link applied to the population-standardized
#'   response mean, `g(E[g^{-1}(eta)])`.
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
#' Stan `generated quantities` block computes.
#'
#' `type = "link"` reports `g(E[g^{-1}(eta)])`: the fitted link applied to
#' that same standardized response mean. Differencing two `type = "link"`
#' predictions therefore gives a contrast on the fitted link scale. That
#' equals a reported effect only where the two scales coincide: a logit
#' binomial fit's `lor_*`. Elsewhere a transformation is needed, because
#' [marginal_effects()] reports on the scale conventional for the family
#' rather than on the fitted link. Poisson fits report the natural-scale rate
#' ratio, so the link difference is its logarithm; normal log-link fits report
#' a natural-scale mean difference; and probit and cloglog fits report
#' `lor_*`, `rd_*` and `rr_*`, none of which is a fitted-link contrast. It is
#' computed from
#' the log-scale generated quantities, so it stays finite where the
#' natural-scale mean would round to 0 or 1. The identity link makes the two
#' definitions agree; logit, probit, cloglog and log separate them.
#'
#' This is a deliberate divergence from \pkg{multinma}, which keeps the two
#' apart: its `predict(type = "link")` returns `E[eta]` and the marginal
#' link-scale contrast lives in `marginal_effects(mtype = "link")`. mlumr has
#' no conditional population estimand to pair `E[eta]` with, since every
#' effect it reports is standardized over a population, so it reports the
#' marginal link under the one name rather than offering two link scales that
#' differ silently.
#'
#' @return A data frame with predictions. When `type = "link"`, values are on
#'   the marginal link scale, `g(E[g^{-1}(eta)])`.
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

  # Survival predictions are curves over the prediction grid, not the single
  # `response`/`link` column this path assembles. The generic route would fail
  # on a missing draw name; say why instead.
  if (identical(family, "survival")) {
    stop("Predictions for survival fits (survival, hazard, cumhaz, rmst, ",
         "median, loghr) arrive with the prediction layer and are not ",
         "available from this build. The fitted draws hold the curves under ",
         "`surv_*`, `haz_*`, `cumhaz_*`, `loghr_*` and `rmst_*`.",
         call. = FALSE)
  }

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
      pred_draws <- .compute_marginal_link(object, pred_cols)
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

  # Survival effects need their own dispatch, which arrives with the prediction
  # layer. Without this guard the generic path below still finds draws for
  # `hr`: family_config maps it to `delta_*`, which Stan writes on the LOG
  # scale, so the caller would receive a log hazard ratio labeled `HR`. Failing
  # here is the difference between a missing feature and a wrong number.
  if (identical(family, "survival")) {
    stop("Marginal effects for survival fits arrive with the prediction layer ",
         "and are not available from this build. The fitted draws hold the ",
         "quantities themselves: `delta_index` / `delta_comparator` (log ",
         "hazard ratio or log time ratio), `rmst_diff_*`, and `loghr_*`.",
         call. = FALSE)
  }

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


#' Compute links of population-standardized response means
#'
#' Internal helper for [predict.mlumr_fit()] with `type = "link"`. Uses the
#' log-scale generated quantities that underlie the marginal response means so
#' the result remains finite when the natural-scale mean rounds to 0 or
#' overflows.
#'
#' @param object An `mlumr_fit` object.
#' @param pred_cols Character vector of response-scale prediction column names.
#' @return Data frame with the same column names as `pred_cols`, on the
#'   marginal link scale.
#' @keywords internal
.compute_marginal_link <- function(object, pred_cols) {
  draws <- object$draws
  family <- object$family %||% "binomial"
  prefix <- get_family_config(family)$predict_prefix
  suffix <- sub(paste0("^", prefix, "_"), "", pred_cols)

  out <- if (family == "binomial") {
    log_p_cols <- paste0("log_p_", suffix)
    log_q_cols <- paste0("log_q_", suffix)
    lnk <- object$link %||% "logit"
    if (all(c(log_p_cols, log_q_cols) %in% names(draws))) {
      as.data.frame(Map(
        function(p, q) .binary_link_from_logs(p, q, lnk),
        draws[log_p_cols], draws[log_q_cols]
      ))
    } else {
      .require_draw_columns(draws, pred_cols, "marginal link prediction")
      p <- as.matrix(draws[pred_cols])
      if (any(!is.finite(p)) || any(p <= 0 | p >= 1)) {
        stop("This older binary fit lacks stable marginal log-probability ",
             "draws; refit the model to obtain them.", call. = FALSE)
      }
      as.data.frame(apply(p, 2L, link_fun, link = lnk))
    }
  } else if (family == "poisson") {
    log_cols <- paste0("log_rate_", suffix)
    if (all(log_cols %in% names(draws))) {
      as.data.frame(draws[log_cols])
    } else {
      .require_draw_columns(draws, pred_cols, "marginal link prediction")
      rate <- as.matrix(draws[pred_cols])
      if (any(!is.finite(rate)) || any(rate <= 0)) {
        stop("This older Poisson fit lacks stable marginal log-rate draws; ",
             "refit the model to obtain them.", call. = FALSE)
      }
      as.data.frame(log(rate))
    }
  } else {
    link_cols <- paste0("link_y_", suffix)
    if (all(link_cols %in% names(draws))) {
      as.data.frame(draws[link_cols])
    } else {
      # Older normal-log fits did not store the stable log marginal mean.
      .require_draw_columns(draws, pred_cols, "marginal link prediction")
      vals <- as.data.frame(log(draws[pred_cols]))
      if (any(!is.finite(as.matrix(vals)))) {
        stop("This older normal-log fit lacks stable marginal link draws; refit ",
             "the model to obtain them.", call. = FALSE)
      }
      vals
    }
  }
  names(out) <- pred_cols
  out
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
