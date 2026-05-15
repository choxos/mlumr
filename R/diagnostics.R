#' Extract the full pointwise log-likelihood matrix from an mlumr_fit
#'
#' Combines the IPD and AgD per-observation log-likelihood draws into a
#' single matrix with one row per posterior draw and one column per
#' observation. The result is suitable for direct use with
#' [loo::loo()] / [loo::waic()].
#'
#' @param object An `mlumr_fit` object.
#' @return A numeric matrix of dimension `n_draws x (n_ipd + n_agd_rows)`.
#'   IPD columns come first, then AgD rows.
#' @keywords internal
extract_log_lik <- function(object) {
  .validate_mlumr_fit_object(object)

  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) {
    stop("`object$draws` must contain named posterior draw columns.",
         call. = FALSE)
  }

  ipd_cols <- .ordered_log_lik_columns(draws, "ipd")
  agd_cols <- .ordered_log_lik_columns(draws, "agd")
  if (length(ipd_cols) == 0L && length(agd_cols) == 0L) {
    stop(
      "Pointwise log-likelihood columns not found in draws. ",
      "This package requires Stan models that generate `log_lik_ipd` and ",
      "`log_lik_agd` as vectors (per-observation). If you have a fit from ",
      "a pre-v0.2 version of mlumr, refit with the current version.",
      call. = FALSE
    )
  }

  selected <- draws[, c(ipd_cols, agd_cols), drop = FALSE]
  numeric_cols <- vapply(selected, is.numeric, logical(1))
  if (!all(numeric_cols)) {
    stop("Pointwise log-likelihood columns must be numeric.", call. = FALSE)
  }

  log_lik <- as.matrix(selected)
  .validate_log_lik_matrix(log_lik)
  log_lik
}


#' Ordered pointwise log-likelihood columns for one data source
#' @keywords internal
.ordered_log_lik_columns <- function(draws, source) {
  pattern <- sprintf("^log_lik_%s\\[[0-9]+\\]$", source)
  cols <- grep(pattern, colnames(draws), value = TRUE)
  if (length(cols) == 0L) {
    return(character())
  }
  cols[order(.log_lik_column_index(cols, source))]
}


#' Extract integer indexes from Stan vector column names
#' @keywords internal
.log_lik_column_index <- function(cols, source) {
  pattern <- sprintf("^log_lik_%s\\[|\\]$", source)
  as.integer(gsub(pattern, "", cols))
}


#' Validate a pointwise log-likelihood matrix
#' @keywords internal
.validate_log_lik_matrix <- function(log_lik) {
  if (!is.matrix(log_lik) || !is.numeric(log_lik)) {
    stop("Pointwise log-likelihood must be a numeric matrix.", call. = FALSE)
  }
  if (nrow(log_lik) == 0L || ncol(log_lik) == 0L) {
    stop("Pointwise log-likelihood matrix must be non-empty.", call. = FALSE)
  }
  if (anyNA(log_lik) || any(!is.finite(log_lik))) {
    stop("Pointwise log-likelihood values must be finite.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Calculate DIC for model comparison
#'
#' Computes the Deviance Information Criterion using the variance-based
#' effective-parameters formula from Gelman et al. (2004): `pD = 0.5 * Var(D)`.
#' This is more stable than the plug-in alternative for multimodal posteriors.
#'
#' DIC is retained for backward compatibility and rough comparison. For
#' principled Bayesian model comparison, prefer [calculate_loo()] or
#' [calculate_waic()] (Vehtari, Gelman, Gabry 2017).
#'
#' @param object An `mlumr_fit` object
#'
#' @return A list of class `mlumr_dic` with components `DIC`, `pD`, `D_bar`
#' @export
#'
#' @examples
#' \dontrun{
#' dic_spfa <- calculate_dic(fit_spfa)
#' dic_relaxed <- calculate_dic(fit_relaxed)
#' }
calculate_dic <- function(object) {

  log_lik <- extract_log_lik(object)
  if (nrow(log_lik) < 2L) {
    stop("DIC requires at least two posterior draws.", call. = FALSE)
  }
  log_lik_total <- rowSums(log_lik)
  D <- -2 * log_lik_total
  D_bar <- mean(D)
  # Effective number of parameters (Gelman et al. 2004, variance-based):
  # pD = 0.5 * Var(D). More stable than the plug-in alternative (D_bar - D_hat)
  # when the posterior is multimodal.
  pD <- 0.5 * var(D)
  DIC <- D_bar + pD

  out <- list(
    DIC = DIC,
    pD = pD,
    D_bar = D_bar,
    model = .mlumr_model_label(object)
  )

  class(out) <- "mlumr_dic"
  out
}


#' @method print mlumr_dic
#' @export
print.mlumr_dic <- function(x, ...) {
  cat("DIC for ML-UMR Model\n")
  cat("====================\n\n")
  cat("Model:", x$model, "\n")
  cat("DIC:", round(x$DIC, 2), "\n")
  cat("pD:", round(x$pD, 2), "\n")
  cat("D_bar:", round(x$D_bar, 2), "\n")
  invisible(x)
}


#' Calculate LOO-CV for an mlumr_fit
#'
#' Computes approximate leave-one-out cross-validation (PSIS-LOO, Vehtari,
#' Gelman, Gabry 2017) using the pointwise log-likelihoods stored by the
#' Stan models. Returns a `loo` object from the `loo` package.
#'
#' Pareto-k diagnostics: values > 0.7 indicate observations for which the
#' PSIS approximation is unreliable; the printed output flags these.
#' Typical remedies are running more iterations, using `moment_match = TRUE`,
#' or (for highly influential AgD rows) refitting without the offending
#' observation to check sensitivity.
#'
#' @note
#' **AgD rows are treated as independent observations.** Each AgD row
#' contributes one column to the pointwise `log_lik` matrix. If two or
#' more AgD rows come from the same study (e.g. subgroup summaries
#' within a single trial) the PSIS-LOO approximation does not account
#' for the within-study clustering; effective sample sizes are
#' inflated and Pareto-k warnings are understated. For clustered AgD,
#' corroborate with [prior_sensitivity()] or refit omitting suspect
#' rows to check the influence on the posterior.
#'
#' @param object An `mlumr_fit` object.
#' @param ... Additional arguments passed to [loo::loo.array()].
#'
#' @return An object of class `psis_loo` (see [loo::loo()]).
#' @export
#' @examples
#' \dontrun{
#' loo_spfa <- calculate_loo(fit_spfa)
#' print(loo_spfa)
#' }
calculate_loo <- function(object, ...) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for calculate_loo(). ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  log_lik <- extract_log_lik(object)
  r_eff <- .relative_eff_from_log_lik(log_lik, .chain_id(object))
  loo::loo(log_lik, r_eff = r_eff, ...)
}


#' Calculate WAIC for an mlumr_fit
#'
#' Watanabe-Akaike Information Criterion (Watanabe 2010) based on the
#' pointwise log-likelihoods. WAIC is asymptotically equivalent to
#' LOO-CV; prefer [calculate_loo()] when Pareto-k is well-behaved.
#'
#' @note
#' As with [calculate_loo()], each AgD row is treated as an
#' independent observation. WAIC will be optimistic for AgD rows
#' that share a study (see the note on `calculate_loo()`).
#'
#' @param object An `mlumr_fit` object.
#' @param ... Additional arguments passed to [loo::waic()].
#'
#' @return An object of class `waic` (see [loo::waic()]).
#' @export
#' @examples
#' \dontrun{
#' waic_spfa <- calculate_waic(fit_spfa)
#' }
calculate_waic <- function(object, ...) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for calculate_waic(). ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  log_lik <- extract_log_lik(object)
  loo::waic(log_lik, ...)
}


#' Compare fitted ML-UMR models
#'
#' Compare two or more `mlumr_fit` objects by DIC (default), LOO, or WAIC.
#' For LOO/WAIC, [loo::loo_compare()] is used under the hood; the output
#' is the standard `loo_compare` table. For DIC the return is a data
#' frame ordered by DIC.
#'
#' DIC is the default for backward compatibility and because it has no
#' additional package dependencies. For principled Bayesian model
#' comparison, LOO (Vehtari, Gelman, Gabry 2017) is preferred and
#' requires the optional `loo` package.
#'
#' @param ... Two or more `mlumr_fit` objects. For DIC, `mlumr_dic`
#'   objects are also accepted.
#' @param criterion One of `"dic"` (default), `"loo"`, or `"waic"`.
#'   LOO and WAIC require the optional `loo` package.
#'
#' @return For `"loo"` / `"waic"`: a `compare.loo` table from
#'   [loo::loo_compare()]. For `"dic"`: a data frame (invisibly) with
#'   columns `Model`, `DIC`, `pD`, `Delta_DIC`.
#' @export
compare_models <- function(..., criterion = c("dic", "loo", "waic")) {

  criterion <- .validate_diagnostic_choice(criterion, c("dic", "loo", "waic"),
                                           "criterion")
  models <- list(...)
  .validate_model_count(models)

  if (criterion == "dic") {
    dics <- lapply(models, function(m) {
      if (inherits(m, "mlumr_fit")) {
        calculate_dic(m)
      } else if (inherits(m, "mlumr_dic")) {
        m
      } else {
        stop("For criterion = 'dic', arguments must be mlumr_fit or mlumr_dic objects",
             call. = FALSE)
      }
    })

    model_names <- vapply(dics, function(d) d$model, character(1))
    dic_vals <- vapply(dics, function(d) d$DIC, numeric(1))
    pD_vals <- vapply(dics, function(d) d$pD, numeric(1))
    model_names <- .comparison_names(models, model_names)

    out <- data.frame(
      Model = model_names,
      DIC = round(dic_vals, 2),
      pD = round(pD_vals, 2),
      Delta_DIC = round(dic_vals - min(dic_vals), 2)
    )

    out <- out[order(out$DIC), ]
    rownames(out) <- NULL

    cat("\nModel Comparison (DIC)\n")
    cat("======================\n\n")
    print(out, row.names = FALSE)
    cat("\nLower DIC = better fit. Delta_DIC > 5 is a rough heuristic for\n")
    cat("meaningful difference, not a formally calibrated threshold.\n")
    cat("DIC should not be the sole basis for model selection.\n")

    return(invisible(out))
  }

  # LOO / WAIC path
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for LOO/WAIC comparison. ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  if (!all(vapply(models, inherits, logical(1), what = "mlumr_fit"))) {
    stop("For criterion = 'loo' or 'waic', all arguments must be mlumr_fit objects",
         call. = FALSE)
  }

  calc_fn <- if (criterion == "loo") calculate_loo else calculate_waic
  ic_list <- lapply(models, calc_fn)
  names(ic_list) <- .comparison_names(
    models,
    vapply(models, .mlumr_model_label, character(1))
  )

  cat(sprintf("\nModel Comparison (%s)\n", toupper(criterion)))
  cat(strrep("=", 22), "\n\n", sep = "")
  cmp <- loo::loo_compare(ic_list)
  print(cmp)
  cat("\nelpd_diff is the difference in expected log pointwise predictive\n")
  cat("density vs the best model. se_diff > 2 is the conventional threshold\n")
  cat("for a meaningful difference (|elpd_diff| > 4 * se_diff is stronger).\n")
  invisible(cmp)
}


# Helper: recover chain ids from draws ordering.
# rstan stores draws in chain-major order (post-warmup iterations per chain
# contiguous). cmdstanr's format = "df" is similar. If sampling_args is
# unavailable we fall back to all-one (treated as a single chain, which
# inflates r_eff but does not produce incorrect elpd).
.chain_id <- function(object) {
  n_draws <- nrow(object$draws)
  if (!is.numeric(n_draws) || length(n_draws) != 1L || n_draws < 1L) {
    return(integer())
  }
  chains <- object$sampling_args$chains %||% 1L
  valid_chains <- is.numeric(chains) &&
    length(chains) == 1L &&
    is.finite(chains) &&
    chains >= 1 &&
    chains == as.integer(chains)
  if (!valid_chains) {
    return(rep(1L, n_draws))
  }
  chains <- as.integer(chains)
  iter_per_chain <- n_draws %/% chains
  if (iter_per_chain * chains != n_draws) return(rep(1L, n_draws))
  rep(seq_len(chains), each = iter_per_chain)
}


#' Compute stable relative effective sample sizes from log-likelihoods
#' @keywords internal
.relative_eff_from_log_lik <- function(log_lik, chain_id) {
  .validate_log_lik_matrix(log_lik)
  col_max <- apply(log_lik, 2L, max)
  stabilized <- sweep(log_lik, 2L, col_max, "-")
  loo::relative_eff(exp(stabilized), chain_id = chain_id)
}


#' Validate a diagnostics/model-comparison choice
#' @keywords internal
.validate_diagnostic_choice <- function(x, choices, name) {
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


#' Validate that model comparison has at least two candidates
#' @keywords internal
.validate_model_count <- function(models) {
  if (length(models) < 2L) {
    stop("compare_models() requires at least two models.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Human-readable model label for an mlumr fit or DIC object
#' @keywords internal
.mlumr_model_label <- function(object) {
  if (inherits(object, "mlumr_dic") && !is.null(object$model)) {
    return(as.character(object$model)[[1L]])
  }

  model <- object$model %||% NA_character_
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    return("ML-UMR")
  }

  switch(model,
    spfa = "SPFA",
    relaxed = "Relaxed SPFA",
    model
  )
}


#' Use user-supplied comparison names when available
#' @keywords internal
.comparison_names <- function(models, fallback) {
  user_names <- names(models)
  if (is.null(user_names)) {
    return(fallback)
  }
  use_user_name <- nzchar(user_names)
  out <- fallback
  out[use_user_name] <- user_names[use_user_name]
  out
}


#' Check MCMC diagnostics and warn if issues found
#' @param fit An `mlumr_fit` object
#' @keywords internal
check_diagnostics <- function(fit) {

  .validate_mlumr_fit_object(fit)
  diag <- fit$diagnostics %||% list()
  sampling_args <- fit$sampling_args %||% list()

  n_divergent <- .diagnostic_count(diag$n_divergent)
  if (n_divergent > 0) {
    warning(sprintf(
      "%d divergent transitions detected. Consider increasing adapt_delta (currently %s).",
      n_divergent, .diagnostic_value(sampling_args$adapt_delta)
    ), call. = FALSE)
  }

  n_max_treedepth <- .diagnostic_count(diag$n_max_treedepth)
  if (n_max_treedepth > 0) {
    warning(sprintf(
      "%d iterations hit max treedepth. Consider increasing max_treedepth (currently %s).",
      n_max_treedepth, .diagnostic_value(sampling_args$max_treedepth)
    ), call. = FALSE)
  }

  rhat_vals <- .finite_numeric_values(fit$summary$Rhat)
  if (length(rhat_vals) > 0L) {
    max_rhat <- max(rhat_vals)
    if (max_rhat > 1.05) {
      warning(sprintf(
        "Some Rhat values > 1.05 (max = %.3f). Chains have likely NOT converged.",
        max_rhat
      ), call. = FALSE)
    } else if (max_rhat > 1.01) {
      warning(sprintf(
        "Some Rhat values > 1.01 (max = %.3f). Chains may not have fully converged.",
        max_rhat
      ), call. = FALSE)
    }
  }

  ess_vals <- .finite_numeric_values(fit$summary$n_eff)
  if (length(ess_vals) > 0L) {
    min_ess <- min(ess_vals)
    if (min_ess < 400) {
      warning(sprintf(
        "Some ESS values < 400 (min = %.1f). Consider running more iterations.",
        min_ess
      ), call. = FALSE)
    }
  }

  invisible(NULL)
}


#' Read a non-negative scalar diagnostic count
#' @keywords internal
.diagnostic_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    return(0)
  }
  as.integer(x)
}


#' Format a diagnostic setting for warning messages
#' @keywords internal
.diagnostic_value <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return("unknown")
  }
  as.character(x)
}


#' Return finite numeric values from a summary column
#' @keywords internal
.finite_numeric_values <- function(x) {
  if (!is.numeric(x)) {
    return(numeric())
  }
  x[is.finite(x)]
}
