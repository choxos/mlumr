#' Refuse a pointwise log-likelihood collapsed over tied aggregate rows
#'
#' Tie aggregation (not shipped yet) would keep one `log_lik_agd` column per
#' distinct row with the multiplicity in `stan_data$agd_count`. LOO, WAIC and
#' DIC need one column per observation, so that shape is refused here.
#' @keywords internal
.assert_agd_loglik_per_observation <- function(object) {
  cnt <- object$stan_data$agd_count
  if (is.null(cnt) || !length(cnt)) return(invisible(TRUE))
  # Validate the multiplicities before summing them.
  if (!is.numeric(cnt) || anyNA(cnt) || !all(is.finite(cnt)) || any(cnt < 1) ||
        any(cnt != trunc(cnt))) {
    stop("`stan_data$agd_count` must hold finite whole-number multiplicities of ",
         "at least one, since each is a count of observations that one retained ",
         "AgD row stands for.", call. = FALSE)
  }
  if (all(cnt == 1)) return(invisible(TRUE))
  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) return(invisible(TRUE))
  n_cols <- length(.ordered_log_lik_columns(draws, "agd"))
  # Expanded means exactly one column per observation.
  if (n_cols == sum(cnt)) return(invisible(TRUE))
  if (n_cols != length(cnt)) {
    stop("`log_lik_agd` has ", n_cols, " column(s), which is neither one per ",
         "retained AgD row (", length(cnt), ") nor one per observation (",
         sum(cnt), "). LOO, WAIC, and DIC cannot align the pointwise ",
         "likelihood with the arm map in that state.", call. = FALSE)
  }
  stop("This fit collapsed tied aggregate rows (`stan_data$agd_count` has ",
       "multiplicities above one), so `log_lik_agd` holds one value per ",
       "UNIQUE row rather than one per observation. LOO, WAIC, and DIC would ",
       "count each tied observation once instead of ", max(cnt), " times, and ",
       "arm grouping would use the collapsed arm map. Expand the pointwise ",
       "log-likelihood and the arm map back to the original observations ",
       "before requesting diagnostics.", call. = FALSE)
}


#' Extract the full pointwise log-likelihood matrix from an mlumr_fit
#'
#' Combines the IPD and AgD per-observation log-likelihood draws into a
#' single matrix with one row per posterior draw and one column per
#' observation. The result is suitable for direct use with
#' [loo::loo()] / [loo::waic()].
#'
#' @param object An `mlumr_fit` object.
#' @return A numeric matrix of dimension `n_draws x (n_ipd + n_agd)`. IPD
#'   columns come first, then the AgD columns. The AgD pointwise unit is whatever
#'   the family's Stan model emits as `log_lik_agd`: for binomial / normal /
#'   poisson this is **one column per aggregate row**; for **survival** it is
#'   **one column per reconstructed pseudo-individual** (not per aggregate row),
#'   so survival LOO/WAIC operate at the pseudo-individual level. See the notes
#'   on [calculate_loo()] / [calculate_waic()].
#' @keywords internal
extract_log_lik <- function(object) {
  .validate_mlumr_fit_object(object)
  .assert_agd_loglik_per_observation(object)

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
      "an older version of mlumr; refit with the current version.",
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
#' @return A list of class `mlumr_dic` with components `DIC`, `pD`, `D_bar`,
#'   `n_obs` and `model`.
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
  # Variance-based pD (Gelman et al. 2004), stable for multimodal posteriors.
  pD <- 0.5 * var(D)
  DIC <- D_bar + pD

  out <- list(
    DIC = DIC,
    pD = pD,
    D_bar = D_bar,
    n_obs = ncol(log_lik),
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
#' Typical remedies are running more iterations or, for highly influential
#' AgD rows, refitting without the offending observation to check
#' sensitivity. Moment matching ([loo::loo_moment_match()]) needs the fitted
#' model rather than a log-likelihood matrix, so `moment_match` is refused.
#'
#' @note
#' **AgD rows are treated as independent observations.** Subgroup rows from
#' one study share no clustering term, so their effective sample sizes are
#' inflated and Pareto-k warnings understated; corroborate with
#' [prior_sensitivity()] or by refitting without suspect rows.
#'
#' **Survival fits.** The comparator enters as reconstructed pseudo-IPD, so
#' the default pointwise unit is one pseudo-individual and the criteria are
#' optimistic relative to leaving out the comparator arm. Set
#' `survival_unit = "arm"` or `"aggregate"` to hold out whole comparator
#' arms or all of the external evidence instead.
#'
#' @param object An `mlumr_fit` object.
#' @param survival_unit For survival fits, the LOO/WAIC pointwise unit:
#'   `"observation"` (default; per reconstructed comparator pseudo-individual,
#'   optimistic), `"arm"` (group the comparator pseudo-IPD by comparator arm, so
#'   each external arm is one held-out unit), or `"aggregate"` (all comparator
#'   pseudo-IPD as a single external-evidence unit). The index IPD always stays
#'   per-individual. Ignored for non-survival families.
#' @param ... Further arguments passed to [loo::loo()]; `r_eff` is computed
#'   from the fit's chains.
#'
#' @return An object of class `psis_loo` (see [loo::loo()]).
#' @export
#' @examples
#' \dontrun{
#' loo_spfa <- calculate_loo(fit_spfa)
#' print(loo_spfa)
#' }
calculate_loo <- function(object,
                          survival_unit = c("observation", "arm", "aggregate"),
                          ...) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for calculate_loo(). ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  if ("moment_match" %in% names(list(...))) {
    stop("`moment_match` is not available: `loo` ignores it for a ",
         "log-likelihood matrix, and moment matching needs the fitted model.",
         call. = FALSE)
  }
  survival_unit <- match.arg(survival_unit)
  log_lik <- .survival_log_lik_by_unit(object, survival_unit)
  r_eff <- .relative_eff_from_log_lik(log_lik, .chain_id(object))
  loo::loo(log_lik, r_eff = r_eff, ...)
}


#' Warn that survival LOO/WAIC pointwise units are reconstructed pseudo-IPD
#'
#' Once per session; suppress with `options(mlumr.quiet_survival_loo = TRUE)`.
#' @keywords internal
.warn_survival_loo_unit <- function(object) {
  if (!identical(object$family, "survival")) return(invisible())
  if (isTRUE(getOption("mlumr.quiet_survival_loo", FALSE))) return(invisible())
  if (isTRUE(getOption("mlumr.survival_loo_warned", FALSE))) return(invisible())
  warning(
    "Survival LOO/WAIC: the comparator AgD enters as reconstructed pseudo-IPD, ",
    "so each AgD pointwise unit is one pseudo-individual and the criteria are ",
    "optimistic relative to leaving out the comparator arm. Treat them as a ",
    "rough check. Suppress with `options(mlumr.quiet_survival_loo = TRUE)`.",
    call. = FALSE
  )
  options(mlumr.survival_loo_warned = TRUE)
  invisible()
}


#' Pointwise log-likelihood for LOO/WAIC, optionally grouped for survival
#'
#' `"observation"` is `extract_log_lik()`. For survival fits `"arm"` sums the
#' comparator pseudo-IPD columns within each arm and `"aggregate"` sums them
#' all, so leaving out a unit leaves out that arm or all external evidence.
#' The index IPD stays per individual.
#' @keywords internal
.survival_log_lik_by_unit <- function(object, survival_unit = "observation") {
  .assert_agd_loglik_per_observation(object)
  if (!identical(object$family, "survival") ||
        identical(survival_unit, "observation")) {
    .warn_survival_loo_unit(object)
    return(extract_log_lik(object))
  }

  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) {
    stop("`object$draws` must contain named posterior draw columns.",
         call. = FALSE)
  }
  ipd_cols <- .ordered_log_lik_columns(draws, "ipd")
  agd_cols <- .ordered_log_lik_columns(draws, "agd")
  if (length(agd_cols) == 0L) {
    stop("Grouped survival LOO/WAIC needs AgD pointwise log-likelihood ",
         "columns (`log_lik_agd`).", call. = FALSE)
  }
  agd_mat <- as.matrix(draws[, agd_cols, drop = FALSE])

  groups <- if (identical(survival_unit, "aggregate")) {
    rep(1L, ncol(agd_mat))
  } else {
    g <- object$stan_data$agd_arm
    if (is.null(g) || length(g) != ncol(agd_mat)) {
      stop("Cannot group survival AgD by arm: the per-pseudo-individual arm ",
           "map (`stan_data$agd_arm`) is missing or the wrong length.",
           call. = FALSE)
    }
    as.integer(g)
  }

  # One column per group: the log predictive density of the whole unit.
  grouped <- vapply(sort(unique(groups)), function(gg) {
    rowSums(agd_mat[, groups == gg, drop = FALSE])
  }, numeric(nrow(agd_mat)))
  grouped <- matrix(grouped, nrow = nrow(agd_mat))

  ipd_mat <- if (length(ipd_cols) > 0L) {
    as.matrix(draws[, ipd_cols, drop = FALSE])
  } else {
    NULL
  }
  out <- if (is.null(ipd_mat)) grouped else cbind(ipd_mat, grouped)
  .validate_log_lik_matrix(out)
  out
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
#' that share a study (see the note on `calculate_loo()`). For
#' **survival** fits the AgD pointwise unit is a reconstructed
#' pseudo-individual, so WAIC is at the pseudo-individual level
#' (see the survival note on [calculate_loo()]).
#'
#' @param object An `mlumr_fit` object.
#' @param survival_unit For survival fits, the WAIC pointwise unit:
#'   `"observation"` (default), `"arm"`, or `"aggregate"` (see [calculate_loo()]
#'   for details). Ignored for non-survival families.
#' @param ... Further arguments passed to [loo::waic()].
#'
#' @return An object of class `waic` (see [loo::waic()]).
#' @export
#' @examples
#' \dontrun{
#' waic_spfa <- calculate_waic(fit_spfa)
#' }
calculate_waic <- function(object,
                           survival_unit = c("observation", "arm", "aggregate"),
                           ...) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for calculate_waic(). ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  survival_unit <- match.arg(survival_unit)
  log_lik <- .survival_log_lik_by_unit(object, survival_unit)
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
#' @param survival_unit For survival fits compared by `"loo"`/`"waic"`, the
#'   pointwise unit forwarded to [calculate_loo()] / [calculate_waic()]:
#'   `"observation"` (default; per reconstructed comparator pseudo-individual,
#'   optimistic), `"arm"`, or `"aggregate"`. Choose `"arm"` or `"aggregate"` to
#'   select on whole-external-arm predictive fit. Ignored for non-survival
#'   families and for `criterion = "dic"`.
#'
#' @return For `"loo"` / `"waic"`: a `compare.loo` table from
#'   [loo::loo_compare()]. For `"dic"`: a data frame (invisibly) with
#'   columns `Model`, `DIC`, `pD`, `Delta_DIC`.
#' @export
compare_models <- function(..., criterion = c("dic", "loo", "waic"),
                           survival_unit = c("observation", "arm", "aggregate")) {

  criterion <- .validate_choice(criterion, c("dic", "loo", "waic"),
                                "criterion")
  survival_unit <- match.arg(survival_unit)
  models <- list(...)
  if (length(models) < 2L) {
    stop("compare_models() requires at least two models.", call. = FALSE)
  }

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
    # DIC is only comparable across fits of one observation set.
    n_obs_vals <- vapply(dics, function(d) d$n_obs %||% NA_integer_, integer(1))
    if (length(unique(stats::na.omit(n_obs_vals))) > 1L) {
      msg <- paste0(
        "Comparing DIC across fits with different observation counts (%s). DIC ",
        "is only comparable on a common data set; this ranking is not meaningful."
      )
      warning(sprintf(msg, paste(n_obs_vals, collapse = ", ")), call. = FALSE)
    }
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
  ic_list <- lapply(models, function(m) calc_fn(m, survival_unit = survival_unit))
  names(ic_list) <- .comparison_names(
    models,
    vapply(models, .mlumr_model_label, character(1))
  )

  cat(sprintf("\nModel Comparison (%s)\n", toupper(criterion)))
  cat(strrep("=", 22), "\n\n", sep = "")
  cmp <- loo::loo_compare(ic_list)
  print(cmp)
  cat(.model_comparison_interpretation(), sep = "\n")
  cat("\n")
  invisible(cmp)
}


# Chain ids per draw: the backend's labels when stored, else reconstructed
# from the chain-major layout, else a single chain (which inflates r_eff).
.chain_id <- function(object) {
  n_draws <- nrow(object$draws)
  if (!is.numeric(n_draws) || length(n_draws) != 1L || n_draws < 1L) {
    return(integer())
  }
  # Authoritative path: real chain labels captured at fit time.
  stored <- object$chain_ids
  if (!is.null(stored) && is.numeric(stored) && length(stored) == n_draws &&
        all(is.finite(stored))) {
    return(as.integer(stored))
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
  out <- fallback
  if (!is.null(user_names)) {
    use_user_name <- nzchar(user_names)
    out[use_user_name] <- user_names[use_user_name]
  }
  # Two unnamed fits of one model type would otherwise share a label.
  make.unique(out, sep = " #")
}


#' Check MCMC diagnostics and warn if issues found
#' @param fit An `mlumr_fit` object
#' @keywords internal
check_diagnostics <- function(fit) {

  .validate_mlumr_fit_object(fit)
  diag <- fit$diagnostics %||% list()
  sampling_args <- fit$sampling_args %||% list()

  # A chain that terminated abnormally is dropped by both backends, so the
  # fit may rest on fewer chains than requested. Report that first.
  n_req <- .diagnostic_count(diag$n_chains_requested)
  n_got <- .diagnostic_count(diag$n_chains_returned)
  # `NA` from the backend means the draws could not be labeled by chain.
  if (n_req > 0 && !isTRUE(n_got > 0) &&
        (is.null(diag$n_chains_returned) ||
           any(is.na(diag$n_chains_returned)))) {
    warning(paste0(
      "The draws could not be labeled by chain, so it is not known whether ",
      "all ", n_req, " requested chain(s) returned. Rhat and the effective ",
      "sample sizes below are computed from the draws as stored. Refit, or ",
      "inspect the backend fit object, before reporting these results."
    ), call. = FALSE)
  }
  if (n_req > 0 && n_got > 0 && n_got < n_req) {
    warning(sprintf(
      paste0("Only %d of %d requested chain(s) returned; the remaining chain(s) ",
             "terminated abnormally. The posterior, Rhat, and every effect ",
             "estimate below are based on the surviving chain(s) only, and Rhat ",
             "is not meaningful from a single chain. Do not report these results ",
             "without refitting successfully."),
      n_got, n_req
    ), call. = FALSE)
  }

  # A count the backend did not supply is unknown, not zero.
  n_divergent <- .transition_count(diag$n_divergent)
  if (is.na(n_divergent)) {
    warning("The number of divergent transitions is not available for this ",
            "fit, so it was not checked. A fit with divergences is not ",
            "distinguishable from a clean one here; inspect the backend fit ",
            "object before reporting these results.", call. = FALSE)
  } else if (n_divergent > 0) {
    warning(sprintf(
      "%d divergent transitions detected. Consider increasing adapt_delta (currently %s).",
      n_divergent, .diagnostic_value(sampling_args$adapt_delta)
    ), call. = FALSE)
  }

  n_max_treedepth <- .transition_count(diag$n_max_treedepth)
  if (is.na(n_max_treedepth)) {
    warning("The number of iterations that hit maximum treedepth is not ",
            "available for this fit, so it was not checked.", call. = FALSE)
  } else if (n_max_treedepth > 0) {
    warning(sprintf(
      "%d iterations hit max treedepth. Consider increasing max_treedepth (currently %s).",
      n_max_treedepth, .diagnostic_value(sampling_args$max_treedepth)
    ), call. = FALSE)
  }

  # Keep an infinite Rhat (chains that did not mix); count a missing one
  # rather than dropping it from the maximum.
  n_par <- nrow(fit$summary)
  rhat <- .usable_diagnostic_values(fit$summary$Rhat, n_par)
  .report_missing_diagnostics(rhat, "Rhat", "convergence",
                              fit$summary$variable)
  if (length(rhat$values) > 0L) {
    max_rhat <- max(rhat$values)
    if (max_rhat > 1.05) {
      warning(sprintf(
        "Some Rhat values > 1.05 (max = %s). Chains have likely not converged.",
        .format_diagnostic(max_rhat)
      ), call. = FALSE)
    } else if (max_rhat > 1.01) {
      warning(sprintf(
        "Some Rhat values > 1.01 (max = %s). Chains may not have fully converged.",
        .format_diagnostic(max_rhat)
      ), call. = FALSE)
    }
  }

  ess <- .usable_diagnostic_values(fit$summary$n_eff, n_par)
  .report_missing_diagnostics(ess, "Bulk ESS", "effective sample size",
                              fit$summary$variable)
  if (length(ess$values) > 0L) {
    min_ess <- min(ess$values)
    if (min_ess < 400) {
      warning(sprintf(
        "Some ESS values < 400 (min = %s). Consider running more iterations.",
        .format_diagnostic(min_ess)
      ), call. = FALSE)
    }
  }

  # Tail ESS (Vehtari et al. 2021) guards the reported tail quantiles. An
  # absent or partly missing column is reported, not passed.
  tail_all <- if ("ess_tail" %in% names(fit$summary)) {
    fit$summary$ess_tail
  } else {
    numeric(0)
  }
  tail_vals <- .finite_numeric_values(tail_all)
  n_missing <- length(tail_all) - length(tail_vals)
  if (length(tail_vals) == 0L) {
    hint <- if (!requireNamespace("posterior", quietly = TRUE)) {
      " Install the 'posterior' package to enable this check."
    } else {
      ""
    }
    message("Tail ESS is unavailable for this fit, so tail quantiles were not ",
            "checked.", hint)
  } else {
    if (n_missing > 0L) {
      message(sprintf(
        paste0("Tail ESS is unavailable for %d of %d parameter(s), which were ",
               "not checked; the remaining %d were."),
        n_missing, length(tail_all), length(tail_vals)
      ))
    }
    if (min(tail_vals) < 400) {
      msg <- paste0(
        "Some tail-ESS values < 400 (min = %.1f). Tail quantiles ",
        "(e.g. 2.5%%/97.5%%) may be unreliable; run more iterations."
      )
      warning(sprintf(msg, min(tail_vals)), call. = FALSE)
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


#' Split a diagnostic column into usable values and missing ones
#'
#' Keeps an infinite value, which is a diagnostic that came out as bad as it
#' can, and counts a missing one rather than dropping it. An absent or
#' non-numeric column counts as `n_expected` missing diagnostics.
#'
#' @param x A summary column, possibly `NULL`.
#' @param n_expected How many parameters should have had a diagnostic.
#' @return A list with `values`, `n_missing`, `n_total` and `missing_idx`.
#' @keywords internal
.usable_diagnostic_values <- function(x, n_expected = length(x)) {
  if (!is.numeric(x)) {
    n <- as.integer(max(n_expected, length(x)))
    return(list(values = numeric(), n_missing = n, n_total = n,
                missing_idx = seq_len(n)))
  }
  keep <- !is.na(x)
  list(values = x[keep],
       n_missing = sum(!keep),
       n_total = length(x),
       missing_idx = which(!keep))
}


#' Say how many parameters had no diagnostic, rather than dropping them
#'
#' Names the outcome, not a cause: a missing diagnostic is as consistent with
#' a constant quantity as with stuck chains or non-finite draws.
#'
#' @param d A [.usable_diagnostic_values()] result.
#' @param label Diagnostic name for the message.
#' @param what What the diagnostic measures, for the message.
#' @param variables Parameter names in the same order as the column, or `NULL`.
#' @return `NULL`, invisibly.
#' @keywords internal
.report_missing_diagnostics <- function(d, label, what, variables = NULL) {
  if (d$n_missing > 0L && d$n_total > 0L) {
    named <- ""
    if (!is.null(variables) && length(variables) == d$n_total) {
      missing_names <- utils::head(as.character(variables)[d$missing_idx], 6L)
      if (length(missing_names)) {
        named <- paste0(" Unavailable for: ",
                        paste(missing_names, collapse = ", "),
                        if (d$n_missing > length(missing_names)) {
                          sprintf(" and %d more.", d$n_missing -
                                    length(missing_names))
                        } else {
                          "."
                        })
      }
    }
    message(sprintf(
      paste0("%s is unavailable for %d of %d parameter(s), which were not ",
             "checked for %s; the remaining %d were.%s A diagnostic that ",
             "could not be computed is legitimately absent for a quantity ",
             "that is constant across every draw, and equally absent when ",
             "the chains are stuck or the draws are not finite."),
      label, d$n_missing, d$n_total, what, d$n_total - d$n_missing, named
    ))
  }
  invisible(NULL)
}


#' Format a diagnostic for a message without turning Inf into a number
#'
#' Four significant digits, with a non-finite value printed by name.
#'
#' @param x A single numeric value.
#' @return A single string.
#' @keywords internal
.format_diagnostic <- function(x) {
  if (!is.finite(x)) {
    return(as.character(x))
  }
  format(x, digits = 4L)
}


#' Print an unknown count as unknown
#'
#' @param n A count, possibly `NA`.
#' @return A single string.
#' @keywords internal
.diagnostic_display <- function(n) {
  if (length(n) != 1L || is.na(n)) {
    return("unknown")
  }
  as.character(n)
}


#' Note, inline, that a printed statistic was computed without some parameters
#'
#' @param d A [.usable_diagnostic_values()] result.
#' @return A single string, empty when nothing was missing.
#' @keywords internal
.missing_suffix <- function(d) {
  if (d$n_missing > 0L && d$n_total > 0L) {
    return(sprintf("(over %d of %d parameters; %d unavailable)",
                   d$n_total - d$n_missing, d$n_total, d$n_missing))
  }
  ""
}


#' Read a transition count that the backend may not have supplied
#'
#' Zero is the count that says the sampler behaved, so an unreported,
#' fractional or out-of-range value is `NA` rather than 0.
#'
#' @param x The recorded count.
#' @return A non-negative integer, or `NA_integer_` when unknown.
#' @keywords internal
.transition_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 ||
        x != trunc(x) || x > .Machine$integer.max) {
    return(NA_integer_)
  }
  as.integer(x)
}


#' The interpretation paragraph the LOO/WAIC comparison prints
#'
#' Callable so a test can assert what users see. The standard error of a
#' difference measures uncertainty about it, not support for it.
#'
#' @return A character vector, one element per printed line.
#' @keywords internal
.model_comparison_interpretation <- function() {
  c("",
    "elpd_diff is the difference in expected log pointwise predictive",
    "density vs the best model, and se_diff is its standard error: the",
    "uncertainty about that difference, not evidence for it. Read the two",
    "together. A difference small relative to se_diff is not distinguished",
    "from zero by this comparison, whatever se_diff itself is.",
    "Treat any ratio as a heuristic, not a decision rule, and check the",
    "PSIS diagnostics and whether the difference matters for the",
    "prediction you care about.")
}
