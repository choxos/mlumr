#' Prior sensitivity analysis for an ML-UMR fit
#'
#' Refit an [mlumr()] model across a grid of `prior_beta` scales (keeping the
#' family, mean, and df fixed) and summarize how the posterior for the
#' marginal treatment effects (`delta_index`, `delta_comparator`) moves. This
#' is the workflow recommended by Vehtari et al.'s prior-choice wiki for
#' judging how much of the posterior is driven by the data versus the prior.
#'
#' @param fit A fitted `mlumr_fit` object to re-fit under alternative priors.
#' @param prior_beta_scales Numeric vector of scales for `prior_beta`.
#'   Default `c(0.5, 1, 2.5, 5, 10)`.
#' @param probs Quantiles for summarizing each posterior
#'   (default `c(0.025, 0.5, 0.975)`).
#' @param verbose Logical; if `FALSE`, suppresses progress messages and final
#'   printed summary table.
#' @param ... Additional arguments forwarded to [mlumr()] on each refit
#'   (e.g. `chains`, `iter`, `refresh`). Sampling defaults otherwise inherit
#'   from the original fit.
#'
#' @details
#' Only the scale of the `prior_beta` family is varied; its distribution
#' (normal / student_t) and mean are preserved so comparisons are apples to
#' apples. `prior_intercept` and `prior_sigma` are carried through
#' unchanged from the original fit. Each value in `prior_beta_scales` is
#' used as the **absolute** scale for every coefficient at that refit --- if
#' the original fit used per-coefficient priors, all coefficients are set
#' to the same scale (the sweep is deliberately homogeneous so the grid
#' reflects a single level of prior informativeness per refit, not a
#' rescaling of existing relative differences). If the original
#' `prior_beta` used an exponential family, it is swapped for a
#' `prior_normal(0, scale)` at each grid point since exponential has no
#' scale parameter to vary.
#'
#' @return A data frame (tibble-style) with one row per
#'   (scale, population, quantile) combination and columns `scale`,
#'   `parameter`, `mean`, `sd`, and the requested quantiles. Side effect:
#'   prints a summary table at the end.
#' @seealso [prior_summary()] for a one-shot description of the priors on
#'   a fit; [marginal_effects()] for the posterior summary quantities this
#'   sweep tracks.
#' @export
#' @examples
#' \dontrun{
#' sens <- prior_sensitivity(fit_spfa, prior_beta_scales = c(1, 2.5, 5))
#' }
prior_sensitivity <- function(fit,
                              prior_beta_scales = c(0.5, 1, 2.5, 5, 10),
                              probs = c(0.025, 0.5, 0.975),
                              verbose = TRUE,
                              ...) {

  if (!inherits(fit, "mlumr_fit")) {
    stop("`fit` must be an mlumr_fit object", call. = FALSE)
  }
  invalid_prior_scales <- !is.numeric(prior_beta_scales) ||
    any(!is.finite(prior_beta_scales)) ||
    any(prior_beta_scales <= 0)
  if (invalid_prior_scales) {
    stop("`prior_beta_scales` must be positive finite numbers", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  # Extract the base prior so we can reconstruct scaled variants.
  base_beta <- fit$priors$beta
  if (is.null(base_beta)) {
    stop("Original `prior_beta` not found on the fit. ",
         "Was the model fitted with an older version of mlumr?", call. = FALSE)
  }

  # Inherit the original sampling args unless overridden by ...
  sa <- fit$sampling_args %||% list()

  # Re-use the original data object (has_integration is already TRUE)
  data <- fit$data

  results <- vector("list", length(prior_beta_scales))

  for (i in seq_along(prior_beta_scales)) {
    s <- prior_beta_scales[[i]]
    prior_beta_i <- .rescale_prior_beta(base_beta, s)

    mlumr_message(sprintf("Prior sensitivity: refit %d/%d with scale %g",
                          i, length(prior_beta_scales), s),
                  verbose = verbose)

    fit_i <- mlumr(
      data = data,
      model = fit$model,
      link = fit$link,
      prior_intercept = fit$priors$intercept,
      prior_beta = prior_beta_i,
      prior_sigma = fit$priors$sigma %||% default_prior_sigma(),
      chains       = sa$chains       %||% 4,
      iter         = sa$iter         %||% 2000,
      warmup       = sa$warmup       %||% 1000,
      seed         = sa$seed,
      adapt_delta  = sa$adapt_delta  %||% 0.95,
      max_treedepth = sa$max_treedepth %||% 15,
      refresh = 0,
      engine = fit$engine,
      verbose = verbose,
      ...
    )

    results[[i]] <- .summarize_sensitivity(fit_i, scale = s, probs = probs)
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL

  if (verbose) {
    cat("\nPrior sensitivity: posterior of marginal treatment effects\n")
    cat("=========================================================\n\n")
    print(out, row.names = FALSE)
    cat("\nInterpretation: if posterior summaries are approximately constant\n")
    cat("across scales, the inference is data-driven rather than prior-driven.\n")
  }

  invisible(out)
}


# ---- Helpers ---------------------------------------------------------------

#' Rescale a prior_beta's scale, preserving family / mean / df.
#' @keywords internal
.rescale_prior_beta <- function(prior, new_scale) {
  if (is_single_prior(prior)) {
    # Replace the scalar sd field. For exponential (not supported on beta
    # but defensively) fall back to a normal(0, new_scale).
    if (prior$distribution == "exponential") {
      return(prior_normal(mean = 0, sd = new_scale))
    }
    prior$sd <- new_scale
    # Strip default/version tags since this is a user-generated variant.
    prior$default <- NULL
    prior$version <- NULL
    return(prior)
  }
  # Per-coefficient list: rescale each element to new_scale (absolute, not
  # ratio — we want a homogeneous sensitivity sweep).
  lapply(prior, function(p) {
    p$sd <- new_scale
    p$default <- NULL
    p$version <- NULL
    p
  })
}

#' Summarize a sensitivity refit.
#' @keywords internal
.summarize_sensitivity <- function(fit, scale, probs) {
  draws <- fit$draws
  family <- fit$family %||% "binomial"

  # Family-specific marginal-effect column names in the draws data frame
  effect_cols <- switch(family,
    binomial = c("lor_index",   "lor_comparator"),
    normal   = c("delta_index", "delta_comparator"),
    poisson  = c("lrr_index",   "lrr_comparator")
  )

  effect_names <- intersect(effect_cols, colnames(draws))
  if (length(effect_names) == 0L) return(NULL)

  qnames <- paste0("q", round(100 * probs))

  rows <- lapply(effect_names, function(nm) {
    x <- draws[, nm]
    qs <- stats::quantile(x, probs = probs, names = FALSE)
    row <- data.frame(
      scale = scale,
      parameter = nm,
      mean = mean(x),
      sd = stats::sd(x),
      stringsAsFactors = FALSE
    )
    for (j in seq_along(probs)) row[[qnames[[j]]]] <- qs[[j]]
    row
  })
  do.call(rbind, rows)
}
