#' Prior sensitivity analysis for an ML-UMR fit
#'
#' Refit an [mlumr()] model across a grid of `prior_beta` scales (keeping the
#' family, mean, and df fixed) and summarize how the posterior for the
#' marginal treatment effects (`delta_index`, `delta_comparator`) moves. This
#' is the workflow recommended by Vehtari et al.'s prior-choice wiki for
#' judging how much of the posterior is driven by the data versus the prior.
#'
#' The design-matrix controls (`center`, `qr`) are taken from the original fit
#' and replayed, so a refit reproduces the original parameterization instead of
#' reverting to the defaults. A fit made with `center = FALSE` or `qr = TRUE` is
#' a different parameterization, and replaying the defaults would vary the model
#' as well as the prior.
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
  # `...` is forwarded to each refit, so it must not carry anything that defines
  # the model. Sampler and backend controls (chains, iter, adapt_delta, engine)
  # are still allowed through.
  dots <- list(...)
  if (length(dots)) {
    if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
      stop("All `...` arguments to prior_sensitivity() must be named.",
           call. = FALSE)
    }
    # Everything that defines the model rather than the sampler. Overriding any
    # of these would vary a second factor alongside the prior scale, so the
    # movement in the output could no longer be attributed to the prior.
    protected <- c("data", "model", "link", "distribution", "prior_beta",
                   "prior_intercept", "prior_sigma", "prior_aux",
                   "prior_smooth", "center", "qr", "n_knots", "knots",
                   "mspline_degree", "pred_times", "rmst_horizon",
                   "n_rmst_grid", "aux_by")
    clash <- intersect(names(dots), protected)
    if (length(clash)) {
      msg <- paste0(
        "`...` cannot override the scenario-defining argument(s): %s. ",
        "prior_sensitivity() sweeps `prior_beta` itself and reuses every other ",
        "setting from the original fit so the sweep varies one factor only; ",
        "pass only sampler or backend controls."
      )
      stop(sprintf(msg, paste(clash, collapse = ", ")), call. = FALSE)
    }
  }

  sa <- fit$sampling_args %||% list()
  # Design-matrix controls. A fit made with `center = FALSE` or `qr = TRUE` is a
  # different parameterization, so replaying the defaults here would vary the
  # model as well as the prior and the sweep would no longer isolate one factor.
  # Fits from earlier versions do not record these; the defaults are what they used.
  mc <- fit$model_controls %||% list()
  # Survival baseline controls needed to reproduce the original baseline; NULL
  # or absent for the other families. `surv_controls` records a control that
  # does not apply as NA rather than NULL (a Weibull fit stores
  # mspline_degree = NA_integer_), and mlumr() rejects NA where it accepts NULL,
  # so NA is normalized back to NULL before the refit.
  sc <- fit$surv_controls %||% list()
  .na_to_null <- function(x) if (length(x) == 1L && is.na(x)) NULL else x

  # Re-use the original data object (has_integration is already TRUE)
  data <- fit$data

  results <- vector("list", length(prior_beta_scales))

  for (i in seq_along(prior_beta_scales)) {
    s <- prior_beta_scales[[i]]
    prior_beta_i <- .rescale_prior_beta(base_beta, s)

    mlumr_message(sprintf("Prior sensitivity: refit %d/%d with scale %g",
                          i, length(prior_beta_scales), s),
                  verbose = verbose)

    args <- list(
      data = data,
      model = fit$model,
      link = fit$link,
      prior_intercept = fit$priors$intercept,
      prior_beta = prior_beta_i,
      prior_sigma = fit$priors$sigma %||% default_prior_sigma(),
      # A fit that predates these controls was fitted on the raw scale, so the
      # historical behavior, not the current default, is what reproduces it.
      center       = mc$center       %||% FALSE,
      qr           = mc$qr           %||% FALSE,
      chains       = sa$chains       %||% 4,
      iter         = sa$iter         %||% 2000,
      warmup       = sa$warmup       %||% 1000,
      seed         = sa$seed,
      adapt_delta  = sa$adapt_delta  %||% 0.95,
      max_treedepth = sa$max_treedepth %||% 15,
      refresh = 0,
      engine = fit$engine,
      verbose = verbose
    )

    # The survival controls are only legal arguments for a survival fit, and
    # mlumr() decides that with missing(), not by testing for NULL. Naming
    # `aux_by` at all makes it non-missing, so listing these unconditionally
    # would make prior_sensitivity() fail on every binomial, normal and Poisson
    # fit. Omit the whole group rather than passing NULL into it.
    if (identical(fit$family, "survival")) {
      args <- c(args, list(
        distribution = fit$distribution,
        prior_aux    = fit$priors$aux,
        prior_smooth = fit$priors$smooth,
        n_knots      = sc$n_knots      %||% 7L,
        knots        = sc$knots,
        mspline_degree = .na_to_null(sc$mspline_degree),
        aux_by       = sc$aux_by       %||% ".study",
        pred_times   = sc$pred_times,
        rmst_horizon = sc$rmst_horizon,
        n_rmst_grid  = sc$n_rmst_grid  %||% 100L
      ))
    }

    # `...` is documented as the way to pass sampler and backend controls, and
    # `protected` above already keeps it away from anything that defines the
    # scenario. Merge rather than concatenate: `args` already names `chains`,
    # `iter` and the rest, so appending a `...` value would match the same
    # formal twice and R refuses the call. That made the one thing `...` is for
    # the one thing it could not do.
    call_args <- args
    if (length(dots)) call_args[names(dots)] <- dots
    fit_i <- do.call(mlumr, call_args)

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
    poisson  = c("lrr_index",   "lrr_comparator"),
    # Log hazard ratio (PH) or log time ratio (AFT). Reported on the log scale,
    # like every other family here, so the scales stay comparable across rows.
    survival = c("delta_index", "delta_comparator")
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
