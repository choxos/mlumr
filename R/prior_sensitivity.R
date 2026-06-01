#' Prior sensitivity analysis for an ML-UMR fit
#'
#' Refit an [mlumr()] model across a grid of `prior_beta` scales (keeping the
#' family, mean, and df fixed) and summarize how the posterior for the
#' marginal treatment effects (`delta_index`, `delta_comparator`) moves. This
#' is a practical way to assess whether the reported posterior summary is
#' sensitive to reasonable prior scales. It is not a decomposition of the
#' posterior into separate data and prior fractions.
#'
#' @param fit A fitted `mlumr_fit` object to re-fit under alternative priors.
#' @param prior_beta_scales Numeric vector of scales for `prior_beta`.
#'   Default `c(0.5, 1, 2.5, 5, 10)`.
#' @param prior_beta_comparator_scales (Relaxed fits only.) Numeric vector of
#'   scales for `prior_beta_comparator`, same length as `prior_beta_scales` and
#'   paired with it elementwise. If `NULL` (default), the comparator prior is
#'   swept in parallel using `prior_beta_scales`. This matters because the
#'   relaxed index-population estimand is driven by the comparator coefficients,
#'   so a faithful sensitivity sweep must vary their prior. Whichever scale is
#'   used is reported in the `scale_comparator` column, so a refit that paired,
#'   say, an index scale of 0.5 with a comparator scale of 2.5 is not labeled
#'   as though only the index prior had been set. Ignored for SPFA fits.
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
#' unchanged from the original fit, as are the survival baseline controls
#' (`n_knots`, `mspline_degree`, `pred_times`, `rmst_horizon`, `n_rmst_grid`,
#' `aux_by`) so a survival refit reproduces the original baseline, and the
#' design-matrix controls (`center`, `qr`) so it reproduces the original
#' parameterization. The integration points, family, link, and engine come from
#' the fit itself. Nothing but the prior scale changes between refits, and
#' `...` may not override any of the settings above. For relaxed fits the
#' comparator prior is swept alongside the index prior (see
#' `prior_beta_comparator_scales`): by default the two move together, which is a
#' single-factor sweep of overall prior informativeness, but supplying
#' `prior_beta_comparator_scales` pairs a different comparator scale with each
#' index scale, and the two scales are then separate factors moved in lockstep
#' rather than one. Both are recorded per row (`scale` and `scale_comparator`),
#' so a row is never identified by only half of the prior it was fitted under.
#' Each
#' value in `prior_beta_scales` is used as the **absolute** scale for every
#' coefficient at that refit: if the original fit used per-coefficient
#' priors, all coefficients are set to the same scale (the sweep is
#' deliberately homogeneous so the grid reflects a single level of prior
#' informativeness per refit, not a rescaling of existing relative
#' differences). If the original `prior_beta` used an exponential family, it
#' is swapped for a `prior_normal(0, scale)` at each grid point since
#' exponential has no scale parameter to vary.
#'
#' @return A data frame (tibble-style) with one row per prior-scale scenario and
#'   marginal-effect parameter; the requested quantiles are columns, not rows.
#'   Columns are `scale`,
#'   `scale_comparator`, `parameter`, `effect`, `at_time`, `mean`, `sd`, and the
#'   requested quantiles. `scale` is the scale applied to `prior_beta` and
#'   `scale_comparator` the scale applied to `prior_beta_comparator` on that
#'   refit; the latter is present for every relaxed fit, including the default
#'   case where the two are equal, and is dropped for SPFA fits, which have no
#'   comparator coefficient prior. `effect` names what the summarized draws are and
#'   `at_time` the evaluation time where one applies, so a survival row can
#'   never be read as a generic log hazard ratio when it is a location contrast
#'   or is tied to one prediction time. Side effect: prints a summary table at
#'   the end.
#'
#'   The tracked `parameter` and its **scale depend on the outcome family**, so
#'   the numbers are comparable across `scale` rows within one call but not
#'   directly across families: binomial reports `lor_*` (marginal log odds
#'   ratio, null = 0); poisson reports `delta_*` (marginal rate ratio,
#'   null = 1); normal reports `delta_*` (marginal mean difference, null = 0);
#'   survival reports `delta_*` on the LOG scale (null = 0), which is one of
#'   three different quantities depending on the fit and is named per row in the
#'   `effect` column: `LOG_HR` (proportional hazards, with the evaluation time
#'   in `at_time`), `LOG_TR` (AFT with shared shapes under SPFA, a genuine log
#'   time ratio), or `DELTA_ETA` (AFT whose shapes differ, or any relaxed AFT,
#'   where the contrast is a location difference and not a time ratio). Read the
#'   `effect` and `at_time` columns, not just `parameter`, to know what a row
#'   means.
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
                              prior_beta_comparator_scales = NULL,
                              probs = c(0.025, 0.5, 0.975),
                              verbose = TRUE,
                              ...) {

  if (!inherits(fit, "mlumr_fit")) {
    stop("`fit` must be an mlumr_fit object", call. = FALSE)
  }
  invalid_prior_scales <- !is.numeric(prior_beta_scales) ||
    length(prior_beta_scales) < 1L ||
    any(!is.finite(prior_beta_scales)) ||
    any(prior_beta_scales <= 0)
  if (invalid_prior_scales) {
    stop("`prior_beta_scales` must be one or more positive finite numbers",
         call. = FALSE)
  }
  invalid_probs <- !is.numeric(probs) || length(probs) < 1L ||
    any(!is.finite(probs)) || any(probs < 0) || any(probs > 1)
  if (invalid_probs) {
    stop("`probs` must be one or more numbers in [0, 1]", call. = FALSE)
  }
  # `...` is forwarded to each refit and, being applied by name, would silently
  # override the swept prior or the model/data that define the scenario, making
  # the `scale` label meaningless. Require named dots and forbid the
  # scenario-defining arguments; sampler/backend controls (chains, iter, seed,
  # adapt_delta, engine, ...) are still allowed through.
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
                   "prior_beta_comparator", "prior_intercept", "prior_sigma",
                   "prior_aux", "prior_smooth", "center", "qr", "n_knots",
                   "knots", "mspline_degree", "pred_times", "rmst_horizon",
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
  if (!is.null(prior_beta_comparator_scales)) {
    invalid_cmp_scales <- !is.numeric(prior_beta_comparator_scales) ||
      any(!is.finite(prior_beta_comparator_scales)) ||
      any(prior_beta_comparator_scales <= 0) ||
      length(prior_beta_comparator_scales) != length(prior_beta_scales)
    if (invalid_cmp_scales) {
      stop("`prior_beta_comparator_scales` must be positive finite numbers ",
           "of the same length as `prior_beta_scales`.", call. = FALSE)
    }
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
  # For relaxed fits the index-population estimand is driven by the comparator
  # coefficients, so sweep their prior in parallel (defaulting to the same
  # scales) unless the user supplies separate comparator scales. SPFA has no
  # comparator coefficient prior.
  is_relaxed <- identical(fit$model, "relaxed")
  base_beta_cmp <- fit$priors$beta_comparator %||% base_beta
  cmp_scales <- if (is_relaxed) {
    prior_beta_comparator_scales %||% prior_beta_scales
  } else {
    NULL
  }

  # Inherit the original sampling args unless overridden by ...
  sa <- fit$sampling_args %||% list()
  # Survival controls needed to reproduce the original baseline (NULL/ignored
  # for non-survival families).
  sc <- fit$surv_controls %||% list()
  # Design-matrix controls. A fit made with `center = FALSE` or `qr = TRUE` is a
  # different parameterization, so replaying the defaults here would vary the
  # model as well as the prior and the sweep would no longer isolate one factor.
  # Fits from mlumr < 0.2.0 do not record these; the defaults are what they used.
  mc <- fit$model_controls %||% list()
  # `surv_controls` records a control that does not apply to the fitted baseline
  # as NA, not NULL: a Weibull fit stores mspline_degree = NA_integer_. mlumr()
  # treats NULL as "not applicable" but rejects NA ("must be a non-negative
  # integer"), and `%||%` only catches NULL, so the refit has to normalize NA
  # back to NULL or prior_sensitivity() fails on every parametric survival fit.
  .na_to_null <- function(x) if (length(x) == 1L && is.na(x)) NULL else x

  # Re-use the original data object (has_integration is already TRUE)
  data <- fit$data

  results <- vector("list", length(prior_beta_scales))

  for (i in seq_along(prior_beta_scales)) {
    s <- prior_beta_scales[[i]]
    prior_beta_i <- .rescale_prior_beta(base_beta, s)
    prior_beta_cmp_i <- if (is_relaxed) {
      .rescale_prior_beta(base_beta_cmp, cmp_scales[[i]])
    } else {
      NULL
    }

    s_cmp <- if (is_relaxed) cmp_scales[[i]] else NA_real_
    mlumr_message(
      if (is_relaxed) {
        sprintf("Prior sensitivity: refit %d/%d with scale %g (comparator %g)",
                i, length(prior_beta_scales), s, s_cmp)
      } else {
        sprintf("Prior sensitivity: refit %d/%d with scale %g",
                i, length(prior_beta_scales), s)
      },
      verbose = verbose)

    base_args <- list(
      data = data,
      model = fit$model,
      link = fit$link,
      prior_intercept = fit$priors$intercept,
      prior_beta = prior_beta_i,
      prior_beta_comparator = prior_beta_cmp_i,
      prior_sigma = fit$priors$sigma %||% default_prior_sigma(),
      distribution = fit$distribution,
      prior_aux = fit$priors$aux,
      prior_smooth = fit$priors$smooth,
      n_knots = .na_to_null(sc$n_knots) %||% 7L,
      mspline_degree = .na_to_null(sc$mspline_degree),
      pred_times = sc$pred_times,
      rmst_horizon = .na_to_null(sc$rmst_horizon),
      # Preserve the original survival RMST grid resolution so the refit
      # reproduces the baseline rather than silently reverting to the default.
      n_rmst_grid = .na_to_null(sc$n_rmst_grid) %||% 100L,
      center       = mc$center       %||% TRUE,
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
    # User-supplied `...` overrides the inherited sampler controls (e.g. chains,
    # iter, refresh) rather than colliding with the named arguments (which
    # previously raised a "matched by multiple actual arguments" error). `dots`
    # was captured and validated once above (named, no scenario-defining args).
    # `aux_by` is survival-only and mlumr() rejects it for every other family.
    # do.call() makes an argument non-missing even when its value is NULL, so it
    # has to be omitted rather than passed as NULL. Supplying it for survival
    # keeps the sweep on the same baseline-hazard structure as the original fit,
    # instead of silently refitting a different model on every row.
    if (identical(fit$family, "survival")) {
      aux_by_i <- .na_to_null(sc$aux_by)
      if (!is.null(aux_by_i)) base_args$aux_by <- aux_by_i
      if (!is.null(sc$knots)) base_args$knots <- sc$knots
    }
    if (length(dots)) base_args[names(dots)] <- dots
    fit_i <- do.call(mlumr, base_args)

    results[[i]] <- .summarize_sensitivity(fit_i, scale = s,
                                          scale_comparator = s_cmp, probs = probs)
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  # Only carry `at_time` when it says something. Every non-survival family, and
  # survival fits whose scalar has no evaluation time, would otherwise gain an
  # all-NA column and the printed table would get noisier for no information.
  if (!is.null(out$at_time) && all(is.na(out$at_time))) out$at_time <- NULL
  # `scale_comparator` is the scale actually applied to `prior_beta_comparator`
  # on that row. It is retained for every relaxed fit, including the default
  # case where it equals `scale`, so a reader never has to infer which prior a
  # row varied; SPFA has no comparator coefficient prior, so the all-NA column
  # is dropped there rather than printed as noise.
  if (!is.null(out$scale_comparator) && all(is.na(out$scale_comparator))) {
    out$scale_comparator <- NULL
  }

  if (verbose) {
    cat("\nPrior sensitivity: posterior of marginal treatment effects\n")
    cat("=========================================================\n\n")
    print(out, row.names = FALSE)
    cat("\nInterpretation: if posterior summaries are approximately constant\n")
    cat("across scales, the result is robust to the tested prior scales.\n")
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
  # ratio; we want a homogeneous sensitivity sweep).
  lapply(prior, function(p) {
    p$sd <- new_scale
    p$default <- NULL
    p$version <- NULL
    p
  })
}

#' Summarize a sensitivity refit.
#' @keywords internal
.summarize_sensitivity <- function(fit, scale, scale_comparator = NA_real_, probs) {
  draws <- fit$draws
  family <- fit$family %||% "binomial"

  # Family-specific marginal-effect column names in the draws data frame.
  # Note: the natural scale differs by family. Binomial `lor_*` is a log odds
  # ratio (null 0); poisson `delta_*` is a rate ratio (null 1); normal `delta_*`
  # is a mean difference (null 0); survival `delta_*` is on the LOG scale and is
  # named per fit by `.surv_scalar_label()` below, because it is a log hazard
  # ratio, a log time ratio, or a bare location contrast depending on the
  # baseline shapes and the model. Each prior_sensitivity() call is
  # single-family, so rows within a call are comparable; do not compare
  # magnitudes across families without accounting for this (documented in
  # @return).
  effect_cols <- switch(family,
    binomial = c("lor_index",   "lor_comparator"),
    normal   = c("delta_index", "delta_comparator"),
    poisson  = c("delta_index", "delta_comparator"),
    survival = c("delta_index", "delta_comparator")
  )

  effect_names <- intersect(effect_cols, colnames(draws))
  if (length(effect_names) == 0L) return(NULL)

  # What the summarized quantity actually IS, per row. Without this the survival
  # rows were printed as a generic "log hazard ratio / log time ratio", which is
  # wrong in two of the three cases: a stratified PH delta is tied to a specific
  # evaluation time, and a stratified or relaxed AFT delta is a location
  # contrast rather than any time ratio. Derived from the same helper
  # marginal_effects() uses, so the two surfaces cannot disagree.
  if (identical(family, "survival")) {
    lab <- .surv_scalar_label(fit, log_scale = TRUE)
    eff_label <- lab$label
    eff_at_time <- lab$at_time
  } else {
    eff_label <- switch(family, binomial = "LOR", normal = "MD", poisson = "RR")
    eff_at_time <- NA_real_
  }

  # Match the quantile column naming used by .summarize_draw_matrix()
  # (`q<probs*100>`, e.g. q2.5/q50/q97.5) so sensitivity output joins cleanly
  # with the main summaries even for non-round `probs`.
  qnames <- paste0("q", probs * 100)

  rows <- lapply(effect_names, function(nm) {
    x <- draws[, nm]
    qs <- stats::quantile(x, probs = probs, names = FALSE)
    row <- data.frame(
      scale = scale,
      scale_comparator = scale_comparator,
      parameter = nm,
      effect = eff_label,
      at_time = eff_at_time,
      mean = mean(x),
      sd = stats::sd(x),
      stringsAsFactors = FALSE
    )
    for (j in seq_along(probs)) row[[qnames[[j]]]] <- qs[[j]]
    row
  })
  do.call(rbind, rows)
}
