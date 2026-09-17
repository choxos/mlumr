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
#' @param prior_beta_comparator_scales (Relaxed fits only.) Numeric vector of
#'   scales for `prior_beta_comparator`, paired elementwise with
#'   `prior_beta_scales`. `NULL` (default) sweeps the comparator prior in
#'   parallel with `prior_beta_scales`; the scale used is reported in the
#'   `scale_comparator` column. Ignored, with a warning, for SPFA fits.
#' @param probs Quantiles for summarizing each posterior
#'   (default `c(0.025, 0.5, 0.975)`).
#' @param verbose Logical; if `FALSE`, suppresses progress messages and final
#'   printed summary table.
#' @param ... Additional arguments forwarded to [mlumr()] on each refit
#'   (e.g. `chains`, `iter`, `refresh`). Sampling defaults otherwise inherit
#'   from the original fit.
#'
#' @details
#' Only the scale of `prior_beta` is varied; its family and mean, and every
#' other prior and setting, come from the original fit. Each scale is applied
#' to every coefficient, so the sweep reflects one level of prior
#' informativeness per refit; an exponential `prior_beta` is swapped for
#' `prior_normal(0, scale)`. For a relaxed fit the comparator prior is swept
#' alongside, because the index-population estimand is driven by the
#' comparator coefficients; holding their prior fixed would report a flat curve
#' for exactly the quantity most exposed to the prior.
#'
#' @return A data frame with one row per (prior scale, summarized parameter)
#'   pair, and columns `scale`, `scale_comparator` (dropped when the model has
#'   no comparator coefficient prior), `parameter`, `effect`, `at_time` (present
#'   only when the summarized effect has an evaluation time, so absent for every
#'   non-survival family and for survival scalars that carry none),
#'   `mean`, `sd`, and one column per requested quantile, named `q` followed by
#'   the percentage (the default `probs` give `q2.5`, `q50`, `q97.5`), matching
#'   [marginal_effects()]. Quantiles are columns, not a row dimension.
#'   Side effect: prints a summary table at the end when `verbose = TRUE`,
#'   which is the default; `verbose = FALSE` returns the same data frame and
#'   prints nothing.
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
  is_relaxed <- identical(fit$model, "relaxed")
  # `any()` of an empty vector is FALSE, so the length check is needed.
  invalid_prior_scales <- !is.numeric(prior_beta_scales) ||
    length(prior_beta_scales) < 1L ||
    any(!is.finite(prior_beta_scales)) ||
    any(prior_beta_scales <= 0)
  if (invalid_prior_scales) {
    stop("`prior_beta_scales` must be one or more positive finite numbers",
         call. = FALSE)
  }
  .validate_probs(probs)
  .validate_flag(verbose, "verbose")

  if (!is.null(prior_beta_comparator_scales)) {
    if (!is_relaxed) {
      warning("`prior_beta_comparator_scales` is ignored for the ",
              fit$model, " model (which has a single shared `beta`); only the ",
              "relaxed model has a comparator-specific coefficient vector.",
              call. = FALSE)
      prior_beta_comparator_scales <- NULL
    } else {
      invalid_cmp <- !is.numeric(prior_beta_comparator_scales) ||
        any(!is.finite(prior_beta_comparator_scales)) ||
        any(prior_beta_comparator_scales <= 0) ||
        length(prior_beta_comparator_scales) != length(prior_beta_scales)
      if (invalid_cmp) {
        stop("`prior_beta_comparator_scales` must be positive finite numbers ",
             "the same length as `prior_beta_scales`, so that each row of the ",
             "sweep pairs one index scale with one comparator scale.",
             call. = FALSE)
      }
    }
  }

  # Extract the base prior so we can reconstruct scaled variants.
  base_beta <- fit$priors$beta
  if (is.null(base_beta)) {
    stop("Original `prior_beta` not found on the fit. ",
         "Was the model fitted with an older version of mlumr?", call. = FALSE)
  }

  # `...` reaches each refit, so it may carry sampler controls only.
  dots <- list(...)
  if (length(dots)) {
    if (is.null(names(dots)) || any(!nzchar(names(dots)))) {
      stop("All `...` arguments to prior_sensitivity() must be named.",
           call. = FALSE)
    }
    protected <- c("data", "model", "link", "distribution", "prior_beta",
                   "prior_beta_comparator", "prior_intercept", "prior_sigma",
                   "prior_aux", "prior_aux2", "prior_smooth", "center", "qr",
                   "n_knots", "knots", "mspline_degree", "pred_times",
                   "rmst_horizon", "n_rmst_grid", "aux_by")
    clash <- intersect(names(dots), protected)
    if (length(clash)) {
      stop(sprintf(paste0("`...` cannot override the scenario-defining ",
                          "argument(s): %s. Pass sampler or backend controls ",
                          "only."), paste(clash, collapse = ", ")),
           call. = FALSE)
    }
  }

  # The comparator prior is swept from the fit's own comparator prior.
  base_beta_cmp <- fit$priors$beta_comparator %||% base_beta
  cmp_scales <- if (is_relaxed) {
    prior_beta_comparator_scales %||% prior_beta_scales
  } else {
    NULL
  }

  # Backend arguments the original fit forwarded were recorded by name only,
  # so a refit cannot replay them; say so once per call.
  .warn_unreplayed_backend_args(
    (fit$sampling_args %||% list())$extra_backend_args, names(dots)
  )

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

    msg <- if (is_relaxed) {
      sprintf("Prior sensitivity: refit %d/%d with scale %g (comparator %g)",
              i, length(prior_beta_scales), s, s_cmp)
    } else {
      sprintf("Prior sensitivity: refit %d/%d with scale %g",
              i, length(prior_beta_scales), s)
    }
    mlumr_message(msg, verbose = verbose)

    args <- .prior_sensitivity_args(fit, prior_beta_i, verbose,
                                    prior_beta_cmp_i)

    # Merge rather than concatenate: `args` already names `chains` and the rest.
    call_args <- .prior_sensitivity_merge_dots(args, dots)
    fit_i <- do.call(mlumr, call_args)

    results[[i]] <- .summarize_sensitivity(
      fit_i, scale = s, scale_comparator = s_cmp, probs = probs
    )
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  # Drop an all-NA `at_time` column.
  if (!is.null(out$at_time) && all(is.na(out$at_time))) out$at_time <- NULL
  # SPFA has no comparator prior, so its all-NA `scale_comparator` is dropped.
  if (!is.null(out$scale_comparator) && all(is.na(out$scale_comparator))) {
    out$scale_comparator <- NULL
  }

  if (verbose) {
    cat("\nPrior sensitivity: posterior of marginal treatment effects\n")
    cat("=========================================================\n\n")
    print(out, row.names = FALSE)
    cat(.prior_sensitivity_interpretation(), sep = "\n")
    cat("\n")
  }

  invisible(out)
}

#' Name the backend settings a refit is not reproducing
#'
#' @param recorded Names the original fit passed through `...`.
#' @param supplied Names the caller has re-supplied for these refits.
#' @keywords internal
.warn_unreplayed_backend_args <- function(recorded, supplied) {
  if (!length(recorded)) {
    return(invisible(NULL))
  }
  absent <- setdiff(recorded[nzchar(recorded)], supplied)
  if (!length(absent)) {
    return(invisible(NULL))
  }
  warning("The original fit passed ", paste(sQuote(absent), collapse = ", "),
          " through to the backend, and only their names were recorded, so ",
          "these refits use the defaults for them. Pass them again through ",
          "`...` to hold them fixed.", call. = FALSE)
  invisible(NULL)
}


#' A caller's `...` merged into the arguments that replay a fit
#'
#' A caller's settings refine the recorded `control` rather than replace it,
#' and the engine is resolved the way `mlumr()` resolves it.
#' @keywords internal
.prior_sensitivity_merge_dots <- function(call_args, dots) {
  if (!length(dots)) return(call_args)
  # A caller's scalar beats the recorded entry; their `control` beats their
  # scalar.
  ctl <- call_args$control
  if (!is.null(ctl)) {
    for (nm in intersect(names(dots), c("adapt_delta", "max_treedepth"))) {
      ctl[[nm]] <- dots[[nm]]
    }
  }
  if (!is.null(dots$control)) {
    ctl <- if (is.null(ctl)) dots$control else
      utils::modifyList(ctl, dots$control)
  }
  fit_engine <- call_args$engine
  call_args[names(dots)] <- dots
  # `control` is rstan's argument, so it is kept only when the engine that
  # will run is rstan; `engine = NULL` means the configured default.
  engine_used <- .validate_engine_name(
    (if ("engine" %in% names(dots)) dots$engine else fit_engine) %||%
      get_engine()
  )
  if (identical(engine_used, "rstan")) {
    if (!is.null(ctl)) {
      call_args$control <- ctl
    }
  } else {
    # An inherited control is dropped; a control the caller passed is refused
    # rather than silently dropped. `control = NULL` means nothing was asked.
    if (!is.null(dots$control)) {
      stop(
        sprintf(paste(
          "`control` is an rstan setting and these refits run on %s, which",
          "has no such argument. Pass `adapt_delta` and `max_treedepth`",
          "directly, or drop `engine`."
        ), engine_used),
        call. = FALSE
      )
    }
    call_args$control <- NULL
  }
  call_args
}

#' Arguments that replay a fit under a rescaled `prior_beta`
#'
#' Everything except the prior being swept has to come from the original fit, or
#' the sweep varies more than one factor. Split out from the refit loop so the
#' replay can be checked without sampling.
#' @keywords internal
.prior_sensitivity_args <- function(fit, prior_beta_i, verbose,
                                    prior_beta_comparator_i = NULL) {
  sa <- fit$sampling_args %||% list()
  # Design-matrix controls; fits from earlier versions used the old defaults.
  mc <- fit$model_controls %||% list()
  # Survival controls; a control that does not apply is recorded as NA, which
  # mlumr() rejects where it accepts NULL.
  sc <- fit$surv_controls %||% list()
  .na_to_null <- function(x) if (length(x) == 1L && is.na(x)) NULL else x

  args <- list(
    data = fit$data,
    model = fit$model,
    link = fit$link,
    prior_intercept = fit$priors$intercept,
    prior_beta = prior_beta_i,
    prior_sigma = fit$priors$sigma %||% default_prior_sigma(),
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

  # Recorded by the rstan backend only.
  if (!is.null(sa$control)) {
    args$control <- sa$control
  }


  if (!is.null(prior_beta_comparator_i)) {
    args$prior_beta_comparator <- prior_beta_comparator_i
  }

  # mlumr() tests these with missing(), so the whole group is omitted rather
  # than passed as NULL for the other families.
  if (identical(fit$family, "survival")) {
    args <- c(args, list(
      distribution = fit$distribution,
      prior_aux    = fit$priors$aux,
      prior_aux2   = fit$priors$aux2,
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
  args
}


# ---- Helpers ---------------------------------------------------------------

#' Rescale a prior_beta's scale, preserving family, mean and df
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

#' Summarize a sensitivity refit
#' @keywords internal
.summarize_sensitivity <- function(fit, scale, scale_comparator = NA_real_,
                                   probs) {
  draws <- fit$draws
  family <- fit$family %||% "binomial"

  # Family-specific marginal-effect column names in the draws data frame
  effect_cols <- switch(family,
    binomial = c("lor_index",   "lor_comparator"),
    normal   = c("delta_index", "delta_comparator"),
    poisson  = c("delta_index", "delta_comparator"),
    # Log scale, like every other family here.
    survival = c("delta_index", "delta_comparator")
  )

  effect_names <- intersect(effect_cols, colnames(draws))
  if (length(effect_names) == 0L) return(NULL)

  # The label comes from the helper marginal_effects() uses.
  if (identical(family, "survival")) {
    lab <- .surv_scalar_label(fit, log_scale = TRUE)
    eff_label <- lab$label
    eff_at_time <- lab$at_time
  } else {
    eff_label <- switch(family, binomial = "LOR", normal = "MD", poisson = "RR")
    eff_at_time <- NA_real_
  }

  # Quantile column names as marginal_effects() names them.
  qnames <- .quantile_names(probs)

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


#' The interpretation paragraph `prior_sensitivity()` prints
#'
#' A function so the vignette can be checked against it.
#' @keywords internal
.prior_sensitivity_interpretation <- function() {
  c("",
    "Interpretation: approximately constant summaries show the posterior",
    "is insensitive to the beta-prior SCALES tested here. That is not the",
    "same as the inference being data-driven: identification also depends",
    "on the prior family and location, the comparator-specific and",
    "auxiliary priors, and the model structure. Vary those too. For a",
    "non-survival fit, check_identification() adds the geometry of the",
    "aggregate rows: exact for a normal identity-link model, descriptive",
    "otherwise.")
}
