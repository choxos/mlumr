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
#'   scales for `prior_beta_comparator`, same length as `prior_beta_scales` and
#'   paired with it elementwise. If `NULL` (default), the comparator prior is
#'   swept in parallel using `prior_beta_scales`. This matters because the
#'   relaxed index-population estimand is driven by the comparator coefficients,
#'   so a faithful sensitivity sweep must vary their prior. Whichever scale is
#'   used is reported in the `scale_comparator` column, so a refit that paired,
#'   say, an index scale of 0.5 with a comparator scale of 2.5 is not labeled
#'   as though only the index prior had been set. Ignored, with a warning, for
#'   SPFA fits, which have no comparator-specific coefficients.
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
#' For a relaxed fit the comparator prior is swept alongside the index prior.
#' By default the two move together, which is a single-factor sweep of overall
#' prior informativeness; supplying `prior_beta_comparator_scales` pairs a
#' different comparator scale with each index scale, so the two are then
#' separate factors moved in lockstep rather than one. Sweeping the comparator
#' prior is what makes the result meaningful: the relaxed index-population
#' estimand is driven by the comparator coefficients, so holding their prior
#' fixed would report a flat, reassuring curve for exactly the quantity most
#' exposed to the prior. Both scales are recorded per row (`scale` and
#' `scale_comparator`), so a row is never labeled by only half of the prior it
#' was fitted under.
#'
#' @return A data frame with one row per (prior scale, summarized parameter)
#'   pair, and columns `scale`, `scale_comparator` (dropped when the model has
#'   no comparator coefficient prior), `parameter`, `effect`, `at_time` (present
#'   only when the summarized effect has an evaluation time, so absent for every
#'   non-survival family and for survival scalars that carry none),
#'   `mean`, `sd`, and one column per requested quantile, named `q` followed by
#'   the percentage (the default `probs` give `q2.5`, `q50`, `q97.5`), matching
#'   [marginal_effects()]. Quantiles are columns, not a row dimension.
#'   Side effect: prints a summary table at the end.
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
  # `length() < 1L` matters on its own: `any()` of an empty vector is FALSE, so
  # an empty grid passed every other test and produced a sweep of nothing.
  invalid_prior_scales <- !is.numeric(prior_beta_scales) ||
    length(prior_beta_scales) < 1L ||
    any(!is.finite(prior_beta_scales)) ||
    any(prior_beta_scales <= 0)
  if (invalid_prior_scales) {
    stop("`prior_beta_scales` must be one or more positive finite numbers",
         call. = FALSE)
  }
  # The shared validator, not a local copy of it. The copy omitted the
  # duplicate check, so two equal probabilities produced two identically named
  # `qNN` columns and the second silently overwrote the first: the caller asked
  # for n quantiles and got fewer, with no error and no way to tell.
  .validate_probs(probs)
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(prior_beta_comparator_scales)) {
    if (!is_relaxed) {
      # Branch on the model before validating. Checking first turned an
      # inapplicable argument into a hard error on a fit that has no comparator
      # coefficients, and a well-formed value was dropped without a word.
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
                   "prior_beta_comparator", "prior_intercept", "prior_sigma",
                   "prior_aux", "prior_aux2", "prior_smooth", "center", "qr",
                   "n_knots", "knots", "mspline_degree", "pred_times",
                   "rmst_horizon", "n_rmst_grid", "aux_by")
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

  # The relaxed model's comparator coefficients carry their own prior, so a
  # sweep that moved only `prior_beta` would leave the comparator regularizer
  # fixed and report the sensitivity of a model nobody fitted. The comparator
  # prior is swept from the fit's OWN comparator prior, paired one-to-one with
  # the index scales, and `prior_beta_comparator_scales` decouples the two when
  # the question is specifically how much the comparator prior is doing.
  base_beta_cmp <- fit$priors$beta_comparator %||% base_beta
  cmp_scales <- if (is_relaxed) {
    prior_beta_comparator_scales %||% prior_beta_scales
  } else {
    NULL
  }

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

    # `...` is documented as the way to pass sampler and backend controls, and
    # `protected` above already keeps it away from anything that defines the
    # scenario. Merge rather than concatenate: `args` already names `chains`,
    # `iter` and the rest, so appending a `...` value would match the same
    # formal twice and R refuses the call. That made the one thing `...` is for
    # the one thing it could not do.
    call_args <- args
    if (length(dots)) {
      # The recorded `control` describes the original fit, so a caller's
      # settings refine it rather than replace it: assigning theirs wholesale
      # dropped every recorded entry they did not happen to name, such as
      # `adapt_engaged`. And a scalar they pass has to beat the recorded entry
      # of the same name, because the merge downstream lets the control win and
      # the recorded control would otherwise silently overrule this caller's
      # own request. Their `control` still beats their scalar, which is the
      # order `mlumr()` itself uses.
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
      call_args[names(dots)] <- dots
      # `control` is rstan's argument. Restoring the recorded one after a
      # caller has switched engines would forward it to cmdstanr's `$sample()`,
      # which has no such argument, so every refit would fail before sampling.
      # The engine that will actually run is the caller's if they named one.
      engine_used <- dots$engine %||% call_args$engine
      if (!is.null(ctl) && identical(engine_used, "rstan")) {
        call_args$control <- ctl
      } else if (!identical(engine_used, "rstan")) {
        call_args$control <- NULL
      }
    }
    fit_i <- do.call(mlumr, call_args)

    results[[i]] <- .summarize_sensitivity(
      fit_i, scale = s, scale_comparator = s_cmp, probs = probs
    )
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
    # What a scale sweep can support and no more. Constant summaries across a
    # few scales of ONE prior family, at one location, on one model show
    # insensitivity to those scales; they do not show that the data rather than
    # the prior is driving the answer, which is the stronger claim this used to
    # print. A weakly identified coefficient direction can be equally
    # prior-determined at every scale tested.
    cat(.prior_sensitivity_interpretation(), sep = "\n")
    cat("\n")
  }

  invisible(out)
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

  args <- list(
    data = fit$data,
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

  # Every other sampler setting the original fit ran under. Recorded only by
  # the rstan backend, which is the only one that takes a `control` list, so
  # its absence is what keeps this off the cmdstanr path. `adapt_delta` and
  # `max_treedepth` appear in both and agree by construction, since the stored
  # scalars are taken from this same merged list.
  if (!is.null(sa$control)) {
    args$control <- sa$control
  }

  # The comparator prior for this refit, already rescaled by the caller. NULL
  # for a non-relaxed fit, which has no comparator coefficients.
  if (!is.null(prior_beta_comparator_i)) {
    args$prior_beta_comparator <- prior_beta_comparator_i
  }

  # The survival controls are only legal arguments for a survival fit, and
  # mlumr() decides that with missing(), not by testing for NULL. Naming
  # `aux_by` at all makes it non-missing, so listing these unconditionally
  # would make prior_sensitivity() fail on every binomial, normal and Poisson
  # fit. Omit the whole group rather than passing NULL into it.
  if (identical(fit$family, "survival")) {
    args <- c(args, list(
      distribution = fit$distribution,
      prior_aux    = fit$priors$aux,
      # Present only for a generalized-gamma fit. NULL here means "reuse
      # prior_aux", which is what every other distribution's refit needs, so
      # this one is safe to pass unconditionally inside the survival branch.
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
    # No Stan model emits `lrr_*`; the poisson marginal effect is `delta_*`,
    # as family_config records. Looking for the wrong name matched no columns
    # at all, so a poisson sweep summarized nothing.
    poisson  = c("delta_index", "delta_comparator"),
    # Log hazard ratio (PH) or log time ratio (AFT). Reported on the log scale,
    # like every other family here, so the scales stay comparable across rows.
    survival = c("delta_index", "delta_comparator")
  )

  effect_names <- intersect(effect_cols, colnames(draws))
  if (length(effect_names) == 0L) return(NULL)

  # What the summarized quantity actually IS, per row. Without this the survival
  # rows are printed as a generic "log hazard ratio / log time ratio", which is
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

  # `round()` here both mislabeled the defaults (2.5 and 97.5 became `q2` and
  # `q98`) and could collide: probs 0.024 and 0.025 both produced `q2`, and the
  # assignment below then overwrote the first quantile with the second without a
  # word. Name them the way the rest of the package does, so a caller can bind
  # prior_sensitivity() output to marginal_effects() output by column name, and
  # through the shared helper, whose names `.validate_probs()` has already
  # checked for the collision that survives full precision.
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
#' Kept as a function rather than inline `cat()` calls so the shipped vignette
#' can be checked against it by CALLING it. The check used to locate these
#' lines by matching the source text and skipped when the markers moved, which
#' turned the one edit the gate exists to catch into a silent pass.
#'
#' @return A character vector, one element per printed line.
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
