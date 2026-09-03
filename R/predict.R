#' Predictions from ML-UMR model
#'
#' Generate population-average absolute-outcome predictions in the index and
#' comparator populations.
#'
#' @param object An `mlumr_fit` object
#' @param population Which population: `"both"`, `"index"`, or `"comparator"`
#' @param type Prediction type. For binomial/normal/poisson: `"response"`
#'   (default) or `"link"`. For survival: `"survival"` (default), `"hazard"`,
#'   `"cumhaz"`, `"rmst"` (restricted mean survival time), `"median"` (median
#'   survival, obtained by linear interpolation on the fitted `pred_times` grid;
#'   if the true median precedes the first grid point it is interpolated between
#'   the known exact point `S(0) = 1` and `(pred_times[1], S(pred_times[1]))`,
#'   so it is never reported as later than the first grid time, though a denser
#'   `pred_times` near zero still resolves very early medians better), or
#'   `"loghr"` (time-varying marginal log hazard ratio of
#'   index vs comparator at each fitted time, per population). For `"response"`:
#'   probabilities (binomial), means
#'   (normal), or rates (poisson). For `"link"`: the fitted link applied to the
#'   population-standardized response mean, `g(E[g^{-1}(eta)])`. This is the
#'   marginal link-scale prediction used by G-computation; it is generally not
#'   the mean conditional linear predictor `E[eta]`.
#' @param summary Return summary statistics (`TRUE`) or full posterior draws (`FALSE`)
#' @param probs Quantiles for summary (default `c(0.025, 0.5, 0.975)`)
#' @param times For survival fits, an optional vector of times at which to
#'   report curve predictions; each is matched to the nearest fitted
#'   `pred_times` grid point. If `NULL`, all fitted times are returned.
#' @param newdata Optional data frame of covariate profiles defining an arbitrary
#'   **target population**. When supplied, per-treatment absolute predictions are
#'   standardized to this population by g-computation (averaging model-based
#'   predictions over the rows at each posterior draw), and `population` is
#'   ignored. Supports `type = "response"`/`"link"` (binomial/normal/poisson) and
#'   `type = "survival"`/`"hazard"`/`"cumhaz"`/`"rmst"`/`"median"`/`"loghr"`
#'   (survival). Survival hazards use the target-specific survival-weighted
#'   definition, so `"loghr"` is population-specific and time-varying. Rows
#'   outside the covariate support used to fit a treatment model are accepted as
#'   model-based extrapolation; the function does not certify overlap or
#'   transportability, so users must assess support and run sensitivity analyses.
#' @param ... Additional arguments (unused)
#'
#' @details
#' **Marginalization on non-identity links.** For `type = "response"` the
#' reported values are `E[g^{-1}(eta)]`, the posterior expectation of the
#' inverse-link-transformed linear predictor, *not* `g^{-1}(E[eta])`. The
#' two differ whenever `g` is non-linear (logit, probit, cloglog, log) by
#' Jensen's inequality. In the index population the expectation is taken
#' over IPD individuals; in the comparator population it is taken over the
#' integration points constructed by [add_integration()] from the AgD
#' moments. This is the population-average prediction for an individual
#' randomly drawn from that population, and it matches what the Stan
#' `generated quantities` block computes. `type = "link"` applies the fitted
#' link only after that marginalization. It coincides with `E[eta]` for an
#' identity link but not generally for logit, probit, cloglog, or log links.
#'
#' @return A data frame with predictions. When `type = "link"`, values are the
#'   fitted link applied to each draw's population-standardized response mean.
#'   `type = "rmst"` adds a `horizon` column: RMST is
#'   an integral to a restriction time, so values computed to different horizons
#'   are different estimands and must not be compared. The horizon reported is
#'   the one actually integrated to. That is the `rmst_horizon` given to
#'   [mlumr()] when one was supplied; when it was left `NULL` it is the default,
#'   which for a study-stratified flexible baseline is the follow-up both
#'   studies observed rather than the pooled maximum.
#'   The plot methods require `summary = TRUE`; with `summary = FALSE` the raw
#'   posterior draws are returned as a plain data frame.
#' @seealso [marginal_effects()] for treatment-effect summaries;
#'   [conditional_predict()] and [conditional_effects()] for predictions
#'   at specific covariate profiles.
#' @examples
#' \dontrun{
#' # Absolute predictions for both populations:
#' predict(fit, population = "both")
#' # Survival RMST, and transport to a target covariate distribution:
#' predict(fit, type = "rmst")
#' predict(fit, newdata = target_population)
#' }
#' @export
predict.mlumr_fit <- function(object,
                              population = c("both", "index", "comparator"),
                              type = NULL,
                              summary = TRUE,
                              probs = c(0.025, 0.5, 0.975),
                              times = NULL,
                              newdata = NULL,
                              ...) {

  .validate_mlumr_fit_object(object)
  summary <- .validate_summary_flag(summary)
  .validate_probs(probs)

  family <- object$family %||% "binomial"

  # Transport absolute predictions to an arbitrary target population
  # (g-computation over `newdata`); isolated from the built-in populations.
  # Dispatched before `population` is validated because the documentation says
  # `population` is ignored here, and an argument that cannot affect the answer
  # should not be able to refuse the call.
  if (!is.null(newdata)) {
    return(.predict_target(object, newdata, type, summary, probs, times))
  }

  population <- .validate_predict_choice(population, c("both", "index", "comparator"),
                                         "population")

  if (family == "survival") {
    ptype <- type %||% "survival"
    out <- .predict_survival(object, population = population,
                             type = ptype,
                             summary = summary, probs = probs, times = times)
    if (isTRUE(summary)) {
      out <- .mlumr_result(out, "mlumr_prediction", ptype = ptype,
                           family = "survival")
    }
    return(out)
  }

  type <- .validate_predict_choice(type %||% "response", c("response", "link"), "type")

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
  .mlumr_result(cbind(labels, summary_df, row.names = NULL),
                "mlumr_prediction", ptype = type, family = family)
}


#' Validate user-supplied survival prediction `times`
#' @keywords internal
.validate_survival_prediction_times <- function(times) {
  if (!is.numeric(times) || length(times) < 1L || any(!is.finite(times)) ||
        any(times <= 0)) {
    stop("`times` must be finite, positive numbers.", call. = FALSE)
  }
  times
}

#' Fitted-grid indices for a user-supplied survival prediction `times`
#'
#' Survival quantities exist only at the times the model was fitted to, so an
#' arbitrary `times` is snapped to its nearest fitted neighbor. Shared by the
#' built-in and `newdata` routes so that both validate before they select, and
#' select the same way.
#' @keywords internal
.surv_time_selection <- function(times, pred_times) {
  if (is.null(times)) return(seq_along(pred_times))
  .validate_survival_prediction_times(times)
  sort(unique(vapply(times, function(t) which.min(abs(pred_times - t)),
                     integer(1))))
}

#' Value of a survival curve at the origin, or `NA_real_` when it has none
#'
#' Survival is 1 and cumulative hazard is 0 at t = 0 (exactly, by definition),
#' so curves of those two types are prepended with that origin: they then start
#' at the top-left corner, matching the Kaplan-Meier convention (cf. multinma's
#' geom_km). Hazard has no universal value at t = 0 (it can be 0 or infinite
#' depending on the distribution), so it is left to start at the first fitted
#' time. Added only for the full default curve (`times = NULL`) and when t = 0
#' is not already a fitted time.
#' @keywords internal
.surv_origin <- function(type, times, pred_times) {
  origin <- switch(type, survival = 1, cumhaz = 0, NA_real_)
  if (is.na(origin) || !is.null(times)) return(NA_real_)
  if (any(abs(pred_times) < sqrt(.Machine$double.eps))) return(NA_real_)
  origin
}

#' Assemble a survival prediction frame from per-cell draw matrices
#'
#' Both prediction routes reduce to the same thing: one draw matrix per
#' displayed cell. Everything after that is layout, and layout written twice
#' drifts, so it is written once here. `values` is a list of draw matrices, one
#' per row of `cells`, with a single column for scalar types (`rmst`, `median`)
#' and one column per selected time for curves. `cells` carries the display
#' labels, `treatment` (absent for `loghr`, which is a within-population
#' contrast) followed by `population`.
#' @keywords internal
.surv_result_frame <- function(values, cells, type, summary, probs,
                               times_out = NULL, origin = NA_real_,
                               horizon = NULL) {
  label_names <- intersect(c("treatment", "population"), names(cells))

  rows <- lapply(seq_along(values), function(i) {
    m <- values[[i]]
    lab <- cells[i, label_names, drop = FALSE]
    rownames(lab) <- NULL
    if (is.null(times_out)) {
      if (!summary) {
        return(data.frame(lab, value = m[, 1], row.names = NULL,
                          check.names = FALSE))
      }
      s <- .summarize_draw_matrix(m, probs)
      # For median survival, draws whose fitted survival never reaches 0.5 over
      # the prediction grid have no finite median ("median not reached"). The
      # shared summarizer uses na.rm, so its mean/SD/quantiles are conditional
      # on the median being reached; expose the posterior probability that it is
      # not rather than silently reporting a finite, precise-looking median.
      if (type == "median") s$p_not_reached <- mean(is.na(m[, 1]))
      return(data.frame(lab, s, row.names = NULL, check.names = FALSE))
    }
    if (!summary) {
      colnames(m) <- sprintf("t_%.15g", times_out)
      df <- data.frame(lab, m, row.names = NULL, check.names = FALSE)
      if (!is.na(origin)) {
        df <- data.frame(df[label_names], t_0 = origin,
                         df[setdiff(names(df), label_names)],
                         check.names = FALSE)
      }
      return(df)
    }
    s <- .summarize_draw_matrix(m, probs)
    df <- data.frame(lab, time = times_out, s, row.names = NULL,
                     check.names = FALSE)
    if (!is.na(origin)) {
      o <- df[1, , drop = FALSE]
      o$time <- 0
      # Only the summarized quantities take the origin value. Treatment labels
      # can be numeric, and overwriting them here set every arm's t = 0 row to
      # the origin (1 for survival), so those rows claimed to belong to a
      # treatment called "1".
      num_cols <- setdiff(names(o)[vapply(o, is.numeric, logical(1))],
                          c("time", label_names))
      o[num_cols] <- origin
      if ("sd" %in% names(o)) o$sd <- 0
      df <- rbind(o, df)
    }
    df
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # RMST is an integral to a restriction time, so the number is only half the
  # estimand without it. Report the horizon actually integrated to as a COLUMN
  # in both layouts, rather than leaving a consumer to know to look for an
  # attribute.
  if (!is.null(horizon)) {
    if (summary) {
      n_lab <- length(label_names)
      out <- cbind(out[, seq_len(n_lab), drop = FALSE], horizon = horizon,
                   out[, -seq_len(n_lab), drop = FALSE])
    } else {
      out$horizon <- horizon
    }
    rownames(out) <- NULL
  }
  if (type == "median" && summary && any(out$p_not_reached > 0)) {
    .median_not_reached_note(max(out$p_not_reached))
  }
  out
}

#' Survival predictions (internal dispatch for [predict.mlumr_fit()])
#'
#' Reads the population-standardized survival generated quantities and returns
#' a tidy summary by treatment, population, and (for curves) time.
#' @keywords internal
.predict_survival <- function(object, population, type, summary, probs,
                              times = NULL) {
  valid_types <- c("survival", "hazard", "cumhaz", "rmst", "median", "loghr")
  if (!is.character(type) || length(type) != 1L || !(type %in% valid_types)) {
    stop(sprintf("For survival fits, `type` must be one of: %s.",
                 paste(valid_types, collapse = ", ")), call. = FALSE)
  }
  if (!is.null(times)) times <- .validate_survival_prediction_times(times)
  draws <- object$draws
  pred_times <- object$pred_times
  idx_trt <- object$data$index_treatment
  cmp_trt <- object$data$comparator_treatment

  pops <- switch(population, index = "index", comparator = "comparator",
                 both = c("index", "comparator"))
  cells <- expand.grid(trt = c("index", "comparator"), pop = pops,
                       stringsAsFactors = FALSE)
  pop_label <- function(p) if (p == "index") "Index" else "Comparator"
  # Index the labels rather than mapping through vapply(): set_ipd()/set_agd()
  # accept numeric and factor treatment identifiers, and `character(1)` rejects
  # those before any prediction is assembled. Subsetting preserves whatever
  # type the fit carries, which is what the per-cell construction this replaced
  # did.
  cell_labels <- data.frame(
    treatment = c(idx_trt, cmp_trt)[match(cells$trt,
                                          c("index", "comparator"))],
    population = vapply(cells$pop, pop_label, character(1)),
    stringsAsFactors = FALSE
  )

  # Absolute predictions in the OTHER study's population carry this study's
  # baseline shape with them, which is an assumption the data cannot check.
  # `loghr` is exempt: it is a contrast within one population.
  if (type != "loghr") .transported_baseline_note(object)

  # Time-varying marginal log hazard ratio (index vs comparator) by population:
  # log( h-bar_index(t | pop) / h-bar_comparator(t | pop) ) at each fitted time.
  if (type == "loghr") {
    sel <- .surv_time_selection(times, pred_times)
    values <- lapply(pops, function(pop) {
      cols_lhr <- sprintf("loghr_%s[%d]", pop, sel)
      if (all(cols_lhr %in% names(draws))) {
        # Current survival models (parametric and M-spline / piecewise
        # exponential) emit the marginal log HR directly in log space, so it
        # stays finite even where the natural-scale marginal hazard underflows
        # to 0 deep in the tail (e.g. generalized gamma, or late extrapolation).
        return(as.matrix(draws[, cols_lhr, drop = FALSE]))
      }
      # M-spline / piecewise-exponential (and older fits): difference of the
      # natural-scale log hazards.
      cols_i <- sprintf("haz_index_%s[%d]", pop, sel)
      cols_c <- sprintf("haz_comparator_%s[%d]", pop, sel)
      .require_draw_columns(draws, c(cols_i, cols_c), "survival prediction")
      log(as.matrix(draws[, cols_i, drop = FALSE])) -
        log(as.matrix(draws[, cols_c, drop = FALSE]))
    })
    return(.surv_result_frame(
      values,
      data.frame(population = vapply(pops, pop_label, character(1)),
                 stringsAsFactors = FALSE),
      type, summary, probs, times_out = pred_times[sel]
    ))
  }

  # Scalar summaries (RMST, median): one row per treatment x population.
  if (type %in% c("rmst", "median")) {
    # `times` selects points on a curve. RMST is an integral to the fitted
    # horizon and the median is read off the whole fitted grid, so neither
    # consults it. Silently ignoring it let a caller believe they had changed
    # the horizon or extended the search for S = 0.5 when they had not.
    if (!is.null(times)) {
      stop("`times` selects points on a predicted curve, but `type = \"", type,
           "\"` is a scalar summary of the whole fitted grid and does not use ",
           "it. RMST integrates to the horizon fixed at fit time, and the ",
           "median is searched over `pred_times`; change either by refitting.",
           call. = FALSE)
    }
    values <- lapply(seq_len(nrow(cells)), function(i) {
      if (type == "rmst") {
        col <- sprintf("rmst_%s_%s", cells$trt[i], cells$pop[i])
        .require_draw_columns(draws, col, "survival prediction")
        return(matrix(draws[[col]], ncol = 1))
      }
      cols <- sprintf("surv_%s_%s[%d]", cells$trt[i], cells$pop[i],
                      seq_along(pred_times))
      .require_draw_columns(draws, cols, "survival prediction")
      matrix(.surv_median_from_draws(as.matrix(draws[, cols, drop = FALSE]),
                                     pred_times), ncol = 1)
    })
    # RMST is an integral to a restriction time, so the number is only half the
    # estimand without it. Report the horizon actually integrated to, read off
    # the fitted grid: that is the requested `rmst_horizon` when one was given,
    # and otherwise mlumr()'s default, which for a study-stratified flexible
    # baseline is the common support rather than the pooled maximum.
    horizon <- if (type == "rmst") {
      g <- object$stan_data$rmst_grid_times
      if (is.null(g)) NA_real_ else max(g)
    } else {
      NULL
    }
    return(.surv_result_frame(values, cell_labels, type, summary, probs,
                              horizon = horizon))
  }

  # Curve summaries (survival, hazard, cumhaz): one row per time.
  sel <- .surv_time_selection(times, pred_times)
  qty <- switch(type, survival = "surv", hazard = "haz", cumhaz = "cumhaz")
  values <- lapply(seq_len(nrow(cells)), function(i) {
    cols <- sprintf("%s_%s_%s[%d]", qty, cells$trt[i], cells$pop[i], sel)
    .require_draw_columns(draws, cols, "survival prediction")
    as.matrix(draws[, cols, drop = FALSE])
  })
  .surv_result_frame(values, cell_labels, type, summary, probs,
                     times_out = pred_times[sel],
                     origin = .surv_origin(type, times, pred_times))
}


#' Marginal posterior variance change for comparator coefficients
#'
#' For each `beta_comparator` coefficient this reports
#' `1 - posterior_variance / prior_variance`. A positive value means the
#' marginal posterior SD is smaller than the marginal prior SD, zero means the
#' two SDs are equal, and a negative value means the posterior SD is larger.
#' This descriptive variance comparison is not a fraction of information
#' learned, an identification test, or a decomposition of data and prior
#' influence. It ignores changes in location, shape, and posterior correlation.
#'
#' The ratio is only interpretable against a prior with a finite variance.
#' `prior_sd` is therefore the prior STANDARD DEVIATION, not the stored scale:
#' for a Student-t prior they differ by `sqrt(df / (df - 2))`, and for `df <= 2`
#' (including the `df = 1` Cauchy) no finite variance exists and `contraction`
#' is `NA` rather than a number that would misstate what was learned.
#'
#' @param object An `mlumr_fit` from `model = "relaxed"`.
#' @return A data frame with one row per covariate (`covariate`, `prior_sd`,
#'   `posterior_sd`, `contraction`), or `NULL` if unavailable. The retained
#'   `contraction` column is the marginal variance change defined above and is
#'   `NA` where the prior has no finite variance.
#' @keywords internal
.relaxed_contraction <- function(object) {
  if (!identical(object$model, "relaxed")) return(NULL)
  # Falls back to the shared coefficient prior where a separate comparator
  # prior is not present, which is what the relaxed models place on
  # beta_comparator in that case.
  prior_scale <- object$stan_data$prior_beta_comparator_sd %||%
    object$stan_data$prior_beta_sd
  covs <- object$data$covariates
  if (is.null(prior_scale) || is.null(covs)) return(NULL)
  prior_scale <- as.numeric(prior_scale)
  # The stored hyperparameter is the prior SCALE, which is the prior standard
  # deviation only for a normal prior. A Student-t prior has
  # SD = scale * sqrt(df / (df - 2)) for df > 2, and NO finite variance for
  # df <= 2 (the Cauchy that `prior_cauchy()` produces is df = 1). Dividing a
  # posterior SD by a scale that is not an SD would not produce the stated
  # marginal variance comparison, so convert where the variance exists and
  # return NA where it does not.
  dist <- object$stan_data$prior_beta_comparator_dist %||%
    object$stan_data$prior_beta_dist %||% 0L
  df <- object$stan_data$prior_beta_comparator_df %||%
    object$stan_data$prior_beta_df %||% NA_real_
  prior_sd <- if (identical(as.integer(dist)[1], 1L)) {
    d <- as.numeric(df)[1]
    if (is.finite(d) && d > 2) prior_scale * sqrt(d / (d - 2)) else NA_real_
  } else {
    prior_scale
  }
  if (length(prior_sd) == 1L && length(prior_scale) > 1L) {
    prior_sd <- rep(prior_sd, length(prior_scale))
  }
  if (length(prior_sd) == 1L) prior_sd <- rep(prior_sd, length(covs))
  if (length(prior_sd) != length(covs)) return(NULL)
  cols <- paste0("beta_comparator[", seq_along(covs), "]")
  if (!all(cols %in% names(object$draws))) return(NULL)
  post_sd <- vapply(cols, function(cl) stats::sd(object$draws[[cl]]), numeric(1))
  ok <- is.finite(prior_sd) & prior_sd > 0
  contraction <- rep(NA_real_, length(covs))
  contraction[ok] <- 1 - (post_sd[ok] / prior_sd[ok])^2
  data.frame(covariate = covs, prior_sd = prior_sd,
             posterior_sd = unname(post_sd), contraction = contraction,
             stringsAsFactors = FALSE, row.names = NULL)
}


#' Note the index-population extrapolation in relaxed fits
#'
#' Emitted from [marginal_effects()] (and the survival dispatch). Once per
#' call; suppress with `options(mlumr.quiet_relaxed_index = TRUE)`.
#' @keywords internal
.relaxed_index_note <- function(object, population) {
  if (!identical(object$model, "relaxed")) return(invisible())
  if (!population %in% c("both", "index")) return(invisible())
  if (isTRUE(getOption("mlumr.quiet_relaxed_index", FALSE))) return(invisible())

  ct <- .relaxed_contraction(object)
  detail <- ""
  if (!is.null(ct) && any(!is.na(ct$contraction))) {
    detail <- paste0(
      " Marginal posterior variance change for `beta_comparator` ",
      "(positive = narrower than its marginal prior, negative = wider): ",
      paste(sprintf("%s %.2f", ct$covariate, ct$contraction), collapse = ", "),
      ".",
      if (any(is.na(ct$contraction))) paste0(
        " (NA where the comparator prior has no finite variance, so the ratio ",
        "is undefined.)"
      ) else "",
      " This is descriptive, not a fraction learned or an identification test."
    )
  }

  message(
    "Relaxed model index-population effects average `beta_comparator` over ",
    "the IPD covariate distribution, while `beta_comparator` is informed only ",
    "by the aggregate likelihood. Whether its components are identified is ",
    "model- and design-dependent, and transport to the index population can ",
    "extrapolate beyond the comparator covariate support.", detail,
    " Inspect the coefficient posterior, report both populations, regularize ",
    "with `prior_beta_comparator` (see ?mlumr), and test conclusions with ",
    "`prior_sensitivity()`. ",
    "Suppress with `options(mlumr.quiet_relaxed_index = TRUE)`."
  )
  invisible()
}


#' Note that some median-survival draws never reach S = 0.5
#'
#' Emitted from [predict.mlumr_fit()] (survival, `type = "median"`) when a
#' positive fraction of posterior draws have an unreached median. Suppress with
#' `options(mlumr.quiet_median_not_reached = TRUE)`.
#' @keywords internal
.median_not_reached_note <- function(max_p) {
  if (isTRUE(getOption("mlumr.quiet_median_not_reached", FALSE))) {
    return(invisible())
  }
  msg <- paste0(
    "Median survival: up to %.0f%% of posterior draws never reach S = 0.5 on ",
    "the prediction grid (median not reached). The reported mean/SD/quantiles ",
    "are conditional on the median being reached; see the `p_not_reached` ",
    "column, refit with an extended `pred_times` grid (the median is searched ",
    "over the fitted grid, so `times` does not affect it), or inspect the ",
    "`summary = FALSE` draws (NA = not reached). Suppress with ",
    "`options(mlumr.quiet_median_not_reached = TRUE)`."
  )
  message(sprintf(msg, 100 * max_p))
  invisible()
}


#' Median survival time from posterior survival-curve draws (linear interp)
#' @keywords internal
.surv_median_from_draws <- function(surv_mat, times) {
  apply(surv_mat, 1, function(s) {
    if (all(s > 0.5)) return(NA_real_)   # median beyond observed follow-up
    k <- which(s <= 0.5)[1]
    if (k == 1L) {
      # The median falls before the first prediction time. times[1] would be an
      # upward-biased bound, not an interpolant, so anchor on the known exact
      # point S(0) = 1 and interpolate between (0, 1) and (times[1], s[1]).
      # s[1] <= 0.5 here, so the result never exceeds times[1].
      return(times[1] * 0.5 / (1 - s[1]))
    }
    s0 <- s[k - 1L]
    s1 <- s[k]
    if (s0 == s1) return(times[k - 1L])
    times[k - 1L] + (0.5 - s0) * (times[k] - times[k - 1L]) / (s1 - s0)
  })
}


#' Marginal treatment effects
#'
#' Extract marginal treatment effects from a fitted ML-UMR model. For
#' binomial: log odds ratio, risk difference, risk ratio. For normal:
#' mean difference. For poisson: rate ratio. For survival: the hazard ratio
#' (proportional-hazards distributions, labeled `HR`), the time ratio
#' (accelerated-failure-time distributions with one shared shape and one shared
#' coefficient vector, labeled `TR`), or the exponentiated linear-predictor
#' contrast (`EXP_DELTA_ETA`, where neither of those holds); all natural-scale,
#' null 1, like the poisson rate ratio. Plus the restricted-mean-survival-time
#' difference (`RMSTD`, null 0) and the RMST ratio (`RMSTR`, null 1), both
#' reported with the restriction time in a `horizon` column. For the
#' time-varying log hazard ratio curve (null 0) use `predict(type = "loghr")`.
#'
#' For survival proportional-hazards models the scalar `"hr"` (the exponentiated
#' `delta_*`) is always a **marginal** quantity, evaluated at one time, and the
#' `at_time` column records which. It is never a conditional coefficient
#' contrast, which is a different estimand and is what [conditional_effects()]
#' returns. Three cases:
#' \itemize{
#'   \item **Shared baseline shape, SPFA.** `delta_*` is the marginal log hazard
#'     ratio in the `t -> 0` limit (`at_time` is 0). Because SPFA gives both
#'     treatments the same coefficients, the covariate term cancels and this
#'     value happens to coincide with the conditional log hazard ratio
#'     `mu_index - mu_comparator`, which IS constant in time and covariates. The
#'     two agree here; they are still different estimands, and they part company
#'     at `t > 0`, where the marginal ratio drifts as the two arms' surviving
#'     covariate distributions diverge.
#'   \item **Shared baseline shape, relaxed.** The coefficients differ by
#'     treatment, so nothing cancels: `delta_*` is the marginal log hazard ratio
#'     at `t -> 0` only, and is time-varying thereafter.
#'   \item **Study-specific shape-bearing baseline** (the `aux_by = ".study"`
#'     default). `delta_*` is taken from the time-varying marginal `loghr_*`
#'     curve at the first prediction time, or at `at_time` when supplied.
#' }
#' Hazard ratios are non-collapsible, so the marginal ratio is time-varying in
#' every case above except the degenerate `t -> 0` evaluation itself. For the
#' full curve use `predict(type = "loghr")`. RMST-based effects
#' (`"rmstd"`, `"rmstr"`) are collapsible within a specified population, but
#' collapsibility alone does not make them invariant or transportable across
#' populations.
#'
#' For accelerated-failure-time distributions the scalar is
#' `exp(E_X[eta_index(X)] - E_X[eta_comparator(X)])`, and what that is depends
#' on the fit:
#' \itemize{
#'   \item **One shared shape, SPFA** (`TR`). The coefficients are shared, so
#'     `eta_index(x) - eta_comparator(x)` is the same constant `a` at every
#'     covariate profile. Every individual's survival time is accelerated by the
#'     same factor, so the population-standardized curves satisfy
#'     `S_index(t) = S_comparator(t / a)` exactly and this IS a population time
#'     ratio.
#'   \item **Otherwise** (`EXP_DELTA_ETA`; differing shapes, or the relaxed
#'     model). The conditional acceleration varies with `x`, so it is the
#'     exponentiated average log ratio: equivalently the conditional time ratio
#'     at the mean linear predictor, or the geometric mean of the
#'     profile-specific conditional time ratios. It is **not** generally a time
#'     ratio between the two standardized survival distributions: there need be
#'     no single `a` with `S_index(t) = S_comparator(t / a)` for all `t`, and
#'     different survival quantiles can imply different apparent acceleration
#'     factors. It is labeled `EXP_DELTA_ETA` rather than `TR` for that reason.
#' }
#' Neither carries an evaluation time (`at_time` is `NA`). For a population
#' contrast under differing covariate effects use the RMST-based effects, which
#' are collapsible and have no such caveat.
#'
#' For binomial fits the `"lor"` measure is always a logit-scale marginal odds
#' ratio computed from response-scale population probabilities, independent of
#' the fitted link. So for a `probit`/`cloglog` fit it is on a different scale
#' than [naive()] / [stc()] `$estimate`, which is the fitted-link (probit /
#' cloglog) difference.
#'
#' **Relaxed-model index-population estimands.** For `model = "relaxed"` the
#' index-population estimand averages `beta_comparator` over the IPD covariate
#' distribution, while `beta_comparator` is identified only by the (typically
#' single) AgD likelihood term and so is integrated outside the support it was
#' identified on. The resulting effects are wider and more prior-sensitive than
#' the comparator-population estimands (which integrate `beta_comparator` over
#' the AgD support, consistent with identification). When `population` is
#' `"both"` or `"index"` for a relaxed fit, `marginal_effects()` emits a
#' one-line note recommending the comparator population, tightening
#' `prior_beta_comparator` (see [mlumr()]), and running [prior_sensitivity()].
#' Suppress the note with `options(mlumr.quiet_relaxed_index = TRUE)`.
#'
#' @param object An `mlumr_fit` object
#' @param population Which population: `"both"` (default), `"index"`, or
#'   `"comparator"`. The **index** population is normally the decision-relevant
#'   target for health technology assessment, since cost-effectiveness models are
#'   built for the population the decision is about; report it as the primary
#'   estimand and the comparator population alongside.
#'   Ignored when `newdata` is supplied (the effect is standardized to the
#'   `newdata` target population instead).
#' @param effect Which effect measure. For binomial: `"all"`, `"lor"`, `"rd"`,
#'   or `"rr"`. For normal: `"all"` or `"md"` (mean difference). For poisson:
#'   `"all"` or `"rr"` (rate ratio). For survival: `"all"`, `"hr"` (hazard ratio
#'   for PH, time ratio for AFT, natural scale, null 1; `"tr"` is an accepted
#'   alias), `"exp_delta_eta"`, `"rmstd"` (RMST difference, null 0), or
#'   `"rmstr"` (RMST ratio, natural scale, null 1). Requesting an effect the fit
#'   cannot produce is an error rather than a differently-named substitute: with
#'   an AFT distribution and `aux_by = ".study"` the study shapes differ, so
#'   `exp(eta_index - eta_comparator)` is not a time ratio and `"hr"` / `"tr"`
#'   are rejected in favor of the explicit `"exp_delta_eta"`.
#' @param at_time Evaluation time for the scalar marginal hazard ratio, for
#'   proportional-hazards fits whose two studies have different baseline shapes
#'   (`aux_by = ".study"` with a distribution that has a shape parameter, or
#'   either flexible baseline). Marginal hazard ratios are non-collapsible and
#'   therefore time-varying, so the scalar is only meaningful with a time
#'   attached; naming it here makes it an estimand choice instead of a
#'   consequence of the `pred_times` output grid. Snapped to the nearest fitted
#'   prediction time, with a message when that is not exact. `NULL` (default)
#'   uses the first prediction time, reproducing `delta_*`. An error for a
#'   shared baseline (where the scalar is the closed-form `t -> 0` limit) and
#'   for AFT fits (whose scalar is a location contrast with no time).
#' @param summary Return summary (`TRUE`) or full draws (`FALSE`)
#' @param probs Quantiles for summary
#' @param newdata Optional data frame of covariate profiles defining an arbitrary
#'   **target population** to transport the effect to (Bayesian g-computation /
#'   model-based standardization over the supplied covariate distribution, as in
#'   Chandler & Ishak Eq 9-10). Each row is one individual / covariate profile;
#'   column names must match the model covariates. When `NULL` (default), effects
#'   are returned for the built-in `index` and/or `comparator` populations from
#'   the Stan generated quantities. When supplied, the marginal effect is
#'   recomputed by averaging model-based predictions over these rows at each
#'   posterior draw, and `population` is ignored. For survival, RMST effects and
#'   the time-specific target-standardized marginal hazard ratio are available;
#'   the latter uses `at_time` (the first fitted prediction time by default).
#'
#' @return A data frame. With `summary = FALSE` the raw posterior draws are
#'   returned as a plain data frame (not plottable; plot methods need
#'   `summary = TRUE`); the column names encode the per-family effect scale
#'   (e.g. poisson `delta_*` is a natural-scale rate ratio, null 1; survival is
#'   the exponentiated HR/TR). With `summary = TRUE` the `effect` column names
#'   the measure; with `summary = FALSE` the scale is carried by the draw column
#'   names themselves (`lor_*`, `rr_*`, `delta_*`, `hr_*` / `tr_*`, `rmst*`).
#'   For survival, RMST-based rows also carry a `horizon` column (the raw-draw
#'   frame, a `horizon` attribute) giving the restriction time the integral runs
#'   to. RMST at different horizons is a different estimand, so results are only
#'   comparable across fits when this value matches.
#'
#'   For survival, the summary carries an `at_time` column and the raw-draw
#'   frame an `at_time` attribute (one named value per column, `NA` for measures
#'   with no evaluation time), so a time-specific hazard ratio never travels
#'   without its time.
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
#' # Transport the effect to an external (e.g. jurisdiction-specific) population
#' marginal_effects(fit, newdata = target_population_covariates)
#'
#' # Full posterior draws rather than summary statistics
#' marginal_effects(fit, summary = FALSE)
#' }
marginal_effects <- function(object,
                             population = c("both", "index", "comparator"),
                             effect = "all",
                             summary = TRUE,
                             probs = c(0.025, 0.5, 0.975),
                             newdata = NULL,
                             at_time = NULL) {

  .validate_mlumr_fit_object(object)
  effect <- .validate_effect_choice(effect)
  summary <- .validate_summary_flag(summary)
  .validate_probs(probs)

  # `at_time` selects the evaluation time of the scalar marginal hazard ratio.
  # It means nothing for a family with no time axis, and nothing for an effect
  # that is an integral to a horizon rather than a value at an instant. Both
  # were silently ignored, and the RMST case additionally emitted the
  # "using the nearest fitted time" message, which reads as though the number
  # reported were taken at that time. Refuse instead of implying it.
  if (!is.null(at_time)) {
    if (!identical(object$family %||% "binomial", "survival")) {
      stop("`at_time` applies to the scalar marginal hazard ratio of a ",
           "survival fit. Family '", object$family %||% "binomial",
           "' has no time axis.", call. = FALSE)
    }
    if (!effect %in% c("all", "hr", "tr", "exp_delta_eta")) {
      stop("`at_time` applies to the scalar marginal hazard ratio, not to ",
           "`effect = \"", effect, "\"`, which is an integral to the RMST ",
           "horizon rather than a value at one time.", call. = FALSE)
    }
  }

  # Transport to an arbitrary target population (g-computation over `newdata`).
  # Isolated from the built-in index/comparator generated-quantities path below.
  # Placed after the `at_time` guards above so the transported route inherits
  # them rather than re-deriving a second, drifting copy.
  if (!is.null(newdata)) {
    return(.marginal_effects_target(object, newdata, effect, summary, probs,
                                    at_time))
  }

  # Only the built-in route reads `population`, and it is documented as ignored
  # above, so it is validated here rather than ahead of the dispatch.
  population <- .validate_predict_choice(population,
                                         c("both", "index", "comparator"),
                                         "population")

  # Relaxed-model index-population: beta_comparator is identified only by the
  # AgD likelihood, so averaging it over the IPD covariate distribution
  # extrapolates outside that support and produces wider, prior-sensitive
  # effects. Surface this once per call (suppress via the documented option).
  .relaxed_index_note(object, population)

  family <- object$family %||% "binomial"

  cfg <- get_family_config(family)

  if (family == "survival") {
    return(.marginal_effects_survival(object, population, effect, summary, probs,
                                      at_time))
  }

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
  # rather than a regex that would collapse both to "delta".
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

  .mlumr_result(cbind(labels, summary_df, row.names = NULL),
                "mlumr_marginal_effects", family = family)
}


#' Target-population standardized response means per treatment (g-computation)
#'
#' Bayesian g-computation / model-based standardization of the per-treatment
#' marginal mean outcome over an arbitrary target population, reusing the
#' validated conditional-effects machinery (`.conditional_*`). For each posterior
#' draw and treatment k, returns
#'   `mu_k = (1/M) sum_m g^{-1}(alpha_k + (x_m - xbar)' beta_k)`
#' averaged over the `M` rows of `newdata` (the target covariate distribution).
#' Non-survival families only.
#' @param object An `mlumr_fit` (binomial / normal / poisson).
#' @param newdata Data frame of target-population covariate profiles.
#' @return A list with `index` and `comparator`, each a length-`n_draws` vector
#'   of target-standardized marginal response means.
#' @keywords internal
.standardize_target_response <- function(object, newdata) {
  family <- object$family %||% "binomial"
  lnk <- object$link %||% get_family_config(family)$link_default
  profiles <- .conditional_profiles(object, newdata)
  x_centered <- profiles$X
  params <- .conditional_parameters(object, profiles$covariates)
  n_target <- nrow(x_centered)

  eta_sum_idx <- 0
  eta_sum_cmp <- 0
  log_idx <- NULL
  log_cmp <- NULL
  log_q_idx <- NULL
  log_q_cmp <- NULL
  for (i in seq_len(n_target)) {
    eta <- .conditional_eta(params, x_centered[i, , drop = FALSE])
    eta_sum_idx <- eta_sum_idx + eta$index
    eta_sum_cmp <- eta_sum_cmp + eta$comparator

    if (family == "binomial") {
      lp_i <- .binary_log_probs(eta$index, lnk)
      lp_c <- .binary_log_probs(eta$comparator, lnk)
      if (is.null(log_idx)) {
        log_idx <- lp_i$event
        log_cmp <- lp_c$event
        log_q_idx <- lp_i$nonevent
        log_q_cmp <- lp_c$nonevent
      } else {
        log_idx <- .logspace_add(log_idx, lp_i$event)
        log_cmp <- .logspace_add(log_cmp, lp_c$event)
        log_q_idx <- .logspace_add(log_q_idx, lp_i$nonevent)
        log_q_cmp <- .logspace_add(log_q_cmp, lp_c$nonevent)
      }
    } else if (lnk == "log") {
      if (is.null(log_idx)) {
        log_idx <- eta$index
        log_cmp <- eta$comparator
      } else {
        log_idx <- .logspace_add(log_idx, eta$index)
        log_cmp <- .logspace_add(log_cmp, eta$comparator)
      }
    }
  }

  if (family == "binomial") {
    log_idx <- log_idx - log(n_target)
    log_cmp <- log_cmp - log(n_target)
    log_q_idx <- log_q_idx - log(n_target)
    log_q_cmp <- log_q_cmp - log(n_target)
    return(list(
      index = exp(log_idx), comparator = exp(log_cmp),
      log_index = log_idx, log_comparator = log_cmp,
      log_nonevent_index = log_q_idx,
      log_nonevent_comparator = log_q_cmp,
      mean_eta_index = eta_sum_idx / n_target,
      mean_eta_comparator = eta_sum_cmp / n_target
    ))
  }
  if (lnk == "log") {
    log_idx <- log_idx - log(n_target)
    log_cmp <- log_cmp - log(n_target)
    return(list(
      index = exp(log_idx), comparator = exp(log_cmp),
      log_index = log_idx, log_comparator = log_cmp,
      mean_eta_index = eta_sum_idx / n_target,
      mean_eta_comparator = eta_sum_cmp / n_target
    ))
  }
  list(
    index = eta_sum_idx / n_target,
    comparator = eta_sum_cmp / n_target,
    mean_eta_index = eta_sum_idx / n_target,
    mean_eta_comparator = eta_sum_cmp / n_target
  )
}


#' Absolute predictions standardized to an arbitrary target population
#'
#' Internal dispatch for [predict.mlumr_fit()] when `newdata` is supplied.
#' g-computation of per-treatment absolute outcomes over the target covariate
#' distribution.
#' @keywords internal
.predict_target <- function(object, newdata, type, summary, probs, times) {
  family <- object$family %||% "binomial"
  idx_trt <- object$data$index_treatment
  cmp_trt <- object$data$comparator_treatment

  if (family == "survival") {
    return(.predict_target_survival(object, newdata, type %||% "survival",
                                    summary, probs, times))
  }

  type <- .validate_predict_choice(type %||% "response",
                                   c("response", "link"), "type")
  lnk <- object$link %||% get_family_config(family)$link_default
  std <- .standardize_target_response(object, newdata)
  if (type == "link") {
    if (family == "binomial") {
      v_idx <- .binary_link_from_logs(std$log_index,
                                      std$log_nonevent_index, lnk)
      v_cmp <- .binary_link_from_logs(std$log_comparator,
                                      std$log_nonevent_comparator, lnk)
    } else if (lnk == "log") {
      v_idx <- std$log_index
      v_cmp <- std$log_comparator
    } else {
      v_idx <- std$index
      v_cmp <- std$comparator
    }
  } else {
    v_idx <- std$index
    v_cmp <- std$comparator
  }
  # Name the raw-draw columns the way the built-in route does, `<prefix>_<arm>_`
  # plus the population, so `newdata` changes which population is reported and
  # not what the columns are called. They were the treatment LABELS, which meant
  # the raw frame's names depended on the user's arm names on one route and not
  # on the other.
  pred_draws <- data.frame(a = v_idx, b = v_cmp)
  colnames(pred_draws) <- paste0(get_family_config(family)$predict_prefix, "_",
                                 c("index", "comparator"), "_target")

  if (!summary) return(pred_draws)

  s <- .summarize_draw_matrix(pred_draws, probs)
  labels <- data.frame(treatment = c(idx_trt, cmp_trt), population = "Target",
                       stringsAsFactors = FALSE)
  .mlumr_result(cbind(labels, s, row.names = NULL),
                "mlumr_prediction", ptype = type, family = family)
}


#' Absolute survival predictions standardized to a target population
#' @keywords internal
.predict_target_survival <- function(object, newdata, type, summary, probs,
                                     times = NULL) {
  type <- .validate_predict_choice(type,
                                   c("survival", "hazard", "cumhaz", "rmst",
                                     "median", "loghr"),
                                   "type")
  # Validate before any branch consults `times`, so an invalid value is refused
  # rather than reported as ignored, matching .predict_survival().
  if (!is.null(times)) times <- .validate_survival_prediction_times(times)
  idx_trt <- object$data$index_treatment
  cmp_trt <- object$data$comparator_treatment
  pred_times <- object$pred_times
  cell_labels <- data.frame(treatment = c(idx_trt, cmp_trt),
                            population = "Target", stringsAsFactors = FALSE)

  # Absolute predictions in an arbitrary target population transport the fitted
  # study-specific baseline shape, same assumption as the built-in populations.
  # `loghr` is exempt for the same reason it is on the built-in route: it is a
  # contrast within one population, so no baseline is carried anywhere.
  if (type != "loghr") .transported_baseline_note(object)

  if (type %in% c("rmst", "median")) {
    # `times` selects points on a curve. RMST is an integral to the fitted
    # horizon and the median is read off the whole fitted grid, so neither
    # consults it. The built-in route refuses rather than ignores, because
    # ignoring let a caller believe they had changed the horizon.
    if (!is.null(times)) {
      stop("`times` selects points on a predicted curve, but `type = \"", type,
           "\"` is a scalar summary of the whole fitted grid and does not use ",
           "it. RMST integrates to the horizon fixed at fit time, and the ",
           "median is searched over `pred_times`; change either by refitting.",
           call. = FALSE)
    }
  }

  if (type == "rmst") {
    tt <- object$stan_data$rmst_grid_times
    sbar <- .standardize_target_survival_s(object, newdata, tt,
                                           object$stan_data$rmst_ibasis,
                                           object$stan_data$rmst_ibasis_cmp)
    values <- list(
      matrix(.rmst_from_surv_matrix(sbar$index, tt), ncol = 1),
      matrix(.rmst_from_surv_matrix(sbar$comparator, tt), ncol = 1)
    )
    tau <- max(tt)
    out <- .surv_result_frame(values, cell_labels, type, summary, probs,
                              horizon = tau)
    if (!summary) return(out)
    return(.mlumr_result(out, "mlumr_prediction", ptype = type,
                         family = "survival", rmst_horizon = tau))
  }

  if (type %in% c("hazard", "loghr")) {
    sel <- .surv_time_selection(times, pred_times)
    # Evaluate only the requested times. Every fitted time was computed for
    # every target row and draw and then discarded, so asking for one time cost
    # roughly `length(pred_times)` times the work it needed.
    log_h <- .standardize_target_survival_log_h(
      object, newdata, pred_times[sel],
      .basis_rows(object$stan_data$pred_ibasis, sel),
      .basis_rows(object$stan_data$pred_ibasis_cmp, sel),
      .basis_rows(object$stan_data$pred_basis, sel),
      .basis_rows(object$stan_data$pred_basis_cmp, sel)
    )
    if (type == "loghr") {
      values <- list(log_h$index - log_h$comparator)
      out <- .surv_result_frame(
        values, data.frame(population = "Target", stringsAsFactors = FALSE),
        type, summary, probs, times_out = pred_times[sel]
      )
      if (!summary) return(out)
      return(.mlumr_result(out, "mlumr_prediction", ptype = type,
                           family = "survival"))
    }
    values <- list(exp(log_h$index), exp(log_h$comparator))
    out <- .surv_result_frame(values, cell_labels, type, summary, probs,
                              times_out = pred_times[sel],
                              origin = .surv_origin(type, times, pred_times))
    if (!summary) return(out)
    return(.mlumr_result(out, "mlumr_prediction", ptype = type,
                         family = "survival"))
  }

  sbar <- .standardize_target_survival_s(object, newdata, pred_times,
                                         object$stan_data$pred_ibasis,
                                         object$stan_data$pred_ibasis_cmp,
                                         log_scale = type == "cumhaz")
  if (type == "median") {
    values <- list(
      matrix(.surv_median_from_draws(sbar$index, pred_times), ncol = 1),
      matrix(.surv_median_from_draws(sbar$comparator, pred_times), ncol = 1)
    )
    out <- .surv_result_frame(values, cell_labels, type, summary, probs)
    if (!summary) return(out)
    return(.mlumr_result(out, "mlumr_prediction", ptype = type,
                         family = "survival"))
  }

  # survival / cumhaz curves: one row per treatment x time, honoring `times`
  # (nearest-grid-point selection, matching the built-in population path).
  sel <- .surv_time_selection(times, pred_times)
  flip <- if (type == "cumhaz") function(m) -m else function(m) m
  values <- list(flip(sbar$index)[, sel, drop = FALSE],
                 flip(sbar$comparator)[, sel, drop = FALSE])
  out <- .surv_result_frame(values, cell_labels, type, summary, probs,
                            times_out = pred_times[sel],
                            origin = .surv_origin(type, times, pred_times))
  if (!summary) return(out)
  .mlumr_result(out, "mlumr_prediction", ptype = type, family = "survival")
}


#' Marginal effects standardized to an arbitrary target population
#'
#' Internal dispatch for [marginal_effects()] when `newdata` is supplied.
#' Transports the marginal effect to the target covariate distribution by
#' g-computation (Chandler & Ishak Eq 9-10). Effect-measure conventions match the
#' built-in populations: binomial `LOR` is the logit-based marginal odds ratio,
#' `RD`/`RR` are natural; normal `MD`; poisson `RR` natural.
#' @keywords internal
.marginal_effects_target <- function(object, newdata, effect, summary, probs,
                                     at_time = NULL) {
  family <- object$family %||% "binomial"
  # A relaxed fit identifies beta_comparator from the aggregate likelihood
  # alone, so standardizing it over an arbitrary target extrapolates at least as
  # far as the index population does. The built-in route says so; this one was
  # silent about the same extrapolation, on every family.
  .relaxed_index_note(object, "index")
  if (family == "survival") {
    return(.marginal_effects_target_survival(object, newdata, effect, summary,
                                             probs, at_time))
  }

  cfg <- get_family_config(family)
  valid_effects <- c("all", cfg$effect_measures)
  if (!effect %in% valid_effects) {
    stop(sprintf("For %s family, `effect` must be one of: %s",
                 family, paste(valid_effects, collapse = ", ")), call. = FALSE)
  }

  std <- .standardize_target_response(object, newdata)
  mu_i <- std$index
  mu_c <- std$comparator

  # Name the raw-draw columns the way the built-in route does, with `_target`
  # in place of `_index` / `_comparator`, and carry the same `variable` column
  # into the summary. They were bare measure labels (`LOR`, `RD`, `RR`), so
  # `$lor_index` became NULL and the summary lost a column the moment a caller
  # added `newdata` to an otherwise identical call.
  target_var <- function(measure) {
    v <- cfg$marginal_effect_vars[[measure]][1]
    paste0(sub("_(index|comparator)$", "", v), "_target")
  }
  push <- function(spec, measure, draws) {
    c(spec, list(list(variable = target_var(measure),
                      effect = toupper(measure), draws = draws)))
  }
  spec <- list()
  if (family == "binomial") {
    if (effect %in% c("all", "lor")) {
      spec <- push(spec, "lor", (std$log_index - std$log_nonevent_index) -
                     (std$log_comparator - std$log_nonevent_comparator))
    }
    if (effect %in% c("all", "rd")) {
      spec <- push(spec, "rd",
                   .exp_difference_logs(std$log_index, std$log_comparator))
    }
    if (effect %in% c("all", "rr")) {
      spec <- push(spec, "rr", exp(std$log_index - std$log_comparator))
    }
  } else if (family == "normal") {
    spec <- push(spec, "md", if (identical(object$link, "log")) {
      .exp_difference_logs(std$log_index, std$log_comparator)
    } else {
      mu_i - mu_c
    })
  } else {
    spec <- push(spec, "rr", exp(std$log_index - std$log_comparator))
  }
  effect_draws <- as.data.frame(
    stats::setNames(lapply(spec, function(x) x$draws),
                    vapply(spec, function(x) x$variable, character(1))),
    check.names = FALSE
  )

  if (!summary) return(effect_draws)

  summary_df <- .summarize_draw_matrix(effect_draws, probs)
  labels <- data.frame(
    variable = vapply(spec, function(x) x$variable, character(1)),
    effect = vapply(spec, function(x) x$effect, character(1)),
    population = "Target",
    stringsAsFactors = FALSE
  )
  out <- cbind(labels, summary_df, row.names = NULL)
  .mlumr_result(out, "mlumr_marginal_effects", family = family)
}


#' Select the rows of a spline basis that match selected prediction times
#'
#' The basis matrices carry one row per fitted time, so a time and its row are
#' chosen together. `NULL` for a parametric fit, which evaluates analytically
#' and has no basis.
#' @keywords internal
.basis_rows <- function(basis, idx) {
  if (is.null(basis)) return(NULL)
  basis[idx, , drop = FALSE]
}

#' Survival S(t | x) at arbitrary times for one linear-predictor draw vector
#'
#' Generalizes [.surv_eval_curve()] to an arbitrary `times` grid with matching
#' I-spline integral basis `ibasis` (used for the M-spline/piecewise baseline;
#' ignored for parametric distributions, which evaluate `S` analytically).
#' @keywords internal
.surv_s_at_times <- function(object, eta, times, ibasis,
                             treatment = c("index", "comparator"),
                             log_scale = FALSE) {
  treatment <- match.arg(treatment)
  draws <- object$draws
  # The baseline belongs to the study, and each study contributes one arm, so it
  # travels with the treatment. Under `aux_by = NULL` there is only one stratum
  # and both treatments read the same parameters.
  log_s <- if (object$surv_info$kind == "parametric") {
    dist <- object$surv_info$dist_code
    aux  <- .surv_aux_draws(object, "aux_val", treatment, length(eta))
    aux2 <- .surv_aux_draws(object, "aux2_val", treatment, length(eta))
    matrix(
      vapply(times,
             function(t) .r_log_surv(dist, t, eta, aux, aux2),
             numeric(length(eta))),
      nrow = length(eta), ncol = length(times)
    )
  } else {
    scoef <- .surv_scoef_draws(object, treatment)
    cum_haz <- scoef %*% t(ibasis)             # [n_draws, n_times]
    -exp(log(cum_haz) + eta)                    # eta recycled down columns
  }
  if (log_scale) log_s else exp(log_s)
}

#' Spline coefficient draws for one treatment's baseline
#'
#' `scoef` is a `[n_scoef, n_strata]` matrix in Stan, so the draws are named
#' `scoef[j,s]`. Stratum 1 is the index study and stratum `n_strata` is the
#' comparator, which coincide when the baseline is shared. Older fits stored a
#' plain vector named `scoef[j]`; those are still readable.
#' @keywords internal
.surv_scoef_draws <- function(object, treatment = c("index", "comparator")) {
  treatment <- match.arg(treatment)
  draws <- object$draws
  n_scoef <- object$stan_data$n_scoef
  n_strata <- object$stan_data$n_strata %||% 1L
  j <- seq_len(n_scoef)
  # Three layouts, newest first:
  #   scoef_idx[j] / scoef_cmp[j]  the named per-treatment views (always emitted)
  #   scoef[j,s]                   the underlying matrix
  #   scoef[j]                     fits made before aux_by existed
  view <- if (identical(treatment, "index")) "scoef_idx" else "scoef_cmp"
  s <- if (identical(treatment, "index")) 1L else n_strata
  for (nm in list(paste0(view, "[", j, "]"),
                  paste0("scoef[", j, ",", s, "]"),
                  paste0("scoef[", j, "]"))) {
    if (all(nm %in% names(draws))) return(as.matrix(draws[, nm, drop = FALSE]))
  }
  stop("Could not find spline coefficient draws for the ", treatment,
       " baseline.", call. = FALSE)
}

#' Auxiliary (shape) draws for one treatment, defaulting to 1 when absent
#' @keywords internal
.surv_aux_draws <- function(object, base, treatment, n) {
  draws <- object$draws
  cmp <- paste0(base, "_cmp")
  nm <- if (identical(treatment, "comparator") && cmp %in% names(draws)) cmp else base
  if (nm %in% names(draws)) draws[[nm]] else rep(1, n)
}


#' Target-population standardized survival curve S-bar(t) per treatment
#'
#' g-computation of the population-average survival function over an arbitrary
#' target population (Chandler & Ishak Eq 14): for each treatment,
#' `S_bar_k(t) = (1/M) sum_m S_k(t | x_m)` over the `M` rows of `newdata`.
#' @return A list with `index` and `comparator`, each an `[n_draws, length(times)]`
#'   matrix of target-standardized survival probabilities.
#' @keywords internal
.standardize_target_survival_s <- function(object, newdata, times, ibasis,
                                           ibasis_cmp = NULL,
                                           log_scale = FALSE) {
  profiles <- .conditional_profiles(object, newdata)
  x_centered <- profiles$X
  params <- .conditional_parameters(object, profiles$covariates)
  n_target <- nrow(x_centered)

  # With `aux_by = ".study"` each study has its own spline basis, so the
  # comparator arm must be evaluated on the comparator's basis. Fits made before
  # per-study bases carry no `_cmp` matrix; fall back to the shared one.
  ibasis_cmp <- ibasis_cmp %||% ibasis

  s_idx <- NULL
  s_cmp <- NULL
  for (i in seq_len(n_target)) {
    eta <- .conditional_eta(params, x_centered[i, , drop = FALSE])
    si <- .surv_s_at_times(object, eta$index, times, ibasis, "index", log_scale)
    sc <- .surv_s_at_times(object, eta$comparator, times, ibasis_cmp,
                           "comparator", log_scale)
    if (is.null(s_idx)) {
      s_idx <- si
      s_cmp <- sc
    } else if (log_scale) {
      s_idx <- .logspace_add(s_idx, si)
      s_cmp <- .logspace_add(s_cmp, sc)
    } else {
      s_idx <- s_idx + si
      s_cmp <- s_cmp + sc
    }
  }
  if (log_scale) {
    list(index = s_idx - log(n_target), comparator = s_cmp - log(n_target))
  } else {
    list(index = s_idx / n_target, comparator = s_cmp / n_target)
  }
}


#' Conditional log hazard at arbitrary times for one predictor draw vector
#' @keywords internal
.surv_log_h_at_times <- function(object, eta, times, mbasis = NULL,
                                 treatment = c("index", "comparator")) {
  treatment <- match.arg(treatment)
  if (object$surv_info$kind == "parametric") {
    dist <- object$surv_info$dist_code
    aux <- .surv_aux_draws(object, "aux_val", treatment, length(eta))
    aux2 <- .surv_aux_draws(object, "aux2_val", treatment, length(eta))
    return(matrix(
      vapply(times,
             function(t) .r_log_haz(dist, t, eta, aux, aux2),
             numeric(length(eta))),
      nrow = length(eta), ncol = length(times)
    ))
  }

  scoef <- .surv_scoef_draws(object, treatment)
  h0 <- scoef %*% t(mbasis)
  log(h0) + eta
}


#' Conditional log density at arbitrary times for one predictor draw vector
#' @keywords internal
.surv_log_f_at_times <- function(object, eta, times, ibasis = NULL,
                                 mbasis = NULL,
                                 treatment = c("index", "comparator")) {
  treatment <- match.arg(treatment)
  if (object$surv_info$kind == "parametric") {
    dist <- object$surv_info$dist_code
    aux <- .surv_aux_draws(object, "aux_val", treatment, length(eta))
    aux2 <- .surv_aux_draws(object, "aux2_val", treatment, length(eta))
    return(matrix(
      vapply(times,
             function(t) .r_log_density(dist, t, eta, aux, aux2),
             numeric(length(eta))),
      nrow = length(eta), ncol = length(times)
    ))
  }

  log_s <- .surv_s_at_times(object, eta, times, ibasis, treatment,
                            log_scale = TRUE)
  log_h <- .surv_log_h_at_times(object, eta, times, mbasis, treatment)
  out <- log_s + log_h
  out[is.nan(out) & is.infinite(log_s) & log_s < 0] <- -Inf
  out
}


#' Target-standardized marginal log hazard by treatment and time
#'
#' Uses the equivalent definition `E(f) / E(S)`, accumulating log density and log
#' survival separately so opposite infinities are never added.
#' @keywords internal
.standardize_target_survival_log_h <- function(object, newdata, times,
                                               ibasis = NULL,
                                               ibasis_cmp = NULL,
                                               mbasis = NULL,
                                               mbasis_cmp = NULL) {
  profiles <- .conditional_profiles(object, newdata)
  x_centered <- profiles$X
  params <- .conditional_parameters(object, profiles$covariates)
  ibasis_cmp <- ibasis_cmp %||% ibasis
  mbasis_cmp <- mbasis_cmp %||% mbasis
  prefer_lower_eta <- isTRUE(object$surv_info$is_ph)

  update_state <- function(old, log_s, log_f, log_h, eta) {
    log_s <- matrix(log_s, nrow = length(eta), ncol = length(times))
    log_f <- matrix(log_f, nrow = length(eta), ncol = length(times))
    log_h <- matrix(log_h, nrow = length(eta), ncol = length(times))
    eta_mat <- matrix(eta, nrow = length(eta), ncol = length(times))
    if (is.null(old)) {
      return(list(max = log_s,
                  den = array(0, dim(log_s)),
                  num = log_h,
                  direct_num = log_f,
                  best_h = log_h,
                  best_eta = eta_mat))
    }
    new_max <- pmax(old$max, log_s)
    old_shift <- old$max - new_max
    new_shift <- log_s - new_max
    both_inf <- is.infinite(old$max) & old$max < 0 &
      is.infinite(log_s) & log_s < 0
    if (any(both_inf)) {
      new_better <- if (prefer_lower_eta) {
        eta_mat < old$best_eta
      } else {
        eta_mat > old$best_eta
      }
      tied <- eta_mat == old$best_eta
      old_shift[both_inf & new_better] <- -Inf
      new_shift[both_inf & new_better] <- 0
      old_shift[both_inf & !new_better] <- 0
      new_shift[both_inf & !new_better & !tied] <- -Inf
      new_shift[both_inf & tied] <- 0
    }
    new_better <- if (prefer_lower_eta) {
      eta_mat < old$best_eta
    } else {
      eta_mat > old$best_eta
    }
    old_num <- old$num + old_shift
    new_num <- log_h + new_shift
    old_bad <- is.nan(old_num)
    new_bad <- is.nan(new_num)
    old_num[old_bad] <- old$direct_num[old_bad] - new_max[old_bad]
    new_num[new_bad] <- log_f[new_bad] - new_max[new_bad]
    list(
      max = new_max,
      den = .logspace_add(old$den + old_shift, new_shift),
      num = .logspace_add(old_num, new_num),
      direct_num = .logspace_add(old$direct_num, log_f),
      best_h = ifelse(new_better, log_h, old$best_h),
      best_eta = if (prefer_lower_eta) pmin(old$best_eta, eta_mat) else
        pmax(old$best_eta, eta_mat)
    )
  }

  state_i <- NULL
  state_c <- NULL
  for (i in seq_len(nrow(x_centered))) {
    eta <- .conditional_eta(params, x_centered[i, , drop = FALSE])
    log_si <- .surv_s_at_times(object, eta$index, times, ibasis,
                               "index", log_scale = TRUE)
    log_sc <- .surv_s_at_times(object, eta$comparator, times, ibasis_cmp,
                               "comparator", log_scale = TRUE)
    log_hi <- .surv_log_h_at_times(object, eta$index, times, mbasis, "index")
    log_hc <- .surv_log_h_at_times(object, eta$comparator, times, mbasis_cmp,
                                   "comparator")
    log_fi <- .surv_log_f_at_times(object, eta$index, times, ibasis, mbasis,
                                   "index")
    log_fc <- .surv_log_f_at_times(object, eta$comparator, times, ibasis_cmp,
                                   mbasis_cmp, "comparator")
    state_i <- update_state(state_i, log_si, log_fi, log_hi, eta$index)
    state_c <- update_state(state_c, log_sc, log_fc, log_hc, eta$comparator)
  }
  finish <- function(state) {
    out <- state$num - state$den
    both_tail <- is.infinite(state$max) & state$max < 0
    out[both_tail] <- state$best_h[both_tail]
    out
  }
  list(index = finish(state_i), comparator = finish(state_c))
}


#' RMST per draw from a target-standardized survival curve (trapezoid)
#' @keywords internal
.rmst_from_surv_matrix <- function(s_mat, times) {
  dt <- diff(times)
  # trapezoid: sum_j (S[,j] + S[,j+1]) / 2 * (t[j+1] - t[j])
  left <- s_mat[, -ncol(s_mat), drop = FALSE]
  right <- s_mat[, -1, drop = FALSE]
  as.numeric(((left + right) / 2) %*% dt)
}


#' Marginal survival effects standardized to an arbitrary target population
#'
#' Internal dispatch for [marginal_effects()] (survival) when `newdata` is
#' supplied. RMST effects are computed from the target-standardized survival
#' curve. For proportional-hazards fits, `"hr"` is the time-specific marginal
#' hazard ratio obtained from the survival-weighted hazards in that same target.
#' RMST differences are directly collapsible, but no effect measure is assumed
#' to be invariant across populations merely because it is collapsible.
#' @keywords internal
.marginal_effects_target_survival <- function(object, newdata, effect, summary,
                                              probs, at_time = NULL) {
  is_ph <- isTRUE(object$surv_info$is_ph)
  # Whether the two studies genuinely have DIFFERENT baseline shapes. This is
  # the same gate the built-in route uses, and it decides the ESTIMAND, not just
  # the presentation: with shared shapes the baseline cancels and the marginal
  # hazard ratio has a closed form; with differing shapes it can only be read
  # off the fitted grid.
  stratified <- .aux_shapes_differ(object)
  # `tr` names the same scalar contrast as `hr` on the built-in route, which
  # accepts it as an alias. Refusing it here made the same request succeed or
  # fail depending only on whether `newdata` was supplied.
  valid_effects <- if (is_ph) c("all", "hr", "tr", "rmstd", "rmstr") else
    c("all", "rmstd", "rmstr")
  if (!effect %in% valid_effects) {
    stop(sprintf(
      "For this survival fit and a `newdata` target, `effect` must be one of: %s.",
      paste(valid_effects, collapse = ", ")
    ), call. = FALSE)
  }
  if (effect == "tr") effect <- "hr"
  if (!is.null(at_time) && !is_ph) {
    stop("`at_time` applies to a time-specific marginal hazard ratio, but this ",
         "fit uses an accelerated-failure-time distribution. Use RMST effects ",
         "for target-standardized comparisons.", call. = FALSE)
  }

  .transported_baseline_note(object)

  times <- object$stan_data$rmst_grid_times
  # RMST is an integral to a restriction time; carry it with the value so a
  # transported result cannot be compared against one computed to a different
  # horizon without the difference being visible. The horizon comes off the
  # grid, so it costs nothing even when no RMST effect is requested.
  rmst_tau <- max(times)
  # Standardizing over the whole RMST grid costs target rows x draws x
  # n_rmst_grid (100 points by default) and is thrown away for an HR-only
  # request, which needs either the linear predictors or one selected time.
  want_rmst <- effect %in% c("all", "rmstd", "rmstr")
  rmst_i <- NULL
  rmst_c <- NULL
  if (want_rmst) {
    sbar <- .standardize_target_survival_s(object, newdata, times,
                                           object$stan_data$rmst_ibasis,
                                           object$stan_data$rmst_ibasis_cmp)
    rmst_i <- .rmst_from_surv_matrix(sbar$index, times)
    rmst_c <- .rmst_from_surv_matrix(sbar$comparator, times)
  }

  spec <- list()
  if (is_ph && effect %in% c("all", "hr")) {
    if (stratified) {
      grid <- object$pred_times
      requested <- at_time %||% grid[1]
      if (!is.numeric(requested) || length(requested) != 1L ||
            !is.finite(requested) || requested <= 0) {
        stop("`at_time` must be a single finite positive time.", call. = FALSE)
      }
      p <- which.min(abs(grid - requested))
      used_time <- grid[p]
      if (!isTRUE(all.equal(used_time, requested))) {
        message("`at_time = ", format(requested, digits = 4L), "` is not a fitted ",
                "prediction time; using the nearest one, t = ",
                format(used_time, digits = 4L), ".")
      }
      # One time is wanted, so evaluate one: the basis matrices have one row
      # per fitted time, so the row and the time are selected together.
      log_h <- .standardize_target_survival_log_h(
        object, newdata, grid[p],
        .basis_rows(object$stan_data$pred_ibasis, p),
        .basis_rows(object$stan_data$pred_ibasis_cmp, p),
        .basis_rows(object$stan_data$pred_basis, p),
        .basis_rows(object$stan_data$pred_basis_cmp, p)
      )
      hr_draws <- exp(log_h$index[, 1] - log_h$comparator[, 1])
    } else {
      # Shared baseline shape: the shape cancels from the ratio, so the marginal
      # hazard ratio is the `t -> 0` limit E[exp(eta_index)] / E[exp(eta_cmp)]
      # over the target rows, which is the closed form Stan writes into
      # `delta_*`. Reading it off the grid instead would return the
      # survival-weighted ratio at pred_times[1], so standardizing to the IPD
      # covariates would NOT reproduce `population = "index"` even though it is
      # the same calculation over the same distribution.
      if (!is.null(at_time) && !isTRUE(all.equal(at_time, 0))) {
        stop("`at_time` applies only when the two studies have different ",
             "baseline shapes. With a shared baseline the marginal hazard ",
             "ratio is the closed-form t -> 0 limit and is not evaluated on ",
             "the prediction grid; use predict(type = \"loghr\", newdata = ) ",
             "for the curve.", call. = FALSE)
      }
      hr_draws <- exp(.target_loghr_origin(object, newdata))
      used_time <- 0
    }
    spec[[length(spec) + 1L]] <- list(
      variable = "hr_target", effect = "HR", population = "Target",
      at_time = used_time, horizon = NA_real_, draws = hr_draws
    )
  }
  if (effect %in% c("all", "rmstd")) {
    spec[[length(spec) + 1L]] <- list(
      variable = "rmst_diff_target", effect = "RMSTD", population = "Target",
      at_time = NA_real_, horizon = rmst_tau, draws = rmst_i - rmst_c
    )
  }
  if (effect %in% c("all", "rmstr")) {
    spec[[length(spec) + 1L]] <- list(
      variable = "rmst_ratio_target", effect = "RMSTR", population = "Target",
      at_time = NA_real_, horizon = rmst_tau, draws = rmst_i / rmst_c
    )
  }

  .surv_effect_frame(spec, summary, probs, rmst_tau)
}

#' Marginal log hazard ratio in a target population at the origin
#'
#' With a shared baseline shape the shape cancels from the ratio of marginal
#' hazards as `t -> 0`, leaving
#' `log E[exp(eta_index)] - log E[exp(eta_comparator)]` over the target rows.
#' This is the same closed form the Stan models use for `delta_*`, so a target
#' equal to the IPD covariates reproduces `population = "index"` exactly. The
#' equal row counts cancel, so no `1/n` appears.
#' @keywords internal
.target_loghr_origin <- function(object, newdata) {
  profiles <- .conditional_profiles(object, newdata)
  params <- .conditional_parameters(object, profiles$covariates)
  x_centered <- profiles$X
  li <- NULL
  lc <- NULL
  for (i in seq_len(nrow(x_centered))) {
    eta <- .conditional_eta(params, x_centered[i, , drop = FALSE])
    li <- if (is.null(li)) eta$index else .logspace_add(li, eta$index)
    lc <- if (is.null(lc)) eta$comparator else .logspace_add(lc, eta$comparator)
  }
  li - lc
}


#' Marginal survival treatment effects (internal dispatch for marginal_effects)
#'
#' Returns the marginal hazard ratio (PH) / time ratio (AFT) on the natural
#' scale (null 1), the RMST difference (null 0), and the natural-scale RMST
#' ratio (null 1), in the index and/or comparator populations, matching the
#' estimands of Chandler & Ishak (ML-UMR survival). The Stan `delta_*` are log
#' HR / log time ratios and are exponentiated here.
#' @keywords internal
.marginal_effects_survival <- function(object, population, effect, summary,
                                       probs, at_time = NULL) {
  draws <- object$draws
  is_ph <- isTRUE(object$surv_info$is_ph)
  # Whether the two studies genuinely have DIFFERENT baseline shapes, which is
  # not the same question as `n_strata > 1`: an exponential has no shape to
  # stratify, so `aux_by = ".study"` leaves its baseline unchanged and Stan
  # keeps the exact closed form. `.aux_shapes_differ()` mirrors the Stan gate,
  # so the time this layer attaches is the time Stan actually evaluated at.
  stratified <- .aux_shapes_differ(object)
  # marginal_effects() reports the hazard ratio on the natural scale (null 1),
  # consistent with the poisson rate ratio: the Stan `delta_*` are log HR (PH) /
  # log time ratio (AFT) and are exponentiated here. The label is `HR` (PH) or
  # `TR` (AFT). For the time-varying log hazard ratio curve (null 0) use
  # predict(type = "loghr").
  # A stratified AFT baseline gives the two studies different shape/scale
  # parameters, and then exp(delta_eta) is NOT a time ratio: the Weibull
  # quantile ratio picks up [-log S]^(1/a_i - 1/a_c), the log-normal picks up
  # exp(z_p (sigma_i - sigma_c)), and so on. Only the location contrast is
  # left, so it must not be labeled TR. (The PH side is genuinely a marginal
  # hazard ratio, taken from loghr_* at `at_time`.)
  # One shared derivation for the label and its evaluation time, so this and
  # prior_sensitivity() cannot drift apart. `EXP_DELTA_ETA` now also covers the
  # RELAXED AFT case: treatment-specific coefficients mean the covariate term
  # does not cancel, so exp(delta) is the geometric mean of covariate-specific
  # time ratios, not one population acceleration factor.
  lab <- .surv_scalar_label(object)
  hr_label <- lab$label
  # The estimand the caller asked for must be the estimand they get. When the
  # AFT shapes differ there is no scalar time ratio, and returning the location
  # contrast under the name `tr` would report one estimand under the name of
  # another. Name the quantity that does exist and make the caller opt into it.
  if (effect %in% c("hr", "tr") && identical(hr_label, "EXP_DELTA_ETA")) {
    why <- if (stratified) {
      paste0("each study has its own AFT shape (`aux_by = \".study\"`), so there ",
             "is no constant acceleration factor at all (the Weibull quantile ",
             "ratio picks up [-log S]^(1/a_i - 1/a_c), the log-normal ",
             "exp(z_p (sigma_i - sigma_c)), and so on)")
    } else {
      paste0("this is a relaxed fit, so the two treatments have different ",
             "coefficients and the covariate term does not cancel from ",
             "mean(eta_index) - mean(eta_comparator). The time ratio varies by ",
             "covariate profile, and exp(delta) is the geometric mean of those ",
             "profile-specific ratios, not one population acceleration factor")
    }
    stop("`effect = \"", effect, "\"` is not available: ", why,
         ". Use `effect = \"exp_delta_eta\"` for the location contrast itself, ",
         "the collapsible `effect = \"rmstd\"` / \"rmstr\", or ",
         "conditional_effects() for profile-specific time ratios.",
         call. = FALSE)
  }
  if (identical(effect, "exp_delta_eta") && !identical(hr_label, "EXP_DELTA_ETA")) {
    stop("`effect = \"exp_delta_eta\"` applies to an AFT distribution whose ",
         "shapes differ by study (`aux_by = \".study\"`) or to any relaxed AFT ",
         "fit, whose treatment-specific coefficients leave the covariate term ",
         "in the contrast. This fit reports ", hr_label,
         "; request `effect = \"", tolower(hr_label), "\"`.", call. = FALSE)
  }

  # `tr` (time ratio) is accepted as an alias for `hr`: for AFT distributions the
  # returned effect is already labeled TR (see hr_label), so a user who thinks in
  # time-ratio terms can request it by name; it routes to the same computation.
  valid_effects <- c("all", "hr", "tr", "exp_delta_eta", "rmstd", "rmstr")
  if (!effect %in% valid_effects) {
    stop(sprintf("For survival family, `effect` must be one of: %s",
                 paste(valid_effects, collapse = ", ")), call. = FALSE)
  }
  # `tr` and `exp_delta_eta` both name the exponentiated location contrast that
  # `hr` computes; the LABEL on the result (HR / TR / EXP_DELTA_ETA) is what
  # says which of the three it actually is, and the guards above have already
  # rejected the combinations where the requested name is not the truth.
  if (effect %in% c("tr", "exp_delta_eta")) effect <- "hr"
  # Resolve the evaluation time of the scalar marginal hazard ratio. This is a
  # genuine estimand choice, not a plotting control, so it is a named argument
  # rather than a side effect of `pred_times`. Stan already stores the whole
  # `loghr_*[p]` curve, so any fitted grid time is available at no cost; the
  # default reproduces `delta_*` exactly.
  hr_index_p <- NULL
  hr_at <- NULL
  if (!is.null(at_time)) {
    if (!is_ph) {
      stop("`at_time` applies only to the marginal hazard ratio of a ",
           "proportional-hazards distribution. This fit is AFT, whose scalar ",
           "effect is a location contrast with no evaluation time.",
           call. = FALSE)
    }
    if (!is.numeric(at_time) || length(at_time) != 1L || !is.finite(at_time) ||
          at_time < 0) {
      stop("`at_time` must be a single finite non-negative time.",
           call. = FALSE)
    }
    if (!stratified && !isTRUE(all.equal(at_time, 0))) {
      stop("`at_time` applies only when the two studies have different ",
           "baseline shapes. With a shared baseline the scalar HR is the ",
           "closed-form t -> 0 marginal limit, so the only evaluation time it ",
           "accepts is 0; use predict(type = \"loghr\") for the curve.",
           call. = FALSE)
    }
    if (stratified && at_time <= 0) {
      stop("`at_time` must be a single finite positive time when the two ",
           "studies have different baseline shapes. The prediction grid ",
           "excludes 0, so a non-positive value has no nearest fitted time ",
           "to snap to.", call. = FALSE)
    }
    if (stratified) {
      # Only the stratified branch reads a time off the grid. With shared
      # shapes the scalar is the closed-form limit and `hr_index_p` stays NULL,
      # which is what selects `delta_*` below.
      grid <- object$pred_times
      hr_index_p <- which.min(abs(grid - at_time))
      hr_at <- grid[hr_index_p]
      if (!isTRUE(all.equal(hr_at, at_time))) {
        message("`at_time = ", format(at_time, digits = 4L), "` is not a ",
                "fitted prediction time; using the nearest one, t = ",
                format(hr_at, digits = 4L),
                ". Refit with that time in `pred_times` for an exact match.")
      }
    }
  }

  pops <- switch(population, index = "index", comparator = "comparator",
                 both = c("index", "comparator"))
  effs <- if (effect == "all") c("hr", "rmstd", "rmstr") else effect

  # The scalar HR is the marginal hazard ratio at the start of follow-up. Hazard
  # ratios are non-collapsible, so the marginal HR drifts with time in BOTH
  # models as the surviving covariate distributions of the two arms diverge; the
  # relaxed model adds effect modification on top. Flag it once per session for
  # either model, naming the relaxed extra where it applies.
  if (is_ph && "hr" %in% effs && !isTRUE(getOption("mlumr.marginal_hr_note"))) {
    extra <- if ((object$model %||% "spfa") == "relaxed") {
      " and additionally under the relaxed model's treatment-specific covariate effects"
    } else {
      ""
    }
    where <- if (stratified) {
      paste0("t = ", format(hr_at %||% object$pred_times[1], digits = 4L),
             if (is.null(hr_at)) {
               paste0(", the first prediction time, because the prediction grid ",
                      "excludes 0. Pass `at_time` to choose the evaluation time ",
                      "explicitly rather than inheriting it from `pred_times`")
             } else {
               ", the requested evaluation time"
             })
    } else {
      "the start of follow-up (the t -> 0 limit)"
    }
    message("Note: the scalar 'HR' is the marginal hazard ratio at ", where,
            ". The `at_time` column records it. ",
            "Hazard ratios are non-collapsible, so the marginal ",
            "hazard ratio varies over time as the surviving covariate ",
            "distributions of the two arms diverge", extra, ". Use ",
            "predict(type = \"loghr\") for the time-varying log hazard ratio ",
            "curve, or the collapsible RMST effects ",
            "(effect = \"rmstd\" / \"rmstr\").")
    options(mlumr.marginal_hr_note = TRUE)
  }

  # The restriction time the RMST effects integrate to. Read off the fitted grid
  # rather than the requested `rmst_horizon`, so it is right whether the horizon
  # was supplied or left to mlumr()'s default (common support for a
  # study-stratified flexible baseline, pooled maximum otherwise).
  rmst_tau <- {
    g <- object$stan_data$rmst_grid_times
    if (is.null(g)) NA_real_ else max(g)
  }

  spec <- list()
  for (eff in effs) {
    for (pop in pops) {
      if (eff == "hr") {
        # Natural-scale hazard ratio (PH) / time ratio (AFT), null 1: exponentiate
        # the Stan log HR / log time ratio.
        d <- if (is.null(hr_index_p)) {
          draws[[sprintf("delta_%s", pop)]]
        } else {
          # The time-varying marginal log hazard ratio Stan already computed at
          # every fitted prediction time. delta_* is exactly this curve's first
          # element under a stratified baseline, so the default path and this
          # one agree when at_time = pred_times[1].
          draws[[sprintf("loghr_%s[%d]", pop, hr_index_p)]]
        }
        vec <- if (is.null(d)) NULL else exp(d)
        # Name the raw-draw column to match the effect scale, so summary = FALSE
        # output is self-describing. `tr_*` would be a lie exactly where
        # hr_label has already established the quantity is NOT a time ratio, so
        # the raw name tracks the label rather than just the PH flag.
        vname <- sprintf("%s_%s", switch(hr_label,
                                         HR = "hr",
                                         TR = "tr",
                                         EXP_DELTA_ETA = "exp_delta_eta"), pop)
        elabel <- hr_label
      } else if (eff == "rmstd") {
        vec <- draws[[sprintf("rmst_diff_%s", pop)]]
        vname <- sprintf("rmst_diff_%s", pop)
        elabel <- "RMSTD"
      } else {
        # Natural-scale RMST ratio (index / comparator), null 1.
        ri <- draws[[sprintf("rmst_index_%s", pop)]]
        rc <- draws[[sprintf("rmst_comparator_%s", pop)]]
        vec <- if (is.null(ri) || is.null(rc)) NULL else ri / rc
        vname <- sprintf("rmst_ratio_%s", pop)
        elabel <- "RMSTR"
      }
      if (is.null(vec)) {
        stop("Required survival draws not found; refit with mlumr (>= 0.2.0).",
             call. = FALSE)
      }
      # The MARGINAL hazard ratio is time-varying in BOTH models, because it
      # weights the covariate distribution by each arm's own survival and the
      # risk sets diverge. So the scalar always needs the time it belongs to:
      #   shared shapes -> the closed form is the t -> 0 limit, so 0;
      #   differing     -> taken from loghr_* at the requested (or first) time.
      # NA only for AFT location contrasts and the collapsible RMST effects,
      # neither of which has an evaluation time.
      eff_at_time <- if (eff != "hr") {
        NA_real_
      } else if (is_ph && stratified) {
        hr_at %||% lab$at_time
      } else {
        lab$at_time
      }
      # RMST is an integral to a restriction time, so a value without its
      # horizon is not an estimand. Two fits with different default horizons
      # produce numbers that must not be put on the same forest plot, and
      # nothing in the output said so.
      eff_horizon <- if (eff %in% c("rmstd", "rmstr")) rmst_tau else NA_real_
      spec[[length(spec) + 1L]] <- list(
        variable = vname, effect = elabel,
        population = if (pop == "index") "Index" else "Comparator",
        at_time = eff_at_time,
        horizon = eff_horizon,
        draws = vec
      )
    }
  }

  .surv_effect_frame(spec, summary, probs, rmst_tau)
}

#' Assemble a survival marginal-effects frame from per-column draw records
#'
#' `spec` is a list with one entry per effect column, each carrying `variable`
#' (the raw-draw column name), `effect` (the display label), `population`,
#' `at_time`, `horizon` and the `draws` vector. Both the built-in and the
#' transported route reduce to that list, so the layout below is written once:
#' writing it twice is how the two came to disagree about column names and
#' about which of `at_time` / `horizon` appear at all.
#' @keywords internal
.surv_effect_frame <- function(spec, summary, probs, rmst_horizon) {
  mat <- do.call(cbind, lapply(spec, function(s) s$draws))
  colnames(mat) <- vapply(spec, function(s) s$variable, character(1))
  at_time <- vapply(spec, function(s) s$at_time, numeric(1))
  horizon <- vapply(spec, function(s) s$horizon, numeric(1))

  if (!summary) {
    out <- as.data.frame(mat)
    # A time-specific marginal HR must carry its time everywhere it appears,
    # and the raw-draw frame has no `effect` column to hang it on. One named
    # numeric per column, NA where the measure has no evaluation time.
    names(at_time) <- colnames(mat)
    attr(out, "at_time") <- at_time
    names(horizon) <- colnames(mat)
    attr(out, "horizon") <- horizon
    return(out)
  }

  summary_df <- .summarize_draw_matrix(mat, probs)
  labels <- data.frame(
    variable = colnames(mat),
    effect = vapply(spec, function(s) s$effect, character(1)),
    population = vapply(spec, function(s) s$population, character(1)),
    stringsAsFactors = FALSE
  )
  # Only carry the column when it says something, so shared-baseline and
  # RMST-only output keeps its previous shape.
  if (any(!is.na(at_time))) labels$at_time <- at_time
  if (any(!is.na(horizon))) labels$horizon <- horizon
  .mlumr_result(cbind(labels, summary_df, row.names = NULL),
                "mlumr_marginal_effects", family = "survival",
                rmst_horizon = rmst_horizon)
}


#' Note that absolute survival predictions transport a study-specific baseline
#'
#' Each study contributes exactly one arm, so a study-specific baseline shape
#' and a treatment-specific baseline shape are perfectly aliased: no part of the
#' data can say whether a difference in shape belongs to the treatment or to the
#' study's eligibility, ascertainment, follow-up, calendar time, supportive
#' care, or unmeasured prognosis. Predicting one treatment in the other
#' population therefore carries that study's shape across, which is a structural
#' assumption on top of the covariate adjustment, not a consequence of it. The
#' contrast estimands are less exposed than the absolute curves, and the RMST
#' estimands are collapsible, so say this where the absolute numbers are
#' produced. Once per session, like the marginal-HR note.
#' @param object A fitted `mlumr_fit`.
#' @return `TRUE` invisibly if the note was emitted.
#' @keywords internal
.transported_baseline_note <- function(object) {
  if (!identical(object$family %||% "", "survival")) return(invisible(FALSE))
  if (!.aux_shapes_differ(object)) return(invisible(FALSE))
  if (isTRUE(getOption("mlumr.transport_baseline_note"))) {
    return(invisible(FALSE))
  }
  message("Note: this fit gives each study its own baseline shape ",
          "(`aux_by = \".study\"`). With one arm per study a ",
          "treatment-specific shape and a study-specific shape are perfectly ",
          "aliased, so predicting a treatment in the other population carries ",
          "that study's shape with it, an assumption the data cannot check. ",
          "Compare against `aux_by = \"none\"`, prefer the collapsible RMST ",
          "estimands for headline numbers, and report which was used. ",
          "Suppress with options(mlumr.transport_baseline_note = TRUE).")
  options(mlumr.transport_baseline_note = TRUE)
  invisible(TRUE)
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
