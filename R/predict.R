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
#'
#'   When supplied, the result has one row per requested time for each
#'   treatment and population cell, in the order requested and including
#'   repeats, with a `requested_time` column beside `time`. With `summary = FALSE` the mapping is carried as the
#'   `requested_time` and `used_time` attributes instead, one entry per time
#'   column. Refit with `pred_times` containing the exact times to avoid the
#'   approximation.
#' @param newdata Optional data frame of covariate profiles defining an arbitrary
#'   **target population**. When supplied, per-treatment absolute predictions are
#'   standardized to this population by g-computation (averaging model-based
#'   predictions over the rows at each posterior draw), and `population` is
#'   ignored. Supports `type = "response"`/`"link"` (binomial/normal/poisson) and
#'   `type = "survival"`/`"hazard"`/`"cumhaz"`/`"rmst"`/`"median"`/`"loghr"`
#'   (survival). Rows outside the fitted covariate support are model-based
#'   extrapolation; overlap is not checked.
#' @param ... Additional arguments (unused)
#'
#' @details
#' **Marginalization on non-identity links.** For `type = "response"` the
#' reported values are `E[g^{-1}(eta)]`, the population-average prediction
#' for an individual drawn from that population, not `g^{-1}(E[eta])`; the
#' expectation runs over the IPD individuals for the index population and
#' over the integration points for the comparator one. `type = "link"`
#' applies the fitted link after that marginalization.
#'
#' @return A data frame with predictions. `type = "rmst"` adds a `horizon`
#'   column with the restriction time actually integrated to, since RMST at
#'   different horizons is a different estimand. The plot methods require
#'   `summary = TRUE`; with `summary = FALSE` the raw posterior draws are
#'   returned as a plain data frame. For `type = "median"` the summary is
#'   conditional on the median being reached, and `p_not_reached` gives the
#'   posterior probability that it is not.
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
  summary <- .validate_flag(summary, "summary")
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

  population <- .validate_choice(population, c("both", "index", "comparator"),
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

  type <- .validate_choice(type %||% "response", c("response", "link"), "type")

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
#' @noRd
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
#' @noRd
.surv_time_selection <- function(times, pred_times) {
  if (is.null(times)) return(seq_along(pred_times))
  .validate_survival_prediction_times(times)
  idx <- vapply(times, function(t) which.min(abs(pred_times - t)), integer(1))
  .warn_snapped_prediction_times(times, pred_times[idx])
  # Order and multiplicity are part of the request; carry the requested times.
  structure(idx, requested = times)
}

#' Say when requested prediction times were moved onto the fitted grid
#'
#' A move, or two distinct requests landing on one grid point, is reported.
#' @param requested The user's `times`, in the order given.
#' @param used The fitted grid times actually selected, aligned to `requested`.
#' @return `NULL`, invisibly; called for the message.
#' @noRd
.warn_snapped_prediction_times <- function(requested, used) {
  # Relative, because a 0.2 gap means something different at t = 1 and t = 500.
  moved <- abs(used - requested) > 1e-8 * pmax(1, abs(requested))
  # Two distinct requests on one grid point lose information; a duplicated
  # request does not.
  n_distinct_requested <- length(unique(requested))
  distinct_collapse <- length(unique(used)) < n_distinct_requested
  if (!any(moved) && !distinct_collapse) {
    return(invisible(NULL))
  }
  parts <- character(0)
  if (any(moved)) {
    show <- which(moved)
    more <- if (length(show) > 5L) {
      show <- show[seq_len(5L)]
      sprintf(" (and %d more)", sum(moved) - 5L)
    } else {
      ""
    }
    moves <- paste(sprintf("%g -> %g", requested[show], used[show]),
                   collapse = ", ")
    parts <- c(parts,
               paste0("Requested prediction time(s) were moved to the nearest ",
                      "fitted grid time: ", moves, more, "."))
  }
  if (distinct_collapse) {
    parts <- c(parts,
               sprintf(paste0("Distinct requested time(s) share a grid point, ",
                              "so %d requested time(s) are answered by %d ",
                              "distinct fitted time(s)."),
                       n_distinct_requested, length(unique(used))))
  }
  message(paste(c(parts,
                  paste0("Refit with `pred_times` containing the exact times ",
                         "to evaluate them directly.")),
                collapse = " "))
  invisible(NULL)
}

#' Value of a survival curve at the origin, or `NA_real_` when it has none
#'
#' Survival is 1 and cumulative hazard is 0 at t = 0, so those curves start
#' at the origin, as a Kaplan-Meier curve does. Hazard has no universal value
#' there. Added only for the full default curve when 0 is not a fitted time.
#' @noRd
.surv_origin <- function(type, times, pred_times) {
  origin <- switch(type, survival = 1, cumhaz = 0, NA_real_)
  if (is.na(origin) || !is.null(times)) return(NA_real_)
  if (any(abs(pred_times) < sqrt(.Machine$double.eps))) return(NA_real_)
  origin
}

#' Assemble a survival prediction frame from per-cell draw matrices
#'
#' Both prediction routes reduce to one draw matrix per displayed cell, so
#' the layout is written once. `values` has one matrix per row of `cells`,
#' with one column for scalar types and one per selected time for curves.
#' @noRd
.surv_result_frame <- function(values, cells, type, summary, probs,
                               times_out = NULL, origin = NA_real_,
                               horizon = NULL, requested_times = NULL) {
  label_names <- intersect(c("treatment", "population"), names(cells))

  # A missing median is an expected outcome with its own diagnostic
  # (`p_not_reached`), not a dropped draw.
  if (summary && type != "median") .warn_dropped_draws(do.call(cbind, values))

  rows <- lapply(seq_along(values), function(i) {
    m <- values[[i]]
    lab <- cells[i, label_names, drop = FALSE]
    rownames(lab) <- NULL
    if (is.null(times_out)) {
      if (!summary) {
        return(data.frame(lab, value = m[, 1], row.names = NULL,
                          check.names = FALSE))
      }
      s <- .summarize_draw_matrix(m, probs, warn = FALSE)
      # A median summary is conditional on the median being reached.
      if (type == "median") s$p_not_reached <- mean(is.na(m[, 1]))
      return(data.frame(lab, s, row.names = NULL, check.names = FALSE))
    }
    if (!summary) {
      colnames(m) <- sprintf("t_%.15g", times_out)
      if (anyDuplicated(colnames(m))) {
        colnames(m) <- make.unique(colnames(m), sep = "_dup")
      }
      df <- data.frame(lab, m, row.names = NULL, check.names = FALSE)
      if (!is.na(origin)) {
        df <- data.frame(df[label_names], t_0 = origin,
                         df[setdiff(names(df), label_names)],
                         check.names = FALSE)
      }
      return(df)
    }
    s <- .summarize_draw_matrix(m, probs, warn = FALSE)
    # Report both the requested and the evaluated time when times were named.
    df <- if (is.null(requested_times)) {
      data.frame(lab, time = times_out, s, row.names = NULL,
                 check.names = FALSE)
    } else {
      data.frame(lab, requested_time = requested_times, time = times_out, s,
                 row.names = NULL, check.names = FALSE)
    }
    if (!is.na(origin)) {
      o <- df[1, , drop = FALSE]
      o$time <- 0
      if ("requested_time" %in% names(o)) o$requested_time <- NA_real_
      # Only the summarized quantities take the origin value.
      num_cols <- setdiff(names(o)[vapply(o, is.numeric, logical(1))],
                          c("time", "requested_time", label_names))
      o[num_cols] <- origin
      if ("sd" %in% names(o)) o$sd <- 0
      df <- rbind(o, df)
    }
    df
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # The raw layout is one column per time, so the mapping goes on attributes.
  if (!summary && !is.null(requested_times) && !is.null(times_out)) {
    time_cols <- setdiff(names(out), c(label_names, "horizon"))
    mapping <- times_out
    if (!is.na(origin)) {
      # The prepended origin column answers no request.
      mapping <- c(NA_real_, mapping)
    }
    if (length(mapping) == length(time_cols)) {
      req <- if (is.na(origin)) requested_times else c(NA_real_, requested_times)
      attr(out, "requested_time") <- stats::setNames(req, time_cols)
      attr(out, "used_time") <- stats::setNames(mapping, time_cols)
    }
  }

  # The horizon integrated to, as a column in both layouts.
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
#' @noRd
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
  # Subsetting keeps numeric and factor treatment labels as they are.
  cell_labels <- data.frame(
    treatment = c(idx_trt, cmp_trt)[match(cells$trt,
                                          c("index", "comparator"))],
    population = vapply(cells$pop, pop_label, character(1)),
    stringsAsFactors = FALSE
  )

  # A prediction in the other study's population carries this study's
  # baseline shape with it, and so does the loghr contrast.
  .transported_baseline_note(object)

  # Time-varying marginal log hazard ratio (index vs comparator) by population:
  # log( h-bar_index(t | pop) / h-bar_comparator(t | pop) ) at each fitted time.
  if (type == "loghr") {
    sel <- .surv_time_selection(times, pred_times)
    values <- lapply(pops, function(pop) {
      cols_lhr <- sprintf("loghr_%s[%d]", pop, sel)
      if (all(cols_lhr %in% names(draws))) {
        # Emitted in log space, so it stays finite where the hazard underflows.
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
      type, summary, probs, times_out = pred_times[sel],
      requested_times = attr(sel, "requested")
    ))
  }

  # Scalar summaries (RMST, median): one row per treatment x population.
  if (type %in% c("rmst", "median")) {
    # RMST and the median use the whole fitted grid, so `times` is refused.
    if (!is.null(times)) {
      stop("`times` selects points on a predicted curve, but `type = \"", type,
           "\"` is a scalar summary of the whole fitted grid and does not use ",
           "it. RMST integrates to the horizon fixed at fit time, and the ",
           "median is searched over `pred_times`; change either by refitting.",
           call. = FALSE)
    }
    if (type == "rmst") .warn_coarse_rmst_grid_builtin(object, pops)
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
    if (type == "median") {
      .warn_early_median(lapply(seq_len(nrow(cells)), function(i) {
        first <- sprintf("surv_%s_%s[1]", cells$trt[i], cells$pop[i])
        .median_early_share(draws[[first]])
      }))
    }
    # The horizon actually integrated to, read off the fitted grid.
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
                     requested_times = attr(sel, "requested"),
                     origin = .surv_origin(type, times, pred_times))
}


#' Marginal posterior variance change for comparator coefficients
#'
#' For each `beta_comparator` coefficient this reports
#' `1 - posterior_variance / prior_variance`, a descriptive comparison of
#' marginal SDs and not an identification test. `prior_sd` is the prior
#' standard deviation, `NA` for a Student-t prior with `df <= 2`.
#'
#' @param object An `mlumr_fit` from `model = "relaxed"`.
#' @return A data frame with one row per covariate (`covariate`, `prior_sd`,
#'   `posterior_sd`, `contraction`), or `NULL` if unavailable.
#' @noRd
.relaxed_contraction <- function(object) {
  if (!identical(object$model, "relaxed")) return(NULL)
  prior_scale <- object$stan_data$prior_beta_comparator_sd
  covs <- object$data$covariates
  if (is.null(prior_scale) || is.null(covs)) return(NULL)
  prior_scale <- as.numeric(prior_scale)
  # The stored hyperparameter is the scale; a Student-t SD is
  # scale * sqrt(df / (df - 2)) and does not exist for df <= 2.
  dist <- object$stan_data$prior_beta_comparator_dist %||% 0L
  df <- object$stan_data$prior_beta_comparator_df %||% NA_real_
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
#' @noRd
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
    "with `prior_beta_comparator` and check with `prior_sensitivity()`. ",
    "Suppress with `options(mlumr.quiet_relaxed_index = TRUE)`."
  )
  invisible()
}


#' Note that some median-survival draws never reach S = 0.5
#'
#' Emitted from [predict.mlumr_fit()] (survival, `type = "median"`) when a
#' positive fraction of posterior draws have an unreached median. Suppress with
#' `options(mlumr.quiet_median_not_reached = TRUE)`.
#' @noRd
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


#' Share of draws whose median falls before the first fitted prediction time
#'
#' There the median is interpolated between `S(0) = 1` and the first grid
#' value, which can be off by a large factor. The share is computed per
#' draw, as in the RMST check.
#' @noRd
.median_early_share <- function(surv_mat) {
  s <- as.matrix(surv_mat)
  if (!nrow(s) || !ncol(s)) return(NA_real_)
  mean(s[, 1L] <= 0.5)
}

#' Warn when the median is being read off the first grid interval
#' @param shares Per-curve values of [.median_early_share()].
#' @noRd
.warn_early_median <- function(shares) {
  shares <- unlist(shares)
  shares <- shares[is.finite(shares)]
  if (!length(shares)) return(invisible(NULL))
  worst <- max(shares)
  if (worst > 0.05) {
    msg <- paste0("In %.0f%% of posterior draws the survival curve is already ",
                  "at or below 0.5 at the first fitted prediction time, so the ",
                  "median is interpolated across that whole first interval and ",
                  "can be off by a large factor. Refit with `pred_times` that ",
                  "start earlier and compare.")
    warning(sprintf(msg, 100 * worst), call. = FALSE)
  }
  invisible(NULL)
}

#' Median survival time from posterior survival-curve draws (linear interp)
#' @noRd
.surv_median_from_draws <- function(surv_mat, times) {
  apply(surv_mat, 1, function(s) {
    if (all(s > 0.5)) return(NA_real_)   # median beyond observed follow-up
    k <- which(s <= 0.5)[1]
    if (k == 1L) {
      # Before the first grid point: interpolate from the exact S(0) = 1.
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
#' For survival proportional-hazards fits the scalar `"hr"` is always a
#' marginal hazard ratio at one time, recorded in the `at_time` column: the
#' `t -> 0` limit under a shared baseline shape (where an SPFA fit's value
#' coincides with the conditional hazard ratio, since the shared
#' coefficients cancel), and the value at the first prediction time, or at
#' `at_time`, under study-specific shapes. Hazard ratios are non-collapsible,
#' so the marginal ratio is time-varying; `predict(type = "loghr")` gives
#' the curve, and the RMST effects are collapsible within a population.
#'
#' For accelerated-failure-time fits the scalar is
#' `exp(E_X[eta_index(X)] - E_X[eta_comparator(X)])`. With one shared shape
#' and SPFA coefficients it is a population time ratio (`TR`): every
#' individual's survival time is accelerated by the same factor. With
#' differing shapes or treatment-specific coefficients no single acceleration
#' factor exists and it is labeled `EXP_DELTA_ETA`; use RMST effects or
#' explicitly indexed survival quantiles there. Neither carries an evaluation
#' time. See `vignette("survival-outcomes", "mlumr")`.
#'
#' For binomial fits `"lor"` is always a logit-scale marginal odds ratio,
#' whatever the fitted link, so for a probit or cloglog fit it is on a
#' different scale than [naive()] and [stc()] `$estimate`.
#'
#' **Relaxed-model index-population estimands** average `beta_comparator`
#' over the IPD covariate distribution, outside the support it was identified
#' on, so they are wider and more prior-sensitive than the comparator-population
#' ones. `marginal_effects()` says so once per call; suppress with
#' `options(mlumr.quiet_relaxed_index = TRUE)`.
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
#'   or `"rr"`. For normal: `"all"` or `"md"`. For poisson: `"all"` or
#'   `"rr"`. For survival: `"all"`, `"rmstd"`, `"rmstr"`, and the one scalar
#'   name the fit's contrast is, `"hr"` for a proportional-hazards fit, `"tr"`
#'   for a shared-shape SPFA accelerated-failure-time fit and
#'   `"exp_delta_eta"` otherwise. There are no aliases: requesting a scale the
#'   fit cannot supply is an error naming the one it can.
#' @param at_time Evaluation time for the scalar marginal hazard ratio of a
#'   proportional-hazards fit whose two studies have different baseline
#'   shapes. Snapped to the nearest fitted prediction time, with a message.
#'   `NULL` uses the first prediction time. Under a shared baseline the
#'   scalar is the `t -> 0` limit, so only `at_time = 0` is accepted; an
#'   error for AFT fits.
#' @param summary Return summary (`TRUE`) or full draws (`FALSE`)
#' @param probs Quantiles for summary
#' @param newdata Optional data frame of covariate profiles defining a target
#'   population to standardize the effect to by g-computation (Chandler and
#'   Ishak, Eq 9 and 10); `population` is then ignored. Column names must
#'   match the model covariates. The same survival scalar selector applies as
#'   without `newdata`.
#'
#' @return A data frame. With `summary = FALSE` the raw posterior draws are
#'   returned as a plain data frame whose column names carry the effect scale
#'   (`lor_*`, `rr_*`, `delta_*`, `hr_*` / `tr_*` / `exp_delta_eta_*`,
#'   `rmst*`); with `summary = TRUE` the `effect` column names the measure.
#'   For survival, RMST rows carry a `horizon` column and the scalar rows an
#'   `at_time` column (attributes of the same names on the raw-draw frame).
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
  summary <- .validate_flag(summary, "summary")
  .validate_probs(probs)

  # `at_time` means nothing without a time axis or for an integral to a horizon.
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

  # Transport to a target population, after the `at_time` guards above.
  if (!is.null(newdata)) {
    return(.marginal_effects_target(object, newdata, effect, summary, probs,
                                    at_time))
  }

  population <- .validate_choice(population,
                                 c("both", "index", "comparator"),
                                 "population")

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

  # Map Stan variable names to family-aware effect labels.
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
#' @noRd
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
#' @noRd
.predict_target <- function(object, newdata, type, summary, probs, times) {
  family <- object$family %||% "binomial"
  idx_trt <- object$data$index_treatment
  cmp_trt <- object$data$comparator_treatment

  if (family == "survival") {
    return(.predict_target_survival(object, newdata, type %||% "survival",
                                    summary, probs, times))
  }

  type <- .validate_choice(type %||% "response",
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
  # Same column naming as the built-in route, with `_target` as the population.
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
#' @noRd
.predict_target_survival <- function(object, newdata, type, summary, probs,
                                     times = NULL) {
  type <- .validate_choice(type,
                           c("survival", "hazard", "cumhaz", "rmst",
                             "median", "loghr"),
                           "type")
  if (!is.null(times)) times <- .validate_survival_prediction_times(times)
  idx_trt <- object$data$index_treatment
  cmp_trt <- object$data$comparator_treatment
  pred_times <- object$pred_times
  cell_labels <- data.frame(treatment = c(idx_trt, cmp_trt),
                            population = "Target", stringsAsFactors = FALSE)

  # Same transported-baseline assumption as the built-in populations.
  .transported_baseline_note(object)

  if (type %in% c("rmst", "median")) {
    # RMST and the median use the whole fitted grid, so `times` is refused.
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
      matrix(.rmst_from_surv_matrix(sbar$index, tt, sbar$share$index),
             ncol = 1),
      matrix(.rmst_from_surv_matrix(sbar$comparator, tt,
                                    sbar$share$comparator), ncol = 1)
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
    # Evaluate only the requested times.
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
        type, summary, probs, times_out = pred_times[sel],
        requested_times = attr(sel, "requested")
      )
      if (!summary) return(out)
      return(.mlumr_result(out, "mlumr_prediction", ptype = type,
                           family = "survival"))
    }
    values <- list(exp(log_h$index), exp(log_h$comparator))
    out <- .surv_result_frame(values, cell_labels, type, summary, probs,
                              times_out = pred_times[sel],
                              requested_times = attr(sel, "requested"),
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
    .warn_early_median(list(.median_early_share(sbar$index),
                            .median_early_share(sbar$comparator)))
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
                            requested_times = attr(sel, "requested"),
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
#' @noRd
.marginal_effects_target <- function(object, newdata, effect, summary, probs,
                                     at_time = NULL) {
  family <- object$family %||% "binomial"
  # A relaxed fit extrapolates beta_comparator to any target, as to the index.
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

  # Same column naming as the built-in route, with `_target` as the population.
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
#' @noRd
.basis_rows <- function(basis, idx) {
  if (is.null(basis)) return(NULL)
  basis[idx, , drop = FALSE]
}

#' Survival S(t | x) at arbitrary times for one linear-predictor draw vector
#'
#' Generalizes [.surv_eval_curve()] to an arbitrary `times` grid with matching
#' I-spline integral basis `ibasis` (used for the M-spline/piecewise baseline;
#' ignored for parametric distributions, which evaluate `S` analytically).
#' @noRd
.surv_s_at_times <- function(object, eta, times, ibasis,
                             treatment = c("index", "comparator"),
                             log_scale = FALSE) {
  treatment <- match.arg(treatment)
  draws <- object$draws
  # The baseline belongs to the study, so it travels with the treatment.
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
#' @noRd
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

#' Auxiliary (shape) draws for one treatment
#'
#' Reads the named per-treatment view (`aux_val`, `aux_val_cmp`) or the raw
#' matrix `aux_raw[1,s]` it is derived from. With one stratum either view
#' serves either treatment; with two they are different studies' shapes and
#' are never crossed. A shape of 1 is returned only where Stan fixes it at 1,
#' never as a substitute for one that could not be found.
#'
#' @param object A fitted `mlumr_fit`.
#' @param base `"aux_val"` or `"aux2_val"`.
#' @param treatment `"index"` or `"comparator"`.
#' @param n Number of draws, for the fixed-at-one case.
#' @return Numeric vector of `n` draws.
#' @noRd
.surv_aux_draws <- function(object, base, treatment, n) {
  draws <- object$draws
  cmp <- paste0(base, "_cmp")
  raw <- sub("_val$", "_raw", base)
  n_strata <- object$stan_data$n_strata %||% 1L
  comparator <- identical(treatment, "comparator")

  # This treatment's own layouts, view before raw matrix.
  own <- if (comparator) {
    c(cmp, paste0(raw, "[1,", n_strata, "]"))
  } else {
    c(base, paste0(raw, "[1,1]"))
  }
  for (nm in own) {
    if (nm %in% names(draws)) {
      return(draws[[nm]])
    }
  }

  # With one stratum both views are the same number.
  if (n_strata == 1L) {
    for (nm in c(base, cmp, paste0(raw, "[1,1]"))) {
      if (nm %in% names(draws)) {
        return(draws[[nm]])
      }
    }
  }

  # Positive membership, so an unrecognized code takes the error path.
  dist <- object$surv_info$dist_code
  fixed_at_one <- if (identical(base, "aux2_val")) {
    isTRUE(dist %in% 1L:8L)
  } else {
    isTRUE(dist %in% c(1L, 4L))
  }
  if (fixed_at_one) {
    return(rep(1, n))
  }
  stop("Could not find ", base, " draws for the ", treatment, " baseline. ",
       "The ", object$surv_info$distribution %||% "fitted",
       " distribution has this shape, so it cannot be defaulted to 1. ",
       "Refit without excluding `", base, "`, `", cmp, "` or `", raw,
       "` from the saved parameters.", call. = FALSE)
}


#' Target-population standardized survival curve S-bar(t) per treatment
#'
#' g-computation of the population-average survival function over an arbitrary
#' target population (Chandler & Ishak Eq 14): for each treatment,
#' `S_bar_k(t) = (1/M) sum_m S_k(t | x_m)` over the `M` rows of `newdata`.
#'
#' The `share` element is the resolution share of [.decay_share()] computed
#' from the decay pieces summed over the profiles, not from the averaged
#' curve: the average can look resolved when none of its parts is.
#' @return A list with `index` and `comparator`, each an `[n_draws, length(times)]`
#'   matrix of target-standardized survival probabilities, and `share`, a
#'   list of two per-draw vectors named the same way.
#' @noRd
.standardize_target_survival_s <- function(object, newdata, times, ibasis,
                                           ibasis_cmp = NULL,
                                           log_scale = FALSE) {
  profiles <- .conditional_profiles(object, newdata)
  x_centered <- profiles$X
  params <- .conditional_parameters(object, profiles$covariates)
  n_target <- nrow(x_centered)

  # Each study has its own basis under `aux_by = ".study"`.
  ibasis_cmp <- ibasis_cmp %||% ibasis

  s_idx <- NULL
  s_cmp <- NULL
  decay_idx <- NULL
  decay_cmp <- NULL
  add_decay <- function(total, s) {
    parts <- .decay_parts(if (log_scale) exp(s) else s)
    if (is.null(total)) parts else Map(`+`, total, parts)
  }
  for (i in seq_len(n_target)) {
    eta <- .conditional_eta(params, x_centered[i, , drop = FALSE])
    si <- .surv_s_at_times(object, eta$index, times, ibasis, "index", log_scale)
    sc <- .surv_s_at_times(object, eta$comparator, times, ibasis_cmp,
                           "comparator", log_scale)
    decay_idx <- add_decay(decay_idx, si)
    decay_cmp <- add_decay(decay_cmp, sc)
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
  share <- list(index = .decay_share(decay_idx$max, decay_idx$total),
                comparator = .decay_share(decay_cmp$max, decay_cmp$total))
  if (log_scale) {
    list(index = s_idx - log(n_target), comparator = s_cmp - log(n_target),
         share = share)
  } else {
    list(index = s_idx / n_target, comparator = s_cmp / n_target,
         share = share)
  }
}


#' Conditional log hazard at arbitrary times for one predictor draw vector
#' @noRd
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
#' @noRd
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
#' @noRd
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
#'
#' @param s_mat Draws by times survival matrix, already averaged over the
#'   target's profiles.
#' @param times The integration grid.
#' @param share The per-draw resolution share of the profiles behind `s_mat`,
#'   the `share` element [.standardize_target_survival_s()] returns. It is
#'   not derived from `s_mat` here on purpose: the average of the profiles can
#'   pass the check when every profile fails it.
#' @noRd
.rmst_from_surv_matrix <- function(s_mat, times, share) {
  dt <- diff(times)
  # trapezoid: sum_j (S[,j] + S[,j+1]) / 2 * (t[j+1] - t[j])
  left <- s_mat[, -ncol(s_mat), drop = FALSE]
  right <- s_mat[, -1, drop = FALSE]
  .warn_coarse_rmst_grid(share)
  as.numeric(((left + right) / 2) %*% dt)
}


#' Warn when the RMST grid is too coarse for the hazard it is integrating
#'
#' When most of the decay falls inside one grid interval the trapezoid rule
#' overstates the integral badly, and both arms alike, so a difference or
#' ratio can lose the whole effect: exponential rates 100 and 200 to
#' `tau = 10` on the default 100-node grid give an RMST ratio of 1.0001
#' against a true 2.0. The trigger is the share of the total decay that lands
#' in one interval.
#'
#' @param share Per-draw shares, one vector per curve, from [.decay_share()]
#'   or [.standardize_target_survival_s()]; `NA` where there is no decay.
#' @return `NULL`, invisibly; called for the warning.
#' @noRd
.warn_coarse_rmst_grid <- function(share) {
  .warn_interval_share(list(share))
}

#' The two pieces of the resolution share, per draw
#'
#' `max` is the largest drop between two adjacent grid points and `total` the
#' drop from the first point to the last. Kept apart so that the pieces of
#' several profiles can be summed before the ratio is taken. `NA` when the
#' grid has fewer than two points.
#' @noRd
.decay_parts <- function(s_mat) {
  s_mat <- as.matrix(s_mat)
  if (ncol(s_mat) < 2L) {
    none <- rep(NA_real_, nrow(s_mat))
    return(list(max = none, total = none))
  }
  drops <- s_mat[, -ncol(s_mat), drop = FALSE] - s_mat[, -1L, drop = FALSE]
  list(max = apply(drops, 1L, max),
       total = s_mat[, 1L] - s_mat[, ncol(s_mat)])
}

#' The resolution share from its pieces, `NA` where there is no decay
#' @noRd
.decay_share <- function(max_drop, total_drop) {
  share <- max_drop / total_drop
  share[!is.finite(share) | !is.finite(total_drop) | total_drop <= 0] <- NA_real_
  share
}

#' The same check for the fit's own populations
#'
#' The `rmst_*` draws are integrated in Stan on the same grid by the same
#' trapezoid rule, so the standardized curve is evaluated on that grid for
#' the requested populations over the rows Stan averaged, and judged per row.
#' A deterministic subset of at most 200 draws and 60 rows keeps the check
#' cheap; it estimates the share rather than taking a census.
#' @param object A survival `mlumr_fit`.
#' @param pops The populations whose RMST is being returned; only their
#'   curves are judged.
#' @return `NULL`, invisibly; called for the warning.
#' @noRd
.warn_coarse_rmst_grid_builtin <- function(object,
                                           pops = c("index", "comparator")) {
  tt <- object$stan_data$rmst_grid_times
  x_int <- object$stan_data$X_int
  if (is.null(tt) || length(tt) < 2L || is.null(x_int)) {
    return(invisible(NULL))
  }
  covariates <- object$data$covariates
  n_cov <- length(covariates)
  if (!n_cov) {
    return(invisible(NULL))
  }
  ib <- object$stan_data$rmst_ibasis
  ib_cmp <- object$stan_data$rmst_ibasis_cmp
  cov_center <- object$stan_data$cov_center %||% rep(0, n_cov)
  index_rows <- object$data$ipd$data[, covariates, drop = FALSE]
  comparator_rows <- matrix(as.numeric(x_int), ncol = n_cov)
  comparator_rows <- as.data.frame(sweep(comparator_rows, 2L, cov_center, "+"))
  names(comparator_rows) <- covariates

  # A deterministic, evenly spaced subset keeps this cheap.
  thin <- function(n, keep) {
    if (n <= keep) seq_len(n) else unique(round(seq(1, n, length.out = keep)))
  }
  object$draws <- object$draws[thin(nrow(object$draws), 200L), , drop = FALSE]

  shares <- list()
  row_sets <- list(index = index_rows, comparator = comparator_rows)
  for (rows in row_sets[intersect(c("index", "comparator"), pops)]) {
    rows <- rows[thin(nrow(rows), 60L), , drop = FALSE]
    sbar <- tryCatch(.standardize_target_survival_s(object, rows, tt, ib,
                                                    ib_cmp),
                     error = function(e) NULL)
    if (is.null(sbar)) next
    shares <- c(shares, list(sbar$share$index, sbar$share$comparator))
  }
  .warn_interval_share(shares)
}

#' Warn when enough posterior draws are integrated from a single straight line
#'
#' A curve is badly resolved in a draw when more than half of its decay falls
#' inside one grid interval; the criterion is the fraction of such draws on
#' the worst curve, ignored below one in twenty.
#' @noRd
.warn_interval_share <- function(shares) {
  worst <- 0
  worst_share <- NA_real_
  for (share in shares) {
    share <- share[is.finite(share)]
    if (!length(share)) next
    bad <- mean(share > 0.5)
    if (bad > worst) {
      worst <- bad
      worst_share <- stats::median(share[share > 0.5])
    }
  }
  if (worst > 0.05) {
    fmt <- paste0("In %.0f%% of posterior draws, more than half of the fitted ",
                  "survival decay (median %.0f%% among them) happens inside a ",
                  "single interval of the RMST grid, so RMST values and their ",
                  "differences and ratios can be badly wrong. Refit with a larger ",
                  "`n_rmst_grid` and confirm the value has stopped moving.")
    warning(sprintf(fmt, 100 * worst, 100 * worst_share), call. = FALSE)
  }
  invisible(NULL)
}


#' Marginal survival effects standardized to an arbitrary target population
#'
#' Internal dispatch for [marginal_effects()] (survival) when `newdata` is
#' supplied. RMST effects are computed from the target-standardized survival
#' curve. For proportional-hazards fits, `"hr"` is the time-specific marginal
#' hazard ratio obtained from the survival-weighted hazards in that same target.
#' RMST differences are directly collapsible, but no effect measure is assumed
#' to be invariant across populations merely because it is collapsible.
#' @noRd
.marginal_effects_target_survival <- function(object, newdata, effect, summary,
                                              probs, at_time = NULL) {
  is_ph <- isTRUE(object$surv_info$is_ph)
  # Differing shapes decide the estimand: no closed form, read off the grid.
  stratified <- .aux_shapes_differ(object)
  # The same literal selector as the built-in route.
  lab <- .surv_scalar_label(object)
  scalar_effect <- .surv_scalar_effect_name(lab$label)
  valid_effects <- c("all", scalar_effect, "rmstd", "rmstr")
  if (!effect %in% valid_effects) {
    stop(.surv_effect_scale_error(effect, lab$label, scalar_effect, stratified,
                                  valid_effects), call. = FALSE)
  }
  if (!is.null(at_time) && !is_ph) {
    stop("`at_time` applies to a time-specific marginal hazard ratio, but this ",
         "fit uses an accelerated-failure-time distribution. Use RMST effects ",
         "for target-standardized comparisons.", call. = FALSE)
  }

  .transported_baseline_note(object)

  times <- object$stan_data$rmst_grid_times
  # The restriction time, carried with every RMST value.
  rmst_tau <- max(times)
  # Standardizing over the whole grid is only paid for when RMST is wanted.
  want_rmst <- effect %in% c("all", "rmstd", "rmstr")
  rmst_i <- NULL
  rmst_c <- NULL
  if (want_rmst) {
    sbar <- .standardize_target_survival_s(object, newdata, times,
                                           object$stan_data$rmst_ibasis,
                                           object$stan_data$rmst_ibasis_cmp)
    rmst_i <- .rmst_from_surv_matrix(sbar$index, times, sbar$share$index)
    rmst_c <- .rmst_from_surv_matrix(sbar$comparator, times,
                                     sbar$share$comparator)
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
      log_h <- .standardize_target_survival_log_h(
        object, newdata, grid[p],
        .basis_rows(object$stan_data$pred_ibasis, p),
        .basis_rows(object$stan_data$pred_ibasis_cmp, p),
        .basis_rows(object$stan_data$pred_basis, p),
        .basis_rows(object$stan_data$pred_basis_cmp, p)
      )
      hr_draws <- exp(log_h$index[, 1] - log_h$comparator[, 1])
    } else {
      # Shared shape: the marginal hazard ratio is the closed-form t -> 0
      # limit E[exp(eta_index)] / E[exp(eta_cmp)], as Stan writes `delta_*`.
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
  if (!is_ph && effect %in% c("all", scalar_effect)) {
    # The target-standardized location contrast; with shared coefficients the
    # covariate term cancels and this equals the built-in scalar exactly.
    spec[[length(spec) + 1L]] <- list(
      variable = paste0(tolower(scalar_effect), "_target"),
      effect = lab$label, population = "Target",
      at_time = NA_real_, horizon = NA_real_,
      draws = exp(.target_delta_eta(object, newdata))
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

#' Target-standardized location contrast for an AFT fit
#'
#' `mean(eta_index) - mean(eta_comparator)` over the target rows; its
#' exponential is a ratio of geometric-mean survival times. With shared
#' coefficients it equals the built-in `delta_eta` for every target.
#' @noRd
.target_delta_eta <- function(object, newdata) {
  profiles <- .conditional_profiles(object, newdata)
  params <- .conditional_parameters(object, profiles$covariates)
  x_centered <- profiles$X
  si <- NULL
  sc <- NULL
  for (i in seq_len(nrow(x_centered))) {
    eta <- .conditional_eta(params, x_centered[i, , drop = FALSE])
    si <- if (is.null(si)) eta$index else si + eta$index
    sc <- if (is.null(sc)) eta$comparator else sc + eta$comparator
  }
  (si - sc) / nrow(x_centered)
}

#' Marginal log hazard ratio in a target population at the origin
#'
#' `log E[exp(eta_index)] - log E[exp(eta_comparator)]` over the target rows,
#' the closed form the Stan models use for `delta_*`.
#' @noRd
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
#' @noRd
.marginal_effects_survival <- function(object, population, effect, summary,
                                       probs, at_time = NULL) {
  draws <- object$draws
  is_ph <- isTRUE(object$surv_info$is_ph)
  # `.aux_shapes_differ()` mirrors the Stan gate.
  stratified <- .aux_shapes_differ(object)
  # Natural scale, null 1. The label and its evaluation time come from one
  # derivation shared with prior_sensitivity().
  lab <- .surv_scalar_label(object)
  hr_label <- lab$label
  # The selector is literal: exactly one scalar name is valid per fit.
  scalar_effect <- .surv_scalar_effect_name(hr_label)
  valid_effects <- c("all", scalar_effect, "rmstd", "rmstr")
  if (!effect %in% valid_effects) {
    stop(.surv_effect_scale_error(effect, hr_label, scalar_effect, stratified,
                                  valid_effects), call. = FALSE)
  }
  # All three scalar names reach the same computation branch.
  if (effect %in% c("tr", "exp_delta_eta")) effect <- "hr"
  # The evaluation time is an estimand choice, so it is a named argument.
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
      # Only the stratified branch reads a time off the grid.
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
  if (any(c("rmstd", "rmstr") %in% effs)) {
    .warn_coarse_rmst_grid_builtin(object, pops)
  }

  # Hazard ratios are non-collapsible, so the marginal HR drifts with time in
  # both models; say so once per session.
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

  # The restriction time, read off the fitted grid.
  rmst_tau <- {
    g <- object$stan_data$rmst_grid_times
    if (is.null(g)) NA_real_ else max(g)
  }

  spec <- list()
  for (eff in effs) {
    for (pop in pops) {
      if (eff == "hr") {
        d <- if (is.null(hr_index_p)) {
          draws[[sprintf("delta_%s", pop)]]
        } else {
          # The `loghr_*` curve Stan computed; `delta_*` is its first element.
          draws[[sprintf("loghr_%s[%d]", pop, hr_index_p)]]
        }
        vec <- if (is.null(d)) NULL else exp(d)
        # The raw-draw column name tracks the label.
        vname <- sprintf("%s_%s", .surv_scalar_effect_name(hr_label), pop)
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
      # The scalar HR always carries its time: 0 for shared shapes, the
      # requested or first prediction time otherwise.
      eff_at_time <- if (eff != "hr") {
        NA_real_
      } else if (is_ph && stratified) {
        hr_at %||% lab$at_time
      } else {
        lab$at_time
      }
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
#' `spec` has one entry per effect column with `variable`, `effect`,
#' `population`, `at_time`, `horizon` and `draws`; both routes reduce to it,
#' so the layout is written once.
#' @noRd
.surv_effect_frame <- function(spec, summary, probs, rmst_horizon) {
  mat <- do.call(cbind, lapply(spec, function(s) s$draws))
  colnames(mat) <- vapply(spec, function(s) s$variable, character(1))
  at_time <- vapply(spec, function(s) s$at_time, numeric(1))
  horizon <- vapply(spec, function(s) s$horizon, numeric(1))

  if (!summary) {
    out <- as.data.frame(mat)
    # The raw frame has no `effect` column, so the times go on attributes.
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
  # Only carry a column when it says something.
  if (any(!is.na(at_time))) labels$at_time <- at_time
  if (any(!is.na(horizon))) labels$horizon <- horizon
  .mlumr_result(cbind(labels, summary_df, row.names = NULL),
                "mlumr_marginal_effects", family = "survival",
                rmst_horizon = rmst_horizon)
}


#' Note that absolute survival predictions transport a study-specific baseline
#'
#' With one arm per study a study-specific shape and a treatment-specific
#' shape are aliased, so predicting a treatment in the other population
#' carries its study's shape across: an assumption the data cannot check.
#' Once per session, like the marginal-HR note.
#' @param object A fitted `mlumr_fit`.
#' @return `TRUE` invisibly if the note was emitted.
#' @noRd
.transported_baseline_note <- function(object) {
  if (!identical(object$family %||% "", "survival")) return(invisible(FALSE))
  if (!.aux_shapes_differ(object)) return(invisible(FALSE))
  if (isTRUE(getOption("mlumr.transport_baseline_note"))) {
    return(invisible(FALSE))
  }
  message("Note: this fit gives each study its own baseline shape ",
          "(`aux_by = \".study\"`). With one arm per study that shape is ",
          "aliased with the treatment, so predicting a treatment in the other ",
          "population carries the study's shape with it. Compare against ",
          "`aux_by = \"none\"` and prefer the RMST estimands for headline ",
          "numbers. Suppress with options(mlumr.transport_baseline_note = TRUE).")
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
#' @noRd
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
#' @noRd
.validate_mlumr_fit_object <- function(object) {
  if (!inherits(object, "mlumr_fit")) {
    stop("`object` must be an mlumr_fit object.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate an exact scalar choice argument
#' @noRd
.validate_choice <- function(x, choices, name) {
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


#' Validate quantile probabilities
#'
#' Two probabilities that differ only beyond the printed percentage would
#' share a summary column, so they are refused too.
#' @noRd
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

  names <- .quantile_names(probs)
  if (anyDuplicated(names)) {
    dup <- unique(names[duplicated(names)])
    stop("`probs` values that differ only beyond the printed precision would ",
         "share a summary column: ", paste(dup, collapse = ", "),
         ". Ask for probabilities that are distinguishable once written as a ",
         "percentage.", call. = FALSE)
  }

  invisible(TRUE)
}


#' The column name a quantile probability is reported under
#'
#' @noRd
.quantile_names <- function(probs) paste0("q", probs * 100)


#' Validate a scalar marginal-effect choice
#' @noRd
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
#' @noRd
.require_draw_columns <- function(draws, columns, context) {
  missing <- setdiff(columns, names(draws))
  if (length(missing) > 0L) {
    stop(sprintf("Missing %s draw column(s): %s.",
                 context, paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}
