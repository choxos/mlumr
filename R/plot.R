# Plotting methods for mlumr result objects:
#   plot(marginal_effects(fit))        -> forest of population-standardized effects
#   plot(predict(fit, type = "..."))   -> survival/hazard/cumhaz/loghr curves, etc.
#   plot(conditional_effects(fit, ...)) -> effects by covariate profile
#   plot_prior_posterior(fit)          -> prior-vs-posterior overlay
# Each returns a ggplot object so it composes with further ggplot2 layers.

# Lower/upper credible-interval column names for a summary data frame
# (`.summarize_draw_matrix()` writes qNN columns, e.g. q2.5 / q97.5).
#' @keywords internal
.ci_cols <- function(df) {
  qn <- grep("^q[0-9.]+$", names(df), value = TRUE)
  if (length(qn) < 2) {
    return(NULL)
  }
  num <- as.numeric(sub("^q", "", qn))
  list(lo = qn[which.min(num)], hi = qn[which.max(num)])
}

# Measures reported as natural ratios (null 1): the two exponentiated
# survival contrasts are included so their reference line is not drawn at 0.
#' @keywords internal
.ratio_measures <- c("RR", "HR", "TR", "RMSTR",
                     "EXP_DELTA_ETA", "EXP_ETA_CONTRAST")

# The additive counterparts (null 0). A label outside both lists is unknown,
# not a difference.
#' @keywords internal
.difference_measures <- c("RMSTD", "RD", "MD", "LINK_EFFECT", "LOR",
                          "LOG_HR", "LOG_TR", "DELTA_ETA", "ETA_CONTRAST")

#' Is this a label whose null the package can state?
#' @keywords internal
.known_measure <- function(effect) {
  toupper(effect) %in% c(.ratio_measures, .difference_measures)
}

# Null reference line implied by an effect label: 1 for ratio measures, 0 for
# differences and log-scale contrasts. Shared by both forest plots so a measure
# added to `.ratio_measures` is right in every figure at once.
#' @keywords internal
.null_ref_for <- function(effect) {
  ifelse(toupper(effect) %in% .ratio_measures, 1, 0)
}

# Coverage of the interval actually drawn, read off the quantile columns.
#' @keywords internal
.ci_label <- function(ci) {
  lo <- as.numeric(sub("^q", "", ci$lo))
  hi <- as.numeric(sub("^q", "", ci$hi))
  sprintf("%g%% credible interval", hi - lo)
}

# Ratio measures belong on a log axis, where reciprocal effects sit at equal
# distances from the null; ggplot2 applies one transform to the whole plot,
# so it is used only when every panel shows a ratio measure.
#' @keywords internal
.all_ratio_measures <- function(effects, values = NULL) {
  e <- toupper(unique(effects))
  if (!length(e) || !all(e %in% .ratio_measures)) return(FALSE)
  # A log axis needs strictly positive values. A ratio bound that has
  # underflowed to 0 cannot be drawn on one, and ggplot2 would warn and drop it
  # rather than show the interval, so keep the identity axis in that case.
  if (is.null(values)) return(TRUE)
  v <- values[is.finite(values)]
  length(v) > 0L && all(v > 0)
}

# A marginal hazard ratio is an estimand only with its evaluation time, so
# the time goes into the facet label and one panel cannot mix times.
#' @keywords internal
.effect_facet_labels <- function(df) {
  if (is.null(df$at_time)) return(df$effect)
  labs <- vapply(split(seq_len(nrow(df)), df$effect), function(idx) {
    times <- unique(df$at_time[idx])
    times <- times[!is.na(times)]
    eff <- df$effect[idx[1]]
    if (length(times) > 1L) {
      stop("One `", eff, "` panel cannot mix evaluation times (",
           paste(format(times, digits = 4L), collapse = ", "),
           "): a marginal hazard ratio is non-collapsible, so those are ",
           "different estimands. Plot them separately.", call. = FALSE)
    }
    if (length(times) == 0L) eff else sprintf("%s at t = %s", eff,
                                              format(times, digits = 4L))
  }, character(1))
  unname(labs[as.character(df$effect)])
}

#' Forest plot of population-standardized marginal effects
#'
#' Plots the point estimate and credible interval for each effect measure,
#' grouped by target population (index / comparator). The interval's coverage is
#' read from the quantile columns present, so it matches the `probs` the result
#' was summarized with. Panels showing only ratio measures are drawn on a log
#' axis, where reciprocal effects sit at equal distances from the null. The mlumr analogue of
#' `plot(multinma::relative_effects(fit))`.
#'
#' @param x A `marginal_effects()` result.
#' @param ref_line Numeric null-effect reference line. By default 0 for
#'   difference/log measures and 1 for natural ratio measures (RR), drawn per
#'   facet. Pass a single value to override for all panels.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @seealso [marginal_effects()]
#' @importFrom ggplot2 .data
#' @export
#' @examples
#' \dontrun{
#' plot(marginal_effects(fit, effect = "all"))
#' }
plot.mlumr_marginal_effects <- function(x, ref_line = NULL, ...) {
  df <- as.data.frame(x)
  ci <- .ci_cols(df)
  if (!all(c("mean", "effect", "population") %in% names(df))) {
    stop("Unexpected marginal_effects structure; cannot plot.", call. = FALSE)
  }
  # `probs` may name a single quantile, which is a valid summary with no
  # interval to draw. The other plot methods already fall back to points, so
  # rejecting it here made the same request plottable or not depending only on
  # which function produced it.
  if (!is.null(ci)) {
    df$.lo <- df[[ci$lo]]
    df$.hi <- df[[ci$hi]]
  }
  df$population <- factor(df$population, levels = unique(df$population))
  df$.facet <- .effect_facet_labels(df)
  df$.facet <- factor(df$.facet, levels = unique(df$.facet))

  # Per-facet null line: user override, else 1 for ratio measures, 0 otherwise.
  ref_df <- unique(df[, c("effect", ".facet"), drop = FALSE])
  ref_df$ref <- if (!is.null(ref_line)) {
    ref_line[1]
  } else {
    .null_ref_for(ref_df$effect)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$mean, y = .data$population)) +
    ggplot2::geom_vline(
      data = ref_df,
      ggplot2::aes(xintercept = .data$ref),
      linetype = "dashed", color = "gray55"
    )
  if (!is.null(ci)) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$.lo, xmax = .data$.hi),
      orientation = "y", width = 0.16, color = "#3B6B9A"
    )
  }
  p <- p +
    ggplot2::geom_point(size = 2.6, color = "#3B6B9A") +
    ggplot2::facet_wrap(~ .data$.facet, scales = "free_x") +
    ggplot2::labs(x = sprintf("Estimate (%s)",
                              if (is.null(ci)) "point estimate" else .ci_label(ci)),
                  y = NULL,
                  caption = .rmst_caption(x, df$effect)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  if (.all_ratio_measures(df$effect, c(df$mean, df$.lo, df$.hi))) {
    p <- p + ggplot2::scale_x_log10()
  }
  p
}


#' Caption naming the RMST restriction time, when the panel shows one
#'
#' An RMST value is defined only together with its restriction time, so a plot
#' that shows one has to name it: two forests drawn to different horizons look
#' comparable and are not.
#'
#' @param x The `mlumr_marginal_effects` object (carries an `rmst_horizon`
#'   attribute for survival fits).
#' @param effects The effect labels on the panel.
#' @return A caption string, or `NULL` when no RMST measure is shown.
#' @keywords internal
.rmst_caption <- function(x, effects) {
  tau <- attr(x, "rmst_horizon")
  if (is.null(tau) || !is.finite(tau)) return(NULL)
  if (!any(effects %in% c("RMSTD", "RMSTR"))) return(NULL)
  sprintf("RMST restricted to tau = %.4g", tau)
}

#' Restriction time behind an RMST prediction, if it carries one
#'
#' Predictions integrated to different horizons are different estimands and
#' are refused rather than drawn on one axis.
#'
#' @param x The `mlumr_prediction` object.
#' @param df Its data-frame form.
#' @return A single finite restriction time, or `NULL` when none is recorded.
#' @keywords internal
.prediction_rmst_horizon <- function(x, df) {
  tau <- if ("horizon" %in% names(df)) df$horizon else attr(x, "rmst_horizon")
  tau <- unique(tau[is.finite(tau)])
  if (length(tau) == 0L) return(NULL)
  if (length(tau) > 1L) {
    stop(sprintf(
      paste0("Cannot plot RMST predictions integrated to different horizons ",
             "(%s). RMST(tau) = integral of S(t) over [0, tau], so values at ",
             "different tau are different estimands and must not share an ",
             "axis. Refit or subset to one horizon."),
      paste(format(sort(tau), digits = 4L), collapse = ", ")
    ), call. = FALSE)
  }
  tau
}

#' Observed Kaplan-Meier layer for survival overlays
#'
#' Returns ggplot2 layers drawing the observed Kaplan-Meier step curves (the
#' index IPD and the reconstructed comparator pseudo-IPD) of a survival
#' `mlumr_data`, colored by treatment so they line up with a survival
#' prediction plot. The mlumr analogue of multinma's `geom_km()`, so a
#' predicted-versus-observed figure is just
#' `plot(predict(fit, type = "survival")) + geom_km(data)`.
#'
#' @param data An `mlumr_data` survival object from [combine_data()].
#' @param treatments Optional character vector of treatment labels to draw. By
#'   default both observed arms are drawn. This cannot separate the arms when
#'   both carry the same label; use `population` there. A label that names no
#'   observed arm is refused rather than drawn as nothing.
#' @param population Optional cohort to draw, `"Index"` and/or `"Comparator"`.
#'   Selects the arm itself rather than its display name, so
#'   `population = "Comparator"` overlays only the comparator KM on a
#'   comparator-population prediction whatever the treatments are called. Only
#'   the selected cohorts are examined: a left- or interval-censored
#'   observation in a cohort that is not drawn does not stop the plot, and one
#'   in a cohort that is drawn refuses it.
#' @param marks Logical; draw censoring marks (default `TRUE`).
#' @param linewidth Step line width (default `0.4`).
#' @param ... Passed to [ggplot2::geom_step()].
#' @return A list of ggplot2 layers (a step layer, plus a censoring-mark layer
#'   when `marks = TRUE`) to add to a plot with `+`. The layers carry the
#'   population each observed arm was measured in, so on a plot faceted by
#'   population each curve appears only in its own panel. A plot standardized to
#'   a `newdata` target therefore shows no observed curve, which is correct:
#'   no arm was observed in that population.
#' @seealso [plot.mlumr_prediction()]
#' @export
#' @examples
#' \dontrun{
#' plot(predict(fit, type = "survival")) + geom_km(dat)
#' plot(predict(fit, type = "survival")) + geom_km(dat, population = "Comparator")
#' }
geom_km <- function(data, treatments = NULL, population = NULL, marks = TRUE,
                    linewidth = 0.4, ...) {
  .validate_km_data(data)
  # The cohorts are chosen before anything is examined. `population` selects
  # the cohort itself, which `treatments` cannot when both arms share a label.
  selected <- if (is.null(population)) {
    c("Index", "Comparator")
  } else if (is.character(population) && length(population) &&
               all(population %in% c("Index", "Comparator"))) {
    unique(population)
  } else {
    stop("`population` must be \"Index\", \"Comparator\", or both.",
         call. = FALSE)
  }
  if (!is.null(treatments)) {
    labels <- c(Index = data$index_treatment,
                Comparator = data$comparator_treatment)
    if (identical(labels[["Index"]], labels[["Comparator"]]) &&
          is.null(population)) {
      warning("Both arms are labelled '", labels[["Index"]],
              "', so `treatments` cannot tell them apart and selects both. ",
              "Use `population = \"Index\"` or `population = \"Comparator\"`.",
              call. = FALSE)
    }
    unknown <- setdiff(treatments, labels)
    if (length(unknown)) {
      stop("`treatments` names an arm that was not observed: ",
           paste0("'", unknown, "'", collapse = ", "), ". The observed arms ",
           "are labelled ", paste0("'", unique(labels), "'", collapse = " and "),
           ".", call. = FALSE)
    }
    selected <- selected[labels[selected] %in% treatments]
    if (!length(selected)) {
      stop("`population` and `treatments` select no arm in common: the ",
           paste(population, collapse = " and "), " cohort",
           if (length(population) > 1L) "s are" else " is", " labelled ",
           paste0("'", unique(labels[population]), "'", collapse = " and "),
           ".", call. = FALSE)
    }
  }
  km <- .km_observed(data, selected)
  # Group on the population: two arms with one label would otherwise be one
  # ggplot2 group and geom_step() would join their points into one curve.
  layers <- list(
    ggplot2::geom_step(
      data = km$steps,
      ggplot2::aes(x = .data$time, y = .data$surv, color = .data$treatment,
                   group = .data$population),
      inherit.aes = FALSE, linewidth = linewidth, ...
    )
  )
  if (isTRUE(marks) && nrow(km$censor) > 0L) {
    layers <- c(layers, list(
      ggplot2::geom_point(
        data = km$censor,
        ggplot2::aes(x = .data$time, y = .data$surv, color = .data$treatment,
                     group = .data$population),
        inherit.aes = FALSE, shape = 3, size = 1.6, show.legend = FALSE
      )
    ))
  }
  layers
}

#' Refuse anything but a survival `mlumr_data` for `geom_km()`
#' @keywords internal
.validate_km_data <- function(data) {
  if (!inherits(data, "mlumr_data") || (data$family %||% "") != "survival") {
    stop("`geom_km()` requires a survival `mlumr_data` object from combine_data().",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Observed Kaplan-Meier step + censoring data for a survival mlumr_data
#'
#' Each selected cohort is fitted on its own, so a single cohort is a single
#' curve and nothing depends on the `strata` a multi-curve fit carries, and
#' the two cohorts stay apart when their treatment labels coincide.
#' @param data A survival `mlumr_data`.
#' @param population The cohorts to fit, `"Index"` and/or `"Comparator"`.
#' @keywords internal
.km_observed <- function(data, population = c("Index", "Comparator")) {
  .validate_km_data(data)
  cohort <- function(df, treatment, label) {
    entry <- if (!is.null(df$.delay_time)) df$.delay_time else rep(0, nrow(df))
    data.frame(entry = entry, time = df$.time, status = df$.status,
               treatment = treatment, population = label)
  }
  frames <- list(
    Index = cohort(data$ipd$data, data$index_treatment, "Index"),
    Comparator = cohort(data$agd$pseudo_ipd, data$comparator_treatment,
                        "Comparator")
  )[population]
  # A right-censored Kaplan-Meier curve cannot represent status 2 or 3.
  unsupported <- names(frames)[vapply(frames, function(o) {
    any(o$status %in% c(2L, 3L))
  }, logical(1L))]
  if (length(unsupported)) {
    stop("`geom_km()` draws a right-censored Kaplan-Meier curve and cannot ",
         "represent left- or interval-censored observations (internal status ",
         "2 or 3), which the ", paste(unsupported, collapse = " and "),
         " cohort", if (length(unsupported) > 1L) "s have" else " has",
         ". Select a cohort without them with `population`, or use an ",
         "interval-censored estimator.", call. = FALSE)
  }
  # Counting-process Surv() under delayed entry. One fit per population, not
  # per treatment label, and the population column is kept on every row so a
  # faceted plot draws each curve in its own panel.
  fit_one <- function(o) {
    sf <- if (any(o$entry > 0)) {
      survival::survfit(survival::Surv(entry, time, status) ~ 1, data = o)
    } else {
      survival::survfit(survival::Surv(time, status) ~ 1, data = o)
    }
    steps <- data.frame(time = sf$time, surv = sf$surv,
                        treatment = o$treatment[[1L]],
                        population = o$population[[1L]])
    # Censoring marks are read off the fitted rows, before the origin is added.
    cens <- if (!is.null(sf$n.censor)) {
      steps[sf$n.censor > 0, , drop = FALSE]
    } else {
      steps[0, , drop = FALSE]
    }
    list(steps = steps, censor = cens)
  }
  fits <- lapply(frames, fit_one)
  steps <- do.call(rbind, lapply(fits, `[[`, "steps"))
  cens <- do.call(rbind, lapply(fits, `[[`, "censor"))
  rownames(cens) <- NULL
  # Start each curve at (0, 1), as survival::survfit0() would.
  origin <- unique(steps[, c("treatment", "population"), drop = FALSE])
  origin$time <- 0
  origin$surv <- 1
  steps <- rbind(origin[, names(steps), drop = FALSE], steps)
  steps <- steps[order(steps$treatment, steps$time), , drop = FALSE]
  rownames(steps) <- NULL
  list(steps = steps, censor = cens)
}

#' Refuse a prediction frame whose two arms cannot be told apart
#'
#' Colour and fill are keyed on `treatment`, so two arms with one label fall
#' into one ggplot2 group and the line joins two different predictions.
#' @keywords internal
.reject_ambiguous_series <- function(x) {
  df <- as.data.frame(x)
  key <- intersect(c("treatment", "population", "time"), names(df))
  if (!all(c("treatment", "population") %in% key)) return(invisible())
  # Repeated requests draw the same point twice and are not an ambiguity;
  # a key that still repeats after collapsing is two values under one label.
  drawn <- unique(df[, setdiff(names(df), "requested_time"), drop = FALSE])
  # `anyDuplicated()` gives the row where the key first repeats.
  clash <- anyDuplicated(drawn[, key, drop = FALSE])
  if (clash) {
    stop("This prediction has two series per population that share the ",
         "treatment label '", drawn$treatment[clash], "', so they cannot be drawn ",
         "as separate curves. Give the two arms distinct treatment names in ",
         "set_ipd() / set_agd_surv() and refit.", call. = FALSE)
  }
  invisible()
}

#' Plot absolute predictions from a fitted ML-UMR model
#'
#' Dispatches on the prediction `type`: time-indexed types
#' (`"survival"`, `"hazard"`, `"cumhaz"`, `"loghr"`) are drawn as curves with
#' credible bands at the coverage the result was summarized with; scalar types
#' (`"rmst"`, `"median"`, `"response"`) as
#' point-intervals. The mlumr analogue of `plot(predict(multinma_fit))`.
#'
#' @param x A `predict()` result (an `mlumr_prediction`).
#' @param ref_line Optional numeric null-reference line(s). Drawn as horizontal
#'   line(s) for curve types and vertical line(s) for scalar types, mirroring the
#'   `plot(predict(fit), ref_line = c(0, 1))` idiom (e.g. probability bounds for
#'   `type = "response"`). The log-hazard-ratio curve defaults to `ref_line = 0`.
#' @param ... Unused.
#' @return A `ggplot` object (compose further layers, e.g. a KM overlay, with `+`).
#' @seealso [predict.mlumr_fit()], [geom_km()]
#' @export
#' @examples
#' \dontrun{
#' plot(predict(fit, type = "survival")) + geom_km(dat)
#' plot(predict(fit, type = "response"), ref_line = c(0, 1))
#' plot(predict(fit, type = "loghr"))
#' }
plot.mlumr_prediction <- function(x, ref_line = NULL, ...) {
  .reject_ambiguous_series(x)
  df <- as.data.frame(x)
  ptype <- attr(x, "ptype") %||% "response"
  ci <- .ci_cols(df)
  has_time <- "time" %in% names(df)
  has_trt <- "treatment" %in% names(df)
  has_pop <- "population" %in% names(df)
  if (!is.null(ci)) {
    df$.lo <- df[[ci$lo]]
    df$.hi <- df[[ci$hi]]
  }

  if (has_time && ptype %in% c("survival", "hazard", "cumhaz", "loghr")) {
    ylab <- switch(ptype,
      survival = "Survival probability",
      hazard = "Marginal hazard",
      cumhaz = "Cumulative hazard",
      loghr = "Marginal log hazard ratio"
    )
    aes_base <- if (has_trt) {
      ggplot2::aes(
        x = .data$time, y = .data$mean,
        color = .data$treatment, fill = .data$treatment
      )
    } else {
      ggplot2::aes(x = .data$time, y = .data$mean)
    }
    p <- ggplot2::ggplot(df, aes_base)
    if (!is.null(ci)) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$.lo, ymax = .data$.hi),
        alpha = 0.18, color = NA
      )
    }
    p <- p + ggplot2::geom_line(linewidth = 0.7)
    if (ptype == "survival") {
      p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
    }
    # Default 0 for the log hazard ratio, else whatever the caller passed.
    rl <- if (is.null(ref_line) && ptype == "loghr") 0 else ref_line
    if (!is.null(rl)) {
      p <- p + ggplot2::geom_hline(
        yintercept = rl, linetype = "dashed",
        color = "gray55"
      )
    }
    if (has_pop) p <- p + ggplot2::facet_wrap(~ .data$population)
    p <- p + ggplot2::labs(x = "Time", y = ylab, color = NULL, fill = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")
    return(p)
  }

  # Scalar predictions: point-interval by treatment, populations dodged.
  yvar <- if (has_trt) "treatment" else names(df)[1]
  xlab <- switch(ptype,
    response = "Predicted response",
    rmst = "Restricted mean survival time",
    median = "Median survival",
    ptype
  )
  # Name tau on the plot itself.
  cap <- NULL
  if (identical(ptype, "rmst")) {
    tau <- .prediction_rmst_horizon(x, df)
    if (!is.null(tau)) {
      cap <- sprintf("RMST restricted to tau = %.4g", tau)
      xlab <- sprintf("Restricted mean survival time (tau = %.4g)", tau)
    }
  }
  # A median summary is conditional on the median being reached; say so.
  if (identical(ptype, "median") && !is.null(df$p_not_reached) &&
        any(df$p_not_reached > 0, na.rm = TRUE)) {
    worst <- max(df$p_not_reached, na.rm = TRUE)
    cap <- sprintf(paste0("Conditional on the median being reached on the ",
                          "prediction grid; up to %.0f%% of draws never reach ",
                          "it and are excluded."), 100 * worst)
    xlab <- paste0(xlab, " (conditional)")
  }
  dodge <- ggplot2::position_dodge(width = 0.5)
  base_aes <- if (has_pop) {
    ggplot2::aes(x = .data$mean, y = .data[[yvar]], color = .data$population)
  } else {
    ggplot2::aes(x = .data$mean, y = .data[[yvar]])
  }
  p <- ggplot2::ggplot(df, base_aes)
  if (!is.null(ref_line)) {
    p <- p + ggplot2::geom_vline(
      xintercept = ref_line, linetype = "dashed",
      color = "gray55"
    )
  }
  ci_aes <- ggplot2::aes(xmin = .data$.lo, xmax = .data$.hi)
  if (has_pop) {
    if (!is.null(ci)) {
      p <- p + ggplot2::geom_errorbar(ci_aes, orientation = "y", width = 0.16,
                                      position = dodge)
    }
    p <- p + ggplot2::geom_point(size = 2.6, position = dodge)
  } else {
    if (!is.null(ci)) {
      p <- p + ggplot2::geom_errorbar(ci_aes, orientation = "y", width = 0.16,
                                      color = "#3B6B9A")
    }
    p <- p + ggplot2::geom_point(size = 2.6, color = "#3B6B9A")
  }
  p <- p + ggplot2::labs(x = xlab, y = NULL, color = NULL, caption = cap) +
    ggplot2::theme_minimal(base_size = 11)
  if (has_pop) p <- p + ggplot2::theme(legend.position = "bottom")
  p
}

#' Plot covariate-conditional treatment effects
#'
#' Point-interval of the conditional effect at each covariate profile.
#'
#' @param x A `conditional_effects()` result.
#' @param ref_line Numeric null-effect reference line. By default it is chosen
#'   per facet from the effect label: 1 for the natural-ratio measures (RR, HR,
#'   TR, and the exponentiated contrast reported when the study baselines
#'   differ) and 0 for the additive ones (RD, MD, LINK_EFFECT). Pass a single
#'   value to override for all panels.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @seealso [conditional_effects()]
#' @export
plot.mlumr_conditional_effects <- function(x, ref_line = NULL, ...) {
  df <- as.data.frame(x)
  ci <- .ci_cols(df)
  yvar <- if ("profile" %in% names(df)) "profile" else names(df)[1]
  df[[yvar]] <- factor(df[[yvar]], levels = unique(df[[yvar]]))
  # Per-facet null line.
  has_effect <- "effect" %in% names(df)
  ref_df <- if (has_effect) {
    data.frame(effect = unique(df$effect), stringsAsFactors = FALSE)
  } else {
    data.frame(effect = NA_character_, stringsAsFactors = FALSE)
  }
  ref_df$ref <- if (!is.null(ref_line)) {
    ref_line[1]
  } else if (has_effect) {
    .null_ref_for(ref_df$effect)
  } else {
    0
  }
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$mean, y = .data[[yvar]]))
  p <- p + if (has_effect) {
    ggplot2::geom_vline(data = ref_df, ggplot2::aes(xintercept = .data$ref),
                        linetype = "dashed", color = "gray55")
  } else {
    ggplot2::geom_vline(xintercept = ref_df$ref[1], linetype = "dashed",
                        color = "gray55")
  }
  if (!is.null(ci)) {
    # Mapped to the quantile columns directly: ggplot() has captured `df`.
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[[ci$lo]], xmax = .data[[ci$hi]]),
      orientation = "y", width = 0.16, color = "#3B6B9A"
    )
  }
  if ("effect" %in% names(df) && length(unique(df$effect)) > 1) {
    p <- p + ggplot2::facet_wrap(~ .data$effect, scales = "free_x")
  }
  p <- p + ggplot2::geom_point(size = 2.6, color = "#3B6B9A") +
    ggplot2::labs(x = sprintf("Conditional effect (%s)",
                              if (is.null(ci)) "point estimate" else .ci_label(ci)),
                  y = "Covariate profile") +
    ggplot2::theme_minimal(base_size = 11)
  # Same log-axis rule as the marginal forest.
  ci_vals <- if (is.null(ci)) NULL else c(df[[ci$lo]], df[[ci$hi]])
  if (has_effect && .all_ratio_measures(df$effect, c(df$mean, ci_vals))) {
    p <- p + ggplot2::scale_x_log10()
  }
  p
}

#' Prior a fitted parameter was actually given
#'
#' Each parameter is mapped to the prior the fit records for it, with the
#' Stan `<lower=0>` constraint carried along for the truncated density.
#'
#' @param object An `mlumr_fit`.
#' @param par One draw column name.
#' @return A list with `prior` (a prior specification) and `lower` (the support
#'   bound), or `NULL` when the fit records no prior for that parameter.
#' @keywords internal
.parameter_prior <- function(object, par) {
  priors <- object$priors %||% list()
  base <- sub("\\[[0-9]+\\]$", "", par)
  idx <- suppressWarnings(as.integer(sub("^.*\\[([0-9]+)\\]$", "\\1", par)))
  if (base %in% c("mu_index", "mu_comparator")) {
    return(list(prior = priors$intercept, lower = -Inf))
  }
  # Stan declares these <lower=0>, which truncates rather than folds.
  if (base == "sigma") return(list(prior = priors$sigma, lower = 0))
  if (base %in% c("aux_val", "aux_val_cmp")) {
    return(list(prior = priors$aux, lower = 0))
  }
  # The second generalized-gamma shape has its own prior; fall back only for
  # fits stored before `aux2` existed.
  if (base %in% c("aux2_val", "aux2_val_cmp")) {
    return(list(prior = priors$aux2 %||% priors$aux, lower = 0))
  }
  if (base == "sigma_smooth") return(list(prior = priors$smooth, lower = 0))
  res <- if (base %in% c("beta", "beta_index")) {
    priors$beta_resolved
  } else if (base == "beta_comparator") {
    # The relaxed models give the comparator coefficients `beta`'s prior
    # unless a comparator-specific one was resolved.
    priors$beta_comparator_resolved %||% priors$beta_resolved
  } else {
    NULL
  }
  if (!is.null(res) && !is.na(idx) && idx >= 1L && idx <= length(res$mean)) {
    # Post-autoscaling: the prior the sampler saw.
    return(list(
      prior = list(distribution = if (isTRUE(res$dist == 1L)) "student_t" else "normal",
                   mean = res$mean[idx], sd = res$sd[idx], df = res$df),
      lower = -Inf
    ))
  }
  NULL
}

#' Density function of a prior specification, truncated at `lower`
#' @keywords internal
.prior_density_fun <- function(pr, lower = -Inf) {
  if (is.null(pr)) return(NULL)
  dist <- pr$distribution %||% "normal"
  m <- pr$mean %||% 0
  sd <- pr$sd %||% 10
  df <- pr$df
  base <- switch(
    dist,
    normal = function(z) stats::dnorm(z, mean = m, sd = sd),
    student_t = function(z) stats::dt((z - m) / sd, df = df) / sd,
    cauchy = function(z) stats::dcauchy(z, location = m, scale = sd),
    exponential = function(z) stats::dexp(z, rate = pr$rate %||% (1 / sd)),
    NULL
  )
  if (is.null(base)) return(NULL)
  if (!is.finite(lower) || identical(dist, "exponential")) return(base)
  # A <lower=0> declaration truncates and renormalizes the prior.
  mass <- switch(
    dist,
    normal = stats::pnorm(lower, mean = m, sd = sd, lower.tail = FALSE),
    student_t = stats::pt((lower - m) / sd, df = df, lower.tail = FALSE),
    cauchy = stats::pcauchy(lower, location = m, scale = sd, lower.tail = FALSE),
    1
  )
  if (!is.finite(mass) || mass <= 0) return(NULL)
  function(z) ifelse(z < lower, 0, base(z) / mass)
}

#' Central mass of a prior, for choosing a plotting window
#'
#' Returns the interval holding the prior's central 99%, truncated at `lower`
#' when the parameter is constrained. `NA`-free and finite: a Cauchy has no
#' variance but its quantiles exist, and an unrecognized prior returns an empty
#' range so the caller keeps the posterior window.
#' @keywords internal
.prior_quantile_range <- function(pr, lower = -Inf) {
  if (is.null(pr)) return(c(Inf, -Inf))
  dist <- pr$distribution %||% "normal"
  m <- pr$mean %||% 0
  sd <- pr$sd %||% 10
  df <- pr$df
  rate <- pr$rate %||% (1 / sd)
  qfun <- switch(
    dist,
    normal = function(pp) stats::qnorm(pp, mean = m, sd = sd),
    student_t = function(pp) m + sd * stats::qt(pp, df = df),
    cauchy = function(pp) stats::qcauchy(pp, location = m, scale = sd),
    exponential = function(pp) stats::qexp(pp, rate = rate),
    NULL
  )
  pfun <- switch(
    dist,
    normal = function(x) stats::pnorm(x, mean = m, sd = sd),
    student_t = function(x) stats::pt((x - m) / sd, df = df),
    cauchy = function(x) stats::pcauchy(x, location = m, scale = sd),
    exponential = function(x) stats::pexp(x, rate = rate),
    NULL
  )
  if (is.null(qfun) || is.null(pfun)) return(c(Inf, -Inf))
  p <- c(0.005, 0.995)
  if (is.finite(lower)) {
    # Quantiles of the truncated prior, which is the density drawn.
    f_lo <- pfun(lower)
    if (!is.finite(f_lo) || f_lo >= 1) return(c(lower, lower))
    p <- f_lo + p * (1 - f_lo)
  }
  q <- qfun(p)
  if (any(!is.finite(q))) return(c(Inf, -Inf))
  if (is.finite(lower)) q[1] <- max(q[1], lower)
  sort(q)
}

#' Prior-versus-posterior overlay
#'
#' Plots the posterior density of the named parameters with their prior density
#' overlaid, reading the prior from the fit. The mlumr analogue of
#' `multinma::plot_prior_posterior()`. Intended for parameters with a known,
#' un-autoscaled prior (the treatment intercepts `mu_index` / `mu_comparator`
#' under the default `prior_intercept`).
#'
#' @param object An `mlumr_fit`.
#' @param pars Character vector of parameter (draw column) names. Default the
#'   treatment intercepts `c("mu_index", "mu_comparator")`.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @seealso [prior_summary()], [mlumr()]
#' @export
#' @examples
#' \dontrun{
#' plot_prior_posterior(fit)
#' plot_prior_posterior(fit, pars = c("mu_index", "mu_comparator"))
#' }
plot_prior_posterior <- function(object, pars = c("mu_index", "mu_comparator"),
                                 ...) {
  .validate_mlumr_fit_object(object)
  draws <- object$draws
  # Every name has to be in the draws.
  missing_pars <- setdiff(pars, colnames(draws))
  if (length(missing_pars)) {
    stop("Not in the fit's posterior draws: ",
         paste(missing_pars, collapse = ", "),
         ". plot_prior_posterior() draws every parameter it is asked for or ",
         "none of them.", call. = FALSE)
  }
  if (!length(pars)) {
    stop("None of `pars` are in the fit's posterior draws.", call. = FALSE)
  }
  # Each parameter gets the prior the fit records for it.
  resolved <- lapply(pars, function(p) .parameter_prior(object, p))
  names(resolved) <- pars
  unknown <- pars[vapply(resolved, function(r) is.null(r) || is.null(r$prior),
                         logical(1))]
  if (length(unknown)) {
    stop("No prior is recorded on the fit for parameter(s) ",
         paste(unknown, collapse = ", "),
         ". plot_prior_posterior() draws each parameter against its own prior ",
         "and will not substitute another one; use prior_summary() to see ",
         "which priors this fit carries.", call. = FALSE)
  }

  long <- do.call(rbind, lapply(pars, function(p) {
    data.frame(
      parameter = p, value = as.numeric(draws[[p]]),
      stringsAsFactors = FALSE
    )
  }))
  # The prior curves are evaluated per parameter over its own range.
  prior_df <- do.call(rbind, lapply(pars, function(p) {
    v <- as.numeric(draws[[p]])
    r <- resolved[[p]]
    fun <- .prior_density_fun(r$prior, r$lower)
    if (is.null(fun)) {
      stop("The prior recorded for `", p, "` has no density this function can ",
           "draw.", call. = FALSE)
    }
    lo <- min(v)
    hi <- max(v)
    pad <- 0.15 * (hi - lo)
    # Widen the grid to cover the prior's central mass as well.
    pq <- .prior_quantile_range(r$prior, r$lower)
    lo <- min(lo - pad, pq[1])
    hi <- max(hi + pad, pq[2])
    grid <- seq(max(lo, r$lower), hi, length.out = 512)
    data.frame(parameter = p, value = grid, density = fun(grid),
               stringsAsFactors = FALSE)
  }))

  ggplot2::ggplot(long, ggplot2::aes(.data$value)) +
    ggplot2::geom_density(ggplot2::aes(color = "posterior"),
      fill = "#3B6B9A", alpha = 0.15, linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = prior_df,
      ggplot2::aes(x = .data$value, y = .data$density, color = "prior"),
      inherit.aes = FALSE, linetype = "dashed"
    ) +
    ggplot2::facet_wrap(~ .data$parameter, scales = "free") +
    ggplot2::scale_color_manual(values = c(posterior = "#3B6B9A", prior = "gray45")) +
    ggplot2::labs(x = NULL, y = "Density", color = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

#' Forest plot of a small set of estimates
#'
#' A one-call forest plot for comparing a handful of estimates supplied as a data
#' frame, e.g. several methods (naive / STC / ML-UMR) or several covariate
#' profiles. Rows are drawn top-to-bottom in the order given. Returns a `ggplot`
#' object, so further ggplot2 layers compose with `+`. This keeps the
#' method-comparison forests in the vignettes to a single line instead of a
#' hand-built `ggplot()` stack.
#'
#' All rows share one axis, so they must be on one effect scale: a frame with an
#' `effect` column naming more than one measure is rejected rather than drawn
#' against a single reference that cannot be right for both.
#'
#' @param data A data frame with one row per estimate. Columns are matched
#'   flexibly (first match wins): the row label from `label` / `method` /
#'   `Method` / `Comparison` (else the first character/factor column); the point
#'   estimate from `est` / `estimate` / `mean`; the interval bounds from
#'   `lo`/`hi`, `q2.5`/`q97.5`, `ci_lower`/`ci_upper`, `conf.low`/`conf.high`, or
#'   `lower`/`upper`.
#' @param ref_line Null-effect reference line. By default it is read from the
#'   `effect` column when the label is one the package produces (`1` for a
#'   ratio measure, `0` for a difference); otherwise `1` when `log_x = TRUE`
#'   and `0` otherwise. Pass it explicitly for a measure this does not name.
#'   Kept inside the clipping window.
#' @param log_x Draw the x axis on a log10 scale (for ratio measures).
#' @param x,title,subtitle Axis label and titles (passed to [ggplot2::labs()]).
#' @param color Point and interval color.
#' @param clip Logical; if `TRUE` (default), when one or two intervals are far
#'   wider than the rest the x axis is clipped to the bulk of the estimates and
#'   the over-wide interval's clipped end(s) are drawn with an arrow, so a single
#'   very uncertain estimate does not compress all the others into a sliver.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @seealso [marginal_effects()], [naive()], [stc()]
#' @importFrom ggplot2 .data
#' @export
#' @examples
#' \dontrun{
#' forest_df <- data.frame(
#'   label = c("Naive", "STC", "ML-UMR SPFA"),
#'   est = c(res_naive$estimate, res_stc$estimate, me$mean),
#'   lo = c(res_naive$ci_lower, res_stc$ci_lower, me$q2.5),
#'   hi = c(res_naive$ci_upper, res_stc$ci_upper, me$q97.5)
#' )
#' mlumr_forest(forest_df, ref_line = 0, x = "Log odds ratio")
#' }
mlumr_forest <- function(data, ref_line = NULL, log_x = FALSE,
                         x = NULL, title = NULL, subtitle = NULL,
                         color = "#3B6B9A", clip = TRUE, ...) {
  df <- as.data.frame(data)
  # One axis carries one scale; labels are compared case-insensitively, as
  # `.null_ref_for()` reads them.
  effect_key <- if ("effect" %in% names(df)) {
    toupper(as.character(df$effect))
  } else {
    character(0)
  }
  if (length(effect_key) && length(unique(effect_key)) > 1L) {
    stop("mlumr_forest() draws one axis, so every row must be on the same ",
         "effect scale; this frame mixes ",
         paste(unique(df$effect), collapse = ", "),
         ". Split it, or drop the `effect` column if the rows really are ",
         "comparable.", call. = FALSE)
  }
  # The null belongs to the measure; the axis is only a hint without a label.
  if (is.null(ref_line)) {
    known <- "effect" %in% names(df) && length(df$effect) &&
      .known_measure(df$effect[[1]])
    ref_line <- if (known) {
      .null_ref_for(df$effect[[1]])
    } else if (isTRUE(log_x)) {
      1
    } else {
      0
    }
  }
  if (isTRUE(log_x) && any(ref_line <= 0)) {
    stop("`ref_line` must be positive when `log_x = TRUE`: a log axis has no ",
         "position for zero or a negative value.", call. = FALSE)
  }
  pick <- function(cands, what) {
    hit <- intersect(cands, names(df))
    if (!length(hit)) {
      stop(sprintf(
        "mlumr_forest(): no %s column (looked for %s).",
        what, paste(cands, collapse = ", ")
      ), call. = FALSE)
    }
    df[[hit[1]]]
  }
  lab_hit <- intersect(c("label", "method", "Method", "Comparison"), names(df))
  labels <- if (length(lab_hit)) {
    df[[lab_hit[1]]]
  } else {
    chr <- names(df)[vapply(
      df, function(z) is.character(z) || is.factor(z),
      logical(1)
    )]
    if (length(chr)) df[[chr[1]]] else as.character(seq_len(nrow(df)))
  }
  pdat <- data.frame(
    .label = factor(as.character(labels), levels = rev(unique(as.character(labels)))),
    .est = pick(c("est", "estimate", "mean"), "point-estimate"),
    .lo = pick(c("lo", "q2.5", "ci_lower", "conf.low", "lower"), "lower-bound"),
    .hi = pick(c("hi", "q97.5", "ci_upper", "conf.high", "upper"), "upper-bound")
  )

  # Clip an over-wide interval to the bulk of the estimates, with an arrow at
  # the clipped end, on the plotted scale.
  fwd <- if (isTRUE(log_x)) function(z) log10(z) else function(z) z
  inv <- if (isTRUE(log_x)) function(z) 10^z else function(z) z
  lim <- .forest_clip_range(fwd(pdat$.est), fwd(pdat$.lo), fwd(pdat$.hi), clip)
  # Keep the null line inside the clipped window.
  if (!is.null(lim)) {
    ref_f <- fwd(ref_line[is.finite(ref_line)])
    ref_f <- ref_f[is.finite(ref_f)]
    if (length(ref_f)) {
      lim <- c(min(lim[1], min(ref_f)), max(lim[2], max(ref_f)))
    }
  }

  pdat$.dlo <- pdat$.lo
  pdat$.dhi <- pdat$.hi
  pdat$.alo <- pdat$.lo
  pdat$.ahi <- pdat$.hi
  pdat$.clo <- FALSE
  pdat$.chi <- FALSE
  xlim_n <- NULL
  if (!is.null(lim)) {
    flo <- fwd(pdat$.lo)
    fhi <- fwd(pdat$.hi)
    # An infinite bound is clipped with an arrow; a missing one leaves the row
    # drawn as its point estimate alone, never as half an interval.
    have_both <- !is.na(pdat$.lo) & !is.na(pdat$.hi)
    pdat$.clo <- have_both & (!is.finite(flo) | flo < lim[1])
    pdat$.chi <- have_both & (!is.finite(fhi) | fhi > lim[2])
    dlo_w <- ifelse(have_both,
                    ifelse(is.finite(flo), pmax(flo, lim[1]), lim[1]),
                    NA_real_)
    dhi_w <- ifelse(have_both,
                    ifelse(is.finite(fhi), pmin(fhi, lim[2]), lim[2]),
                    NA_real_)
    alen <- 0.10 * (lim[2] - lim[1])
    pdat$.dlo <- inv(dlo_w)
    pdat$.dhi <- inv(dhi_w)
    pdat$.alo <- inv(dlo_w + alen)
    pdat$.ahi <- inv(dhi_w - alen)
    xlim_n <- inv(lim)
  }

  ar <- ggplot2::arrow(length = ggplot2::unit(6, "pt"), type = "closed")
  p <- ggplot2::ggplot(pdat, ggplot2::aes(y = .data$.label)) +
    ggplot2::geom_vline(
      xintercept = ref_line, linetype = "dashed", color = "gray55"
    ) +
    ggplot2::geom_segment(
      # A row with no interval contributes no segment.
      data = pdat[!is.na(pdat$.dlo) & !is.na(pdat$.dhi), , drop = FALSE],
      ggplot2::aes(x = .data$.dlo, xend = .data$.dhi,
                   y = .data$.label, yend = .data$.label),
      color = color, linewidth = 0.6
    )
  if (any(pdat$.clo)) {
    p <- p + ggplot2::geom_segment(
      data = pdat[pdat$.clo, , drop = FALSE],
      ggplot2::aes(x = .data$.alo, xend = .data$.dlo,
                   y = .data$.label, yend = .data$.label),
      color = color, linewidth = 0.6, arrow = ar
    )
  }
  if (any(pdat$.chi)) {
    p <- p + ggplot2::geom_segment(
      data = pdat[pdat$.chi, , drop = FALSE],
      ggplot2::aes(x = .data$.ahi, xend = .data$.dhi,
                   y = .data$.label, yend = .data$.label),
      color = color, linewidth = 0.6, arrow = ar
    )
  }
  p <- p +
    ggplot2::geom_point(ggplot2::aes(x = .data$.est), size = 2.6, color = color) +
    ggplot2::labs(x = x, y = NULL, title = title, subtitle = subtitle) +
    ggplot2::theme_minimal(base_size = 11)
  if (isTRUE(log_x)) p <- p + ggplot2::scale_x_log10()
  if (!is.null(xlim_n)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim_n)
  }
  p
}

# Robust x-range for a forest plot. Inputs are on the plotted scale (log10 for a
# ratio axis). Returns c(lo, hi) to clip to when one or two intervals are far
# wider than the rest, else NULL (no clipping). The range covers every point
# estimate plus the bounds of the "typical" (non-outlier) intervals.
#' @keywords internal
.forest_clip_range <- function(est, lo, hi, clip = TRUE) {
  if (!isTRUE(clip)) {
    return(NULL)
  }
  ok <- is.finite(est) & is.finite(lo) & is.finite(hi)
  if (sum(ok) < 3) {
    return(NULL)
  }
  w <- (hi - lo)[ok]
  medw <- stats::median(w)
  if (!is.finite(medw) || medw <= 0) {
    return(NULL)
  }
  outlier <- w > 5 * medw
  if (!any(outlier)) {
    return(NULL)
  }
  keep <- ok
  keep[which(ok)[outlier]] <- FALSE
  # Every finite point estimate, so a row without an interval stays in view.
  rng <- range(c(est[is.finite(est)], lo[keep], hi[keep]), na.rm = TRUE)
  if (!all(is.finite(rng)) || isTRUE(rng[1] == rng[2])) {
    return(NULL)
  }
  pad <- 0.05 * (rng[2] - rng[1])
  c(rng[1] - pad, rng[2] + pad)
}
