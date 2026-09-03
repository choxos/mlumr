# Plotting methods for mlumr result objects:
#   plot(marginal_effects(fit))        -> forest of population-standardized effects
#   plot(predict(fit, type = "..."))   -> survival/hazard/cumhaz/loghr curves, etc.
#   plot(conditional_effects(fit, ...)) -> effects by covariate profile
#   plot_prior_posterior(fit)          -> prior-vs-posterior overlay
# Each returns a ggplot object so it composes with further ggplot2 layers.


#' @keywords internal
.need_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Plotting requires the 'ggplot2' package.", call. = FALSE)
  }
}

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

# Measures reported as natural ratios (null = 1) rather than differences/logs:
# poisson rate ratio (RR), the survival hazard ratio (HR) / time ratio (TR) /
# RMST ratio (RMSTR), and the two exponentiated survival contrasts that are
# reported when the study baselines differ and so cannot be called HR or TR:
# EXP_DELTA_ETA (marginal, from marginal_effects()) and EXP_ETA_CONTRAST
# (conditional, from conditional_effects()). Both are exp() of a difference and
# therefore have null 1; omitting them drew a reference line at 0 on a ratio
# axis. The RMST difference (RMSTD), risk difference (RD), mean difference (MD)
# and link-scale contrast (LINK_EFFECT) are additive (null 0).
#' @keywords internal
.ratio_measures <- c("RR", "HR", "TR", "RMSTR",
                     "EXP_DELTA_ETA", "EXP_ETA_CONTRAST")

# Null reference line implied by an effect label: 1 for ratio measures, 0 for
# differences and log-scale contrasts. Shared by both forest plots so a measure
# added to `.ratio_measures` is right in every figure at once.
#' @keywords internal
.null_ref_for <- function(effect) {
  ifelse(toupper(effect) %in% .ratio_measures, 1, 0)
}

#' Forest plot of population-standardized marginal effects
#'
#' Plots the point estimate and 95% credible interval for each effect measure,
#' grouped by target population (index / comparator). The mlumr analogue of
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
  .need_ggplot2()
  df <- as.data.frame(x)
  ci <- .ci_cols(df)
  if (is.null(ci) || !all(c("mean", "effect", "population") %in% names(df))) {
    stop("Unexpected marginal_effects structure; cannot plot.", call. = FALSE)
  }
  df$.lo <- df[[ci$lo]]
  df$.hi <- df[[ci$hi]]
  df$population <- factor(df$population, levels = unique(df$population))

  # Per-facet null line: user override, else 1 for ratio measures, 0 otherwise.
  ref_df <- data.frame(effect = unique(df$effect))
  ref_df$ref <- if (!is.null(ref_line)) {
    ref_line[1]
  } else {
    .null_ref_for(ref_df$effect)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data$mean, y = .data$population)) +
    ggplot2::geom_vline(
      data = ref_df,
      ggplot2::aes(xintercept = .data$ref),
      linetype = "dashed", color = "gray55"
    ) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$.lo, xmax = .data$.hi),
      orientation = "y", width = 0.16, color = "#3B6B9A"
    ) +
    ggplot2::geom_point(size = 2.6, color = "#3B6B9A") +
    ggplot2::facet_wrap(~ .data$effect, scales = "free_x") +
    ggplot2::labs(x = "Estimate (95% credible interval)", y = NULL,
                  caption = .rmst_caption(x, df$effect)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
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
#' `predict(type = "rmst")` returns a `horizon` column (and survival fits also
#' record an `rmst_horizon` attribute). An RMST value is only defined together
#' with that time, so the plotting method has to be able to recover it.
#' Predictions integrated to different horizons are different estimands and are
#' refused rather than drawn on one axis.
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
#'   default both observed arms are drawn; pass `data$comparator_treatment` to
#'   overlay only the comparator KM on a comparator-population prediction (so the
#'   observed and predicted curves refer to the same population).
#' @param marks Logical; draw censoring marks (default `TRUE`).
#' @param linewidth Step line width (default `0.4`).
#' @param ... Passed to [ggplot2::geom_step()].
#' @return A list of ggplot2 layers (a step layer, plus a censoring-mark layer
#'   when `marks = TRUE`) to add to a plot with `+`.
#' @seealso [plot.mlumr_prediction()]
#' @export
#' @examples
#' \dontrun{
#' plot(predict(fit, type = "survival")) + geom_km(dat)
#' plot(predict(fit, type = "survival")) + geom_km(dat, dat$comparator_treatment)
#' }
geom_km <- function(data, treatments = NULL, marks = TRUE, linewidth = 0.4, ...) {
  .need_ggplot2()
  km <- .km_observed(data)
  if (!is.null(treatments)) {
    km$steps <- km$steps[km$steps$treatment %in% treatments, , drop = FALSE]
    km$censor <- km$censor[km$censor$treatment %in% treatments, , drop = FALSE]
  }
  layers <- list(
    ggplot2::geom_step(
      data = km$steps,
      ggplot2::aes(x = .data$time, y = .data$surv, color = .data$treatment),
      inherit.aes = FALSE, linewidth = linewidth, ...
    )
  )
  if (isTRUE(marks) && nrow(km$censor) > 0L) {
    layers <- c(layers, list(
      ggplot2::geom_point(
        data = km$censor,
        ggplot2::aes(x = .data$time, y = .data$surv, color = .data$treatment),
        inherit.aes = FALSE, shape = 3, size = 1.6, show.legend = FALSE
      )
    ))
  }
  layers
}

#' Observed Kaplan-Meier step + censoring data for a survival mlumr_data
#' @keywords internal
.km_observed <- function(data) {
  if (!inherits(data, "mlumr_data") || (data$family %||% "") != "survival") {
    stop("`geom_km()` requires a survival `mlumr_data` object from combine_data().",
      call. = FALSE
    )
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for geom_km().", call. = FALSE)
  }
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  ipd_entry <- if (!is.null(ipd$.delay_time)) ipd$.delay_time else
    rep(0, nrow(ipd))
  pseudo_entry <- if (!is.null(pseudo$.delay_time)) pseudo$.delay_time else
    rep(0, nrow(pseudo))
  obs <- rbind(
    data.frame(
      entry = ipd_entry, time = ipd$.time, status = ipd$.status,
      treatment = data$index_treatment
    ),
    data.frame(
      entry = pseudo_entry, time = pseudo$.time, status = pseudo$.status,
      treatment = data$comparator_treatment
    )
  )
  # geom_km() draws a right-censored Kaplan-Meier curve. Internal `.status`
  # encodes 0 = right-censored, 1 = event, 2 = left-censored, 3 = interval-
  # censored. The survival model handles 2/3, but a plain right-censored KM step
  # function cannot represent them, so reject rather than silently mislabel them
  # as right-censored.
  if (any(obs$status %in% c(2L, 3L))) {
    stop("`geom_km()` draws a right-censored Kaplan-Meier curve and cannot ",
         "represent left- or interval-censored observations (internal status ",
         "2 or 3). These are supported by the survival model but not by this KM ",
         "helper; restrict the plot to right-censored/event data or use an ",
         "interval-censored estimator.", call. = FALSE)
  }
  # Honor delayed entry (left truncation): with any positive entry time, use the
  # counting-process Surv(entry, time, status) so the risk set is counted from
  # each subject's entry, matching naive()/stc() and the survival model. With all
  # entries 0 this reduces exactly to the standard right-censored KM.
  km_fit <- if (any(obs$entry > 0)) {
    survival::survfit(survival::Surv(entry, time, status) ~ treatment, data = obs)
  } else {
    survival::survfit(survival::Surv(time, status) ~ treatment, data = obs)
  }
  sf <- survival::survfit0(km_fit)
  trt <- rep(sub("treatment=", "", names(sf$strata)), sf$strata)
  steps <- data.frame(time = sf$time, surv = sf$surv, treatment = trt)
  cens <- if (!is.null(sf$n.censor)) {
    steps[sf$n.censor > 0, , drop = FALSE]
  } else {
    steps[0, , drop = FALSE]
  }
  list(steps = steps, censor = cens)
}

#' Plot absolute predictions from a fitted ML-UMR model
#'
#' Dispatches on the prediction `type`: time-indexed types
#' (`"survival"`, `"hazard"`, `"cumhaz"`, `"loghr"`) are drawn as curves with
#' 95% credible bands; scalar types (`"rmst"`, `"median"`, `"response"`) as
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
  .need_ggplot2()
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
      # Survival probability lives on [0, 1]; predictions already start at the
      # (t = 0, S = 1) origin (see predict()), so the curve fills the corner.
      p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
    }
    # Null reference line(s): default 0 for the log hazard ratio, otherwise honor
    # whatever the caller passed (the multinma plot(..., ref_line = ) idiom).
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

  # Scalar predictions: point-interval by treatment. With both populations,
  # draw them in one panel, distinguished by color and dodged, rather than
  # faceting (a single readable plot).
  yvar <- if (has_trt) "treatment" else names(df)[1]
  xlab <- switch(ptype,
    response = "Predicted response",
    rmst = "Restricted mean survival time",
    median = "Median survival",
    ptype
  )
  # An RMST figure without its restriction time reads as though the quantity
  # were horizon-independent, which is exactly the misreading that puts two
  # incomparable numbers side by side in a report. Name tau on the plot itself.
  cap <- NULL
  if (identical(ptype, "rmst")) {
    tau <- .prediction_rmst_horizon(x, df)
    if (!is.null(tau)) {
      cap <- sprintf("RMST restricted to tau = %.4g", tau)
      xlab <- sprintf("Restricted mean survival time (tau = %.4g)", tau)
    }
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
#'   value to override for all panels. A fixed 0 was previously drawn for every
#'   effect, which put the null line off-scale on every ratio panel.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @seealso [conditional_effects()]
#' @export
plot.mlumr_conditional_effects <- function(x, ref_line = NULL, ...) {
  .need_ggplot2()
  df <- as.data.frame(x)
  ci <- .ci_cols(df)
  yvar <- if ("profile" %in% names(df)) "profile" else names(df)[1]
  df[[yvar]] <- factor(df[[yvar]], levels = unique(df[[yvar]]))
  # Per-facet null line, so a panel of risk ratios and a panel of risk
  # differences each get the reference their own scale implies.
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
    # Map the interval directly to the quantile columns. Assigning helper `.lo`
    # / `.hi` columns here would not reach the layer: ggplot() above has already
    # captured `df`, so a later geom only sees the columns present at that point.
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[[ci$lo]], xmax = .data[[ci$hi]]),
      orientation = "y", width = 0.16, color = "#3B6B9A"
    )
  }
  if ("effect" %in% names(df) && length(unique(df$effect)) > 1) {
    p <- p + ggplot2::facet_wrap(~ .data$effect, scales = "free_x")
  }
  p + ggplot2::geom_point(size = 2.6, color = "#3B6B9A") +
    ggplot2::labs(x = "Conditional effect (95% credible interval)", y = "Covariate profile") +
    ggplot2::theme_minimal(base_size = 11)
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
  .need_ggplot2()
  .validate_mlumr_fit_object(object)
  draws <- object$draws
  pars <- intersect(pars, colnames(draws))
  if (!length(pars)) {
    stop("None of `pars` are in the fit's posterior draws.", call. = FALSE)
  }
  # The intercepts share prior_intercept; this is the documented use. Overlaying
  # this prior on a non-intercept parameter (e.g. a regression coefficient) draws
  # the wrong reference curve, so warn rather than mislead.
  non_intercept <- setdiff(pars, c("mu_index", "mu_comparator"))
  if (length(non_intercept)) {
    warning(
      "plot_prior_posterior() overlays the intercept prior (prior_intercept) ",
      "for every parameter; for non-intercept parameter(s) ",
      paste(non_intercept, collapse = ", "),
      " the prior curve shown is only an approximation, not the parameter's ",
      "own prior.",
      call. = FALSE
    )
  }
  pr <- object$priors$intercept
  prior_mean <- pr$mean %||% 0
  prior_sd <- pr$sd %||% 10
  prior_df <- pr$df

  long <- do.call(rbind, lapply(pars, function(p) {
    data.frame(
      parameter = p, value = as.numeric(draws[[p]]),
      stringsAsFactors = FALSE
    )
  }))
  prior_fun <- if (!is.null(prior_df) && !is.na(prior_df)) {
    function(z) stats::dt((z - prior_mean) / prior_sd, df = prior_df) / prior_sd
  } else {
    function(z) stats::dnorm(z, mean = prior_mean, sd = prior_sd)
  }

  ggplot2::ggplot(long, ggplot2::aes(.data$value)) +
    ggplot2::geom_density(ggplot2::aes(color = "posterior"),
      fill = "#3B6B9A", alpha = 0.15, linewidth = 0.7
    ) +
    ggplot2::stat_function(
      fun = prior_fun, ggplot2::aes(color = "prior"),
      linetype = "dashed"
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
#' @param data A data frame with one row per estimate. Columns are matched
#'   flexibly (first match wins): the row label from `label` / `method` /
#'   `Method` / `Comparison` (else the first character/factor column); the point
#'   estimate from `est` / `estimate` / `mean`; the interval bounds from
#'   `lo`/`hi`, `q2.5`/`q97.5`, `ci_lower`/`ci_upper`, `conf.low`/`conf.high`, or
#'   `lower`/`upper`.
#' @param ref_line Null-effect reference line (default `0`; pass `1` for ratio
#'   measures).
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
mlumr_forest <- function(data, ref_line = 0, log_x = FALSE,
                         x = NULL, title = NULL, subtitle = NULL,
                         color = "#3B6B9A", clip = TRUE, ...) {
  .need_ggplot2()
  df <- as.data.frame(data)
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

  # Clip an over-wide interval (e.g. a weakly identified relaxed-model effect)
  # to the bulk of the estimates and mark its clipped end(s) with an arrow, so
  # one very uncertain estimate does not squeeze every other into a sliver. Work
  # on the plotted scale (log10 for a ratio axis).
  fwd <- if (isTRUE(log_x)) function(z) log10(z) else function(z) z
  inv <- if (isTRUE(log_x)) function(z) 10^z else function(z) z
  lim <- .forest_clip_range(fwd(pdat$.est), fwd(pdat$.lo), fwd(pdat$.hi), clip)

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
    pdat$.clo <- !is.finite(flo) | flo < lim[1]
    pdat$.chi <- !is.finite(fhi) | fhi > lim[2]
    dlo_w <- ifelse(is.finite(flo), pmax(flo, lim[1]), lim[1])
    dhi_w <- ifelse(is.finite(fhi), pmin(fhi, lim[2]), lim[2])
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
  rng <- range(c(est[ok], lo[keep], hi[keep]), na.rm = TRUE)
  if (!all(is.finite(rng)) || isTRUE(rng[1] == rng[2])) {
    return(NULL)
  }
  pad <- 0.05 * (rng[2] - rng[1])
  c(rng[1] - pad, rng[2] + pad)
}
