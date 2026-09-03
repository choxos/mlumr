

#' @method print mlumr_fit
#' @export
print.mlumr_fit <- function(x, ...) {
  cat("ML-UMR Fit\n")
  cat("==========\n\n")

  family <- x$family %||% "binomial"
  family_label <- switch(family,
    binomial = "Binary",
    normal   = "Continuous (Normal)",
    poisson  = "Count (Poisson)",
    survival = "Time-to-event"
  )
  if (family == "survival" && !is.null(x$distribution)) {
    family_label <- sprintf("%s [%s]", family_label, x$distribution)
  }

  model_label <- if (x$model == "spfa") {
    "SPFA (Shared Prognostic Factors)"
  } else {
    "Relaxed SPFA (Treatment-specific effects)"
  }

  link_label <- x$link %||% switch(family, binomial = "logit",
                                   normal = "identity", poisson = "log",
                                   survival = "log")

  cat("Model:", model_label, "\n")
  cat("Family:", family_label, "\n")
  cat("Link:", link_label, "\n")
  if (!is.null(x$engine)) cat("Engine:", x$engine, "\n")
  cat("Treatments:\n")
  cat("  Index (IPD):", x$data$index_treatment, "\n")
  cat("  Comparator (AgD):", x$data$comparator_treatment, "\n\n")

  cat("Data:\n")
  cat("  IPD: n =", x$stan_data$n_ipd, "observations\n")
  cat("  AgD:", x$stan_data$n_agd_rows, "rows\n")
  cat("  Covariates:", x$stan_data$n_cov, "\n")
  cat("  Integration points:", x$stan_data$n_int, "\n\n")

  cat("Sampling:\n")
  cat("  Chains:", x$sampling_args$chains, "\n")
  cat("  Iterations:", x$sampling_args$iter,
      "(warmup:", x$sampling_args$warmup, ")\n\n")

  # Key parameters depend on family
  if (family == "binomial") {
    params <- c("mu_index", "mu_comparator",
                "lor_index", "lor_comparator",
                "rd_index", "rd_comparator")
  } else if (family == "normal") {
    params <- c("mu_index", "mu_comparator", "sigma",
                "delta_index", "delta_comparator")
  } else if (family == "poisson") {
    params <- c("mu_index", "mu_comparator",
                "delta_index", "delta_comparator")
  } else {
    # aux_val_cmp / sigma_smooth[2] only exist when aux_by = ".study"; the
    # match below is on exact names, so listing them is harmless otherwise.
    params <- c("mu_index", "mu_comparator", "aux_val", "aux_val_cmp",
                "sigma_smooth", "sigma_smooth[1]", "sigma_smooth[2]",
                "delta_conditional", "delta_index", "delta_comparator")
  }

  idx <- x$summary$variable %in% params
  if (any(idx)) {
    cat("Key Parameters:\n")
    sub_df <- x$summary[idx, c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")]
    print(sub_df, row.names = FALSE)
    # `delta_conditional` is mu_index - mu_comparator, which is eta_index(x) -
    # eta_comparator(x) evaluated at x = 0. Two separate conditions matter and
    # were previously conflated. A SHARED baseline shape is what makes it a
    # conditional log hazard / time ratio at all, and it is then that contrast
    # at the reference profile, whether or not the coefficients are shared.
    # SHARED coefficients are what make it constant across covariate profiles.
    # With study-specific shapes neither holds: the baseline ratio does not
    # cancel, and with per-study M-spline bases each intercept is additionally
    # tied to its own basis normalization.
    if ("delta_conditional" %in% x$summary$variable[idx]) {
      differs <- .aux_shapes_differ(x)
      relaxed <- identical(x$model %||% "spfa", "relaxed")
      # `center = TRUE` (the default) shifts the design to the pooled covariate
      # mean, so the reference profile is that mean rather than raw zero.
      ref <- if (isTRUE(x$model_controls$center %||% TRUE)) {
        "the pooled covariate mean"
      } else {
        "x = 0"
      }
      if (differs) {
        cat("   (delta_conditional is mu_index - mu_comparator. With",
            " study-specific\n    baseline shapes the baseline ratio does not",
            " cancel, so this is an\n    intercept contrast under this fit's",
            " normalization, not a conditional\n    hazard or time ratio. Use",
            " marginal_effects() for the treatment effect.)\n", sep = "")
      } else if (relaxed) {
        cat("   (delta_conditional is the conditional log effect at ", ref,
            ".\n    The coefficients are treatment-specific, so it varies with",
            " the\n    covariates and is not one number for the population.)\n",
            sep = "")
      }
    }
  }

  cat("\nUse summary() for full results\n")
  invisible(x)
}


#' @method summary mlumr_fit
#' @export
summary.mlumr_fit <- function(object, ...) {

  cat("ML-UMR Model Summary\n")
  cat("====================\n\n")

  family <- object$family %||% "binomial"
  model_label <- if (object$model == "spfa") "SPFA" else "Relaxed SPFA"
  family_label <- switch(family,
    binomial = "Binary",
    normal   = "Continuous (Normal)",
    poisson  = "Count (Poisson)",
    survival = "Time-to-event"
  )
  if (family == "survival" && !is.null(object$distribution)) {
    family_label <- sprintf("%s [%s]", family_label, object$distribution)
  }

  link_label <- object$link %||% switch(family, binomial = "logit",
                                        normal = "identity", poisson = "log",
                                        survival = "log")

  cat("Model:", model_label, "\n")
  cat("Family:", family_label, "\n")
  cat("Link:", link_label, "\n")
  if (!is.null(object$engine)) cat("Engine:", object$engine, "\n")
  cat("Treatments:", object$data$index_treatment, "(IPD) vs",
      object$data$comparator_treatment, "(AgD)\n\n")

  # Diagnostics
  cat("MCMC Diagnostics:\n")
  n_req <- object$diagnostics$n_chains_requested
  n_got <- object$diagnostics$n_chains_returned
  if (!is.null(n_req) && !is.null(n_got) && isTRUE(n_got < n_req)) {
    cat(sprintf("  Chains: %d of %d returned (INCOMPLETE, see warnings)\n",
                n_got, n_req))
  }
  cat("  Divergent transitions:", object$diagnostics$n_divergent, "\n")
  cat("  Max treedepth hits:", object$diagnostics$n_max_treedepth, "\n")
  if (!is.null(object$summary$Rhat)) {
    finite_rhat <- object$summary$Rhat[is.finite(object$summary$Rhat)]
    cat("  Max Rhat:",
        if (length(finite_rhat)) round(max(finite_rhat), 3) else "unavailable", "\n")
  }
  if (!is.null(object$summary$n_eff)) {
    finite_ess <- object$summary$n_eff[is.finite(object$summary$n_eff)]
    cat("  Min ESS:",
        if (length(finite_ess)) round(min(finite_ess), 0) else "unavailable", "\n")
  }
  cat("\n")

  # Intercepts
  scale_label <- paste0(link_label, " scale")
  cat(sprintf("Intercepts (%s):\n", scale_label))
  mu_idx <- grep("^mu_", object$summary$variable)
  print(object$summary[mu_idx, c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")],
        row.names = FALSE)

  # Residual SD (normal only)
  if (family == "normal") {
    sigma_idx <- which(object$summary$variable == "sigma")
    if (length(sigma_idx) > 0) {
      cat("\nResidual SD:\n")
      print(object$summary[sigma_idx, c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")],
            row.names = FALSE)
    }
  }

  # Regression coefficients
  cat("\nRegression Coefficients:\n")
  if (object$model == "spfa") {
    beta_idx <- grep("^beta\\[", object$summary$variable)
  } else {
    beta_idx <- grep("^beta_(index|comparator)\\[", object$summary$variable)
  }
  if (length(beta_idx) > 0) {
    # Relabel beta[1] as beta[age] for readability. The underlying `variable`
    # strings in object$summary are untouched, so code indexing by name keeps
    # working; only this printed copy is relabeled.
    beta_df <- object$summary[beta_idx,
                              c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")]
    print(.label_beta_rows(beta_df, object$data$covariates), row.names = FALSE)

  }

  # Shape / smoothing parameters (survival)
  if (family == "survival") {
    # Say which baseline structure produced these numbers: a stratified baseline
    # makes the hazard ratio time-varying, which changes how delta_* reads.
    n_strata <- object$stan_data$n_strata %||% 1L
    differs <- .aux_shapes_differ(object)
    cat("\nBaseline hazard: ",
        if (!differs && n_strata > 1L) {
          # aux_by = ".study" was requested but the exponential (and its AFT
          # form) has no SHAPE parameter to stratify, so `aux_by` changes
          # nothing and every closed-form contrast stays exact. Do not call the
          # baselines "shared": each study still has its own hazard LEVEL
          # through its own intercept. What coincides is the shape, trivially,
          # because there is none.
          paste0("constant within study; the exponential has no shape ",
                 "parameter for `aux_by` to stratify, and each study's hazard ",
                 "level is carried by its own intercept")
        } else if (differs) {
          "stratified by study (aux_by = \".study\")"
        } else {
          "shared by both studies"
        },
        "\n", sep = "")
    aux_idx <- which(object$summary$variable %in%
                       c("aux_val", "aux2_val", "aux_val_cmp", "aux2_val_cmp",
                         "sigma_smooth", "sigma_smooth[1]", "sigma_smooth[2]"))
    if (length(aux_idx) > 0) {
      cat("\nShape / smoothing parameters:\n")
      print(object$summary[aux_idx,
                           c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")],
            row.names = FALSE)
    }
  }

  # Marginal effects
  cat("\nMarginal Treatment Effects:\n")
  if (family == "binomial") {
    lor_idx <- grep("^lor_", object$summary$variable)
    if (length(lor_idx) > 0) {
      cat("  Log Odds Ratios:\n")
      print(object$summary[lor_idx, c("variable", "mean", "sd", "2.5%", "97.5%")],
            row.names = FALSE)
    }

    rd_idx <- grep("^rd_", object$summary$variable)
    if (length(rd_idx) > 0) {
      cat("  Risk Differences:\n")
      print(object$summary[rd_idx, c("variable", "mean", "sd", "2.5%", "97.5%")],
            row.names = FALSE)
    }

    rr_idx <- grep("^rr_", object$summary$variable)
    if (length(rr_idx) > 0) {
      cat("  Risk Ratios:\n")
      print(object$summary[rr_idx, c("variable", "mean", "sd", "2.5%", "97.5%")],
            row.names = FALSE)
    }
  } else if (family == "normal") {
    delta_idx <- grep("^delta_(index|comparator)$", object$summary$variable)
    if (length(delta_idx) > 0) {
      cat("  Mean Differences:\n")
      print(object$summary[delta_idx, c("variable", "mean", "sd", "2.5%", "97.5%")],
            row.names = FALSE)
    }
  } else if (family == "survival") {
    # delta_* is a LOG hazard ratio (PH) or LOG time ratio (AFT), null 0, not a
    # rate ratio. Print it under the name the fit's own label helper resolves,
    # so the heading cannot claim an estimand the distribution does not give,
    # and carry the evaluation time the scalar belongs to.
    delta_idx <- grep("^delta_(index|comparator)$", object$summary$variable)
    if (length(delta_idx) > 0) {
      lab <- .surv_scalar_label(object, log_scale = TRUE)
      heading <- switch(lab$label,
                        LOG_HR = "Log Hazard Ratios",
                        LOG_TR = "Log Time Ratios",
                        DELTA_ETA = "Location Contrasts (not a time ratio)")
      at <- lab$at_time
      when <- if (identical(lab$label, "LOG_HR")) {
        if (isTRUE(at == 0)) {
          " at the start of follow-up (t -> 0)"
        } else {
          paste0(" at t = ", format(at, digits = 4L))
        }
      } else {
        ""
      }
      cat("  ", heading, when, ":\n", sep = "")
      print(object$summary[delta_idx, c("variable", "mean", "sd", "2.5%", "97.5%")],
            row.names = FALSE)
    }
    rmst_idx <- grep("^rmst_diff_(index|comparator)$", object$summary$variable)
    if (length(rmst_idx) > 0) {
      # RMST is an integral to a restriction time, so the value without its
      # horizon is not an estimand: two fits with different horizons produce
      # numbers that must not be read side by side.
      g <- object$stan_data$rmst_grid_times
      tau <- if (is.null(g)) NA_real_ else max(g)
      cat("  RMST Differences",
          if (is.na(tau)) "" else paste0(" (to t = ", format(tau, digits = 4L), ")"),
          ":\n", sep = "")
      print(object$summary[rmst_idx, c("variable", "mean", "sd", "2.5%", "97.5%")],
            row.names = FALSE)
    }
  } else if (family == "poisson") {
    delta_idx <- grep("^delta_(index|comparator)$", object$summary$variable)
    if (length(delta_idx) > 0) {
      cat("  Rate Ratios:\n")
      print(object$summary[delta_idx, c("variable", "mean", "sd", "2.5%", "97.5%")],
            row.names = FALSE)
    }
  }

  cat("\n")
  invisible(object)
}




#' @method print mlumr_naive
#' @export
print.mlumr_naive <- function(x, ...) {
  family <- x$family %||% "binomial"
  cat("Naive Unadjusted Indirect Comparison\n")
  cat("=====================================\n\n")
  cat("Treatments:", x$data$index_treatment, "vs", x$data$comparator_treatment, "\n\n")
  cat("Population basis: index-study outcome versus comparator-population outcome; no common standardized target.\n\n")

  if (family == "binomial") {
    cat("Event rates:\n")
    cat(sprintf("  Index (IPD):      %.3f (%d/%d)\n",
                x$p_index, round(x$p_index * x$n_index), x$n_index))
    cat(sprintf("  Comparator (AgD): %.3f (%d/%d)\n",
                x$p_comparator, round(x$p_comparator * x$n_comparator), x$n_comparator))
    link_label <- switch(x$link %||% "logit",
      logit   = "Log Odds Ratio",
      probit  = "Probit Difference",
      cloglog = "Cloglog Difference"
    )
    cat(sprintf("\n%s: %.4f (SE: %.4f)\n", link_label, x$estimate, x$se))
  } else if (family == "normal") {
    cat("Mean outcomes:\n")
    cat(sprintf("  Index (IPD):      %.4f\n", x$mean_index))
    cat(sprintf("  Comparator (AgD): %.4f\n", x$mean_comparator))
    cat(sprintf("\nMean Difference: %.4f (SE: %.4f)\n", x$estimate, x$se))
  } else if (family == "poisson") {
    cat("Rates:\n")
    cat(sprintf("  Index (IPD):      %.4f\n", x$rate_index))
    cat(sprintf("  Comparator (AgD): %.4f\n", x$rate_comparator))
    cat(sprintf("\nLog Rate Ratio: %.4f (SE: %.4f)\n", x$estimate, x$se))
  } else {
    fmt_med <- function(m) if (is.na(m)) "not reached" else sprintf("%.3f", m)
    cat("Median survival:\n")
    cat(sprintf("  Index (IPD):      %s (%d events / %d)\n",
                fmt_med(x$median_index), x$events_index, x$n_index))
    cat(sprintf("  Comparator (AgD): %s (%d events / %d)\n",
                fmt_med(x$median_comparator), x$events_comparator, x$n_comparator))
    cat(sprintf("\nLog Hazard Ratio (Cox): %.4f (SE: %.4f)\n", x$estimate, x$se))
  }

  cat(sprintf("%.0f%% CI: [%.4f, %.4f]\n",
              x$conf_level * 100, x$ci_lower, x$ci_upper))
  .print_effect_measures(x)
  invisible(x)
}


#' @method summary mlumr_naive
#' @export
summary.mlumr_naive <- function(object, ...) {
  print.mlumr_naive(object, ...)
}


#' @method print mlumr_stc
#' @export
print.mlumr_stc <- function(x, ...) {
  family <- x$family %||% "binomial"
  cat("Simulated Treatment Comparison (G-computation)\n")
  cat("===============================================\n\n")
  cat("Treatments:", x$data$index_treatment, "vs", x$data$comparator_treatment, "\n\n")
  cat("Estimand population: comparator\n")
  cat("Treating this as the index-population effect requires a separate ",
      "effect-equality assumption; this calculation does not transport to ",
      "the index population.\n\n", sep = "")

  if (family == "binomial") {
    cat(sprintf("Marginalized P(Y=1|index trt, comp pop): %.4f\n", x$p_hat_index))
    cat(sprintf("Observed P(Y=1|comp trt, comp pop):      %.4f\n", x$p_comparator))
    link_label <- switch(x$link %||% "logit",
      logit   = "Log Odds Ratio",
      probit  = "Probit Difference",
      cloglog = "Cloglog Difference"
    )
    cat(sprintf("\n%s: %.4f (SE: %.4f)\n", link_label, x$estimate, x$se))
  } else if (family == "normal") {
    cat(sprintf("Marginalized E[Y|index trt, comp pop]: %.4f\n", x$y_hat_index))
    cat(sprintf("Observed E[Y|comp trt, comp pop]:      %.4f\n", x$y_comparator))
    # .stc_normal() standardizes on the RESPONSE scale and returns
    # y_hat_A - y_B for every link, with a delta-method SE for that difference.
    # The link chooses the model that is fitted, not the scale of the contrast,
    # so the label must not change with it: calling this a log mean ratio and
    # exponentiating it reported exp(difference) as a ratio.
    cat(sprintf("\nMean Difference: %.4f (SE: %.4f)\n", x$estimate, x$se))
  } else if (family == "poisson") {
    cat(sprintf("Marginalized rate (index trt, comp pop): %.4f\n", x$rate_hat_index))
    cat(sprintf("Observed rate (comp trt, comp pop):      %.4f\n", x$rate_comparator))
    cat(sprintf("\nLog Rate Ratio: %.4f (SE: %.4f)\n", x$estimate, x$se))
  } else {
    cat("Method note: package-specific parametric survival extension.\n")
    if (!is.null(x$out_of_family)) {
      # The fitted shape/Q left the parameter space of the Bayesian model with
      # the same name, so naming only the distribution would imply a
      # like-for-like comparison that does not hold.
      cat(sprintf(paste0("Distribution: %s (%s = %s is outside mlumr()'s '%s' ",
                         "parameter space) | RMST horizon: %.3f\n"),
                  x$distribution_fit, x$out_of_family,
                  paste(sprintf("%.4g", x$family_par), collapse = " / "),
                  x$distribution, x$horizon))
    } else if (isTRUE(x$approximated)) {
      cat(sprintf(paste0("Distribution: %s (Weibull approximation; '%s' has no",
                         " parametric STC analogue) | RMST horizon: %.3f\n"),
                  x$distribution_fit %||% "weibull", x$distribution, x$horizon))
    } else {
      cat(sprintf("Distribution: %s | RMST horizon: %.3f\n",
                  x$distribution, x$horizon))
    }
    # Both numbers come from a parametric fit. The comparator's is an
    # intercept-only flexsurvreg fit to the reconstructed pseudo-IPD, not a
    # Kaplan-Meier area, so calling it "observed" would invite a reader to
    # blame any discrepancy on the index standardization when it can just as
    # easily come from the comparator's own distributional assumption.
    cat(sprintf("Marginalized RMST (index trt, comp pop): %.4f\n", x$rmst_index))
    cat(sprintf("Fitted RMST (comp trt, comp pop):        %.4f\n",
                x$rmst_comparator))
    req <- x$n_boot_requested %||% x$n_boot %||% 0L
    okn <- x$n_boot_ok %||% x$n_boot %||% 0L
    if (is.na(x$se)) {
      # sd() returns NA both when every resample failed and when exactly one
      # survived, and the two are not the same event. Branch on the success
      # count, not on the NA, so a single surviving replicate is not reported
      # as a total failure.
      if (req == 0L) {
        cat(sprintf(paste0("\nRMST Difference: %.4f (point estimate only; ",
                           "bootstrap disabled, n_boot = 0)\n"), x$estimate))
      } else if (okn == 0L) {
        cat(sprintf(paste0("\nRMST Difference: %.4f (point estimate only; all ",
                           "%d bootstrap resample(s) failed)\n"),
                    x$estimate, req))
      } else {
        cat(sprintf(paste0("\nRMST Difference: %.4f (point estimate only; ",
                           "%d of %d bootstrap resample(s) succeeded, too few ",
                           "for a standard error)\n"),
                    x$estimate, okn, req))
      }
    } else if (okn < req) {
      cat(sprintf(paste0("\nRMST Difference: %.4f (bootstrap SE: %.4f, %d of %d ",
                         "reps succeeded, %d failed)\n"),
                  x$estimate, x$se, okn, req, req - okn))
    } else {
      cat(sprintf("\nRMST Difference: %.4f (bootstrap SE: %.4f, %d reps)\n",
                  x$estimate, x$se, okn))
    }
    # The log cumulative-hazard-ratio interval printed below can rest on fewer
    # replicates than the RMST one: a resample can give a finite RMST difference
    # while H(horizon) is undefined for one arm. Say so beside the count that
    # applies to it rather than letting the RMST count stand for both.
    okc <- x$n_boot_ok_log_chr
    if (req > 0L && !is.null(okc) &&
          !identical(as.integer(okc), as.integer(okn))) {
      cat(sprintf(paste0("Log cumulative-hazard-ratio SE based on %d of %d ",
                         "resample(s)\n"), as.integer(okc), req))
    }
    # Family membership is decided per fit, so the point estimate can sit inside
    # mlumr()'s parameter space while some resamples do not; those refits still
    # enter the standard error.
    oof <- x$n_boot_out_of_family
    if (!is.null(oof) && !is.na(oof) && oof > 0L) {
      cat(sprintf(paste0("Bootstrap: %d of %d resample(s) fitted %s < 0, ",
                         "outside mlumr()'s '%s' parameter space, and are ",
                         "included in the SE\n"),
                  as.integer(oof), req, x$family_par_name %||% "the shape",
                  x$distribution))
    }
  }

  if (!is.na(x$ci_lower)) {
    cat(sprintf("%.0f%% CI: [%.4f, %.4f]\n",
                x$conf_level * 100, x$ci_lower, x$ci_upper))
  }
  .print_effect_measures(x)

  if (!is.null(x$glm_fit)) {
    cat("\nOutcome model coefficients:\n")
    print(round(coef(x$glm_fit), 4))
  }
  invisible(x)
}


#' @method summary mlumr_stc
#' @export
summary.mlumr_stc <- function(object, ...) {
  print.mlumr_stc(object, ...)
  # A survival STC has no GLM behind it, so this footer printed "NULL" under a
  # "Full GLM summary" heading. print.mlumr_stc already guards its own copy;
  # the heading moves inside the guard so nothing is announced that is absent.
  if (!is.null(object$glm_fit)) {
    cat("\nFull GLM summary:\n")
    print(summary(object$glm_fit))
  }
  invisible(object)
}


# Attach an mlumr result class (+ attributes) while keeping data.frame as a parent
# so existing data-frame behavior (indexing, knitr::kable, the reporting engine,
# tests using inherits()) is unchanged. S3 print dispatch falls through to
# print.data.frame, so these print as ordinary tables.
#' @keywords internal
.mlumr_result <- function(df, subclass, ...) {
  df <- as.data.frame(df)
  attrs <- list(...)
  for (nm in names(attrs)) attr(df, nm) <- attrs[[nm]]
  class(df) <- c(subclass, "data.frame")
  df
}


# All effect measures derivable from a naive/STC result, as a tidy data frame
# (Measure, Estimate, SE, CI_lower, CI_upper). The per-arm absolute outcomes
# yield every comparative measure for the family, so the benchmarks are reported
# as completely as the method allows.
.effect_measures_df <- function(x) {
  fam <- x$family %||% "binomial"
  link <- x$link %||% "logit"
  # A missing bound is NULL on some result objects and NA_real_ on others.
  # exp(NULL) errors before `%||%` can rescue it, so normalize first: any value
  # that is absent, non-numeric, NA, or NaN becomes NA_real_. Infinite values
  # are retained because an exact natural-scale ratio can legitimately overflow.
  num <- function(v) {
    if (is.null(v) || length(v) != 1L || !is.numeric(v) || is.na(v) || is.nan(v)) {
      return(NA_real_)
    }
    as.numeric(v)
  }
  acc <- new.env(parent = emptyenv())
  acc$rows <- list()
  add <- function(measure, est, se = NA_real_, lo = NA_real_, hi = NA_real_) {
    est <- num(est)
    if (is.na(est)) return(invisible())
    acc$rows[[length(acc$rows) + 1L]] <- data.frame(
      Measure = measure,
      Estimate = est,
      SE = num(se),
      CI_lower = num(lo),
      CI_upper = num(hi),
      stringsAsFactors = FALSE
    )
    invisible()
  }
  # exp() of an already-normalized bound, so a NULL/NA bound stays NA instead of
  # raising "non-numeric argument to mathematical function".
  eexp <- function(v) exp(num(v))
  if (fam == "binomial") {
    lab <- switch(link, logit = "Log odds ratio", probit = "Probit difference",
                  cloglog = "Cloglog difference", "Link-scale difference")
    add(lab, x$estimate, x$se, x$ci_lower, x$ci_upper)
    if (identical(link, "logit")) {
      add("Odds ratio", eexp(x$estimate), NA_real_, eexp(x$ci_lower), eexp(x$ci_upper))
    }
    add("Risk difference", x$rd, x$rd_se, x$rd_lower, x$rd_upper)
    if (!is.null(x$log_rr)) {
      add("Risk ratio", eexp(x$log_rr), NA_real_, eexp(x$log_rr_lower), eexp(x$log_rr_upper))
    }
  } else if (fam == "normal") {
    # One row for every link: the estimand is the response-scale mean
    # difference regardless of which link fitted the model. `x$md` was never
    # populated, so the previous log-link branch also dropped the only true
    # mean difference it claimed to report.
    add("Mean difference", x$estimate, x$se, x$ci_lower, x$ci_upper)
  } else if (fam == "poisson") {
    add("Log rate ratio", x$estimate, x$se, x$ci_lower, x$ci_upper)
    add("Rate ratio", eexp(x$estimate), NA_real_, eexp(x$ci_lower), eexp(x$ci_upper))
    # Rate difference on the natural (per-unit-exposure) scale. Both the naive
    # and STC poisson estimators compute it; a result object from an older
    # version that predates the field simply omits the row.
    add("Rate difference", x$rd, x$rd_se, x$rd_lower, x$rd_upper)
  } else {  # survival
    if (!is.null(x$rmst_diff)) {                       # STC: RMST + cumhaz ratio
      add("RMST difference", x$rmst_diff, x$se, x$ci_lower, x$ci_upper)
      if (!is.null(x$log_chr)) {
        # A ratio of cumulative hazards at the horizon, not a hazard ratio.
        add("Log cumulative-hazard ratio (at horizon)",
            x$log_chr, x$log_chr_se, x$log_chr_lower, x$log_chr_upper)
        add("Cumulative-hazard ratio (at horizon)",
            eexp(x$log_chr), NA_real_, eexp(x$log_chr_lower), eexp(x$log_chr_upper))
      }
    } else {                                           # naive Cox: conditional HR
      add("Log hazard ratio (Cox)", x$estimate, x$se, x$ci_lower, x$ci_upper)
      add("Hazard ratio (Cox)", eexp(x$estimate), NA_real_, eexp(x$ci_lower), eexp(x$ci_upper))
    }
  }
  if (!length(acc$rows)) return(NULL)
  do.call(rbind, acc$rows)
}

#' Label indexed beta rows with covariate names for display
#'
#' Rewrites `beta[1]` to `beta[age]` (and the relaxed model's
#' `beta_index[1]` / `beta_comparator[1]` likewise) so printed coefficient
#' tables name the covariate instead of its position, matching the
#' `beta[age]` idiom used by `multinma`. Display only: the underlying
#' `variable` strings in `fit$summary` are unchanged, so code that indexes
#' on `beta[1]` keeps working.
#'
#' @param df A slice of `fit$summary`.
#' @param covariates Character vector of covariate names, in model order.
#' @return `df` with its `variable` column relabeled where possible.
#' @keywords internal
.label_beta_rows <- function(df, covariates) {
  if (!length(covariates) || !nrow(df)) return(df)
  m <- regmatches(df$variable,
                  regexec("^(beta|beta_index|beta_comparator)\\[([0-9]+)\\]$",
                          df$variable))
  df$variable <- vapply(seq_along(m), function(i) {
    p <- m[[i]]
    if (length(p) != 3L) return(df$variable[i])
    k <- as.integer(p[[3]])
    if (is.na(k) || k < 1L || k > length(covariates)) return(df$variable[i])
    sprintf("%s[%s]", p[[2]], covariates[k])
  }, character(1))
  df
}

# Print the full effect-measures table for a naive/STC benchmark.
.print_effect_measures <- function(x) {
  df <- tryCatch(.effect_measures_df(x), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(invisible())
  cl <- x$conf_level %||% 0.95
  fmt <- function(v) if (is.na(v)) "" else formatC(v, format = "f", digits = 4)
  cat(sprintf("\nAll effect measures (%.0f%% CI):\n", cl * 100))
  for (i in seq_len(nrow(df))) {
    se <- if (is.na(df$SE[i])) "" else sprintf(" (SE %s)", fmt(df$SE[i]))
    ci <- if (is.na(df$CI_lower[i])) "" else
      sprintf(" [%s, %s]", fmt(df$CI_lower[i]), fmt(df$CI_upper[i]))
    cat(sprintf("  %-28s %8s%s%s\n", df$Measure[i], fmt(df$Estimate[i]), se, ci))
  }
  invisible()
}
