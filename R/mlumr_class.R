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

  model_label <- if (x$model == "spfa") {
    "SPFA (Shared Prognostic Factors)"
  } else {
    "Relaxed SPFA (Treatment-specific effects)"
  }

  link_label <- x$link %||% switch(family, binomial = "logit", normal = "identity", poisson = "log")

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
  } else {
    params <- c("mu_index", "mu_comparator",
                "delta_index", "delta_comparator")
  }

  idx <- x$summary$variable %in% params
  if (any(idx)) {
    cat("Key Parameters:\n")
    sub_df <- x$summary[idx, c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")]
    print(sub_df, row.names = FALSE)
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

  link_label <- object$link %||% switch(family, binomial = "logit", normal = "identity", poisson = "log")

  cat("Model:", model_label, "\n")
  cat("Family:", family_label, "\n")
  cat("Link:", link_label, "\n")
  if (!is.null(object$engine)) cat("Engine:", object$engine, "\n")
  cat("Treatments:", object$data$index_treatment, "(IPD) vs",
      object$data$comparator_treatment, "(AgD)\n\n")

  # Diagnostics
  cat("MCMC Diagnostics:\n")
  cat("  Divergent transitions:", object$diagnostics$n_divergent, "\n")
  cat("  Max treedepth hits:", object$diagnostics$n_max_treedepth, "\n")
  if (!is.null(object$summary$Rhat)) {
    cat("  Max Rhat:", round(max(object$summary$Rhat, na.rm = TRUE), 3), "\n")
  }
  if (!is.null(object$summary$n_eff)) {
    cat("  Min ESS:", round(min(object$summary$n_eff, na.rm = TRUE), 0), "\n")
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
    print(object$summary[beta_idx, c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")],
          row.names = FALSE)
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
      cat("  RMST Differences:\n")
      print(object$summary[rmst_idx, c("variable", "mean", "sd", "2.5%", "97.5%")],
            row.names = FALSE)
    }
  } else {
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
  } else {
    cat("Rates:\n")
    cat(sprintf("  Index (IPD):      %.4f\n", x$rate_index))
    cat(sprintf("  Comparator (AgD): %.4f\n", x$rate_comparator))
    cat(sprintf("\nLog Rate Ratio: %.4f (SE: %.4f)\n", x$estimate, x$se))
  }

  cat(sprintf("%.0f%% CI: [%.4f, %.4f]\n",
              x$conf_level * 100, x$ci_lower, x$ci_upper))
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
    cat(sprintf("\nMean Difference: %.4f (SE: %.4f)\n", x$estimate, x$se))
  } else {
    cat(sprintf("Marginalized rate (index trt, comp pop): %.4f\n", x$rate_hat_index))
    cat(sprintf("Observed rate (comp trt, comp pop):      %.4f\n", x$rate_comparator))
    cat(sprintf("\nLog Rate Ratio: %.4f (SE: %.4f)\n", x$estimate, x$se))
  }

  cat(sprintf("%.0f%% CI: [%.4f, %.4f]\n",
              x$conf_level * 100, x$ci_lower, x$ci_upper))

  cat("\nOutcome model coefficients:\n")
  print(round(coef(x$glm_fit), 4))
  invisible(x)
}


#' @method summary mlumr_stc
#' @export
summary.mlumr_stc <- function(object, ...) {
  print.mlumr_stc(object, ...)
  cat("\nFull GLM summary:\n")
  print(summary(object$glm_fit))
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
