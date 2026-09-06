#' Conditional treatment effects
#'
#' Compute conditional (individual-level) treatment effects at specific
#' covariate values from a fitted ML-UMR model. Unlike [marginal_effects()],
#' which averages over a population's covariate distribution, conditional
#' effects evaluate the treatment effect at a particular covariate profile.
#'
#' @param object An `mlumr_fit` object
#' @param newdata Data frame of covariate values at which to compute effects.
#'   Each row defines one covariate profile. Column names must match the
#'   covariates used in fitting. If `NULL` (default), uses the covariate means
#'   from the IPD as a single reference profile.
#' @param effect Which effect measure. For binomial: `"all"`, `"link_effect"`,
#'   `"rd"`, or `"rr"`. For normal: `"all"` or `"md"`. For Poisson: `"all"`
#'   or `"rr"`. The legacy value `"lor"` is accepted as an alias for
#'   `"link_effect"` when the fitted link is logit.
#'
#'   For **survival**, what is available depends on whether the two studies
#'   share a baseline, because `exp(eta_index - eta_comparator)` is a
#'   conditional hazard ratio only when the baseline factor cancels:
#'   \itemize{
#'     \item **Shared baseline shape** (`aux_by = "none"`, or any exponential
#'       fit, which has no shape to stratify): `"hr"` returns the exact
#'       conditional hazard ratio, labeled `"HR"`, for a proportional-hazards
#'       distribution, and `"tr"` the exact time ratio (`"TR"`) for an
#'       accelerated failure time one. The two are different estimands and
#'       `"tr"` is **not** an alias for `"hr"`: a proportional-hazards model
#'       has no constant time ratio and an AFT model has no constant hazard
#'       ratio, so `"tr"` on a PH fit and `"hr"` on an AFT fit are both errors
#'       rather than the other measure returned under the label it does have.
#'     \item **Study-specific shape-bearing baseline** (`aux_by = ".study"`,
#'       the default, with a distribution that has a shape parameter or either
#'       flexible baseline): an explicit `"hr"` / `"tr"` request is an
#'       **error**. The conditional hazard ratio is
#'       `h0_index(t) / h0_comparator(t) * exp(eta_index - eta_comparator)` and
#'       the baseline ratio does not cancel, so no scalar hazard ratio exists to
#'       return. Returning the bare exponent under the name `hr` would report a
#'       different estimand than the one requested.
#'     \item `"all"` still works in that case and returns the exponentiated
#'       linear-predictor contrast under the honest label
#'       `"EXP_ETA_CONTRAST"`, with a warning explaining what it is not. The
#'       warning is emitted whenever the baseline shapes differ, for
#'       accelerated failure time models as well as proportional-hazards ones.
#'     \item The collapsible RMST effects from [marginal_effects()] are
#'       unaffected and are the recommended alternative.
#'       `predict(type = "loghr")` gives the time-varying hazard ratio
#'       standardized over a **population**, so it is a marginal quantity and
#'       not the conditional effect at a supplied covariate profile; it answers
#'       a different question rather than substituting for this one.
#'   }
#' @param summary Return summary statistics (`TRUE`) or full posterior draws
#'   (`FALSE`)
#' @param probs Quantiles for summary (default `c(0.025, 0.5, 0.975)`)
#'
#' @details
#' For SPFA models, the conditional link-scale treatment effect is constant
#' across all covariate values because the shared beta cancels in the treatment
#' contrast on the fitted link scale. However, risk difference (RD) and risk
#' ratio (RR) still vary with covariates because they depend on absolute
#' probability levels. For relaxed models, all conditional effects vary with
#' covariate values because the index and comparator treatments have different
#' regression coefficients.
#'
#' For binomial / normal / Poisson the conditional link-scale effect is computed
#' directly as `eta_index - eta_comparator`. This avoids numerical distortion
#' from transforming extreme response-scale probabilities back through the link
#' function. For **survival** the contrast is reported on the natural scale,
#' exponentiated from `eta_index - eta_comparator` (null 1, like the rate
#' ratio). That exponent is the conditional hazard ratio (or AFT time ratio)
#' only when the two studies share a baseline shape; under the stratified
#' default it is labeled `"EXP_ETA_CONTRAST"` instead, because the baseline
#' ratio `h0_index(t) / h0_comparator(t)` does not cancel, and for an
#' accelerated failure time model because differing shapes add
#' quantile-dependent factors. `predict(type = "loghr")` gives the time-varying
#' log hazard ratio standardized over a population, which is a marginal
#' quantity rather than a profile-specific conditional one.
#'
#' **Conditional vs marginal on non-identity links.** Conditional effects are
#' evaluated at a single covariate profile, so there is no averaging over a
#' population and no Jensen's-inequality gap between the conditional and
#' marginal response. Compare with [marginal_effects()] and
#' [predict.mlumr_fit()], which average over either the IPD individuals
#' (index population) or the AgD integration points (comparator population)
#' and therefore return `E[g^{-1}(eta)]`, not `g^{-1}(E[eta])`.
#'
#' @return A data frame. If `summary = TRUE`, contains columns `profile`,
#'   `effect`, `mean`, `sd`, and quantile columns. If `summary = FALSE`,
#'   returns a single combined data frame of full posterior draws with a
#'   `profile` column indicating which covariate profile each draw belongs
#'   to.
#' @seealso [marginal_effects()] for population-averaged treatment effects;
#'   [conditional_predict()] for absolute predictions at specific profiles;
#'   [predict.mlumr_fit()] for population-level predictions.
#' @export
#'
#' @examples
#' \dontrun{
#' # Conditional effects at IPD covariate means (default)
#' conditional_effects(fit)
#'
#' # At specific covariate values
#' conditional_effects(fit, newdata = data.frame(age = 60, sex = 1))
#'
#' # Multiple profiles
#' profiles <- data.frame(age = c(50, 60, 70), sex = c(0, 0, 1))
#' conditional_effects(fit, newdata = profiles)
#' }
conditional_effects <- function(object,
                                newdata = NULL,
                                effect = "all",
                                summary = TRUE,
                                probs = c(0.025, 0.5, 0.975)) {

  .validate_mlumr_fit_object(object)
  effect <- .validate_effect_choice(effect)
  summary <- .validate_summary_flag(summary)
  .validate_probs(probs)

  family <- object$family %||% "binomial"
  cfg_family <- get_family_config(family)
  lnk <- object$link %||% cfg_family$link_default

  if (effect == "lor") {
    if (family == "binomial" && lnk == "logit") {
      effect <- "link_effect"
    } else if (family == "binomial") {
      stop(sprintf(
        "'lor' is only valid when link is 'logit'. For link '%s', use 'link_effect' instead.",
        lnk
      ), call. = FALSE)
    }
  }

  valid_effects <- .conditional_effect_choices(family)
  if (!effect %in% valid_effects) {
    stop(sprintf("For %s family, `effect` must be one of: %s",
                 family, paste(valid_effects, collapse = ", ")), call. = FALSE)
  }
  # A hazard ratio and a time ratio are different estimands, and only one of
  # them exists as a scalar for any given fit. `tr` was previously aliased to
  # `hr` and the answer labeled from the fitted distribution, which meant a
  # request for one could be answered with the other. Validate instead: an
  # explicit request either returns what was asked for or errors.
  if (identical(family, "survival") && effect %in% c("hr", "tr")) {
    is_ph <- isTRUE(object$surv_info$is_ph)
    if (is_ph && identical(effect, "tr")) {
      stop("`effect = \"tr\"` is only available for accelerated failure time ",
           "distributions. This is a proportional-hazards '",
           object$distribution %||% "survival",
           "' fit, whose conditional treatment effect is a hazard ratio; a ",
           "proportional-hazards model has no constant time ratio. Use ",
           "`effect = \"hr\"`.",
           call. = FALSE)
    }
    if (!is_ph && identical(effect, "hr")) {
      stop("`effect = \"hr\"` is not a scalar conditional effect for this ",
           "accelerated failure time ('", object$distribution %||% "survival",
           "') model: the conditional hazard ratio varies with time. Use ",
           "`effect = \"tr\"` for the time ratio when the baseline shapes are ",
           "shared, or predict(type = \"loghr\") for the population-",
           "standardized time-varying hazard ratio.",
           call. = FALSE)
    }
    # Under differing baseline shapes exp(eta_index - eta_comparator) is
    # neither a hazard ratio (the h0 ratio does not cancel) nor a time ratio
    # (differing shapes add quantile-dependent factors). Returning it under the
    # requested name would be the relabeling this guard exists to prevent, so
    # an EXPLICIT `hr` / `tr` request is an error. The default `effect = "all"`
    # still returns the contrast under its own name, with the warning below.
    if (.aux_shapes_differ(object)) {
      stop("`effect = \"", effect, "\"` is not available: each study has its own ",
           "baseline ", if (is_ph) "hazard" else "shape",
           " (`aux_by = \".study\"`), so exp(eta_index - eta_comparator) is not ",
           "a conditional ", if (is_ph) "hazard ratio" else "time ratio",
           ". Use `effect = \"all\"` for the contrast under its own name, ",
           "predict(type = \"loghr\") for the time-varying hazard ratio, or ",
           "refit with `aux_by = \"none\"`.",
           call. = FALSE)
    }
  }

  profiles <- .conditional_profiles(object, newdata)
  X <- profiles$X
  n_profiles <- nrow(X)
  params <- .conditional_parameters(object, profiles$covariates)
  results <- vector("list", n_profiles)

  # The contrast type depends only on the fit, not on the covariate profile, so
  # resolve it once. Inside the loop this emitted the same long warning for
  # every row of `newdata`.
  if (.aux_shapes_differ(object)) {
    why <- if (isTRUE(object$surv_info$is_ph)) {
      paste0("the conditional hazard ratio varies with time and the ",
             "reported exp(eta_index - eta_comparator) is not it: the ",
             "baseline ratio h0_index(t) / h0_comparator(t) does not cancel")
    } else {
      paste0("the conditional time ratio depends on the survival quantile ",
             "and the reported exp(eta_index - eta_comparator) is not it: ",
             "differing shapes add quantile-dependent factors (the Weibull ",
             "[-log S]^(1/a_i - 1/a_c), the log-normal ",
             "exp(z_p (sigma_i - sigma_c)), and so on) that do not cancel")
    }
    warning("Each study has its own baseline ",
            if (isTRUE(object$surv_info$is_ph)) "hazard" else "shape",
            " (`aux_by = \".study\"`, the default), so ", why,
            ". Target-standardized RMST effects from marginal_effects() ",
            "remain available, and `aux_by = \"none\"` gives a single shared ",
            "baseline under which this contrast is exact. Note that ",
            "predict(type = \"loghr\") is a population-standardized ",
            "MARGINAL curve, not the conditional effect at this covariate ",
            "profile, so it answers a different question rather than ",
            "replacing this one.",
            call. = FALSE)
  }

  for (i in seq_len(n_profiles)) {
    eta <- .conditional_eta(params, X[i, , drop = FALSE])
    eta_idx <- eta$index
    eta_cmp <- eta$comparator

    if (family == "binomial") {
      logp_idx <- .binary_log_probs(eta_idx, lnk)$event
      logp_cmp <- .binary_log_probs(eta_cmp, lnk)$event
      profile_draws <- data.frame(
        link_effect = eta_idx - eta_cmp,
        rd = .exp_difference_logs(logp_idx, logp_cmp),
        rr = exp(logp_idx - logp_cmp)
      )
    } else if (family == "normal") {
      if (lnk == "identity") {
        profile_draws <- data.frame(
          md = eta_idx - eta_cmp
        )
      } else {
        profile_draws <- data.frame(
          md = .exp_difference_logs(eta_idx, eta_cmp)
        )
      }
    } else if (family == "poisson") {
      profile_draws <- data.frame(
        rr = exp(eta_idx - eta_cmp)
      )
    } else {
      # survival: conditional hazard ratio (PH) / time ratio (AFT) on the natural
      # scale (exponentiated, null 1), matching the poisson rate ratio above. The
      # column is named hr (PH) or tr (AFT) so summary = FALSE output is
      # self-describing.
      #
      # exp(eta_index - eta_comparator) is the conditional hazard ratio ONLY when
      # the two studies share a baseline. That is no longer the default: with
      # aux_by = ".study" the true
      # conditional HR is h0_index(t)/h0_comparator(t) * exp(eta_i - eta_c), and
      # the baseline ratio does not cancel. Reporting the bare exponent would be
      # the same error the marginal delta_* used to make, so say so rather than
      # print a number that means something else.
      # The EXP_ETA_CONTRAST label is applied whenever the shapes differ, PH or
      # AFT alike, so the warning has to cover both. Gating it on `is_ph` left a
      # stratified AFT fit relabeling its estimand silently while the
      # documentation promised a warning.
      # Under a stratified baseline the exponentiated contrast is neither a
      # hazard ratio (PH: the h0 ratio does not cancel) nor a time ratio (AFT:
      # differing shape/scale add quantile-dependent factors). Name it for what
      # it is, so the returned column cannot be read as HR/TR. The warning above
      # explains why; the name is what stops it being published as an HR.
      profile_draws <- data.frame(value = exp(eta_idx - eta_cmp))
      names(profile_draws) <- .surv_contrast_name(object)
    }

    results[[i]] <- profile_draws
  }

  # Filter to requested effects
  if (family == "binomial") {
    keep_cols <- if (effect == "all") c("link_effect", "rd", "rr") else effect
  } else if (family == "normal") {
    keep_cols <- "md"
  } else if (family == "poisson") {
    keep_cols <- "rr"
  } else {
    keep_cols <- .surv_contrast_name(object)
  }

  if (!summary) {
    out <- lapply(seq_along(results), function(i) {
      d <- results[[i]][, keep_cols, drop = FALSE]
      d$profile <- i
      d
    })
    return(do.call(rbind, out))
  }

  summary_list <- lapply(seq_along(results), function(i) {
    d <- results[[i]][, keep_cols, drop = FALSE]
    summary_df <- .summarize_draw_matrix(d, probs)
    summary_df$profile <- i
    summary_df$effect <- toupper(rownames(summary_df))
    summary_df
  })

  out <- do.call(rbind, summary_list)
  out <- out[, c("profile", "effect", "mean", "sd",
                 .quantile_names(probs)), drop = FALSE]
  # Survival effects are on the natural scale (null 1): the effect label is
  # already HR (PH) or TR (AFT) from the hr / tr column name set above.
  rownames(out) <- NULL
  .mlumr_result(out, "mlumr_conditional_effects", family = family)
}

#' Build covariate profiles for conditional summaries
#' @keywords internal
.conditional_profiles <- function(object, newdata = NULL) {
  covariates <- object$data$covariates

  if (is.null(newdata)) {
    X_ipd <- as.matrix(object$data$ipd$data[, covariates, drop = FALSE])
    col_means <- colMeans(X_ipd)
    newdata <- as.data.frame(
      matrix(col_means, nrow = 1, dimnames = list(NULL, covariates))
    )
  }

  if (!is.data.frame(newdata)) {
    stop("`newdata` must be a data frame.", call. = FALSE)
  }
  if (nrow(newdata) == 0L) {
    stop("`newdata` must contain at least one row.", call. = FALSE)
  }
  if (!all(covariates %in% names(newdata))) {
    missing <- setdiff(covariates, names(newdata))
    stop(sprintf("Missing covariates in `newdata`: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }

  profile_data <- newdata[, covariates, drop = FALSE]
  non_numeric <- covariates[
    !vapply(profile_data, is.numeric, logical(1))
  ]
  if (length(non_numeric) > 0L) {
    stop(sprintf("Covariates in `newdata` must be numeric: %s",
                 paste(non_numeric, collapse = ", ")), call. = FALSE)
  }

  X <- as.matrix(profile_data)
  storage.mode(X) <- "double"
  if (any(!is.finite(X))) {
    stop("`newdata` covariates must be finite.", call. = FALSE)
  }

  # Shift user-supplied (raw-scale) covariate values onto the centered scale
  # used at fit time, so they are consistent with the fitted (centered)
  # intercept and coefficients. `cov_center` is set for all families when
  # center = TRUE (the mlumr default); a vector of zeros (center = FALSE) makes
  # this a no-op.
  cov_center <- object$stan_data$cov_center %||% rep(0, length(covariates))
  X <- sweep(X, 2, cov_center)

  list(
    X = X,
    covariates = covariates
  )
}

#' Extract posterior parameter draws for conditional summaries
#' @keywords internal
.conditional_parameters <- function(object, covariates) {
  draws <- object$draws
  n_cov <- length(covariates)
  is_relaxed <- object$model == "relaxed"

  .require_draw_columns(draws, c("mu_index", "mu_comparator"),
                        "conditional parameter")
  out <- list(
    mu_index = draws$mu_index,
    mu_comparator = draws$mu_comparator,
    is_relaxed = is_relaxed
  )

  if (is_relaxed) {
    beta_index_cols <- paste0("beta_index[", seq_len(n_cov), "]")
    beta_comparator_cols <- paste0("beta_comparator[", seq_len(n_cov), "]")
    .require_draw_columns(draws, c(beta_index_cols, beta_comparator_cols),
                          "conditional parameter")
    out$beta_index <- as.matrix(
      draws[, beta_index_cols, drop = FALSE]
    )
    out$beta_comparator <- as.matrix(
      draws[, beta_comparator_cols, drop = FALSE]
    )
  } else {
    beta_cols <- paste0("beta[", seq_len(n_cov), "]")
    .require_draw_columns(draws, beta_cols, "conditional parameter")
    out$beta <- as.matrix(
      draws[, beta_cols, drop = FALSE]
    )
  }

  out
}

#' Compute conditional linear predictors for one profile
#' @keywords internal
.conditional_eta <- function(params, x) {
  if (params$is_relaxed) {
    eta_idx <- params$mu_index + as.vector(params$beta_index %*% t(x))
    eta_cmp <- params$mu_comparator +
      as.vector(params$beta_comparator %*% t(x))
  } else {
    eta_idx <- params$mu_index + as.vector(params$beta %*% t(x))
    eta_cmp <- params$mu_comparator + as.vector(params$beta %*% t(x))
  }

  list(index = eta_idx, comparator = eta_cmp)
}

#' Conditional effect choices by family
#' @keywords internal
.conditional_effect_choices <- function(family) {
  if (family == "binomial") {
    c("all", "link_effect", "rd", "rr")
  } else if (family == "normal") {
    c("all", "md")
  } else if (family == "poisson") {
    c("all", "rr")
  } else {
    c("all", "hr", "tr")
  }
}

#' Conditional predictions
#'
#' Generate absolute predictions at specific covariate values for both
#' treatments.
#'
#' @param object An `mlumr_fit` object
#' @param newdata Data frame of covariate values. If `NULL`, uses IPD covariate
#'   means.
#' @param type `"response"` for probabilities, means, or rates; `"link"` for
#'   the fitted linear-predictor scale. `NULL`, the default, resolves to
#'   `"response"` for binomial, normal and Poisson fits. Ignored for survival
#'   fits, which return the conditional survival probability S(t | x) at each
#'   fitted prediction time.
#' @param summary Return summary (`TRUE`) or full draws (`FALSE`)
#' @param probs Quantiles for summary
#'
#' @return A data frame with predictions for each treatment at each profile.
#'   For survival fits there is one row per profile, treatment, and time.
#' @seealso [conditional_effects()] for covariate-conditional treatment
#'   *effects*; [predict.mlumr_fit()] for population-level predictions.
#' @export
#'
#' @examples
#' \dontrun{
#' conditional_predict(fit)
#' conditional_predict(fit, newdata = data.frame(age = 60, sex = 1))
#' }
conditional_predict <- function(object,
                                newdata = NULL,
                                type = NULL,
                                summary = TRUE,
                                probs = c(0.025, 0.5, 0.975)) {

  .validate_mlumr_fit_object(object)
  summary <- .validate_summary_flag(summary)
  .validate_probs(probs)

  family <- object$family %||% "binomial"
  if (family == "survival") {
    return(.conditional_predict_survival(object, newdata, summary, probs))
  }

  type <- .validate_predict_choice(type %||% "response", c("response", "link"),
                                   "type")
  profiles <- .conditional_profiles(object, newdata)
  X <- profiles$X
  n_profiles <- nrow(X)
  params <- .conditional_parameters(object, profiles$covariates)
  lnk <- object$link %||% get_family_config(family)$link_default

  summarize_col <- function(x) .summarize_draw_vector(x, probs)

  results <- vector("list", n_profiles)

  for (i in seq_len(n_profiles)) {
    eta <- .conditional_eta(params, X[i, , drop = FALSE])
    eta_idx <- eta$index
    eta_cmp <- eta$comparator

    if (type == "response" && lnk != "identity") {
      val_idx <- inverse_link(eta_idx, lnk)
      val_cmp <- inverse_link(eta_cmp, lnk)
    } else {
      val_idx <- eta_idx
      val_cmp <- eta_cmp
    }

    if (!summary) {
      results[[i]] <- data.frame(
        profile = i,
        index = val_idx,
        comparator = val_cmp
      )
    } else {
      s_idx <- summarize_col(val_idx)
      s_cmp <- summarize_col(val_cmp)
      qcols <- .quantile_names(probs)

      results[[i]] <- data.frame(
        profile = c(i, i),
        treatment = c(object$data$index_treatment,
                      object$data$comparator_treatment),
        mean = c(s_idx["mean"], s_cmp["mean"]),
        sd = c(s_idx["sd"], s_cmp["sd"]),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      for (j in seq_along(probs)) {
        qname <- qcols[j]
        results[[i]][[qname]] <- c(s_idx[[qname]], s_cmp[[qname]])
      }
    }
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}


#' Conditional survival predictions S(t | x) at covariate profiles
#'
#' Internal dispatch for [conditional_predict()] on survival fits. Returns the
#' conditional survival probability for each treatment at each profile and
#' fitted prediction time. Conditional hazard and RMST are well-defined, but
#' this helper currently returns survival only; use [predict.mlumr_fit()] for
#' population-standardized hazard and RMST summaries.
#' @keywords internal
.conditional_predict_survival <- function(object, newdata, summary, probs) {
  profiles <- .conditional_profiles(object, newdata)
  X <- profiles$X
  n_profiles <- nrow(X)
  params <- .conditional_parameters(object, profiles$covariates)
  pred_times <- object$pred_times
  idx_trt <- object$data$index_treatment
  cmp_trt <- object$data$comparator_treatment

  rows <- list()
  for (i in seq_len(n_profiles)) {
    eta <- .conditional_eta(params, X[i, , drop = FALSE])
    cells <- list(
      list(trt = idx_trt, mat = .surv_eval_curve(object, eta$index, "index")),
      list(trt = cmp_trt,
           mat = .surv_eval_curve(object, eta$comparator, "comparator"))
    )
    for (cell in cells) {
      if (!summary) {
        df <- as.data.frame(cell$mat)
        colnames(df) <- sprintf("t_%.15g", pred_times)
        df$profile <- i
        df$treatment <- cell$trt
        rows[[length(rows) + 1L]] <- df
      } else {
        sm <- .summarize_draw_matrix(cell$mat, probs)
        rows[[length(rows) + 1L]] <- data.frame(
          profile = i, treatment = cell$trt, time = pred_times, sm,
          row.names = NULL
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Name for the conditional survival contrast
#'
#' `exp(eta_index - eta_comparator)` is a conditional hazard ratio only when the
#' two studies share a baseline hazard, and a time ratio only when the AFT
#' shape/scale parameters are shared. The test is whether the auxiliary
#' shape/scale draws actually differ, not whether `aux_by` asked for strata: an
#' exponential has no shape to stratify, so `aux_by = ".study"` leaves its
#' baseline shared and the exact `hr` label stands.
#' @param object An `mlumr_fit` (survival).
#' @return `"hr"`, `"tr"`, or `"exp_eta_contrast"` when the two baselines'
#'   shape/scale parameters differ.
#' @keywords internal
.surv_contrast_name <- function(object) {
  # `n_strata > 1` is not the question: an exponential has no shape to
  # stratify, so its baseline hazard is exp(eta) with the study intercept
  # already inside eta, and exp(eta_index - eta_comparator) is an exact
  # conditional hazard ratio however `aux_by` is set. `.aux_shapes_differ()`
  # asks whether the baselines actually differ.
  if (.aux_shapes_differ(object)) return("exp_eta_contrast")
  if (isTRUE(object$surv_info$is_ph)) "hr" else "tr"
}

#' Evaluate the conditional survival curve S(t | profile) per posterior draw
#'
#' @param object An `mlumr_fit` (survival).
#' @param eta Numeric vector (length n_draws) of the per-draw linear predictor
#'   at one covariate profile for one treatment.
#' @param treatment Which arm's baseline hazard to evaluate against. With
#'   `aux_by = ".study"` (the default) each study has its own baseline, so the
#'   curve depends on which arm is being predicted.
#' @return A matrix of survival probabilities, draws (rows) by fitted
#'   prediction times (columns).
#' @keywords internal
.surv_eval_curve <- function(object, eta,
                             treatment = c("index", "comparator")) {
  treatment <- match.arg(treatment)
  pred_times <- object$pred_times

  # The baseline belongs to the study and each study contributes one arm, so it
  # travels with the treatment. Reading it through the shared helpers keeps this
  # path working for a stratified baseline AND for the matrix draw names that
  # every fit now uses, stratified or not.
  if (object$surv_info$kind == "parametric") {
    dist <- object$surv_info$dist_code
    aux  <- .surv_aux_draws(object, "aux_val", treatment, length(eta))
    aux2 <- .surv_aux_draws(object, "aux2_val", treatment, length(eta))
    # vapply keeps the dimensions when there are several draws, but collapses
    # to a bare vector when the posterior holds exactly one, and the caller
    # then applies over a non-existent second margin. Shape it explicitly, as
    # .surv_s_at_times() already does on the same quantity.
    matrix(vapply(pred_times,
                  function(t) exp(.r_log_surv(dist, t, eta, aux, aux2)),
                  numeric(length(eta))),
           nrow = length(eta), ncol = length(pred_times))
  } else {
    scoef <- .surv_scoef_draws(object, treatment)
    # Each study has its own basis under `aux_by = ".study"`; the comparator arm
    # must not be evaluated on the index study's spline. Older fits have no
    # `_cmp` matrix, so fall back to the shared basis.
    pred_ib <- if (identical(treatment, "comparator")) {
      object$stan_data$pred_ibasis_cmp %||% object$stan_data$pred_ibasis
    } else {
      object$stan_data$pred_ibasis
    }
    cum_haz <- scoef %*% t(pred_ib)            # [n_draws, n_times]
    exp(-exp(log(cum_haz) + eta))              # eta recycled down columns
  }
}


#' Log survival S(t | eta) in R, mirroring the Stan log_surv_scalar()
#'
#' Vectorized over posterior draws (`eta`, `aux`, `aux2` are vectors). `t` is
#' usually a scalar time, but a vector recycled against the draws is also
#' supported, which is how the likelihood tests evaluate several times at once.
#' @keywords internal
.r_log_surv <- function(dist, t, eta, aux, aux2) {
  if (dist == 1L) return(-exp(log(t) + eta))
  if (dist == 2L) return(-exp(aux * log(t) + eta))
  if (dist == 3L) {
    at <- aux * t
    return(-exp(eta - log(aux) + at + log(-expm1(-at))))
  }
  if (dist == 4L) return(-exp(log(t) - eta))
  if (dist == 5L) return(-exp(aux * (log(t) - eta)))
  if (dist == 6L) {
    return(stats::plnorm(t, meanlog = eta, sdlog = aux,
                         lower.tail = FALSE, log.p = TRUE))
  }
  if (dist == 7L) {
    z <- aux * (log(t) - eta)
    return(-(pmax(z, 0) + log1p(exp(-abs(z)))))
  }
  if (dist == 8L) {
    return(stats::pgamma(exp(log(t) - eta), shape = aux, rate = 1,
                         lower.tail = FALSE, log.p = TRUE))
  }
  # Generalized gamma (dist 9). Form the incomplete-gamma argument in log
  # space and let R's upper-tail pgamma implementation choose its stable
  # central or tail algorithm. The continued fraction used for deep tails is
  # not reliable just above a very large shape parameter.
  q <- 1 / sqrt(aux2)
  log_w <- q * (log(t) - eta) / aux + log(aux2)
  k <- rep_len(aux2, length(log_w))
  out <- rep(NA_real_, length(log_w))
  # `exp(log_w)` underflows to zero below about -745, and `pgamma(0, k)` then
  # reports survival 1. For a small shape that is badly wrong: the survival
  # depends on `w^k`, which is `exp(k * log_w)` and stays of order one however
  # far `log_w` has gone. At `k = 1e-6` and `log_w = -1013.8` the true value is
  # 0.0010127 and the underflowed one is exactly 1, so a censored observation
  # contributes nothing to the likelihood instead of about -6.9.
  #
  # Below the threshold the leading term of the series for the lower
  # regularized gamma, `w^k / gamma(k + 1)`, is exact to double precision,
  # because the next term is smaller by a factor of `w`.
  small <- !is.na(log_w) & log_w < -700
  if (any(small)) {
    log_p <- pmin(k[small] * log_w[small] - lgamma(k[small] + 1), 0)
    out[small] <- log(-expm1(log_p))
  }
  rest <- !is.na(log_w) & !small
  if (any(rest)) {
    out[rest] <- stats::pgamma(exp(log_w[rest]), shape = k[rest],
                               lower.tail = FALSE, log.p = TRUE)
  }
  out
}


#' Log of the upper-incomplete-gamma continued-fraction factor
#' @keywords internal
.r_log_gamma_q_cf_factor <- function(k, x) {
  tiny <- 1e-300
  b <- x + 1 - k
  c <- 1 / tiny
  d <- 1 / b
  h <- d
  for (i in seq_len(300L)) {
    an <- -i * (i - k)
    b <- b + 2
    d <- an * d + b
    if (abs(d) < tiny) d <- tiny
    c <- b + an / c
    if (abs(c) < tiny) c <- tiny
    d <- 1 / d
    delta <- d * c
    h <- h * delta
    if (abs(delta - 1) < 1e-14) break
  }
  log(h)
}


#' Log hazard h(t | eta) in R, mirroring Stan log_haz_full()
#' @keywords internal
.r_log_haz <- function(dist, t, eta, aux, aux2) {
  log_t <- log(t)
  if (dist == 1L) return(eta)
  if (dist == 2L) return(log(aux) + aux * log_t + eta - log_t)
  if (dist == 3L) return(eta + aux * t)
  if (dist == 4L) return(-eta)
  if (dist == 5L) return(log(aux) + aux * (log_t - eta) - log_t)
  if (dist == 6L) {
    z <- (log_t - eta) / aux
    log_mills <- stats::dnorm(z, log = TRUE) -
      stats::pnorm(z, lower.tail = FALSE, log.p = TRUE)
    tail <- z > 20
    if (any(tail)) {
      iz2 <- 1 / z[tail]^2
      ratio <- 1 + iz2 * (1 + iz2 * (-2 + iz2 * (10 - 74 * iz2)))
      log_mills[tail] <- log(z[tail]) + log(ratio)
    }
    return(log_mills - log(aux) - log_t)
  }
  if (dist == 7L) {
    z <- aux * (log_t - eta)
    return(log(aux) - log_t - (pmax(-z, 0) + log1p(exp(-abs(z)))))
  }
  if (dist == 8L) {
    log_z <- log_t - eta
    z <- exp(log_z)
    a <- rep_len(aux, length(eta))
    out <- numeric(length(eta))
    infinite <- is.infinite(z)
    tail <- !infinite & z > a + pmax(1, sqrt(a))
    central <- !infinite & !tail
    if (any(central)) {
      log_s <- stats::pgamma(z[central], shape = a[central],
                             lower.tail = FALSE, log.p = TRUE)
      out[central] <- (a[central] - 1) * log_z[central] - eta[central] -
        z[central] - lgamma(a[central]) - log_s
    }
    if (any(tail)) {
      out[tail] <- -log_t - mapply(.r_log_gamma_q_cf_factor,
                                    a[tail], z[tail])
    }
    out[infinite] <- -eta[infinite]
    return(out)
  }

  k <- rep_len(aux2, length(eta))
  sigma <- rep_len(aux, length(eta))
  z <- (1 / sqrt(k)) * (log_t - eta) / sigma
  log_w <- log(k) + z
  w <- exp(log_w)
  out <- numeric(length(eta))
  infinite <- is.infinite(w)
  tail <- !infinite & w > k + pmax(1, sqrt(k))
  central <- !infinite & !tail
  if (any(central)) {
    log_f <- -log(sigma[central]) - log_t -
      0.5 * log(k[central]) * (1 - 2 * k[central]) +
      k[central] * z[central] - w[central] - lgamma(k[central])
    log_s <- stats::pgamma(w[central], shape = k[central],
                           lower.tail = FALSE, log.p = TRUE)
    out[central] <- log_f - log_s
  }
  if (any(tail)) {
    out[tail] <- -log(sigma[tail]) - log_t - 0.5 * log(k[tail]) -
      mapply(.r_log_gamma_q_cf_factor, k[tail], w[tail])
  }
  out[infinite] <- -log(sigma[infinite]) - log_t +
    0.5 * log(k[infinite]) + z[infinite]
  out
}


#' Log density f(t | eta) in R, mirroring Stan log_density_scalar()
#' @keywords internal
.r_log_density <- function(dist, t, eta, aux, aux2) {
  log_t <- log(t)
  n <- length(eta)
  a <- rep_len(aux, n)
  a2 <- rep_len(aux2, n)

  if (dist <= 5L) {
    log_ch <- switch(
      as.character(dist),
      `1` = log_t + eta,
      `2` = a * log_t + eta,
      `3` = eta - log(a) + a * t + log(-expm1(-a * t)),
      `4` = log_t - eta,
      `5` = a * (log_t - eta)
    )
    out <- rep(-Inf, n)
    keep <- is.finite(log_ch) & log_ch <= 700
    if (any(keep)) {
      out[keep] <- .r_log_haz(
        dist, t, eta[keep], a[keep], a2[keep]
      ) - exp(log_ch[keep])
    }
    return(out)
  }
  if (dist == 6L) {
    z <- (log_t - eta) / a
    return(-0.5 * z^2 - log(a) - log_t - 0.5 * log(2 * pi))
  }
  if (dist == 7L) {
    z <- a * (log_t - eta)
    out <- log(a) - log_t
    upper <- z >= 0
    out[upper] <- out[upper] - z[upper] -
      2 * log1p(exp(-z[upper]))
    out[!upper] <- out[!upper] + z[!upper] -
      2 * log1p(exp(z[!upper]))
    return(out)
  }
  if (dist == 8L) {
    log_z <- log_t - eta
    out <- rep(-Inf, n)
    keep <- is.finite(log_z) & log_z <= 700
    out[keep] <- (a[keep] - 1) * log_z[keep] - eta[keep] -
      exp(log_z[keep]) - lgamma(a[keep])
    return(out)
  }

  z <- (log_t - eta) / (a * sqrt(a2))
  log_w <- log(a2) + z
  out <- rep(-Inf, n)
  keep <- is.finite(log_w) & log_w <= 700
  out[keep] <- -log(a[keep]) - log_t -
    0.5 * log(a2[keep]) * (1 - 2 * a2[keep]) +
    a2[keep] * z[keep] - exp(log_w[keep]) - lgamma(a2[keep])
  out
}
