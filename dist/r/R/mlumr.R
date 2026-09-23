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
#'   For **survival**, `exp(eta_index - eta_comparator)` is a conditional
#'   hazard ratio (`"hr"`, proportional-hazards distributions) or time ratio
#'   (`"tr"`, accelerated failure time distributions) only when the two
#'   studies share a baseline shape (`aux_by = "none"`, or any exponential
#'   fit). The two are different measures: `"tr"` on a proportional-hazards
#'   fit and `"hr"` on an AFT fit are errors, whose message gives the
#'   conversion where one exists (`TR = HR^(-1/shape)` for a Weibull, applied
#'   draw by draw; `TR = 1/HR` for an exponential). Under the default
#'   study-specific shapes an explicit `"hr"` or `"tr"` is an error, since
#'   the baseline ratio does not cancel; `"all"` returns the contrast under
#'   the label `"EXP_ETA_CONTRAST"` with a warning. The RMST effects from
#'   [marginal_effects()] are the recommended alternative, and
#'   `predict(type = "loghr")` gives the population-standardized curve.
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
#' For binomial, normal and Poisson the conditional link-scale effect is
#' `eta_index - eta_comparator`; for survival the contrast is exponentiated
#' (null 1) and labeled `"EXP_ETA_CONTRAST"` when the baseline shapes
#' differ. A conditional effect is evaluated at one profile, so unlike
#' [marginal_effects()] and [predict.mlumr_fit()] there is no averaging over
#' a population and no gap between `E[g^{-1}(eta)]` and `g^{-1}(E[eta])`.
#'
#' @return A data frame. If `summary = TRUE`, contains columns `profile`,
#'   `effect`, `mean`, `sd` and quantile columns. If `summary = FALSE`,
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
  summary <- .validate_flag(summary, "summary")
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
  # Only one of `hr` and `tr` exists as a scalar for a given fit.
  if (identical(family, "survival") && effect %in% c("hr", "tr")) {
    is_ph <- isTRUE(object$surv_info$is_ph)
    dist <- object$distribution %||% "survival"
    # The stratified case answers first: its refusal does not presume one shape.
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
    if (is_ph && identical(effect, "tr")) {
      stop("`effect = \"tr\"` is not what this fit parameterizes. This is a ",
           "proportional-hazards '", dist,
           "' fit, so the coefficient it estimates is a log hazard ratio and ",
           "`effect = \"hr\"` is what returns it. ",
           .dual_family_note(dist, "tr"),
           call. = FALSE)
    }
    if (!is_ph && identical(effect, "hr")) {
      stop("`effect = \"hr\"` is not what this fit parameterizes. This is an ",
           "accelerated failure time '", dist,
           "' fit, so the coefficient it estimates is a log time ratio and ",
           "`effect = \"tr\"` is what returns it. ",
           .dual_family_note(dist, "hr"),
           call. = FALSE)
    }
  }

  profiles <- .conditional_profiles(object, newdata)
  X <- profiles$X
  n_profiles <- nrow(X)
  params <- .conditional_parameters(object, profiles$covariates)
  results <- vector("list", n_profiles)

  # Resolved once: the contrast type does not depend on the profile.
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
            ". RMST effects from marginal_effects() remain available, and ",
            "`aux_by = \"none\"` makes this contrast exact. predict(type = ",
            "\"loghr\") is a population-standardized MARGINAL curve, not the ",
            "conditional effect at this profile.",
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
      # Survival: natural scale, null 1, named hr or tr only when the shapes
      # are shared and exp_eta_contrast otherwise.
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

  kept <- lapply(results, function(r) r[, keep_cols, drop = FALSE])
  .warn_dropped_draws(do.call(cbind, kept))
  summary_list <- lapply(seq_along(kept), function(i) {
    summary_df <- .summarize_draw_matrix(kept[[i]], probs, warn = FALSE)
    summary_df$profile <- i
    summary_df$effect <- toupper(rownames(summary_df))
    summary_df
  })

  out <- do.call(rbind, summary_list)
  out <- out[, c("profile", "effect", "mean", "sd", .quantile_names(probs)),
             drop = FALSE]
  rownames(out) <- NULL
  .mlumr_result(out, "mlumr_conditional_effects", family = family)
}

#' Say whether the unrequested measure exists as a scalar for this family
#'
#' The exponential and the Weibull are both proportional hazards and AFT, so
#' with a shared shape the other measure is a deterministic transform; the
#' log-normal, log-logistic, gamma and generalized gamma have a time-varying
#' hazard ratio, where no scalar exists.
#'
#' @param dist The fitted distribution.
#' @param asked The measure the caller asked for, `"hr"` or `"tr"`.
#' @return A single string to append to the error message.
#' @noRd
.dual_family_note <- function(dist, asked) {
  dual <- c("exponential", "weibull", "exponential-aft", "weibull-aft")
  if (dist %in% dual) {
    common <- paste0(
      "This distribution is both proportional-hazards and accelerated ",
      "failure time, so with a baseline shape shared across arms the other ",
      "measure is a deterministic transform of this one: "
    )
    # The exponential has NO shape parameter, so the paired-draw and
    # transformed-prior advice below does not apply to it: its conversion is a
    # reciprocal, and applying it to each draw is all there is to do.
    if (grepl("^exponential", dist)) {
      return(paste0(
        common, "TR = 1/HR for an exponential. ",
        "The exponential carries no shape parameter, so apply that to each ",
        "draw; there is nothing further to pair it with, and a normal prior ",
        "on log(HR) is the same normal prior on log(TR) with its sign ",
        "reversed."
      ))
    }
    return(paste0(
      common, "TR = HR^(-1/shape) for a Weibull. ",
      "Convert DRAW BY DRAW, pairing each effect draw with the shape draw it ",
      "was sampled with: the shape is uncertain, and transforming a posterior ",
      "mean or the endpoints of a reported interval with a single shape ",
      "estimate does not reproduce the posterior of the other measure. ",
      "Refitting in the other parameterization is an equivalent analysis only ",
      "if the priors are transformed to match, since a normal prior on ",
      "log(HR) induces one on log(TR) whose scale depends on the shape."
    ))
  }
  if (identical(asked, "hr")) {
    return(paste0(
      "For this distribution the conditional hazard ratio generally varies ",
      "with time, so there is no scalar to convert to. Use ",
      "predict(type = \"loghr\") for the population-standardized ",
      "time-varying hazard ratio."
    ))
  }
  "For this distribution there is no constant time ratio to convert to."
}

#' Build covariate profiles for conditional summaries
#' @noRd
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

  # Onto the centered scale used at fit time (zeros when center = FALSE).
  cov_center <- object$stan_data$cov_center %||% rep(0, length(covariates))
  X <- sweep(X, 2, cov_center)

  list(
    X = X,
    covariates = covariates
  )
}

#' Extract posterior parameter draws for conditional summaries
#' @noRd
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
#' @noRd
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
#' @noRd
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
#'   For survival fits the summary has one row per profile, treatment, and
#'   time; with `summary = FALSE` the draws are rows and the prediction times
#'   are `t_*` columns.
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
  summary <- .validate_flag(summary, "summary")
  .validate_probs(probs)

  family <- object$family %||% "binomial"
  if (family == "survival") {
    return(.conditional_predict_survival(object, newdata, summary, probs))
  }

  type <- .validate_choice(type %||% "response", c("response", "link"),
                           "type")
  profiles <- .conditional_profiles(object, newdata)
  X <- profiles$X
  n_profiles <- nrow(X)
  params <- .conditional_parameters(object, profiles$covariates)
  lnk <- object$link %||% get_family_config(family)$link_default

  # Dropped draws are reported once for the whole set, after the loop.
  drawn <- list()
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
      drawn[[length(drawn) + 1L]] <- cbind(val_idx, val_cmp)
      s_idx <- .summarize_draw_vector(val_idx, probs, warn = FALSE)
      s_cmp <- .summarize_draw_vector(val_cmp, probs, warn = FALSE)
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
  if (summary) .warn_dropped_draws(do.call(cbind, drawn))

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
#' @noRd
.conditional_predict_survival <- function(object, newdata, summary, probs) {
  profiles <- .conditional_profiles(object, newdata)
  X <- profiles$X
  n_profiles <- nrow(X)
  params <- .conditional_parameters(object, profiles$covariates)
  pred_times <- object$pred_times
  idx_trt <- object$data$index_treatment
  cmp_trt <- object$data$comparator_treatment

  drawn <- list()
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
        drawn[[length(drawn) + 1L]] <- cell$mat
        sm <- .summarize_draw_matrix(cell$mat, probs, warn = FALSE)
        rows[[length(rows) + 1L]] <- data.frame(
          profile = i, treatment = cell$trt, time = pred_times, sm,
          row.names = NULL
        )
      }
    }
  }
  if (summary) .warn_dropped_draws(do.call(cbind, drawn))

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}


#' Name for the conditional survival contrast
#'
#' `exp(eta_index - eta_comparator)` is a hazard ratio or time ratio only
#' when the two studies' shape parameters agree. An exponential has no shape
#' to stratify, so `aux_by = ".study"` leaves its label exact.
#' @param object An `mlumr_fit` (survival).
#' @return `"hr"`, `"tr"`, or `"exp_eta_contrast"` when the two baselines'
#'   shape/scale parameters differ.
#' @noRd
.surv_contrast_name <- function(object) {
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
#' @noRd
.surv_eval_curve <- function(object, eta,
                             treatment = c("index", "comparator")) {
  treatment <- match.arg(treatment)
  pred_times <- object$pred_times

  # The baseline belongs to the study, so it travels with the treatment.
  if (object$surv_info$kind == "parametric") {
    dist <- object$surv_info$dist_code
    aux  <- .surv_aux_draws(object, "aux_val", treatment, length(eta))
    aux2 <- .surv_aux_draws(object, "aux2_val", treatment, length(eta))
    # Shaped explicitly: vapply collapses to a vector for a single draw.
    matrix(vapply(pred_times,
                  function(t) exp(.r_log_surv(dist, t, eta, aux, aux2)),
                  numeric(length(eta))),
           nrow = length(eta), ncol = length(pred_times))
  } else {
    scoef <- .surv_scoef_draws(object, treatment)
    # Each study has its own basis under `aux_by = ".study"`.
    pred_ib <- if (identical(treatment, "comparator")) {
      object$stan_data$pred_ibasis_cmp %||% object$stan_data$pred_ibasis
    } else {
      object$stan_data$pred_ibasis
    }
    cum_haz <- scoef %*% t(pred_ib)            # [n_draws, n_times]
    exp(-exp(log(cum_haz) + eta))              # eta recycled down columns
  }
}


#' Log upper regularized gamma from the log of its argument, in R
#'
#' Mirrors Stan's `log_gamma_surv_from_log_x()`. Below a `log_x` of -700,
#' where `exp(log_x)` would underflow and `pgamma(0, k)` report survival 1,
#' the lower tail is the exact leading series term `w^k / gamma(k + 1)` and
#' the return is its log complement, `log(1 - w^k / gamma(k + 1))`. Every R site needing
#' this quantity calls here, as every Stan site does.
#'
#' @param k Shape, recycled to the length of `log_x`.
#' @param log_x Log of the incomplete-gamma argument.
#' @return Log survival, the same length as `log_x`.
#' @noRd
.r_log_gamma_surv_from_log_x <- function(k, log_x) {
  k <- rep_len(k, length(log_x))
  out <- rep(NA_real_, length(log_x))
  ok <- !is.na(log_x)
  big <- ok & is.infinite(log_x) & log_x > 0
  out[big] <- -Inf
  small <- ok & !big & log_x < -700
  if (any(small)) {
    log_p <- pmin(k[small] * log_x[small] - lgamma(k[small] + 1), 0)
    out[small] <- log(-expm1(log_p))
  }
  rest <- ok & !big & !small
  if (any(rest)) {
    out[rest] <- stats::pgamma(exp(log_x[rest]), shape = k[rest],
                               lower.tail = FALSE, log.p = TRUE)
  }
  out
}

#' Log survival S(t | eta) in R, mirroring the Stan log_surv_scalar()
#'
#' Vectorized over posterior draws (`eta`, `aux`, `aux2` are vectors). `t` is
#' usually a scalar time, but a vector recycled against the draws is also
#' supported, which is how the likelihood tests evaluate several times at once.
#' @noRd
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
    return(.r_log_gamma_surv_from_log_x(aux, log(t) - eta))
  }
  # Generalized gamma (dist 9): form the argument in log space.
  q <- 1 / sqrt(aux2)
  log_w <- q * (log(t) - eta) / aux + log(aux2)
  .r_log_gamma_surv_from_log_x(aux2, log_w)
}


#' Log of the upper-incomplete-gamma continued-fraction factor
#' @noRd
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
#' @noRd
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
      log_s <- .r_log_gamma_surv_from_log_x(a[central], log_z[central])
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
    log_s <- .r_log_gamma_surv_from_log_x(k[central], log_w[central])
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
#' @noRd
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

#' Set up individual patient data (IPD)
#'
#' Prepare IPD from the index treatment for an unanchored indirect comparison.
#'
#' @param data Data frame containing IPD
#' @param treatment Column name for treatment variable
#' @param outcome Column name for outcome variable. For `family = "binomial"`,
#'   must be binary (0/1). For `family = "normal"`, any numeric. For
#'   `family = "poisson"`, non-negative integer counts. Not used (leave `NULL`)
#'   for `family = "survival"`, which uses `Surv`/`time`/`status` instead.
#' @param covariates Character vector of covariate column names
#' @param family Outcome family: `"binomial"`, `"normal"`, `"poisson"`, or
#'   `"survival"` (time-to-event)
#' @param exposure Column name for exposure/time-at-risk (required when
#'   `family = "poisson"`)
#' @param study Column name for study identifier (optional)
#' @param Surv For `family = "survival"`, an optional [survival::Surv()] object
#'   describing the outcome (use for left/interval censoring or delayed entry).
#' @param time,status,entry_time For `family = "survival"`, character column
#'   names as an alternative to `Surv` (right-censoring with status `0`/`1`,
#'   plus optional delayed entry).
#'
#' @return An object of class `mlumr_ipd`. Its `$data` holds the treatment,
#'   study, outcome and covariate columns under internal names, which cannot
#'   be used as column names in `data`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Binary outcome
#' ipd <- set_ipd(
#'   data = trial_a,
#'   treatment = "trt",
#'   outcome = "response",
#'   covariates = c("age", "sex")
#' )
#'
#' # Continuous outcome
#' ipd <- set_ipd(
#'   data = trial_a,
#'   treatment = "trt",
#'   outcome = "score",
#'   covariates = c("age", "sex"),
#'   family = "normal"
#' )
#'
#' # Count outcome with exposure
#' ipd <- set_ipd(
#'   data = trial_a,
#'   treatment = "trt",
#'   outcome = "events",
#'   covariates = c("age", "sex"),
#'   family = "poisson",
#'   exposure = "person_years"
#' )
#' }
set_ipd <- function(data, treatment, outcome = NULL, covariates,
                    family = c("binomial", "normal", "poisson", "survival"),
                    exposure = NULL, study = NULL,
                    Surv = NULL, time = NULL, status = NULL, entry_time = NULL) {

  family <- match.arg(family)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }

  if (family == "survival") {
    return(.set_ipd_survival(data, treatment, covariates, study,
                             Surv, time, status, entry_time))
  }

  if (is.null(outcome)) {
    stop("`outcome` is required for binomial, normal, and poisson families",
         call. = FALSE)
  }
  .validate_non_empty_data(data, "IPD")
  .validate_required_covariates(covariates, "covariates")

  required_cols <- .ipd_required_columns(treatment, outcome, covariates,
                                         exposure, study)

  .check_required_columns(data, required_cols)
  .validate_ipd_outcome(data, outcome, family, exposure)
  .validate_reserved_internal_names(
    c(covariates, treatment, outcome, exposure, study),
    c(".study", ".trt", ".outcome", ".exposure"),
    "Column name(s)"
  )
  .validate_ipd_covariates(data, covariates)
  .validate_ipd_finite_columns(data, outcome, exposure, family)

  data <- .drop_missing_rows(data, required_cols)
  .validate_complete_rows_remain(data, "IPD")
  .warn_constant_ipd_covariates(data, covariates)
  .warn_collinear_ipd_covariates(data, covariates)
  .validate_single_treatment(data, treatment, "IPD")
  ipd_data <- .standardize_ipd_data(data, treatment, outcome, covariates,
                                    family, exposure, study)

  out <- c(
    list(
      data = ipd_data,
      n = nrow(ipd_data),
      treatment = unique(ipd_data$.trt),
      covariates = covariates,
      family = family,
      type = "ipd"
    ),
    .ipd_outcome_summary(ipd_data, family)
  )

  class(out) <- c("mlumr_ipd", "list")
  out
}

#' Required source columns for IPD setup
#' @noRd
.ipd_required_columns <- function(treatment, outcome, covariates,
                                  exposure = NULL, study = NULL) {
  cols <- c(treatment, outcome, covariates)
  if (!is.null(exposure)) cols <- c(cols, exposure)
  if (!is.null(study)) cols <- c(cols, study)
  cols
}

#' Validate that required columns are present
#' @noRd
.check_required_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Missing columns in data: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate that input data contain at least one row
#' @noRd
.validate_non_empty_data <- function(data, label) {
  if (nrow(data) == 0L) {
    stop(sprintf("%s data must contain at least one row", label),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate covariate argument shape before column lookup
#' @noRd
.validate_required_covariates <- function(covariates, arg_name) {
  if (!is.character(covariates) || length(covariates) == 0L) {
    stop(sprintf("`%s` must be a non-empty character vector", arg_name),
         call. = FALSE)
  }
  if (any(is.na(covariates)) || any(covariates == "")) {
    stop(sprintf("`%s` must not contain missing or empty names", arg_name),
         call. = FALSE)
  }
  dup_covariates <- unique(covariates[duplicated(covariates)])
  if (length(dup_covariates) > 0L) {
    stop(sprintf("Duplicate covariates: %s",
                 paste(dup_covariates, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate IPD outcome and exposure columns
#' @noRd
.validate_ipd_outcome <- function(data, outcome, family, exposure = NULL) {
  if (family == "binomial") {
    outcome_vals <- data[[outcome]]
    if (!is.numeric(outcome_vals) && !is.logical(outcome_vals)) {
      stop("`outcome` must be numeric or logical 0/1 for binomial family",
           call. = FALSE)
    }
    invalid <- !is.na(outcome_vals) & !(outcome_vals %in% c(0, 1))
    if (any(invalid)) {
      stop("`outcome` must be binary (0/1) for binomial family", call. = FALSE)
    }
  } else if (family == "normal") {
    if (!is.numeric(data[[outcome]])) {
      stop("`outcome` must be numeric for normal family", call. = FALSE)
    }
  } else {
    if (is.null(exposure)) {
      stop("`exposure` is required for poisson family", call. = FALSE)
    }
    outcome_vals <- data[[outcome]]
    if (!.is_whole_number_count(outcome_vals, allow_missing = TRUE)) {
      stop("`outcome` must be non-negative integer counts for poisson family",
           call. = FALSE)
    }
    exposure_vals <- data[[exposure]]
    # The Stan models declare the exposures `<lower=1e-12>`.
    if (!is.numeric(exposure_vals) ||
          any(exposure_vals < .mlumr_min_positive, na.rm = TRUE)) {
      stop("`exposure` must be positive numeric values of at least 1e-12",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate a data source contains only one treatment
#' @noRd
.validate_single_treatment <- function(data, treatment, label) {
  raw <- data[[treatment]]
  # Reject missing labels: an all-NA column collapses to a single unique value
  # and would otherwise pass as "one treatment", fitting an unlabeled arm.
  if (anyNA(raw)) {
    stop(sprintf("%s treatment column '%s' contains missing (NA) values.",
                 label, treatment), call. = FALSE)
  }
  trt_vals <- unique(raw)
  if (length(trt_vals) < 1L) {
    stop(sprintf("%s treatment column '%s' has no values.", label, treatment),
         call. = FALSE)
  }
  if (length(trt_vals) > 1L) {
    stop(sprintf("%s should contain a single treatment. Found: %s",
                 label, paste(trt_vals, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Reject user names that would overwrite standardized internal columns
#' @noRd
.validate_reserved_internal_names <- function(user_names, reserved, label) {
  reserved_hit <- intersect(user_names, reserved)
  if (length(reserved_hit) > 0L) {
    msg <- sprintf(
      "%s collide with reserved internal columns: %s. Please rename and retry.",
      label,
      paste(reserved_hit, collapse = ", ")
    )
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate IPD covariates
#' @noRd
.validate_ipd_covariates <- function(data, covariates) {
  for (cov in covariates) {
    if (!is.numeric(data[[cov]])) {
      stop(sprintf("Covariate '%s' must be numeric", cov), call. = FALSE)
    }
    vals <- data[[cov]][!is.na(data[[cov]])]
    if (any(!is.finite(vals))) {
      stop(sprintf("Covariate '%s' contains non-finite values (Inf, -Inf, or NaN)", cov),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate finite IPD outcome and exposure values
#' @noRd
.validate_ipd_finite_columns <- function(data, outcome, exposure, family) {
  outcome_nona <- data[[outcome]][!is.na(data[[outcome]])]
  if (any(!is.finite(outcome_nona))) {
    stop("`outcome` contains non-finite values", call. = FALSE)
  }
  if (family == "poisson") {
    exposure_nona <- data[[exposure]][!is.na(data[[exposure]])]
    if (any(!is.finite(exposure_nona))) {
      stop("`exposure` contains non-finite values", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Drop rows with missing setup inputs
#' @noRd
.drop_missing_rows <- function(data, required_cols) {
  complete <- stats::complete.cases(data[, required_cols])
  if (!all(complete)) {
    n_missing <- sum(!complete)
    warning(sprintf("%d rows with missing values will be excluded", n_missing),
            call. = FALSE)
    data <- data[complete, ]
  }
  data
}

#' Validate that complete-case filtering left rows to analyze
#' @noRd
.validate_complete_rows_remain <- function(data, label) {
  if (nrow(data) == 0L) {
    stop(sprintf("No complete %s rows remain after excluding missing values",
                 label), call. = FALSE)
  }
  invisible(TRUE)
}

#' Warn when IPD covariates have no empirical variation
#' @noRd
.warn_constant_ipd_covariates <- function(data, covariates) {
  constant <- vapply(covariates, function(cov) {
    length(unique(data[[cov]])) <= 1L
  }, logical(1))

  if (any(constant)) {
    warning(
      paste(
        "IPD covariate(s) have zero empirical variation after missing-row",
        "filtering:",
        paste(covariates[constant], collapse = ", "),
        "Regression coefficients for constant covariates are not identified",
        "from the IPD; consider removing them or using informative priors."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Warn when two or more varying IPD covariates are near-linearly dependent
# (condition number of their correlation matrix above 1000). Constant
# covariates are handled by .warn_constant_ipd_covariates().
.warn_collinear_ipd_covariates <- function(data, covariates) {
  if (length(covariates) < 2L) {
    return(invisible(TRUE))
  }
  x <- data[stats::complete.cases(data[, covariates, drop = FALSE]), covariates,
            drop = FALSE]
  sds <- vapply(x, function(col) stats::sd(as.numeric(col)), numeric(1))
  keep <- is.finite(sds) & sds > 0
  x <- x[, keep, drop = FALSE]
  if (ncol(x) < 2L) {
    return(invisible(TRUE))
  }
  if (nrow(x) < ncol(x) + 1L) {
    # Fewer complete rows than intercept plus covariates: rank deficient.
    warning(
      paste0(
        "Only ", nrow(x), " complete IPD row(s) for ", ncol(x),
        " varying covariate(s): ", paste(colnames(x), collapse = ", "),
        ". The covariate design is rank deficient, so these coefficients are",
        " not separately identified from the IPD; consider dropping covariates",
        " or using informative priors."
      ),
      call. = FALSE
    )
    return(invisible(TRUE))
  }
  cor_mat <- suppressWarnings(stats::cor(data.matrix(x)))
  if (anyNA(cor_mat)) {
    return(invisible(TRUE))
  }
  ev <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values
  min_ev <- min(ev)
  condition_number <- if (min_ev > 0) max(ev) / min_ev else Inf
  if (condition_number > 1000) {
    warning(
      paste(
        "IPD covariates are highly collinear (condition number",
        format(round(condition_number), big.mark = ","),
        "of the covariate correlation matrix):",
        paste(colnames(x), collapse = ", "),
        ". Near-linearly-dependent covariates are only weakly identified and can",
        "cause slow sampling or divergences; consider dropping or combining",
        "redundant covariates."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Standardize IPD to mlumr's internal column contract
#' @noRd
.standardize_ipd_data <- function(data, treatment, outcome, covariates,
                                  family, exposure = NULL, study = NULL) {
  ipd_data <- data.frame(
    .study = if (!is.null(study)) data[[study]] else "IPD_Study",
    .trt = data[[treatment]],
    stringsAsFactors = FALSE
  )

  if (family == "normal") {
    ipd_data$.outcome <- as.numeric(data[[outcome]])
  } else {
    ipd_data$.outcome <- .as_count_integer(data[[outcome]])
  }
  if (family == "poisson") {
    ipd_data$.exposure <- as.numeric(data[[exposure]])
  }

  for (cov in covariates) {
    ipd_data[[cov]] <- data[[cov]]
  }
  ipd_data
}

#' Summarize standardized IPD outcomes
#' @noRd
.ipd_outcome_summary <- function(ipd_data, family) {
  switch(family,
    binomial = list(n_events = sum(ipd_data$.outcome)),
    normal = list(mean_outcome = mean(ipd_data$.outcome),
                  sd_outcome = sd(ipd_data$.outcome)),
    poisson = list(total_events = sum(ipd_data$.outcome),
                   total_exposure = sum(ipd_data$.exposure))
  )
}


#' Set up aggregate data (AgD)
#'
#' Prepare AgD from the comparator treatment for an unanchored indirect
#' comparison.
#'
#' @param data Data frame containing AgD summary statistics
#' @param treatment Column name for treatment variable
#' @param family Outcome family: `"binomial"`, `"normal"`, or `"poisson"`.
#'   Time-to-event comparator data go to [set_agd_surv()] instead, which takes
#'   reconstructed pseudo-IPD rather than a scalar outcome summary.
#' @param outcome_n Column name for sample size. Required for binomial. For
#'   normal, required when there is more than one aggregate row, because the
#'   comparator-population estimand is the size-weighted mixture of those rows
#'   and they cannot be combined without knowing how large each is; optional for
#'   a single row, where the weighting is irrelevant.
#' @param outcome_r Column name for number of events (required for binomial
#'   and poisson)
#' @param outcome_mean Column name for mean outcome (required for normal)
#' @param outcome_se Column name for standard error of outcome (required for
#'   normal)
#' @param outcome_E Column name for total exposure (required for poisson)
#' @param cov_means Character vector of column names for covariate means/proportions
#' @param cov_sds Character vector of column names for covariate SDs
#'   (`NA` for binary covariates)
#' @param cov_types Character vector specifying `"continuous"` or `"binary"` for
#'   each covariate. If `NULL`, inferred from presence of SD.
#' @param study Column name for study identifier (optional)
#'
#' @details
#' **Rows must partition the aggregate sample.** Each row contributes its own
#' likelihood factor, as if the rows were disjoint sets of patients. One arm,
#' or one set of mutually exclusive subgroup cells, is right; several
#' overlapping subgroup tables of the same participants count every patient
#' once per table and overstate the precision. Nothing in the data reveals
#' the overlap, so it is not checked. See
#' `vignette("subgroup-identification", "mlumr")` for how many rows the
#' relaxed model needs.
#'
#' **Scales.** For `family = "normal"`, `outcome_mean` and `outcome_se` are on
#' the arithmetic scale under both links; a geometric mean or a log-scale
#' summary is a different quantity and cannot be converted by the delta
#' method. For `family = "poisson"`, `outcome_r` is the total count and
#' `outcome_E` the total person-time, and the covariate distribution the rate
#' is averaged over has to describe the covariates weighted by exposure;
#' person-level moments stand in for that only when exposure carries no
#' information about the rate within the row. For `family = "binomial"`,
#' `outcome_r` and `outcome_n` are counts of events and trials.
#'
#' @return An object of class `mlumr_agd`. As for [set_ipd()], the internal
#'   column names cannot be used as column names in `data`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Binary outcome
#' agd <- set_agd(
#'   data = trial_b,
#'   treatment = "trt",
#'   outcome_n = "n_total",
#'   outcome_r = "n_events",
#'   cov_means = c("age_mean", "sex_prop"),
#'   cov_sds = c("age_sd", NA),
#'   cov_types = c("continuous", "binary")
#' )
#'
#' # Continuous outcome
#' agd <- set_agd(
#'   data = trial_b,
#'   treatment = "trt",
#'   family = "normal",
#'   outcome_mean = "mean_score",
#'   outcome_se = "se_score",
#'   outcome_n = "n_total",
#'   cov_means = c("age_mean", "sex_prop")
#' )
#'
#' # Count outcome
#' agd <- set_agd(
#'   data = trial_b,
#'   treatment = "trt",
#'   family = "poisson",
#'   outcome_r = "n_events",
#'   outcome_E = "person_years",
#'   cov_means = c("age_mean", "sex_prop")
#' )
#' }
set_agd <- function(data, treatment,
                    family = c("binomial", "normal", "poisson"),
                    outcome_n = NULL, outcome_r = NULL,
                    outcome_mean = NULL, outcome_se = NULL,
                    outcome_E = NULL,
                    cov_means, cov_sds = NULL, cov_types = NULL,
                    study = NULL) {

  family <- match.arg(family)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  .validate_non_empty_data(data, "AgD")
  .validate_required_covariates(cov_means, "cov_means")

  .validate_agd_outcome_args(family, outcome_n, outcome_r, outcome_mean,
                             outcome_se, outcome_E)
  spec <- .agd_covariate_spec(cov_means, cov_sds, cov_types)
  cov_sds <- spec$cov_sds
  cov_types <- spec$cov_types
  cov_names <- spec$cov_names

  required_cols <- .agd_required_columns(
    treatment = treatment,
    cov_means = cov_means,
    cov_sds = cov_sds,
    outcome_n = outcome_n,
    outcome_r = outcome_r,
    outcome_mean = outcome_mean,
    outcome_se = outcome_se,
    outcome_E = outcome_E,
    study = study
  )

  .check_required_columns(data, required_cols)
  .validate_single_treatment(data, treatment, "AgD")
  .validate_reserved_internal_names(
    c(cov_means, cov_sds[!is.na(cov_sds)], treatment, study,
      outcome_n, outcome_r, outcome_mean, outcome_se, outcome_E),
    c(".study", ".trt", ".n", ".r", ".y", ".se", ".E"),
    "Column name(s)"
  )
  .validate_agd_covariate_names(cov_means)
  .validate_agd_outcomes(data, family, outcome_n, outcome_r, outcome_mean,
                         outcome_se, outcome_E)
  .validate_agd_covariates(data, cov_means, cov_sds)
  .validate_agd_cov_types(cov_types)
  .validate_agd_binary_covariates(data, cov_means, cov_sds, cov_types)

  agd_data <- .standardize_agd_data(
    data = data,
    treatment = treatment,
    family = family,
    outcome_n = outcome_n,
    outcome_r = outcome_r,
    outcome_mean = outcome_mean,
    outcome_se = outcome_se,
    outcome_E = outcome_E,
    cov_means = cov_means,
    cov_sds = cov_sds,
    cov_names = cov_names,
    study = study
  )
  cov_info <- .agd_cov_info(cov_names, cov_sds, cov_types)

  out <- c(
    list(
      data = agd_data,
      treatment = unique(agd_data$.trt),
      covariates = cov_names,
      cov_info = cov_info,
      family = family,
      type = "agd"
    ),
    .agd_outcome_summary(data, family, outcome_n, outcome_r,
                         outcome_mean, outcome_se, outcome_E)
  )

  class(out) <- c("mlumr_agd", "list")
  out
}

#' Validate AgD outcome column arguments
#' @noRd
.validate_agd_outcome_args <- function(family, outcome_n, outcome_r,
                                       outcome_mean, outcome_se, outcome_E) {
  if (family == "binomial" && (is.null(outcome_n) || is.null(outcome_r))) {
    stop("`outcome_n` and `outcome_r` are required for binomial family",
         call. = FALSE)
  }
  if (family == "normal" && (is.null(outcome_mean) || is.null(outcome_se))) {
    stop("`outcome_mean` and `outcome_se` are required for normal family",
         call. = FALSE)
  }
  if (family == "poisson" && (is.null(outcome_r) || is.null(outcome_E))) {
    stop("`outcome_r` and `outcome_E` are required for poisson family",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Normalize AgD covariate specification
#' @noRd
.agd_covariate_spec <- function(cov_means, cov_sds = NULL, cov_types = NULL) {
  n_cov <- length(cov_means)

  if (is.null(cov_sds)) {
    cov_sds <- rep(NA_character_, n_cov)
  } else if (length(cov_sds) != n_cov) {
    stop("`cov_sds` must have same length as `cov_means` or be NULL",
         call. = FALSE)
  }

  if (is.null(cov_types)) {
    cov_types <- ifelse(is.na(cov_sds), "binary", "continuous")
  } else if (length(cov_types) != n_cov) {
    stop("`cov_types` must have same length as `cov_means` or be NULL",
         call. = FALSE)
  }

  list(
    cov_sds = cov_sds,
    cov_types = cov_types,
    cov_names = .strip_agd_cov_suffix(cov_means)
  )
}

#' AgD source columns required for setup
#' @noRd
.agd_required_columns <- function(treatment, cov_means, cov_sds,
                                  outcome_n = NULL, outcome_r = NULL,
                                  outcome_mean = NULL, outcome_se = NULL,
                                  outcome_E = NULL, study = NULL) {
  cols <- c(treatment, cov_means)
  for (col in c(outcome_n, outcome_r, outcome_mean, outcome_se, outcome_E)) {
    if (!is.null(col)) cols <- c(cols, col)
  }
  sd_cols <- cov_sds[!is.na(cov_sds)]
  if (length(sd_cols) > 0L) cols <- c(cols, sd_cols)
  if (!is.null(study)) cols <- c(cols, study)
  cols
}

#' Strip AgD covariate suffixes
#' @noRd
.strip_agd_cov_suffix <- function(cov_means) {
  sub("_prop$", "", sub("_mean$", "", cov_means))
}

#' Validate AgD covariate names after suffix stripping
#' @noRd
.validate_agd_covariate_names <- function(cov_means) {
  stripped <- .strip_agd_cov_suffix(cov_means)
  dup_stripped <- unique(stripped[duplicated(stripped)])
  if (length(dup_stripped) > 0L) {
    msg <- sprintf(
      paste0(
        "Covariate-name collision after stripping _mean/_prop suffix: %s. ",
        "Pass distinct `cov_means` entries."
      ),
      paste(dup_stripped, collapse = ", ")
    )
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate AgD outcome summaries
#' @noRd
.validate_agd_outcomes <- function(data, family, outcome_n, outcome_r,
                                   outcome_mean, outcome_se, outcome_E) {
  if (family == "binomial") {
    .validate_agd_binomial_outcomes(data[[outcome_r]], data[[outcome_n]])
  } else if (family == "normal") {
    .validate_agd_normal_outcomes(data[[outcome_mean]], data[[outcome_se]])
    if (nrow(data) > 1L && is.null(outcome_n)) {
      stop("`outcome_n` is required for multiple normal aggregate rows so ",
           "they can be combined using population weights.", call. = FALSE)
    }
    if (!is.null(outcome_n)) {
      .validate_agd_sample_size(data[[outcome_n]])
    }
  } else {
    .validate_agd_poisson_outcomes(data[[outcome_r]], data[[outcome_E]])
  }
  invisible(TRUE)
}

#' Validate binomial AgD outcomes
#' @noRd
.validate_agd_binomial_outcomes <- function(r_vals, n_vals) {
  if (any(is.na(r_vals)) || any(is.na(n_vals))) {
    stop("`outcome_r` and `outcome_n` must not contain NA values", call. = FALSE)
  }
  if (!is.numeric(r_vals) || !is.numeric(n_vals)) {
    stop("`outcome_r` and `outcome_n` must be numeric", call. = FALSE)
  }
  if (any(!is.finite(r_vals)) || any(!is.finite(n_vals))) {
    stop("`outcome_r` and `outcome_n` must be finite", call. = FALSE)
  }
  if (!.is_whole_number_count(r_vals)) {
    stop("`outcome_r` must be integer counts", call. = FALSE)
  }
  if (!.is_whole_number_count(n_vals)) {
    stop("`outcome_n` must be integer sample sizes", call. = FALSE)
  }
  if (any(r_vals < 0)) {
    stop("`outcome_r` must be non-negative", call. = FALSE)
  }
  if (any(n_vals <= 0)) {
    stop("`outcome_n` must be positive", call. = FALSE)
  }
  if (any(r_vals > n_vals)) {
    stop("`outcome_r` must not exceed `outcome_n`", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate optional AgD sample sizes
#' @noRd
.validate_agd_sample_size <- function(n_vals) {
  if (any(is.na(n_vals))) {
    stop("`outcome_n` must not contain NA values", call. = FALSE)
  }
  if (!is.numeric(n_vals)) {
    stop("`outcome_n` must be numeric", call. = FALSE)
  }
  if (any(!is.finite(n_vals))) {
    stop("`outcome_n` must be finite", call. = FALSE)
  }
  if (!.is_whole_number_count(n_vals)) {
    stop("`outcome_n` must be integer sample sizes", call. = FALSE)
  }
  if (any(n_vals <= 0)) {
    stop("`outcome_n` must be positive", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate normal AgD outcomes
#' @noRd
.validate_agd_normal_outcomes <- function(y_vals, se_vals) {
  if (any(is.na(y_vals)) || any(is.na(se_vals))) {
    stop("`outcome_mean` and `outcome_se` must not contain NA values",
         call. = FALSE)
  }
  if (!is.numeric(y_vals) || !is.numeric(se_vals)) {
    stop("`outcome_mean` and `outcome_se` must be numeric", call. = FALSE)
  }
  if (any(!is.finite(y_vals)) || any(!is.finite(se_vals))) {
    stop("`outcome_mean` and `outcome_se` must be finite", call. = FALSE)
  }
  # Matches the `<lower=1e-12>` bound the normal Stan models declare.
  if (any(se_vals < .mlumr_min_positive)) {
    stop("`outcome_se` must be positive and at least 1e-12", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate Poisson AgD outcomes
#' @noRd
.validate_agd_poisson_outcomes <- function(r_vals, E_vals) {
  if (any(is.na(r_vals)) || any(is.na(E_vals))) {
    stop("`outcome_r` and `outcome_E` must not contain NA values", call. = FALSE)
  }
  if (!is.numeric(r_vals) || !is.numeric(E_vals)) {
    stop("`outcome_r` and `outcome_E` must be numeric", call. = FALSE)
  }
  if (any(!is.finite(r_vals)) || any(!is.finite(E_vals))) {
    stop("`outcome_r` and `outcome_E` must be finite", call. = FALSE)
  }
  if (!.is_whole_number_count(r_vals)) {
    stop("`outcome_r` must be integer counts", call. = FALSE)
  }
  if (any(r_vals < 0)) {
    stop("`outcome_r` must be non-negative", call. = FALSE)
  }
  # Matches the `<lower=1e-12>` bound the poisson Stan models declare.
  if (any(E_vals < .mlumr_min_positive)) {
    stop("`outcome_E` must be positive and at least 1e-12", call. = FALSE)
  }
  invisible(TRUE)
}

#' Check whether values are valid non-negative whole-number counts
#' @noRd
.is_whole_number_count <- function(x, allow_missing = FALSE) {
  if (!is.numeric(x)) {
    return(FALSE)
  }
  vals <- x
  if (allow_missing) {
    vals <- vals[!is.na(vals)]
  } else if (any(is.na(vals))) {
    return(FALSE)
  }
  if (length(vals) == 0L) {
    return(TRUE)
  }
  all(is.finite(vals) &
        vals >= 0 &
        vals <= .Machine$integer.max &
        abs(vals - round(vals)) <= sqrt(.Machine$double.eps))
}

#' Validate AgD covariate summaries
#' @noRd
.validate_agd_covariates <- function(data, cov_means, cov_sds) {
  for (cm in cov_means) {
    if (!is.numeric(data[[cm]])) {
      stop(sprintf("Covariate mean column '%s' must be numeric", cm),
           call. = FALSE)
    }
    if (any(!is.finite(data[[cm]]))) {
      stop(sprintf("Covariate mean column '%s' contains non-finite values", cm),
           call. = FALSE)
    }
  }

  for (cs in cov_sds[!is.na(cov_sds)]) {
    if (!is.numeric(data[[cs]])) {
      stop(sprintf("Covariate SD column '%s' must be numeric", cs),
           call. = FALSE)
    }
    if (any(!is.finite(data[[cs]]))) {
      stop(sprintf("Covariate SD column '%s' contains non-finite values", cs),
           call. = FALSE)
    }
    if (any(data[[cs]] < 0)) {
      stop(sprintf("Covariate SD column '%s' must be non-negative", cs),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate AgD covariate type labels
#' @noRd
.validate_agd_cov_types <- function(cov_types) {
  valid_cov_types <- c("binary", "continuous")
  bad_types <- setdiff(unique(cov_types), valid_cov_types)
  if (length(bad_types) > 0L) {
    stop(sprintf("Invalid cov_types: %s. Must be 'binary' or 'continuous'.",
                 paste(sQuote(bad_types), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Half a unit in the coarsest decimal place a value lands on exactly
#'
#' A published 0.53 is compared against an exact bound with an allowance of
#' 0.005, a published 0.5 with 0.05. The scan runs to the precision a double
#' can distinguish, so a value quoted to nine places gets an allowance too.
#' The allowance is lenient on purpose: it never rejects a valid summary for
#' having been rounded.
#'
#' @param x Numeric vector as reported.
#' @return Numeric vector of allowances, one per element.
#' @noRd
.rounding_allowance <- function(x) {
  # A double carries roughly 15 to 17 significant decimal digits; past that,
  # rounding is not a property of the number as written.
  max_digits <- 15L
  vapply(x, function(v) {
    if (!is.finite(v)) return(0)
    for (d in 0:max_digits) {
      if (isTRUE(all.equal(round(v, d), v, tolerance = 0))) {
        return(0.5 * 10^(-d))
      }
    }
    0
  }, numeric(1))
}

#' Validate binary AgD covariate summaries
#' @noRd
.validate_agd_binary_covariates <- function(data, cov_means, cov_sds,
                                            cov_types) {
  for (i in seq_along(cov_types)) {
    if (cov_types[[i]] != "binary") next

    mean_vals <- data[[cov_means[[i]]]]
    cov_label <- .strip_agd_cov_suffix(cov_means[[i]])

    if (any(mean_vals < 0 | mean_vals > 1)) {
      msg <- sprintf(
        "Binary covariate '%s': mean/proportion must be in [0, 1], got %s",
        cov_label,
        paste(mean_vals[mean_vals < 0 | mean_vals > 1], collapse = ", ")
      )
      stop(msg, call. = FALSE)
    }

    if (!is.na(cov_sds[[i]])) {
      sd_vals <- data[[cov_sds[[i]]]]
      # The largest sample SD of a binary column is sqrt(2 * p * (1 - p)), at
      # n = 2; the population bound sqrt(p * (1 - p)) rejects valid rows.
      # The proportion is rounded too, so take p(1-p) at the point of its
      # reported interval closest to 0.5.
      p_tol <- .rounding_allowance(mean_vals)
      p_lo <- pmax(0, mean_vals - p_tol)
      p_hi <- pmin(1, mean_vals + p_tol)
      p_worst <- pmin(pmax(0.5, p_lo), p_hi)
      max_sd <- sqrt(2 * p_worst * (1 - p_worst))
      tol <- 1e-10 + .rounding_allowance(sd_vals)
      impossible <- sd_vals > max_sd + tol
      if (any(impossible)) {
        msg <- sprintf(
          paste0(
            "Binary covariate '%s': SD exceeds the largest a binary sample ",
            "can have, sqrt(2 * p*(1-p)). ",
            "Got SD=%s for p=%s (max SD=%s)"
          ),
          cov_label,
          paste(round(sd_vals[impossible], 4), collapse = ", "),
          paste(round(mean_vals[impossible], 4), collapse = ", "),
          paste(round(max_sd[impossible], 4), collapse = ", ")
        )
        stop(msg, call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

#' Standardize AgD to mlumr's internal column contract
#' @noRd
.standardize_agd_data <- function(data, treatment, family, outcome_n,
                                  outcome_r, outcome_mean, outcome_se,
                                  outcome_E, cov_means, cov_sds,
                                  cov_names, study = NULL) {
  agd_data <- data.frame(
    .study = if (!is.null(study)) data[[study]] else "AgD_Study",
    .trt = data[[treatment]],
    stringsAsFactors = FALSE
  )

  if (family == "binomial") {
    agd_data$.n <- data[[outcome_n]]
    agd_data$.r <- data[[outcome_r]]
  } else if (family == "normal") {
    agd_data$.y <- data[[outcome_mean]]
    agd_data$.se <- data[[outcome_se]]
    if (!is.null(outcome_n)) agd_data$.n <- data[[outcome_n]]
  } else {
    agd_data$.r <- data[[outcome_r]]
    agd_data$.E <- data[[outcome_E]]
  }

  for (i in seq_along(cov_means)) {
    cov_name <- cov_names[[i]]
    agd_data[[paste0(cov_name, "_mean")]] <- data[[cov_means[[i]]]]
    if (!is.na(cov_sds[[i]])) {
      agd_data[[paste0(cov_name, "_sd")]] <- data[[cov_sds[[i]]]]
    }
  }
  agd_data
}

#' Build AgD covariate metadata
#' @noRd
.agd_cov_info <- function(cov_names, cov_sds, cov_types) {
  cov_info <- list()
  for (i in seq_along(cov_names)) {
    cov_name <- cov_names[[i]]
    cov_info[[cov_name]] <- list(
      mean_col = paste0(cov_name, "_mean"),
      sd_col = if (!is.na(cov_sds[[i]])) paste0(cov_name, "_sd") else NA,
      type = cov_types[[i]]
    )
  }
  cov_info
}

#' Summarize AgD outcomes
#' @noRd
.agd_outcome_summary <- function(data, family, outcome_n, outcome_r,
                                 outcome_mean, outcome_se, outcome_E) {
  switch(family,
    binomial = list(n = data[[outcome_n]], n_events = data[[outcome_r]]),
    normal = list(y = data[[outcome_mean]], se = data[[outcome_se]],
                  n = if (!is.null(outcome_n)) data[[outcome_n]] else NULL),
    poisson = list(n_events = data[[outcome_r]],
                   total_exposure = data[[outcome_E]])
  )
}


#' Combine IPD and AgD for unanchored comparison
#'
#' @param ipd An `mlumr_ipd` object from [set_ipd()]
#' @param agd An `mlumr_agd` object from [set_agd()]
#'
#' @return An object of class `mlumr_data`
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- combine_data(ipd, agd)
#' }
combine_data <- function(ipd, agd) {

  if (!inherits(ipd, "mlumr_ipd")) {
    stop("`ipd` must be created with set_ipd()", call. = FALSE)
  }
  if (!inherits(agd, "mlumr_agd")) {
    stop("`agd` must be created with set_agd()", call. = FALSE)
  }

  if (ipd$family != agd$family) {
    stop(sprintf("Family mismatch: IPD uses '%s' but AgD uses '%s'",
                 ipd$family, agd$family), call. = FALSE)
  }

  if (!identical(sort(ipd$covariates), sort(agd$covariates))) {
    stop("Covariates must match between IPD and AgD", call. = FALSE)
  }

  shared_trt <- intersect(ipd$treatment, agd$treatment)
  if (length(shared_trt) > 0) {
    # A shared label would make the two intercepts describe one treatment and
    # report a baseline difference as a treatment effect.
    msg <- paste0(
      "IPD and AgD share treatment label(s): %s. An unanchored comparison ",
      "requires two distinct treatments (one per source); a shared label would ",
      "estimate a treatment effect from baseline differences between the ",
      "studies. Relabel the arms, or use an anchored method for shared-comparator ",
      "evidence."
    )
    stop(sprintf(msg, paste(shared_trt, collapse = ", ")), call. = FALSE)
  }

  # A shared study label is likely a data-entry error.
  shared_studies <- intersect(unique(ipd$data$.study), unique(agd$data$.study))
  if (length(shared_studies) > 0) {
    msg <- paste0(
      "IPD and AgD share study label(s): %s. An unanchored comparison normally ",
      "draws IPD and AgD from different studies; check the `study` columns."
    )
    warning(sprintf(msg, paste(shared_studies, collapse = ", ")), call. = FALSE)
  }

  out <- list(
    ipd = ipd,
    agd = agd,
    family = ipd$family,
    covariates = ipd$covariates,
    n_covariates = length(ipd$covariates),
    treatments = c(ipd$treatment, agd$treatment),
    index_treatment = ipd$treatment,
    comparator_treatment = agd$treatment,
    has_integration = FALSE
  )

  class(out) <- c("mlumr_data", "list")
  out
}


#' @method print mlumr_data
#' @export
print.mlumr_data <- function(x, ...) {
  family <- x$family %||% "binomial"
  family_label <- switch(family,
    binomial = "Binary",
    normal   = "Continuous",
    poisson  = "Count",
    survival = "Time-to-event"
  )

  cat(sprintf("Unanchored Comparison Data (%s)\n", family_label))
  cat("====================================\n\n")
  cat("Index treatment (IPD):", x$index_treatment, "\n")
  cat("  N =", x$ipd$n, "\n")

  if (family == "binomial") {
    cat("  Events =", x$ipd$n_events,
        sprintf("(%.1f%%)", 100 * x$ipd$n_events / x$ipd$n), "\n\n")
  } else if (family == "normal") {
    cat(sprintf("  Mean outcome = %.3f (SD = %.3f)\n\n",
                x$ipd$mean_outcome, x$ipd$sd_outcome))
  } else if (family == "survival") {
    cat(sprintf("  Events = %d (%.1f%%), censored = %d\n\n",
                x$ipd$n_events, 100 * x$ipd$n_events / x$ipd$n,
                x$ipd$n - x$ipd$n_events))
  } else {
    cat(sprintf("  Total events = %d, Total exposure = %.1f\n\n",
                x$ipd$total_events, x$ipd$total_exposure))
  }

  cat("Comparator treatment (AgD):", x$comparator_treatment, "\n")
  if (family == "binomial") {
    cat("  N =", x$agd$n, "\n")
    cat("  Events =", x$agd$n_events,
        sprintf("(%.1f%%)", 100 * x$agd$n_events / x$agd$n), "\n\n")
  } else if (family == "normal") {
    if (!is.null(x$agd$n)) cat("  N =", x$agd$n, "\n")
    cat(sprintf("  Mean outcome = %s, SE = %s\n\n",
                paste(round(x$agd$y, 3), collapse = ", "),
                paste(round(x$agd$se, 3), collapse = ", ")))
  } else if (family == "survival") {
    cat(sprintf("  Reconstructed pseudo-IPD: %d row(s)\n", x$agd$n_pseudo))
    cat(sprintf("  Events = %d (%.1f%%), censored = %d\n\n",
                x$agd$n_events, 100 * x$agd$n_events / x$agd$n_pseudo,
                x$agd$n_pseudo - x$agd$n_events))
  } else {
    cat(sprintf("  Total events = %d, Total exposure = %.1f\n\n",
                sum(x$agd$n_events), sum(x$agd$total_exposure)))
  }

  cat("Covariates (", x$n_covariates, "):",
      paste(x$covariates, collapse = ", "), "\n")
  if (x$has_integration) {
    cat("Integration points:", x$n_int, "(QMC with Gaussian copula)\n")
  } else {
    cat("Integration points: not yet added (use add_integration())\n")
  }
  invisible(x)
}

# Example datasets bundled with mlumr.
#
# The psoriasis and ndmm datasets are subsets of datasets distributed by the
# multinma package (GPL-3), trimmed to exactly the arms and columns used in
# mlumr's binary and survival vignettes and re-exposed so the examples are
# self-contained (see data-raw/prepare_multinma_subsets.R). The shoulder and
# caries datasets (further below) are synthetic. Provenance and original trial
# sources are in each @source. Note: multinma's psoriasis/ndmm individual patient
# data are themselves simulated / reconstructed (not real patient records).

#' Plaque psoriasis: index individual patient data (binary)
#'
#' Individual patient data for the index arm (UNCOVER-2, ixekizumab Q4W), the
#' binary (PASI 75 response) ML-UMR worked example; pair with
#' \code{\link{psoriasis_agd}}.
#'
#' @format A data frame with 347 rows and 8 columns:
#' \describe{
#'   \item{study, treatment}{study and treatment labels}
#'   \item{subject}{row identifier}
#'   \item{pasi75}{binary PASI 75 response indicator}
#'   \item{age, bsa, weight, prevsys}{patient covariates (age in years,
#'     body-surface area in percent, weight in kg, and previous systemic
#'     treatment as a 1/0 indicator)}
#' }
#'
#'   \code{weight} is missing for 2 of the 347 rows, as it is in the multinma
#'   source. \code{\link{set_ipd}} drops incomplete rows with a warning, so a
#'   fit adjusting for weight uses 345 of them.
#' @details The two source trials are not genuinely disconnected: UNCOVER-2 and
#'   FIXTURE both include placebo and etanercept arms. Those arms are omitted
#'   here deliberately, so that this pair of datasets poses the unanchored
#'   problem ML-UMR exists for. \code{vignette("binary-outcomes")} puts them
#'   back at the end to check the unanchored estimate against the anchored one.
#'
#' @source The UNCOVER-2 ixekizumab-Q4W arm of
#'   \code{multinma::plaque_psoriasis_ipd} (GPL-3; Phillippo, \emph{multinma},
#'   2024, \doi{10.5281/zenodo.3904454}), subset to the columns used in the binary
#'   vignette. multinma provides \emph{simulated} IPD resembling the UNCOVER-2 /
#'   UNCOVER-3 (ixekizumab; Griffiths et al. 2015,
#'   \doi{10.1016/s0140-6736(15)60125-8}) and secukinumab (Langley et al. 2014,
#'   \doi{10.1056/nejmoa1314258}) plaque-psoriasis trials. See
#'   \code{data-raw/prepare_multinma_subsets.R}.
"psoriasis_ipd"

#' Plaque psoriasis: comparator aggregate data (binary)
#'
#' Aggregate (arm-level) summary for the comparator arm (FIXTURE, secukinumab
#' 300 mg), the AgD comparator in the binary ML-UMR example; pair with
#' \code{\link{psoriasis_ipd}}.
#'
#' @format A data frame with 1 row and 11 columns: \code{study},
#'   \code{treatment}; response counts \code{pasi75_r} / \code{pasi75_n}; and
#'   covariate summaries \code{age_mean} / \code{age_sd}, \code{bsa_mean} /
#'   \code{bsa_sd}, \code{weight_mean} / \code{weight_sd}, and
#'   \code{prevsys_prop} (a proportion).
#' @source The FIXTURE secukinumab-300 arm of
#'   \code{multinma::plaque_psoriasis_agd} (GPL-3; Phillippo, \emph{multinma},
#'   2024), subset to the columns used in the binary vignette. See
#'   \code{\link{psoriasis_ipd}} for the original trial sources.
"psoriasis_agd"

#' Newly diagnosed multiple myeloma: index individual patient data (survival)
#'
#' Individual patient data for the index arm (McCarthy 2012, lenalidomide), the
#' survival (progression-free survival) ML-UMR worked example; pair with
#' \code{\link{ndmm_agd}} and \code{\link{ndmm_agd_covs}}.
#'
#' @format A data frame with 231 rows and 9 columns:
#' \describe{
#'   \item{study, treatment}{study and treatment labels}
#'   \item{subject}{row identifier}
#'   \item{age, iss_stage3, response_cr_vgpr, male}{patient covariates}
#'   \item{eventtime, status}{progression-free survival time and event
#'     indicator (1 = event, 0 = censored)}
#' }
#' @details The two source trials are not genuinely disconnected: McCarthy 2012
#'   and Morgan 2012 both include a placebo arm. Those arms are omitted here
#'   deliberately, so that this pair of datasets poses the unanchored problem
#'   ML-UMR exists for. \code{vignette("survival-outcomes")} puts them back at
#'   the end to check the unanchored estimate against the anchored one.
#'
#'   McCarthy 2012 is the index arm because population overlap, not sample size,
#'   governs how far an unanchored comparison has to extrapolate. Unanchored MAIC
#'   effective sample size against the Morgan 2012 thalidomide arm is 35.8 of 231
#'   (15.5\%) for McCarthy 2012, against 3.0 of 126 (2.4\%) for the Palumbo 2014
#'   lenalidomide arm, the other candidate for this comparison. See
#'   \code{data-raw/prepare_multinma_subsets.R} for the full pair ranking.
#'
#' @source The McCarthy-2012 lenalidomide arm of \code{multinma::ndmm_ipd} (GPL-3;
#'   Phillippo, \emph{multinma}, 2024), subset to the columns used in the survival
#'   vignette. multinma provides \emph{simulated} IPD resembling published
#'   newly-diagnosed-multiple-myeloma trials; the vignette compares lenalidomide
#'   (McCarthy et al. 2012) with thalidomide (Morgan et al. 2012). See
#'   \code{data-raw/prepare_multinma_subsets.R}.
"ndmm_ipd"

#' Newly diagnosed multiple myeloma: comparator pseudo-IPD (survival)
#'
#' Reconstructed Kaplan-Meier pseudo-IPD for the comparator arm (Morgan 2012,
#' thalidomide), the survival AgD comparator; pair with \code{\link{ndmm_ipd}}
#' and \code{\link{ndmm_agd_covs}}.
#'
#' @format A data frame with 408 rows and 5 columns: \code{study},
#'   \code{treatment}, \code{subject}, \code{eventtime}, \code{status}.
#' @source The Morgan-2012 thalidomide arm of \code{multinma::ndmm_agd} (GPL-3;
#'   Phillippo, \emph{multinma}, 2024); event/censoring times reconstructed from
#'   published Kaplan-Meier curves. See \code{\link{ndmm_ipd}} for trial sources.
"ndmm_agd"

#' Newly diagnosed multiple myeloma: comparator covariate summaries (survival)
#'
#' Published covariate summaries for the comparator arm (Morgan 2012,
#' thalidomide), supplying the covariate moments for the AgD integration points.
#'
#' @format A data frame with 1 row and 7 columns: \code{study},
#'   \code{treatment}, \code{age_mean} / \code{age_sd}, and the proportions
#'   \code{iss_stage3_prop}, \code{response_cr_vgpr_prop}, \code{male_prop}.
#' @source The Morgan-2012 thalidomide row of \code{multinma::ndmm_agd_covs}
#'   (GPL-3; Phillippo, \emph{multinma}, 2024), subset to the columns used in the
#'   survival vignette. See \code{\link{ndmm_ipd}} for trial sources.
"ndmm_agd_covs"


# ---------------------------------------------------------------------------
# Simulated example datasets (mlumr's own work, package GPL-3). These are not
# real patient records: each is generated by a model fitted to a real, openly
# licensed (CC BY 4.0) trial, preserving the covariate distributions and the
# covariate-outcome / treatment relationships (the same approach multinma uses
# for its data). The source trials are cited as the modeling basis only; the
# generation is in data-raw/simulate_external_data.R (seed 2026).
# ---------------------------------------------------------------------------

#' Shoulder pain: simulated index IPD (continuous)
#'
#' Simulated individual patient data for the index treatment (arthroscopic
#' subacromial decompression, ASD) in a continuous-outcome ML-UMR example. Pair
#' with \code{\link{shoulder_agd}} (the comparator). Not real patient data.
#'
#' @format A data frame with 147 rows and 7 columns:
#' \describe{
#'   \item{study}{study label (\code{"FIMPACT"})}
#'   \item{treatment}{index treatment label (\code{"ASD"})}
#'   \item{subject}{row identifier}
#'   \item{age}{age in years}
#'   \item{sex}{1 = male, 0 = female}
#'   \item{baseline_vas}{baseline shoulder pain on activity (VAS 0-100)}
#'   \item{pain_vas_activity}{shoulder pain on activity at 24 months (VAS 0-100)}
#' }
#'
#' @details The index and comparator arms come from the same trial, so both
#'   carry the study label \code{"FIMPACT"}. Splitting one randomized trial into a
#'   single-arm IPD source and a single-arm aggregate source is what makes this
#'   an unanchored example that still has a full-data comparison to be checked
#'   against: fitting the two arms together on the complete synthetic records
#'   gives the quantity the unanchored methods are trying to recover once one
#'   arm has been reduced to summaries. That is a data-reduction benchmark. It
#'   is not a randomized reference and not a known population causal effect.
#'   The generator draws treatment first and then synthesizes the covariates
#'   conditional on it, so the synthetic arms are not a fresh randomized
#'   assignment independent of the generated baseline variables; and the
#'   full-data estimate carries its own sampling error. Validating an estimator
#'   against a causal target would need a specified covariate distribution,
#'   potential-outcome mechanism, assignment rule and estimand, which these
#'   records do not provide.
#'   Because the label
#'   is shared, \code{\link{combine_data}} warns that IPD and AgD come from
#'   the same study; that warning is expected here and is the honest reading of
#'   the data. It does not fire for \code{\link{psoriasis_ipd}} or
#'   \code{\link{ndmm_ipd}}, whose arms really do come from different trials.
#' @source Simulated (not real patient data), generated with the \pkg{synthpop}
#'   package (sequential CART; Nowok, Raab and Dibben 2016,
#'   \doi{10.18637/jss.v074.i11}) from the FIMPACT 10-year trial
#'   (BMJ 2025;391:e086201; dataset CC BY 4.0, University of Helsinki / Finnish
#'   Ministry of Education open-data portal,
#'   \doi{10.23729/fd-d323a34b-f698-3bc6-b38d-9c93aeadbe74}), preserving its
#'   covariate and covariate-outcome relationships; fidelity validated with the
#'   \pkg{syntheticdata} package. See \code{data-raw/simulate_external_data.R}.
"shoulder_ipd"

#' Shoulder pain: simulated comparator aggregate data (continuous)
#'
#' Aggregate summary of the comparator treatment (exercise therapy, ET) for the
#' continuous ML-UMR example; pair with \code{\link{shoulder_ipd}}. The two arms
#' are treated as separate single-arm sources to illustrate an unanchored
#' comparison. Not real patient data.
#'
#' @format A data frame with 1 row and 10 columns: \code{study},
#'   \code{treatment}, \code{n}, outcome summary (\code{y_mean}, \code{y_se}) and
#'   covariate summaries (\code{age_mean}/\code{age_sd}, \code{sex_prop} (a
#'   proportion), \code{baseline_vas_mean}/\code{baseline_vas_sd}).
#' @source Simulated; see \code{\link{shoulder_ipd}} for the modeling basis.
"shoulder_agd"

#' Dental caries: simulated index IPD (count)
#'
#' Simulated individual patient data for the index treatment (silver diamine
#' fluoride, SDF) in a count-outcome ML-UMR example; pair with
#' \code{\link{caries_agd}}. Not real patient data.
#'
#' @format A data frame with 103 rows and 8 columns:
#' \describe{
#'   \item{study}{study label (\code{"Ammar 2025"})}
#'   \item{treatment}{index treatment label (\code{"SDF"})}
#'   \item{subject}{row identifier}
#'   \item{age}{age in years}
#'   \item{gender}{1/0 indicator, as coded in the source trial, which does not
#'     record which level is which. Only the covariate's distribution matters
#'     for the adjustment, so the labeling does not affect the estimand.}
#'   \item{log_cfu}{log baseline salivary \emph{S. mutans} count,
#'     \code{log(CFU/mL + 1)}}
#'   \item{dmft}{decayed-missing-filled-teeth count (the count outcome)}
#'   \item{exposure}{Poisson offset, 1 per child. \code{dmft} is a whole-mouth
#'     count with no time at risk and no per-tooth denominator, so the rate is
#'     "dmft per child" and the offset is 1. The column is present because
#'     \code{\link{set_ipd}} requires \code{exposure} for the Poisson family;
#'     \code{log(1) = 0}, so it contributes nothing to the linear predictor.
#'     The comparator's \code{E} is the matching total, one unit per child.}
#' }
#'
#'   \code{log_cfu} is bimodal: 9 of the 103 children (8.7\%) have no
#'   detectable baseline count and so sit at exactly 0, and the rest are
#'   concentrated near 10.9 (SD 0.8). The comparator arm declares this covariate
#'   as a single normal distribution (\code{log_cfu_mean} 11.0,
#'   \code{log_cfu_sd} 0.7) because the source trial's comparator arm contains
#'   no such zeros. Integrating a normal over the comparator therefore puts
#'   almost no weight where the index arm's zero mode sits, so effects adjusted
#'   for \code{log_cfu} extrapolate over that part of the covariate space. This
#'   is a property of the source trial, not of the synthesis; treat it as a
#'   worked illustration of a covariate whose arms are not fully overlapping.
#'
#' @details The index and comparator arms come from the same trial, so both
#'   carry the study label \code{"Ammar 2025"}. Splitting one randomized trial into a
#'   single-arm IPD source and a single-arm aggregate source is what makes this
#'   an unanchored example that still has a full-data comparison to be checked
#'   against: fitting the two arms together on the complete synthetic records
#'   gives the quantity the unanchored methods are trying to recover once one
#'   arm has been reduced to summaries. That is a data-reduction benchmark. It
#'   is not a randomized reference and not a known population causal effect.
#'   The generator draws treatment first and then synthesizes the covariates
#'   conditional on it, so the synthetic arms are not a fresh randomized
#'   assignment independent of the generated baseline variables; and the
#'   full-data estimate carries its own sampling error. Validating an estimator
#'   against a causal target would need a specified covariate distribution,
#'   potential-outcome mechanism, assignment rule and estimand, which these
#'   records do not provide. Here the treatment contrast itself was imposed:
#'   \code{dmft} was a balanced baseline characteristic in the source trial and
#'   the index arm's synthetic counts were thinned binomially to create an
#'   effect, so the benchmark recovers a contrast this package manufactured,
#'   not one the trial reported.
#'   Because the label
#'   is shared, \code{\link{combine_data}} warns that IPD and AgD come from
#'   the same study; that warning is expected here and is the honest reading of
#'   the data. It does not fire for \code{\link{psoriasis_ipd}} or
#'   \code{\link{ndmm_ipd}}, whose arms really do come from different trials.
#' @source Simulated (not real patient data), generated with the \pkg{synthpop}
#'   package (sequential CART; Nowok, Raab and Dibben 2016,
#'   \doi{10.18637/jss.v074.i11}) from the silver-diamine-fluoride vs
#'   nano-silver-fluoride dental caries RCT (Ammar et al., \emph{BMC Oral Health}
#'   2025;25:945, CC BY 4.0; data Synapse syn43185346), preserving the covariate
#'   relationships; fidelity validated with the \pkg{syntheticdata} package. A
#'   plausible caries-arresting SDF effect was imposed for illustration (dmft was
#'   a balanced baseline characteristic in the source trial). See
#'   \code{data-raw/simulate_external_data.R}.
"caries_ipd"

#' Dental caries: simulated comparator aggregate data (count)
#'
#' Aggregate summary of the comparator treatment (nano-silver fluoride, NSF) for
#' the count ML-UMR example; pair with \code{\link{caries_ipd}}. Not real patient
#' data.
#'
#' @format A data frame with 1 row and 10 columns: \code{study},
#'   \code{treatment}, \code{n}, count outcome \code{r} (total dmft) with exposure
#'   \code{E} (n children), and covariate summaries (\code{age_mean}/\code{age_sd},
#'   \code{gender_prop} (a proportion), \code{log_cfu_mean}/\code{log_cfu_sd}).
#' @source Simulated; see \code{\link{caries_ipd}} for the modeling basis.
"caries_agd"

#' Refuse a pointwise log-likelihood collapsed over tied aggregate rows
#'
#' Tie aggregation (not shipped yet) would keep one `log_lik_agd` column per
#' distinct row with the multiplicity in `stan_data$agd_count`. LOO, WAIC and
#' DIC need one column per observation, so that shape is refused here.
#' @noRd
.assert_agd_loglik_per_observation <- function(object) {
  cnt <- object$stan_data$agd_count
  if (is.null(cnt) || !length(cnt)) return(invisible(TRUE))
  # Validate the multiplicities before summing them.
  if (!is.numeric(cnt) || anyNA(cnt) || !all(is.finite(cnt)) || any(cnt < 1) ||
        any(cnt != trunc(cnt))) {
    stop("`stan_data$agd_count` must hold finite whole-number multiplicities of ",
         "at least one, since each is a count of observations that one retained ",
         "AgD row stands for.", call. = FALSE)
  }
  if (all(cnt == 1)) return(invisible(TRUE))
  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) return(invisible(TRUE))
  n_cols <- length(.ordered_log_lik_columns(draws, "agd"))
  # Expanded means exactly one column per observation.
  if (n_cols == sum(cnt)) return(invisible(TRUE))
  if (n_cols != length(cnt)) {
    stop("`log_lik_agd` has ", n_cols, " column(s), which is neither one per ",
         "retained AgD row (", length(cnt), ") nor one per observation (",
         sum(cnt), "). LOO, WAIC, and DIC cannot align the pointwise ",
         "likelihood with the arm map in that state.", call. = FALSE)
  }
  stop("This fit collapsed tied aggregate rows (`stan_data$agd_count` has ",
       "multiplicities above one), so `log_lik_agd` holds one value per ",
       "UNIQUE row rather than one per observation. LOO, WAIC, and DIC would ",
       "count each tied observation once instead of ", max(cnt), " times, and ",
       "arm grouping would use the collapsed arm map. Expand the pointwise ",
       "log-likelihood and the arm map back to the original observations ",
       "before requesting diagnostics.", call. = FALSE)
}


#' Refuse a pointwise log-likelihood with terms not saved
#'
#' A fit saved with `pars = "log_lik_agd", include = FALSE` (or the IPD
#' counterpart) still used that evidence in the posterior, so scoring the
#' columns that remain would report a subset score as the full-data one. The
#' expected counts come from `stan_data`; a side without one is not checked.
#' @noRd
.assert_log_lik_complete <- function(object, ipd_cols, agd_cols) {
  sd <- object$stan_data
  expected_agd <- if (!is.null(sd$agd_count)) {
    sum(sd$agd_count)
  } else if (identical(object$family, "survival")) {
    sd$n_agd
  } else {
    sd$n_agd_rows
  }
  sides <- list(list("log_lik_ipd", sd$n_ipd, length(ipd_cols)),
                list("log_lik_agd", expected_agd, length(agd_cols)))
  for (s in sides) {
    if (is.null(s[[2]]) || s[[2]] == s[[3]]) next
    stop("`", s[[1]], "` has ", s[[3]], " column(s) but the fit has ", s[[2]],
         " such observation(s), so the columns saved are not the full ",
         "likelihood. Refit with the generated quantities `log_lik_ipd` and ",
         "`log_lik_agd` saved before requesting LOO, WAIC or DIC.",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' The per-observation likelihood inputs, in the order the scores are stored
#'
#' Two fits can be paired pointwise only when these agree. Covariates and
#' priors are left out on purpose: compared models legitimately differ in them.
#' @noRd
.scoring_identity <- function(object) {
  keep <- c("y_ipd", "E_ipd", "r_agd", "n_agd", "y_agd", "se_agd", "E_agd",
            "ipd_time", "ipd_start_time", "ipd_delay_time", "ipd_status",
            "agd_time", "agd_start_time", "agd_delay_time", "agd_status",
            "agd_arm")
  sd <- object$stan_data
  sd[intersect(keep, names(sd))]
}


#' Extract the full pointwise log-likelihood matrix from an mlumr_fit
#'
#' Combines the IPD and AgD per-observation log-likelihood draws into a
#' single matrix with one row per posterior draw and one column per
#' observation. The result is suitable for direct use with
#' [loo::loo()] / [loo::waic()].
#'
#' @param object An `mlumr_fit` object.
#' @return A numeric matrix of dimension `n_draws x (n_ipd + n_agd)`. IPD
#'   columns come first, then the AgD columns. The AgD pointwise unit is whatever
#'   the family's Stan model emits as `log_lik_agd`: for binomial / normal /
#'   poisson this is **one column per aggregate row**; for **survival** it is
#'   **one column per reconstructed pseudo-individual** (not per aggregate row),
#'   so survival LOO/WAIC operate at the pseudo-individual level. See the notes
#'   on [calculate_loo()] / [calculate_waic()].
#' @noRd
extract_log_lik <- function(object) {
  .validate_mlumr_fit_object(object)
  .assert_agd_loglik_per_observation(object)

  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) {
    stop("`object$draws` must contain named posterior draw columns.",
         call. = FALSE)
  }

  ipd_cols <- .ordered_log_lik_columns(draws, "ipd")
  agd_cols <- .ordered_log_lik_columns(draws, "agd")
  if (length(ipd_cols) == 0L && length(agd_cols) == 0L) {
    stop(
      "Pointwise log-likelihood columns not found in draws. ",
      "This package requires Stan models that generate `log_lik_ipd` and ",
      "`log_lik_agd` as vectors (per-observation). If you have a fit from ",
      "an older version of mlumr; refit with the current version.",
      call. = FALSE
    )
  }
  .assert_log_lik_complete(object, ipd_cols, agd_cols)
  selected <- draws[, c(ipd_cols, agd_cols), drop = FALSE]
  numeric_cols <- vapply(selected, is.numeric, logical(1))
  if (!all(numeric_cols)) {
    stop("Pointwise log-likelihood columns must be numeric.", call. = FALSE)
  }

  log_lik <- as.matrix(selected)
  .validate_log_lik_matrix(log_lik)
  log_lik
}


#' Ordered pointwise log-likelihood columns for one data source
#' @noRd
.ordered_log_lik_columns <- function(draws, source) {
  pattern <- sprintf("^log_lik_%s\\[[0-9]+\\]$", source)
  cols <- grep(pattern, colnames(draws), value = TRUE)
  if (length(cols) == 0L) {
    return(character())
  }
  cols[order(.log_lik_column_index(cols, source))]
}


#' Extract integer indexes from Stan vector column names
#' @noRd
.log_lik_column_index <- function(cols, source) {
  pattern <- sprintf("^log_lik_%s\\[|\\]$", source)
  as.integer(gsub(pattern, "", cols))
}


#' Validate a pointwise log-likelihood matrix
#' @noRd
.validate_log_lik_matrix <- function(log_lik) {
  if (!is.matrix(log_lik) || !is.numeric(log_lik)) {
    stop("Pointwise log-likelihood must be a numeric matrix.", call. = FALSE)
  }
  if (nrow(log_lik) == 0L || ncol(log_lik) == 0L) {
    stop("Pointwise log-likelihood matrix must be non-empty.", call. = FALSE)
  }
  if (anyNA(log_lik) || any(!is.finite(log_lik))) {
    stop("Pointwise log-likelihood values must be finite.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Calculate DIC for model comparison
#'
#' Computes the Deviance Information Criterion using the variance-based
#' effective-parameters formula from Gelman et al. (2004): `pD = 0.5 * Var(D)`.
#' This is more stable than the plug-in alternative for multimodal posteriors.
#'
#' DIC is retained for backward compatibility and rough comparison. For
#' principled Bayesian model comparison, prefer [calculate_loo()] or
#' [calculate_waic()] (Vehtari, Gelman, Gabry 2017).
#'
#' @param object An `mlumr_fit` object
#'
#' @return A list of class `mlumr_dic` with components `DIC`, `pD`, `D_bar`,
#'   `n_obs` and `model`.
#' @export
#'
#' @examples
#' \dontrun{
#' dic_spfa <- calculate_dic(fit_spfa)
#' dic_relaxed <- calculate_dic(fit_relaxed)
#' }
calculate_dic <- function(object) {

  log_lik <- extract_log_lik(object)
  if (nrow(log_lik) < 2L) {
    stop("DIC requires at least two posterior draws.", call. = FALSE)
  }
  log_lik_total <- rowSums(log_lik)
  D <- -2 * log_lik_total
  D_bar <- mean(D)
  # Variance-based pD (Gelman et al. 2004), stable for multimodal posteriors.
  pD <- 0.5 * var(D)
  DIC <- D_bar + pD

  out <- list(
    DIC = DIC,
    pD = pD,
    D_bar = D_bar,
    n_obs = ncol(log_lik),
    model = .mlumr_model_label(object)
  )

  class(out) <- "mlumr_dic"
  out
}


#' @method print mlumr_dic
#' @export
print.mlumr_dic <- function(x, ...) {
  cat("DIC for ML-UMR Model\n")
  cat("====================\n\n")
  cat("Model:", x$model, "\n")
  cat("DIC:", round(x$DIC, 2), "\n")
  cat("pD:", round(x$pD, 2), "\n")
  cat("D_bar:", round(x$D_bar, 2), "\n")
  invisible(x)
}


#' Calculate LOO-CV for an mlumr_fit
#'
#' Computes approximate leave-one-out cross-validation (PSIS-LOO, Vehtari,
#' Gelman, Gabry 2017) using the pointwise log-likelihoods stored by the
#' Stan models. Returns a `loo` object from the `loo` package.
#'
#' Pareto-k diagnostics: values > 0.7 indicate observations for which the
#' PSIS approximation is unreliable; the printed output flags these.
#' Typical remedies are running more iterations or, for highly influential
#' AgD rows, refitting without the offending observation to check
#' sensitivity. Moment matching ([loo::loo_moment_match()]) needs the fitted
#' model rather than a log-likelihood matrix, so `moment_match` is refused.
#'
#' @note
#' **AgD rows are treated as independent observations.** Subgroup rows from
#' one study share no clustering term, so their effective sample sizes are
#' inflated and Pareto-k warnings understated; corroborate with
#' [prior_sensitivity()] or by refitting without suspect rows.
#'
#' **Survival fits.** The comparator enters as reconstructed pseudo-IPD, so
#' the default pointwise unit is one pseudo-individual and the criteria are
#' optimistic relative to leaving out the comparator arm. Set
#' `survival_unit = "arm"` or `"aggregate"` to hold out whole comparator
#' arms or all of the external evidence instead.
#'
#' @param object An `mlumr_fit` object.
#' @param survival_unit For survival fits, the LOO/WAIC pointwise unit:
#'   `"observation"` (default; per reconstructed comparator pseudo-individual,
#'   optimistic), `"arm"` (group the comparator pseudo-IPD by comparator arm, so
#'   each external arm is one held-out unit), or `"aggregate"` (all comparator
#'   pseudo-IPD as a single external-evidence unit). The index IPD always stays
#'   per-individual. Ignored for non-survival families.
#' @param ... Further arguments passed to [loo::loo()]; `r_eff` is computed
#'   from the fit's chains.
#'
#' @return An object of class `psis_loo` (see [loo::loo()]).
#' @export
#' @examples
#' \dontrun{
#' loo_spfa <- calculate_loo(fit_spfa)
#' print(loo_spfa)
#' }
calculate_loo <- function(object,
                          survival_unit = c("observation", "arm", "aggregate"),
                          ...) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for calculate_loo(). ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  if ("moment_match" %in% names(list(...))) {
    stop("`moment_match` is not available: `loo` ignores it for a ",
         "log-likelihood matrix, and moment matching needs the fitted model.",
         call. = FALSE)
  }
  survival_unit <- match.arg(survival_unit)
  log_lik <- .survival_log_lik_by_unit(object, survival_unit)
  r_eff <- .relative_eff_from_log_lik(log_lik, .chain_id(object))
  out <- loo::loo(log_lik, r_eff = r_eff, ...)
  # `loo::loo_compare()` warns when these differ between the objects.
  attr(out, "yhash") <- .scoring_identity(object)
  out
}


#' Warn that survival LOO/WAIC pointwise units are reconstructed pseudo-IPD
#'
#' Once per session; suppress with `options(mlumr.quiet_survival_loo = TRUE)`.
#' @noRd
.warn_survival_loo_unit <- function(object) {
  if (!identical(object$family, "survival")) return(invisible())
  if (isTRUE(getOption("mlumr.quiet_survival_loo", FALSE))) return(invisible())
  if (isTRUE(getOption("mlumr.survival_loo_warned", FALSE))) return(invisible())
  warning(
    "Survival LOO/WAIC: the comparator AgD enters as reconstructed pseudo-IPD, ",
    "so each AgD pointwise unit is one pseudo-individual and the criteria are ",
    "optimistic relative to leaving out the comparator arm. Treat them as a ",
    "rough check. Suppress with `options(mlumr.quiet_survival_loo = TRUE)`.",
    call. = FALSE
  )
  options(mlumr.survival_loo_warned = TRUE)
  invisible()
}


#' Pointwise log-likelihood for LOO/WAIC, optionally grouped for survival
#'
#' `"observation"` is `extract_log_lik()`. For survival fits `"arm"` sums the
#' comparator pseudo-IPD columns within each arm and `"aggregate"` sums them
#' all, so leaving out a unit leaves out that arm or all external evidence.
#' The index IPD stays per individual.
#' @noRd
.survival_log_lik_by_unit <- function(object, survival_unit = "observation") {
  .assert_agd_loglik_per_observation(object)
  if (!identical(object$family, "survival") ||
        identical(survival_unit, "observation")) {
    .warn_survival_loo_unit(object)
    return(extract_log_lik(object))
  }

  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) {
    stop("`object$draws` must contain named posterior draw columns.",
         call. = FALSE)
  }
  ipd_cols <- .ordered_log_lik_columns(draws, "ipd")
  agd_cols <- .ordered_log_lik_columns(draws, "agd")
  if (length(agd_cols) == 0L) {
    stop("Grouped survival LOO/WAIC needs AgD pointwise log-likelihood ",
         "columns (`log_lik_agd`).", call. = FALSE)
  }
  .assert_log_lik_complete(object, ipd_cols, agd_cols)
  agd_mat <- as.matrix(draws[, agd_cols, drop = FALSE])

  groups <- if (identical(survival_unit, "aggregate")) {
    rep(1L, ncol(agd_mat))
  } else {
    g <- object$stan_data$agd_arm
    if (is.null(g) || length(g) != ncol(agd_mat)) {
      stop("Cannot group survival AgD by arm: the per-pseudo-individual arm ",
           "map (`stan_data$agd_arm`) is missing or the wrong length.",
           call. = FALSE)
    }
    as.integer(g)
  }

  # One column per group: the log predictive density of the whole unit.
  grouped <- vapply(sort(unique(groups)), function(gg) {
    rowSums(agd_mat[, groups == gg, drop = FALSE])
  }, numeric(nrow(agd_mat)))
  grouped <- matrix(grouped, nrow = nrow(agd_mat))

  ipd_mat <- if (length(ipd_cols) > 0L) {
    as.matrix(draws[, ipd_cols, drop = FALSE])
  } else {
    NULL
  }
  out <- if (is.null(ipd_mat)) grouped else cbind(ipd_mat, grouped)
  .validate_log_lik_matrix(out)
  out
}


#' Calculate WAIC for an mlumr_fit
#'
#' Watanabe-Akaike Information Criterion (Watanabe 2010) based on the
#' pointwise log-likelihoods. WAIC is asymptotically equivalent to
#' LOO-CV; prefer [calculate_loo()] when Pareto-k is well-behaved.
#'
#' @note
#' As with [calculate_loo()], each AgD row is treated as an
#' independent observation. WAIC will be optimistic for AgD rows
#' that share a study (see the note on `calculate_loo()`). For
#' **survival** fits the AgD pointwise unit is a reconstructed
#' pseudo-individual, so WAIC is at the pseudo-individual level
#' (see the survival note on [calculate_loo()]).
#'
#' @param object An `mlumr_fit` object.
#' @param survival_unit For survival fits, the WAIC pointwise unit:
#'   `"observation"` (default), `"arm"`, or `"aggregate"` (see [calculate_loo()]
#'   for details). Ignored for non-survival families.
#' @param ... Further arguments passed to [loo::waic()].
#'
#' @return An object of class `waic` (see [loo::waic()]).
#' @export
#' @examples
#' \dontrun{
#' waic_spfa <- calculate_waic(fit_spfa)
#' }
calculate_waic <- function(object,
                           survival_unit = c("observation", "arm", "aggregate"),
                           ...) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for calculate_waic(). ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  survival_unit <- match.arg(survival_unit)
  log_lik <- .survival_log_lik_by_unit(object, survival_unit)
  out <- loo::waic(log_lik, ...)
  attr(out, "yhash") <- .scoring_identity(object)
  out
}


#' Compare fitted ML-UMR models
#'
#' Compare two or more `mlumr_fit` objects by DIC (default), LOO, or WAIC.
#' For LOO/WAIC, [loo::loo_compare()] is used under the hood; the output
#' is the standard `loo_compare` table. The pointwise scores are paired
#' observation by observation, so the fits must be of the same data in the
#' same row order; fits whose stored outcomes differ are refused. For DIC the
#' return is a data frame ordered by DIC.
#'
#' DIC is the default for backward compatibility and because it has no
#' additional package dependencies. For principled Bayesian model
#' comparison, LOO (Vehtari, Gelman, Gabry 2017) is preferred and
#' requires the optional `loo` package.
#'
#' @param ... Two or more `mlumr_fit` objects. For DIC, `mlumr_dic`
#'   objects are also accepted.
#' @param criterion One of `"dic"` (default), `"loo"`, or `"waic"`.
#'   LOO and WAIC require the optional `loo` package.
#' @param survival_unit For survival fits compared by `"loo"`/`"waic"`, the
#'   pointwise unit forwarded to [calculate_loo()] / [calculate_waic()]:
#'   `"observation"` (default; per reconstructed comparator pseudo-individual,
#'   optimistic), `"arm"`, or `"aggregate"`. Choose `"arm"` or `"aggregate"` to
#'   select on whole-external-arm predictive fit. Ignored for non-survival
#'   families and for `criterion = "dic"`.
#'
#' @return For `"loo"` / `"waic"`: a `compare.loo` table from
#'   [loo::loo_compare()]. For `"dic"`: a data frame (invisibly) with
#'   columns `Model`, `DIC`, `pD`, `Delta_DIC`.
#' @export
compare_models <- function(..., criterion = c("dic", "loo", "waic"),
                           survival_unit = c("observation", "arm", "aggregate")) {

  criterion <- .validate_choice(criterion, c("dic", "loo", "waic"),
                                "criterion")
  survival_unit <- match.arg(survival_unit)
  models <- list(...)
  if (length(models) < 2L) {
    stop("compare_models() requires at least two models.", call. = FALSE)
  }

  if (criterion == "dic") {
    dics <- lapply(models, function(m) {
      if (inherits(m, "mlumr_fit")) {
        calculate_dic(m)
      } else if (inherits(m, "mlumr_dic")) {
        m
      } else {
        stop("For criterion = 'dic', arguments must be mlumr_fit or mlumr_dic objects",
             call. = FALSE)
      }
    })

    model_names <- vapply(dics, function(d) d$model, character(1))
    dic_vals <- vapply(dics, function(d) d$DIC, numeric(1))
    pD_vals <- vapply(dics, function(d) d$pD, numeric(1))
    # DIC is only comparable across fits of one observation set.
    n_obs_vals <- vapply(dics, function(d) d$n_obs %||% NA_integer_, integer(1))
    if (length(unique(stats::na.omit(n_obs_vals))) > 1L) {
      msg <- paste0(
        "Comparing DIC across fits with different observation counts (%s). DIC ",
        "is only comparable on a common data set; this ranking is not meaningful."
      )
      warning(sprintf(msg, paste(n_obs_vals, collapse = ", ")), call. = FALSE)
    }
    model_names <- .comparison_names(models, model_names)

    out <- data.frame(
      Model = model_names,
      DIC = round(dic_vals, 2),
      pD = round(pD_vals, 2),
      Delta_DIC = round(dic_vals - min(dic_vals), 2)
    )

    out <- out[order(out$DIC), ]
    rownames(out) <- NULL

    cat("\nModel Comparison (DIC)\n")
    cat("======================\n\n")
    print(out, row.names = FALSE)
    cat("\nLower DIC = better fit. Delta_DIC > 5 is a rough heuristic for\n")
    cat("meaningful difference, not a formally calibrated threshold.\n")
    cat("DIC should not be the sole basis for model selection.\n")

    return(invisible(out))
  }

  # LOO / WAIC path
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for LOO/WAIC comparison. ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  if (!all(vapply(models, inherits, logical(1), what = "mlumr_fit"))) {
    stop("For criterion = 'loo' or 'waic', all arguments must be mlumr_fit objects",
         call. = FALSE)
  }

  model_names <- .comparison_names(
    models,
    vapply(models, .mlumr_model_label, character(1))
  )
  # `loo_compare()` subtracts pointwise scores by position: a fit of the same
  # data in another row order would give a wrong `se_diff` without notice.
  ids <- lapply(models, .scoring_identity)
  same <- vapply(ids, function(id) isTRUE(all.equal(id, ids[[1L]])), logical(1))
  if (any(lengths(ids) > 0L) && !all(same)) {
    stop("LOO and WAIC pair the models observation by observation, so the ",
         "fits must be of the same data in the same row order. The outcomes ",
         "stored by ", paste(model_names[!same], collapse = ", "), " differ ",
         "from those of ", model_names[[1L]], ".", call. = FALSE)
  }

  calc_fn <- if (criterion == "loo") calculate_loo else calculate_waic
  ic_list <- lapply(models, function(m) calc_fn(m, survival_unit = survival_unit))
  names(ic_list) <- model_names

  cat(sprintf("\nModel Comparison (%s)\n", toupper(criterion)))
  cat(strrep("=", 22), "\n\n", sep = "")
  cmp <- loo::loo_compare(ic_list)
  print(cmp)
  cat(.model_comparison_interpretation(), sep = "\n")
  cat("\n")
  invisible(cmp)
}


# Chain ids per draw: the backend's labels when stored, else reconstructed
# from the chain-major layout, else a single chain (which inflates r_eff).
.chain_id <- function(object) {
  n_draws <- nrow(object$draws)
  if (!is.numeric(n_draws) || length(n_draws) != 1L || n_draws < 1L) {
    return(integer())
  }
  # Authoritative path: real chain labels captured at fit time.
  stored <- object$chain_ids
  if (!is.null(stored) && is.numeric(stored) && length(stored) == n_draws &&
        all(is.finite(stored))) {
    return(as.integer(stored))
  }
  chains <- object$sampling_args$chains %||% 1L
  valid_chains <- is.numeric(chains) &&
    length(chains) == 1L &&
    is.finite(chains) &&
    chains >= 1 &&
    chains == as.integer(chains)
  if (!valid_chains) {
    return(rep(1L, n_draws))
  }
  chains <- as.integer(chains)
  iter_per_chain <- n_draws %/% chains
  if (iter_per_chain * chains != n_draws) return(rep(1L, n_draws))
  rep(seq_len(chains), each = iter_per_chain)
}


#' Compute stable relative effective sample sizes from log-likelihoods
#' @noRd
.relative_eff_from_log_lik <- function(log_lik, chain_id) {
  .validate_log_lik_matrix(log_lik)
  col_max <- apply(log_lik, 2L, max)
  stabilized <- sweep(log_lik, 2L, col_max, "-")
  loo::relative_eff(exp(stabilized), chain_id = chain_id)
}



#' Human-readable model label for an mlumr fit or DIC object
#' @noRd
.mlumr_model_label <- function(object) {
  if (inherits(object, "mlumr_dic") && !is.null(object$model)) {
    return(as.character(object$model)[[1L]])
  }

  model <- object$model %||% NA_character_
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    return("ML-UMR")
  }

  switch(model,
    spfa = "SPFA",
    relaxed = "Relaxed SPFA",
    model
  )
}


#' Use user-supplied comparison names when available
#' @noRd
.comparison_names <- function(models, fallback) {
  user_names <- names(models)
  out <- fallback
  if (!is.null(user_names)) {
    use_user_name <- nzchar(user_names)
    out[use_user_name] <- user_names[use_user_name]
  }
  # Two unnamed fits of one model type would otherwise share a label.
  make.unique(out, sep = " #")
}


#' Check the sampler diagnostics of a fit
#'
#' Warns about divergent transitions, iterations that hit the maximum tree
#' depth, chains that did not come back, split-Rhat above 1.01 or 1.05, and
#' bulk or tail effective sample sizes below 400. A diagnostic the backend did
#' not supply is reported as unavailable rather than read as clean.
#' [mlumr()] runs this check after sampling; call it again on a stored fit to
#' see the same verdict.
#'
#' @param fit An `mlumr_fit` object.
#' @return `NULL`, invisibly; called for its warnings and messages.
#' @seealso [mlumr()], [prior_sensitivity()].
#' @export
check_diagnostics <- function(fit) {

  .validate_mlumr_fit_object(fit)
  diag <- fit$diagnostics %||% list()
  sampling_args <- fit$sampling_args %||% list()

  # A chain that terminated abnormally is dropped by both backends, so the
  # fit may rest on fewer chains than requested. Report that first.
  n_req <- .diagnostic_count(diag$n_chains_requested)
  n_got <- .diagnostic_count(diag$n_chains_returned)
  # `NA` from the backend means the draws could not be labeled by chain.
  if (n_req > 0 && !isTRUE(n_got > 0) &&
        (is.null(diag$n_chains_returned) ||
           any(is.na(diag$n_chains_returned)))) {
    warning(paste0(
      "The draws could not be labeled by chain, so it is not known whether ",
      "all ", n_req, " requested chain(s) returned. Rhat and the effective ",
      "sample sizes below are computed from the draws as stored. Refit, or ",
      "inspect the backend fit object, before reporting these results."
    ), call. = FALSE)
  }
  if (n_req > 0 && n_got > 0 && n_got < n_req) {
    warning(sprintf(
      paste0("Only %d of %d requested chain(s) returned; the remaining chain(s) ",
             "terminated abnormally. The posterior, Rhat, and every effect ",
             "estimate below are based on the surviving chain(s) only, and Rhat ",
             "is not meaningful from a single chain. Do not report these results ",
             "without refitting successfully."),
      n_got, n_req
    ), call. = FALSE)
  }

  # A count the backend did not supply is unknown, not zero.
  n_divergent <- .transition_count(diag$n_divergent)
  if (is.na(n_divergent)) {
    warning("The number of divergent transitions is not available for this ",
            "fit, so it was not checked. A fit with divergences is not ",
            "distinguishable from a clean one here; inspect the backend fit ",
            "object before reporting these results.", call. = FALSE)
  } else if (n_divergent > 0) {
    warning(sprintf(
      "%d divergent transitions detected. Consider increasing adapt_delta (currently %s).",
      n_divergent, .diagnostic_value(sampling_args$adapt_delta)
    ), call. = FALSE)
  }

  n_max_treedepth <- .transition_count(diag$n_max_treedepth)
  if (is.na(n_max_treedepth)) {
    warning("The number of iterations that hit maximum treedepth is not ",
            "available for this fit, so it was not checked.", call. = FALSE)
  } else if (n_max_treedepth > 0) {
    warning(sprintf(
      "%d iterations hit max treedepth. Consider increasing max_treedepth (currently %s).",
      n_max_treedepth, .diagnostic_value(sampling_args$max_treedepth)
    ), call. = FALSE)
  }

  # Keep an infinite Rhat (chains that did not mix); count a missing one
  # rather than dropping it from the maximum.
  n_par <- nrow(fit$summary)
  rhat <- .usable_diagnostic_values(fit$summary$Rhat, n_par)
  .report_missing_diagnostics(rhat, "Rhat", "convergence",
                              fit$summary$variable)
  if (length(rhat$values) > 0L) {
    max_rhat <- max(rhat$values)
    if (max_rhat > 1.05) {
      warning(sprintf(
        "Some Rhat values > 1.05 (max = %s). Chains have likely not converged.",
        .format_diagnostic(max_rhat)
      ), call. = FALSE)
    } else if (max_rhat > 1.01) {
      warning(sprintf(
        "Some Rhat values > 1.01 (max = %s). Chains may not have fully converged.",
        .format_diagnostic(max_rhat)
      ), call. = FALSE)
    }
  }

  ess <- .usable_diagnostic_values(fit$summary$n_eff, n_par)
  .report_missing_diagnostics(ess, "Bulk ESS", "effective sample size",
                              fit$summary$variable)
  if (length(ess$values) > 0L) {
    min_ess <- min(ess$values)
    if (min_ess < 400) {
      warning(sprintf(
        "Some ESS values < 400 (min = %s). Consider running more iterations.",
        .format_diagnostic(min_ess)
      ), call. = FALSE)
    }
  }

  # Tail ESS (Vehtari et al. 2021) guards the reported tail quantiles. An
  # absent or partly missing column is reported, not passed.
  tail_all <- if ("ess_tail" %in% names(fit$summary)) {
    fit$summary$ess_tail
  } else {
    numeric(0)
  }
  tail_vals <- .finite_numeric_values(tail_all)
  n_missing <- length(tail_all) - length(tail_vals)
  if (length(tail_vals) == 0L) {
    hint <- if (!requireNamespace("posterior", quietly = TRUE)) {
      " Install the 'posterior' package to enable this check."
    } else {
      ""
    }
    message("Tail ESS is unavailable for this fit, so tail quantiles were not ",
            "checked.", hint)
  } else {
    if (n_missing > 0L) {
      message(sprintf(
        paste0("Tail ESS is unavailable for %d of %d parameter(s), which were ",
               "not checked; the remaining %d were."),
        n_missing, length(tail_all), length(tail_vals)
      ))
    }
    if (min(tail_vals) < 400) {
      msg <- paste0(
        "Some tail-ESS values < 400 (min = %.1f). Tail quantiles ",
        "(e.g. 2.5%%/97.5%%) may be unreliable; run more iterations."
      )
      warning(sprintf(msg, min(tail_vals)), call. = FALSE)
    }
  }

  invisible(NULL)
}


#' Read a non-negative scalar diagnostic count
#' @noRd
.diagnostic_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    return(0)
  }
  as.integer(x)
}


#' Format a diagnostic setting for warning messages
#' @noRd
.diagnostic_value <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return("unknown")
  }
  as.character(x)
}


#' Return finite numeric values from a summary column
#' @noRd
.finite_numeric_values <- function(x) {
  if (!is.numeric(x)) {
    return(numeric())
  }
  x[is.finite(x)]
}


#' Split a diagnostic column into usable values and missing ones
#'
#' Keeps an infinite value, which is a diagnostic that came out as bad as it
#' can, and counts a missing one rather than dropping it. An absent or
#' non-numeric column counts as `n_expected` missing diagnostics.
#'
#' @param x A summary column, possibly `NULL`.
#' @param n_expected How many parameters should have had a diagnostic.
#' @return A list with `values`, `n_missing`, `n_total` and `missing_idx`.
#' @noRd
.usable_diagnostic_values <- function(x, n_expected = length(x)) {
  if (!is.numeric(x)) {
    n <- as.integer(max(n_expected, length(x)))
    return(list(values = numeric(), n_missing = n, n_total = n,
                missing_idx = seq_len(n)))
  }
  keep <- !is.na(x)
  list(values = x[keep],
       n_missing = sum(!keep),
       n_total = length(x),
       missing_idx = which(!keep))
}


#' Say how many parameters had no diagnostic, rather than dropping them
#'
#' Names the outcome, not a cause: a missing diagnostic is as consistent with
#' a constant quantity as with stuck chains or non-finite draws.
#'
#' @param d A [.usable_diagnostic_values()] result.
#' @param label Diagnostic name for the message.
#' @param what What the diagnostic measures, for the message.
#' @param variables Parameter names in the same order as the column, or `NULL`.
#' @return `NULL`, invisibly.
#' @noRd
.report_missing_diagnostics <- function(d, label, what, variables = NULL) {
  if (d$n_missing > 0L && d$n_total > 0L) {
    named <- ""
    if (!is.null(variables) && length(variables) == d$n_total) {
      missing_names <- utils::head(as.character(variables)[d$missing_idx], 6L)
      if (length(missing_names)) {
        named <- paste0(" Unavailable for: ",
                        paste(missing_names, collapse = ", "),
                        if (d$n_missing > length(missing_names)) {
                          sprintf(" and %d more.", d$n_missing -
                                    length(missing_names))
                        } else {
                          "."
                        })
      }
    }
    message(sprintf(
      paste0("%s is unavailable for %d of %d parameter(s), which were not ",
             "checked for %s; the remaining %d were.%s A diagnostic that ",
             "could not be computed is legitimately absent for a quantity ",
             "that is constant across every draw, and equally absent when ",
             "the chains are stuck or the draws are not finite."),
      label, d$n_missing, d$n_total, what, d$n_total - d$n_missing, named
    ))
  }
  invisible(NULL)
}


#' Format a diagnostic for a message without turning Inf into a number
#'
#' Four significant digits, with a non-finite value printed by name.
#'
#' @param x A single numeric value.
#' @return A single string.
#' @noRd
.format_diagnostic <- function(x) {
  if (!is.finite(x)) {
    return(as.character(x))
  }
  format(x, digits = 4L)
}


#' Print an unknown count as unknown
#'
#' @param n A count, possibly `NA`.
#' @return A single string.
#' @noRd
.diagnostic_display <- function(n) {
  if (length(n) != 1L || is.na(n)) {
    return("unknown")
  }
  as.character(n)
}


#' Note, inline, that a printed statistic was computed without some parameters
#'
#' @param d A [.usable_diagnostic_values()] result.
#' @return A single string, empty when nothing was missing.
#' @noRd
.missing_suffix <- function(d) {
  if (d$n_missing > 0L && d$n_total > 0L) {
    return(sprintf("(over %d of %d parameters; %d unavailable)",
                   d$n_total - d$n_missing, d$n_total, d$n_missing))
  }
  ""
}


#' Read a transition count that the backend may not have supplied
#'
#' Zero is the count that says the sampler behaved, so an unreported,
#' fractional or out-of-range value is `NA` rather than 0.
#'
#' @param x The recorded count.
#' @return A non-negative integer, or `NA_integer_` when unknown.
#' @noRd
.transition_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 ||
        x != trunc(x) || x > .Machine$integer.max) {
    return(NA_integer_)
  }
  as.integer(x)
}


#' The interpretation paragraph the LOO/WAIC comparison prints
#'
#' Callable so a test can assert what users see. The standard error of a
#' difference measures uncertainty about it, not support for it.
#'
#' @return A character vector, one element per printed line.
#' @noRd
.model_comparison_interpretation <- function() {
  c("",
    "elpd_diff is the difference in expected log pointwise predictive",
    "density vs the best model, and se_diff is its standard error: the",
    "uncertainty about that difference, not evidence for it. Read the two",
    "together. A difference small relative to se_diff is not distinguished",
    "from zero by this comparison, whatever se_diff itself is.",
    "Treat any ratio as a heuristic, not a decision rule, and check the",
    "PSIS diagnostics and whether the difference matters for the",
    "prediction you care about.")
}

# Moment-parameterized marginal distributions for add_integration(), as in
# multinma: the stats:: functions are shadowed by versions that also accept
# `mean` and `sd`, which override the native parameters when both are given.

#' The Gamma distribution, parameterized by mean and standard deviation
#'
#' Density, distribution, and quantile functions for the gamma distribution,
#' accepting either the native `shape` / `rate` (or `scale`) parameters or a
#' `mean` and `sd`, which override them. Useful with [distr()] and
#' [add_integration()], where covariate moments come straight from a published
#' baseline table. The moment parameterization uses
#' `shape = (mean / sd)^2` and `rate = mean / sd^2`.
#'
#' @param x,q Vector of quantiles.
#' @param p Vector of probabilities.
#' @param shape,rate,scale See [stats::GammaDist].
#' @param lower.tail,log.p,log See [stats::GammaDist].
#' @param mean,sd Mean and standard deviation, overriding `shape` and
#'   `rate` / `scale` when both are supplied. Both must be named in full.
#' @param ... Must be empty; it keeps `mean` and `sd` out of partial matching.
#'
#' @return A numeric vector, as the corresponding \pkg{stats} function.
#' @seealso [distr()], [add_integration()], [qbern()]
#' @name GammaDist
#' @examples
#' # Equivalent specifications
#' qgamma(0.5, mean = 65, sd = 8)
#' qgamma(0.5, shape = (65 / 8)^2, rate = 65 / 8^2)
NULL

#' @rdname GammaDist
#' @export
qgamma <- function(p, shape, rate = 1, scale = 1 / rate, lower.tail = TRUE,
                   log.p = FALSE, ..., mean, sd) {
  .reject_gamma_dots(...)
  gp <- .gamma_moment_pars(missing(mean), missing(sd),
                           if (missing(mean)) NULL else mean,
                           if (missing(sd)) NULL else sd)
  if (!is.null(gp)) {
    return(stats::qgamma(p, shape = gp$shape, rate = gp$rate,
                         lower.tail = lower.tail, log.p = log.p))
  }
  .reject_rate_and_scale(missing(rate), missing(scale))
  stats::qgamma(p, shape = shape, scale = scale,
                lower.tail = lower.tail, log.p = log.p)
}

#' @rdname GammaDist
#' @export
pgamma <- function(q, shape, rate = 1, scale = 1 / rate, lower.tail = TRUE,
                   log.p = FALSE, ..., mean, sd) {
  .reject_gamma_dots(...)
  gp <- .gamma_moment_pars(missing(mean), missing(sd),
                           if (missing(mean)) NULL else mean,
                           if (missing(sd)) NULL else sd)
  if (!is.null(gp)) {
    return(stats::pgamma(q, shape = gp$shape, rate = gp$rate,
                         lower.tail = lower.tail, log.p = log.p))
  }
  .reject_rate_and_scale(missing(rate), missing(scale))
  stats::pgamma(q, shape = shape, scale = scale,
                lower.tail = lower.tail, log.p = log.p)
}

#' @rdname GammaDist
#' @export
dgamma <- function(x, shape, rate = 1, scale = 1 / rate, log = FALSE,
                   ..., mean, sd) {
  .reject_gamma_dots(...)
  gp <- .gamma_moment_pars(missing(mean), missing(sd),
                           if (missing(mean)) NULL else mean,
                           if (missing(sd)) NULL else sd)
  if (!is.null(gp)) {
    return(stats::dgamma(x, shape = gp$shape, rate = gp$rate, log = log))
  }
  .reject_rate_and_scale(missing(rate), missing(scale))
  stats::dgamma(x, shape = shape, scale = scale, log = log)
}

#' Gamma shape and rate from a mean and a standard deviation
#'
#' Returns `NULL` when neither moment was supplied, so the caller falls through
#' to the native parameterization.
#'
#' @param no_mean,no_sd Whether the caller's `mean` / `sd` were missing.
#' @param mean,sd The supplied moments, or `NULL`.
#' @return A list with `shape` and `rate`, or `NULL`.
#' @noRd
.gamma_moment_pars <- function(no_mean, no_sd, mean, sd) {
  if (no_mean && no_sd) return(NULL)
  if (no_mean || no_sd) {
    stop("The gamma moment parameterization needs both `mean` and `sd`. ",
         "Supply the other one, or give `shape` and `rate` / `scale`.",
         call. = FALSE)
  }
  if (!is.numeric(mean) || !is.numeric(sd)) {
    stop("Gamma `mean` and `sd` must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(mean) | mean <= 0)) {
    stop("Gamma `mean` must be finite and strictly positive.", call. = FALSE)
  }
  if (any(!is.finite(sd) | sd <= 0)) {
    stop("Gamma `sd` must be finite and strictly positive.", call. = FALSE)
  }
  # Dividing twice avoids overflow in sd^2.
  ratio <- mean / sd
  list(shape = ratio^2, rate = ratio / sd)
}

#' Refuse an argument these wrappers do not have
#'
#' `mean` and `sd` sit behind `...` so they cannot take part in partial
#' matching; this check keeps `...` from swallowing a typo.
#' @noRd
.reject_gamma_dots <- function(...) {
  nm <- names(list(...))
  if (length(nm) || ...length() > 0L) {
    lbl <- if (length(nm)) paste(nm[nzchar(nm)], collapse = ", ") else ""
    stop(sprintf(paste0("unused argument%s%s. `mean` and `sd` must be given ",
                        "by their full names."),
                 if (...length() > 1L) "s" else "",
                 if (nzchar(lbl)) paste0(" (", lbl, ")") else ""),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Refuse a conflicting `rate` and `scale`, as \pkg{stats} does
#' @noRd
.reject_rate_and_scale <- function(no_rate, no_scale) {
  if (!no_rate && !no_scale) {
    stop("specify 'rate' or 'scale' but not both", call. = FALSE)
  }
  invisible(TRUE)
}


#' The logit-Normal distribution
#'
#' Density, distribution, and quantile functions for the logit-normal
#' distribution: the distribution of `plogis(z)` where `z` is normal with mean
#' `mu` and standard deviation `sigma`. It is the natural marginal for a
#' covariate reported as a proportion on `(0, 1)`, such as percent body surface
#' area.
#'
#' For convenience the distribution may be given by its `mean` and `sd` on the
#' natural `(0, 1)` scale instead of `mu` and `sigma` on the logit scale. There
#' is no closed form for that reparameterization, so `mu` and `sigma` are found
#' numerically; supply `mu` / `sigma` directly if you have them.
#'
#' @param x,q Vector of quantiles, in `(0, 1)`.
#' @param p Vector of probabilities.
#' @param mu,sigma Location and scale, on the logit scale.
#' @param log Return the log density. Positional, as in [stats::dnorm()].
#' @param ... For `plogitnorm()` and `qlogitnorm()`, passed to the underlying
#'   \pkg{stats} normal function ([stats::pnorm()], [stats::qnorm()]), so
#'   `lower.tail` and `log.p` work as usual. `dlogitnorm()` builds its density
#'   from [stats::dnorm()] and a Jacobian rather than delegating, so it has
#'   nothing to forward and refuses anything passed here; in its signature
#'   `...` serves only to keep `mean` and `sd` from matching positionally.
#' @param mean,sd Mean and standard deviation on the `(0, 1)` scale,
#'   overriding `mu` and `sigma` when both are supplied.
#'
#' @return A numeric vector.
#' @seealso [distr()], [add_integration()], [GammaDist]
#' @name logitNormal
#' @examples
#' qlogitnorm(0.5, mean = 0.34, sd = 0.19)
NULL

#' Refuse arguments a function has no use for
#'
#' A function that computes its own answer would otherwise discard whatever
#' `...` collected, so a misspelled argument would read as a default.
#' @noRd
.reject_unused_dots <- function(...) {
  n <- ...length()
  if (n == 0L) {
    return(invisible(NULL))
  }
  nms <- ...names()
  labels <- vapply(seq_len(n), function(i) {
    if (!is.null(nms) && !is.na(nms[[i]]) && nzchar(nms[[i]])) {
      nms[[i]]
    } else {
      paste0("[[", i, "]] (unnamed)")
    }
  }, character(1))
  stop(sprintf("unused argument%s: %s", if (n > 1L) "s" else "",
               paste(labels, collapse = ", ")), call. = FALSE)
}

#' @rdname logitNormal
#' @export
dlogitnorm <- function(x, mu = 0, sigma = 1, log = FALSE, ..., mean, sd) {
  pars <- .logitnorm_pars(mu, sigma, if (missing(mean)) NULL else mean,
                          if (missing(sd)) NULL else sd,
                          !missing(mean), !missing(sd))
  # Nothing is delegated here, so `...` must be empty.
  .reject_unused_dots(...)
  # The Jacobian form is 0/0 at the support boundaries, so evaluate it on the
  # open interval only and fill in the zero density elsewhere. Recycle before
  # subsetting so each point keeps its own parameters.
  x <- as.numeric(x)
  mu_v <- as.numeric(pars[["mu"]])
  sigma_v <- as.numeric(pars[["sigma"]])
  n <- max(length(x), length(mu_v), length(sigma_v))
  if (n == 0L || !length(x) || !length(mu_v) || !length(sigma_v)) {
    return(numeric(0))
  }
  x <- rep_len(x, n)
  mu_v <- rep_len(mu_v, n)
  sigma_v <- rep_len(sigma_v, n)

  # Coerce `log` as the stats functions do.
  if (length(log) != 1L || is.na(as.logical(log))) {
    stop("`log` must be a single non-missing value coercible to TRUE or FALSE.",
         call. = FALSE)
  }
  is_log <- as.logical(log)
  out <- rep(if (is_log) -Inf else 0, n)
  # An unusable parameter gives NA, not zero.
  bad <- !is.finite(mu_v) | !is.finite(sigma_v) | sigma_v < 0
  inside <- !is.na(x) & x > 0 & x < 1 & !bad
  if (any(inside)) {
    ld <- stats::dnorm(stats::qlogis(x[inside]), mean = mu_v[inside],
                       sd = sigma_v[inside], log = TRUE) -
      base::log(x[inside]) - log1p(-x[inside])
    out[inside] <- if (is_log) ld else exp(ld)
  }
  out[bad] <- NA_real_
  out[is.na(x)] <- x[is.na(x)]   # keeps NA as NA and NaN as NaN
  out
}

#' @rdname logitNormal
#' @export
plogitnorm <- function(q, mu = 0, sigma = 1, ..., mean, sd) {
  pars <- .logitnorm_pars(mu, sigma, if (missing(mean)) NULL else mean,
                          if (missing(sd)) NULL else sd,
                          !missing(mean), !missing(sd))
  # Clamping to [0, 1] gives 0 below the support and 1 above it under every
  # `lower.tail` and `log.p` combination.
  q <- as.numeric(q)
  finite <- !is.na(q)
  q[finite] <- pmin(pmax(q[finite], 0), 1)
  stats::pnorm(stats::qlogis(q), mean = pars[["mu"]], sd = pars[["sigma"]], ...)
}

#' @rdname logitNormal
#' @export
qlogitnorm <- function(p, mu = 0, sigma = 1, ..., mean, sd) {
  pars <- .logitnorm_pars(mu, sigma, if (missing(mean)) NULL else mean,
                          if (missing(sd)) NULL else sd,
                          !missing(mean), !missing(sd))
  stats::plogis(stats::qnorm(p, mean = pars[["mu"]], sd = pars[["sigma"]], ...))
}

#' Moments of a logit-normal, by numerical integration
#'
#' Integrates over the latent normal variable, splitting the range at
#' `z0 = -mu / sigma`, clamped between -8 and 8, where the logistic transition sits, with `abs.tol = 0`
#' because the variance of a concentrated margin is far below the default
#' absolute tolerance. Returns `NULL` when the quadrature fails.
#' @noRd
.ln_moments <- function(mu, sigma) {
  if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) return(NULL)
  z0 <- max(-8, min(8, -mu / sigma))
  int <- function(f) {
    stats::integrate(f, -Inf, z0, rel.tol = 1e-11, abs.tol = 0)$value +
      stats::integrate(f, z0, Inf, rel.tol = 1e-11, abs.tol = 0)$value
  }
  g <- function(z) stats::plogis(mu + sigma * z)
  out <- tryCatch({
    m <- int(function(z) g(z) * stats::dnorm(z))
    v <- int(function(z) (g(z) - m)^2 * stats::dnorm(z))
    if (!is.finite(m) || !is.finite(v) || v < 0) {
      NULL
    } else {
      c(mean = m, sd = sqrt(v))
    }
  }, error = function(e) NULL)
  out
}

#' Squared relative distance between a logit-normal's moments and a target
#'
#' `est` is `(mu, log sigma)`; the residuals are relative to their targets so
#' a small margin is judged on its own scale.
#' @noRd
.lndiff <- function(est, m, s) {
  mom <- .ln_moments(est[[1L]], exp(est[[2L]]))
  if (is.null(mom)) return(.Machine$double.xmax^0.5)
  ((mom[["mean"]] - m) / m)^2 + ((mom[["sd"]] - s) / s)^2
}

#' Solve for one logit-normal (mu, sigma) from a mean and SD
#'
#' Starts from the delta-method approximation on the logit scale, restarts
#' Nelder-Mead, at most eight attempts, until a restart no longer improves the objective, and checks
#' that the recovered moments reproduce the target to within `tol` (relative).
#' @noRd
.lnopt <- function(m, s, tol = 1e-4) {
  par <- c(stats::qlogis(m), log(s / (m * (1 - m))))
  prev <- Inf
  bad <- TRUE
  for (k in seq_len(8L)) {
    opt <- stats::optim(par, .lndiff, m = m, s = s,
                        control = list(reltol = 1e-12, maxit = 2000L))
    par <- opt$par
    bad <- opt$convergence != 0
    if (opt$value >= prev * (1 - 1e-6)) break
    prev <- opt$value
  }
  pars <- c(mu = par[[1L]], sigma = exp(par[[2L]]))
  if (!bad) {
    mom <- .ln_moments(pars[["mu"]], pars[["sigma"]])
    # Convergence is not the same as having hit the target.
    bad <- is.null(mom) ||
      abs(mom[["mean"]] - m) > tol * m ||
      abs(mom[["sd"]] - s) > tol * s
  }
  if (bad) {
    warning(sprintf(paste0("logit-normal moment matching failed for mean = %g, ",
                           "sd = %g; NAs produced. Supply `mu` and `sigma` on ",
                           "the logit scale instead."), m, s), call. = FALSE)
    return(c(mu = NA_real_, sigma = NA_real_))
  }
  pars
}

#' Estimate logit-normal mu / sigma from a mean and SD on (0, 1)
#'
#' A variable on `(0, 1)` has `Var(X) < mean * (1 - mean)`, so an impossible
#' pair is refused before the optimizer is asked.
#' @noRd
.pars_logitnorm <- function(m, s) {
  if (length(m) != length(s) && length(m) > 1 && length(s) > 1) {
    stop("`mean` and `sd` must be the same length.", call. = FALSE)
  }
  if (!is.numeric(m) || !is.numeric(s) || any(!is.finite(m)) ||
        any(!is.finite(s))) {
    stop("logit-normal `mean` and `sd` must be finite numbers.", call. = FALSE)
  }
  if (!length(m) || !length(s)) return(as.data.frame(list(mu = numeric(0),
                                                          sigma = numeric(0))))
  # Recycle before validating, so the offending pair can be named.
  n <- max(length(m), length(s))
  m <- rep_len(m, n)
  s <- rep_len(s, n)
  # Open interval: the logit of 0 or 1 is infinite, so a boundary mean has no
  # logit-normal representation at all.
  if (any(m <= 0 | m >= 1)) {
    stop("logit-normal `mean` must be strictly inside (0, 1). Have you ",
         "rescaled a percentage?", call. = FALSE)
  }
  if (any(s <= 0)) {
    stop("logit-normal `sd` must be strictly positive.", call. = FALSE)
  }
  infeasible <- s^2 >= m * (1 - m)
  if (any(infeasible)) {
    i <- which(infeasible)[[1L]]
    stop(sprintf(paste0("logit-normal `sd` = %g is impossible for `mean` = %g: ",
                        "sd must be under sqrt(mean * (1 - mean)) = %g."),
                 s[[i]], m[[i]], sqrt(m[[i]] * (1 - m[[i]]))), call. = FALSE)
  }
  as.data.frame(do.call(rbind, mapply(.lnopt, m, s, SIMPLIFY = FALSE)))
}

#' Resolve logit-normal parameters from either parameterization
#' @noRd
.logitnorm_pars <- function(mu, sigma, mean, sd, has_mean, has_sd) {
  if (has_mean && has_sd) return(.pars_logitnorm(mean, sd))
  if (has_mean || has_sd) {
    stop("The logit-normal moment parameterization needs both `mean` and ",
         "`sd`. Supply the other one, or give `mu` and `sigma` on the logit ",
         "scale.", call. = FALSE)
  }
  .validate_logitnorm_native(mu, sigma)
}

#' Validate native logit-normal `mu` / `sigma`
#' @noRd
.validate_logitnorm_native <- function(mu, sigma) {
  # Test missingness before type, since a bare NA is logical.
  if (anyNA(mu) || anyNA(sigma)) {
    stop("logit-normal `mu` and `sigma` must not be missing.", call. = FALSE)
  }
  if (!is.numeric(mu) || !is.numeric(sigma)) {
    stop("logit-normal `mu` and `sigma` must be numeric.", call. = FALSE)
  }
  if (!length(mu) || !length(sigma)) return(list(mu = mu, sigma = sigma))
  if (any(!is.finite(mu))) {
    stop("logit-normal `mu` must be finite.", call. = FALSE)
  }
  if (any(!is.finite(sigma))) {
    stop("logit-normal `sigma` must be finite.", call. = FALSE)
  }
  if (any(sigma <= 0)) {
    stop("logit-normal `sigma` must be strictly positive.", call. = FALSE)
  }
  # Partial recycling pairs values with the wrong parameter.
  if (length(mu) != length(sigma) && length(mu) > 1L && length(sigma) > 1L) {
    stop("logit-normal `mu` and `sigma` must be the same length, or one of ",
         "them a single value.", call. = FALSE)
  }
  list(mu = mu, sigma = sigma)
}

#' Get or set the Stan engine
#'
#' mlumr fits its models through rstan by default. `mlumr_engine("cmdstanr")`
#' switches to cmdstanr for the session; the choice is stored in
#' `options(mlumr.stan_engine)`, so a permanent default belongs in
#' `.Rprofile`. When cmdstanr or CmdStan is missing, an interactive session is
#' offered their installation (cmdstanr from stan-dev's maintained
#' repository); otherwise the install commands are printed and the engine is
#' left unchanged.
#'
#' @param engine `"rstan"` or `"cmdstanr"`, matched exactly. `NULL` (the
#'   default) returns the current engine without changing it.
#' @return The current engine, invisibly when setting.
#' @export
#' @examples
#' mlumr_engine()
#' \dontrun{
#' mlumr_engine("cmdstanr")
#' }
mlumr_engine <- function(engine = NULL) {
  if (is.null(engine)) return(get_engine())
  engine <- .validate_engine_name(engine)
  if (engine == "cmdstanr" && !.install_cmdstanr()) {
    current <- getOption("mlumr.stan_engine", "rstan")
    message("Engine unchanged (", current, ").")
    return(invisible(current))
  }
  options(mlumr.stan_engine = engine)
  message("mlumr engine set to: ", engine)
  invisible(engine)
}


#' Install cmdstanr and CmdStan when they are missing, asking first
#'
#' cmdstanr comes from stan-dev's maintained repository, not the one pinned in
#' `Additional_repositories`, which serves a cmdstanr too old to build CmdStan
#' on Windows with current R.
#' @return `TRUE` when cmdstanr and CmdStan are available afterwards.
#' @noRd
.install_cmdstanr <- function() {
  repos <- c("https://stan-dev.r-universe.dev", getOption("repos"))
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    message("cmdstanr is not installed.")
    if (!interactive() ||
          utils::menu(c("Yes", "No"), title = "Install cmdstanr from stan-dev.r-universe.dev?") != 1L) {
      message("Install it with:\n",
              '  install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))')
      return(FALSE)
    }
    utils::install.packages("cmdstanr", repos = repos)
    if (!requireNamespace("cmdstanr", quietly = TRUE)) return(FALSE)
  }
  if (!.cmdstan_available()) {
    message("CmdStan is not installed.")
    if (!interactive() ||
          utils::menu(c("Yes", "No"), title = "Install CmdStan with cmdstanr::install_cmdstan()?") != 1L) {
      message("Install it with:\n  cmdstanr::install_cmdstan()")
      return(FALSE)
    }
    cmdstanr::install_cmdstan()
  }
  .cmdstan_available()
}


#' Get the current Stan engine (internal)
#' @noRd
get_engine <- function() {
  .validate_engine_name(getOption("mlumr.stan_engine", "rstan"))
}


#' Validate a Stan engine name
#' @noRd
.validate_engine_name <- function(engine) {
  valid <- is.character(engine) &&
    length(engine) == 1L &&
    !is.na(engine) &&
    nzchar(engine)

  if (!valid || !engine %in% .supported_engines()) {
    stop("`engine` must be 'rstan' or 'cmdstanr'.", call. = FALSE)
  }

  engine
}


#' Supported Stan engines
#' @noRd
.supported_engines <- function() {
  c("rstan", "cmdstanr")
}


#' Check whether CmdStan is configured for cmdstanr
#' @noRd
.cmdstan_available <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    return(FALSE)
  }

  isTRUE(tryCatch(
    nzchar(cmdstanr::cmdstan_path()),
    error = function(e) FALSE
  ))
}

#' Family metadata registry
#'
#' Family-specific Stan model names, AgD weighting, prediction-variable
#' prefixes, and supported links and effect measures, looked up by every
#' family branch in the R code.
#'
#' Fields:
#' \describe{
#'   \item{`stan_prefix`}{Prefix for the Stan model name (the full name
#'     is `<stan_prefix>_{spfa,relaxed}`).}
#'   \item{`predict_prefix`}{Column prefix for generated-quantity variables
#'     in [predict.mlumr_fit()] (e.g. `"p"`, `"y"`, `"rate"`).}
#'   \item{`link_default`}{The default link when the user passes
#'     `link = NULL`.}
#'   \item{`links`}{Vector of supported links. Should match the branches
#'     in [check_link()].}
#'   \item{`effect_measures`}{Supported values of the `effect` argument in
#'     [marginal_effects()] (excluding `"all"`). For `"survival"` the scalar
#'     contrast is chosen per fit by `.surv_scalar_effect_name()`; `"hr"`
#'     stands for it here.}
#'   \item{`marginal_effect_vars`}{Generated-quantity column names for each
#'     effect measure, per population. Expanded in [marginal_effects()].}
#'   \item{`comp_weight_field`}{The Stan-data field the comparator-population
#'     marginal predictions are weighted by, which must match the field the
#'     family's `generated quantities` block uses: `n_agd` (binomial), `E_agd`
#'     (poisson), `agd_weight` (normal) and `NULL` for survival.}
#' }
#'
#' @noRd
family_config <- list(
  binomial = list(
    stan_prefix          = "mlumr_binary",
    predict_prefix       = "p",
    link_default         = "logit",
    links                = c("logit", "probit", "cloglog"),
    effect_measures      = c("lor", "rd", "rr"),
    marginal_effect_vars = list(
      lor = c("lor_index", "lor_comparator"),
      rd  = c("rd_index",  "rd_comparator"),
      rr  = c("rr_index",  "rr_comparator")
    ),
    comp_weight_field    = "n_agd"
  ),
  normal = list(
    stan_prefix          = "mlumr_normal",
    predict_prefix       = "y",
    link_default         = "identity",
    links                = c("identity", "log"),
    effect_measures      = c("md"),
    marginal_effect_vars = list(
      md = c("delta_index", "delta_comparator")
    ),
    # The normal Stan models weight the comparator-population marginal by
    # `agd_weight` (required outcome_n for multiple rows, or one for a single
    # row), so every marginal path must use the same target weights.
    comp_weight_field    = "agd_weight"
  ),
  poisson = list(
    stan_prefix          = "mlumr_poisson",
    predict_prefix       = "rate",
    link_default         = "log",
    links                = c("log"),
    effect_measures      = c("rr"),
    marginal_effect_vars = list(
      rr = c("delta_index", "delta_comparator")
    ),
    comp_weight_field    = "E_agd"
  ),
  survival = list(
    # NB stan_prefix is overridden to "mlumr_survival_mspline" in mlumr() for
    # the flexible-baseline distributions ("mspline", "pexp"); see
    # .survival_distribution_info().
    stan_prefix          = "mlumr_survival",
    predict_prefix       = "surv",
    link_default         = "log",
    links                = c("log"),
    # `delta_*` is a log HR or log time ratio, exponentiated by
    # marginal_effects(); `rmstr` is formed there from the rmst_* draws.
    effect_measures      = c("hr", "rmstd", "rmstr"),
    # `rmstr` has no draw column of its own, so it has no entry here.
    marginal_effect_vars = list(
      hr    = c("delta_index", "delta_comparator"),
      rmstd = c("rmst_diff_index", "rmst_diff_comparator")
    ),
    comp_weight_field    = NULL
  )
)

#' Lookup helper for the family registry
#' @noRd
get_family_config <- function(family) {
  if (!is.character(family) || length(family) != 1L) {
    stop("`family` must be a single string", call. = FALSE)
  }
  cfg <- family_config[[family]]
  if (is.null(cfg)) {
    stop(sprintf("Unknown family '%s'. Supported: %s.",
                 family, paste(names(family_config), collapse = ", ")),
         call. = FALSE)
  }
  cfg
}

#' Can the aggregate data identify the comparator coefficients?
#'
#' In the relaxed model the comparator coefficients `beta_comparator` are
#' informed only by the aggregate rows. With `K` covariates there are `K + 1`
#' comparator parameters, so at least `K + 1` distinct aggregate rows are
#' needed, and under an identity link the rows must also differ in every
#' covariate direction.
#'
#' The subgroup mean profiles are centered, divided by the IPD covariate SDs
#' and decomposed. `cond_inv` is the ratio of the smallest to the largest
#' singular value and goes to 0 as the rows collapse onto a lower-dimensional
#' set. `eff_dim` is the participation ratio of the squared singular values,
#' the number of directions the rows effectively spread along, from 1 to
#' `K`; it is 0 when the rows do not vary or cannot be decomposed. `spread`
#' is the RMS distance of the rows from their center along the
#' dominant direction, in IPD SDs; it supplies the absolute scale `cond_inv`
#' lacks. For a normal identity-link model the subgroup means are the
#' aggregate design and the screen flags `cond_inv < 0.2` or `spread < 0.05`,
#' which are package heuristics. For other links the integrated response also
#' depends on each row's covariate distribution, so the geometry is
#' descriptive only and `flagged` is `NA` unless there are too few rows.
#' Reconstructed survival curves are refused, since a curve is not one scalar
#' summary per row. Neither measure sees subgroup sizes or outcome precision,
#' so confirm any verdict with the coefficient posterior and
#' [prior_sensitivity()]. The subgroup-identification vignette works through
#' the cases.
#'
#' @param x An `mlumr_data` object or a fitted relaxed `mlumr_fit`.
#' @param verbose Print a readable report (default `TRUE`).
#' @param link Planned link for an unfitted data object. Defaults to the
#'   family default. A fitted object always uses its stored link.
#'
#' @return Invisibly, a list with `n_rows`, `n_distinct` (rows that do not
#'   repeat another's integration grid), `n_cov`, `n_rows_needed` (`K + 1`),
#'   `cond_inv`, `eff_dim`, `spread`, `singular_values`, `means` (the scaled,
#'   centered subgroup mean matrix), `diagnostic_scope` (`"identity"` or
#'   `"descriptive"`) and `flagged`.
#'
#' @seealso [mlumr()] for `model = "relaxed"`; [prior_sensitivity()].
#' @export
#' @examples
#' \dontrun{
#' dat <- add_integration(combine_data(ipd, agd), n_int = 64, ...)
#' check_identification(dat)
#' }
check_identification <- function(x, verbose = TRUE, link = NULL) {
  is_fit <- inherits(x, "mlumr_fit")
  if (is_fit && !is.null(link)) {
    stop("`link` is determined by the fitted object and cannot be overridden.",
         call. = FALSE)
  }
  if (is_fit && !identical(x$model, "relaxed")) {
    stop("check_identification() diagnoses the comparator coefficients of a ",
         "relaxed fit. This fit used model = \"", x$model, "\", which shares ",
         "one coefficient vector across treatments and so has no ",
         "comparator-only coefficients. Pass the mlumr_data object to see ",
         "the aggregate design geometry on its own.", call. = FALSE)
  }
  data <- if (is_fit) x$data else x
  if (!inherits(data, "mlumr_data")) {
    stop("`x` must be an mlumr_data object (from combine_data()) or an ",
         "mlumr_fit.", call. = FALSE)
  }
  family <- data$family %||% "binomial"
  if (identical(family, "survival")) {
    stop("check_identification() is a subgroup-mean geometry diagnostic for ",
         "the binomial, normal and poisson families and is not valid for ",
         "reconstructed survival curves, whose repeated event and censoring ",
         "times identify model-dependent combinations of the comparator ",
         "parameters. For a relaxed survival fit, inspect the coefficient ",
         "posterior and run prior_sensitivity() instead.", call. = FALSE)
  }
  covs <- data$covariates
  n_cov <- length(covs)

  means <- .agd_mean_profiles(data)
  ref_sd <- apply(as.matrix(data$ipd$data[, covs, drop = FALSE]), 2, stats::sd)
  geom <- .subgroup_geometry(means, ref_sd)

  out <- c(list(n_rows = nrow(means), n_cov = n_cov,
                n_rows_needed = n_cov + 1L,
                n_distinct = .agd_distinct_profiles(data)), geom)
  resolved_link <- if (is_fit) x$link else check_link(family, link)$link
  out$diagnostic_scope <- if (family == "normal" && resolved_link == "identity") {
    "identity"
  } else {
    "descriptive"
  }
  out$flagged <- if (out$n_distinct < out$n_rows_needed) {
    TRUE
  } else if (out$diagnostic_scope == "identity") {
    out$cond_inv < 0.2 || !.at_least(out$spread, 0.05)
  } else {
    NA
  }

  if (verbose) .print_identification(out, covs)
  invisible(out)
}


# The declared `<covariate>_mean` columns define the aggregate design and do
# not move with the integration resolution, so they are preferred. The
# realized integration means are the fallback for objects without those
# columns and the check that each `distr()` reads its own row.
.agd_mean_profiles <- function(data) {
  covs <- data$covariates
  mean_cols <- paste0(covs, "_mean")
  agd <- data$agd$data
  realized <- .agd_realized_profiles(data, covs)
  if (all(mean_cols %in% names(agd))) {
    means <- as.matrix(agd[, mean_cols, drop = FALSE])
    storage.mode(means) <- "double"
    if (all(is.finite(means))) {
      colnames(means) <- covs
      ipd_cov <- data$ipd$data[, covs, drop = FALSE]
      ref_sd <- apply(as.matrix(ipd_cov), 2L, stats::sd)
      if (!.realized_matches_declared(means, realized, ref_sd)) {
        warning("The integration distributions do not reproduce the declared ",
                "aggregate covariate means, so the realized integration ",
                "means, which are what the likelihood sees, are reported. ",
                "Check that each `distr()` reads its row's summaries.",
                call. = FALSE)
        return(realized)
      }
      return(means)
    }
  }
  if (is.null(realized)) {
    stop("Aggregate covariate means are unavailable: this object has no ",
         "`<covariate>_mean` columns and no integration points. Run ",
         "add_integration().", call. = FALSE)
  }
  realized
}


#' Mean covariate profile realized by each row's integration points
#' @noRd
.agd_realized_profiles <- function(data, covs) {
  x_int <- data$integration_points
  if (is.null(x_int)) return(NULL)
  means <- apply(x_int, c(1L, 3L), mean)
  means <- matrix(means, nrow = dim(x_int)[1L], ncol = dim(x_int)[3L])
  colnames(means) <- covs
  means
}


#' Singular-value geometry of the subgroup mean profiles
#'
#' Rows are centered and divided by the IPD SDs, so a covariate measured in
#' large units cannot dominate by units alone. Returns `cond_inv` (smallest
#' over largest singular value), `eff_dim` (participation ratio of the squared
#' singular values, the number of directions effectively spanned), `spread`
#' (RMS distance of the rows from their center along the dominant direction,
#' in IPD SDs), `singular_values` and the scaled `means`. A design whose rows
#' do not vary or cannot be decomposed reports zero geometry, `eff_dim`
#' included, as `.profile_rank()` does.
#' @noRd
.subgroup_geometry <- function(means, ref_sd) {
  M <- scale(as.matrix(means), center = TRUE, scale = FALSE)
  k <- ncol(M)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2, ref_sd, "/")
  degenerate <- list(cond_inv = 0, eff_dim = 0, spread = 0,
                     singular_values = rep(0, k), means = M)
  if (nrow(M) < 2L || !all(is.finite(M))) return(degenerate)
  d <- tryCatch(svd(M)$d, error = function(e) NULL)
  if (is.null(d)) return(degenerate)
  d <- d[is.finite(d)]
  if (!length(d) || max(d) <= 0) return(degenerate)
  if (length(d) < k) d <- c(d, rep(0, k - length(d)))
  # The participation ratio is scale-free, but its fourth powers overflow for
  # huge singular values, so normalize first.
  dn <- d / max(d)
  list(cond_inv = min(d) / max(d),
       eff_dim = sum(dn^2)^2 / sum(dn^4),
       spread = max(d) / sqrt(nrow(M)),
       singular_values = d,
       means = M)
}


# Relative slack shared by every comparison of a spread against a threshold in
# this file, so screens that measure one design through different
# decompositions cannot disagree by a few ULPs.
.spread_tol <- 1e-8

.at_least <- function(x, threshold) {
  if (!all(is.finite(threshold))) return(x >= threshold)
  x >= threshold - abs(threshold) * .spread_tol
}


#' Number of directions an aggregate design spreads along, plus the intercept
#'
#' Profiles are centered and divided by the IPD SDs, then the directions whose
#' RMS spread reaches `min_spread` IPD SDs are counted; the floor is the value
#' [check_identification()] screens `spread` on, so the two agree. `qr()` is
#' not used because it judges each column against its own norm: an offset of
#' 1e7 collapses the rank and a separation of 1e-11 still counts. A design
#' that cannot be decomposed returns 0.
#' @noRd
.profile_rank <- function(profiles, ref_sd, min_spread = 0.05) {
  M <- scale(as.matrix(profiles), center = TRUE, scale = FALSE)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2L, ref_sd, "/")
  if (!all(is.finite(M))) return(0L)
  d <- tryCatch(svd(M)$d, error = function(e) NULL)
  if (is.null(d) || !length(d) || any(!is.finite(d))) return(0L)
  as.integer(sum(.at_least(d / sqrt(nrow(M)), min_spread))) + 1L
}


#' Numerical rank of the centered aggregate profile matrix, plus the intercept
#'
#' `.profile_rank()` says how far a design moves; this says whether the
#' directions exist at all, with the usual `max(dim) * eps * max(d)`
#' tolerance. Profiles at -0.01 and 0.01 have a spread below the screen and a
#' numerical rank of 2, and precise aggregate outcomes can still pin the slope
#' down there.
#' @noRd
.profile_numeric_rank <- function(profiles, ref_sd) {
  M <- scale(as.matrix(profiles), center = TRUE, scale = FALSE)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2L, ref_sd, "/")
  if (!all(is.finite(M))) return(0L)
  d <- tryCatch(svd(M)$d, error = function(e) NULL)
  if (is.null(d) || !length(d) || any(!is.finite(d))) return(0L)
  tol <- max(dim(M)) * .Machine$double.eps * max(d)
  as.integer(sum(d > tol)) + 1L
}


#' Number of distinct aggregate likelihood profiles
#'
#' Two rows built from the same integration grid contribute the same
#' likelihood term whatever the link, so the second adds no constraint. Each
#' grid is sorted into a canonical order before comparing, because the
#' likelihood sees the multiset of points and not their order. Returns the row
#' count when there are no integration points.
#' @noRd
.agd_distinct_profiles <- function(data) {
  x_int <- data$integration_points
  n_rows <- nrow(data$agd$data)
  if (is.null(x_int) || length(dim(x_int)) != 3L) return(n_rows)
  n <- dim(x_int)[[1L]]
  if (n < 2L) return(n)
  keys <- vapply(seq_len(n), function(i) {
    grid <- x_int[i, , , drop = FALSE]
    dim(grid) <- dim(x_int)[2:3]
    ord <- do.call(order, as.data.frame(grid))
    paste(sprintf("%.17g", grid[ord, , drop = FALSE]), collapse = "\r")
  }, character(1))
  length(unique(keys))
}


#' Print the identification report
#' @noRd
.print_identification <- function(x, covs) {
  cat("\nComparator identification (relaxed model)\n\n")
  cat(sprintf("Aggregate rows:      %d (%d distinct)\n", x$n_rows,
              x$n_distinct))
  cat(sprintf("Covariates:          %d (%s)\n", x$n_cov,
              paste(covs, collapse = ", ")))
  cat(sprintf("Rows needed (K + 1): %d\n", x$n_rows_needed))
  cat(sprintf("Spectral dimension:  %.2f of %d (eff_dim)\n", x$eff_dim, x$n_cov))
  cat(sprintf("Balance (cond_inv):  %.4f\n", x$cond_inv))
  cat(sprintf("Spread (IPD SDs):    %.4g\n\n", x$spread))
  verdict <- if (x$n_distinct < x$n_rows_needed) {
    sprintf(paste("WEAK: %d distinct aggregate row(s) cannot separate the %d",
                  "comparator parameters; supply jointly defined subgroup",
                  "rows or use model = \"spfa\"."),
            x$n_distinct, x$n_rows_needed)
  } else if (x$diagnostic_scope == "descriptive") {
    paste("DESCRIPTIVE ONLY: under a nonlinear link the subgroup means do not",
          "determine the likelihood geometry, so the spread reported above",
          "neither flags nor clears identification.")
  } else if (x$cond_inv < 0.2) {
    paste("WEAK: the subgroup means lie close to a lower-dimensional set, as",
          "when subgroups are reported one variable at a time, so some",
          "comparator coefficients are barely separated.")
  } else if (!.at_least(x$spread, 0.05)) {
    sprintf(paste("WEAK: the subgroup means sit within %.3g IPD SD of their",
                  "center, so every comparator slope rests on a short lever."),
            x$spread)
  } else {
    paste("NOT FLAGGED: the row count, balance and spread are above the",
          "screening values. This does not establish identification.")
  }
  cat(strwrap(paste(verdict, "Confirm with the coefficient posterior and",
                    "prior_sensitivity()."), width = 78), sep = "\n")
  invisible(x)
}


#' Does the realized integration design reproduce the declared one?
#'
#' Compares each row's realized integration means with its declared
#' `<covariate>_mean` values, in reference SDs. A finite grid misses its
#' declared mean by a few hundredths of an SD, while a `distr()` that ignores
#' its row misses by the whole distance to whatever it was given, so a quarter
#' of an SD separates the two. `TRUE` when there is nothing to compare, or
#' when the two cannot be compared (with a warning).
#'
#' @param declared Matrix of declared mean profiles, rows by covariates.
#' @param realized Matrix of realized integration means, or `NULL`.
#' @param ref_sd Reference SD per covariate; `NULL` falls back to each
#'   declared column's range.
#' @param max_location_gap Largest per-row distance, in reference SDs, that
#'   still counts as a match.
#' @noRd
.realized_matches_declared <- function(declared, realized, ref_sd = NULL,
                                       max_location_gap = 0.25) {
  if (is.null(realized)) return(TRUE)
  if (!identical(dim(declared), dim(realized))) {
    warning("The realized integration means could not be compared with the ",
            "declared aggregate means because the two have different shapes; ",
            "the declared columns are reported unchecked.", call. = FALSE)
    return(TRUE)
  }
  scale_by <- if (is.null(ref_sd)) {
    apply(declared, 2L, function(col) {
      s <- diff(range(col))
      if (!is.finite(s) || s <= 0) s <- max(abs(col))
      if (!is.finite(s) || s <= 0) s <- 1
      s
    })
  } else {
    s <- as.numeric(ref_sd)
    s[!is.finite(s) | s <= 0] <- 1
    s
  }
  gap <- sweep(as.matrix(realized) - as.matrix(declared), 2L, scale_by, "/")
  if (!all(is.finite(gap))) {
    warning("The realized integration means could not be compared with the ",
            "declared aggregate means because one of them is not finite; ",
            "the declared columns are reported unchecked.", call. = FALSE)
    return(TRUE)
  }
  all(sqrt(rowSums(gap^2)) <= max_location_gap)
}

#' Add numerical integration points
#'
#' Generate quasi-Monte Carlo integration points using Sobol sequences and a
#' Gaussian copula to account for correlations between covariates in the AgD.
#'
#' @param data An `mlumr_data` object from [combine_data()]
#' @param n_int Number of integration points (default 64; use powers of 2).
#'   More points can improve quasi-Monte Carlo integration of the AgD likelihood
#'   and comparator-population estimands. Increase it when
#'   [check_integration()] shows numerical sensitivity. Wider posterior
#'   intervals alone indicate neither an inadequate grid nor a need for more
#'   points. Larger values cost more sampling time.
#' @param cor Correlation matrix for covariates, on the covariate scale. If
#'   `NULL` (the default) it is estimated from the IPD; see the
#'   correlation-transport note in Details.
#' @param cor_adjust Adjustment method: `"spearman"`, `"pearson"`, or `"none"`
#' @param verbose Logical; if `FALSE`, suppresses progress messages.
#' @param ... Distribution specifications for each covariate using [distr()]
#'
#' @return An `mlumr_data` object with integration points added
#' @export
#'
#' @details
#' **The correlation structure is assumed to transport.** Aggregate data
#' report marginal summaries only, so with `cor = NULL` the within-row
#' correlation is estimated from the IPD and applied to every comparator
#' row, as in ML-NMR (Phillippo et al. 2020). The assumption is untestable
#' from the data; supply `cor` from an external source to vary it, and use
#' [check_integration()] to confirm the realized moments and correlations.
#'
#' `cor_adjust` maps the covariate-scale correlation onto the Gaussian copula:
#' the Spearman map is exact for continuous margins, the Pearson map holds for
#' Gaussian margins, and pairs involving a binary margin use
#' prevalence-independent heuristics. A nonbinary discrete margin (a count or
#' an ordered category) is treated as continuous and its realized association
#' need not match the target; `add_integration()` warns when it sees one.
#' `"none"` passes a latent Gaussian-copula matrix through unchanged.
#'
#' @examples
#' \dontrun{
#' dat <- add_integration(
#'   dat,
#'   n_int = 64,
#'   x1 = distr(qnorm, mean = x1_mean, sd = x1_sd),
#'   x2 = distr(qbern, prob = x2_mean)
#' )
#' }
add_integration <- function(data, n_int = 64, cor = NULL,
                            cor_adjust = NULL, verbose = TRUE, ...) {
  ds <- list(...)
  .validate_integration_args(data, n_int, cor_adjust, verbose, ds)
  .validate_integration_distributions(ds, data)

  # Stan receives integration arrays without dimnames, so the third array
  # dimension must use the same covariate order as the IPD design matrix.
  ds <- ds[data$covariates]
  cov_names <- data$covariates
  n_cov <- length(ds)
  n_int <- as.integer(n_int)

  .warn_integration_size(n_int, n_cov)

  cor_info <- .resolve_integration_cor(
    data = data,
    cov_names = cov_names,
    n_cov = n_cov,
    cor = cor,
    cor_adjust = cor_adjust,
    ds = ds,
    verbose = verbose
  )

  u_cor <- .generate_copula_uniforms(n_int, n_cov, cor_info$copula_cor)
  integration_points <- .transform_integration_points(
    u_cor = u_cor,
    data = data,
    ds = ds,
    cov_names = cov_names,
    n_int = n_int,
    n_cov = n_cov
  )
  .warn_integration_vs_agd_moments(integration_points, data$agd$data, cov_names)

  data <- .attach_integration_points(
    data = data,
    X_int_array = integration_points,
    n_int = n_int,
    cor = cor_info$cor,
    copula_cor = cor_info$copula_cor,
    cor_adjust = cor_info$cor_adjust
  )

  mlumr_message(sprintf("Added %d integration points for AgD", n_int),
                verbose = verbose)
  data
}


#' Validate top-level integration arguments
#' @noRd
.validate_integration_args <- function(data, n_int, cor_adjust, verbose, ds) {
  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data()", call. = FALSE)
  }
  .validate_flag(verbose, "verbose")
  valid_cor_adjust <- c("spearman", "pearson", "none")
  invalid_cor_adjust <- !is.null(cor_adjust) &&
    (!is.character(cor_adjust) ||
       length(cor_adjust) != 1L ||
       is.na(cor_adjust) ||
       !cor_adjust %in% valid_cor_adjust)
  if (invalid_cor_adjust) {
    stop(sprintf("`cor_adjust` must be one of: %s",
                 paste(valid_cor_adjust, collapse = ", ")), call. = FALSE)
  }
  invalid_n_int <- !is.numeric(n_int) || length(n_int) != 1 ||
    !is.finite(n_int) || n_int <= 0 || n_int != floor(n_int)
  if (invalid_n_int) {
    stop("`n_int` should be a positive integer", call. = FALSE)
  }
  if (length(ds) == 0) {
    stop("No covariate distributions specified. Use distr() to specify distributions.",
         call. = FALSE)
  }
  invalid_ds <- any(vapply(ds, function(x) !inherits(x, "mlumr_distr"), logical(1))) ||
    is.null(names(ds)) || any(!nzchar(names(ds)))
  if (invalid_ds) {
    stop("Covariate distributions should be specified as named arguments using distr()",
         call. = FALSE)
  }
  if (anyDuplicated(names(ds)) > 0L) {
    duplicates <- unique(names(ds)[duplicated(names(ds))])
    stop(sprintf("Duplicate distribution specifications for: %s",
                 paste(duplicates, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate integration distributions against the combined data
#' @noRd
.validate_integration_distributions <- function(ds, data) {
  cov_names <- names(ds)
  if (anyDuplicated(cov_names) > 0L) {
    duplicates <- unique(cov_names[duplicated(cov_names)])
    stop(sprintf("Duplicate distribution specifications for: %s",
                 paste(duplicates, collapse = ", ")), call. = FALSE)
  }

  if (!all(cov_names %in% data$covariates)) {
    missing <- setdiff(cov_names, data$covariates)
    stop(sprintf("Unknown covariates: %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  if (length(cov_names) != length(data$covariates)) {
    missing <- setdiff(data$covariates, cov_names)
    stop(sprintf("Missing distribution specifications for: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}


#' Warn when the generated grid contradicts the declared AgD moments
#'
#' A hand-written `distr()` that ignores the AgD columns integrates the wrong
#' population silently. Only gross contradictions are flagged; suppress with
#' `options(mlumr.quiet_integration_moments = TRUE)`.
#' @noRd
.warn_integration_vs_agd_moments <- function(X_int_array, agd_data, cov_names) {
  if (isTRUE(getOption("mlumr.quiet_integration_moments", FALSE))) {
    return(invisible())
  }
  if (is.null(agd_data) || is.null(dim(X_int_array))) return(invisible())
  n_agd_rows <- dim(X_int_array)[1]
  issues <- character(0)
  for (j in seq_len(n_agd_rows)) {
    for (i in seq_along(cov_names)) {
      cov <- cov_names[[i]]
      mean_col <- paste0(cov, "_mean")
      if (!mean_col %in% names(agd_data)) next
      declared_mean <- suppressWarnings(as.numeric(agd_data[[mean_col]][j]))
      if (!is.finite(declared_mean)) next
      sd_col <- paste0(cov, "_sd")
      declared_sd <- if (sd_col %in% names(agd_data)) {
        suppressWarnings(as.numeric(agd_data[[sd_col]][j]))
      } else {
        NA_real_
      }
      grid_mean <- mean(X_int_array[j, , i])
      has_sd <- is.finite(declared_sd) && declared_sd > 0
      grid_sd <- if (has_sd) stats::sd(X_int_array[j, , i]) else NA_real_
      # Scale for a "gross contradiction": prefer the declared SD; fall back to
      # a fraction of |declared mean| for SD-less (binary) covariates.
      scale <- if (has_sd) declared_sd else max(0.1 * abs(declared_mean), 1e-6)
      mean_off <- abs(grid_mean - declared_mean) > scale &&
        abs(grid_mean - declared_mean) > 0.25 * abs(declared_mean)
      sd_off <- has_sd && abs(grid_sd - declared_sd) > 0.5 * declared_sd
      if (isTRUE(mean_off) || isTRUE(sd_off)) {
        sd_dec <- if (has_sd) sprintf("%.3g", declared_sd) else "NA"
        sd_grid <- if (has_sd) sprintf("%.3g", grid_sd) else "NA"
        fmt <- "%s (AgD row %d): declared mean=%.3g, sd=%s; grid mean=%.3g, sd=%s"
        one <- sprintf(fmt, cov, j, declared_mean, sd_dec, grid_mean, sd_grid)
        issues <- c(issues, one)
      }
    }
  }
  if (length(issues)) {
    msg <- paste0(
      "The integration grid contradicts the declared AgD moments for:\n  %s\n",
      "Check that each distr() references the AgD mean/SD columns. ",
      "Suppress with options(mlumr.quiet_integration_moments = TRUE)."
    )
    warning(sprintf(msg, paste(issues, collapse = "\n  ")), call. = FALSE)
  }
  invisible()
}


#' Warn when integration resolution is low for the covariate dimension
#' @noRd
.warn_integration_size <- function(n_int, n_cov) {
  min_recommended <- 2^(n_cov + 4)
  if (n_int < min_recommended) {
    warning(sprintf(
      paste0("n_int = %d may be insufficient for %d covariate(s). ",
             "Package heuristic: %d (= 2^(n_cov+4)). ",
             "Consider increasing n_int or using check_integration() to assess accuracy."),
      n_int, n_cov, min_recommended
    ), call. = FALSE)
  }
  invisible(TRUE)
}


#' Resolve raw and copula correlation matrices for integration
#' @noRd
.resolve_integration_cor <- function(data, cov_names, n_cov, cor, cor_adjust,
                                     ds, verbose) {
  if (n_cov == 1L) {
    list(cor = matrix(1), copula_cor = matrix(1), cor_adjust = "none")
  } else {
    if (is.null(cor)) {
      mlumr_message("Computing correlation matrix from IPD...", verbose = verbose)
      if (is.null(cor_adjust)) cor_adjust <- "spearman"
      cor_method <- if (cor_adjust == "none") "pearson" else cor_adjust
      cor <- suppressWarnings(stats::cor(
        data$ipd$data[, cov_names],
        method = cor_method,
        use = "complete.obs"
      ))
      .validate_computed_integration_cor(cor)
    } else {
      cor <- .validate_integration_cor(cor, n_cov, cov_names = cov_names)
      if (is.null(cor_adjust)) cor_adjust <- "pearson"
    }

    dtypes <- do.call(
      get_distribution_type,
      c(ds, list(data = utils::head(data$agd$data)))
    )
    if (identical(cor_adjust, "pearson")) {
      non_gaussian <- dtypes == "continuous" &
        vapply(ds, function(d) !identical(d$qfun_name, "qnorm"), logical(1))
      off_diagonal <- cor
      diag(off_diagonal) <- 0
      affected <- non_gaussian &
        apply(abs(off_diagonal) > sqrt(.Machine$double.eps), 1, any)
      if (any(affected)) {
        stop("`cor_adjust = \"pearson\"` cannot be used with non-Gaussian ",
             "continuous margins: a covariate-scale Pearson correlation is ",
             "not the Gaussian-copula correlation for ",
             paste(cov_names[affected], collapse = ", "), ". Supply a ",
             "Spearman correlation matrix with `cor_adjust = \"spearman\"`, ",
             "use Gaussian margins for an observed Pearson matrix, or supply ",
             "a latent Gaussian-copula matrix with `cor_adjust = \"none\"`.",
             call. = FALSE)
      }
    }
    .warn_discrete_copula(dtypes, cov_names, cor_adjust)
    copula_cor <- .adjust_integration_cor(cor, cor_adjust, dtypes)
    copula_cor <- .ensure_positive_definite_cor(copula_cor)

    list(cor = cor, copula_cor = copula_cor, cor_adjust = cor_adjust)
  }
}


#' Warn that the copula correction does not cover nonbinary discrete margins
#'
#' The Spearman and Pearson corrections cover continuous margins exactly and
#' binary margins heuristically. A count or ordinal margin goes through the
#' continuous branch, so its realized association need not match the target.
#'
#' @param dtypes Distribution types from [get_distribution_type()].
#' @param cov_names Covariate names, same order as `dtypes`.
#' @param cor_adjust The adjustment method in force.
#' @return `TRUE` invisibly if a warning was issued, `FALSE` otherwise.
#' @noRd
.warn_discrete_copula <- function(dtypes, cov_names, cor_adjust) {
  if (identical(cor_adjust, "none")) return(invisible(FALSE))
  hit <- which(dtypes == "discrete")
  if (!length(hit)) return(invisible(FALSE))
  nms <- if (length(cov_names) == length(dtypes)) cov_names[hit] else hit
  msg <- paste0(
    "Covariate(s) %s have a nonbinary discrete marginal (a count or ordered ",
    "category). The `cor_adjust = \"%s\"` copula correction covers ",
    "continuous margins exactly and binary margins heuristically, so the ",
    "realized association for these need not match the target. Check it with ",
    "check_integration(), passing the same `cor`."
  )
  warning(sprintf(msg, paste(nms, collapse = ", "), cor_adjust), call. = FALSE)
  invisible(TRUE)
}


#' Validate a user-supplied integration correlation matrix
#' @noRd
.validate_integration_cor <- function(cor, n_cov, cov_names = NULL) {
  cor <- as.matrix(cor)
  if (!is.numeric(cor) || nrow(cor) != n_cov || ncol(cor) != n_cov) {
    stop("Correlation matrix dimensions don't match number of covariates",
         call. = FALSE)
  }
  if (!is.null(cov_names)) {
    row_names <- rownames(cor)
    col_names <- colnames(cor)
    has_dimnames <- !is.null(row_names) || !is.null(col_names)
    if (has_dimnames) {
      dimnames_match <- !is.null(row_names) &&
        !is.null(col_names) &&
        setequal(row_names, cov_names) &&
        setequal(col_names, cov_names)
      if (!dimnames_match) {
        stop("`cor` dimnames must match covariate names when provided",
             call. = FALSE)
      }
      cor <- cor[cov_names, cov_names, drop = FALSE]
    }
  }
  valid <- all(is.finite(cor)) &&
    isSymmetric(cor) &&
    max(abs(diag(cor) - 1)) <= sqrt(.Machine$double.eps) &&
    all(eigen(cor, symmetric = TRUE)$values > 0)

  if (!valid) {
    stop("`cor` must be a valid correlation matrix", call. = FALSE)
  }
  cor
}


#' Validate an IPD-derived correlation matrix before copula adjustment
#' @noRd
.validate_computed_integration_cor <- function(cor) {
  if (any(!is.finite(cor))) {
    stop("Computed IPD correlation matrix contains non-finite values. ",
         "Check for constant or missing covariates, or provide `cor` manually.",
         call. = FALSE)
  }
  if (!isSymmetric(cor) ||
        max(abs(diag(cor) - 1)) > sqrt(.Machine$double.eps)) {
    stop("Computed IPD correlation matrix is invalid. Provide `cor` manually.",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Adjust correlations to Gaussian-copula scale
#' @noRd
.adjust_integration_cor <- function(cor, cor_adjust, dtypes) {
  if (cor_adjust == "spearman") {
    cor_adjust_spearman(cor, types = dtypes)
  } else if (cor_adjust == "pearson") {
    cor_adjust_pearson(cor, types = dtypes)
  } else {
    cor
  }
}


#' Ensure adjusted integration correlation is positive definite
#' @noRd
.ensure_positive_definite_cor <- function(copula_cor) {
  eigen_tol <- .Machine$double.eps * max(dim(copula_cor)) * 100
  if (all(eigen(copula_cor, symmetric = TRUE)$values > eigen_tol)) {
    return(copula_cor)
  }
  warning("Adjusted correlation matrix not positive definite; applying nearPD correction.",
          call. = FALSE)
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required to repair a correlation matrix that ",
         "is not positive definite.", call. = FALSE)
  }
  result <- as.matrix(Matrix::nearPD(copula_cor, corr = TRUE)$mat)
  # nearPD can leave the smallest eigenvalue marginally negative.
  if (!all(eigen(result, symmetric = TRUE)$values > eigen_tol)) {
    stop("Could not produce a positive-definite integration correlation matrix ",
         "after nearPD correction. Supply a valid positive-definite `cor` or ",
         "reduce the correlation magnitudes.", call. = FALSE)
  }
  result
}


#' Generate correlated uniform quasi-Monte Carlo points
#'
#' Sobol points pushed through a Gaussian copula: normal scores, multiplied by
#' the Cholesky factor of `copula_cor`, mapped back to uniforms. The three
#' steps together are the inverse Rosenblatt transform of that copula.
#' @noRd
.generate_copula_uniforms <- function(n_int, n_cov, copula_cor) {
  u <- as.matrix(randtoolbox::sobol(n = n_int, dim = n_cov))
  if (n_cov == 1L) return(u)
  stats::pnorm(stats::qnorm(u) %*% chol(copula_cor))
}


#' Transform uniform integration points to covariate scales
#' @noRd
.transform_integration_points <- function(u_cor, data, ds, cov_names, n_int,
                                          n_cov) {
  agd_data <- data$agd$data
  n_agd_rows <- nrow(agd_data)

  X_int_array <- array(NA_real_, dim = c(n_agd_rows, n_int, n_cov))
  dimnames(X_int_array)[[3]] <- cov_names

  for (j in 1:n_agd_rows) {
    row_data <- as.list(agd_data[j, , drop = FALSE])
    for (i in seq_along(cov_names)) {
      cov <- cov_names[i]
      u_i <- as.vector(u_cor[, i])
      X_int_array[j, , i] <- eval_distr(ds[[cov]], u_i, row_data)
    }
  }

  if (any(is.na(X_int_array) | is.infinite(X_int_array) | is.nan(X_int_array))) {
    stop("Invalid integration points generated. Check covariate distribution parameters.",
         call. = FALSE)
  }
  X_int_array
}


#' Attach generated integration points to an mlumr_data object
#' @noRd
.attach_integration_points <- function(data, X_int_array, n_int, cor, copula_cor,
                                       cor_adjust) {
  data$integration_points <- X_int_array
  data$n_int <- n_int
  data$int_cor <- cor
  data$int_copula_cor <- copula_cor
  data$int_cor_adjust <- cor_adjust
  data$has_integration <- TRUE
  data
}


#' Expand integration points into a long-format data frame
#'
#' @param data An `mlumr_data` object with integration points
#'
#' @return A data frame with columns for each covariate plus `.int_id` and `.agd_row`
#' @export
unnest_integration <- function(data) {

  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be an mlumr_data object", call. = FALSE)
  }
  if (!data$has_integration) {
    stop("No integration points found. Use add_integration() first.", call. = FALSE)
  }

  X_int <- data$integration_points
  dims <- dim(X_int)
  n_agd <- dims[1]
  n_int <- dims[2]
  cov_names <- dimnames(X_int)[[3]]

  out_list <- vector("list", n_agd)
  for (i in 1:n_agd) {
    mat <- X_int[i, , , drop = FALSE]
    dim(mat) <- dims[2:3]
    colnames(mat) <- cov_names
    df <- as.data.frame(mat)
    df$.int_id <- seq_len(n_int)
    df$.agd_row <- i
    out_list[[i]] <- df
  }

  do.call(rbind, out_list)
}


#' Check integration point adequacy
#'
#' Compare integration results at the current `n_int` against a doubled
#' resolution to assess numerical accuracy. Large discrepancies indicate
#' that `n_int` should be increased. Because the Sobol sequence is nested
#' (the doubled set contains the current set), this current-vs-doubled
#' difference is a convergence heuristic, not an error bound. Agreement between
#' the two grids does not establish accuracy for rare discrete margins or for a
#' final treatment-effect estimand.
#'
#' @param data An `mlumr_data` object with integration points
#' @param ... Distribution specifications (same as passed to
#'   [add_integration()])
#' @param cor Correlation matrix (same as passed to [add_integration()])
#' @param cor_adjust Adjustment method (same as passed to [add_integration()])
#' @param check_joint If `TRUE` (default), also compare pairwise correlation
#'   matrices between the current and doubled `n_int`, and the maximum
#'   per-AgD-row absolute deviation from the user-supplied `cor`. The
#'   pairwise comparison catches cases where marginals converge but joint
#'   dependence structure does not (rare in practice for QMC with sensible
#'   `cor_adjust` but worth flagging when `n_int` is small).
#'
#' @return A list with `marginals`, a data frame of grid means and SDs at the
#'   current and doubled `n_int` against the declared targets, and `verdict`,
#'   whose entries are `"stable"` or `"close"` when a comparison met the
#'   heuristic, `"review"` when it did not, `"partial"` when the measured
#'   correlation pairs passed but some pair could not be measured, and
#'   `"unavailable"` when there was nothing finite to compare (a latent
#'   matrix under `cor_adjust = "none"` is never compared). With
#'   `check_joint = TRUE` and two or more covariates it also holds
#'   `correlations`, the pairwise correlations per AgD row on both grids, and
#'   `correlation_pairs`, which counts the pairs measured and names the rest.
#'   A binary margin's target SD is `sqrt(p * (1 - p))` from the declared
#'   mean, and grid SDs are population SDs.
#' @param verbose Logical; if `FALSE`, suppresses printed diagnostic messages.
#' @export
check_integration <- function(data, ..., cor = NULL, cor_adjust = NULL,
                              check_joint = TRUE, verbose = TRUE) {

  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be an mlumr_data object", call. = FALSE)
  }
  .validate_flag(verbose, "verbose")
  .validate_flag(check_joint, "check_joint")
  if (!data$has_integration) {
    stop("No integration points found. Use add_integration() first.", call. = FALSE)
  }

  n_int_orig <- data$n_int
  n_int_double <- n_int_orig * 2

  # Compute stats at current resolution
  X_orig <- data$integration_points
  cov_names <- dimnames(X_orig)[[3]]
  n_agd <- dim(X_orig)[1]

  stats_orig <- .int_stats(X_orig, cov_names, n_agd)

  # Re-run at doubled resolution (temporarily)
  data_copy <- data
  data_copy$has_integration <- FALSE
  # Reuse the correlation the original integration used unless overridden.
  data_doubled <- suppressMessages(suppressWarnings(
    add_integration(data_copy, n_int = n_int_double, cor = cor %||% data$int_cor,
                    cor_adjust = cor_adjust %||% data$int_cor_adjust,
                    verbose = FALSE, ...)
  ))
  X_double <- data_doubled$integration_points
  stats_double <- .int_stats(X_double, cov_names, n_agd)

  # The mean denominator includes the SD so a near-zero target mean does not
  # inflate the relative difference.
  rel_diff_mean <- abs(stats_orig$mean - stats_double$mean) /
    (abs(stats_double$mean) + abs(stats_double$sd) + 1e-8)
  rel_diff_sd <- abs(stats_orig$sd - stats_double$sd) /
    (abs(stats_double$sd) + 1e-8)

  agd <- data$agd$data
  # The target SD comes from the declared distribution: sqrt(m * (1 - m)) for
  # a binary margin, whatever `_sd` column the AgD carries, and the declared
  # SD column otherwise.
  dtypes <- do.call(
    get_distribution_type,
    c(list(...), list(data = utils::head(agd)))
  )
  target_mean <- target_sd <- numeric(nrow(stats_orig))
  for (i in seq_len(nrow(stats_orig))) {
    cov <- stats_orig$covariate[i]
    row <- stats_orig$agd_row[i]
    target_mean[i] <- as.numeric(agd[[paste0(cov, "_mean")]][row])
    sd_col <- paste0(cov, "_sd")
    is_binary <- isTRUE(unname(dtypes[cov]) == "binary")
    target_sd[i] <- if (is_binary) {
      if (isTRUE(target_mean[i] >= 0 && target_mean[i] <= 1)) {
        sqrt(target_mean[i] * (1 - target_mean[i]))
      } else {
        NA_real_
      }
    } else if (sd_col %in% names(agd)) {
      as.numeric(agd[[sd_col]][row])
    } else {
      NA_real_
    }
  }
  target_scale <- abs(target_mean) +
    ifelse(is.finite(target_sd), abs(target_sd), 0) + 1e-8
  target_diff_mean <- abs(stats_orig$mean - target_mean) / target_scale
  target_diff_sd <- abs(stats_orig$sd - target_sd) /
    (abs(target_sd) + 1e-8)

  result <- data.frame(
    covariate = stats_orig$covariate,
    agd_row = stats_orig$agd_row,
    mean_current = round(stats_orig$mean, 6),
    mean_doubled = round(stats_double$mean, 6),
    rel_diff_mean = round(rel_diff_mean, 6),
    mean_target = round(target_mean, 6),
    rel_diff_mean_target = round(target_diff_mean, 6),
    sd_current = round(stats_orig$sd, 6),
    sd_doubled = round(stats_double$sd, 6),
    rel_diff_sd = round(rel_diff_sd, 6),
    sd_target = round(target_sd, 6),
    rel_diff_sd_target = round(target_diff_sd, 6),
    stringsAsFactors = FALSE
  )

  max_diff <- .max_finite(c(rel_diff_mean, rel_diff_sd))
  max_target_diff <- .max_finite(c(target_diff_mean, target_diff_sd))

  if (verbose) {
    cat(sprintf("Integration check: n_int = %d vs %d\n", n_int_orig, n_int_double))
    if (is.na(max_diff)) {
      cat("Resolution heuristic: not available (no finite comparison).\n")
    } else {
      cat(sprintf("Resolution heuristic, max relative difference: %.4f\n", max_diff))
      if (max_diff > 0.05) {
        cat("Warning: >5% marginal relative difference. Increase n_int.\n")
      } else if (max_diff > 0.01) {
        cat("Caution: 1-5% marginal relative difference. Consider increasing n_int.\n")
      } else {
        cat("Resolution stable within the package's 1% heuristic.\n")
      }
    }
    if (is.na(max_target_diff)) {
      cat("Declared-target fidelity: not available; the AgD declares no",
          "comparable moments for these covariates.\n")
    } else {
      cat(sprintf("Declared-target fidelity, max relative difference: %.4f\n",
                  max_target_diff))
      if (max_target_diff > 0.05) {
        cat("Warning: grid moments differ from declared AgD moments by >5%.\n")
      } else if (max_target_diff > 0.01) {
        cat("Caution: grid moments differ from declared AgD moments by 1-5%.\n")
      } else {
        cat("Grid moments agree with declared AgD moments within the package's 1% heuristic.\n")
      }
    }
  }

  # A verdict needs a comparison: an all-NA maximum is "unavailable".
  out <- list(
    marginals = result,
    verdict = list(
      resolution = .moment_verdict(max_diff, 0.01, "stable"),
      target_moments = .moment_verdict(max_target_diff, 0.01, "close")
    )
  )

  if (isTRUE(check_joint) && length(cov_names) >= 2L) {
    # Measure the realized correlation on the method that defined the target,
    # and put a caller-supplied matrix in covariate order first.
    cor_target <- if (is.null(cor)) {
      data$int_cor
    } else {
      .validate_integration_cor(cor, length(cov_names), cov_names = cov_names)
    }
    target_method <- cor_adjust %||% data$int_cor_adjust %||% "pearson"
    # A latent matrix under `cor_adjust = "none"` is not what the realized
    # covariate-scale correlation estimates, so it is not compared.
    latent_target <- identical(target_method, "none")
    if (latent_target) {
      target_method <- "pearson"
      cor_target <- NULL
    }
    cor_result <- .int_cor_stats(X_orig, X_double, cov_names, n_agd,
                                 cor_target = cor_target,
                                 cor_method = target_method)
    # The maxima run over the pairs with a correlation to realize; the rest
    # are counted and named.
    pairs <- .int_cor_pair_status(cor_result$diff, stats_orig, target_sd)
    applicable <- cor_result$diff[pairs$applicable, , drop = FALSE]
    max_cor_diff <- .max_finite(applicable$abs_diff)
    max_target_cor_diff <- if (is.null(cor_target)) NA_real_ else
      .max_finite(applicable$abs_diff_target)
    if (verbose) {
      if (is.na(max_cor_diff)) {
        cat("Joint resolution: not available (no finite comparison).\n")
      } else {
        cat(sprintf("Joint: max |cor(current) - cor(doubled)|: %.4f\n",
                    max_cor_diff))
        if (max_cor_diff > 0.05) {
          cat("Warning: pairwise correlations differ by > 0.05 between resolutions.\n")
        } else {
          cat("Joint resolution stable within the package's 0.05 heuristic.\n")
        }
      }
      if (latent_target) {
        cat("Target correlation: not compared. `cor_adjust = \"none\"` declares",
            "a latent Gaussian-copula matrix, which the realized covariate-scale",
            "correlation does not estimate.\n")
      } else if (!is.null(cor_target) && !is.na(max_target_cor_diff)) {
        cat(sprintf("Target (%s): max |cor(doubled) - cor_target|: %.4f\n",
                    target_method, max_target_cor_diff))
      } else if (!is.null(cor_target)) {
        cat("Target correlation: not available (no finite comparison).\n")
      }
      if (nrow(pairs$omitted)) {
        cat(sprintf(paste0("Pairs measured: %d of %d on the doubled grid, %d ",
                           "of %d between resolutions. Not measured: %s. ",
                           "A maximum above is over the measured pairs ",
                           "only.\n"),
                    pairs$measured, pairs$expected,
                    pairs$measured_resolution, pairs$expected,
                    paste(sprintf("%s (row %d, %s)", pairs$omitted$pair,
                                  pairs$omitted$agd_row,
                                  pairs$omitted$reason),
                          collapse = "; ")))
      }
      if (nrow(pairs$not_applicable)) {
        cat(sprintf(paste0("Pairs with a margin declared without variance, ",
                           "which have no correlation to realize: %s.\n"),
                    paste(sprintf("%s (row %d)", pairs$not_applicable$pair,
                                  pairs$not_applicable$agd_row),
                          collapse = "; ")))
      }
    }
    # `partial`: the measured pairs passed and some pair was not measured.
    qualify <- function(verdict, pass, measured) {
      if (identical(verdict, pass) && measured < pairs$expected) {
        "partial"
      } else {
        verdict
      }
    }
    out$verdict$target_correlation <- if (is.null(cor_target)) {
      "unavailable"
    } else {
      qualify(.moment_verdict(max_target_cor_diff, 0.05, "close"), "close",
              pairs$measured)
    }
    out$verdict$resolution_correlation <- qualify(
      .moment_verdict(max_cor_diff, 0.05, "stable"), "stable",
      pairs$measured_resolution
    )
    out$correlations <- cor_result$diff
    out$correlation_pairs <- pairs
  }

  invisible(out)
}


#' Pairwise-correlation diagnostics for integration points
#' @noRd
.int_cor_stats <- function(X_orig, X_double, cov_names, n_agd, cor_target = NULL,
                           cor_method = "pearson") {
  K <- length(cov_names)
  pairs <- utils::combn(seq_len(K), 2, simplify = FALSE)
  rows <- vector("list", length(pairs) * n_agd)
  idx <- 1L
  for (k in seq_len(n_agd)) {
    Xo <- X_orig[k, , , drop = FALSE]
    Xd <- X_double[k, , , drop = FALSE]
    dim(Xo) <- dim(X_orig)[2:3]
    dim(Xd) <- dim(X_double)[2:3]
    colnames(Xo) <- cov_names
    colnames(Xd) <- cov_names
    # Measured on the method the target was measured on.
    cor_o <- suppressWarnings(stats::cor(Xo, method = cor_method))
    cor_d <- suppressWarnings(stats::cor(Xd, method = cor_method))
    for (ij in pairs) {
      i <- ij[[1L]]
      j <- ij[[2L]]
      rho_o <- cor_o[i, j]
      rho_d <- cor_d[i, j]
      rho_t <- if (!is.null(cor_target)) cor_target[i, j] else NA_real_
      rows[[idx]] <- data.frame(
        agd_row = k,
        # `pair` is a label; `covariate_1` and `covariate_2` identify the members.
        covariate_1 = cov_names[i],
        covariate_2 = cov_names[j],
        pair = sprintf("%s~%s", cov_names[i], cov_names[j]),
        cor_method = cor_method,
        cor_current = round(rho_o, 4),
        cor_doubled = round(rho_d, 4),
        cor_target = round(rho_t, 4),
        abs_diff = round(abs(rho_o - rho_d), 4),
        abs_diff_target = round(abs(rho_d - rho_t), 4),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  list(diff = do.call(rbind, rows))
}


#' Which correlation pairs were measured, and why the others were not
#'
#' @param diff The pair table from [.int_cor_stats()].
#' @param stats The marginal statistics of the current grid.
#' @param target_sd Declared target SDs, one per row of `stats`.
#' @return List with `expected` (pairs that have a correlation to realize),
#'   `measured` and `measured_resolution` (how many had a finite correlation on
#'   the doubled grid, and on both grids), `omitted` (the expected pairs that
#'   fell short, with a reason), `not_applicable` (pairs in which a margin is
#'   declared with no variance) and `applicable`, the logical vector the
#'   maxima are taken over.
#' @noRd
.int_cor_pair_status <- function(diff, stats, target_sd) {
  degenerate <- vapply(seq_len(nrow(diff)), function(i) {
    members <- c(diff$covariate_1[i], diff$covariate_2[i])
    rows <- stats$covariate %in% members & stats$agd_row == diff$agd_row[i]
    any(is.finite(target_sd[rows]) & target_sd[rows] == 0)
  }, logical(1))
  applicable <- !degenerate
  measured <- applicable & is.finite(diff$cor_doubled)
  measured_resolution <- applicable & is.finite(diff$abs_diff)
  short <- applicable & !(measured & measured_resolution)
  omitted <- diff[short, c("agd_row", "pair"), drop = FALSE]
  omitted$reason <- ifelse(measured[short], "constant_on_current_grid",
                           "constant_on_grid")
  rownames(omitted) <- NULL
  not_applicable <- diff[degenerate, c("agd_row", "pair"), drop = FALSE]
  rownames(not_applicable) <- NULL
  list(expected = sum(applicable), measured = sum(measured),
       measured_resolution = sum(measured_resolution), omitted = omitted,
       not_applicable = not_applicable, applicable = applicable)
}


#' Compute summary statistics for integration points
#'
#' The SD is the population one: the grid represents a distribution rather
#' than sampling it, and a sample SD carries a `sqrt(m / (m - 1))` factor
#' that no target shares.
#' @noRd
.int_stats <- function(X_int, cov_names, n_agd) {
  rows <- vector("list", n_agd * length(cov_names))
  idx <- 1
  for (k in seq_len(n_agd)) {
    for (j in seq_along(cov_names)) {
      vals <- X_int[k, , j]
      rows[[idx]] <- data.frame(
        covariate = cov_names[j], agd_row = k,
        mean = mean(vals), sd = sqrt(mean((vals - mean(vals))^2)),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  do.call(rbind, rows)
}


#' Largest finite value, or NA when there is none
#'
#' `max(x, na.rm = TRUE)` returns `-Inf` for an all-missing vector.
#' @noRd
.max_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else max(x)
}


#' Turn a difference into a verdict, keeping "not measured" distinct from "close"
#' @noRd
.moment_verdict <- function(value, threshold, pass) {
  if (is.na(value)) {
    "unavailable"
  } else if (value <= threshold) {
    pass
  } else {
    "review"
  }
}

#' Validate and resolve link function for a given family
#'
#' Checks that `link` is valid for `family` and returns the resolved link name
#' plus an integer code for Stan. `family` is the canonical name the data
#' setup records: `"binomial"`, `"normal"` or `"poisson"` from [set_ipd()] and
#' [set_agd()], or `"survival"` from [set_ipd()] with [set_agd_surv()].
#'
#' The likelihood/link matrix is:
#'
#' \tabular{lll}{
#'   \strong{Family} \tab \strong{Likelihoods} \tab \strong{Link functions} \cr
#'   binomial \tab bernoulli (IPD), binomial (AgD) \tab logit, probit, cloglog \cr
#'   poisson  \tab poisson                         \tab log                    \cr
#'   normal   \tab normal                          \tab identity, log          \cr
#'   survival \tab parametric or M-spline hazard   \tab log (the only one)     \cr
#' }
#'
#' @param family Character: `"binomial"`, `"normal"`, `"poisson"` or
#'   `"survival"`.
#' @param link Character or `NULL`. If `NULL`, uses default for family.
#' @return List with components:
#' \describe{
#'   \item{family}{Canonical family name (e.g. `"binomial"`)}
#'   \item{link}{Resolved link name (e.g. `"probit"`)}
#'   \item{code}{Integer code for Stan data block}
#' }
#' @noRd
check_link <- function(family, link = NULL) {

  family <- .validate_link_string(family, "family")
  family <- tolower(family)

  if (!family %in% names(family_config)) {
    stop(sprintf("Unknown family '%s'. Valid: %s",
                 family, paste(names(family_config), collapse = ", ")),
         call. = FALSE)
  }

  cfg <- family_config[[family]]

  if (is.null(link)) {
    link <- cfg$link_default
  } else {
    link <- .validate_link_string(link, "link")
  }
  link <- tolower(link)

  if (!link %in% cfg$links) {
    stop(sprintf("Link '%s' is not valid for family '%s'. Valid: %s",
                 link, family, paste(cfg$links, collapse = ", ")),
         call. = FALSE)
  }

  list(
    family = family,
    link = link,
    code = as.integer(match(link, cfg$links))
  )
}

#' Inverse link function (linear predictor -> response scale)
#' @param x Numeric vector
#' @param link Character: link function name
#' @return Numeric vector on response scale
#' @importFrom stats pnorm
#' @noRd
inverse_link <- function(x, link = c("identity", "log", "logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(x, "x")
  switch(link,
    identity = x,
    log      = exp(x),
    logit    = plogis(x),
    probit   = pnorm(x),
    cloglog  = -expm1(-exp(x))
  )
}


#' Stable log probabilities for binary inverse links
#'
#' Returns `log(P(Y = 1))` and `log(P(Y = 0))` without first rounding either
#' probability to zero or one. This is used when a marginal contrast remains
#' finite even though its natural-scale probabilities are outside double
#' precision.
#' @noRd
.binary_log_probs <- function(eta, link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(eta, "eta")

  if (link == "logit") {
    return(list(
      event = -(pmax(-eta, 0) + log1p(exp(-abs(eta)))),
      nonevent = -(pmax(eta, 0) + log1p(exp(-abs(eta))))
    ))
  }
  if (link == "probit") {
    return(list(
      event = stats::pnorm(eta, log.p = TRUE),
      nonevent = stats::pnorm(eta, lower.tail = FALSE, log.p = TRUE)
    ))
  }

  exp_eta <- exp(eta)
  log_event <- log(-expm1(-exp_eta))
  # Missing input propagates as missing output.
  small <- !is.na(eta) & eta < -18
  if (any(small)) {
    # Series for log(1 - exp(-x)) with x = exp(eta), exact where the direct
    # form loses its leading digits.
    x <- exp_eta[small]
    log_event[small] <- eta[small] + log1p(-x / 2 + x^2 / 6)
  }
  list(event = log_event, nonevent = -exp_eta)
}


#' Binary link of a marginal probability represented on both log tails
#' @noRd
.binary_link_from_logs <- function(log_event, log_nonevent,
                                   link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  if (link == "logit") return(log_event - log_nonevent)
  if (link == "probit") {
    # Invert through the smaller tail, which carries the digits.
    out <- rep(NA_real_, length(log_event))
    known <- !is.na(log_event) & !is.na(log_nonevent)
    lower <- known & log_event <= log(0.5)
    upper <- known & !lower
    out[lower] <- stats::qnorm(log_event[lower], log.p = TRUE)
    out[upper] <- stats::qnorm(log_nonevent[upper], lower.tail = FALSE,
                               log.p = TRUE)
    return(out)
  }

  out <- log(-log_nonevent)
  # For a tiny event probability cloglog(p) is log(p) to double precision,
  # where log(1 - p) has rounded to zero.
  small <- !is.na(log_event) & log_event < -18
  out[small] <- log_event[small]
  out
}


#' Elementwise log(exp(x) + exp(y))
#' @noRd
.logspace_add <- function(x, y) {
  out <- pmax(x, y)
  finite <- is.finite(out)
  out[finite] <- out[finite] +
    log1p(exp(-abs(x[finite] - y[finite])))
  both_neg_inf <- is.infinite(x) & x < 0 & is.infinite(y) & y < 0
  out[both_neg_inf] <- -Inf
  out
}


#' Weighted log mean of exponentiated values
#' @noRd
.weighted_log_mean_exp <- function(x, weights = rep(1, length(x))) {
  if (length(x) != length(weights) || any(!is.finite(weights)) ||
        any(weights < 0) || !any(weights > 0)) {
    stop("`weights` must be finite, non-negative, and match `x`.", call. = FALSE)
  }
  # A zero weight contributes nothing, but log(0) is -Inf and x + -Inf is NaN
  # for an infinite x, which would poison the maximum. Drop them first.
  keep <- weights > 0
  x <- x[keep]
  weights <- weights[keep]
  log_weights <- log(weights)
  # Shift by the largest `x` before adding the weights: a log probability can
  # be so large that adding log(w) rounds it away.
  m_x <- max(x)
  if (is.infinite(m_x)) return(m_x)
  # Identical values have the maximum as their mean, exactly.
  if (all(x == m_x)) return(m_x)
  # Near a probability of one the shift throws away the correction that is
  # the answer; `log1p(mean(expm1(x)))` keeps it and is used where it is
  # also safe, `m_x` in (-1, 0].
  if (m_x <= 0 && m_x > -1) {
    # Normalized by the largest weight so the sum neither overflows nor
    # underflows.
    scaled <- weights / max(weights)
    mean_expm1 <- sum(scaled * expm1(x)) / sum(scaled)
    # Only near one; past -0.5 the shifted form is the accurate one.
    if (mean_expm1 > -0.5) return(log1p(mean_expm1))
  }
  z <- (x - m_x) + log_weights
  m_num <- max(z)
  m_den <- max(log_weights)
  log_num <- m_num + log(sum(exp(z - m_num)))
  log_den <- m_den + log(sum(exp(log_weights - m_den)))
  m_x + (log_num - log_den)
}


#' Normalize non-negative weights without overflowing their sum
#' @noRd
.normalize_weights <- function(weights) {
  if (!length(weights) || any(!is.finite(weights)) || any(weights < 0) ||
        !any(weights > 0)) {
    stop("`weights` must be finite, non-negative, and include a positive value.",
         call. = FALSE)
  }
  scaled <- weights / max(weights)
  scaled / sum(scaled)
}


#' Stable difference exp(log_x) - exp(log_y)
#'
#' Cancellation happens before the return to the natural scale. Equal logs
#' return exactly `0`, two `+Inf` logs return `NaN`, arguments recycle and
#' `NA` propagates.
#' @noRd
.exp_difference_logs <- function(log_x, log_y) {
  n <- max(length(log_x), length(log_y))
  if (length(log_x) != n) log_x <- rep_len(log_x, n)
  if (length(log_y) != n) log_y <- rep_len(log_y, n)
  out <- rep(NaN, n)
  known <- !is.na(log_x) & !is.na(log_y)
  # Two positive infinities have no difference; equal finite or -Inf logs do.
  both_unbounded <- known & log_x == Inf & log_y == Inf
  same <- known & !both_unbounded & log_x == log_y
  x_larger <- known & !same & log_x > log_y
  y_larger <- known & !same & log_y > log_x

  out[same] <- 0
  out[x_larger] <- exp(
    log_x[x_larger] + log(-expm1(log_y[x_larger] - log_x[x_larger]))
  )
  out[y_larger] <- -exp(
    log_y[y_larger] + log(-expm1(log_x[y_larger] - log_y[y_larger]))
  )
  out
}

#' Link function (response scale -> linear predictor)
#' @param x Numeric vector on response scale
#' @param link Character: link function name
#' @return Numeric vector on linear predictor scale
#' @noRd
link_fun <- function(x, link = c("identity", "log", "logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(x, "x")
  eps <- .Machine$double.eps
  p <- pmin(pmax(x, eps), 1 - eps)
  switch(link,
    identity = x,
    log      = log(pmax(x, eps)),
    logit    = qlogis(p),
    probit   = qnorm(p),
    cloglog  = log(-log1p(-p))
  )
}

#' Apply a boundary-only binomial continuity correction
#'
#' At zero or all events, uses the pseudo-count estimate
#' `(r + min_count) / (n + 2 * min_count)`. Interior probabilities are
#' unchanged.
#' @noRd
bound_probability <- function(p, n, min_count = 0.5) {
  .validate_numeric_vector(p, "p")
  .validate_positive_numeric(n, "n")
  .validate_positive_numeric(min_count, "min_count")
  if (any(2 * min_count > n)) {
    stop("`min_count` must be no larger than n / 2.", call. = FALSE)
  }
  # A probability outside [0, 1] is an upstream error, not a boundary arm.
  if (any(!is.na(p) & (p < 0 | p > 1))) {
    stop("`p` must lie in [0, 1].", call. = FALSE)
  }
  # ifelse() sizes its result by the test, so recycle first.
  len <- max(length(p), length(n))
  p <- rep_len(p, len)
  n <- rep_len(n, len)
  lower <- min_count / (n + 2 * min_count)
  upper <- (n + min_count) / (n + 2 * min_count)
  ifelse(p == 0, lower, ifelse(p == 1, upper, p))
}

#' Exact interval for a directly observed binomial proportion
#'
#' Clopper and Pearson's interval, as `stats::binom.test()` reports it but
#' without the integer check: beta quantiles, with the lower bound at 0 when
#' the count is 0 and the upper bound at 1 when the count equals `n`. For
#' integer counts its coverage is at least nominal for every true
#' probability, which the bounded Wald interval it replaced lacked; a
#' fractional count has no such guarantee.
#'
#' @param r Event count, in `[0, n]`.
#' @param n Number of trials, positive.
#' @param conf_level Confidence level.
#' @return List with `lower` and `upper`.
#' @noRd
.clopper_pearson_interval <- function(r, n, conf_level) {
  .validate_numeric_vector(r, "r")
  .validate_positive_numeric(n, "n")
  if (any(r < 0 | r > n)) {
    stop("`r` must lie in [0, n].", call. = FALSE)
  }
  alpha <- 1 - conf_level
  len <- max(length(r), length(n))
  r <- rep_len(r, len)
  n <- rep_len(n, len)
  # The boundary bound is set outright, never asked of a shape-zero beta.
  lower <- numeric(len)
  upper <- rep(1, len)
  inner <- r > 0
  lower[inner] <- stats::qbeta(alpha / 2, r[inner], n[inner] - r[inner] + 1)
  inner <- r < n
  upper[inner] <- stats::qbeta(1 - alpha / 2, r[inner] + 1, n[inner] - r[inner])
  list(lower = lower, upper = upper)
}

#' Exact interval for a directly observed Poisson rate
#'
#' Garwood's interval, as `stats::poisson.test()` reports it: gamma quantiles
#' over the exposure, with the lower bound at 0 when the count is 0. Coverage
#' is at least nominal for every true rate.
#'
#' @param x Event count, non-negative.
#' @param exposure Total exposure, positive.
#' @param conf_level Confidence level.
#' @return List with `lower` and `upper`.
#' @noRd
.garwood_interval <- function(x, exposure, conf_level) {
  .validate_numeric_vector(x, "x")
  .validate_positive_numeric(exposure, "exposure")
  if (any(x < 0)) {
    stop("`x` must be non-negative.", call. = FALSE)
  }
  alpha <- 1 - conf_level
  len <- max(length(x), length(exposure))
  x <- rep_len(x, len)
  exposure <- rep_len(exposure, len)
  lower <- numeric(len)
  inner <- x > 0
  lower[inner] <- stats::qgamma(alpha / 2, x[inner]) / exposure[inner]
  list(lower = lower, upper = stats::qgamma(1 - alpha / 2, x + 1) / exposure)
}

#' Derivative of a binomial link with respect to probability
#' @noRd
link_derivative_response <- function(p, link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  .validate_numeric_vector(p, "p")
  p <- .bound_unit_interval(p)

  switch(link,
    logit = 1 / (p * (1 - p)),
    probit = 1 / dnorm(qnorm(p)),
    cloglog = 1 / ((1 - p) * (-log1p(-p)))
  )
}

#' Delta-method variance for a transformed binomial proportion
#' @noRd
binomial_link_variance <- function(p, n, link = c("logit", "probit", "cloglog")) {
  .validate_numeric_vector(p, "p")
  .validate_positive_numeric(n, "n")
  p <- .bound_unit_interval(p)
  p * (1 - p) * link_derivative_response(p, link)^2 / n
}

#' Emit package progress messages when enabled
#' @noRd
mlumr_message <- function(..., verbose = TRUE) {
  .validate_flag(verbose, "verbose")
  if (isTRUE(verbose)) {
    message(...)
  }
  invisible(NULL)
}


#' Validate a scalar link/family string
#' @noRd
.validate_link_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a single non-missing string.", name),
         call. = FALSE)
  }
  x
}


#' Validate a numeric vector
#' @noRd
.validate_numeric_vector <- function(x, name) {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric.", name), call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate positive finite numeric input
#' @noRd
.validate_positive_numeric <- function(x, name) {
  if (!is.numeric(x) || length(x) == 0L ||
        any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf("`%s` must contain positive finite values.", name),
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Bound probabilities to the open unit interval
#' @noRd
.bound_unit_interval <- function(p) {
  eps <- .Machine$double.eps
  pmin(pmax(p, eps), 1 - eps)
}

#' Select the summary columns that are present
#'
#' A fit whose summary lacks a diagnostic column reports it as unavailable and
#' must still print.
#' @noRd
.summary_columns <- function(df, cols) {
  df[, intersect(cols, names(df)), drop = FALSE]
}


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
    keep_cols <- c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")
    sub_df <- .summary_columns(x$summary[idx, , drop = FALSE], keep_cols)
    print(sub_df, row.names = FALSE)
    # `delta_conditional` is mu_index - mu_comparator. A shared baseline shape
    # makes it a conditional log hazard or time ratio at the reference
    # profile; shared coefficients make it constant across profiles.
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
  } else if (!is.null(n_req) && (is.null(n_got) || any(is.na(n_got)))) {
    cat(sprintf("  Chains: %d requested, layout UNKNOWN (see warnings)\n",
                n_req))
  }
  # Same resolver as the warning path, so the two cannot disagree.
  cat("  Divergent transitions:",
      .diagnostic_display(.transition_count(object$diagnostics$n_divergent)), "\n")
  cat("  Max treedepth hits:",
      .diagnostic_display(.transition_count(object$diagnostics$n_max_treedepth)), "\n")
  # A summary without the column still prints the line, as unavailable.
  n_par <- nrow(object$summary)
  rhat <- .usable_diagnostic_values(object$summary$Rhat, n_par)
  cat("  Max Rhat:",
      if (length(rhat$values)) {
        .format_diagnostic(max(rhat$values))
      } else {
        "unavailable"
      },
      .missing_suffix(rhat), "\n")
  ess <- .usable_diagnostic_values(object$summary$n_eff, n_par)
  cat("  Min ESS:",
      if (length(ess$values)) {
        .format_diagnostic(min(ess$values))
      } else {
        "unavailable"
      },
      .missing_suffix(ess), "\n")
  cat("\n")

  # Intercepts
  scale_label <- paste0(link_label, " scale")
  cat(sprintf("Intercepts (%s):\n", scale_label))
  mu_idx <- grep("^mu_", object$summary$variable)
  keep_cols <- c("variable", "mean", "sd", "2.5%", "97.5%", "Rhat")
  print(.summary_columns(object$summary[mu_idx, , drop = FALSE], keep_cols),
        row.names = FALSE)

  # Residual SD (normal only)
  if (family == "normal") {
    sigma_idx <- which(object$summary$variable == "sigma")
    if (length(sigma_idx) > 0) {
      cat("\nResidual SD:\n")
      print(.summary_columns(object$summary[sigma_idx, , drop = FALSE],
                             keep_cols),
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
    # Relabel beta[1] as beta[age] in the printed copy only.
    beta_df <- .summary_columns(object$summary[beta_idx, , drop = FALSE],
                                keep_cols)
    print(.label_beta_rows(beta_df, object$data$covariates), row.names = FALSE)

  }

  # Shape / smoothing parameters (survival)
  if (family == "survival") {
    # A stratified baseline makes the hazard ratio time-varying, which changes
    # how delta_* reads.
    n_strata <- object$stan_data$n_strata %||% 1L
    differs <- .aux_shapes_differ(object)
    cat("\nBaseline hazard: ",
        if (!differs && n_strata > 1L) {
          # The exponential has no shape to stratify; each study keeps its own
          # hazard level through its intercept.
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
      aux_df <- .summary_columns(object$summary[aux_idx, , drop = FALSE],
                                 keep_cols)
      print(aux_df, row.names = FALSE)
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
    # Heading from the fit's own label helper, with its evaluation time.
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
      # An RMST without its horizon is not an estimand.
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

  # An unverified estimate must not print like a verified one.
  if (identical((x$separation %||% list())$status, "unknown")) {
    cat("Separation: NOT VERIFIED (", x$separation$reason, "). Only the ",
        "fitted-value screen ran, which cannot see quasi-complete ",
        "separation.\n\n", sep = "")
  }

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
    # Under a log link `estimate` is the log mean ratio and `md` the mean
    # difference; a v0.1.0 result has no `md` and holds the difference in
    # `estimate`.
    if (identical(x$link, "log") && !is.null(x$md)) {
      cat(sprintf("\nLog Mean Ratio: %.4f (SE: %.4f)\n", x$estimate, x$se))
    } else {
      cat(sprintf("\nMean Difference: %.4f (SE: %.4f)\n", x$estimate, x$se))
    }
  } else if (family == "poisson") {
    cat(sprintf("Marginalized rate (index trt, comp pop): %.4f\n", x$rate_hat_index))
    cat(sprintf("Observed rate (comp trt, comp pop):      %.4f\n", x$rate_comparator))
    cat(sprintf("\nLog Rate Ratio: %.4f (SE: %.4f)\n", x$estimate, x$se))
  } else {
    cat("Method note: package-specific parametric survival extension.\n")
    if (!is.null(x$out_of_family)) {
      # The fitted shape or Q left the Bayesian model's parameter space.
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
    # The comparator RMST is a parametric fit too, not a Kaplan-Meier area.
    cat(sprintf("Marginalized RMST (index trt, comp pop): %.4f\n", x$rmst_index))
    cat(sprintf("Fitted RMST (comp trt, comp pop):        %.4f\n",
                x$rmst_comparator))
    req <- x$n_boot_requested %||% x$n_boot %||% 0L
    okn <- x$n_boot_ok %||% x$n_boot %||% 0L
    if (is.na(x$se)) {
      # Branch on the success count: one surviving resample is not a failure.
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
    # The cumulative-hazard ratio can rest on fewer resamples than the RMST.
    okc <- x$n_boot_ok_log_chr
    if (req > 0L && !is.null(okc) &&
          !identical(as.integer(okc), as.integer(okn))) {
      cat(sprintf(paste0("Log cumulative-hazard-ratio SE based on %d of %d ",
                         "resample(s)\n"), as.integer(okc), req))
    }
    # Resamples can leave the parameter space while the point estimate stays
    # inside; they still enter the standard error.
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
  # A survival STC has no GLM behind it.
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
#' @noRd
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
  # A missing bound is NULL on some results and NA on others; normalize to
  # NA_real_ and keep Inf, which an exact ratio can reach.
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
  # exp() of a normalized bound, so NULL stays NA.
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
    if (is.null(x$md)) {
      # naive() and a v0.1.0 stc() result hold the response-scale mean
      # difference in `estimate` under every link, and carry no `md`.
      add("Mean difference", x$estimate, x$se, x$ci_lower, x$ci_upper)
    } else {
      # Under a log link `estimate` is the log mean ratio and gets its own rows.
      if (identical(link, "log")) {
        add("Log mean ratio", x$estimate, x$se, x$ci_lower, x$ci_upper)
        add("Mean ratio", eexp(x$estimate), NA_real_, eexp(x$ci_lower), eexp(x$ci_upper))
      }
      add("Mean difference", x$md, x$md_se, x$md_lower, x$md_upper)
    }
  } else if (fam == "poisson") {
    add("Log rate ratio", x$estimate, x$se, x$ci_lower, x$ci_upper)
    add("Rate ratio", eexp(x$estimate), NA_real_, eexp(x$ci_lower), eexp(x$ci_upper))
    # Rate difference per unit exposure; an older result without it omits the row.
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
#' Rewrites `beta[1]` to `beta[age]` (and `beta_index[1]`,
#' `beta_comparator[1]` likewise) in the printed copy, as multinma does.
#'
#' @param df A slice of `fit$summary`.
#' @param covariates Character vector of covariate names, in model order.
#' @return `df` with its `variable` column relabeled where possible.
#' @noRd
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

#' mlumr: Multilevel Unanchored Meta-Regression for Indirect Treatment
#' Comparisons
#'
#' Bayesian multilevel unanchored meta-regression (ML-UMR) for population-
#' adjusted indirect comparisons between a single-arm individual patient
#' data (IPD) study on the index treatment and aggregate data (AgD) on a
#' comparator. Two model variants are provided: a shared-prognostic-factor
#' (SPFA) model and a relaxed-SPFA model that allows treatment-specific
#' covariate coefficients. Supports binary (binomial), continuous (normal),
#' count (Poisson), and time-to-event (survival) outcomes, each with
#' appropriate link functions. Frequentist outcome regression (Simulated
#' Treatment Comparison) and a naive unadjusted benchmark are included for
#' side-by-side comparison.
#'
#' @section Typical workflow:
#' \enumerate{
#'   \item [set_ipd()]: declare the individual patient data and its
#'     covariates.
#'   \item [set_agd()]: declare the aggregate-data arm.
#'   \item [combine_data()]: join the two into an `mlumr_data` object.
#'   \item [add_integration()]: build the Gaussian-copula QMC
#'     integration points used to marginalize over the AgD covariate
#'     distribution.
#'   \item [mlumr()]: fit the Bayesian ML-UMR model (SPFA or relaxed).
#'   \item [prior_summary()], [check_integration()]: sanity-check the
#'     model before inferring from it.
#'   \item [predict()][predict.mlumr_fit()], [marginal_effects()],
#'     [conditional_effects()], [conditional_predict()]: extract
#'     population-level and profile-specific quantities.
#'   \item [calculate_dic()], [calculate_loo()], [calculate_waic()],
#'     [compare_models()]: compare SPFA vs relaxed or competing
#'     specifications.
#'   \item [prior_sensitivity()]: examine robustness over specified prior
#'     choices.
#' }
#'
#' @section Priors:
#' The prior constructors [prior_normal()], [prior_student_t()],
#' [prior_cauchy()], and [prior_exponential()] all plug into [mlumr()] via
#' `prior_intercept`, `prior_beta`, and (normal family only) `prior_sigma`.
#' Survival models add two further knobs: `prior_aux` for the parametric shape or
#' scale parameter(s) (Weibull/Gompertz shape, log-normal sdlog, generalized
#' gamma shapes; see [default_prior_aux()]) and `prior_smooth` for the M-spline /
#' piecewise-exponential baseline smoothing standard deviation (see
#' [default_prior_smooth()]). The package defaults are generic starting values;
#' calibrate them using prior predictive checks and subject-matter knowledge.
#'
#' @section Quiet mode:
#' Set `options(mlumr.quiet = TRUE)` to suppress the package startup banner in
#' scripted sessions (in addition to the standard
#' `suppressPackageStartupMessages()`).
#'
#' @section Alternative methods:
#' [stc()] performs G-computation by fitting a one-arm outcome model and
#' standardizing index-treatment
#' predictions to the comparator population before contrasting them with the
#' observed comparator outcome there. It does not standardize to the index
#' population. [naive()] computes an unadjusted contrast between the observed
#' index study and comparator study, so it does not have a single common target
#' population. Both consume the same `mlumr_data` object as [mlumr()], but their
#' estimands must be interpreted before numerical results are compared.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib mlumr, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstantools rstan_config
#' @importFrom survival is.Surv
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stats as.formula binomial coef complete.cases cor dbinom glm
#' @importFrom stats pbinom plogis predict qbinom qlogis qnorm quantile sd
#' @importFrom stats setNames var vcov weighted.mean
## usethis namespace: end
NULL

#' Refuse normal IPD that its own covariates fit exactly
#'
#' With an exact fit the marginal density of the residual SD behaves as
#' `sigma^(rank - n)` near zero and does not integrate, and the sampler
#' drifts toward zero with ordinary-looking diagnostics. A constant outcome
#' and a least-squares fit that is exact to numerical precision (residual
#' sum of squares at most 1e-12 of the total) are refused; a saturated design
#' (`n <= rank`) has a proper posterior whose residual SD is not separated
#' from the coefficients, so it warns. Under `link = "log"` the question is
#' asked on `log(y)` when every outcome is positive, and not otherwise.
#'
#' @param data An `mlumr_data` object.
#' @param link The resolved link, `"identity"` or `"log"`.
#' @param center The centers the model subtracts from the covariates, or a
#'   logical for the raw design.
#' @return `TRUE` invisibly if the data were warned about.
#' @noRd
.check_normal_residual_variation <- function(data, link = "identity",
                                             center = TRUE) {
  ipd <- data$ipd$data
  y <- suppressWarnings(as.numeric(ipd$.outcome))
  X <- as.matrix(ipd[, data$covariates, drop = FALSE])
  if (!length(y) || !all(is.finite(y)) || !all(is.finite(X))) {
    return(invisible(FALSE))
  }
  if (is.numeric(center)) X <- sweep(X, 2, as.numeric(center))
  if (identical(link, "log")) {
    if (any(y <= 0)) return(invisible(FALSE))
    y <- log(y)
  }
  X <- cbind(1, X)
  n <- length(y)
  rank <- qr(X)$rank
  if (n <= rank) {
    warning("The IPD design has as many free columns as rows (", n, " rows, ",
            "rank ", rank, "), so nothing in the data separates the residual ",
            "SD from the coefficients and its estimate follows the priors.",
            call. = FALSE)
    return(invisible(TRUE))
  }
  # Center and scale the outcome so the ratio below does not depend on its
  # units; with an intercept in the design only the intercept moves.
  y <- y - mean(y)
  scale <- max(abs(y))
  if (is.finite(scale) && scale > 0) y <- y / scale
  tss <- sum(y^2)
  if (tss == 0) {
    stop("The IPD outcome is constant, so the normal model has no residual ",
         "variation and the posterior for the residual SD is improper. The ",
         "outcome needs variation the covariates do not explain.",
         call. = FALSE)
  }
  rss <- sum(stats::lm.fit(X, y)$residuals^2)
  if (rss <= 1e-12 * tss) {
    stop("The IPD covariates fit the outcome exactly, so the posterior for ",
         "the residual SD is improper. The outcome needs variation the ",
         "covariates do not explain.", call. = FALSE)
  }
  invisible(FALSE)
}


#' Fit ML-UMR Model
#'
#' Fit a Bayesian multilevel unanchored meta-regression model using individual
#' patient data (IPD) and aggregate data (AgD). Supports binary, continuous,
#' count, and time-to-event outcomes.
#'
#' @param data An `mlumr_data` object with integration points (from
#'   [add_integration()])
#' @param model Model type: `"spfa"` (shared prognostic factor assumption) or
#'   `"relaxed"` (treatment-specific coefficients). Default `"spfa"`.
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
#'   poisson and survival: `"log"` (default, only option). If `NULL`, uses the
#'   canonical default for the family.
#' @param prior_intercept Prior for treatment intercepts. Default from
#'   [default_prior_intercept()] (`prior_normal(0, 10)`), on the
#'   linear-predictor scale; for `family = "normal"` with the identity link,
#'   where the intercepts are in outcome units, `normal(0, 10 * sd(y))` with
#'   `sd(y)` the IPD outcome SD. See [prior_normal()].
#' @param prior_beta Prior for regression coefficients. A single prior
#'   broadcast to all covariates, or a `list` of priors of length `n_cov`
#'   sharing one family (and, for Student-t, one df). Default from
#'   [default_prior_beta()] (`prior_normal(0, 2.5)`, times `sd(y)` for
#'   `family = "normal"` with the identity link). Set `autoscale = TRUE`
#'   on the prior to divide the scale by each covariate's empirical SD (and,
#'   for the normal identity link, multiply it by `sd(y)`). For
#'   `model = "spfa"` this is the prior on the shared `beta`; for
#'   `model = "relaxed"` on `beta_index`, with `beta_comparator` taking
#'   `prior_beta_comparator`.
#' @param prior_beta_comparator Relaxed model only: prior for the
#'   comparator-arm coefficients `beta_comparator`, with the same
#'   specification rules as `prior_beta` and any supported family. `NULL`
#'   (the default) reuses `prior_beta`. `beta_comparator` is informed only by
#'   the aggregate likelihood, so a tighter prior here regularizes the
#'   index-population estimand; see [check_identification()] and
#'   [prior_sensitivity()]. Ignored for `model = "spfa"`.
#' @param prior_sigma Prior for residual SD (normal family only). Default
#'   from [default_prior_sigma()], a half-normal (through the Stan
#'   `<lower=0>` constraint) with scale `2.5 * sd(y)`, the residual SD being
#'   in outcome units under either link. [prior_exponential()] is also
#'   supported for sigma.
#' @param distribution For `family = "survival"` only: the survival
#'   distribution. Proportional hazards: `"exponential"`, `"weibull"`
#'   (default), `"gompertz"` (positive shape, so an increasing hazard).
#'   Accelerated failure time: `"exponential-aft"`, `"weibull-aft"`,
#'   `"lognormal"`, `"loglogistic"`, `"gamma"`, `"gengamma"` (the positive
#'   Lawless `Q` subfamily). Flexible baseline hazard: `"mspline"` and
#'   `"pexp"` (piecewise exponential). `"gengamma"` is the least numerically
#'   robust option, so inspect its MCMC diagnostics. Must be `NULL` for other
#'   families.
#' @param prior_aux For `family = "survival"` parametric distributions: prior
#'   for the shape or scale parameter(s), half-normal, half-t or exponential
#'   through the `<lower=0>` constraint. Default [default_prior_aux()]. The
#'   Gompertz shape has units of 1 / time, so set this explicitly for a
#'   Gompertz baseline and check it against the time unit.
#' @param prior_aux2 For `distribution = "gengamma"` only: prior for the
#'   second auxiliary parameter. `NULL` (the default) reuses `prior_aux`.
#'   Supplying it for any other distribution warns and has no effect.
#' @param prior_smooth For `family = "survival"` flexible baselines
#'   (`"mspline"`/`"pexp"`): prior for the random-walk smoothing SD. Default
#'   [default_prior_smooth()].
#' @param n_knots For `family = "survival"` flexible baselines: number of
#'   internal spline knots (default 7). See [make_knots()].
#' @param knots Optional custom knots for a flexible survival baseline. With a
#'   shared baseline (`aux_by = "none"`), supply one [make_knots()] result. With
#'   study-specific baselines, supply `list(index = ..., comparator = ...)`,
#'   where each element has the same structure and coefficient count.
#' @param aux_by For `family = "survival"`: how the baseline hazard is shared
#'   between the two studies, the unanchored analogue of `multinma::nma()`'s
#'   `aux_by`. `".study"` (the default, and what `NULL` means) gives each
#'   study its own baseline shape, with its own knots over its own observed
#'   support for a flexible baseline. `"none"` gives both studies one shared
#'   shape, a stronger assumption that buys precision; fit it as a
#'   sensitivity analysis when the two Kaplan-Meier curves plainly share a
#'   shape. Each study contributes one arm, so a study-specific and a
#'   treatment-specific baseline shape cannot be told apart, and a stratified
#'   fit carries each study's shape with its treatment when predictions are
#'   transported; where that is doubtful, prefer the RMST estimands and
#'   report both settings. With the stratified default the marginal hazard
#'   ratio varies with time, so [marginal_effects()] reports it at one
#'   `at_time`.
#' @param mspline_degree For `family = "survival"` flexible baselines: spline
#'   degree override (default derived from `distribution`: 3 for `"mspline"`,
#'   0 for `"pexp"`).
#' @param pred_times For `family = "survival"`: times at which survival,
#'   hazard and cumulative-hazard predictions are produced. If `NULL`, a grid
#'   up to the maximum observed time is used.
#' @param rmst_horizon For `family = "survival"`: the upper time limit for the
#'   restricted mean survival time. If `NULL`, the maximum observed time,
#'   except for a flexible baseline stratified by study, where it defaults to
#'   the follow-up both studies observed so the headline RMST does not
#'   extrapolate the shorter study. A longer horizon warns.
#' @param n_rmst_grid For `family = "survival"`: number of equally spaced nodes
#'   (default `100`) on `[0, rmst_horizon]` for the trapezoidal RMST integral.
#'   Increase for sharp early hazards or long horizons.
#' @param center Logical (default `TRUE`). Center the covariates about the
#'   pooled IPD and population-weighted declared AgD means before fitting.
#'   The likelihood is unchanged and sampling is usually easier, but
#'   `prior_intercept` then applies to the intercept at the pooled covariate
#'   mean. Set `FALSE` to fit on the raw covariate scale. A fit whose
#'   centering rounds two integration points onto one is refused.
#' @param qr Logical (default `FALSE`). Apply a thin-QR reparameterization to
#'   the combined design matrix, which decorrelates its columns for HMC. The
#'   priors still apply to the original coefficients. Useful with many
#'   correlated or ill-scaled covariates.
#' @param chains Number of MCMC chains (default 4)
#' @param iter Total iterations per chain (default 2000)
#' @param warmup Number of warmup iterations (default 1000)
#' @param seed Random seed for reproducibility. If `NULL` (default), the fixed
#'   seed 2026 is used and a warning says so. The seed used is reported in the
#'   fitting messages.
#' @param adapt_delta Target acceptance rate (default 0.95)
#' @param max_treedepth Maximum tree depth for NUTS (default 15)
#' @param refresh How often to print progress (0 = silent, default 200)
#' @param engine Stan backend: `"rstan"` (default) or `"cmdstanr"`. If `NULL`,
#'   uses the engine set by [mlumr_engine()]. See [mlumr_engine()] for setup.
#' @param verbose Logical; if `FALSE`, suppresses mlumr progress messages.
#'   Stan sampler progress is still controlled by `refresh`.
#' @param ... Additional arguments passed to the Stan sampling function
#'   ([rstan::sampling()] or cmdstanr's `$sample()` method)
#'
#' @details
#' The model assumes that all AgD rows come from the same comparator treatment
#' and that, conditional on covariates, there is no between-study
#' heterogeneity. No random effects for study-level heterogeneity are
#' included.
#'
#' **AgD scale (family = `"normal"`).** The AgD likelihood is
#' `y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
#' `y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`; in both cases
#' [set_agd()] expects `outcome_mean` and `outcome_se` on the arithmetic
#' scale.
#'
#' **The comparator population is the size-weighted mixture of its aggregate
#' rows.** Comparator-population predictions weight each row by the
#' population it represents: `n_agd` for binomial, `outcome_n` for normal
#' (required for more than one row) and `E_agd` for poisson. These are
#' mixing weights, separate from the likelihood's precision weights, so
#' splitting a comparator population into subgroup rows leaves the estimand
#' unchanged.
#'
#' **Identifying the relaxed model.** `beta_comparator` is informed only by
#' the aggregate likelihood. Jointly defined subgroup rows, one [set_agd()]
#' row per stratum, are what can separate it from the comparator intercept;
#' a single aggregate summary constrains one combination of them. Use
#' [check_identification()] before fitting and [prior_sensitivity()] after;
#' the subgroup-identification vignette works through the geometry.
#'
#' **Normal outcomes.** A normal fit whose IPD covariates reproduce the
#' outcome exactly has an improper posterior for the residual SD, so
#' `mlumr()` refuses a constant outcome and a least-squares fit that is exact
#' to numerical precision (residual sum of squares at most 1e-12 of the
#' total) before sampling, and warns when the design is saturated. Everything
#' else is left to the sampler and its diagnostics.
#'
#' @seealso [prior_sensitivity()], [check_identification()], [set_agd()],
#'   [prior_summary()].
#'
#' @return An object of class `mlumr_fit`
#' @export
#'
#' @examples
#' \dontrun{
#' # Binary SPFA model
#' fit_spfa <- mlumr(dat, model = "spfa")
#'
#' # Relaxed SPFA (allows effect modification)
#' fit_relaxed <- mlumr(dat, model = "relaxed")
#' }
mlumr <- function(data,
                  model = c("spfa", "relaxed"),
                  link = NULL,
                  prior_intercept = default_prior_intercept(),
                  prior_beta = default_prior_beta(),
                  prior_sigma = default_prior_sigma(),
                  distribution = NULL,
                  prior_aux = NULL,
                  prior_smooth = NULL,
                  n_knots = 7L,
                  knots = NULL,
                  mspline_degree = NULL,
                  aux_by = ".study",
                  pred_times = NULL,
                  rmst_horizon = NULL,
                  n_rmst_grid = 100L,
                  center = TRUE,
                  qr = FALSE,
                  chains = 4,
                  iter = 2000,
                  warmup = 1000,
                  seed = NULL,
                  adapt_delta = 0.95,
                  max_treedepth = 15,
                  refresh = 200,
                  engine = NULL,
                  verbose = TRUE,
                  prior_beta_comparator = NULL,
                  prior_aux2 = NULL,
                  ...) {

  model <- match.arg(model)

  .validate_flag(verbose, "verbose")
  .validate_flag(center, "center")
  .validate_flag(qr, "qr")
  .validate_mlumr_sampling_args(
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh
  )
  engine <- .resolve_mlumr_engine(engine)
  seed_info <- .resolve_mlumr_seed(seed)
  seed <- seed_info$value

  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data()", call. = FALSE)
  }
  if (!data$has_integration) {
    stop("Integration points not found. Use add_integration() first.", call. = FALSE)
  }

  validate_prior(prior_intercept, "intercept")
  if (prior_intercept$distribution == "exponential") {
    stop("prior_intercept does not support exponential priors ",
         "(treatment intercepts are unconstrained on the link scale). ",
         "Use prior_normal(), prior_student_t(), or prior_cauchy().",
         call. = FALSE)
  }
  # prior_beta may be a single prior or a list of per-coefficient priors;
  # per-coefficient validation happens inside stan_prior_fields_beta().
  if (is_single_prior(prior_beta)) {
    validate_prior(prior_beta, "beta")
    if (prior_beta$distribution == "exponential") {
      stop("prior_beta does not support exponential priors ",
           "(coefficients are unconstrained on the link scale). ",
           "Use prior_normal(), prior_student_t(), or prior_cauchy().",
           call. = FALSE)
    }
  } else if (!is.list(prior_beta)) {
    stop("`prior_beta` must be a prior list or a list of priors.", call. = FALSE)
  }

  # autoscale applies to the coefficient priors only.
  if (isTRUE(prior_intercept$autoscale)) {
    warning("`autoscale = TRUE` on prior_intercept is ignored; ",
            "autoscaling is only applied to prior_beta and ",
            "prior_beta_comparator.", call. = FALSE)
  }

  family <- data$family %||% "binomial"
  link_info <- check_link(family, link)

  if (!is.null(prior_beta_comparator)) {
    if (model == "spfa") {
      # Warn rather than validate: the SPFA model never reads it.
      warning("`prior_beta_comparator` is ignored for the SPFA model ",
              "(which has a single shared `beta`); only the relaxed model ",
              "has a comparator-specific coefficient vector.",
              call. = FALSE)
      prior_beta_comparator <- NULL
    } else if (is_single_prior(prior_beta_comparator)) {
      validate_prior(prior_beta_comparator, "beta_comparator")
      if (prior_beta_comparator$distribution == "exponential") {
        stop("prior_beta_comparator does not support exponential priors ",
             "(coefficients are unconstrained on the link scale). ",
             "Use prior_normal(), prior_student_t(), or prior_cauchy().",
             call. = FALSE)
      }
    } else if (!is.list(prior_beta_comparator)) {
      stop("`prior_beta_comparator` must be a prior list or a list of priors.",
           call. = FALSE)
    }
  }

  # Survival distribution + auxiliary/smoothing priors
  surv_info <- NULL
  if (family == "survival") {
    surv_info <- .survival_distribution_info(distribution)
    if (!is.null(knots) && surv_info$kind != "flexible") {
      stop("`knots` can only be supplied with `distribution = \"mspline\"` ",
           "or `distribution = \"pexp\"`.", call. = FALSE)
    }
    if (!is.null(mspline_degree)) {
      # The degree defines the distribution ("pexp" is 0, "mspline" is 3), so
      # a contradicting override is refused.
      requested <- as.integer(mspline_degree)
      canonical <- surv_info$mspline_degree
      if (is.na(canonical)) {
        stop("`mspline_degree` applies only to the flexible baselines, ",
             "`distribution = \"mspline\"` (degree 3) or \"pexp\" ",
             "(degree 0). Got distribution = \"", distribution, "\".",
             call. = FALSE)
      }
      if (!identical(requested, canonical)) {
        stop("`mspline_degree = ", requested, "` contradicts `distribution = \"",
             distribution, "\"`, which is degree ", canonical, ". Use ",
             "`distribution = \"pexp\"` for a degree-0 piecewise-exponential ",
             "baseline or `distribution = \"mspline\"` for the degree-3 ",
             "M-spline; the fit would otherwise be reported under the wrong ",
             "name.", call. = FALSE)
      }
      surv_info$mspline_degree <- requested
    }
    prior_aux <- prior_aux %||% default_prior_aux()
    # Only the generalized gamma has a second auxiliary parameter.
    if (!is.null(prior_aux2) && (surv_info$n_aux %||% 0L) < 2L) {
      warning("`prior_aux2` applies to the second auxiliary parameter of ",
              "`distribution = \"gengamma\"`; `distribution = \"",
              surv_info$distribution, "\"` has ",
              if ((surv_info$n_aux %||% 0L) == 0L) "no" else "one",
              " auxiliary parameter, so it is ignored. Use `prior_aux`.",
              call. = FALSE)
      # Dropped before validation, so an ignored prior warns whether or not
      # it is well formed.
      prior_aux2 <- NULL
    }
    prior_aux2 <- prior_aux2 %||% prior_aux
    prior_smooth <- prior_smooth %||% default_prior_smooth()
    validate_prior(prior_aux, "prior_aux")
    validate_prior(prior_aux2, "prior_aux2")
    validate_prior(prior_smooth, "prior_smooth")
    .validate_survival_controls(pred_times, rmst_horizon, mspline_degree,
                                n_knots, n_rmst_grid,
                                distribution = distribution,
                                knots = knots)
    invisible(.resolve_aux_strata(aux_by))   # fail on a bad value before fitting
    .validate_survival_studies(data, aux_by)
  } else {
    if (!is.null(knots)) {
      stop("`knots` is only used for flexible survival models.", call. = FALSE)
    }
    if (!is.null(distribution)) {
      stop("`distribution` is only used for family = 'survival'.", call. = FALSE)
    }
    # `aux_by` has a default, so object only when it was supplied.
    if (!missing(aux_by)) {
      stop("`aux_by` is only used for family = 'survival': it stratifies the ",
           "baseline hazard, which the other families do not have.",
           call. = FALSE)
    }
    # Controls with a NULL default are evidence when non-NULL; n_knots and
    # n_rmst_grid have real defaults, so use missing().
    unused <- c(
      if (!is.null(mspline_degree)) "mspline_degree",
      if (!is.null(pred_times)) "pred_times",
      if (!is.null(rmst_horizon)) "rmst_horizon",
      if (!missing(n_knots)) "n_knots",
      if (!missing(n_rmst_grid)) "n_rmst_grid"
    )
    if (length(unused)) {
      stop("`", paste(unused, collapse = "`, `"), "` ",
           if (length(unused) == 1L) "describes" else "describe",
           " a survival baseline hazard or its prediction grid, which ",
           "family = '", family, "' does not have.", call. = FALSE)
    }
    if (!is.null(prior_aux) || !is.null(prior_aux2) || !is.null(prior_smooth)) {
      warning("`prior_aux` / `prior_aux2` / `prior_smooth` are ignored for ",
              "non-survival families.", call. = FALSE)
    }
  }

  if (family == "normal") {
    validate_prior(prior_sigma, "sigma")
    if (isTRUE(prior_sigma$autoscale)) {
      warning("`autoscale = TRUE` on prior_sigma is ignored; ",
              "autoscaling is only applied to prior_beta and ",
              "prior_beta_comparator.", call. = FALSE)
    }
  } else if (!is.null(prior_sigma) && !isTRUE(prior_sigma$default)) {
    warning("`prior_sigma` is ignored for non-normal families.",
            call. = FALSE)
  }

  # Weak identifiability of `beta_comparator` in relaxed models. It is informed
  # only by the aggregate likelihood, so the note differs by what that
  # likelihood actually is.
  if (model == "relaxed") {
    n_cov_check <- data$n_covariates
    # The remedy depends on whether a comparator prior is already set.
    remedy <- if (is.null(prior_beta_comparator)) {
      paste0("Add jointly defined subgroup rows, set an informative ",
             "`prior_beta_comparator`, or use model = \"spfa\".")
    } else {
      paste0("With `prior_beta_comparator` supplied those directions follow ",
             "the prior; refit at other scales to see how far the ",
             "index-population estimand moves, or add jointly defined ",
             "subgroup rows.")
    }
    if (family == "survival") {
      # A reconstructed curve is not one scalar constraint, so nothing here
      # gates on a row or event count.
      warning(sprintf(
        paste0("Relaxed survival model with %d covariate(s): the comparator ",
               "coefficients are informed only through the reconstructed ",
               "comparator curve, which may leave some directions weakly ",
               "determined. Inspect their posterior and run ",
               "prior_sensitivity() with `prior_beta_comparator_scales`; ",
               "`prior_beta_comparator` regularizes them and ",
               "model = \"spfa\" avoids them."),
        n_cov_check
      ), call. = FALSE)
    } else if (family == "normal" && link_info$link == "identity") {
      n_agd_rows_check <- nrow(data$agd$data)
      agd_rank <- .agd_covariate_rank(data)
      # The numerical rank counts directions that exist; `.profile_rank()`
      # counts those with practical spread.
      agd_numeric_rank <- .agd_covariate_numeric_rank(data)
      if (agd_numeric_rank < n_cov_check + 1L) {
        warning(sprintf(
          paste0("Relaxed model with %d AgD row(s) and %d covariate(s): the ",
                 "aggregate mean profiles span only %d of the %d independent ",
                 "directions the identity-link design needs, so some ",
                 "comparator coefficients are not separated by the ",
                 "likelihood. Supply the comparator as jointly defined ",
                 "subgroup rows, one set_agd() row per stratum. %s"),
          n_agd_rows_check, n_cov_check, agd_numeric_rank, n_cov_check + 1L,
          remedy
        ), call. = FALSE)
      } else if (agd_rank < n_cov_check + 1L) {
        warning(sprintf(
          paste0("Relaxed model with %d AgD row(s) and %d covariate(s): the ",
                 "aggregate mean profiles span the %d directions needed, but ",
                 "%d of them move less than 0.05 IPD SD, so the corresponding ",
                 "comparator coefficients lean on the precision of each row's ",
                 "outcome. This is a screening heuristic about spread; read ",
                 "the fitted intervals. %s"),
          n_agd_rows_check, n_cov_check, n_cov_check + 1L,
          n_cov_check + 1L - agd_rank, remedy
        ), call. = FALSE)
      }
    } else {
      n_agd_rows_check <- nrow(data$agd$data)
      # Under a nonlinear link rows with equal means can still differ in
      # spread, so count distinct integration grids rather than rows.
      n_distinct_check <- .agd_distinct_profiles(data)
      if (n_distinct_check < n_cov_check + 1L) {
        warning(sprintf(
          paste0("Relaxed model with %d AgD row(s), %d of them distinct, and ",
                 "%d covariate(s): the comparator side has %d parameters but ",
                 "only %d distinct aggregate summaries, so its coefficients ",
                 "cannot all be separated without prior information. %s"),
          n_agd_rows_check, n_distinct_check, n_cov_check, n_cov_check + 1L,
          n_distinct_check, remedy
        ), call. = FALSE)
      }
    }
  }

  prepared <- .mlumr_build_stan_data(
    data = data,
    family = family,
    link_info = link_info,
    prior_intercept = prior_intercept,
    prior_beta = prior_beta,
    prior_beta_comparator = prior_beta_comparator,
    prior_sigma = prior_sigma,
    surv_info = surv_info,
    prior_aux = prior_aux,
    prior_aux2 = prior_aux2,
    prior_smooth = prior_smooth,
    n_knots = n_knots,
    knots = knots,
    pred_times = pred_times,
    rmst_horizon = rmst_horizon,
    n_rmst_grid = n_rmst_grid,
    aux_by = aux_by,
    model = model,
    center = center,
    qr = qr
  )
  stan_data <- prepared$stan_data
  # Judged on the design the model fits, before any backend is chosen.
  if (family == "normal") {
    .check_normal_residual_variation(data, link_info$link,
                                     center = stan_data$cov_center)
  }

  # Select Stan model. family_config gives the default prefix; the survival
  # family overrides it for the flexible-baseline distributions.
  stan_prefix <- if (family == "survival") {
    surv_info$stan_prefix
  } else {
    get_family_config(family)$stan_prefix
  }
  model_name <- paste0(stan_prefix, "_", model)

  .mlumr_log_fit_start(model_name, family, link_info$link, stan_data,
                       engine, seed_info, verbose)

  # Fit model
  result <- .mlumr_fit_backend(
    engine = engine,
    model_name = model_name,
    stan_data = stan_data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    refresh = refresh,
    verbose = verbose,
    ...
  )

  fit <- result$native_fit
  draws <- result$draws
  chain_ids <- result$chain_ids
  summary_df <- result$summary_df
  n_divergent <- result$n_divergent
  n_max_td <- result$n_max_td

  # User priors plus the Stan-scale values used, for prior_summary().
  priors <- .mlumr_prior_metadata(
    data = data,
    family = family,
    model = model,
    prior_intercept = prior_intercept,
    prior_beta = prior_beta,
    prior_beta_comparator = prior_beta_comparator,
    prior_sigma = prior_sigma,
    prior_aux = prior_aux,
    prior_aux2 = prior_aux2,
    prior_smooth = prior_smooth,
    surv_info = surv_info,
    beta_fields = prepared$beta_fields,
    beta_comparator_fields = prepared$beta_comparator_fields,
    sd_x = prepared$sd_x,
    intercept_resolved = prepared$prior_intercept,
    sigma_resolved = prepared$prior_sigma,
    sd_y = prepared$sd_y
  )

  out <- list(
    stanfit = fit,
    draws = draws,
    # Per-draw chain labels from the backend, NULL if unavailable.
    chain_ids = chain_ids,
    summary = summary_df,
    diagnostics = list(
      n_divergent = n_divergent,
      n_max_treedepth = n_max_td,
      n_chains_requested = result$n_chains_requested %||% as.integer(chains),
      n_chains_returned = result$n_chains_returned %||% as.integer(chains)
    ),
    data = data,
    family = family,
    link = link_info$link,
    link_code = link_info$code,
    model = model,
    model_name = model_name,
    # What was fitted, kept on the fit: prior_sensitivity() and predict()
    # read these.
    distribution = if (!is.null(surv_info)) surv_info$distribution else NULL,
    surv_info = surv_info,
    pred_times = stan_data$pred_times,
    # Survival-only controls are stored as NULL for other families so a
    # replay does not pass them.
    surv_controls = list(
      n_knots = n_knots,
      knots = if (!is.null(surv_info) && surv_info$kind == "flexible") knots else NULL,
      aux_by = if (family == "survival") aux_by else NULL,
      mspline_degree = if (!is.null(surv_info) &&
                             surv_info$kind == "flexible") {
        surv_info$mspline_degree
      } else {
        NULL
      },
      pred_times = stan_data$pred_times,
      rmst_horizon = if (family == "survival")
        max(stan_data$rmst_grid_times) else NULL,
      n_rmst_grid = if (family == "survival")
        length(stan_data$rmst_grid_times) else NULL
    ),
    stan_data = stan_data,
    # Design controls travel with the fit so a refit reproduces the same
    # parameterization.
    model_controls = list(
      center = center,
      qr = qr
    ),
    engine = engine,
    priors = priors,
    sampling_args = list(
      chains = chains,
      iter = iter,
      warmup = warmup,
      seed = seed,
      # The effective settings when the backend reports them; rstan's
      # `control` can change them.
      adapt_delta = result$adapt_delta_used %||% adapt_delta,
      max_treedepth = result$max_treedepth_used %||% max_treedepth,
      # The merged sampler control (NULL for cmdstanr), and the names of
      # anything else forwarded to the backend, so a replay can say what it
      # cannot reproduce.
      control = result$control_used,
      extra_backend_args = setdiff(
        names(list(...)),
        c("chains", "iter", "warmup", "seed", "adapt_delta", "max_treedepth",
          "control")
      )
    )
  )

  class(out) <- c("mlumr_fit", "list")

  check_diagnostics(out)

  mlumr_message("Fitting complete!", verbose = verbose)
  out
}

#' Build the Stan data list for mlumr()
#' @noRd
.mlumr_build_stan_data <- function(data, family, link_info, prior_intercept,
                                   prior_beta, prior_beta_comparator = NULL,
                                   prior_sigma,
                                   surv_info = NULL, prior_aux = NULL,
                                   prior_aux2 = NULL,
                                   prior_smooth = NULL, n_knots = 7L,
                                   knots = NULL,
                                   pred_times = NULL, rmst_horizon = NULL,
                                   n_rmst_grid = 100L, aux_by = NULL,
                                   model = "spfa", center = TRUE, qr = FALSE) {
  ipd_data <- data$ipd$data
  agd_data <- data$agd$data
  X_ipd <- as.matrix(ipd_data[, data$covariates])

  # The normal family's intercepts and coefficients (identity link) and its
  # residual SD (either link) are in the outcome's units, so the package
  # defaults are read in units of the IPD outcome SD there.
  sd_y <- if (family == "normal") .outcome_sd(ipd_data$.outcome) else 1
  sd_y_eta <- if (identical(link_info$link, "identity")) sd_y else 1
  prior_intercept <- .outcome_scaled_prior(prior_intercept, sd_y_eta)
  prior_sigma <- .outcome_scaled_prior(prior_sigma, sd_y)

  # Both coefficient blocks use the IPD SD as the autoscaling reference. A
  # single IPD row leaves sd() undefined; treat it as no empirical scale.
  sd_x <- apply(X_ipd, 2, stats::sd)
  sd_x[!is.finite(sd_x)] <- 0
  intercept_fields <- stan_prior_fields(prior_intercept)
  beta_fields <- stan_prior_fields_beta(
    prior_beta,
    data$n_covariates,
    sd_x = sd_x,
    covariate_names = data$covariates,
    sd_y = sd_y_eta
  )
  # The comparator prior carries its own family and df into Stan; NULL
  # reuses prior_beta.
  beta_comparator_fields <- if (is.null(prior_beta_comparator)) {
    beta_fields
  } else {
    stan_prior_fields_beta(
      prior_beta_comparator,
      data$n_covariates,
      sd_x = sd_x,
      covariate_names = data$covariates,
      sd_y = sd_y_eta
    )
  }

  stan_data <- list(
    n_ipd = nrow(ipd_data),
    n_cov = data$n_covariates,
    X_ipd = X_ipd,
    n_agd_rows = nrow(agd_data),
    n_int = data$n_int,
    X_int = data$integration_points,
    prior_intercept_mean = intercept_fields$mean,
    prior_intercept_sd = intercept_fields$sd,
    prior_intercept_dist = intercept_fields$dist,
    prior_intercept_df = intercept_fields$df,
    prior_beta_mean = as.array(beta_fields$mean),
    prior_beta_sd = as.array(beta_fields$sd),
    prior_beta_dist = beta_fields$dist,
    prior_beta_df = beta_fields$df,
    # Always passed; SPFA models ignore the unused entries.
    prior_beta_comparator_mean = as.array(beta_comparator_fields$mean),
    prior_beta_comparator_sd = as.array(beta_comparator_fields$sd),
    prior_beta_comparator_dist = beta_comparator_fields$dist,
    prior_beta_comparator_df = beta_comparator_fields$df,
    link = link_info$code
  )

  if (family == "binomial") {
    stan_data$y_ipd <- .as_count_integer(ipd_data$.outcome)
    stan_data$n_agd <- array(.as_count_integer(agd_data$.n))
    stan_data$r_agd <- array(.as_count_integer(agd_data$.r))
  } else if (family == "normal") {
    bad_n <- is.null(agd_data$.n) || any(!is.finite(agd_data$.n)) ||
      any(agd_data$.n <= 0)
    if (nrow(agd_data) > 1L && bad_n) {
      stop("`outcome_n` is required when normal aggregate data contain ",
           "multiple rows, because those rows are population strata and must ",
           "be combined using their sample sizes.", call. = FALSE)
    }
    sigma_fields <- stan_prior_fields(prior_sigma)
    stan_data$y_ipd <- as.numeric(ipd_data$.outcome)
    stan_data$y_agd <- array(as.numeric(agd_data$.y))
    stan_data$se_agd <- array(as.numeric(agd_data$.se))
    # Mixing weights for the comparator estimand (sample sizes), not the
    # likelihood's precision weights.
    n_agd_rows <- length(stan_data$y_agd)
    agd_n <- agd_data$.n
    stan_data$agd_weight <- if (!is.null(agd_n) &&
                                  all(is.finite(agd_n)) && all(agd_n > 0)) {
      as.array(as.numeric(agd_n))
    } else {
      as.array(rep(1, n_agd_rows))
    }
    stan_data$prior_sigma_location <- sigma_fields$mean
    stan_data$prior_sigma_scale <- sigma_fields$sd
    stan_data$prior_sigma_dist <- sigma_fields$dist
    stan_data$prior_sigma_df <- sigma_fields$df
  } else if (family == "poisson") {
    stan_data$y_ipd <- .as_count_integer(ipd_data$.outcome)
    stan_data$E_ipd <- as.numeric(ipd_data$.exposure)
    stan_data$r_agd <- array(.as_count_integer(agd_data$.r))
    stan_data$E_agd <- array(as.numeric(agd_data$.E))
  } else {
    stan_data <- .build_stan_data_survival(
      stan_data = stan_data, data = data, surv_info = surv_info,
      pred_times = pred_times, n_knots = n_knots,
      knots = knots,
      rmst_horizon = rmst_horizon, n_rmst_grid = n_rmst_grid,
      prior_aux = prior_aux, prior_aux2 = prior_aux2,
      prior_smooth = prior_smooth,
      n_strata = .resolve_aux_strata(aux_by)
    )
  }

  # Centering leaves the likelihood unchanged but not the intercept prior;
  # QR is an affine reparameterization for HMC.
  agd_means <- as.matrix(agd_data[, paste0(data$covariates, "_mean"),
                                  drop = FALSE])
  stan_data <- .mlumr_center_covariates(
    stan_data, center = center, family = family, agd_means = agd_means
  )
  # Asked before the QR rank check, which would refuse a collapsed grid for
  # a different reason.
  .check_grid_nodes_kept(data$integration_points, stan_data$X_int,
                         covariates = data$covariates)
  stan_data <- .mlumr_qr_design(stan_data, model = model, qr = qr)

  list(stan_data = stan_data,
       beta_fields = beta_fields,
       beta_comparator_fields = beta_comparator_fields,
       sd_x = sd_x,
       # The intercept and sigma priors as the model used them.
       prior_intercept = prior_intercept,
       prior_sigma = prior_sigma,
       sd_y = sd_y)
}

#' Population weights for the AgD rows used in covariate centering
#'
#' Returns one weight per aggregate row, taken from the family's comparator
#' weight field (`n_agd`, `agd_weight`, `E_agd`), or the pseudo-individual count
#' for survival. Falls back to equal weights when no usable field is present.
#' Weights must be positive and finite, and must sum over a split subgroup to
#' the same total as the unsplit one, which is what makes the center invariant
#' to how the aggregate evidence is tabulated.
#'
#' @param stan_data The assembled Stan data list.
#' @param family Outcome family name.
#' @param n_agd_rows Number of aggregate rows.
#' @return Numeric vector of length `n_agd_rows`.
#' @noRd
.agd_center_weights <- function(stan_data, family, n_agd_rows) {
  fallback <- rep(1, n_agd_rows)
  cfg <- tryCatch(get_family_config(family), error = function(e) NULL)
  field <- if (is.null(cfg)) NULL else cfg$comp_weight_field
  w <- if (!is.null(field)) stan_data[[field]] else NULL
  # Survival has no comparator weight field; the pseudo-IPD count per arm,
  # expanded by the tie-aggregation multiplicities, is the population size.
  if (is.null(w) && identical(family, "survival")) {
    arm <- stan_data$agd_arm
    cnt <- stan_data$agd_count
    w <- if (!is.null(arm) && n_agd_rows >= 1L) {
      arm_int <- as.integer(arm)
      if (!is.null(cnt)) {
        if (length(cnt) != length(arm_int) || !all(is.finite(cnt)) ||
              any(cnt < 1)) {
          stop("`stan_data$agd_count` must hold one positive multiplicity per ",
               "retained AgD row.", call. = FALSE)
        }
        arm_int <- rep(arm_int, times = .as_count_integer(cnt))
      }
      counts <- tabulate(arm_int, nbins = n_agd_rows)
      # An arm with no pseudo-individuals is a data problem; say so rather
      # than silently equal-weight.
      if (any(counts <= 0L)) {
        warning("Some aggregate survival arm(s) have no reconstructed ",
                "pseudo-individuals, so covariate centering falls back to ",
                "equal row weights. Check the arm labels on the pseudo-IPD.",
                call. = FALSE)
      }
      counts
    } else {
      stan_data$n_agd
    }
  }
  if (is.null(w)) return(fallback)
  w <- as.numeric(w)
  if (length(w) == 1L && n_agd_rows > 1L) w <- rep(w / n_agd_rows, n_agd_rows)
  if (length(w) != n_agd_rows || !all(is.finite(w)) || any(w <= 0)) {
    return(fallback)
  }
  w
}


#' Refuse a centered integration grid that merged declared nodes
#'
#' Centering subtracts the pooled covariate mean from every integration point
#' in floating point. When the populations sit far from the origin relative
#' to the node spacing, two declared nodes can round to one, and the sampler
#' would integrate over a distribution other than the one declared. The check
#' counts the distinct values per covariate and row before and after
#' centering.
#' @param declared The integration grid as `add_integration()` stored it,
#'   `[n_agd_rows, n_int, n_cov]`.
#' @param fitted The grid the sampler receives, centered as the model centers
#'   it, with the same dimensions.
#' @param covariates The covariate names, in the grid's order.
#' @return `TRUE` invisibly; stops otherwise.
#' @noRd
.check_grid_nodes_kept <- function(declared, fitted, covariates = NULL) {
  if (is.null(declared) || is.null(fitted)) return(invisible(TRUE))
  d <- dim(fitted)
  if (length(d) != 3L || any(d == 0L) || !identical(dim(declared), d)) {
    return(invisible(TRUE))
  }
  for (k in seq_len(d[1L])) {
    for (j in seq_len(d[3L])) {
      before <- length(unique(declared[k, , j]))
      after <- length(unique(fitted[k, , j]))
      if (after >= before) next
      name <- if (length(covariates) >= j) paste0("`", covariates[[j]], "`") else paste("covariate", j)
      stop(errorCondition(paste0(
        "Centering the covariates merged integration points: on aggregate row ",
        k, ", ", name, " has ", before, " distinct values as declared and ",
        after, " on the centered grid, so the sampler would integrate over a ",
        "distribution other than the one declared. Re-express the covariates ",
        "near a common origin before `set_ipd()` and `set_agd()`, or fit with ",
        "`center = FALSE`."
      ), class = "mlumr_grid_representation", call = NULL))
    }
  }
  invisible(TRUE)
}


#' Center IPD and integration covariates about their pooled mean
#'
#' The intercept then sits at the average covariate, which removes the
#' intercept and slope collinearity that forces deep NUTS trajectories on
#' raw-scale covariates. The likelihood is unchanged; the intercept prior is
#' not. `cov_center` is stored whenever both covariate matrices are present
#' (zeros when `center = FALSE`) so the prediction functions can map raw
#' covariate values onto the model scale.
#' @noRd
.mlumr_center_covariates <- function(stan_data, center = TRUE,
                                     family = "binomial", agd_means = NULL) {
  if (is.null(stan_data$X_ipd) || is.null(stan_data$X_int)) {
    return(stan_data)
  }
  X_ipd <- as.matrix(stan_data$X_ipd)              # [n_ipd, n_cov]
  X_int <- stan_data$X_int                         # [n_agd_rows, n_int, n_cov]
  n_cov <- ncol(X_ipd)
  if (center) {
    n_ipd_rows <- nrow(X_ipd)
    n_agd_rows <- dim(X_int)[1]
    # Declared AgD means when supplied, so the center does not move with the
    # integration resolution.
    if (is.null(agd_means)) {
      agd_means <- apply(X_int, c(1, 3), mean)
    }
    agd_row_means <- matrix(as.numeric(agd_means), nrow = n_agd_rows,
                            ncol = n_cov)
    # Weight each AgD row by the population it represents, so the center does
    # not depend on how the evidence was tabulated.
    w_agd <- .agd_center_weights(stan_data, family, n_agd_rows)
    xbar <- (n_ipd_rows * colMeans(X_ipd) +
               colSums(agd_row_means * w_agd)) /
      (n_ipd_rows + sum(w_agd))
    stan_data$X_ipd <- sweep(X_ipd, 2, xbar)
    stan_data$X_int <- sweep(X_int, 3, xbar)
    stan_data$cov_center <- xbar
  } else {
    stan_data$cov_center <- rep(0, n_cov)
  }
  stan_data
}

#' Build the combined (intercepts + covariates) design and optional thin-QR
#'
#' Mirrors QR machinery: the design matrix `D` stacks the IPD rows and
#' all AgD integration rows, with leading dummy columns for the index and
#' comparator intercepts followed by the (centered) covariate columns. SPFA uses
#' one shared covariate block (`nB = 2 + n_cov`); the relaxed model uses
#' treatment-specific blocks (`nB = 2 + 2 * n_cov`). When `qr = TRUE` the design
#' is replaced by the scaled thin-QR factor `Q` (`Q = qr.Q(D) * sqrt(N - 1)`) and
#' `R_inv = solve(qr.R(D) / sqrt(N - 1))` is returned so Stan can recover the
#' original-scale coefficients via `allbeta = R_inv * beta_tilde`. When
#' `qr = FALSE`, `Xq_*` is the raw design `D` and `R_inv` is the identity, so
#' `allbeta = beta_tilde` and the linear predictor is unchanged. The original
#' (centered) `X_ipd` / `X_int` are kept for the generated-quantities block.
#' @noRd
.mlumr_qr_design <- function(stan_data, model = "spfa", qr = FALSE) {
  if (is.null(stan_data$X_ipd) || is.null(stan_data$X_int)) {
    return(stan_data)
  }
  X_ipd <- as.matrix(stan_data$X_ipd)              # [n_ipd, n_cov], centered
  X_int <- stan_data$X_int                         # [n_agd_rows, n_int, n_cov]
  n_ipd <- nrow(X_ipd)
  n_cov <- ncol(X_ipd)
  n_agd <- dim(X_int)[1]
  n_int <- dim(X_int)[2]
  # Arm-major flatten of the integration grid: row (k-1)*n_int + m = X_int[k, m, ].
  X_int_flat <- matrix(aperm(X_int, c(2, 1, 3)), nrow = n_agd * n_int, ncol = n_cov)

  zero_ipd <- matrix(0, n_ipd, n_cov)
  zero_int <- matrix(0, n_agd * n_int, n_cov)
  if (identical(model, "relaxed")) {
    # [I_index, I_comparator, beta_index cols, beta_comparator cols]
    nB <- 2L + 2L * n_cov
    d_ipd <- cbind(1, 0, X_ipd, zero_ipd)
    d_int <- cbind(0, 1, zero_int, X_int_flat)
  } else {
    # SPFA: [I_index, I_comparator, shared beta cols]
    nB <- 2L + n_cov
    d_ipd <- cbind(1, 0, X_ipd)
    d_int <- cbind(0, 1, X_int_flat)
  }
  design <- rbind(d_ipd, d_int)
  n_rows <- nrow(design)

  if (qr) {
    # A thin QR inverts R, so the design needs full column rank; a constant
    # covariate is collinear with the intercept.
    if (n_rows < nB) {
      stop(sprintf(paste0("`qr = TRUE` needs at least as many rows as design ",
                          "columns, but the combined design has %d row(s) and ",
                          "%d column(s). Use `qr = FALSE`."),
                   n_rows, nB), call. = FALSE)
    }
    design_rank <- qr(design)$rank
    if (design_rank < nB) {
      stop(sprintf(paste0("`qr = TRUE` needs a full-rank design, but the ",
                          "combined (intercepts + covariates) design has rank ",
                          "%d of %d columns. A constant or collinear covariate ",
                          "is the usual cause; a constant covariate is exactly ",
                          "collinear with the intercept. Drop it, or use ",
                          "`qr = FALSE`, which does not invert the design."),
                   design_rank, nB), call. = FALSE)
    }
    qr_decomp <- qr(design)
    scale_factor <- sqrt(n_rows - 1)
    q_mat <- qr.Q(qr_decomp) * scale_factor
    r_mat <- qr.R(qr_decomp) / scale_factor
    r_inv <- solve(r_mat)
    xq_ipd <- q_mat[seq_len(n_ipd), , drop = FALSE]
    xq_int_flat <- q_mat[(n_ipd + 1L):n_rows, , drop = FALSE]
  } else {
    xq_ipd <- d_ipd
    xq_int_flat <- d_int
    r_inv <- diag(nB)
  }
  # Reshape the AgD design rows back to [n_agd, n_int, nB] (inverse of the
  # arm-major flatten above) for Stan's `array[n_agd_rows] matrix[n_int, nB]`.
  xq_int <- aperm(array(xq_int_flat, dim = c(n_int, n_agd, nB)), c(2, 1, 3))

  stan_data$qr <- as.integer(qr)
  stan_data$nB <- nB
  stan_data$Xq_ipd <- xq_ipd
  stan_data$Xq_int <- xq_int
  stan_data$R_inv <- r_inv
  stan_data
}

#' Rank of the AgD comparator covariate design
#'
#' The relaxed model's `mu_comparator` and `beta_comparator` are informed only
#' by the AgD likelihood, which contributes one term per AgD row evaluated at
#' that row's integration grid. The number of comparator parameters those terms
#' can separate is therefore the rank of the per-row mean covariate profiles
#' augmented with an intercept column, not the number of rows: rows that repeat
#' the same covariate summaries add likelihood terms but no new direction.
#'
#' Uses the declared aggregate covariate means, which define the identity-link
#' design exactly and do not vary with integration resolution.
#'
#' @param data An `mlumr_data` object with integration points.
#' @return Integer rank, at least 1.
#' @noRd
.agd_covariate_rank <- function(data) {
  covs <- data$covariates
  ipd_cov <- data$ipd$data[, covs, drop = FALSE]
  ref_sd <- apply(as.matrix(ipd_cov), 2L, stats::sd)
  .profile_rank(.agd_mean_profiles(data), ref_sd)
}

#' Numerical rank of the aggregate mean profiles
#'
#' The companion to [.agd_covariate_rank()] that answers the different question
#' of whether the directions exist at all, rather than whether they are spread
#' widely enough to be informative in practice.
#' @param data An `mlumr_data` object.
#' @return Integer rank including the intercept.
#' @noRd
.agd_covariate_numeric_rank <- function(data) {
  covs <- data$covariates
  ipd_cov <- data$ipd$data[, covs, drop = FALSE]
  ref_sd <- apply(as.matrix(ipd_cov), 2L, stats::sd)
  .profile_numeric_rank(.agd_mean_profiles(data), ref_sd)
}

#' Map `aux_by` onto the Stan `n_strata` switch
#'
#' `".study"` (and `NULL`, as in multinma) means one baseline per study, 2;
#' `"none"` means one shared baseline, 1. `".trt"` is refused: each study
#' contributes one arm, so it would be the same stratification.
#' @param aux_by `NULL`, `".study"`, or `"none"`.
#' @return Integer number of baseline strata (1 or 2).
#' @noRd
.resolve_aux_strata <- function(aux_by) {
  if (is.null(aux_by)) return(2L)
  if (!is.character(aux_by) || length(aux_by) != 1L) {
    stop("`aux_by` must be NULL or \".study\" (a baseline per study), or ",
         "\"none\" (one shared baseline).", call. = FALSE)
  }
  if (identical(aux_by, ".study")) return(2L)
  if (identical(aux_by, "none")) return(1L)
  if (identical(aux_by, ".trt")) {
    stop("`aux_by = \".trt\"` is the same as \".study\" here: each study ",
         "contributes a single arm, so stratifying by treatment and by study ",
         "give the same two strata. Use \".study\".", call. = FALSE)
  }
  stop("`aux_by` must be NULL or \".study\" (a baseline per study, the ",
       "default, as in multinma), or \"none\" (one shared baseline). Got \"",
       aux_by, "\".", call. = FALSE)
}


#' Validate the two-source study contract of the survival Stan models
#' @noRd
.validate_survival_studies <- function(data, aux_by) {
  if (identical(aux_by, "none")) return(invisible(TRUE))
  ipd_studies <- unique(as.character(data$ipd$data$.study))
  agd_studies <- unique(as.character(data$agd$pseudo_ipd$.study))
  if (length(ipd_studies) != 1L || length(agd_studies) != 1L) {
    stop(
      "Survival fits with the default `aux_by = \".study\"` contract require ",
      "exactly one index study and one comparator study. The current Stan ",
      "models have two source-specific baselines and cannot represent multiple ",
      "studies within either source. Use `aux_by = \"none\"` only if complete ",
      "baseline pooling across those studies is scientifically intended.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Augment the Stan data list with survival-specific arrays
#'
#' @param stan_data The partially built Stan data list to add to.
#' @param data The combined `mlumr_data` object holding the IPD and pseudo-IPD.
#' @param surv_info Distribution metadata from `.survival_distribution_info()`.
#' @param pred_times Prediction grid, or `NULL` for the default grid.
#' @param n_knots Number of internal knots for a flexible baseline.
#' @param knots Explicit [make_knots()] result, or `NULL` to derive them.
#' @param rmst_horizon RMST restriction time, or `NULL` for the default.
#' @param n_rmst_grid Number of RMST integration points.
#' @param prior_aux Prior on the parametric auxiliary shape parameters.
#' @param prior_aux2 Prior on the second generalized-gamma auxiliary parameter,
#'   or `NULL` to reuse `prior_aux`.
#' @param prior_smooth Prior on the flexible-baseline smoothing SD.
#' @param n_strata Number of baseline strata (1 or 2), from `.resolve_aux_strata()`.
#' @return `stan_data` with the survival arrays, bases and grids added.
#' @noRd
.build_stan_data_survival <- function(stan_data, data, surv_info, pred_times,
                                      n_knots, knots = NULL, rmst_horizon,
                                      n_rmst_grid = 100L,
                                      prior_aux, prior_aux2 = NULL,
                                      prior_smooth, n_strata = 1L) {
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  arm_summary <- data$agd$data
  # 1 = one baseline shared by both studies; 2 = one per study (see `aux_by`).
  stan_data$n_strata <- as.integer(n_strata)

  # IPD survival arrays (index treatment)
  stan_data$ipd_time <- as.numeric(ipd$.time)
  stan_data$ipd_start_time <- as.numeric(ipd$.start_time)
  stan_data$ipd_delay_time <- as.numeric(ipd$.delay_time)
  stan_data$ipd_status <- as.integer(ipd$.status)

  # Comparator pseudo-IPD arrays + map each pseudo-individual to its arm
  stan_data$n_agd <- nrow(pseudo)
  stan_data$agd_time <- as.numeric(pseudo$.time)
  stan_data$agd_start_time <- as.numeric(pseudo$.start_time)
  stan_data$agd_delay_time <- as.numeric(pseudo$.delay_time)
  stan_data$agd_status <- as.integer(pseudo$.status)
  stan_data$agd_arm <- as.integer(match(pseudo$.arm, arm_summary$.arm))

  # Prediction + RMST grids (avoid t = 0 for the hazard-bearing pred grid).
  # Sort + unique so curve/median interpolation sees an increasing grid.
  max_time <- max(c(ipd$.time, pseudo$.time))
  if (is.null(pred_times)) {
    pred_times <- seq(max_time / 50, max_time, length.out = 50)
  }
  pred_times <- sort(unique(as.numeric(pred_times)))
  stan_data$n_pred_times <- length(pred_times)
  stan_data$pred_times <- pred_times

  # A stratified flexible baseline defaults to the follow-up both studies
  # observed, so the headline RMST does not extrapolate the shorter study.
  horizon <- rmst_horizon
  if (is.null(horizon)) {
    horizon <- if (surv_info$kind == "flexible" && n_strata > 1L) {
      min(max(ipd$.time), max(pseudo$.time))
    } else {
      max_time
    }
  }
  # RMST is the trapezoidal integral of the survival curve on this grid.
  rmst_grid <- seq(0, horizon, length.out = n_rmst_grid)
  stan_data$n_rmst_grid <- length(rmst_grid)
  stan_data$rmst_grid_times <- rmst_grid

  if (surv_info$kind == "parametric") {
    stan_data$dist <- surv_info$dist_code
    aux_fields <- stan_prior_fields(prior_aux)
    stan_data$prior_aux_location <- aux_fields$mean
    stan_data$prior_aux_scale <- aux_fields$sd
    stan_data$prior_aux_dist <- aux_fields$dist
    stan_data$prior_aux_df <- aux_fields$df
    # The second generalized-gamma auxiliary has its own four slots.
    aux2_fields <- stan_prior_fields(prior_aux2 %||% prior_aux)
    stan_data$prior_aux2_location <- aux2_fields$mean
    stan_data$prior_aux2_scale <- aux2_fields$sd
    stan_data$prior_aux2_dist <- aux2_fields$dist
    stan_data$prior_aux2_df <- aux2_fields$df
  } else {
    # One basis per stratum over its own observed support (multinma's
    # `type = "quantile"`): a pooled basis leaves the shorter study columns it
    # never observes, along which the intercept and the simplex trade off
    # with the likelihood flat.
    degree <- surv_info$mspline_degree
    if (n_strata > 1L) {
      if (is.null(knots)) {
        specs <- .matched_per_study_bases(ipd, pseudo, n_knots, degree)
        spec_idx <- specs$index
        spec_cmp <- specs$comparator
      } else {
        if (!is.list(knots) ||
            !all(c("index", "comparator") %in% names(knots))) {
          stop("With `aux_by = \".study\"`, `knots` must be a named list ",
               "with `index` and `comparator` knot specifications.",
               call. = FALSE)
        }
        k_idx <- .validate_user_knots(knots$index, max(ipd$.time), "index")
        k_cmp <- .validate_user_knots(knots$comparator, max(pseudo$.time),
                                      "comparator")
        spec_idx <- .build_mspline_basis(k_idx, degree)
        spec_cmp <- .build_mspline_basis(k_cmp, degree)
        if (spec_idx$n_scoef != spec_cmp$n_scoef) {
          stop("The index and comparator knot specifications must produce ",
               "the same number of spline coefficients.", call. = FALSE)
        }
        .assert_basis_support(spec_idx, max(ipd$.time), "index",
                              ipd$.delay_time, ipd$.time,
                              ipd$.time[ipd$.status == 1])
        .assert_basis_support(spec_cmp, max(pseudo$.time), "comparator",
                              pseudo$.delay_time, pseudo$.time,
                              pseudo$.time[pseudo$.status == 1])
      }
    } else {
      observed_max <- max(c(ipd$.time, pseudo$.time))
      knots_i <- if (is.null(knots)) {
        make_knots(data, n_knots = n_knots)
      } else {
        .validate_user_knots(knots, observed_max, "shared")
      }
      spec_idx <- spec_cmp <- .build_mspline_basis(knots_i, degree)
      # The shared basis needs the same support check: a boundary far past
      # the data leaves unsupported columns.
      .assert_basis_support(spec_idx, observed_max, "shared",
                            c(ipd$.delay_time, pseudo$.delay_time),
                            c(ipd$.time, pseudo$.time),
                            c(ipd$.time[ipd$.status == 1],
                              pseudo$.time[pseudo$.status == 1]))
      # Support per column is not enough with one shared simplex: the two
      # studies must overlap on some column.
      .assert_shared_basis_identified(
        spec_idx,
        list(index = list(observed_max = max(ipd$.time),
                          entry = ipd$.delay_time,
                          exit = ipd$.time,
                          event = ipd$.time[ipd$.status == 1]),
             comparator = list(observed_max = max(pseudo$.time),
                               entry = pseudo$.delay_time,
                               exit = pseudo$.time,
                               event = pseudo$.time[pseudo$.status == 1])))
    }
    spec <- spec_idx
    if (spec$n_scoef < 2L) {
      # Reachable only at degree 0 with no internal knot: one constant hazard
      # and no increments to smooth.
      stop("Baseline basis has < 2 coefficients, so the random-walk smoothing ",
           "prior has no increments to smooth. A degree-", spec$degree,
           " basis needs at least one internal knot for that; use ",
           "`n_knots >= 1`, or `distribution = \"exponential\"` if a single ",
           "constant hazard is what you want. Got n_scoef = ", spec$n_scoef,
           ".", call. = FALSE)
    }
    # The baseline is a constant hazard past the upper boundary knot.
    upper <- min(spec_idx$boundary[2], spec_cmp$boundary[2])
    if (max(pred_times) > upper || max(rmst_grid) > upper) {
      which_arm <- if (n_strata > 1L && spec_cmp$boundary[2] < spec_idx$boundary[2]) {
        "the comparator study's baseline (its follow-up is the shorter one)"
      } else if (n_strata > 1L && spec_idx$boundary[2] < spec_cmp$boundary[2]) {
        "the index study's baseline (its follow-up is the shorter one)"
      } else {
        "the M-spline baseline"
      }
      warning("Flexible-baseline prediction/RMST times extend beyond the ",
              "largest time that study observed (",
              format(upper, digits = 4L), "); ", which_arm,
              " is extrapolated as constant there, so extrapolated estimates ",
              "are unreliable. Reduce `pred_times` / `rmst_horizon`.",
              call. = FALSE)
    }
    stan_data$n_scoef <- spec$n_scoef
    # Each likelihood block uses its own stratum's basis; the prediction and
    # RMST grids are evaluated on both.
    stan_data$b_ipd <- .eval_basis(spec_idx, ipd$.time, integral = FALSE)
    stan_data$ib_ipd <- .eval_basis(spec_idx, ipd$.time, integral = TRUE)
    stan_data$ib_ipd_start <- .eval_basis(spec_idx, ipd$.start_time, integral = TRUE)
    stan_data$ib_ipd_delay <- .eval_basis(spec_idx, ipd$.delay_time, integral = TRUE)
    stan_data$b_agd <- .eval_basis(spec_cmp, pseudo$.time, integral = FALSE)
    stan_data$ib_agd <- .eval_basis(spec_cmp, pseudo$.time, integral = TRUE)
    stan_data$ib_agd_start <- .eval_basis(spec_cmp, pseudo$.start_time, integral = TRUE)
    stan_data$ib_agd_delay <- .eval_basis(spec_cmp, pseudo$.delay_time, integral = TRUE)
    stan_data$pred_basis <- .eval_basis(spec_idx, pred_times, integral = FALSE)
    stan_data$pred_ibasis <- .eval_basis(spec_idx, pred_times, integral = TRUE)
    stan_data$rmst_ibasis <- .eval_basis(spec_idx, rmst_grid, integral = TRUE)
    stan_data$pred_basis_cmp <- .eval_basis(spec_cmp, pred_times, integral = FALSE)
    stan_data$pred_ibasis_cmp <- .eval_basis(spec_cmp, pred_times, integral = TRUE)
    stan_data$rmst_ibasis_cmp <- .eval_basis(spec_cmp, rmst_grid, integral = TRUE)
    # Per stratum, since the constant-hazard centering and RW1 weights are
    # properties of a basis.
    stan_data$lscoef_prior_mean <- cbind(.mspline_constant_hazard(spec_idx),
                                         .mspline_constant_hazard(spec_cmp))[, seq_len(n_strata),
                                                                             drop = FALSE]
    stan_data$lscoef_weights <- cbind(.rw1_prior_weights(spec_idx),
                                      .rw1_prior_weights(spec_cmp))[, seq_len(n_strata),
                                                                    drop = FALSE]
    smooth_fields <- stan_prior_fields(prior_smooth)
    stan_data$prior_sigma_smooth_location <- smooth_fields$mean
    stan_data$prior_sigma_smooth_scale <- smooth_fields$sd
    stan_data$prior_sigma_smooth_dist <- smooth_fields$dist
    stan_data$prior_sigma_smooth_df <- smooth_fields$df
  }
  stan_data
}


#' Validate user-supplied flexible-baseline knots
#' @noRd
.validate_user_knots <- function(knots, max_time, label) {
  if (!is.list(knots) ||
      !all(c("internal", "boundary") %in% names(knots))) {
    stop("The ", label, " knot specification must contain `internal` and ",
         "`boundary`.", call. = FALSE)
  }
  internal <- knots$internal
  boundary <- knots$boundary
  valid_internal <- is.numeric(internal) && all(is.finite(internal)) &&
    (length(internal) < 2L || all(diff(internal) > 0))
  valid_boundary <- is.numeric(boundary) && length(boundary) == 2L &&
    all(is.finite(boundary)) && boundary[1] == 0 && boundary[2] >= max_time
  if (!valid_internal || !valid_boundary ||
      any(internal <= boundary[1] | internal >= boundary[2])) {
    stop("The ", label, " knots must have strictly increasing finite internal ",
         "knots inside `boundary = c(0, upper)`, with `upper` covering all ",
         "observed times for that baseline.", call. = FALSE)
  }
  list(internal = internal, boundary = boundary,
       n_knots = length(internal))
}

#' Validate `adapt_delta`, wherever it arrived from
#'
#' Shared by the argument validator and the rstan control merge.
#' @param adapt_delta The value to check.
#' @return `NULL`, invisibly; called for the error.
#' @noRd
.validate_mlumr_adapt_delta <- function(adapt_delta) {
  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L ||
        !is.finite(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) {
    stop("`adapt_delta` must be a single finite number between 0 and 1.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Validate mlumr() sampler controls before backend dispatch
#' @noRd
.validate_mlumr_sampling_args <- function(chains, iter, warmup, seed,
                                          adapt_delta, max_treedepth,
                                          refresh) {
  .validate_mlumr_integer(chains, "chains", lower = 1L)
  .validate_mlumr_integer(iter, "iter", lower = 1L)
  .validate_mlumr_integer(warmup, "warmup", lower = 0L)
  if (warmup >= iter) {
    stop("`warmup` must be smaller than `iter` so post-warmup draws remain.",
         call. = FALSE)
  }
  if (!is.null(seed)) {
    .validate_mlumr_integer(seed, "seed", lower = 0L)
  }
  .validate_mlumr_adapt_delta(adapt_delta)
  .validate_mlumr_integer(max_treedepth, "max_treedepth", lower = 1L)
  .validate_mlumr_integer(refresh, "refresh", lower = 0L)
  invisible(TRUE)
}


#' Validate an integer-like mlumr() argument
#' @noRd
.validate_mlumr_integer <- function(x, name, lower) {
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    is.finite(x) &&
    x == floor(x) &&
    x >= lower &&
    x <= .Machine$integer.max
  if (!valid) {
    # Enforce the upper bound too: without it a value such as seed = 2^31 passes
    # every finite/floor check here and only becomes NA_integer_ later at
    # as.integer(), silently discarding the seed.
    stop(sprintf("`%s` must be a single integer >= %d and <= %.0f.",
                 name, lower, .Machine$integer.max), call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate survival-specific prediction grids and spline controls
#' @noRd
.validate_survival_controls <- function(pred_times, rmst_horizon,
                                        mspline_degree, n_knots,
                                        n_rmst_grid = 100L,
                                        distribution = NULL,
                                        knots = NULL) {
  valid_grid <- is.numeric(n_rmst_grid) && length(n_rmst_grid) == 1L &&
    is.finite(n_rmst_grid) && n_rmst_grid >= 2 &&
    n_rmst_grid == floor(n_rmst_grid)
  if (!valid_grid) {
    stop("`n_rmst_grid` must be a single integer >= 2 (RMST trapezoid nodes).",
         call. = FALSE)
  }
  if (!is.null(pred_times)) {
    valid <- is.numeric(pred_times) && length(pred_times) >= 1L &&
      all(is.finite(pred_times)) && all(pred_times > 0)
    if (!valid) {
      stop("`pred_times` must be finite, positive numbers.", call. = FALSE)
    }
  }
  if (!is.null(rmst_horizon)) {
    valid <- is.numeric(rmst_horizon) && length(rmst_horizon) == 1L &&
      is.finite(rmst_horizon) && rmst_horizon > 0
    if (!valid) {
      stop("`rmst_horizon` must be a single finite, positive number.",
           call. = FALSE)
    }
  }
  if (!is.null(mspline_degree)) {
    valid <- is.numeric(mspline_degree) && length(mspline_degree) == 1L &&
      is.finite(mspline_degree) && mspline_degree >= 0 &&
      mspline_degree == floor(mspline_degree)
    if (!valid) {
      stop("`mspline_degree` must be a non-negative integer.", call. = FALSE)
    }
  }
  # `n_knots` is read only when no custom knots are supplied.
  if (!is.null(knots)) {
    return(invisible(TRUE))
  }
  valid_knots <- is.numeric(n_knots) && length(n_knots) == 1L &&
    is.finite(n_knots) && n_knots >= 0 && n_knots == floor(n_knots) &&
    n_knots <= 50
  if (!valid_knots) {
    stop("`n_knots` must be an integer in [0, 50]. (Flexible baselines rarely ",
         "need more than a handful of internal knots; the cap also blocks ",
         "accidental enormous allocations.)", call. = FALSE)
  }
  # At degree 0 no internal knot leaves one coefficient and no increments for
  # the smoothing prior; a cubic basis with no internal knots still has four.
  if (!is.null(distribution) && identical(distribution, "pexp") && n_knots < 1) {
    stop("`n_knots = 0` cannot be used with `distribution = \"pexp\"`: a ",
         "degree-0 basis with no internal knots leaves a single spline ",
         "coefficient, and the random-walk smoothing prior is defined on ",
         "differences between coefficients, so it needs at least two. Use ",
         "`n_knots >= 1`, or `distribution = \"exponential\"` for a constant ",
         "baseline hazard.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve and validate mlumr() backend engine
#' @noRd
.resolve_mlumr_engine <- function(engine) {
  .validate_engine_name(engine %||% get_engine())
}

#' Resolve the sampling seed
#'
#' An explicit `seed` wins; otherwise the fixed default 2026 is used with a
#' warning, rather than a draw from the session RNG that nothing records.
#' @noRd
.resolve_mlumr_seed <- function(seed) {
  if (!is.null(seed)) {
    return(list(value = as.integer(seed), source = "user"))
  }
  warning("No `seed` supplied; using the default seed 2026 so the fit is ",
          "reproducible. Pass `seed = ` to control it.", call. = FALSE)
  list(value = 2026L, source = "default")
}

#' Log mlumr() fit metadata
#' @noRd
.mlumr_log_fit_start <- function(model_name, family, link, stan_data,
                                 engine, seed_info, verbose) {
  mlumr_message(sprintf("Fitting ML-UMR (%s, %s, link=%s)...",
                        model_name, family, link),
                verbose = verbose)
  mlumr_message(sprintf("  IPD: n = %d", stan_data$n_ipd),
                verbose = verbose)

  if (family == "binomial") {
    mlumr_message(sprintf("  AgD: %d rows, total n = %d",
                          stan_data$n_agd_rows, sum(stan_data$n_agd)),
                  verbose = verbose)
  } else if (family == "normal") {
    mlumr_message(sprintf("  AgD: %d rows", stan_data$n_agd_rows),
                  verbose = verbose)
  } else if (family == "survival") {
    mlumr_message(sprintf("  AgD: %d rows, %d reconstructed pseudo-individuals",
                          stan_data$n_agd_rows, stan_data$n_agd),
                  verbose = verbose)
  } else {
    mlumr_message(sprintf("  AgD: %d rows, total exposure = %.1f",
                          stan_data$n_agd_rows, sum(stan_data$E_agd)),
                  verbose = verbose)
  }

  mlumr_message(sprintf("  Covariates: %d, Integration points: %d",
                        stan_data$n_cov, stan_data$n_int),
                verbose = verbose)
  mlumr_message(sprintf("  Engine: %s", engine), verbose = verbose)
  seed_note <- if (identical(seed_info$source, "default")) " (default)" else ""
  mlumr_message(sprintf("  Seed: %d%s", seed_info$value, seed_note),
                verbose = verbose)
}

#' Dispatch mlumr() sampling to the selected backend
#' @noRd
.mlumr_fit_backend <- function(engine, model_name, stan_data, chains, iter,
                               warmup, seed, adapt_delta, max_treedepth,
                               refresh, verbose = TRUE, ...) {
  if (engine == "cmdstanr") {
    fit_cmdstanr(model_name, stan_data, chains, iter, warmup,
                 seed, adapt_delta, max_treedepth, refresh,
                 verbose = verbose, ...)
  } else {
    fit_rstan(model_name, stan_data, chains, iter, warmup,
              seed, adapt_delta, max_treedepth, refresh, ...)
  }
}

#' Store user priors plus the resolved Stan-scale beta prior
#' @noRd
.mlumr_prior_metadata <- function(data, family, model = "spfa",
                                  prior_intercept, prior_beta,
                                  prior_beta_comparator = NULL,
                                  prior_sigma,
                                  beta_fields,
                                  beta_comparator_fields = NULL,
                                  sd_x,
                                  prior_aux = NULL, prior_aux2 = NULL,
                                  prior_smooth = NULL,
                                  surv_info = NULL,
                                  intercept_resolved = NULL,
                                  sigma_resolved = NULL,
                                  sd_y = 1) {
  # `intercept` and `sigma` are what the user passed, which a refit replays;
  # the `_resolved` entries are what the model used.
  priors <- list(
    intercept = prior_intercept,
    intercept_resolved = intercept_resolved %||% prior_intercept,
    beta = prior_beta,
    beta_resolved = list(
      covariate_names = data$covariates,
      mean = beta_fields$mean,
      sd = beta_fields$sd,
      dist = beta_fields$dist,
      df = beta_fields$df,
      autoscale = beta_fields$autoscale,
      sd_x = sd_x,
      sd_y = beta_fields$sd_y
    )
  )

  # Only relaxed fits consume the comparator prior; NULL resolved to
  # prior_beta, and the resolved block reflects that.
  if (identical(model, "relaxed")) {
    priors$beta_comparator <- prior_beta_comparator %||% prior_beta
    if (!is.null(beta_comparator_fields)) {
      priors$beta_comparator_resolved <- list(
        covariate_names = data$covariates,
        mean = beta_comparator_fields$mean,
        sd = beta_comparator_fields$sd,
        dist = beta_comparator_fields$dist,
        df = beta_comparator_fields$df,
        autoscale = beta_comparator_fields$autoscale,
        sd_x = sd_x,
        sd_y = beta_comparator_fields$sd_y,
        user_specified = !is.null(prior_beta_comparator)
      )
    }
  }

  if (family == "normal") {
    priors$sigma <- prior_sigma
    priors$sigma_resolved <- sigma_resolved %||% prior_sigma
    priors$outcome_sd <- sd_y
  }

  if (family == "survival") {
    if (!is.null(surv_info) && surv_info$n_aux > 0) {
      priors$aux <- prior_aux
      if (surv_info$n_aux > 1L) priors$aux2 <- prior_aux2 %||% prior_aux
    }
    if (!is.null(surv_info) && surv_info$kind == "flexible") {
      priors$smooth <- prior_smooth
    }
  }

  priors
}

#' Choose M-spline knots for a flexible-baseline survival model
#'
#' Place boundary and internal knots for the M-spline baseline hazard used by
#' the flexible survival models (`distribution = "mspline"` or `"pexp"`). Knots
#' are chosen from the pooled event/censoring times of the index IPD and the
#' reconstructed comparator pseudo-IPD.
#'
#' @param data An `mlumr_data` object (survival family) from [combine_data()].
#' @param n_knots Number of internal knots (default 7; capped at 50). Use a
#'   smaller value when events are scarce; the recommended rule of thumb is to
#'   keep the number of spline coefficients (`n_knots + degree + 1`) below half
#'   the number of events.
#' @param type Internal-knot placement: `"quantile"` (default, at quantiles of
#'   the pooled event times) or `"equal"` (evenly spaced between the boundaries).
#'
#' @return A list with `internal` (internal knot locations), `boundary` (lower
#'   and upper boundary knots) and `n_knots` (the realized number of internal
#'   knots after dropping any that coincide with the boundaries).
#'
#' @details
#' The lower boundary knot is fixed at 0 (not the minimum delayed-entry time),
#' so the cumulative hazard is anchored at `H(0) = 0` and stays continuous with
#' the backward constant-hazard extrapolation; delayed-entry times (> 0) are
#' evaluated on the basis. The upper boundary knot is the maximum observed time
#' across both data sources. The M-spline basis is normalized so that the
#' baseline cumulative hazard equals 1 at the upper boundary; the hazard scale
#' is carried by the model intercepts.
#'
#' @examples
#' \dontrun{
#' # Build a survival network, then choose M-spline baseline knots:
#' dat <- combine_data(index_ipd, comparator_agd)
#' knots <- make_knots(dat, n_knots = 5)
#' knots$boundary  # lower (0) and upper boundary knots
#' knots$internal  # internal knot locations
#' }
#' @seealso [mlumr()] with `distribution = "mspline"`.
#' @export
make_knots <- function(data, n_knots = 7, type = c("quantile", "equal")) {
  type <- match.arg(type)
  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data()", call. = FALSE)
  }
  if ((data$family %||% "") != "survival") {
    stop("make_knots() requires a survival mlumr_data object", call. = FALSE)
  }
  if (!is.numeric(n_knots) || length(n_knots) != 1L || n_knots < 0 ||
        n_knots != floor(n_knots) || n_knots > 50) {
    stop("`n_knots` must be an integer in [0, 50]", call. = FALSE)
  }

  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  all_times <- c(ipd$.time, pseudo$.time)
  event_times <- c(ipd$.time[ipd$.status == 1], pseudo$.time[pseudo$.status == 1])
  .knots_from_times(all_times, event_times, n_knots, type)
}


#' Knot placement from a single set of survival times
#'
#' The core of [make_knots()], applied either to the pooled times (one shared
#' baseline) or to each study's own times (`aux_by = ".study"`). Per-study
#' boundary knots keep a stratified baseline identified: a basis function with
#' no support over a study's observed period leaves its spline scale free to
#' trade off against the intercept.
#'
#' @param all_times All observed times for the stratum (events and censorings).
#' @param event_times Event times only; falls back to `all_times` if empty.
#' @param n_knots Number of internal knots.
#' @param type `"quantile"` (event-time quantiles) or `"equal"` (equally spaced).
#' @return A list with `internal`, `boundary`, and `n_knots`.
#' @noRd
.knots_from_times <- function(all_times, event_times, n_knots,
                              type = c("quantile", "equal")) {
  type <- match.arg(type)
  if (length(event_times) == 0L) event_times <- all_times

  # Anchor at t = 0, not at the minimum entry time, so H(0) = 0 stays
  # continuous with the backward extrapolation under delayed entry.
  lower <- 0
  upper <- max(all_times, na.rm = TRUE)
  if (!is.finite(upper) || upper <= lower) {
    stop("Could not determine valid boundary knots from the survival times",
         call. = FALSE)
  }

  internal <- numeric(0)
  if (n_knots >= 1L) {
    probs <- seq_len(n_knots) / (n_knots + 1)
    internal <- if (type == "quantile") {
      unname(stats::quantile(event_times, probs = probs, names = FALSE))
    } else {
      lower + probs * (upper - lower)
    }
  }
  internal <- sort(unique(internal[internal > lower & internal < upper]))

  list(internal = internal, boundary = c(lower, upper), n_knots = length(internal))
}


#' Build an M-spline basis specification
#'
#' @param knots A list from [make_knots()] (`internal`, `boundary`).
#' @param degree Spline degree: 3 (cubic M-spline) or 0 (piecewise exponential).
#' @return A basis spec list with `internal`, `boundary`, `degree`, `n_scoef`.
#' @noRd
.build_mspline_basis <- function(knots, degree) {
  spec <- list(
    internal = knots$internal,
    boundary = knots$boundary,
    degree = as.integer(degree)
  )
  probe <- splines2::mSpline(
    mean(knots$boundary),
    knots = spec$internal, degree = spec$degree,
    Boundary.knots = spec$boundary, intercept = TRUE
  )
  spec$n_scoef <- ncol(probe)
  spec
}


#' Evaluate an M-spline (or integrated I-spline) basis at given times
#'
#' Values outside the boundary knots are extrapolated with constant boundary
#' hazards. For the integrated basis, this adds a linear tail beyond the
#' boundary so cumulative hazards continue increasing. Times at or before zero
#' contribute no cumulative hazard.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param times Numeric vector of evaluation times.
#' @param integral If `TRUE`, return the integrated (I-spline) basis; otherwise
#'   the M-spline basis.
#' @return A numeric matrix with `length(times)` rows and `spec$n_scoef` columns.
#' @noRd
.eval_basis <- function(spec, times, integral = FALSE) {
  if (length(times) == 0L) {
    return(matrix(numeric(0), nrow = 0L, ncol = spec$n_scoef))
  }
  lower <- spec$boundary[1]
  upper <- spec$boundary[2]
  times_clamped <- pmin(pmax(times, lower), upper)
  basis <- splines2::mSpline(
    times_clamped,
    knots = spec$internal, degree = spec$degree,
    Boundary.knots = spec$boundary, intercept = TRUE, integral = integral
  )
  basis <- matrix(as.numeric(basis), nrow = length(times), ncol = spec$n_scoef)
  if (integral) {
    before_zero <- times <= 0
    before_lower <- times > 0 & times < lower
    after_upper <- times > upper

    if (any(before_lower)) {
      lower_haz <- splines2::mSpline(
        rep(lower, sum(before_lower)),
        knots = spec$internal, degree = spec$degree,
        Boundary.knots = spec$boundary, intercept = TRUE, integral = FALSE
      )
      lower_haz <- matrix(as.numeric(lower_haz), nrow = sum(before_lower),
                          ncol = spec$n_scoef)
      basis[before_lower, ] <- times[before_lower] * lower_haz
    }

    if (any(after_upper)) {
      upper_haz <- splines2::mSpline(
        rep(upper, sum(after_upper)),
        knots = spec$internal, degree = spec$degree,
        Boundary.knots = spec$boundary, intercept = TRUE, integral = FALSE
      )
      upper_haz <- matrix(as.numeric(upper_haz), nrow = sum(after_upper),
                          ncol = spec$n_scoef)
      basis[after_upper, ] <- basis[after_upper, ] +
        (times[after_upper] - upper) * upper_haz
    }

    if (any(before_zero)) {
      basis[before_zero, ] <- 0
    }
  }
  basis
}


#' RW1 anchor for the log-ratio spline coefficients (centers on a flat baseline)
#'
#' Returns the inverse-softmax (length `n_scoef - 1`) of the M-spline coefficient
#' vector that produces a **constant baseline hazard**, so the RW1 smoothing
#' prior is centered on a flat baseline even when the knots are unevenly spaced.
#' Combined in Stan as `softmax(append_row(0, lscoef_prior_mean))`, this recovers
#' the constant-hazard simplex exactly. Uses the knot-spacing construction of
#' Jackson (arXiv:2306.03957); reimplemented from `multinma`
#' (GPL-3, `multinma:::mspline_constant_hazard`).
#' @noRd
.mspline_constant_hazard <- function(spec) {
  ord <- spec$degree + 1L
  n <- spec$n_scoef
  knots <- c(rep(spec$boundary[1], ord), spec$internal, rep(spec$boundary[2], ord))
  coefs <- (knots[(1:n) + ord] - knots[1:n]) / (ord * diff(spec$boundary))
  log(coefs[-1]) - log(coefs[1])           # inverse softmax (reference = coef 1)
}


#' Knot-spacing-aware RW1 step weights for the spline coefficients
#'
#' Returns `sqrt` of the normalized knot gaps (length `n_scoef - 1`) so the RW1
#' increments are scaled by interval width under unevenly spaced knots.
#' Reimplemented from `multinma` (GPL-3, `multinma:::rw1_prior_weights`).
#' @noRd
.rw1_prior_weights <- function(spec) {
  ord <- spec$degree + 1L
  n <- spec$n_scoef
  knots <- c(rep(spec$boundary[1], ord), spec$internal, rep(spec$boundary[2], ord))
  wts <- if (ord == 1L) {
    (knots[2:n] - knots[1:(n - 1)]) / (knots[n] - spec$boundary[1])
  } else {
    (knots[(ord + 1):(n + ord - 1)] - knots[2:n]) / ((ord - 1) * diff(spec$boundary))
  }
  sqrt(wts)
}



#' Per-study M-spline bases of matching dimension
#'
#' One basis per stratum from that study's own observed times, which is what
#' keeps a stratified flexible baseline identified. The Stan models share one
#' simplex dimension across strata, so when tied event times collapse
#' quantile knots in one study only, the internal-knot count is reduced until
#' both studies agree; a pooled fallback would restore the nonidentified
#' configuration the per-study knots exist to prevent.
#'
#' @param ipd The index study's individual data (`.time`, `.status`).
#' @param pseudo The comparator study's reconstructed pseudo-IPD.
#' @param n_knots Requested number of internal knots.
#' @param degree Spline degree (3 = cubic M-spline, 0 = piecewise exponential).
#' @return A list with `index` and `comparator` basis specs of equal
#'   `n_scoef`, and `n_knots` (the realized count actually used).
#' @noRd
.matched_per_study_bases <- function(ipd, pseudo, n_knots, degree) {
  build <- function(nk) {
    k_idx <- .knots_from_times(ipd$.time, ipd$.time[ipd$.status == 1], nk)
    k_cmp <- .knots_from_times(pseudo$.time, pseudo$.time[pseudo$.status == 1], nk)
    list(index = .build_mspline_basis(k_idx, degree),
         comparator = .build_mspline_basis(k_cmp, degree))
  }

  # A degree-3 basis with no internal knots still has four coefficients; a
  # degree-0 one collapses to a constant hazard, so it keeps one knot.
  min_nk <- if (degree >= 1L) 0L else 1L

  nk <- as.integer(n_knots)
  repeat {
    specs <- build(nk)
    if (specs$index$n_scoef == specs$comparator$n_scoef) break
    nk <- nk - 1L
    if (nk < min_nk) {
      stop("Could not place per-study M-spline knots of equal dimension: the ",
           "index and comparator studies realize different numbers of internal ",
           "knots at every `n_knots` down to ", min_nk, ", which happens when ",
           "tied event times collapse quantile knots in one study only. Reduce ",
           "the number of distinct tied times in the reconstructed comparator ",
           "curve, or fit a parametric `distribution` instead.",
           call. = FALSE)
    }
  }

  if (nk < as.integer(n_knots)) {
    message("Reduced `n_knots` from ", n_knots, " to ", nk,
            " so the index and comparator M-spline bases have the same ",
            "dimension (tied event times collapsed quantile knots in one ",
            "study). Each study still gets knots over its own observed times.")
  }

  specs$n_knots <- nk
  # Support is judged over the period each study was at risk, so a column
  # living only before the earliest entry counts as unsupported.
  .assert_basis_support(specs$index, max(ipd$.time), "index",
                        ipd$.delay_time, ipd$.time,
                        ipd$.time[ipd$.status == 1])
  .assert_basis_support(specs$comparator, max(pseudo$.time), "comparator",
                        pseudo$.delay_time, pseudo$.time,
                        pseudo$.time[pseudo$.status == 1])
  specs
}


#' Which basis columns carry likelihood over one study's observed risk set
#'
#' Shared by [.assert_basis_support()] and
#' [.assert_shared_basis_identified()], so what counts as support is decided
#' once.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param observed_max Largest observed time; used when `exit` is absent.
#' @param entry,exit,event Delayed-entry, exit and event times.
#' @return A logical vector with one entry per basis column. All `TRUE` when
#'   there is no risk period to evaluate over, which is not this function's to
#'   refuse.
#' @noRd
.live_basis_columns <- function(spec, observed_max, entry = NULL, exit = NULL,
                                event = NULL) {
  # Evaluate at structural points (knots, inter-knot midpoints, boundaries),
  # not a uniform grid that can miss a narrow degree-0 interval.
  risk <- .risk_intervals(entry, exit, observed_max)
  knots <- sort(unique(c(spec$boundary, spec$internal)))
  # Only within each covered stretch: nobody is at risk in the gaps.
  grid <- unlist(lapply(risk, function(iv) {
    b <- sort(unique(c(iv[["lo"]], iv[["hi"]],
                       knots[knots > iv[["lo"]] & knots < iv[["hi"]]])))
    if (length(b) < 2L) {
      return(numeric(0))
    }
    mids <- (utils::head(b, -1L) + b[-1L]) / 2
    inner <- seq(iv[["lo"]], iv[["hi"]], length.out = 66L)
    # Strictly inside: an endpoint carries no exposure.
    c(mids, inner[-c(1L, length(inner))])
  }))
  # The event term evaluates the hazard at each event time, so a column
  # positive only there still carries likelihood.
  grid <- c(grid, event[is.finite(event)])
  grid <- sort(unique(grid[is.finite(grid)]))
  if (!length(grid)) {
    return(rep(TRUE, spec$n_scoef))
  }
  b <- .eval_basis(spec, grid, integral = FALSE)
  # Support is structural: positive somewhere, whatever the time unit.
  apply(is.finite(b) & b > 0, 2, any)
}


#' Refuse a shared baseline whose weights float against the study intercepts
#'
#' With `aux_by = "none"` the model carries one weight simplex and a
#' separate intercept per study, so if the studies' exposure falls on
#' disjoint sets of basis columns, mass can be moved between the sets and
#' absorbed exactly by the intercepts while the treatment contrast moves
#' freely. What rules it out is that the studies and the columns they touch
#' form one connected component. Connectivity is exact for a degree-0 basis
#' and necessary rather than sufficient above it.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param studies A named list; each element a list with `observed_max`,
#'   `entry`, `exit` and `event`.
#' @return `TRUE`, invisibly.
#' @noRd
.assert_shared_basis_identified <- function(spec, studies) {
  inc <- vapply(studies, function(st) {
    .live_basis_columns(spec, st$observed_max, st$entry, st$exit, st$event)
  }, logical(spec$n_scoef))
  inc <- matrix(inc, nrow = spec$n_scoef, ncol = length(studies),
                dimnames = list(NULL, names(studies)))
  if (ncol(inc) < 2L) {
    return(invisible(TRUE))
  }
  # Grow one component out from the first study.
  seen_study <- c(TRUE, rep(FALSE, ncol(inc) - 1L))
  seen_col <- rep(FALSE, nrow(inc))
  repeat {
    next_col <- seen_col | apply(inc[, seen_study, drop = FALSE], 1, any)
    next_study <- seen_study | apply(inc[next_col, , drop = FALSE], 2, any)
    if (identical(next_col, seen_col) && identical(next_study, seen_study)) {
      break
    }
    seen_col <- next_col
    seen_study <- next_study
  }
  if (all(seen_study)) {
    return(invisible(TRUE))
  }
  reached <- names(studies)[seen_study]
  cut_off <- names(studies)[!seen_study]
  stop("The shared baseline is unidentified: ",
       paste(reached, collapse = ", "), " and ",
       paste(cut_off, collapse = ", "),
       " are observed over disjoint sets of spline columns, so the weights on ",
       "one set can be rescaled and absorbed exactly by the study intercepts. ",
       "Give each study its own baseline with `aux_by = \".study\"`, or ",
       "reduce `n_knots` until the studies share a column.", call. = FALSE)
}


#' Stop if any basis column has no support over a study's observed period
#'
#' An unsupported column is the nonidentification condition: simplex mass can
#' be parked on it and traded against the study intercept at no cost in fit.
#'
#' @param spec A basis spec from [.build_mspline_basis()].
#' @param observed_max The largest time that study actually observed.
#' @param label Study label used in the error message.
#' @param entry,exit The study's per-subject entry and exit times; omit both
#'   for data with no delayed entry.
#' @param event The study's event times, or `NULL`.
#' @return `TRUE`, invisibly.
#' @noRd
.assert_basis_support <- function(spec, observed_max, label,
                                  entry = NULL, exit = NULL, event = NULL) {
  risk <- .risk_intervals(entry, exit, observed_max)
  at_risk_start <- risk[[1L]][["lo"]]
  live <- .live_basis_columns(spec, observed_max, entry, exit, event)
  dead <- which(!live)
  if (length(dead) > 0L) {
    where <- if (length(risk) > 1L || at_risk_start > 0) {
      paste0("its observed risk set (",
             paste(vapply(risk, function(iv) {
               sprintf("[%s, %s]", format(iv[["lo"]], digits = 4),
                       format(iv[["hi"]], digits = 4))
             }, character(1)), collapse = " and "), ")")
    } else {
      "its observed follow-up"
    }
    stop("The ", label, " study's M-spline basis has ", length(dead),
         " column(s) with no support over ", where, " (columns ",
         paste(dead, collapse = ", "), "). That is an exact likelihood ridge: ",
         "the spline scale is unidentified against the study intercept. ",
         if (length(risk) > 1L || at_risk_start > 0) {
           paste0("Nobody is under observation outside that risk set, so a ",
                  "column supported only there enters no event hazard and no ",
                  "exposure increment. ")
         } else {
           ""
         },
         "Reduce `n_knots`.", call. = FALSE)
  }
  if (at_risk_start > 0) {
    # Below the earliest entry the hazard is extrapolated under the spline
    # restrictions; conditioning on survival to a landmark cancels it.
    message("The ", label, " study enters at ",
            format(at_risk_start, digits = 4),
            ", so nobody was at risk below that time and its hazard there is ",
            "extrapolated under the spline restrictions rather than observed. ",
            "Absolute survival and RMST integrate from 0 and depend on that ",
            "stretch; conditioning on survival to a landmark cancels it, so ",
            "compare survival conditional on reaching entry, or report RMST ",
            "from a landmark at or after it.")
  }
  invisible(TRUE)
}


#' The stretches of time a study actually had someone under observation
#'
#' The union of each subject's `[entry, exit]`, merged, so a gap with an
#' empty risk set is not treated as observed. Without entry times this is
#' one interval from zero.
#'
#' @param entry Entry times, or `NULL`.
#' @param exit Exit times, or `NULL`.
#' @param observed_max Last observed time, used when `exit` is absent.
#' @return A list of `c(lo, hi)` intervals, in increasing order.
#' @noRd
.risk_intervals <- function(entry, exit, observed_max) {
  whole <- list(c(lo = 0, hi = observed_max))
  if (is.null(entry) || !is.numeric(entry) || !length(entry)) {
    return(whole)
  }
  if (is.null(exit) || !is.numeric(exit) || length(exit) != length(entry)) {
    # Entry times without exits still say where observation starts.
    lo <- suppressWarnings(min(entry[is.finite(entry)]))
    if (!is.finite(lo)) {
      return(whole)
    }
    return(list(c(lo = max(0, lo), hi = observed_max)))
  }
  # Compare against the clamped lower bound, so an interval entirely before
  # zero is dropped rather than inverted.
  keep <- is.finite(entry) & is.finite(exit) & exit > pmax(0, entry)
  if (!any(keep)) {
    return(whole)
  }
  lo <- pmax(0, entry[keep])
  hi <- exit[keep]
  ord <- order(lo)
  lo <- lo[ord]
  hi <- hi[ord]
  out <- list()
  cur <- c(lo = lo[[1L]], hi = hi[[1L]])
  for (i in seq_along(lo)[-1L]) {
    if (lo[[i]] <= cur[["hi"]]) {
      cur[["hi"]] <- max(cur[["hi"]], hi[[i]])
    } else {
      out[[length(out) + 1L]] <- cur
      cur <- c(lo = lo[[i]], hi = hi[[i]])
    }
  }
  out[[length(out) + 1L]] <- cur
  out
}

#' Naive unadjusted indirect comparison
#'
#' Compute an unadjusted (naive) indirect treatment comparison by comparing
#' crude outcomes from the IPD and AgD without any covariate adjustment.
#' The index outcome remains marginal over the index-study population and the
#' comparator outcome remains marginal over the comparator population. The
#' contrast therefore has no single standardized target population. It returns
#' the link-scale contrast plus the two observed marginal outcomes and available
#' natural-scale contrasts.
#'
#' The two arms are observed directly, so their intervals are exact: the
#' Clopper-Pearson interval for a binomial proportion and the Garwood interval
#' for a Poisson rate, pooled across aggregate rows. The arm standard errors
#' and the contrasts use the boundary pseudo-count `(r + 0.5) / (n + 1)` when
#' an arm has zero or all events, or 0.5 events for a zero Poisson count; the
#' reported crude proportions and rates are unchanged. The link-scale
#' contrast, the log risk ratio and the risk difference get Wald intervals,
#' whose coverage is approximate.
#'
#' Scale note: `$estimate` (and the binomial `$log_rr`) is on the link / log
#' scale, where the null is 0. To compare against the natural-scale risk ratio
#' or rate ratio from [marginal_effects()] (where the null is 1), exponentiate
#' it (e.g. `exp(result$estimate)`).
#'
#' @param data An `mlumr_data` object from [combine_data()]
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal/poisson: ignored (identity/log always used).
#'   For survival: ignored (an unadjusted Cox proportional-hazards log hazard
#'   ratio is returned). The naive Cox benchmark accepts only right-censored /
#'   event data (optionally with delayed entry); left- or interval-censored data
#'   (which the Bayesian [mlumr()] model supports) are rejected. If `NULL`, uses
#'   the canonical default.
#' @param conf_level Confidence level for the interval (default 0.95)
#'
#' @section Normal-family weighting:
#' Across multiple AgD rows the normal-family comparator mean here is
#' population weighted using `outcome_n`, matching the Bayesian ML-UMR
#' comparator-population estimand. `outcome_n` is required when there is more
#' than one row; a single row has weight one. The comparator-mean variance
#' combines independent, mutually exclusive strata as `sum(w^2 * se^2)` using
#' normalized population weights. The same weighting applies to [stc()].
#'
#' @return An object of class `mlumr_naive`
#' @export
#'
#' @examples
#' \dontrun{
#' result <- naive(dat)
#' print(result)
#' }
naive <- function(data, link = NULL, conf_level = 0.95) {

  .validate_mlumr_data_object(data)

  family <- data$family %||% "binomial"
  ipd <- data$ipd$data
  agd <- data$agd$data
  z <- .z_from_conf_level(conf_level)

  out <- switch(
    family,
    binomial = .naive_binomial(data, ipd, agd, link, conf_level, z),
    normal = .naive_normal(data, ipd, agd, conf_level, z),
    poisson = .naive_poisson(data, ipd, agd, conf_level, z),
    survival = .naive_survival(data, conf_level, z),
    stop("Unsupported outcome family.", call. = FALSE)
  )

  class(out) <- c("mlumr_naive", "list")
  out
}


#' Naive comparison for binomial outcomes
#' @noRd
.naive_binomial <- function(data, ipd, agd, link, conf_level, z) {
  link_info <- check_link("binomial", link)
  link_resolved <- link_info$link

  n_index <- nrow(ipd)
  n_comparator <- sum(agd$.n)
  p_index <- mean(ipd$.outcome)
  p_comparator <- sum(agd$.r) / n_comparator
  p_index_effect <- bound_probability(p_index, n_index)
  p_comparator_effect <- bound_probability(p_comparator, n_comparator)

  estimate <- link_fun(p_index_effect, link_resolved) -
    link_fun(p_comparator_effect, link_resolved)

  # Boundary-corrected, so an all-events or no-events arm keeps its variance.
  p_index_se <- sqrt(p_index_effect * (1 - p_index_effect) / n_index)
  # Several aggregate rows are strata: the variance of their size-weighted
  # proportion is sum(w_k^2 p_k (1 - p_k) / n_k).
  row_p <- agd$.r / agd$.n
  row_w <- .normalize_weights(agd$.n)
  # The boundary correction uses the pooled n, so equivalent tabulations of
  # one arm agree; the row-level variance keeps heterogeneous strata apart.
  row_p_effect <- bound_probability(row_p, n_comparator)
  var_p_comparator_effect <- sum(
    row_w^2 * row_p_effect * (1 - row_p_effect) / agd$.n
  )
  var_link_index <- binomial_link_variance(
    p_index_effect, n_index, link_resolved
  )
  var_link_comparator <- link_derivative_response(
    p_comparator_effect, link_resolved
  )^2 * var_p_comparator_effect
  se <- sqrt(var_link_index + var_link_comparator)
  # Boundary-corrected on the absolute scale too.
  p_comparator_se <- sqrt(var_p_comparator_effect)
  # The arms are observed directly, so their intervals are exact.
  p_index_ci <- .clopper_pearson_interval(sum(ipd$.outcome), n_index,
                                          conf_level)
  p_comparator_ci <- .clopper_pearson_interval(sum(agd$.r), n_comparator,
                                               conf_level)
  rd <- p_index - p_comparator
  rd_se <- sqrt(p_index_se^2 + p_comparator_se^2)
  log_rr <- log(p_index_effect) - log(p_comparator_effect)
  log_rr_se <- sqrt(
    (1 - p_index_effect) / (n_index * p_index_effect) +
      var_p_comparator_effect / p_comparator_effect^2
  )

  list(
    estimate = estimate,
    link_effect = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "binomial",
    link = link_resolved,
    p_index = p_index,
    p_index_se = p_index_se,
    p_index_lower = p_index_ci$lower,
    p_index_upper = p_index_ci$upper,
    p_comparator = p_comparator,
    p_comparator_se = p_comparator_se,
    p_comparator_lower = p_comparator_ci$lower,
    p_comparator_upper = p_comparator_ci$upper,
    rd = rd,
    rd_se = rd_se,
    rd_lower = rd - z * rd_se,
    rd_upper = rd + z * rd_se,
    log_rr = log_rr,
    log_rr_se = log_rr_se,
    log_rr_lower = log_rr - z * log_rr_se,
    log_rr_upper = log_rr + z * log_rr_se,
    n_index = n_index,
    n_comparator = n_comparator,
    data = data
  )
}


#' Naive comparison for normal outcomes
#' @noRd
.naive_normal <- function(data, ipd, agd, conf_level, z) {
  mean_index <- mean(ipd$.outcome)
  n_index <- nrow(ipd)
  if (n_index < 2L) {
    stop("The naive normal benchmark needs at least two IPD observations to ",
         "estimate the index-mean variance; ", n_index, " supplied.",
         call. = FALSE)
  }
  var_index <- var(ipd$.outcome) / n_index

  if (nrow(agd) > 1L && is.null(agd$.n)) {
    stop("`outcome_n` is required for multiple normal AgD rows.", call. = FALSE)
  }
  agd_weights <- agd$.n %||% 1
  w_norm <- .normalize_weights(agd_weights)
  mean_comparator <- sum(w_norm * agd$.y)
  var_comparator <- sum(w_norm^2 * agd$.se^2)

  estimate <- mean_index - mean_comparator
  se <- sqrt(var_index + var_comparator)
  mean_index_se <- sqrt(var_index)
  mean_comparator_se <- sqrt(var_comparator)

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "normal",
    mean_index = mean_index,
    mean_index_se = mean_index_se,
    mean_index_lower = mean_index - z * mean_index_se,
    mean_index_upper = mean_index + z * mean_index_se,
    mean_comparator = mean_comparator,
    mean_comparator_se = mean_comparator_se,
    mean_comparator_lower = mean_comparator - z * mean_comparator_se,
    mean_comparator_upper = mean_comparator + z * mean_comparator_se,
    n_index = n_index,
    data = data
  )
}


#' Naive comparison for Poisson outcomes
#' @noRd
.naive_poisson <- function(data, ipd, agd, conf_level, z) {
  n_index <- nrow(ipd)
  events_index <- sum(ipd$.outcome)
  exposure_index <- sum(ipd$.exposure)
  rate_index <- events_index / exposure_index

  events_comparator <- sum(agd$.r)
  exposure_comparator <- sum(agd$.E)
  rate_comparator <- events_comparator / exposure_comparator

  events_index_adjusted <- max(events_index, 0.5)
  events_comparator_adjusted <- max(events_comparator, 0.5)
  log_rate_index <- log(events_index_adjusted / exposure_index)
  log_rate_comparator <- log(events_comparator_adjusted / exposure_comparator)

  estimate <- log_rate_index - log_rate_comparator
  se <- sqrt(1 / events_index_adjusted + 1 / events_comparator_adjusted)
  # Continuity-corrected on the absolute scale too.
  rate_index_se <- sqrt(events_index_adjusted) / exposure_index
  rate_comparator_se <- sqrt(events_comparator_adjusted) / exposure_comparator
  # Rate difference per unit exposure; the arms are independent.
  rd <- rate_index - rate_comparator
  rd_se <- sqrt(rate_index_se^2 + rate_comparator_se^2)
  # The arms are observed directly, so their intervals are exact (Garwood).
  rate_index_ci <- .garwood_interval(events_index, exposure_index, conf_level)
  rate_comparator_ci <- .garwood_interval(events_comparator,
                                          exposure_comparator, conf_level)

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "poisson",
    rd = rd,
    rd_se = rd_se,
    rd_lower = rd - z * rd_se,
    rd_upper = rd + z * rd_se,
    rate_index = rate_index,
    rate_index_se = rate_index_se,
    rate_index_lower = rate_index_ci$lower,
    rate_index_upper = rate_index_ci$upper,
    rate_comparator = rate_comparator,
    rate_comparator_se = rate_comparator_se,
    rate_comparator_lower = rate_comparator_ci$lower,
    rate_comparator_upper = rate_comparator_ci$upper,
    n_index = n_index,
    events_index = events_index,
    exposure_index = exposure_index,
    events_comparator = events_comparator,
    exposure_comparator = exposure_comparator,
    data = data
  )
}


#' Naive comparison for survival outcomes
#'
#' Unadjusted Cox proportional-hazards log hazard ratio comparing the index IPD
#' against the reconstructed comparator pseudo-IPD, plus Kaplan-Meier median
#' survival per arm. Because this benchmark is a right-censored Cox model, left-
#' and interval-censored records (internal status 2/3) are rejected rather than
#' collapsed to right-censoring.
#' @noRd
.naive_survival <- function(data, conf_level, z) {
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd

  # A right-censored Cox model: left- and interval-censored records (status
  # 2 and 3) are refused rather than collapsed, as stc() and geom_km() do.
  if (any(c(ipd$.status, pseudo$.status) %in% c(2L, 3L))) {
    stop("`naive()` fits a right-censored Cox benchmark and does not support ",
         "left- or interval-censored survival data (internal status 2 or 3), ",
         "which `mlumr()` does. Restrict the naive comparison to right-censored ",
         "and event data (status 0/1, optional delayed entry).",
         call. = FALSE)
  }

  pooled <- data.frame(
    time = c(ipd$.time, pseudo$.time),
    entry = c(ipd$.delay_time, pseudo$.delay_time),
    event = as.integer(c(ipd$.status, pseudo$.status) == 1L),
    arm = factor(c(rep("index", nrow(ipd)), rep("comparator", nrow(pseudo))),
                 levels = c("comparator", "index")),
    stringsAsFactors = FALSE
  )
  has_delay <- any(pooled$entry > 0)
  surv_obj <- if (has_delay) {
    survival::Surv(pooled$entry, pooled$time, pooled$event)
  } else {
    survival::Surv(pooled$time, pooled$event)
  }

  # The partial likelihood needs events in both arms.
  events_by_arm <- tapply(pooled$event, pooled$arm, sum)
  events_by_arm[is.na(events_by_arm)] <- 0L
  if (sum(pooled$event) == 0L || any(events_by_arm == 0L)) {
    stop("The naive Cox comparison needs at least one event in each arm: ",
         sprintf("observed %d in the comparator arm and %d in the index arm. ",
                 as.integer(events_by_arm[["comparator"]]),
                 as.integer(events_by_arm[["index"]])),
         "With an event-free arm the treatment coefficient is not identified ",
         "by the partial likelihood.", call. = FALSE)
  }

  # Events in both arms is necessary and not sufficient: the partial
  # likelihood can be monotone in the coefficient, in which case coxph()
  # stops on its convergence criterion with finite numbers and a warning.
  # That warning is what is read; the arrangement of event times is not.
  cox_warnings <- character(0)
  cox <- withCallingHandlers(
    survival::coxph(surv_obj ~ arm, data = pooled),
    warning = function(w) {
      cox_warnings <<- c(cox_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  monotone <- grepl("may be infinite", cox_warnings, fixed = TRUE)
  # A fit that ran out of iterations is not an estimate either.
  unconverged <- !monotone &
    (grepl("did not converge", cox_warnings, fixed = TRUE) |
       grepl("Ran out of iterations", cox_warnings, fixed = TRUE))
  # Anything coxph() reported that is neither is still the caller's to see.
  for (w in cox_warnings[!monotone & !unconverged]) warning(w, call. = FALSE)

  estimate <- unname(stats::coef(cox)[1])
  se <- sqrt(diag(stats::vcov(cox))[1])
  if (any(monotone)) {
    stop("The naive Cox comparison has no interior maximum: the partial ",
         "likelihood is monotone in the treatment coefficient, which happens ",
         "when no risk set ever compares the two arms in both directions. ",
         "coxph() returned a coefficient of ", format(estimate, digits = 4),
         " with a standard error of ", format(se, digits = 4),
         ", which describe where the iteration stopped rather than the data. ",
         "Use mlumr(), whose prior makes the posterior proper.", call. = FALSE)
  }
  if (any(unconverged)) {
    stop("The naive Cox comparison did not converge: coxph() reported ",
         paste(sQuote(cox_warnings[unconverged]), collapse = "; "),
         ". Its coefficient of ", format(estimate, digits = 4),
         " with a standard error of ", format(se, digits = 4),
         " describes the state the iteration stopped in, not an estimate. ",
         "Use mlumr(), whose prior makes the posterior proper.", call. = FALSE)
  }
  if (!is.finite(estimate) || !is.finite(se) || se <= 0) {
    stop("The naive Cox comparison did not produce an estimable treatment ",
         "effect (coefficient ", format(estimate), ", standard error ",
         format(se), "). This usually means the arms are separated in time, ",
         "so the partial likelihood has no interior maximum.", call. = FALSE)
  }

  km <- survival::survfit(surv_obj ~ arm, data = pooled)
  km_tab <- summary(km)$table
  med <- if (is.matrix(km_tab)) km_tab[, "median"] else km_tab["median"]
  med_comparator <- unname(med[grep("comparator", names(med))][1])
  med_index <- unname(med[grep("index", names(med))][1])

  list(
    estimate = estimate,
    log_hr = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "survival",
    median_index = med_index,
    median_comparator = med_comparator,
    n_index = nrow(ipd),
    n_comparator = nrow(pseudo),
    events_index = sum(ipd$.status == 1L),
    events_comparator = sum(pseudo$.status == 1L),
    data = data
  )
}

#' Numerical evaluation in the Stan models
#'
#' `mlumr` evaluates tail probabilities, marginal means, ratios, differences,
#' and survival quantities on the log scale where possible. The models do not
#' clip event probabilities, rates, survival probabilities, cumulative hazards,
#' risk ratios, or rate ratios to finite reporting bounds.
#'
#' @section Log-scale calculations:
#'
#' \describe{
#'   \item{Binary outcomes}{Event and non-event log probabilities are evaluated
#'     directly for logit, probit, and complementary log-log links. Aggregate
#'     probabilities are then formed with `log_sum_exp`, so an extreme but
#'     finite linear predictor is not first rounded to probability 0 or 1.}
#'   \item{Log-link outcomes}{Marginal normal means and Poisson rates use
#'     log-mean-exp calculations. Population weights are normalized on the log
#'     scale, so multiplying every weight by the same finite constant does not
#'     change the result.}
#'   \item{Survival outcomes}{Cumulative hazards and log hazards combine time,
#'     shape, and linear-predictor terms before exponentiation. Marginal
#'     survival and hazard calculations use log-sum-exp identities, including
#'     tail-specific expressions for log-normal, gamma, generalized-gamma,
#'     Weibull, Gompertz, and log-logistic models.}
#'   \item{Natural-scale contrasts}{Differences of positive means are evaluated
#'     from their logarithms before conversion to the natural scale. Ratios are
#'     evaluated through log contrasts. A mathematically overflowing ratio may
#'     therefore be `Inf`, and an underflowing natural-scale probability may be
#'     0, rather than an arbitrary finite replacement. The corresponding
#'     log-scale quantity remains the preferred diagnostic.}
#' }
#'
#' @section Structural constraints and roundoff:
#'
#' Positive model parameters such as the normal residual SD use Stan lower
#' bounds and therefore truncate their priors to the positive half-line. For
#' flexible survival baselines, differences of cumulative I-spline bases are
#' computed before the coefficient dot product and tiny negative values caused
#' by floating-point cancellation are projected to zero. This projection
#' enforces the mathematical non-negativity of a cumulative-hazard increment;
#' it is not a floor on a positive event probability or hazard.
#'
#' @section Interpreting extreme draws:
#'
#' Inspect the log-scale generated quantities when a natural-scale contrast is
#' zero or infinite. For example, binary models retain marginal log event and
#' non-event probabilities internally, Poisson models form log rates before
#' exponentiation, and survival models form log survival and log mean hazards.
#' An infinite natural-scale ratio can be the correct floating-point
#' representation of a finite log ratio whose exponential exceeds double
#' precision; replacing it with a fixed finite number would change the
#' estimand.
#'
#' @name mlumr-numerical-evaluation
#' @keywords internal
NULL

# Plotting methods for mlumr result objects:
#   plot(marginal_effects(fit))        -> forest of population-standardized effects
#   plot(predict(fit, type = "..."))   -> survival/hazard/cumhaz/loghr curves, etc.
#   plot(conditional_effects(fit, ...)) -> effects by covariate profile
#   plot_prior_posterior(fit)          -> prior-vs-posterior overlay
# Each returns a ggplot object so it composes with further ggplot2 layers.

# Lower/upper credible-interval column names for a summary data frame
# (`.summarize_draw_matrix()` writes qNN columns, e.g. q2.5 / q97.5).
#' @noRd
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
#' @noRd
.ratio_measures <- c("RR", "HR", "TR", "RMSTR",
                     "EXP_DELTA_ETA", "EXP_ETA_CONTRAST")

# The additive counterparts (null 0). A label outside both lists is unknown,
# not a difference.
#' @noRd
.difference_measures <- c("RMSTD", "RD", "MD", "LINK_EFFECT", "LOR",
                          "LOG_HR", "LOG_TR", "DELTA_ETA", "ETA_CONTRAST")

#' Is this a label whose null the package can state?
#' @noRd
.known_measure <- function(effect) {
  toupper(effect) %in% c(.ratio_measures, .difference_measures)
}

# Null reference line implied by an effect label: 1 for ratio measures, 0 for
# differences and log-scale contrasts. Shared by both forest plots so a measure
# added to `.ratio_measures` is right in every figure at once.
#' @noRd
.null_ref_for <- function(effect) {
  ifelse(toupper(effect) %in% .ratio_measures, 1, 0)
}

# Coverage of the interval actually drawn, read off the quantile columns.
#' @noRd
.ci_label <- function(ci) {
  lo <- as.numeric(sub("^q", "", ci$lo))
  hi <- as.numeric(sub("^q", "", ci$hi))
  sprintf("%g%% credible interval", hi - lo)
}

# Ratio measures belong on a log axis, where reciprocal effects sit at equal
# distances from the null; ggplot2 applies one transform to the whole plot,
# so it is used only when every panel shows a ratio measure.
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
.parameter_prior <- function(object, par) {
  priors <- object$priors %||% list()
  base <- sub("\\[[0-9]+\\]$", "", par)
  idx <- suppressWarnings(as.integer(sub("^.*\\[([0-9]+)\\]$", "\\1", par)))
  # The priors the model used, which for a normal fit can be a default
  # rescaled to the outcome.
  if (base %in% c("mu_index", "mu_comparator")) {
    return(list(prior = priors$intercept_resolved %||% priors$intercept,
                lower = -Inf))
  }
  # Stan declares these <lower=0>, which truncates rather than folds.
  if (base == "sigma") {
    return(list(prior = priors$sigma_resolved %||% priors$sigma, lower = 0))
  }
  if (base %in% c("aux_val", "aux_val_cmp")) {
    return(list(prior = priors$aux, lower = 0))
  }
  # The second generalized-gamma shape has its own prior.
  if (base %in% c("aux2_val", "aux2_val_cmp")) {
    return(list(prior = priors$aux2, lower = 0))
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' comparator, which coincide when the baseline is shared.
#' @noRd
.surv_scoef_draws <- function(object, treatment = c("index", "comparator")) {
  treatment <- match.arg(treatment)
  draws <- object$draws
  n_scoef <- object$stan_data$n_scoef
  n_strata <- object$stan_data$n_strata %||% 1L
  j <- seq_len(n_scoef)
  # Two layouts, newest first:
  #   scoef_idx[j] / scoef_cmp[j]  the named per-treatment views (always emitted)
  #   scoef[j,s]                   the underlying matrix
  view <- if (identical(treatment, "index")) "scoef_idx" else "scoef_cmp"
  s <- if (identical(treatment, "index")) 1L else n_strata
  for (nm in list(paste0(view, "[", j, "]"),
                  paste0("scoef[", j, ",", s, "]"))) {
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
#'   Default `c(0.5, 1, 2.5, 5, 10)`. For a normal identity-link fit whose
#'   `prior_beta` is the package default or autoscaled, each scale is in
#'   units of the IPD outcome SD, as the fit's own prior is (see
#'   [prior_normal()]). The intercept and `sigma` priors are held at the
#'   fit's own.
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
.rescale_prior_beta <- function(prior, new_scale) {
  if (is_single_prior(prior)) {
    # Replace the scalar sd field. For exponential (not supported on beta
    # but defensively) fall back to a normal(0, new_scale).
    if (prior$distribution == "exponential") {
      return(prior_normal(mean = 0, sd = new_scale))
    }
    # A default's scale is in outcome SDs for a normal fit; the swept scales
    # stay in the same units, so the row at the original scale reproduces it.
    prior$outcome_scale <- .on_outcome_scale(prior)
    prior$sd <- new_scale
    # Strip default/version tags since this is a user-generated variant.
    prior$default <- NULL
    prior$version <- NULL
    return(prior)
  }
  # Per-coefficient list: rescale each element to new_scale (absolute, not
  # ratio; we want a homogeneous sensitivity sweep).
  lapply(prior, function(p) {
    p$outcome_scale <- .on_outcome_scale(p)
    p$sd <- new_scale
    p$default <- NULL
    p$version <- NULL
    p
  })
}

#' Summarize a sensitivity refit
#' @noRd
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
#' @noRd
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

#' Summary of priors used by a fitted ML-UMR model
#'
#' Print a human-readable summary of every prior that was used to fit an
#' [mlumr()] model, including the effective per-coefficient scales after
#' autoscaling. Mirrors the spirit of `rstanarm::prior_summary()`.
#'
#' @param object An `mlumr_fit` object.
#' @param digits Number of significant digits for numeric values (default 3).
#' @param ... Unused.
#'
#' @return Invisibly returns a list describing the priors; the side effect is
#'   printing a formatted summary.
#' @seealso [prior_sensitivity()] to quantify how much the posterior moves
#'   under alternative `prior_beta` scales; [prior_normal()],
#'   [prior_student_t()], [prior_cauchy()], [prior_exponential()] for the
#'   prior constructors themselves.
#' @export
#' @examples
#' \dontrun{
#' fit <- mlumr(dat)
#' prior_summary(fit)
#' }
prior_summary <- function(object, ...) {
  UseMethod("prior_summary")
}

#' @rdname prior_summary
#' @export
prior_summary.default <- function(object, ...) {
  stop("prior_summary() has no method for class ",
       paste(class(object), collapse = "/"),
       call. = FALSE)
}

#' @rdname prior_summary
#' @method prior_summary mlumr_fit
#' @export
prior_summary.mlumr_fit <- function(object, digits = 3, ...) {

  .validate_mlumr_fit_object(object)
  digits <- .validate_prior_summary_digits(digits)

  priors <- object$priors
  if (is.null(priors)) {
    stop("No prior information stored on this fit (was it fitted with an ",
         "older version of mlumr?).", call. = FALSE)
  }
  if (!is.null(priors$beta_resolved)) {
    priors$beta_resolved <- .validate_resolved_beta_prior(priors$beta_resolved)
  }
  if (!is.null(priors$beta_comparator_resolved)) {
    priors$beta_comparator_resolved <-
      .validate_resolved_beta_prior(priors$beta_comparator_resolved)
  }

  cat("Priors for ML-UMR Fit\n")
  cat("=====================\n\n")

  # Intercepts
  cat("Intercepts (mu_index, mu_comparator):\n")
  cat("  ", .format_prior(priors$intercept, digits = digits), "\n", sep = "")
  .print_default_tag(priors$intercept)
  .print_outcome_scaled(priors$intercept, priors$intercept_resolved,
                        priors$outcome_sd, digits)
  cat("\n")

  # Beta (regression coefficients)
  .print_beta_prior_block("Regression coefficients (beta):",
                          priors$beta_resolved, priors$beta, digits)

  # The relaxed model's comparator coefficients can carry their own prior.
  if (!is.null(priors$beta_comparator_resolved)) {
    .print_beta_prior_block(
      "Comparator regression coefficients (beta_comparator):",
      priors$beta_comparator_resolved, priors$beta_comparator, digits
    )
    if (!isTRUE(priors$beta_comparator_resolved$user_specified)) {
      cat("  (not set; reuses the `beta` prior above)\n\n")
    }
  }

  # Sigma (normal family only)
  if (!is.null(priors$sigma)) {
    cat("Residual SD (sigma, ", .constrained_prior_label(priors$sigma),
        "):\n", sep = "")
    cat("  ", .format_prior(priors$sigma, digits = digits), "\n", sep = "")
    .print_default_tag(priors$sigma)
    .print_outcome_scaled(priors$sigma, priors$sigma_resolved,
                          priors$outcome_sd, digits)
    cat("\n")
  }

  # Survival baseline: `aux` is the parametric shape or scale, `smooth` the
  # random-walk SD of a flexible baseline.
  if (!is.null(priors$aux)) {
    aux_label <- .constrained_prior_label(priors$aux)
    label <- if (is.null(priors$aux2)) {
      paste0("Survival auxiliary (shape / scale, ", aux_label, "):")
    } else {
      paste0("Survival auxiliary 1 (gengamma sigma, ", aux_label, "):")
    }
    cat(label, "\n", sep = "")
    cat("  ", .format_prior(priors$aux, digits = digits), "\n", sep = "")
    .print_default_tag(priors$aux)
    cat("\n")
  }
  # Generalized gamma only.
  if (!is.null(priors$aux2)) {
    cat("Survival auxiliary 2 (gengamma k = 1 / Q^2, where Q is the Lawless\n")
    cat("  shape; ", .constrained_prior_label(priors$aux2), "):\n",
        sep = "")
    cat("  ", .format_prior(priors$aux2, digits = digits), "\n", sep = "")
    .print_default_tag(priors$aux2)
    cat("\n")
  }
  if (!is.null(priors$smooth)) {
    cat("Survival baseline smoothing (random-walk SD, ",
        .constrained_prior_label(priors$smooth), "):\n", sep = "")
    cat("  ", .format_prior(priors$smooth, digits = digits), "\n", sep = "")
    .print_default_tag(priors$smooth)
    cat("\n")
  }

  invisible(priors)
}


#' @noRd
.format_prior <- function(prior, digits = 3) {
  if (is.null(prior$distribution)) {
    return("<missing prior>")
  }
  switch(prior$distribution,
    normal = sprintf("normal(%s, %s)%s",
                     format(prior$mean, digits = digits),
                     format(prior$sd,   digits = digits),
                     if (isTRUE(prior$autoscale)) " [autoscale]" else ""),
    student_t = sprintf("student_t(df = %g, %s, %s)%s",
                        prior$df,
                        format(prior$mean, digits = digits),
                        format(prior$sd,   digits = digits),
                        if (isTRUE(prior$autoscale)) " [autoscale]" else ""),
    exponential = sprintf("exponential(rate = %s)",
                          format(prior$rate, digits = digits)),
    sprintf("%s(...)", prior$distribution)
  )
}

#' @noRd
.format_prior_collection <- function(prior, digits = 3) {
  if (is_single_prior(prior) || is.null(prior)) {
    return(.format_prior(prior, digits = digits))
  }
  if (is.list(prior) && all(vapply(prior, is_single_prior, logical(1)))) {
    return(vapply(seq_along(prior), function(i) {
      sprintf("beta[%d]: %s", i, .format_prior(prior[[i]], digits = digits))
    }, character(1)))
  }
  "<missing prior>"
}

#' Show a default prior as the model used it, in outcome units
#'
#' Prints nothing when the model used the prior as written.
#' @param user,resolved The prior as passed and as used.
#' @param sd_y The IPD outcome SD the default was multiplied by.
#' @noRd
.print_outcome_scaled <- function(user, resolved, sd_y, digits) {
  if (is.null(resolved) || is.null(sd_y) ||
        isTRUE(all.equal(resolved$sd, user$sd))) {
    return(invisible())
  }
  cat("  used as ", .format_prior(resolved, digits = digits),
      ", the default times the IPD outcome SD (",
      format(sd_y, digits = digits), ")\n", sep = "")
  invisible()
}

#' @noRd
.print_default_tag <- function(prior) {
  if (isTRUE(prior$default) && !is.null(prior$version)) {
    cat("  (package default, mlumr ", prior$version, ")\n", sep = "")
  }
}



#' Print one resolved regression-coefficient prior block
#'
#' `beta` and `beta_comparator` are reported the same way, so the broadcast /
#' per-coefficient decision, the autoscaling footnote and the default tag are
#' written once. `resolved` is the per-coefficient struct stored on the fit;
#' `user_prior` is what the caller passed, used for the fallback on older fits
#' that carry no resolved struct and for the package-default tag.
#' @noRd
.print_beta_prior_block <- function(heading, resolved, user_prior, digits) {
  cat(heading, "\n", sep = "")
  if (is.null(resolved)) {
    # Fallback for older fits: just print the user-specified prior.
    cat(paste0("  ", .format_prior_collection(user_prior, digits = digits)),
        sep = "\n")
    cat("\n")
  } else {
    # Detect whether the resolved per-coefficient priors are homogeneous.
    means_eq <- length(unique(round(resolved$mean, 12))) == 1L
    sds_eq   <- length(unique(round(resolved$sd,   12))) == 1L
    autos_any <- any(resolved$autoscale)
    # The outcome-SD factor of a normal identity-link fit; absent on older fits.
    y_scaled <- !is.null(resolved$sd_y) & resolved$sd_y != 1
    sd_y <- if (any(y_scaled)) resolved$sd_y[y_scaled][[1L]] else NULL

    family_label <- .resolved_prior_family_label(resolved$dist)
    broadcast_label <- .resolved_prior_broadcast_label(resolved, digits)

    if (means_eq && sds_eq && !autos_any) {
      # Broadcast summary
      cat(sprintf("  %s applied to all %d covariate(s)\n",
                  broadcast_label, length(resolved$mean)))
    } else {
      # Per-coefficient table
      tbl <- data.frame(
        coefficient = resolved$covariate_names,
        mean = round(resolved$mean, digits),
        scale = round(resolved$sd,  digits),
        autoscaled = resolved$autoscale,
        sd_x = round(resolved$sd_x, digits),
        stringsAsFactors = FALSE
      )
      cat(sprintf("  Family: %s%s\n", family_label,
                  if (resolved$dist == 1L) sprintf(" (df = %g)", resolved$df) else ""))
      print(tbl, row.names = FALSE)
      if (autos_any) {
        cat(if (is.null(sd_y)) {
          "  (scale = user_scale / sd_x for autoscaled rows)\n"
        } else {
          "  (scale = user_scale * sd_y / sd_x for autoscaled rows)\n"
        })
      }
    }
    if (!is.null(sd_y)) {
      cat(sprintf(paste0("  (sd_y = %s is the IPD outcome SD; the identity ",
                         "link puts\n   the coefficients in outcome units)\n"),
                  format(sd_y, digits = digits)))
    }
  }
  .print_default_tag(user_prior)
  cat("\n")
}


#' Validate prior_summary digits
#' @noRd
.validate_prior_summary_digits <- function(digits) {
  valid <- is.numeric(digits) &&
    length(digits) == 1L &&
    is.finite(digits) &&
    digits >= 1 &&
    digits <= 22 &&
    digits == as.integer(digits)
  if (!valid) {
    stop("`digits` must be a single integer between 1 and 22.",
         call. = FALSE)
  }
  as.integer(digits)
}


#' Validate resolved beta-prior metadata stored on a fit
#' @noRd
.validate_resolved_beta_prior <- function(br) {
  required <- c("mean", "sd", "dist", "df", "autoscale", "sd_x",
                "covariate_names")
  missing <- setdiff(required, names(br))
  if (length(missing) > 0L) {
    stop(sprintf("Resolved beta prior metadata is missing: %s.",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }

  n_beta <- length(br$mean)
  valid <- is.numeric(br$mean) &&
    is.numeric(br$sd) &&
    is.logical(br$autoscale) &&
    is.numeric(br$sd_x) &&
    is.character(br$covariate_names) &&
    n_beta > 0L &&
    length(br$sd) == n_beta &&
    length(br$autoscale) == n_beta &&
    length(br$sd_x) == n_beta &&
    length(br$covariate_names) == n_beta &&
    all(is.finite(br$mean)) &&
    all(is.finite(br$sd)) &&
    all(br$sd > 0) &&
    all(is.finite(br$sd_x)) &&
    all(!is.na(br$autoscale)) &&
    all(!is.na(br$covariate_names)) &&
    all(nzchar(br$covariate_names))

  if (!valid) {
    stop("Resolved beta prior metadata is malformed.", call. = FALSE)
  }
  if (!is.numeric(br$dist) || length(br$dist) != 1L ||
        !is.finite(br$dist)) {
    stop("Resolved beta prior distribution code is malformed.", call. = FALSE)
  }
  if (!is.numeric(br$df) || length(br$df) != 1L || !is.finite(br$df)) {
    stop("Resolved beta prior degrees of freedom is malformed.",
         call. = FALSE)
  }

  br$dist <- as.integer(br$dist)
  br
}


#' Label a resolved Stan prior family code
#' @noRd
.resolved_prior_family_label <- function(dist) {
  switch(as.character(dist),
    "0" = "normal",
    "1" = "student_t",
    sprintf("dist=%s", dist)
  )
}


#' Format a homogeneous resolved beta prior
#' @noRd
.resolved_prior_broadcast_label <- function(br, digits) {
  switch(as.character(br$dist),
    "0" = sprintf("normal(%s, %s)",
                  format(br$mean[[1L]], digits = digits),
                  format(br$sd[[1L]], digits = digits)),
    "1" = sprintf("student_t(df = %g, %s, %s)",
                  br$df,
                  format(br$mean[[1L]], digits = digits),
                  format(br$sd[[1L]], digits = digits)),
    sprintf("dist=%s(...)", br$dist)
  )
}


#' Describe how a positive-constrained prior is constrained
#'
#' An exponential is already positive, and only a zero-location normal or t
#' truncated at zero is a half-normal or half-t.
#' @param prior A prior specification list.
#' @return A one-line character label for the constrained form.
#' @noRd
.constrained_prior_label <- function(prior) {
  dist <- prior$distribution %||% ""
  loc <- suppressWarnings(as.numeric(prior$mean %||% NA_real_))
  centered <- isTRUE(is.finite(loc) && loc == 0)
  if (identical(dist, "exponential")) {
    "already positive; <lower=0> truncates nothing"
  } else if (identical(dist, "normal")) {
    if (centered) "half-normal via <lower=0>" else "normal truncated at 0"
  } else if (identical(dist, "student_t")) {
    if (centered) "half-t via <lower=0>" else "t truncated at 0"
  } else {
    "truncated to positive values via <lower=0>"
  }
}

#' Specify a normal prior
#'
#' Construct a normal prior for passing to [mlumr()] via `prior_intercept`,
#' `prior_beta`, or `prior_sigma`. The resulting list is consumed by the
#' Stan models.
#'
#' @section Choosing a scale:
#' The default intercept prior `normal(0, 10)` is very weak on the link scale,
#' and the data usually constrain the intercept strongly. The coefficient
#' default `normal(0, 2.5)` is a generic starting value on the link scale per
#' unit of covariate, not a calibrated choice; use `autoscale = TRUE` for
#' predictors on different scales and calibrate with prior predictive checks
#' (Gelman et al., 2008; the Stan prior-choice wiki). `prior_sigma` is a
#' normal truncated at zero through the Stan `<lower=0>` constraint, a
#' half-normal at the default mean of 0.
#'
#' For `family = "normal"` the identity link is not unit-free: the intercepts
#' and coefficients are in the outcome's units, and so is the residual SD
#' under either link. There the package defaults are read in units of the IPD
#' outcome SD, `sd(y)`: `normal(0, 10 * sd(y))` for the intercepts,
#' `normal(0, 2.5 * sd(y))` for the coefficients (identity link) and a
#' half-normal with scale `2.5 * sd(y)` for the residual SD (either link), and
#' `autoscale = TRUE` gives a coefficient scale of `sd * sd(y) / sd(x)`. A
#' prior written out by the user is used as given. [prior_summary()] prints
#' the scales the model used. Run
#' [prior_sensitivity()] for the relaxed model, whose `beta_comparator` is
#' identified only by the aggregate likelihood.
#'
#' @param mean Prior mean (default 0).
#' @param sd Prior standard deviation (default 10). The default matches
#'   the historical "very weak" scale; explicit tighter values are
#'   recommended for regression coefficients (see Details).
#' @param autoscale If `TRUE` and this prior is passed as `prior_beta`,
#'   the scale is divided by each covariate's empirical SD so the prior
#'   is weakly-informative regardless of predictor scaling, and for
#'   `family = "normal"` with the identity link also multiplied by the IPD
#'   outcome SD, since the coefficients are then in outcome units. Default
#'   `FALSE` to preserve backward-compatible behavior; set to `TRUE`
#'   explicitly when passing unstandardized predictors. Ignored for
#'   `prior_intercept` and `prior_sigma`.
#'
#' @return A list with components `distribution`, `mean`, `sd`, `df`,
#'   `autoscale`.
#' @export
#'
#' @references
#' Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly
#' informative default prior distribution for logistic and other
#' regression models. *Annals of Applied Statistics*, 2(4), 1360-1383.
#'
#' Vehtari, A. et al. Prior Choice Recommendations (Stan wiki):
#' <https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations>.
#'
#' @examples
#' # Default weakly-very-weak intercept prior
#' prior_normal(mean = 0, sd = 10)
#'
#' # Package starting value for regression coefficients
#' prior_normal(mean = 0, sd = 2.5)
#'
#' # Autoscaled coefficient prior (dividing 2.5 by each covariate's SD)
#' prior_normal(mean = 0, sd = 2.5, autoscale = TRUE)
prior_normal <- function(mean = 0, sd = 10, autoscale = FALSE) {
  .validate_prior_number(mean, "mean")
  .validate_prior_number(sd, "sd", positive = TRUE)
  autoscale <- .validate_prior_autoscale(autoscale)
  list(
    distribution = "normal",
    mean = mean,
    sd = sd,
    df = NA_real_,
    autoscale = isTRUE(autoscale)
  )
}

#' Specify a Student-t prior
#'
#' Heavier-tailed alternative to [prior_normal()]. A Student-t with moderate
#' degrees of freedom can be a robust weakly informative starting family, but
#' its scale still requires calibration to the link, outcome, and predictor
#' scaling.
#'
#' @param df Degrees of freedom (must be positive).
#' @param mean Prior location (default 0).
#' @param sd Prior scale (default 2.5).
#' @param autoscale See [prior_normal()]. Default `FALSE`.
#'
#' @return A list with components `distribution = "student_t"`, `df`,
#'   `mean`, `sd`, `autoscale`.
#' @export
#'
#' @examples
#' # A moderately heavy-tailed coefficient prior
#' prior_student_t(df = 5, mean = 0, sd = 2.5)
prior_student_t <- function(df = 5, mean = 0, sd = 2.5, autoscale = FALSE) {
  .validate_prior_number(df, "df", positive = TRUE)
  .validate_prior_number(mean, "mean")
  .validate_prior_number(sd, "sd", positive = TRUE)
  autoscale <- .validate_prior_autoscale(autoscale)
  list(
    distribution = "student_t",
    mean = mean,
    sd = sd,
    df = df,
    autoscale = isTRUE(autoscale)
  )
}

#' Specify a Cauchy prior
#'
#' Cauchy is Student-t with `df = 1`, a wrapper around [prior_student_t()].
#' Its very heavy tails can slow sampling; a Student-t with 3 to 7 degrees of
#' freedom is usually preferred for regression coefficients.
#'
#' @param mean Prior location (default 0).
#' @param sd Prior scale (default 2.5).
#' @param autoscale See [prior_normal()]. Default `FALSE`.
#'
#' @return A list with components `distribution = "student_t"`, `df = 1`,
#'   `mean`, `sd`, `autoscale`.
#' @export
#'
#' @examples
#' prior_cauchy(mean = 0, sd = 2.5)
prior_cauchy <- function(mean = 0, sd = 2.5, autoscale = FALSE) {
  prior_student_t(df = 1, mean = mean, sd = sd, autoscale = autoscale)
}

#' Specify an exponential prior
#'
#' Exponential prior on a positive scalar. Currently supported for
#' [`prior_sigma`][mlumr] (normal-family residual SD) only; rejected for
#' unconstrained intercepts and regression coefficients.
#' `prior_exponential(rate = 1)` has mean 1 and is a reasonable
#' weakly-informative choice when the outcome is standardized.
#'
#' @param rate Rate parameter (default 1). Larger `rate` = tighter prior
#'   concentrated near zero.
#' @return A list with components `distribution = "exponential"`, `rate`,
#'   and placeholders (`mean = 0`, `sd = 1 / rate`) so the same
#'   Stan-field translation works.
#' @export
#' @examples
#' prior_exponential(rate = 1)
prior_exponential <- function(rate = 1) {
  .validate_prior_number(rate, "rate", positive = TRUE)
  list(
    distribution = "exponential",
    rate = rate,
    mean = 0,
    sd = 1 / rate,
    df = NA_real_,
    autoscale = FALSE
  )
}


# ---- Default priors (carry $default = TRUE and $version) -------------------

#' Default priors used by [mlumr()]
#'
#' These accessors return the current default priors used by [mlumr()],
#' tagged with `$default = TRUE` and the package version. For
#' `family = "normal"` their scales are multiples of the IPD outcome SD
#' wherever the parameter is in outcome units; see [prior_normal()]. [prior_summary()]
#' prints the version so cross-release reproducibility is diagnosable: if a
#' later release changes a default, fits produced with an older version
#' will still carry the correct `$version` tag.
#'
#' @return A prior list (see [prior_normal()]).
#' @name default_priors
#' @examples
#' default_prior_intercept()
#' default_prior_beta()
#' default_prior_sigma()
NULL

#' @rdname default_priors
#' @export
default_prior_intercept <- function() {
  .tag_default(prior_normal(mean = 0, sd = 10))
}

#' @rdname default_priors
#' @export
default_prior_beta <- function() {
  .tag_default(prior_normal(mean = 0, sd = 2.5))
}

#' @rdname default_priors
#' @export
default_prior_sigma <- function() {
  .tag_default(prior_normal(mean = 0, sd = 2.5))
}

#' @rdname default_priors
#' @export
#' @details
#' `default_prior_aux()` and `default_prior_smooth()` apply to the survival
#' family only. `prior_aux` is a half-normal(0, 2) on the shape/scale
#' parameter(s) of parametric survival distributions (Weibull/Gompertz/gamma
#' shape, log-normal sdlog, generalized-gamma shapes). `prior_smooth` is a
#' half-normal(0, 1) on the random-walk smoothing SD of the M-spline /
#' piecewise-exponential baseline hazard.
default_prior_aux <- function() {
  .tag_default(prior_normal(mean = 0, sd = 2))
}

#' @rdname default_priors
#' @export
default_prior_smooth <- function() {
  .tag_default(prior_normal(mean = 0, sd = 1))
}

#' @noRd
.tag_default <- function(prior) {
  prior$default <- TRUE
  prior$version <- as.character(utils::packageVersion("mlumr"))
  prior
}


# ---- Validation ------------------------------------------------------------

#' Validate prior specification
#' @param prior Prior specification list
#' @param param_name Parameter name for error messages
#' @noRd
validate_prior <- function(prior, param_name = "parameter") {
  if (!is.list(prior)) {
    stop(sprintf("Prior for %s must be a list", param_name), call. = FALSE)
  }
  valid_distribution <- is.character(prior$distribution) &&
    length(prior$distribution) == 1L &&
    prior$distribution %in% c("normal", "student_t", "exponential")
  if (!valid_distribution) {
    msg <- paste0(
      "Unsupported prior distribution '%s' for %s. ",
      "Use prior_normal(), prior_student_t(), prior_cauchy(), ",
      "or prior_exponential()."
    )
    stop(sprintf(
      msg,
      .prior_distribution_label(prior$distribution),
      param_name
    ), call. = FALSE)
  }
  if (prior$distribution == "exponential") {
    .validate_prior_number(prior$rate, "rate", positive = TRUE,
                           param_name = param_name)
    return(invisible(TRUE))
  }
  .validate_prior_number(prior$mean, "mean", param_name = param_name)
  .validate_prior_number(prior$sd, "sd", positive = TRUE,
                         param_name = param_name)
  if (prior$distribution == "student_t") {
    .validate_prior_number(prior$df, "df", positive = TRUE,
                           param_name = param_name)
  }
  invisible(TRUE)
}

.validate_prior_number <- function(x, field, positive = FALSE,
                                   param_name = NULL) {
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    is.finite(x) &&
    (!positive || x > 0)
  if (!valid) {
    qualifier <- if (positive) "positive finite" else "finite"
    if (is.null(param_name)) {
      stop(sprintf("`%s` must be a single %s number", field, qualifier),
           call. = FALSE)
    }
    stop(sprintf("Prior %s must be a single %s number for %s",
                 field, qualifier, param_name),
         call. = FALSE)
  }
  invisible(TRUE)
}

.validate_prior_autoscale <- function(autoscale) {
  if (!is.logical(autoscale) || length(autoscale) != 1L || is.na(autoscale)) {
    stop("`autoscale` must be TRUE or FALSE", call. = FALSE)
  }
  autoscale
}

.prior_distribution_label <- function(distribution) {
  if (is.null(distribution)) {
    return("<missing>")
  }
  paste(distribution, collapse = ", ")
}

#' Is this object a single prior (vs a list of priors)?
#' @noRd
is_single_prior <- function(x) {
  is.list(x) && !is.null(x$distribution)
}


# ---- Stan-data translation -------------------------------------------------

#' IPD outcome SD, the unit of the normal family's default priors
#'
#' Falls back to 1, no rescaling, when the IPD outcome has no SD, as with a
#' single row or a constant outcome; `mlumr()` warns about the first and
#' refuses the second.
#' @param y The IPD outcome.
#' @return A positive finite number.
#' @noRd
.outcome_sd <- function(y) {
  s <- stats::sd(as.numeric(y))
  if (is.finite(s) && s > 0) s else 1
}

#' Is this prior's scale a multiple of the outcome SD?
#'
#' A package default is, and so is a prior derived from one by
#' [prior_sensitivity()], which marks it `outcome_scale`.
#' @noRd
.on_outcome_scale <- function(prior) {
  isTRUE(prior$default) || isTRUE(prior$outcome_scale)
}

#' Put a default prior in units of the outcome SD
#'
#' The normal family's intercepts and coefficients (identity link) and its
#' residual SD (either link) are in the outcome's units, so a default there
#' is `sd_y` times its nominal scale. A prior the user wrote is returned
#' unchanged, and so is every prior when `sd_y` is 1.
#' @param prior A single prior list.
#' @param sd_y The outcome SD, or 1 where the parameter is unit-free.
#' @return The prior as the model uses it.
#' @noRd
.outcome_scaled_prior <- function(prior, sd_y = 1) {
  if (sd_y == 1 || !.on_outcome_scale(prior)) return(prior)
  prior$mean <- prior$mean * sd_y
  prior$sd <- prior$sd * sd_y
  if (identical(prior$distribution, "exponential")) {
    prior$rate <- prior$rate / sd_y
  }
  prior
}

#' Translate a scalar prior spec to the Stan data fields
#'
#' Stan scalar-prior fields: `prior_*_mean`, `prior_*_sd`,
#' `prior_*_dist` (0 = normal, 1 = student_t, 2 = exponential for sigma
#' only), `prior_*_df` (used only when dist == 1; positive placeholder
#' otherwise).
#'
#' @param prior A prior list.
#' @return A list with `mean`, `sd`, `dist`, `df`.
#' @noRd
stan_prior_fields <- function(prior) {
  validate_prior(prior)
  dist_code <- switch(prior$distribution,
    normal      = 0L,
    student_t   = 1L,
    exponential = 2L,
    stop("Unsupported distribution: ", prior$distribution)
  )
  df_value <- if (prior$distribution == "student_t") prior$df else 3
  # For exponential, Stan interprets `scale` as 1 / rate (see priors_functions.stan).
  location <- if (prior$distribution == "exponential") 0 else prior$mean
  scale    <- if (prior$distribution == "exponential") 1 / prior$rate else prior$sd
  list(mean = location, sd = scale, dist = dist_code, df = df_value)
}


#' Translate a `prior_beta` spec to vector-valued Stan data fields
#'
#' The Stan models declare `prior_beta_mean` and `prior_beta_sd` as
#' `vector[n_cov]` so the same data contract supports:
#'
#' \itemize{
#'   \item (i) a single prior broadcast to all covariates (scalar user input),
#'   \item (ii) per-coefficient priors (a list of prior lists, length `n_cov`),
#'   \item (iii) autoscaling: each covariate's scale is divided by `sd(x_j)`
#'     so the prior is weakly-informative on the standardized scale.
#' }
#'
#' For a list of per-coefficient priors, all elements must use the same
#' `distribution` family and `df` (Stan branches on a single dist code).
#'
#' @param prior A prior list from [prior_normal()] / [prior_student_t()] /
#'   [prior_cauchy()], OR a list of such priors of length `n_cov`.
#' @param n_cov Number of covariates.
#' @param sd_x Optional numeric vector of covariate SDs (length `n_cov`).
#'   Required when any prior has `autoscale = TRUE`; ignored otherwise.
#' @param covariate_names Optional character vector of covariate names
#'   (length `n_cov`). Used only to produce informative warnings when
#'   `autoscale = TRUE` meets a zero-SD covariate.
#' @param sd_y The IPD outcome SD for a normal identity-link fit, whose
#'   coefficients are in outcome units, else 1. Multiplies the scale of each
#'   default or autoscaled element.
#' @return A list with numeric vectors `mean` and `sd` (length `n_cov`)
#'   and scalars `dist`, `df`, a logical vector `autoscale` recording
#'   which elements were autoscaled, and `sd_y`, the outcome-SD factor each
#'   element carries (for `prior_summary()`).
#' @noRd
stan_prior_fields_beta <- function(prior, n_cov, sd_x = NULL,
                                   covariate_names = NULL, sd_y = 1) {

  # Expand to a list of n_cov single-priors
  if (is_single_prior(prior)) {
    validate_prior(prior, "beta")
    prior_list <- rep(list(prior), n_cov)
  } else if (is.list(prior)) {
    if (length(prior) != n_cov) {
      stop(sprintf(
        "Per-coefficient prior list has length %d but n_cov = %d.",
        length(prior), n_cov
      ), call. = FALSE)
    }
    # Validate each
    for (i in seq_along(prior)) {
      validate_prior(prior[[i]], sprintf("beta[%d]", i))
    }
    prior_list <- prior
  } else {
    stop("`prior_beta` must be a single prior or a list of priors", call. = FALSE)
  }

  # Check all elements share a distribution family and df
  dists <- vapply(prior_list, function(p) p$distribution, character(1))
  if (length(unique(dists)) > 1L) {
    stop("All per-coefficient priors must use the same distribution family ",
         "(mixed normal / student_t is not supported by the Stan dispatch).",
         call. = FALSE)
  }
  if (dists[[1L]] == "exponential") {
    stop("Exponential priors are not supported for regression coefficients.",
         call. = FALSE)
  }
  dfs <- vapply(prior_list, function(p) if (p$distribution == "student_t") p$df else NA_real_,
                numeric(1))
  if (dists[[1L]] == "student_t" && length(unique(dfs)) > 1L) {
    stop("All per-coefficient Student-t priors must share the same df.",
         call. = FALSE)
  }

  means <- vapply(prior_list, function(p) as.numeric(p$mean)[[1L]], numeric(1))
  sds   <- vapply(prior_list, function(p) as.numeric(p$sd)[[1L]],   numeric(1))
  autos <- vapply(prior_list, function(p) isTRUE(p$autoscale),      logical(1))

  # Under the normal identity link a coefficient is in outcome units per
  # covariate unit, so a default or autoscaled prior carries the outcome SD.
  on_y <- vapply(prior_list, .on_outcome_scale, logical(1)) | autos
  y_scale <- ifelse(on_y, sd_y, 1)
  means <- means * y_scale
  sds <- sds * y_scale

  if (any(autos)) {
    if (is.null(sd_x)) {
      stop("`autoscale = TRUE` requires covariate SDs; did you call ",
           "stan_prior_fields_beta() without sd_x?", call. = FALSE)
    }
    if (length(sd_x) != n_cov) {
      stop("`sd_x` must have length n_cov", call. = FALSE)
    }
    # A covariate with no usable SD keeps the unscaled prior, with a warning.
    no_scale <- !is.finite(sd_x) | sd_x <= 0
    zero_var <- autos & no_scale
    if (any(zero_var)) {
      if (is.null(covariate_names) || length(covariate_names) != n_cov) {
        bad <- sprintf("column %d", which(zero_var))
      } else {
        bad <- covariate_names[zero_var]
      }
      warning(sprintf(
        paste0("`autoscale = TRUE` is requested for covariate(s) with no ",
               "usable empirical SD in the IPD (zero or undefined): %s. ",
               "Their prior scale is used as supplied."),
        paste(bad, collapse = ", ")
      ), call. = FALSE)
    }
    sd_x_safe <- ifelse(no_scale, 1, sd_x)
    means <- ifelse(autos, means / sd_x_safe, means)
    sds <- ifelse(autos, sds / sd_x_safe, sds)
  }

  dist_code <- switch(dists[[1L]], normal = 0L, student_t = 1L)
  df_value <- if (dists[[1L]] == "student_t") dfs[[1L]] else 3

  list(
    mean = means,
    sd = sds,
    dist = dist_code,
    df = df_value,
    autoscale = autos,
    sd_y = y_scale
  )
}

#' Simulated treatment comparison via G-computation
#'
#' Perform an unanchored simulated treatment comparison (STC) with marginal
#' standardization. Fits an outcome regression to the single IPD treatment arm,
#' standardizes its predictions over the comparator-population covariate
#' distribution, and contrasts that outcome with the reported comparator-arm
#' outcome in the same population. It does not standardize either treatment to
#' the index population.
#'
#' For binomial outcomes the result carries the link-scale effect, the
#' standardized and observed event probabilities, the risk difference and the
#' log risk ratio, with delta-method Wald intervals; the observed comparator
#' proportion gets the exact Clopper-Pearson interval, as in [naive()]. An
#' observed comparator arm with zero or all events uses the pseudo-count
#' `(r + 0.5) / (n + 1)` in transformed measures. For Poisson outcomes `$rd`
#' is a rate difference per unit exposure and `$estimate` the log rate ratio,
#' with a 0.5 continuity correction for a zero comparator count.
#'
#' Scale note: `$estimate` (and the binomial `$log_rr`) is on the link / log
#' scale, where the null is 0. To compare against the natural-scale risk ratio
#' or rate ratio from [marginal_effects()] (where the null is 1), exponentiate
#' it (e.g. `exp(result$estimate)`).
#'
#' Normal-family weighting note: across multiple AgD rows, the normal STC
#' comparator-population prediction and observed mean use sample-size
#' (`outcome_n`) weights, matching the Bayesian ML-UMR comparator-population
#' estimand. `outcome_n` is required when there is more than one row; a single
#' row has weight one. The observed comparator-mean variance combines
#' independent, mutually exclusive strata as `sum(w^2 * se^2)` using normalized
#' population weights.
#'
#' @param data An `mlumr_data` object from [combine_data()]. Integration points
#'   from [add_integration()] are required whenever the outcome model uses a
#'   nonlinear link (binomial, Poisson, normal-log, or survival). Substitution
#'   of aggregate means is exact only for a normal identity-link model.
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
#'   poisson: `"log"` (default). Ignored for survival. If `NULL`, uses the
#'   canonical default.
#' @param conf_level Confidence level for the interval (default 0.95)
#' @param distribution For `family = "survival"`: the parametric distribution
#'   of the survival G-computation (default `"weibull"`), fitted with
#'   \pkg{flexsurv}; the estimand is the restricted mean survival time
#'   difference. `"mspline"` and `"pexp"` have no parametric analogue and fall
#'   back to a Weibull fit with a warning, recorded as `approximated = TRUE`.
#'   Survival STC supports right-censored data without delayed entry.
#' @param n_boot For `family = "survival"` only: number of bootstrap resamples
#'   for the RMST-difference standard error (default `200`; 0 gives a point
#'   estimate with no interval). Other families use the delta method.
#' @param seed For `family = "survival"` only: optional integer seed for the
#'   bootstrap, making the standard error reproducible. The global random
#'   number stream is restored on exit. Ignored for other families.
#' @param rmst_horizon For `family = "survival"` only: the restriction time of
#'   the RMST difference. Defaults to the largest observed time across both
#'   arms; set it explicitly to match an [mlumr()] fit, whose default can
#'   differ. A horizon beyond the observed range extrapolates and warns.
#'
#' @return An object of class `mlumr_stc`. Its `separation` component records
#'   the outcome of the binomial separation check: `status` is
#'   `"not_separated"` when the exact check ran and found none, `"unknown"`
#'   when only the fitted-value screen ran (it cannot see quasi-complete
#'   separation), and `"not_applicable"` for other outcome models. A
#'   separated fit, and a Poisson fit with no events, are refused rather than
#'   returned.
#' @importFrom stats gaussian poisson dnorm
#' @export
#'
#' @details
#' A GLM is fitted to the IPD, its predictions are averaged over the
#' comparator covariate distribution (the integration points, or the AgD means
#' for the identity-link normal case) on the response scale, and the average
#' is contrasted with the reported comparator outcome, as in Ren et al.'s
#' unanchored STC. Standard errors are first-order delta-method values,
#' conditional on the integration grid and the reported comparator summaries.
#' The estimand is `E_B[m_A(X)] - E_B[Y_B]` in the comparator population
#' (`$rd` for binomial, `$md` for normal); `$estimate` is the link-scale
#' contrast of the two standardized quantities. Survival STC contrasts the
#' index RMST standardized to the comparator covariates with the RMST of an
#' intercept-only [flexsurv::flexsurvreg()] fit to the pseudo-IPD. The effect
#' is not transported to the index population; `mlumr(model = "relaxed")`
#' estimates comparator-specific covariate effects for any other target.
#'
#' @references
#' Ren S, Ren S, Welton NJ, Strong M (2024). Advancing unanchored simulated
#' treatment comparisons: A novel implementation and simulation study.
#' *Research Synthesis Methods*, 15(4), 657-670.
#' \doi{10.1002/jrsm.1718}
#'
#' Remiro-Azocar A, Heath A, Baio G (2022). Parametric G-computation for
#' compatible indirect treatment comparisons with limited individual patient
#' data. *Research Synthesis Methods*, 13(6), 716-744.
#' \doi{10.1002/jrsm.1565}
#'
#' @examples
#' \dontrun{
#' result <- stc(dat)
#' print(result)
#' }
stc <- function(data, link = NULL, conf_level = 0.95, distribution = "weibull",
                n_boot = 200L, seed = NULL, rmst_horizon = NULL) {

  .validate_mlumr_data_object(data)

  family <- data$family %||% "binomial"
  z <- .z_from_conf_level(conf_level)

  if (family == "survival") {
    if (!isTRUE(data$has_integration)) {
      stop("Survival STC requires comparator-population integration points. ",
           "Call add_integration() with a joint covariate distribution; ",
           "substituting aggregate means is not marginal standardization.",
           call. = FALSE)
    }
    .validate_mlumr_integer(n_boot, "n_boot", lower = 0L)
    if (n_boot == 1L) {
      stop("`n_boot` must be 0 (no bootstrap) or at least 2: the standard ",
           "error of a single resample is undefined.", call. = FALSE)
    }
    if (!is.null(seed)) {
      .validate_mlumr_integer(seed, "seed", lower = 0L)
    }
    out <- .stc_survival(data, conf_level, z, distribution,
                         n_boot = as.integer(n_boot), seed = seed,
                         rmst_horizon = rmst_horizon)
    # No binomial GLM, so the separation field says so instead of being absent.
    out$separation <- list(
      status = "not_applicable",
      reason = paste("survival STC fits no binomial GLM, so the separation",
                     "test does not apply")
    )
    class(out) <- c("mlumr_stc", "list")
    return(out)
  }

  ipd <- data$ipd$data
  agd <- data$agd$data
  cov_names <- data$covariates

  link_info <- check_link(family, link)
  link_resolved <- link_info$link

  if (!isTRUE(data$has_integration) &&
        !(family == "normal" && link_resolved == "identity")) {
    stop("STC with a nonlinear link requires comparator-population integration ",
         "points. Call add_integration(); substituting aggregate covariate ",
         "means is generally biased.", call. = FALSE)
  }

  if (family == "binomial") {
    glm_family <- binomial(link = link_resolved)
  } else if (family == "normal") {
    glm_family <- gaussian(link = link_resolved)
  } else {
    glm_family <- poisson(link = link_resolved)
  }

  fit <- glm(.stc_formula(cov_names, family), family = glm_family, data = ipd)
  glm_params <- .stc_glm_parameters(fit)

  comparator <- .stc_comparator_data(data, cov_names, family)
  newdata <- comparator$newdata
  n_int <- comparator$n_int

  beta_hat <- glm_params$beta_hat
  V <- glm_params$V

  out <- switch(family,
    binomial = .stc_binomial(data, fit, ipd, agd, newdata, link_resolved,
                             conf_level, z, beta_hat, V, n_int),
    normal = .stc_normal(data, fit, ipd, agd, newdata, link_resolved,
                         conf_level, z, beta_hat, V, n_int),
    poisson = .stc_poisson(data, fit, ipd, agd, newdata, link_resolved,
                           conf_level, z, beta_hat, V, n_int)
  )

  out$separation <- glm_params$separation
  class(out) <- c("mlumr_stc", "list")
  out
}

#' Build the STC GLM formula without assuming syntactic covariate names
#' @noRd
.stc_formula <- function(cov_names, family) {
  terms <- lapply(cov_names, as.name)
  if (family == "poisson") {
    terms <- c(terms, list(call("offset", call("log", as.name(".exposure")))))
  }
  rhs <- Reduce(function(left, right) call("+", left, right), terms)
  stats::as.formula(call("~", as.name(".outcome"), rhs),
                    env = parent.frame())
}

#' Validate fitted STC GLM parameters before delta-method calculations
#' @noRd
.stc_glm_parameters <- function(fit) {
  if (!isTRUE(fit$converged)) {
    stop("STC GLM did not converge; check the IPD model or use mlumr().",
         call. = FALSE)
  }
  # Decided before the coefficients are read: a fit with no finite maximum
  # can stop at finite numbers.
  fam <- tryCatch(stats::family(fit)$family, error = function(e) NA_character_)
  separation <- if (identical(fam, "poisson")) {
    .stc_refuse_poisson_recession(fit)
  } else {
    .stc_refuse_separation(fit)
  }
  beta_hat <- coef(fit)
  V <- vcov(fit)
  if (anyNA(beta_hat) || anyNA(V)) {
    stop(
      paste(
        "STC GLM produced aliased coefficients or covariance terms.",
        "Check for collinear covariates or insufficient variation."
      ),
      call. = FALSE
    )
  }
  if (any(!is.finite(beta_hat)) || any(!is.finite(V))) {
    stop(
      paste(
        "STC GLM produced non-finite coefficients or covariance terms.",
        "This can occur with separation, collinearity, or insufficient",
        "outcome variation; consider mlumr() for a Bayesian fit."
      ),
      call. = FALSE
    )
  }
  list(beta_hat = beta_hat, V = V, separation = separation)
}


#' Refuse a Poisson fit with no events
#'
#' With no events the Poisson likelihood rises toward a supremum it never
#' attains as the log rate falls, so no finite coefficient maximizes it, yet
#' iterative reweighting stops and reports convergence.
#' @param fit A fitted Poisson `glm`.
#' @return The status list recorded on the result, invisibly.
#' @noRd
.stc_refuse_poisson_recession <- function(fit) {
  if (all(fit$y == 0)) {
    stop("The STC outcome model has no events, so the Poisson likelihood ",
         "has no finite maximum and the fit would describe where the ",
         "iteration stopped. Use mlumr(), whose prior makes the posterior ",
         "proper.", call. = FALSE)
  }
  invisible(list(
    status = "not_applicable",
    reason = paste("the outcome model is Poisson, so the binomial separation",
                   "test does not apply")
  ))
}

#' Refuse a separated binomial fit
#'
#' A separated GLM reports convergence with finite coefficients. Complete
#' separation shows in the fitted values; quasi-complete separation needs the
#' linear program in [.stc_separation_status()].
#' @return The separation status, invisibly (`status` and `reason`). A
#'   separated fit throws instead.
#' @noRd
.stc_refuse_separation <- function(fit) {
  fam <- tryCatch(stats::family(fit)$family, error = function(e) NA_character_)
  if (!identical(fam, "binomial")) {
    return(invisible(list(
      status = "not_applicable",
      reason = paste("the outcome model is not binomial, so the separation",
                     "test does not apply")
    )))
  }
  mu <- stats::fitted(fit)
  mu <- mu[is.finite(mu)]
  if (!length(mu)) {
    return(invisible(list(
      status = "unknown",
      reason = "the fit has no finite fitted values to screen"
    )))
  }
  eps <- .Machine$double.eps^0.5
  # Every fitted probability at a boundary, not all at the same one: a
  # perfectly separating covariate sends the two groups to opposite ends.
  if (all(mu < eps | mu > 1 - eps)) {
    stop("The STC outcome model is separated: every fitted probability sits ",
         "at 0 or 1, so the maximum likelihood estimate is not finite. Use ",
         "mlumr(), whose prior makes the posterior proper.", call. = FALSE)
  }
  exact <- .stc_separation_status(fit)
  if (identical(exact$status, "unknown")) {
    warning("The exact separation check did not run for the STC outcome ",
            "model, because ", exact$reason, ". The fitted-value screen ",
            "cannot see quasi-complete separation, so treat the estimate as ",
            "unverified; install detectseparation to run the check.",
            call. = FALSE)
  }
  if (identical(exact$status, "separated")) {
    stop("The STC outcome model is separated: a linear combination of the ",
         "covariates separates the outcome, so the maximum likelihood ",
         "estimate is not finite. Use mlumr(), whose prior makes the ",
         "posterior proper.", call. = FALSE)
  }
  invisible(exact)
}


#' Exact separation test, when the optional dependency is present
#'
#' Separation is a linear-programming question, which \pkg{detectseparation}
#' solves. Without it, or when the refit errors, the status is `"unknown"`
#' with the reason; a warning from the refit is muffled, since a separated
#' fit is the case that warns.
#'
#' @param fit A fitted binomial `glm`.
#' @return A list with `status`, one of `"separated"`, `"not_separated"` or
#'   `"unknown"`, and `reason`, a string explaining an unknown.
#' @noRd
.stc_separation_status <- function(fit) {
  unknown <- function(reason) list(status = "unknown", reason = reason)
  if (!requireNamespace("detectseparation", quietly = TRUE)) {
    return(unknown(paste("the optional detectseparation package is not",
                         "installed, so the linear program was not run")))
  }
  # Rebuild the fit's own call in the environment its formula was built in;
  # `update()` would evaluate in this frame and not find the data.
  cl <- stats::getCall(fit)
  if (is.null(cl)) {
    return(unknown("the fit records no call, so it could not be re-run"))
  }
  cl$method <- quote(detectseparation::detect_separation)
  env <- environment(stats::formula(fit))
  if (!is.environment(env)) {
    env <- parent.frame()
  }
  # Warnings are muffled: a separated refit is the case that warns.
  failure <- NULL
  outcome <- tryCatch(
    withCallingHandlers(eval(cl, env)$outcome,
                        warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) {
      failure <<- conditionMessage(e)
      NA
    }
  )
  if (!is.null(failure)) {
    return(unknown(paste0("the linear program could not be run: ", failure)))
  }
  if (length(outcome) != 1L || !is.logical(outcome) || is.na(outcome)) {
    return(unknown("the linear program returned no usable outcome"))
  }
  list(status = if (outcome) "separated" else "not_separated", reason = NA)
}

#' Build a model matrix aligned with fitted GLM coefficients
#' @noRd
.stc_model_matrix <- function(fit, newdata) {
  X <- stats::model.matrix(stats::delete.response(stats::terms(fit)), newdata)
  beta_names <- names(coef(fit))
  missing_cols <- setdiff(beta_names, colnames(X))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Cannot build STC model matrix columns: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  X[, beta_names, drop = FALSE]
}

#' Binomial STC estimator
#' @noRd
.stc_binomial <- function(data, fit, ipd, agd, newdata, link_resolved,
                          conf_level, z, beta_hat, V, n_int) {
  weights <- if (data$has_integration) rep(agd$.n, each = n_int) else agd$.n
  eta_comp <- as.numeric(predict(fit, newdata = newdata, type = "link"))
  lp_comp <- .binary_log_probs(eta_comp, link_resolved)
  log_p_A_comp <- .weighted_log_mean_exp(lp_comp$event, weights)
  log_q_A_comp <- .weighted_log_mean_exp(lp_comp$nonevent, weights)
  p_hat_A_comp <- exp(log_p_A_comp)
  n_B <- sum(agd$.n)
  p_B <- sum(agd$.r) / n_B
  p_B_effect <- bound_probability(p_B, n_B)
  row_p <- agd$.r / agd$.n
  row_w <- .normalize_weights(agd$.n)
  # Boundary correction from the pooled n, as `.naive_binomial()` does, so
  # the answer does not depend on how the arm was tabulated.
  row_p_effect <- bound_probability(row_p, n_B)
  var_p_B_effect <- sum(
    row_w^2 * row_p_effect * (1 - row_p_effect) / agd$.n
  )

  comp_delta <- .stc_binomial_comparator_delta(
    fit, newdata, weights, beta_hat, V, link_resolved,
    log_p_A_comp, log_q_A_comp, p_B_effect, var_p_B_effect
  )

  estimate <- comp_delta$link_effect
  se <- comp_delta$link_effect_se
  p_hat_A_comp_se <- .sqrt_variance(comp_delta$var_p_A,
                                    "comparator probability variance")
  # Boundary-corrected variance on the absolute scale too, so a zero-event
  # comparator arm still contributes uncertainty.
  p_B_se <- .sqrt_variance(var_p_B_effect, "comparator probability variance")
  # The standardized index probability is a prediction (delta-method
  # interval); the observed comparator arm gets an exact interval.
  p_hat_A_comp_ci <- .bounded_wald_interval(p_hat_A_comp, p_hat_A_comp_se, z,
                                            lower = 0, upper = 1)
  p_B_ci <- .clopper_pearson_interval(sum(agd$.r), n_B, conf_level)

  rd <- p_hat_A_comp - p_B
  se_rd <- .sqrt_variance(comp_delta$var_p_A + var_p_B_effect,
                          "risk-difference variance")

  log_rr <- log_p_A_comp - log(p_B_effect)
  se_log_rr <- .sqrt_variance(
    comp_delta$var_log_p_A + var_p_B_effect / p_B_effect^2,
    "log-risk-ratio variance"
  )

  list(
    estimate = estimate,
    link_effect = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "binomial",
    link = link_resolved,
    population = "comparator",
    p_index_comparator = p_hat_A_comp,
    p_hat_index = p_hat_A_comp,
    p_hat_index_se = p_hat_A_comp_se,
    p_hat_index_lower = p_hat_A_comp_ci$lower,
    p_hat_index_upper = p_hat_A_comp_ci$upper,
    p_comparator_comparator = p_B,
    p_comparator = p_B,
    p_comparator_se = p_B_se,
    p_comparator_lower = p_B_ci$lower,
    p_comparator_upper = p_B_ci$upper,
    rd = rd,
    rd_se = se_rd,
    rd_lower = rd - z * se_rd,
    rd_upper = rd + z * se_rd,
    log_rr = log_rr,
    log_rr_se = se_log_rr,
    log_rr_lower = log_rr - z * se_log_rr,
    log_rr_upper = log_rr + z * se_log_rr,
    glm_fit = fit,
    data = data
  )
}

#' Comparator-population delta-method terms for binomial STC
#'
#' The standardized event probability is `p = sum(w_i p_i) / sum(w_i)` over
#' the comparator grid; its gradient in the coefficients is analytic, so the
#' uncertainty does not depend on the predictors' units. The gradients are
#' formed from the log probabilities; see [.stc_binomial_gradients()].
#' @noRd
.stc_binomial_comparator_delta <- function(fit, newdata, weights,
                                           beta_hat, V, link_resolved,
                                           log_p_A, log_q_A, p_B,
                                           var_p_B) {
  X_comp_design <- .stc_model_matrix(fit, newdata)
  eta <- as.vector(X_comp_design %*% beta_hat)
  grads <- .stc_binomial_gradients(X_comp_design, eta, weights, link_resolved)
  grad_link <- grads$link
  grad_mean <- grads$mean
  grad_log_p <- grads$log_mean

  var_link_A <- as.numeric(t(grad_link) %*% V %*% grad_link)
  var_link_B <- link_derivative_response(p_B, link_resolved)^2 * var_p_B
  var_link_effect <- .nonnegative_variance(var_link_A + var_link_B,
                                           "comparator link-effect variance")
  var_p_A <- .nonnegative_variance(as.numeric(t(grad_mean) %*% V %*% grad_mean),
                                   "comparator probability variance")
  var_log_p_A <- .nonnegative_variance(
    as.numeric(t(grad_log_p) %*% V %*% grad_log_p),
    "comparator log-probability variance"
  )

  list(
    link_effect = .binary_link_from_logs(log_p_A, log_q_A, link_resolved) -
      link_fun(p_B, link_resolved),
    link_effect_se = sqrt(var_link_effect),
    var_link_effect = var_link_effect,
    var_p_A = var_p_A,
    var_log_p_A = var_log_p_A
  )
}

#' Analytic gradients of the standardized binomial functionals
#'
#' With `log p_i` and `log q_i` the event and non-event log probabilities at
#' each grid point, the standardized log probability is
#' `log(sum(w_i p_i) / sum(w_i))` and its gradient is
#' `sum(c_i (d log p_i / d eta) X_i)` with `c_i = w_i p_i / sum(w p)`. The
#' per-point derivatives are `q_i` (logit), `phi(eta) / Phi(eta)` (probit)
#' and `exp(eta - exp(eta)) / p_i` (cloglog), formed from the log
#' probabilities so tail points keep their share. The link-scale functional
#' follows by the chain rule on the two log means.
#'
#' @param X Comparator design, one row per grid point.
#' @param eta Linear predictor at each grid point.
#' @param weights Non-negative weights, one per grid point.
#' @param link The binomial link.
#' @return List of gradient vectors `log_mean`, `log_nonevent_mean`, `mean`
#'   and `link`, one entry per coefficient.
#' @noRd
.stc_binomial_gradients <- function(X, eta, weights,
                                    link = c("logit", "probit", "cloglog")) {
  link <- match.arg(link)
  if (any(!is.finite(weights)) || any(weights < 0)) {
    stop("`weights` must be finite and non-negative.", call. = FALSE)
  }
  # A point with no weight has no share; keeping it would put log(0) beside
  # a log probability of -Inf and make NaN of nothing.
  keep <- weights > 0
  X <- X[keep, , drop = FALSE]
  eta <- eta[keep]
  weights <- weights[keep]
  lp <- .binary_log_probs(eta, link)
  if (link == "logit") {
    d_log_p <- exp(lp$nonevent)
    d_log_q <- -exp(lp$event)
  } else if (link == "probit") {
    log_phi <- stats::dnorm(eta, log = TRUE)
    d_log_p <- exp(log_phi - lp$event)
    d_log_q <- -exp(log_phi - lp$nonevent)
  } else {
    d_log_p <- exp(eta - exp(eta) - lp$event)
    d_log_q <- NULL
  }
  # Normalized by a shifted log-sum-exp, as [.weighted_log_mean_exp()] does.
  log_weights <- log(weights)
  m_w <- max(log_weights)
  log_w <- log_weights - (m_w + log(sum(exp(log_weights - m_w))))
  log_p_mean <- .weighted_log_mean_exp(lp$event, weights)
  log_q_mean <- .weighted_log_mean_exp(lp$nonevent, weights)
  # The share `w_i p_i / sum(w p)`, formed after cancelling the largest log
  # probability so the shares sum to 1 by construction even when the log
  # probabilities are of order 1e17.
  log_shares <- function(x) {
    m_x <- max(x)
    z <- (x - m_x) + log_w
    m_z <- max(z)
    z - (m_z + log(sum(exp(z - m_z))))
  }
  log_share_p <- log_shares(lp$event)
  log_share_q <- log_shares(lp$nonevent)
  share_p <- exp(log_share_p)
  share_q <- exp(log_share_q)
  grad_log_p <- colSums(share_p * d_log_p * X)
  grad_log_q <- if (link == "cloglog") {
    # Formed as one exponent so a saturated point (share 0, derivative
    # -Inf) underflows to 0 rather than NaN.
    colSums(-exp(log_share_q + eta) * X)
  } else {
    colSums(share_q * d_log_q * X)
  }
  grad_mean <- exp(log_p_mean) * grad_log_p
  grad_link <- if (link == "logit") {
    grad_log_p - grad_log_q
  } else if (link == "probit") {
    z <- .binary_link_from_logs(log_p_mean, log_q_mean, link)
    log_phi_z <- stats::dnorm(z, log = TRUE)
    if (log_p_mean <= log(0.5)) {
      exp(log_p_mean - log_phi_z) * grad_log_p
    } else {
      -exp(log_q_mean - log_phi_z) * grad_log_q
    }
  } else if (log_q_mean == 0) {
    # Only where `log q` has rounded to zero is the link `log p` to double
    # precision.
    grad_log_p
  } else {
    grad_log_q / log_q_mean
  }
  list(log_mean = grad_log_p, log_nonevent_mean = grad_log_q,
       mean = grad_mean, link = grad_link)
}

#' Stable Euclidean norm of two standard errors
#' @noRd
.stc_hypot <- function(x, y) {
  if (any(is.infinite(c(x, y)))) return(Inf)
  scale <- max(abs(c(x, y)))
  if (scale == 0) return(0)
  scale * sqrt((x / scale)^2 + (y / scale)^2)
}

#' Normal-outcome STC estimator
#' @noRd
.stc_normal <- function(data, fit, ipd, agd, newdata, link_resolved,
                        conf_level, z, beta_hat, V, n_int) {
  if (nrow(agd) > 1L && is.null(agd$.n)) {
    stop("`outcome_n` is required for multiple normal AgD rows.", call. = FALSE)
  }
  agd_weights <- agd$.n %||% 1
  weights <- if (data$has_integration) {
    rep(agd_weights, each = n_int)
  } else {
    agd_weights
  }
  y_B <- sum(.normalize_weights(agd_weights) * agd$.y)
  X_comp_design <- .stc_model_matrix(fit, newdata)
  w_norm <- .normalize_weights(weights)
  eta_comp <- as.vector(X_comp_design %*% beta_hat)

  if (link_resolved == "identity") {
    y_hat_A <- sum(w_norm * eta_comp)
    grad <- colSums(w_norm * X_comp_design)
    var_A <- .nonnegative_variance(as.numeric(t(grad) %*% V %*% grad),
                                   "normal STC standardized-mean variance")
    se_A <- sqrt(var_A)
  } else {
    log_y_hat_A <- .weighted_log_mean_exp(eta_comp, weights)
    y_hat_A <- exp(log_y_hat_A)
    log_contribution <- log(weights) + eta_comp
    contribution_max <- max(log_contribution)
    centered_weights <- exp(log_contribution - contribution_max)
    centered_weights <- centered_weights / sum(centered_weights)
    grad_log <- colSums(centered_weights * X_comp_design)
    var_log_A <- .nonnegative_variance(
      as.numeric(t(grad_log) %*% V %*% grad_log),
      "normal STC standardized log-mean variance"
    )
    se_A <- if (var_log_A == 0) 0 else
      exp(log_y_hat_A + 0.5 * log(var_log_A))
  }
  w_B <- .normalize_weights(agd_weights)
  var_B <- sum(w_B^2 * agd$.se^2)
  se_B <- sqrt(var_B)
  md <- if (link_resolved == "log" && y_B > 0) {
    .exp_difference_logs(log_y_hat_A, log(y_B))
  } else {
    y_hat_A - y_B
  }
  md_se <- .stc_hypot(se_A, se_B)
  if (link_resolved == "log") {
    if (!is.finite(y_B) || y_B <= 0) {
      stop("Normal-log STC requires a positive comparator aggregate mean.",
           call. = FALSE)
    }
    estimate <- log_y_hat_A - log(y_B)
    se <- .sqrt_variance(var_log_A + var_B / y_B^2,
                         "normal STC log-mean-ratio variance")
  } else {
    estimate <- md
    se <- md_se
  }
  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "normal",
    link = link_resolved,
    population = "comparator",
    md = md,
    md_se = md_se,
    md_lower = md - z * md_se,
    md_upper = md + z * md_se,
    y_index_comparator = y_hat_A,
    y_hat_index = y_hat_A,
    y_hat_index_se = se_A,
    y_comparator = y_B,
    y_comparator_se = sqrt(var_B),
    glm_fit = fit,
    data = data
  )
}

#' Poisson-outcome STC estimator
#' @noRd
.stc_poisson <- function(data, fit, ipd, agd, newdata, link_resolved,
                         conf_level, z, beta_hat, V, n_int) {
  weights <- if (data$has_integration) rep(agd$.E, each = n_int) else agd$.E
  events_B <- sum(agd$.r)
  exposure_B <- sum(agd$.E)
  rate_B <- events_B / exposure_B
  events_B_adjusted <- max(events_B, 0.5)
  rate_B_for_log <- events_B_adjusted / exposure_B
  X_comp_design <- .stc_model_matrix(fit, newdata)
  eta_comp <- as.vector(X_comp_design %*% beta_hat)
  log_rate_hat_A <- .weighted_log_mean_exp(eta_comp, weights)
  rate_hat_A <- exp(log_rate_hat_A)
  estimate <- log_rate_hat_A - log(rate_B_for_log)
  log_contribution <- log(weights) + eta_comp
  contribution_max <- max(log_contribution)
  contribution <- exp(log_contribution - contribution_max)
  contribution <- contribution / sum(contribution)
  grad_log_rate <- colSums(contribution * X_comp_design)
  var_lrr_A <- .nonnegative_variance(
    as.numeric(t(grad_log_rate) %*% V %*% grad_log_rate),
    "poisson STC standardized log-rate variance"
  )
  var_lrr_B <- 1 / events_B_adjusted
  se <- .sqrt_variance(var_lrr_A + var_lrr_B,
                       "poisson STC contrast variance")

  # Gradient of the standardized rate itself, a different weighting from the
  # log-rate gradient above.
  w_norm <- weights / sum(weights)
  lambda_comp <- exp(eta_comp)
  grad_rate <- colSums(w_norm * lambda_comp * X_comp_design)
  var_rate_A <- .nonnegative_variance(
    as.numeric(t(grad_rate) %*% V %*% grad_rate),
    "poisson STC rate variance"
  )
  # Rate difference per unit exposure; the arms are independent, so the
  # variances add, with the comparator count continuity corrected.
  rd <- rate_hat_A - rate_B
  var_rd <- var_rate_A + events_B_adjusted / exposure_B^2
  se_rd <- .sqrt_variance(var_rd, "poisson STC rate-difference variance")

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "poisson",
    link = link_resolved,
    population = "comparator",
    rd = rd,
    rd_se = se_rd,
    rd_lower = rd - z * se_rd,
    rd_upper = rd + z * se_rd,
    rate_hat_index = rate_hat_A,
    rate_hat_index_se = sqrt(var_rate_A),
    rate_comparator = rate_B,
    rate_comparator_se = sqrt(events_B_adjusted) / exposure_B,
    events_comparator = events_B,
    exposure_comparator = exposure_B,
    events_comparator_adjusted = events_B_adjusted,
    glm_fit = fit,
    data = data
  )
}

#' Build comparator-population covariates for STC prediction
#' @noRd
.stc_comparator_data <- function(data, cov_names, family) {
  agd <- data$agd$data
  n_int <- NULL

  if (data$has_integration) {
    X_int <- data$integration_points
    n_agd_rows <- dim(X_int)[1]
    n_int <- dim(X_int)[2]

    X_comp <- matrix(NA_real_,
                     nrow = n_agd_rows * n_int,
                     ncol = length(cov_names))
    colnames(X_comp) <- cov_names
    for (k in seq_len(n_agd_rows)) {
      rows <- ((k - 1) * n_int + 1):(k * n_int)
      X_comp[rows, ] <- X_int[k, , ]
    }
    newdata <- as.data.frame(X_comp)
  } else {
    # Reached only where the mean profile IS the standardized quantity: stc()
    # rejects a nonlinear link without integration points up front, because
    # g^-1(mu + Xbar'beta) is not E_X[g^-1(mu + X'beta)] there.
    newdata <- .stc_agd_mean_newdata(agd, cov_names)
  }

  if (family == "poisson") {
    newdata$.exposure <- 1
  }

  list(newdata = newdata, n_int = n_int)
}

#' Package-specific parametric survival G-computation (RMST difference)
#'
#' Fits a parametric survival model to the index IPD (adjusting for
#' covariates), G-computes the marginal restricted mean survival time (RMST) in
#' the comparator population, and contrasts it with the comparator RMST from the
#' reconstructed pseudo-IPD. Standard errors come from a nonparametric
#' bootstrap. This survival extension is a package benchmark, not the
#' established binary/continuous/count STC procedure. Requires the `flexsurv`
#' package.
#' @noRd
.stc_survival <- function(data, conf_level, z, distribution, n_boot = 200L,
                          seed = NULL, rmst_horizon = NULL) {
  if (!requireNamespace("flexsurv", quietly = TRUE)) {
    stop("Package 'flexsurv' is required for survival STC. ",
         "Install it or use mlumr() / naive().", call. = FALSE)
  }
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  cov_names <- data$covariates
  # Flexible baselines have no flexsurv analogue: fit a Weibull and say so.
  valid_distributions <- c("exponential", "weibull", "gompertz",
                           "exponential-aft", "weibull-aft", "lognormal",
                           "loglogistic", "gamma", "gengamma",
                           "mspline", "pexp")
  if (!is.character(distribution) || length(distribution) != 1L ||
        is.na(distribution) || !distribution %in% valid_distributions) {
    stop("`distribution` must be one of: ",
         paste(valid_distributions, collapse = ", "), ".", call. = FALSE)
  }
  approximated <- distribution %in% c("mspline", "pexp")
  dist_fit <- if (approximated) "weibull" else distribution
  if (approximated) {
    warning(sprintf(paste0("Survival STC has no parametric analogue for a ",
                           "'%s' baseline; a Weibull G-computation is fitted ",
                           "instead (distribution_fit = \"weibull\")."),
                    distribution), call. = FALSE)
  }
  dist_fs <- .stc_flexsurv_dist(dist_fit)
  # RMST at another horizon is another estimand, so the horizon is settable.
  horizon <- if (is.null(rmst_horizon)) {
    max(c(ipd$.time, pseudo$.time))
  } else {
    if (!is.numeric(rmst_horizon) || length(rmst_horizon) != 1L ||
          !is.finite(rmst_horizon) || rmst_horizon <= 0) {
      stop("`rmst_horizon` must be a single positive finite time.",
           call. = FALSE)
    }
    obs_max <- max(c(ipd$.time, pseudo$.time))
    if (rmst_horizon > obs_max) {
      warning(sprintf(paste0("`rmst_horizon` = %.4g is beyond the largest ",
                             "observed time (%.4g); the fitted parametric ",
                             "survival function is extrapolated past the data ",
                             "there."), rmst_horizon, obs_max), call. = FALSE)
    }
    rmst_horizon
  }
  comp_cov <- .stc_comparator_data(data, cov_names, "survival")$newdata

  .validate_stc_survival_right_censored(ipd, pseudo)
  .validate_stc_survival_events(ipd, pseudo)

  point <- .stc_survival_point(ipd, pseudo, cov_names, comp_cov, dist_fs, horizon)

  # A negative fitted shape or Q is outside the parameter space of the
  # Bayesian model of the same name, so the two are different families there.
  out_of_family <- NULL
  if (!is.null(point$family_par_name) && any(point$family_par < 0)) {
    out_of_family <- point$family_par_name
    approximated <- TRUE
    dist_fit <- sprintf("flexsurv %s, unrestricted %s", distribution,
                        point$family_par_name)
    warning(sprintf(
      paste0("The STC '%s' fit has %s = %s, outside the parameter space of ",
             "mlumr()'s '%s' model (%s > 0), so the two fits are different ",
             "families here. Compare the RMST estimands, or choose a ",
             "distribution whose parameter spaces agree."),
      distribution, point$family_par_name,
      paste(sprintf("%.4g", point$family_par), collapse = " / "),
      distribution, point$family_par_name
    ), call. = FALSE)
  }

  if (n_boot > 0L) {
    # Seed the bootstrap reproducibly without perturbing the user's global RNG
    # stream: snapshot .Random.seed and restore it when this function returns.
    if (!is.null(seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        saved_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
        on.exit(assign(".Random.seed", saved_seed, envir = globalenv()), # nolint: object_name_linter.
                add = TRUE)
      } else {
        on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())),
                add = TRUE)
      }
      set.seed(seed)
    }
    # Each replicate carries its family parameter too, since a resample can
    # leave mlumr()'s parameter space while the point estimate stays inside.
    boot <- vapply(seq_len(n_boot), function(b) {
      ib <- ipd[sample(nrow(ipd), replace = TRUE), , drop = FALSE]
      pb <- pseudo[sample(nrow(pseudo), replace = TRUE), , drop = FALSE]
      # A resample with no events in an arm is a failed fit, whatever
      # flexsurvreg() returns.
      if (sum(ib$.status == 1L) == 0L || sum(pb$.status == 1L) == 0L) {
        return(rep(NA_real_, 4L))
      }
      tryCatch({
        pt <- .stc_survival_point(ib, pb, cov_names, comp_cov, dist_fs, horizon)
        par_b <- c(NA_real_, NA_real_)
        if (!is.null(pt$family_par)) par_b <- unname(pt$family_par)
        c(pt$rmst_diff, pt$log_chr, par_b)
      }, error = function(e) rep(NA_real_, 4L))
    }, numeric(4))
    se <- stats::sd(boot[1, ], na.rm = TRUE)
    log_chr_se <- stats::sd(boot[2, ], na.rm = TRUE)
    # Counted separately: the cumulative-hazard ratio can be undefined where
    # the RMST difference is finite.
    n_boot_ok <- sum(!is.na(boot[1, ]))
    n_boot_ok_chr <- sum(!is.na(boot[2, ]))
    if (n_boot_ok < n_boot || n_boot_ok_chr < n_boot) {
      warning(sprintf(
        paste0("Bootstrap successes: RMST difference %d/%d; log cumulative-",
               "hazard ratio %d/%d. Each standard error uses its own ",
               "successful resamples; consider another `distribution` or a ",
               "larger `n_boot`."),
        n_boot_ok, n_boot, n_boot_ok_chr, n_boot
      ), call. = FALSE)
    }
    # NA_integer_ when the distribution has no such parameter.
    n_boot_out_of_family <- if (is.null(point$family_par_name)) {
      NA_integer_
    } else {
      as.integer(sum(apply(boot[3:4, , drop = FALSE], 2L,
                           function(v) any(!is.na(v) & v < 0))))
    }
    if (!is.na(n_boot_out_of_family) && n_boot_out_of_family > 0L) {
      warning(sprintf(
        paste0("%d of %d bootstrap resample(s) fitted %s < 0, outside the ",
               "parameter space of mlumr()'s '%s' model, and are included ",
               "in the standard error."),
        n_boot_out_of_family, n_boot, point$family_par_name, distribution
      ), call. = FALSE)
    }
    # Fewer than two successes leaves sd() undefined.
    if (n_boot_ok < 2L) se <- NA_real_
    if (n_boot_ok_chr < 2L) log_chr_se <- NA_real_
  } else {
    se <- NA_real_
    log_chr_se <- NA_real_
    n_boot_ok <- 0L
    n_boot_ok_chr <- 0L
    n_boot_out_of_family <- NA_integer_
  }

  list(
    estimate = point$rmst_diff,
    rmst_diff = point$rmst_diff,
    se = se,
    ci_lower = if (is.na(se)) NA_real_ else point$rmst_diff - z * se,
    ci_upper = if (is.na(se)) NA_real_ else point$rmst_diff + z * se,
    conf_level = conf_level,
    family = "survival",
    population = "comparator",
    method = "package-specific parametric survival G-computation",
    distribution = distribution,
    distribution_fit = dist_fit,
    approximated = approximated,
    # Non-NULL when the fitted shape/Q left the Bayesian model's parameter
    # space; names the parameter that did so.
    out_of_family = out_of_family,
    family_par = point$family_par,
    # The parameter whose sign decides family membership, NULL when none.
    family_par_name = point$family_par_name,
    horizon = horizon,
    rmst_index_comparator = point$rmst_index,
    rmst_index = point$rmst_index,
    rmst_comparator = point$rmst_comparator,
    # Cumulative-hazard ratio at the horizon, not a hazard ratio in general.
    log_chr = point$log_chr,
    chr = exp(point$log_chr),
    log_chr_se = log_chr_se,
    log_chr_lower = if (is.na(log_chr_se)) NA_real_ else point$log_chr - z * log_chr_se,
    log_chr_upper = if (is.na(log_chr_se)) NA_real_ else point$log_chr + z * log_chr_se,
    n_index = nrow(ipd),
    n_comparator = nrow(pseudo),
    n_boot = n_boot_ok,
    n_boot_requested = as.integer(n_boot),
    n_boot_ok = n_boot_ok,
    n_boot_ok_log_chr = n_boot_ok_chr,
    n_boot_out_of_family = n_boot_out_of_family,
    data = data
  )
}

#' Require events in both arms before a parametric survival STC
#'
#' With no events in an arm the likelihood for its event-time distribution has
#' no finite interior maximum. `flexsurvreg()` returns optimizer-boundary
#' parameters with a warning rather than failing, after which an RMST
#' difference and its interval look ordinary.
#' @noRd
.validate_stc_survival_events <- function(ipd, pseudo) {
  n_idx <- sum(ipd$.status == 1L)
  n_cmp <- sum(pseudo$.status == 1L)
  if (n_idx == 0L || n_cmp == 0L) {
    stop("Survival STC needs at least one event in each arm: observed ",
         n_idx, " in the index arm and ", n_cmp, " in the comparator arm.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate survival STC input supported by flexsurv formula construction
#' @noRd
.validate_stc_survival_right_censored <- function(ipd, pseudo) {
  status <- c(ipd$.status, pseudo$.status)
  start_time <- c(ipd$.start_time, pseudo$.start_time)
  delay_time <- c(ipd$.delay_time, pseudo$.delay_time)

  if (any(!status %in% c(0L, 1L)) ||
        any(start_time > 0, na.rm = TRUE) ||
        any(delay_time > 0, na.rm = TRUE)) {
    stop(
      "Survival STC currently supports only right-censored data without delayed entry. ",
      "Use mlumr() for left-censored, interval-censored, or delayed-entry survival data.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Require a flexsurv fit that converged to a maximum
#'
#' `flexsurvreg()` warns rather than fails at an optimizer boundary, so this
#' raises an error and the bootstrap counts such a replicate as failed.
#' @noRd
.validate_flexsurv_fit <- function(fit, arm) {
  conv <- fit$opt$convergence
  if (!is.null(conv) && !identical(as.integer(conv), 0L)) {
    stop("The survival STC fit for the ", arm, " arm did not converge ",
         "(optimizer code ", as.integer(conv), ").", call. = FALSE)
  }
  est <- tryCatch(fit$res[, "est"], error = function(e) NULL)
  if (is.null(est) || anyNA(est) || any(!is.finite(est))) {
    stop("The survival STC fit for the ", arm, " arm returned non-finite ",
         "parameter estimates.", call. = FALSE)
  }
  if (!is.null(fit$cov) && (anyNA(fit$cov) || any(!is.finite(fit$cov)))) {
    stop("The survival STC fit for the ", arm, " arm returned a non-finite ",
         "covariance matrix, so its uncertainty cannot be quantified.",
         call. = FALSE)
  }
  # A finite covariance that is not positive definite is a saddle or
  # boundary point, not a maximum.
  if (!is.null(fit$cov) && length(fit$cov) > 0L) {
    v <- diag(as.matrix(fit$cov))
    ev <- tryCatch(
      eigen(as.matrix(fit$cov), symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) NA_real_
    )
    if (any(v <= 0) || anyNA(ev) || min(ev) <= 0) {
      stop("The survival STC fit for the ", arm, " arm returned a covariance ",
           "matrix that is not positive definite, so the optimizer did not ",
           "stop at a maximum.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' One STC survival point estimate (RMST_index, RMST_comparator, difference)
#' @noRd
.stc_survival_point <- function(ipd, pseudo, cov_names, comp_cov, dist_fs, horizon) {
  ipd$.stc_event <- as.integer(ipd$.status == 1L)
  pseudo$.stc_event <- as.integer(pseudo$.status == 1L)

  # Built from symbols, as `.stc_formula()` does, so any column name parses.
  rhs <- Reduce(function(left, right) call("+", left, right),
                lapply(cov_names, as.name))
  form_a <- stats::as.formula(
    call("~", quote(survival::Surv(.time, .stc_event)), rhs),
    env = parent.frame()
  )
  fit_a <- flexsurv::flexsurvreg(form_a, data = ipd, dist = dist_fs)
  .validate_flexsurv_fit(fit_a, "index")
  rmst_a_rows <- summary(fit_a, newdata = comp_cov, type = "rmst",
                         t = horizon, ci = FALSE, tidy = TRUE)
  # Survival AgD carries one arm, so the grid points are equally weighted.
  rmst_index <- mean(rmst_a_rows$est)

  fit_b <- flexsurv::flexsurvreg(survival::Surv(.time, .stc_event) ~ 1,
                                 data = pseudo, dist = dist_fs)
  .validate_flexsurv_fit(fit_b, "comparator")
  rmst_b <- summary(fit_b, type = "rmst", t = horizon, ci = FALSE,
                    tidy = TRUE)$est[1]

  # Cumulative-hazard ratio at the horizon, H(t) = -log S(t), for the
  # standardized index survival against the comparator. Not a hazard ratio
  # in general; NA where either survival sits at a boundary.
  cumhaz_a_rows <- summary(fit_a, newdata = comp_cov, type = "cumhaz",
                           t = horizon, ci = FALSE, tidy = TRUE)$est
  log_surv_a <- .weighted_log_mean_exp(-cumhaz_a_rows)
  cumhaz_a <- -log_surv_a
  cumhaz_b <- summary(fit_b, type = "cumhaz", t = horizon, ci = FALSE,
                      tidy = TRUE)$est[1]
  log_chr <- if (is.finite(cumhaz_a) && is.finite(cumhaz_b) &&
                   cumhaz_a > 0 && cumhaz_b > 0) {
    log(cumhaz_a) - log(cumhaz_b)
  } else {
    NA_real_
  }

  # flexsurv admits the negative Gompertz shape and negative Q, which the
  # Bayesian models of the same name do not; report the parameter so stc()
  # can say so.
  par_name <- switch(dist_fs, gompertz = "shape", gengamma = "Q", NULL)
  family_par <- NULL
  if (!is.null(par_name)) {
    family_par <- c(index = unname(fit_a$res[par_name, "est"]),
                    comparator = unname(fit_b$res[par_name, "est"]))
  }

  list(rmst_index = rmst_index, rmst_comparator = rmst_b,
       rmst_diff = rmst_index - rmst_b, log_chr = log_chr,
       family_par = family_par, family_par_name = par_name)
}

#' Map an mlumr survival distribution to a flexsurv distribution name
#' @noRd
.stc_flexsurv_dist <- function(distribution) {
  switch(distribution,
    exponential = "exp", "exponential-aft" = "exp",
    weibull = "weibull", "weibull-aft" = "weibull",
    gompertz = "gompertz", lognormal = "lnorm", loglogistic = "llogis",
    gamma = "gamma", gengamma = "gengamma",
    # Flexible baselines have no parametric STC analogue; approximate with
    # a Weibull G-computation.
    mspline = "weibull", pexp = "weibull",
    # No unnamed default: stc() validates the name first.
    stop("Unsupported survival distribution: ", distribution, call. = FALSE)
  )
}

#' Build comparator-population covariates from AgD means
#' @noRd
.stc_agd_mean_newdata <- function(agd, cov_names) {
  newdata <- data.frame(row.names = seq_len(nrow(agd)))
  for (cov in cov_names) {
    mean_col <- paste0(cov, "_mean")
    if (!mean_col %in% names(agd)) {
      stop(sprintf(
        paste0(
          "Cannot find mean for covariate '%s' in AgD. ",
          "Either add integration points or ensure AgD has '%s' column."
        ),
        cov, mean_col
      ), call. = FALSE)
    }
    newdata[[cov]] <- agd[[mean_col]]
  }
  newdata
}

#' Convert a time column, refusing a factor
#'
#' `as.numeric()` on a factor returns its level codes, which look like
#' plausible times and pass every later check.
#' @noRd
.reject_factor_time <- function(x, nm) {
  if (is.factor(x)) {
    stop("`", nm, "` is a factor; as.numeric() would use its level codes ",
         "rather than the times shown. Convert explicitly, e.g. ",
         "as.numeric(as.character(", nm, ")).", call. = FALSE)
  }
  as.numeric(x)
}


#' Parse survival outcome columns into mlumr's internal contract
#'
#' Maps either a [survival::Surv()] object or character column names to the
#' internal columns `.time`, `.start_time`, `.delay_time`, `.status` with status
#' codes `0` = right-censored, `1` = event, `2` = left-censored,
#' `3` = interval-censored.
#'
#' @param data Source data frame (used for the character-column route).
#' @param Surv An optional `survival::Surv()` object.
#' @param time,status,entry_time Character column names (character route). Only
#'   right-censoring (status `0`/`1`) and optional delayed entry are supported
#'   via this route; use a `Surv` object for left/interval censoring.
#' @return A data frame with `.time`, `.start_time`, `.delay_time`, `.status`.
#' @noRd
.get_surv_data <- function(data, Surv = NULL, time = NULL, status = NULL,
                           entry_time = NULL) {
  if (!is.null(Surv)) {
    # survival::is.Surv() is the one use that justifies the survival import.
    if (!survival::is.Surv(Surv)) {
      stop("`Surv` must be a survival::Surv() object", call. = FALSE)
    }
    sm <- unclass(Surv)
    type <- attr(Surv, "type")
    n <- nrow(sm)
    # A shorter Surv would be recycled across the data rows downstream.
    if (!is.null(data) && n != nrow(data)) {
      stop(sprintf(paste0("`Surv` object has %d row(s) but `data` has %d ",
                          "row(s); they must match exactly."),
                   n, nrow(data)), call. = FALSE)
    }
    out <- data.frame(
      .time = rep(NA_real_, n), .start_time = rep(0, n),
      .delay_time = rep(0, n), .status = rep(NA_integer_, n)
    )
    if (type == "right") {
      out$.time <- as.numeric(sm[, "time"])
      out$.status <- ifelse(sm[, "status"] == 1, 1L, 0L)
    } else if (type == "counting") {
      out$.delay_time <- as.numeric(sm[, "start"])
      out$.time <- as.numeric(sm[, "stop"])
      out$.status <- ifelse(sm[, "status"] == 1, 1L, 0L)
    } else if (type == "left") {
      out$.time <- as.numeric(sm[, "time"])
      out$.status <- ifelse(sm[, "status"] == 1, 1L, 2L)
    } else if (type %in% c("interval", "interval2")) {
      st <- as.integer(sm[, "status"])
      t1 <- as.numeric(sm[, "time1"])
      t2 <- as.numeric(sm[, "time2"])
      out$.status <- st
      out$.time <- ifelse(st == 3L, t2, t1)
      out$.start_time <- ifelse(st == 3L, t1, 0)
    } else {
      stop(sprintf("Unsupported Surv type: '%s'", type), call. = FALSE)
    }
    # A counting-type Surv already carries the entry time; for the other types
    # `entry_time` supplies `.delay_time`.
    if (!is.null(entry_time)) {
      if (type == "counting") {
        stop("Delayed entry is already encoded in the counting-type Surv() ",
             "`start` column; do not also pass `entry_time`.", call. = FALSE)
      }
      if (!entry_time %in% names(data)) {
        stop(sprintf("`entry_time` column '%s' not found in `data`", entry_time),
             call. = FALSE)
      }
      out$.delay_time <- .reject_factor_time(data[[entry_time]], entry_time)
    }
    return(out)
  }

  if (is.null(time) || is.null(status)) {
    stop("Provide either a `Surv` object or both `time` and `status` columns",
         call. = FALSE)
  }
  time_vals <- .reject_factor_time(data[[time]], time)
  status_raw <- data[[status]]
  # A factor with the single level "0" has level codes of 1, which the 0/1
  # check below would accept as events.
  if (is.factor(status_raw)) {
    stop("`", status, "` is a factor; as.numeric() would use its level codes ",
         "rather than the values shown, and a single-level factor would map ",
         "every row to 1. Convert explicitly, e.g. as.numeric(as.character(",
         status, ")).", call. = FALSE)
  }
  status_num <- if (is.logical(status_raw)) {
    as.integer(status_raw)
  } else {
    as.numeric(status_raw)
  }
  # The column route supports right-censoring only.
  non_na <- status_num[!is.na(status_num)]
  if (length(non_na) > 0L && !all(non_na %in% c(0, 1))) {
    stop("`status` must be 0/1 (or logical) for the column route. For left- or ",
         "interval-censoring or delayed entry, pass a survival::Surv() object.",
         call. = FALSE)
  }
  delay <- if (!is.null(entry_time)) {
    .reject_factor_time(data[[entry_time]], entry_time)
  } else {
    rep(0, length(time_vals))
  }
  data.frame(
    .time = time_vals,
    .start_time = rep(0, length(time_vals)),
    .delay_time = delay,
    .status = ifelse(status_num == 1, 1L, 0L),
    stringsAsFactors = FALSE
  )
}


#' Validate parsed survival times and status codes
#' @noRd
.validate_survival_times <- function(time, start_time, delay_time, status, label) {
  if (any(is.na(time)) || any(is.na(status))) {
    stop(sprintf("%s survival times/status must not contain NA", label),
         call. = FALSE)
  }
  if (any(!is.finite(time)) || any(time <= 0)) {
    stop(sprintf("%s survival times must be finite and strictly positive", label),
         call. = FALSE)
  }
  if (!all(status %in% c(0L, 1L, 2L, 3L))) {
    stop(sprintf("%s status must be coded 0 (right), 1 (event), 2 (left), 3 (interval)",
                 label), call. = FALSE)
  }
  if (any(!is.finite(delay_time)) || any(delay_time < 0)) {
    stop(sprintf("%s delayed-entry times must be finite and non-negative", label),
         call. = FALSE)
  }
  if (any(delay_time >= time)) {
    stop(sprintf("%s delayed-entry times must be earlier than event/censoring times",
                 label), call. = FALSE)
  }
  interval <- status == 3L
  if (any(interval) && any(start_time[interval] >= time[interval])) {
    stop(sprintf("%s interval lower bounds must be earlier than upper bounds",
                 label), call. = FALSE)
  }
  # Left-censoring with delayed entry is handled by the likelihood as interval
  # censoring on (delay, time]. An interval-censored lower bound cannot precede
  # the entry time.
  if (any(interval & start_time < delay_time)) {
    msg <- paste0(
      "%s has interval-censored observations (status 3) whose interval lower ",
      "bound precedes the delayed-entry time; the lower bound must be at or ",
      "after the entry time."
    )
    stop(sprintf(msg, label), call. = FALSE)
  }
  invisible(TRUE)
}


#' Set up survival IPD (internal; dispatched from [set_ipd()])
#' @noRd
.set_ipd_survival <- function(data, treatment, covariates, study,
                              Surv, time, status, entry_time) {
  .validate_non_empty_data(data, "IPD")
  .validate_required_covariates(covariates, "covariates")

  surv_cols <- if (is.null(Surv)) c(time, status, entry_time) else character(0)
  required_cols <- c(treatment, covariates, surv_cols, study)
  .check_required_columns(data, required_cols)
  .validate_reserved_internal_names(
    c(covariates, treatment, study, time, status, entry_time),
    c(".study", ".trt", ".time", ".start_time", ".delay_time", ".status"),
    "Column name(s)"
  )
  .validate_ipd_covariates(data, covariates)

  surv_df <- .get_surv_data(data, Surv = Surv, time = time, status = status,
                            entry_time = entry_time)

  ipd_data <- data.frame(
    .study = if (!is.null(study)) data[[study]] else "IPD_Study",
    .trt = data[[treatment]],
    stringsAsFactors = FALSE
  )
  ipd_data <- cbind(ipd_data, surv_df)
  for (cov in covariates) ipd_data[[cov]] <- data[[cov]]

  # Drop incomplete rows with a warning, as the non-survival path does.
  keep <- stats::complete.cases(ipd_data[, c(".study", ".trt", ".time",
                                             ".start_time", ".delay_time",
                                             ".status", covariates)])
  if (!all(keep)) {
    warning(sprintf("%d rows with missing values will be excluded", sum(!keep)),
            call. = FALSE)
    ipd_data <- ipd_data[keep, , drop = FALSE]
  }
  .validate_complete_rows_remain(ipd_data, "IPD")
  .validate_survival_times(ipd_data$.time, ipd_data$.start_time,
                           ipd_data$.delay_time, ipd_data$.status, "IPD")
  .validate_single_treatment(ipd_data, ".trt", "IPD")
  .warn_constant_ipd_covariates(ipd_data, covariates)

  out <- list(
    data = ipd_data,
    n = nrow(ipd_data),
    treatment = unique(ipd_data$.trt),
    covariates = covariates,
    family = "survival",
    type = "ipd",
    # Statuses 2 and 3 are left- and interval-censored failures: the event is
    # known to have occurred, only its time is not. Counting status 1 alone
    # reported n_events = 0 for a fully interval-censored arm.
    n_events = sum(ipd_data$.status != 0L)
  )
  class(out) <- c("mlumr_ipd", "list")
  out
}


#' Set up aggregate survival data (reconstructed pseudo-IPD)
#'
#' Prepare comparator aggregate survival data for an unanchored indirect
#' comparison. The comparator arm is supplied as **reconstructed pseudo-IPD**
#' (event/censoring times digitized from a published Kaplan-Meier curve, e.g.
#' via the Guyot algorithm) together with summary covariate moments
#' (means/SDs). The Stan model integrates the comparator likelihood over the
#' covariate distribution implied by those moments.
#'
#' @section Reconstruction uncertainty is not propagated:
#' The pseudo-individual records enter the likelihood as observed data, so
#' credible intervals are conditional on this one reconstruction and narrower
#' than the evidence supports. Treat the reconstruction as an analysis choice:
#' digitize the curve more than once or perturb the points within their
#' reading error, refit, and report the spread across refits beside the
#' within-fit interval.
#'
#' @param data Data frame of reconstructed pseudo-IPD (one row per
#'   pseudo-individual).
#' @param treatment Column name for the (single) comparator treatment.
#' @param Surv Optional [survival::Surv()] object describing the outcome. Use
#'   this for left/interval censoring or delayed entry.
#' @param time,status,entry_time Character column names as an alternative to
#'   `Surv` (right-censoring with status `0`/`1`, plus optional delayed entry).
#' @param cov_means Character vector of covariate mean/proportion column names
#'   (constant within each arm). Suffixes `_mean`/`_prop` are stripped to match
#'   the IPD covariate names.
#' @param cov_sds Character vector of covariate SD column names (`NA` for binary
#'   covariates). `NULL` treats all covariates as binary.
#' @param cov_types Character vector of `"continuous"`/`"binary"` per covariate.
#'   If `NULL`, inferred from the presence of an SD column.
#' @param study Optional study identifier column.
#' @param arm Optional arm identifier column. Only a single comparator arm is
#'   supported; if supplied, it must have one unique value.
#'   Multi-arm reconstructed survival comparators are rejected until a
#'   weighting estimand is implemented. Defaults to a single arm.
#'
#' @details
#' Under delayed entry the comparator likelihood conditions each integration
#' point on survival to its entry time and then averages, so `cov_means`,
#' `cov_sds` and the distributions [add_integration()] builds must describe
#' the population observed at entry (those in the risk set), not a baseline
#' population before selection. With varying entry times that population can
#' differ by entry time while the model has one distribution per arm; pooled
#' summaries are right only under a common entry time or when the covariate
#' distribution among those observed at entry is the same at every entry
#' time. Neither condition is checkable from the summaries supplied. Delayed
#' entry in the individual arm is unaffected.
#'
#' @return An object of class `mlumr_agd_surv` (also inheriting `mlumr_agd`).
#'   The internal column names cannot be used as column names in `data`.
#' @seealso [set_agd()] for non-survival aggregate data;
#'   `multinma::set_agd_surv()` is the ML-NMR equivalent.
#' @export
#'
#' @examples
#' \dontrun{
#' agd <- set_agd_surv(
#'   data = comparator_km,
#'   treatment = "trt",
#'   time = "time", status = "status",
#'   cov_means = c("age_mean", "male_prop"),
#'   cov_sds = c("age_sd", NA),
#'   cov_types = c("continuous", "binary")
#' )
#' }
set_agd_surv <- function(data, treatment, Surv = NULL,
                         time = NULL, status = NULL, entry_time = NULL,
                         cov_means, cov_sds = NULL, cov_types = NULL,
                         study = NULL, arm = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  .validate_non_empty_data(data, "AgD survival")
  .validate_required_covariates(cov_means, "cov_means")

  spec <- .agd_covariate_spec(cov_means, cov_sds, cov_types)
  cov_sds <- spec$cov_sds
  cov_types <- spec$cov_types
  cov_names <- spec$cov_names

  surv_cols <- if (is.null(Surv)) c(time, status, entry_time) else character(0)
  required_cols <- c(treatment, cov_means, cov_sds[!is.na(cov_sds)],
                     surv_cols, study, arm)
  .check_required_columns(data, required_cols)
  .validate_single_treatment(data, treatment, "AgD survival")
  .validate_reserved_internal_names(
    c(cov_means, cov_sds[!is.na(cov_sds)], treatment, study, arm,
      time, status, entry_time),
    c(".study", ".trt", ".arm", ".time", ".start_time", ".delay_time", ".status"),
    "Column name(s)"
  )
  .validate_agd_covariate_names(cov_means)
  .validate_agd_covariates(data, cov_means, cov_sds)
  .validate_agd_cov_types(cov_types)
  .validate_agd_binary_covariates(data, cov_means, cov_sds, cov_types)

  surv_df <- .get_surv_data(data, Surv = Surv, time = time, status = status,
                            entry_time = entry_time)
  .validate_survival_times(surv_df$.time, surv_df$.start_time,
                           surv_df$.delay_time, surv_df$.status, "AgD")

  arm_vec <- if (!is.null(arm)) {
    .require_identity(data[[arm]], arm)
  } else if (!is.null(study)) {
    .require_identity(data[[study]], study)
  } else {
    rep("AgD_Arm", nrow(data))
  }
  study_vec <- if (!is.null(study)) {
    .require_identity(data[[study]], study)
  } else {
    arm_vec
  }
  trt_vec <- .require_identity(data[[treatment]], treatment, as_char = FALSE)

  pseudo_ipd <- data.frame(
    .study = study_vec, .trt = trt_vec, .arm = arm_vec,
    .time = surv_df$.time, .start_time = surv_df$.start_time,
    .delay_time = surv_df$.delay_time, .status = surv_df$.status,
    stringsAsFactors = FALSE
  )

  arms <- unique(arm_vec)
  # An arm is one reconstructed curve from one study, and only one comparator
  # arm is supported: the generated quantities would otherwise average an
  # undefined equal-arm mixture.
  .require_single_identity(study_vec, arm_vec, "study")
  if (length(arms) > 1L) {
    stop("Multi-arm reconstructed survival comparators are not yet supported. ",
         "Supply a single comparator arm (one reconstructed Kaplan-Meier curve).",
         call. = FALSE)
  }
  arm_summary <- .build_arm_summary(data, arms, arm_vec, study_vec, trt_vec,
                                    cov_means, cov_sds, cov_names)
  cov_info <- .agd_cov_info(cov_names, cov_sds, cov_types)

  out <- list(
    data = arm_summary,
    pseudo_ipd = pseudo_ipd,
    treatment = unique(trt_vec),
    covariates = cov_names,
    cov_info = cov_info,
    family = "survival",
    type = "agd",
    n_arms = length(arms),
    n_pseudo = nrow(pseudo_ipd),
    n_events = sum(pseudo_ipd$.status != 0L)
  )
  class(out) <- c("mlumr_agd_surv", "mlumr_agd", "list")
  out
}


#' Refuse a missing grouping identifier
#'
#' A missing identifier matches no rows, so the arm summary would be all NA.
#' @param as_char Return `as.character(x)` rather than `x`.
#' @noRd
.require_identity <- function(x, nm, as_char = TRUE) {
  if (anyNA(x)) {
    stop("`", nm, "` must not contain missing values: it identifies which ",
         "rows belong together.", call. = FALSE)
  }
  if (as_char) as.character(x) else x
}


#' Refuse an arm that spans more than one study
#'
#' @param values The identifier to check within each arm.
#' @param arm_vec Arm labels.
#' @param label Name of the identifier, for the message.
#' @noRd
.require_single_identity <- function(values, arm_vec, label) {
  for (a in unique(arm_vec)) {
    vals <- unique(values[arm_vec == a])
    if (length(vals) > 1L) {
      stop(sprintf(paste0("Arm '%s' spans more than one %s (%s). An arm is one ",
                          "reconstructed curve from one study on one ",
                          "treatment; give each its own `arm` label."),
                   a, label, paste(sQuote(vals), collapse = ", ")),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}


#' Build the per-arm covariate-summary table for survival AgD
#' @noRd
.build_arm_summary <- function(data, arms, arm_vec, study_vec, trt_vec,
                               cov_means, cov_sds, cov_names) {
  rows <- lapply(arms, function(a) {
    idx <- which(arm_vec == a)
    row <- data.frame(
      .study = study_vec[idx[1]], .trt = trt_vec[idx[1]], .arm = a,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(cov_means)) {
      mean_vals <- data[[cov_means[[i]]]][idx]
      if (length(unique(mean_vals)) > 1L) {
        stop(sprintf("Covariate '%s' must be constant within arm '%s'",
                     cov_means[[i]], a), call. = FALSE)
      }
      row[[paste0(cov_names[[i]], "_mean")]] <- mean_vals[1]
      if (!is.na(cov_sds[[i]])) {
        sd_vals <- data[[cov_sds[[i]]]][idx]
        if (length(unique(sd_vals)) > 1L) {
          stop(sprintf("Covariate SD '%s' must be constant within arm '%s'",
                       cov_sds[[i]], a), call. = FALSE)
        }
        row[[paste0(cov_names[[i]], "_sd")]] <- sd_vals[1]
      }
    }
    row
  })
  do.call(rbind, rows)
}


#' Resolve survival distribution metadata
#'
#' @param distribution One of the survival distribution strings (default
#'   `"weibull"`).
#' @return A list with the distribution name, `kind`
#'   (`"parametric"`/`"flexible"`), integer `dist_code` (1-9 for parametric,
#'   `NA` for flexible), `mspline_degree`, `is_ph` (proportional hazards flag),
#'   `n_aux` (number of shape parameters), and the Stan model `stan_prefix`.
#' @noRd
.survival_distribution_info <- function(distribution = NULL) {
  distribution <- distribution %||% "weibull"
  valid <- c("exponential", "weibull", "gompertz", "exponential-aft",
             "weibull-aft", "lognormal", "loglogistic", "gamma", "gengamma",
             "mspline", "pexp")
  if (!is.character(distribution) || length(distribution) != 1L ||
        !(distribution %in% valid)) {
    stop(sprintf("`distribution` must be one of: %s",
                 paste(valid, collapse = ", ")), call. = FALSE)
  }
  flexible <- distribution %in% c("mspline", "pexp")
  dist_code <- switch(distribution,
    exponential = 1L, weibull = 2L, gompertz = 3L,
    "exponential-aft" = 4L, "weibull-aft" = 5L, lognormal = 6L,
    loglogistic = 7L, gamma = 8L, gengamma = 9L,
    NA_integer_
  )
  n_aux <- switch(distribution,
    exponential = 0L, "exponential-aft" = 0L, gengamma = 2L,
    mspline = 0L, pexp = 0L, 1L
  )
  list(
    distribution = distribution,
    kind = if (flexible) "flexible" else "parametric",
    dist_code = dist_code,
    mspline_degree = switch(distribution, mspline = 3L, pexp = 0L, NA_integer_),
    is_ph = distribution %in% c("exponential", "weibull", "gompertz",
                                "mspline", "pexp"),
    n_aux = n_aux,
    stan_prefix = if (flexible) "mlumr_survival_mspline" else "mlumr_survival"
  )
}


#' @method print mlumr_agd_surv
#' @export
print.mlumr_agd_surv <- function(x, ...) {
  cat("Aggregate survival data (reconstructed pseudo-IPD)\n")
  cat("==================================================\n")
  cat("Comparator treatment:", x$treatment, "\n")
  cat(sprintf("  Arms = %d | pseudo-individuals = %d | events = %d\n",
              x$n_arms, x$n_pseudo, x$n_events))
  cat("  Covariates:", paste(x$covariates, collapse = ", "), "\n")
  invisible(x)
}


#' Does the baseline shape differ between strata?
#'
#' `n_strata > 1` alone is not the question: the exponential has no shape
#' parameter to stratify, so `aux_by = ".study"` changes nothing there. This
#' mirrors the Stan gate `n_strata > 1 && nonexp && dist <= 3`; the flexible
#' models stratify their whole baseline.
#'
#' @param object An `mlumr_fit` (survival family).
#' @return `TRUE` when the strata have different baseline shapes.
#' @noRd
.aux_shapes_differ <- function(object) {
  n_strata <- object$stan_data$n_strata %||% 1L
  if (n_strata <= 1L) return(FALSE)
  info <- object$surv_info
  if (is.null(info)) return(FALSE)
  identical(info$kind, "flexible") || (info$n_aux %||% 0L) > 0L
}


#' Label and evaluation time for the scalar survival treatment effect
#'
#' `delta_*` is a marginal log hazard ratio under proportional hazards (at
#' `t -> 0` when the shapes are shared, otherwise at the first prediction
#' time), a log time ratio for a shared-shape SPFA AFT fit, and otherwise a
#' location contrast that is not a time ratio: with different shapes there is no
#' constant acceleration factor, and in a relaxed fit the covariate term does
#' not cancel. [marginal_effects()] and [prior_sensitivity()] both read this.
#'
#' @param object An `mlumr_fit` (survival family).
#' @param log_scale `TRUE` for the log-scale name (as stored in `delta_*`),
#'   `FALSE` for the natural-scale name [marginal_effects()] reports.
#' @return A list with `label` and `at_time` (`NA` when the measure has no
#'   evaluation time).
#' @noRd
.surv_scalar_label <- function(object, log_scale = FALSE) {
  is_ph <- isTRUE(object$surv_info$is_ph)
  differs <- .aux_shapes_differ(object)
  relaxed <- identical(object$model %||% "spfa", "relaxed")
  if (is_ph) {
    list(label = if (log_scale) "LOG_HR" else "HR",
         at_time = if (differs) object$pred_times[1] else 0)
  } else if (differs || relaxed) {
    list(label = if (log_scale) "DELTA_ETA" else "EXP_DELTA_ETA",
         at_time = NA_real_)
  } else {
    list(label = if (log_scale) "LOG_TR" else "TR", at_time = NA_real_)
  }
}

#' The one scalar `effect` name a survival fit can supply
#'
#' @param label The `label` from [.surv_scalar_label()] (natural scale).
#' @return One of `"hr"`, `"tr"`, `"exp_delta_eta"`.
#' @noRd
.surv_scalar_effect_name <- function(label) {
  switch(label, HR = "hr", TR = "tr", EXP_DELTA_ETA = "exp_delta_eta",
         stop("Unrecognized survival scalar label: ", label, call. = FALSE))
}

#' Message for an `effect` this survival fit cannot supply
#'
#' Says which scalar estimand the fit does have and why the requested one does
#' not exist for it.
#' @param effect The requested selector.
#' @param label,scalar_effect The fit's natural-scale label and its selector.
#' @param stratified `TRUE` when the baseline shapes differ by study.
#' @param valid_effects The accepted selectors for this fit.
#' @noRd
.surv_effect_scale_error <- function(effect, label, scalar_effect, stratified,
                                     valid_effects) {
  wrong_scalar <- effect %in% c("hr", "tr", "exp_delta_eta")
  if (!wrong_scalar) {
    return(sprintf("For survival family, `effect` must be one of: %s",
                   paste(valid_effects, collapse = ", ")))
  }
  if (identical(effect, "exp_delta_eta")) {
    return(paste0("`effect = \"exp_delta_eta\"` applies to an AFT fit whose ",
                  "shapes differ by study or to a relaxed AFT fit. This fit ",
                  "reports ", label, "; request `effect = \"", scalar_effect,
                  "\"`."))
  }
  why <- switch(
    label,
    HR = "a proportional-hazards fit has a marginal hazard ratio, not a time ratio",
    TR = paste0("a shared-shape SPFA AFT fit has a time ratio, and its hazard ",
                "ratio is not constant in time"),
    EXP_DELTA_ETA = if (stratified) {
      "each study has its own AFT shape, so there is no constant acceleration factor"
    } else {
      paste0("this is a relaxed fit, so the covariate term does not cancel and ",
             "the time ratio varies by covariate profile")
    }
  )
  paste0("`effect = \"", effect, "\"` is not available for this fit: ", why,
         ". Request `effect = \"", scalar_effect, "\"`, `effect = \"rmstd\"` ",
         "or \"rmstr\", or conditional_effects() for profile-specific effects.")
}

# Null-coalescing operator (available in base R >= 4.4.0, but we support >= 4.1.0)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Smallest strictly positive value the Stan models accept for an exposure or
# an aggregate standard error. Their data blocks declare `<lower=1e-12>`, so
# the R validators use the same number and reject it by column name.
.mlumr_min_positive <- 1e-12


#' Specify a marginal distribution
#'
#' Used to specify marginal distributions for covariates when adding integration
#' points. Wraps an inverse CDF (quantile) function with its parameters.
#'
#' @param qfun Inverse CDF function (e.g., `qnorm`, `qbern`)
#' @param ... Parameters of the distribution, can reference column names in AgD
#'
#' @return A list with class `"mlumr_distr"` containing the distribution specification
#' @export
#'
#' @examples
#' # Normal distribution
#' distr(qnorm, mean = 0, sd = 1)
#'
#' # Bernoulli distribution with probability 0.3
#' distr(qbern, prob = 0.3)
distr <- function(qfun, ...) {
  qfun_resolved <- match.fun(qfun)
  qfun_name <- tryCatch(deparse(substitute(qfun)), error = function(e) "user_function")
  # The bare name, so `distr(stats::qpois, ...)` still classifies as a count
  # margin in get_distribution_type().
  qfun_name <- sub("^.*:::?", "", qfun_name)

  # Capture arguments as unevaluated expressions
  args <- as.list(match.call(expand.dots = FALSE))[["..."]]
  # Name positional arguments now: everything downstream reads them by name.
  args <- .name_distr_args(args, qfun_resolved, qfun_name)

  if (!"p" %in% names(formals(qfun_resolved))) {
    stop("`qfun` should be an inverse CDF function with a formal argument `p`",
         call. = FALSE)
  }

  d <- list(
    qfun = qfun_resolved,
    args = args,
    # Where the specification was written, so its arguments can see the
    # variables in scope there as well as the aggregate row.
    envir = parent.frame(),
    qfun_name = qfun_name
  )

  class(d) <- "mlumr_distr"
  d
}

#' Evaluate a mlumr_distr object with data context
#'
#' @param d A `mlumr_distr` object
#' @param p Vector of probabilities
#' @param data A named list or data frame to evaluate expressions in
#' @return Numeric vector of quantiles
#' @noRd
eval_distr <- function(d, p, data = list()) {
  # By position: unnamed arguments are passed through in order.
  nms <- names(d$args)
  if (is.null(nms)) {
    nms <- rep("", length(d$args))
  }
  enc <- if (is.environment(d$envir)) d$envir else parent.frame(2)
  vals <- lapply(seq_along(d$args), function(i) {
    label <- if (nzchar(nms[[i]])) nms[[i]] else paste0("[[", i, "]]")
    tryCatch(
      eval(d$args[[i]], envir = data, enclos = enc),
      error = function(e) {
        stop(sprintf(
          "Error evaluating distribution argument '%s': %s\nAvailable data columns: %s",
          label, e$message, paste(names(data), collapse = ", ")
        ), call. = FALSE)
      }
    )
  })
  names(vals) <- nms
  do.call(d$qfun, c(list(p = p), vals))
}


#' Summarize a single draw vector into mean, sd and quantiles
#'
#' Internal helper. Centralizes the (mean, sd, quantile) triplet used by
#' [predict.mlumr_fit()], [marginal_effects()], [conditional_effects()] and
#' [conditional_predict()] so that any later change to the canonical posterior
#' summary only needs to happen in one place.
#'
#' @param x Numeric vector of posterior draws.
#' @param probs Quantile probabilities.
#' @param warn Whether to report dropped draws. Callers that summarize many
#'   vectors set this to `FALSE` and report once over the whole set instead.
#' @return Named numeric vector: `c(mean, sd, <named quantiles>)`.
#' @noRd
.summarize_draw_vector <- function(x, probs, warn = TRUE) {
  if (warn) {
    .warn_dropped_draws(x)
  }
  # Package names for the quantiles: R's `format()` names differ from the
  # names the lookups build for a non-round probability.
  c(mean = mean(x, na.rm = TRUE),
    sd   = stats::sd(x, na.rm = TRUE),
    stats::setNames(
      stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE),
      .quantile_names(probs)
    ))
}

#' Summarize a draws matrix column-wise into a tidy data frame
#'
#' Applies `.summarize_draw_vector()` across columns of `draws` and renames
#' the quantile columns to `qNN` form (e.g., `q2.5`, `q50`, `q97.5`).
#'
#' @param draws A numeric matrix or data frame of posterior draws (one column
#'   per quantity, one row per draw).
#' @param probs Quantile probabilities.
#' @param warn Whether to report dropped draws. Set `FALSE` where a missing
#'   draw is an expected outcome with a diagnostic of its own.
#' @return Data frame with columns `mean`, `sd` and one `qNN` column per
#'   element of `probs`.
#' @noRd
.summarize_draw_matrix <- function(draws, probs, warn = TRUE) {
  if (warn) {
    .warn_dropped_draws(draws)
  }
  summary_mat <- t(apply(draws, 2, .summarize_draw_vector, probs = probs,
                         warn = FALSE))
  summary_df <- as.data.frame(summary_mat)
  colnames(summary_df) <- c("mean", "sd", .quantile_names(probs))
  summary_df
}


#' Report posterior draws dropped from a summary
#'
#' `.summarize_draw_vector()` drops NA and NaN draws with `na.rm = TRUE`, so a
#' summary built on part of the chain would otherwise read like one built on
#' all of it. An infinite draw propagates into the mean and is not counted.
#'
#' @param draws Numeric vector, matrix or data frame of posterior draws.
#' @return `TRUE` if a warning was issued, `FALSE` otherwise, invisibly.
#' @noRd
.warn_dropped_draws <- function(draws) {
  m <- if (is.matrix(draws)) draws else as.matrix(draws)
  if (nrow(m) == 0L || ncol(m) == 0L) {
    return(invisible(FALSE))
  }
  dropped <- colSums(is.na(m))
  if (!any(dropped > 0L)) {
    return(invisible(FALSE))
  }
  n <- nrow(m)
  msg <- if (ncol(m) == 1L) {
    fmt <- paste0("%d of %d posterior draws are NA or NaN and were dropped ",
                  "from the summary, which therefore describes the remaining ",
                  "%d.")
    sprintf(fmt, dropped[[1]], n, n - dropped[[1]])
  } else {
    fmt <- paste0("%d of %d summarized quantities have NA or NaN draws, which ",
                  "were dropped from their summaries; the worst loses %d of ",
                  "%d draws. Those summaries describe the remaining draws ",
                  "only.")
    sprintf(fmt, sum(dropped > 0L), ncol(m), max(dropped), n)
  }
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

#' Validate an mlumr_data object
#' @noRd
.validate_mlumr_data_object <- function(data) {
  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data().", call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate a single TRUE or FALSE
#' @noRd
.validate_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  x
}


#' Convert a confidence level to a two-sided normal critical value
#' @noRd
.z_from_conf_level <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
        !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single finite number between 0 and 1.",
         call. = FALSE)
  }
  stats::qnorm(1 - (1 - conf_level) / 2)
}


#' Bound a Wald interval to a valid numerical range
#' @noRd
.bounded_wald_interval <- function(center, se, z,
                                   lower = -Inf, upper = Inf) {
  .validate_numeric_vector(center, "center")
  .validate_numeric_vector(se, "se")
  .validate_numeric_vector(z, "z")
  if (length(z) != 1L || !is.finite(z) || z < 0) {
    stop("`z` must be a single non-negative finite value.", call. = FALSE)
  }
  if (any(!is.finite(center)) || any(!is.finite(se)) || any(se < 0)) {
    stop("`center` and `se` must be finite, with non-negative `se`.",
         call. = FALSE)
  }
  if (!is.numeric(lower) || !is.numeric(upper) ||
        length(lower) != 1L || length(upper) != 1L ||
        is.na(lower) || is.na(upper) || lower > upper) {
    stop("`lower` and `upper` must define a valid interval.", call. = FALSE)
  }
  list(
    lower = pmax(lower, center - z * se),
    upper = pmin(upper, center + z * se)
  )
}


#' Truncate tiny negative variance estimates caused by numerical noise
#' @noRd
.nonnegative_variance <- function(x, name = "variance", tol = 1e-10) {
  .validate_numeric_vector(x, name)
  if (any(!is.finite(x))) {
    stop(sprintf("`%s` must be finite.", name), call. = FALSE)
  }
  if (any(x < -tol)) {
    stop(sprintf("`%s` must be non-negative.", name), call. = FALSE)
  }
  pmax(x, 0)
}


#' Square root of a variance estimate with numerical guarding
#' @noRd
.sqrt_variance <- function(x, name = "variance", tol = 1e-10) {
  sqrt(.nonnegative_variance(x, name, tol))
}


# -----------------------------------------------------------------------------
# Bernoulli wrappers (qbern / pbern / dbern)
#
# One-line wrappers around stats::qbinom/pbinom/dbinom with size = 1, as in
# multinma (Phillippo et al., GPL-3, R/integration.R), so `qbern` works in
# `distr()` without multinma attached.
# -----------------------------------------------------------------------------

#' Bernoulli quantile function
#'
#' @param p Vector of probabilities
#' @param prob Success probability
#' @param lower.tail Logical; if TRUE, probabilities are P(X <= x)
#' @param log.p Logical; if TRUE, probabilities are given as log(p)
#'
#' @return Integer vector of 0s and 1s
#' @export
qbern <- function(p, prob, lower.tail = TRUE, log.p = FALSE) {
  qbinom(p, size = 1, prob = prob, lower.tail = lower.tail, log.p = log.p)
}

#' Bernoulli CDF
#'
#' @param q Vector of quantiles
#' @param prob Success probability
#' @param lower.tail Logical
#' @param log.p Logical
#'
#' @return Numeric vector
#' @export
pbern <- function(q, prob, lower.tail = TRUE, log.p = FALSE) {
  pbinom(q, size = 1, prob = prob, lower.tail = lower.tail, log.p = log.p)
}

#' Bernoulli PMF
#'
#' @param x Vector of values
#' @param prob Success probability
#' @param log Logical; if TRUE, return log-density
#'
#' @return Numeric vector
#' @export
dbern <- function(x, prob, log = FALSE) {
  dbinom(x, size = 1, prob = prob, log = log)
}

#' Get distribution type (continuous, discrete, or binary)
#' @param ... distr() objects
#' @param data Sample data for evaluation
#' @return Named character vector
#' @noRd
get_distribution_type <- function(..., data = list()) {
  ds <- list(...)
  dnames <- names(ds)

  out <- vector("character", length = length(ds))
  names(out) <- dnames

  # A concentrated logit-normal evaluates to 1 on the probe grid below, so it
  # has to be listed rather than probed.
  known_continuous <- c("qbeta", "qcauchy", "qchisq", "qexp", "qf", "qgamma",
                        "qlnorm", "qlogitnorm", "qnorm", "qt", "qunif",
                        "qweibull")
  known_discrete <- c("qgeom", "qnbinom", "qpois")
  known_binary <- "qbern"

  for (i in seq_along(ds)) {
    di <- ds[[i]]
    if (di$qfun_name %in% known_continuous) {
      out[i] <- "continuous"
    } else if (di$qfun_name %in% known_discrete) {
      out[i] <- "discrete"
    } else if (di$qfun_name %in% known_binary) {
      out[i] <- "binary"
    } else if (di$qfun_name == "qbinom") {
      # Evaluated in the specification's own scope, like every other argument.
      size_val <- eval_distr_arg(di$args$size, data, di$envir)
      out[i] <- if (all(size_val == 1)) "binary" else "discrete"
    } else {
      # Test distribution on a grid
      ps <- 1:99 / 100
      support <- eval_distr(di, ps, data)
      is_int <- all(abs(support - round(support)) < .Machine$double.eps^0.5,
                    na.rm = TRUE)
      if (is_int) {
        out[i] <- if (all(abs(support) < 1.5, na.rm = TRUE)) "binary" else "discrete"
      } else {
        out[i] <- "continuous"
      }
    }
  }

  out
}

#' Evaluate a single mlumr_distr argument expression
#' @param expr An unevaluated expression
#' @param data Data context
#' @param enclos The environment the specification was written in, from
#'   `distr()`; anything else falls back to the caller's frame, as before.
#' @return Evaluated value
#' @noRd
eval_distr_arg <- function(expr, data, enclos = NULL) {
  if (!is.environment(enclos)) enclos <- parent.frame(2)
  eval(expr, envir = data, enclos = enclos)
}

#' Convert Spearman correlations to Gaussian copula correlations
#'
#' Applies theoretical relationships between Spearman's rho and the
#' Gaussian copula parameter (Kurowicka & Cooke, 2006; Lebrun & Dutfoy, 2009):
#'   - Continuous-continuous: rho_copula = 2 * sin(pi * rho_S / 6) (exact)
#'   - Binary-binary: rho_copula = sin(pi * rho_S / 2) (heuristic; the exact
#'     relationship depends on marginal prevalences, not accounted for here)
#'   - Continuous-binary: rho_copula = sqrt(2) * sin(pi * rho_S / (2*sqrt(3)))
#'     (heuristic)
#'
#' @param X Correlation matrix (Spearman)
#' @param types Character vector of distribution types
#' @return Adjusted correlation matrix for Gaussian copula
#' @noRd
cor_adjust_spearman <- function(X, types) {
  if (length(types) != nrow(X)) {
    stop("`types` length must match correlation matrix dimensions", call. = FALSE)
  }
  bin <- types == "binary"
  cont <- !bin

  X[cont, cont] <- 2 * sin(pi * X[cont, cont] / 6)
  X[bin, bin] <- sin(pi * X[bin, bin] / 2)
  # The continuous-binary heuristic can map strong input correlations to a
  # magnitude > 1 (|rho_S| > sqrt(3)/2); clamp to keep a valid correlation
  # entry before the positive-definite projection.
  X[cont, bin] <- .clamp_cor(sqrt(2) * sin(pi * X[cont, bin] / (2 * sqrt(3))))
  X[bin, cont] <- .clamp_cor(sqrt(2) * sin(pi * X[bin, cont] / (2 * sqrt(3))))

  diag(X) <- 1
  X
}

#' Clamp correlation entries to a valid open interval
#' @noRd
.clamp_cor <- function(x) {
  pmin(pmax(x, -0.999), 0.999)
}

#' Convert Pearson correlations to Gaussian copula correlations
#'
#' For continuous-continuous pairs, Pearson rho equals the Gaussian copula
#' parameter only under normality of both margins. For non-normal continuous
#' covariates, this no-adjustment assumption introduces approximation error.
#' For binary and mixed pairs:
#'   - Binary-binary: rho_copula = sin(pi * rho_P / 2)
#'   - Continuous-binary: rho_copula = sqrt(pi/2) * rho_P
#'
#' @param X Correlation matrix (Pearson)
#' @param types Character vector of distribution types
#' @return Adjusted correlation matrix for Gaussian copula
#' @noRd
cor_adjust_pearson <- function(X, types) {
  if (length(types) != nrow(X)) {
    stop("`types` length must match correlation matrix dimensions", call. = FALSE)
  }
  bin <- types == "binary"
  cont <- !bin

  X[bin, bin] <- sin(pi * X[bin, bin] / 2)
  # Continuous-binary heuristic can exceed |1| for |rho_P| > 1/sqrt(pi/2);
  # clamp to a valid correlation entry before the positive-definite projection.
  X[cont, bin] <- .clamp_cor(sqrt(pi / 2) * X[cont, bin])
  X[bin, cont] <- .clamp_cor(sqrt(pi / 2) * X[bin, cont])

  diag(X) <- 1
  X
}


#' Give distribution arguments the names R would match them to
#'
#' `distr()` stores its arguments unevaluated and everything downstream reads
#' them by name, so positional and abbreviated arguments are matched to their
#' formals once here, the way R would match them at call time.
#'
#' @param args The captured `...`, possibly partly named or abbreviated.
#' @param qfun The resolved quantile function.
#' @param qfun_name Its name, for error messages.
#' @return `args` with every element named in full.
#' @noRd
.name_distr_args <- function(args, qfun, qfun_name = "qfun") {
  if (!length(args)) {
    return(args)
  }
  nms <- names(args)
  if (is.null(nms)) {
    nms <- rep("", length(args))
  }
  # `p` is supplied by the evaluator. R matches unnamed and abbreviated
  # arguments only against the formals before `...`.
  formal_names <- names(formals(qfun))
  dots <- match("...", formal_names)
  if (!is.na(dots)) {
    formal_names <- formal_names[seq_len(dots - 1L)]
  }
  formal_names <- setdiff(formal_names, "p")
  # R's order: exact names first, then unique partial matches against the
  # formals no exact name took.
  named_idx <- which(nzchar(nms))
  exact <- intersect(nms[named_idx], formal_names)
  remaining <- setdiff(formal_names, exact)
  claimed <- list()
  for (i in named_idx) {
    nm <- nms[i]
    if (nm %in% exact) {
      next
    }
    hits <- remaining[startsWith(remaining, nm)]
    # R refuses an ambiguous abbreviation before binding anything positional.
    if (length(hits) > 1L) {
      stop(sprintf(paste0("`distr()` received argument `%s`, which matches more ",
                          "than one parameter of `%s` (%s). Name it in full."),
                   nm, qfun_name, paste(hits, collapse = ", ")), call. = FALSE)
    }
    if (length(hits) == 1L) {
      # R also refuses two abbreviations of the same formal.
      if (hits %in% names(claimed)) {
        stop(sprintf(paste0("`distr()` received arguments `%s` and `%s`, which ",
                            "both abbreviate parameter `%s` of `%s`."),
                     claimed[[hits]], nm, hits, qfun_name), call. = FALSE)
      }
      claimed[[hits]] <- nm
      nms[i] <- hits
    }
  }
  remaining <- setdiff(remaining, names(claimed))
  unnamed <- which(!nzchar(nms))
  if (length(unnamed)) {
    available <- remaining
    n_match <- min(length(unnamed), length(available))
    if (n_match) {
      nms[unnamed[seq_len(n_match)]] <- available[seq_len(n_match)]
    }
    # Anything left over belongs in `...`, if the function has one.
    leftover <- length(unnamed) - n_match
    if (leftover > 0L && !("..." %in% names(formals(qfun)))) {
      fmt <- paste0("`distr()` received %d unnamed argument(s) for `%s`, which ",
                    "has no remaining parameter to match them to and no `...`. ",
                    "Name them explicitly or drop them.")
      stop(sprintf(fmt, leftover, qfun_name), call. = FALSE)
    }
  }
  names(args) <- nms
  args
}

#' Coerce a validated count to integer without truncating
#'
#' The count validators accept values within `sqrt(.Machine$double.eps)` of a
#' whole number, and `as.integer()` truncates, so round first.
#'
#' @param x A numeric vector that has passed a whole-number count check.
#' @return An integer vector.
#' @noRd
.as_count_integer <- function(x) {
  as.integer(round(x))
}

