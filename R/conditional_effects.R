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
#' The conditional link-scale effect is computed directly as
#' `eta_index - eta_comparator`. This avoids numerical distortion from
#' transforming extreme response-scale probabilities back through the link
#' function.
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

  profiles <- .conditional_profiles(object, newdata)
  X <- profiles$X
  n_profiles <- nrow(X)
  params <- .conditional_parameters(object, profiles$covariates)
  results <- vector("list", n_profiles)

  for (i in seq_len(n_profiles)) {
    eta <- .conditional_eta(params, X[i, , drop = FALSE])
    eta_idx <- eta$index
    eta_cmp <- eta$comparator

    if (family == "binomial") {
      p_idx <- inverse_link(eta_idx, lnk)
      p_cmp <- inverse_link(eta_cmp, lnk)
      profile_draws <- data.frame(
        link_effect = eta_idx - eta_cmp,
        rd = p_idx - p_cmp,
        rr = .safe_positive_ratio(p_idx, p_cmp)
      )
    } else if (family == "normal") {
      if (lnk == "identity") {
        profile_draws <- data.frame(
          md = eta_idx - eta_cmp
        )
      } else {
        val_idx <- inverse_link(eta_idx, lnk)
        val_cmp <- inverse_link(eta_cmp, lnk)
        profile_draws <- data.frame(
          md = val_idx - val_cmp
        )
      }
    } else {
      profile_draws <- data.frame(
        rr = exp(eta_idx - eta_cmp)
      )
    }

    results[[i]] <- profile_draws
  }

  # Filter to requested effects
  if (family == "binomial") {
    keep_cols <- if (effect == "all") c("link_effect", "rd", "rr") else effect
  } else if (family == "normal") {
    keep_cols <- "md"
  } else {
    keep_cols <- "rr"
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
                 paste0("q", probs * 100)), drop = FALSE]
  rownames(out) <- NULL
  out
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
  } else {
    c("all", "rr")
  }
}

#' Ratio with the same positive-denominator guard used by Stan generated quantities
#' @keywords internal
.safe_positive_ratio <- function(num, denom) {
  num / pmax(denom, 1e-10)
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
#'   the fitted linear-predictor scale.
#' @param summary Return summary (`TRUE`) or full draws (`FALSE`)
#' @param probs Quantiles for summary
#'
#' @return A data frame with predictions for each treatment at each profile
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
                                type = c("response", "link"),
                                summary = TRUE,
                                probs = c(0.025, 0.5, 0.975)) {

  .validate_mlumr_fit_object(object)
  type <- .validate_predict_choice(type, c("response", "link"), "type")
  summary <- .validate_summary_flag(summary)
  .validate_probs(probs)

  profiles <- .conditional_profiles(object, newdata)
  X <- profiles$X
  n_profiles <- nrow(X)
  params <- .conditional_parameters(object, profiles$covariates)
  family <- object$family %||% "binomial"
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
      qcols <- paste0("q", probs * 100)

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
        pname <- paste0(probs[j] * 100, "%")
        results[[i]][[qname]] <- c(s_idx[pname], s_cmp[pname])
      }
    }
  }

  out <- do.call(rbind, results)
  rownames(out) <- NULL
  out
}


#' Log survival S(t | eta) in R, mirroring the Stan log_surv_scalar()
#'
#' Vectorized over posterior draws (`eta`, `aux`, `aux2` are vectors; `t` is a
#' scalar time).
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
  w <- exp(log_w)
  stats::pgamma(w, shape = rep_len(aux2, length(w)),
                lower.tail = FALSE, log.p = TRUE)
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
