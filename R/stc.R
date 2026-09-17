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
