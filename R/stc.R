#' Simulated treatment comparison via G-computation
#'
#' Perform an unanchored simulated treatment comparison (STC) with marginal
#' standardization. Fits an outcome regression to the single IPD treatment arm,
#' standardizes its predictions over the comparator-population covariate
#' distribution, and contrasts that outcome with the reported comparator-arm
#' outcome in the same population. It does not standardize either treatment to
#' the index population.
#'
#' For binomial outcomes, returns the treatment effect on the link scale plus
#' event probabilities, risk difference, and log risk ratio with SEs and CIs
#' in the comparator population. The observed comparator proportion gets the
#' exact Clopper-Pearson interval, as in [naive()]. The standardized index
#' probability is a model prediction, and its interval is the delta-method
#' Wald interval bounded to `[0, 1]`: an asymptotic approximation whose
#' calibration has not been studied here, as are the intervals of the
#' contrasts. When the observed comparator arm has zero or all events,
#' transformed effect measures use the boundary-only pseudo-count
#' `(r + 0.5) / (n + 1)`; model predictions are never corrected. For Poisson
#' outcomes, the comparator log rate uses a 0.5 continuity correction when the
#' observed event count is zero.
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
#'   used for the package-specific survival G-computation (default
#'   `"weibull"`). Requires the `flexsurv`
#'   package. The STC estimand is the restricted-mean-survival-time difference.
#'   The flexible baselines `"mspline"` and `"pexp"` have no parametric
#'   `flexsurv` analogue: requesting either fits a Weibull G-computation as an
#'   approximate benchmark, emits a warning, and records
#'   `distribution_fit = "weibull"` and `approximated = TRUE` in the result
#'   (`$distribution` keeps the requested value). Survival STC currently
#'   supports right-censored data without delayed entry; use [mlumr()] for
#'   left-censored, interval-censored, or delayed-entry survival data.
#' @param n_boot For `family = "survival"` only: number of nonparametric
#'   bootstrap resamples used for the RMST-difference standard error (default
#'   `200`). Set `n_boot = 0` for a fast point estimate with no interval
#'   (`se`/CI returned as `NA`). Must be 0 or at least 2, since a single
#'   resample has no standard error; several hundred are needed before the
#'   interval is usable, so treat anything below the default as exploratory.
#'   Ignored for other families, which use the delta method.
#' @param seed For `family = "survival"` only: optional integer seed for the
#'   bootstrap, making the standard error reproducible. The global random
#'   number stream is restored on exit. Ignored for other families.
#' @param rmst_horizon For `family = "survival"` only: the restriction time the
#'   RMST difference is integrated to. Defaults to the largest observed time
#'   across both arms. RMST at a different horizon is a different estimand, so
#'   set this explicitly whenever the result is to be compared with an
#'   [mlumr()] fit, whose own default can be the follow-up both studies
#'   observed rather than the pooled maximum; read that fit's horizon from the
#'   `horizon` column of `predict(type = "rmst")`. A value beyond the observed
#'   range extrapolates the fitted parametric survival function and warns.
#'   Ignored for other families.
#'
#' @return An object of class `mlumr_stc`. Its `separation` component records
#'   the outcome of the binomial separation check, and only that: `status` is
#'   `"not_separated"` when the exact check ran on a binomial outcome model
#'   and found no separation, `"unknown"` when it could not run (only the
#'   fitted-value screen was applied, which cannot see quasi-complete
#'   separation), and `"not_applicable"` when the outcome model is not a
#'   binomial GLM, so this particular test has nothing to say. A Poisson
#'   outcome model has a boundary of its own kind: with no events, or with a
#'   subgroup without events that a direction of the coefficients can send
#'   to a rate of zero while every other row's rate stays fixed, the
#'   likelihood rises without bound and the maximum likelihood estimate is
#'   not finite, although the fitting reports convergence with finite
#'   numbers. Such a fit is refused, as is one where the question could not
#'   be decided, so a returned Poisson result has a finite maximum and its
#'   `reason` says so. A separated fit is refused rather than returned, so
#'   `"separated"` never appears here.
#' @importFrom stats gaussian poisson dnorm
#' @export
#'
#' @details
#' The STC procedure is:
#' 1. Fit a GLM on IPD (binomial/gaussian/poisson as appropriate).
#' 2. Predict on comparator-population covariates (from integration points or
#'    AgD covariate means for the identity-link normal special case).
#' 3. Marginalize predictions over the comparator population.
#' 4. Contrast with the reported comparator outcome in that population.
#' 5. Compute first-order, fixed-integration-grid delta-method standard errors.
#'
#' The response-scale standardization follows the marginalization order used by
#' Ren et al.'s unanchored STC and by parametric G-computation: predict each
#' target profile, average the natural-scale outcomes, then transform that
#' average. This is a one-arm standardization benchmark: only the index-treatment
#' outcome model is fitted because comparator IPD are unavailable. Remiro-Azocar
#' et al. implement two-arm G-computation, where both potential outcomes are
#' predicted from an IPD study; that is a different data design even though the
#' response-scale marginalization step is shared.
#'
#' The non-survival standard error is conditional on the supplied integration
#' grid and reported comparator covariate summaries. It propagates fitted
#' regression-coefficient uncertainty and observed comparator-outcome
#' uncertainty, but not uncertainty from reconstructing the comparator
#' covariate distribution. Ren et al. instead resample the IPD, reconstruct the
#' target distribution, and use a nonparametric bootstrap. Use the present
#' delta-method result as a fast benchmark and use sensitivity analyses when
#' reconstruction uncertainty may matter.
#'
#' The estimator relies on correct specification of the index-treatment outcome
#' model and its applicability to the comparator population. It does not model
#' posterior uncertainty in population covariate distributions or relax
#' treatment-specific covariate effects.
#' When clinically meaningful effect modification is plausible, prefer
#' `mlumr(..., model = "relaxed")` as the primary analysis and use STC as a
#' sensitivity or benchmarking analysis.
#'
#' The returned effect is defined in the comparator population. Applying that
#' same effect to the index or another decision population is a separate
#' effect-equality assumption. `stc()` does not standardize to, perform, or
#' validate transport to the index population. This differs from two-arm
#' parametric G-computation, which fits treatment-specific outcome regressions
#' and can standardize both potential outcomes to a chosen target population.
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
    # A single resample gives sd() = NA, which is indistinguishable downstream
    # from "every resample failed" and was reported as such. Either the
    # bootstrap is off (0) or it has enough replicates to have a variance.
    if (n_boot == 1L) {
      stop("`n_boot` must be 0 (no bootstrap) or at least 2: the standard ",
           "error of a single resample is undefined. Several hundred ",
           "resamples are needed for a usable interval; the default is 200.",
           call. = FALSE)
    }
    if (!is.null(seed)) {
      .validate_mlumr_integer(seed, "seed", lower = 0L)
    }
    out <- .stc_survival(data, conf_level, z, distribution,
                         n_boot = as.integer(n_boot), seed = seed,
                         rmst_horizon = rmst_horizon)
    # Survival STC fits a parametric survival model, not a binomial GLM, so the
    # separation question does not arise. Say so rather than leave the field
    # absent, so a caller can read it without knowing the family first.
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
#' @keywords internal
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
#' @keywords internal
.stc_glm_parameters <- function(fit) {
  if (!isTRUE(fit$converged)) {
    stop("STC GLM did not converge; check the IPD model or use mlumr().",
         call. = FALSE)
  }
  # Whether the likelihood has a finite maximum is decided before the
  # coefficients and covariance are read: a fit with none can stop at
  # finite numbers, and on a platform where it stops at non-finite ones
  # the message should still name the cause rather than the symptom.
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


#' Refuse a Poisson fit whose likelihood has no finite maximum
#'
#' The Poisson log-likelihood is `sum(y_i eta_i - E_i exp(eta_i))` up to a
#' constant. Along a direction `d` of the coefficients that leaves every
#' positive-count row's predictor fixed, `X_i d = 0`, and raises none of
#' the zero-count rows', `X_i d <= 0` with some strictly below, the linear
#' term is constant and the exponential terms fall, so the likelihood rises
#' all the way out and has no maximum to reach. Iterative reweighting stops
#' anyway, when the deviance stops changing: 80 zero counts on a nonconstant
#' covariate stop after 25 iterations at an intercept near -27.3 with a
#' standard error near 57,500, and 40 zeros beside 40 positive counts on a
#' binary covariate stop at a slope near 21.2. Every number there describes
#' where the iteration stopped, not the data, and the fitted-value screen
#' for the binomial case does not apply: a rate can legitimately be small.
#'
#' A zero total count is the plain case and is refused outright. Otherwise
#' the question is the linear feasibility one [.zero_boundary()] decides,
#' in its weak form: positive rows spanning the design leave no such
#' direction, which is where ordinary data land; a direction found is a
#' refusal; and a question it cannot decide (more than two free directions,
#' or zero rows within rounding of the positive rows' span) is refused too,
#' since a possibly infinite estimate is not one to report.
#'
#' Some functionals can remain estimable when the coefficients are not,
#' but estimating them needs a method built for that boundary, and the
#' ordinary Wald machinery here is not it.
#'
#' @param fit A fitted Poisson `glm`.
#' @return The status list recorded on the result, invisibly: `status`
#'   `"not_applicable"` for the binomial separation test, with a `reason`
#'   that records the finite-maximum check ran and passed.
#' @keywords internal
.stc_refuse_poisson_recession <- function(fit) {
  y <- fit$y
  X <- stats::model.matrix(fit)
  if (all(y == 0)) {
    stop(
      paste(
        "The STC outcome model has no events: the Poisson likelihood rises",
        "without bound as the log rate falls, so the maximum likelihood",
        "estimate is not finite, and the coefficients, the interval and the",
        "standardized rate would describe where the fitting stopped rather",
        "than the data. Use mlumr(), whose prior makes the posterior proper."
      ),
      call. = FALSE
    )
  }
  pos <- y > 0
  Xs <- .scale_design(X)
  reach <- .zero_boundary(Xs[pos, , drop = FALSE], Xs[!pos, , drop = FALSE],
                          X[pos, , drop = FALSE], X[!pos, , drop = FALSE],
                          strict = FALSE)
  if (identical(reach, "reachable")) {
    stop(
      paste(
        "The STC outcome model has no finite maximum likelihood estimate: a",
        "direction of the coefficients leaves the rate of every row with",
        "events fixed while lowering the rate of rows without, so the",
        "likelihood rises along it without bound, even though the fitting",
        "reported convergence and every returned number is finite. A",
        "subgroup with no events is the usual cause. Use mlumr(), whose",
        "prior makes the posterior proper."
      ),
      call. = FALSE
    )
  }
  if (!identical(reach, "unreachable")) {
    stop(
      paste(
        "Whether the STC outcome model has a finite maximum likelihood",
        "estimate could not be decided: the rows without events could",
        "load on more than two free directions of the coefficients, lie",
        "within rounding of the span of the rows with events, or point",
        "opposite ways to within rounding, and this check does not attempt",
        "those cases. A possibly infinite estimate is not reported as an",
        "ordinary one. Use mlumr(), whose prior makes the posterior",
        "proper."
      ),
      call. = FALSE
    )
  }
  invisible(list(
    status = "not_applicable",
    reason = paste("the outcome model is Poisson, so the binomial separation",
                   "test does not apply; its likelihood was checked for a",
                   "direction along which it rises without bound and has",
                   "none, so the maximum likelihood estimate is finite")
  ))
}

#' Refuse a fit whose likelihood has no finite maximum
#'
#' The checks above look for a failure the fitting reports, and separation is
#' not one. Iterative reweighting stops when the deviance stops changing, and a
#' separated fit has no maximum for it to stop at: the deviance falls to about
#' 6e-10 while the intercept is still drifting, so the criterion fires anyway
#' and a binomial arm with no events comes back with `converged = TRUE`, finite
#' coefficients and a finite covariance. 100 rows with the outcome always zero
#' produce a coefficient of about -26.6 and a largest fitted probability of
#' 3e-12, with a confidence interval to match. Raising `maxit` changes none of
#' those numbers, which is what shows the iteration limit is not what stopped
#' it. Every number there is a property of where the iteration stopped, not of
#' the data, and reporting it as an estimate is worse than reporting nothing,
#' because nothing about it looks wrong.
#'
#' The symptom is the one thing separation always leaves: every fitted
#' probability pinned against 0 or 1, whether they all sit at one boundary
#' (an arm with no events) or split between the two (a covariate that
#' separates the outcome). A rate can legitimately be small, so the test is on
#' the boundary rather than on smallness, and it applies only where a boundary
#' exists.
#'
#' The fitted values cannot catch quasi-complete separation, where rows sit on
#' the separating hyperplane: `y = c(0, 0, 1, 1)` on `x = c(-1, 0, 0, 1)` has
#' no finite slope, yet the two tied rows keep fitted probabilities of exactly
#' 0.5, so not every probability has reached a boundary. Telling that apart
#' from a strong but identified fit takes more than the fitted values, since a
#' legitimate signal here reaches a linear predictor of 20.1 while this case
#' reaches 19.6. The exact test is a linear program, so it lives behind
#' [.stc_separation_status()] and runs only when the optional
#' \pkg{detectseparation} package is installed. The threshold test stays as
#' the part that always runs.
#' @param fit A fitted `glm`.
#' @return The separation status, invisibly: a list with `status`, one of
#'   `"not_separated"`, `"unknown"` or `"not_applicable"`, and `reason` for
#'   the latter two. `"separated"` is never returned, since it throws.
#'   `"not_applicable"` means this binomial separation test does not apply to
#'   the fitted family, not that the family has no finite-maximum problem of
#'   its own. Callers record it on the result so a verified estimate can be
#'   told apart from an unverified one after the warning has scrolled away.
#' @keywords internal
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
  # Every fitted probability at *a* boundary, not all at the same one. A
  # covariate that perfectly separates the outcome sends its two groups to
  # opposite boundaries, which is the ordinary presentation of separation and
  # the one an arm-level test misses: `y ~ x` with the two equal gives fitted
  # probabilities of 2e-11 and 1, `converged = TRUE`, and a slope of 49.
  if (all(mu < eps | mu > 1 - eps)) {
    stop(
      paste(
        "The STC outcome model is separated: every fitted probability sits at",
        "0 or 1, which happens when an arm has no events or no non-events.",
        "The likelihood has no finite maximum there, so the coefficients and",
        "the interval would describe where the fitting stopped rather than",
        "the data. Use mlumr(), whose prior makes the posterior proper."
      ),
      call. = FALSE
    )
  }
  exact <- .stc_separation_status(fit)
  if (identical(exact$status, "unknown")) {
    # Not a refusal: the estimate is still returned. But an unchecked fit must
    # not be handed back looking like a checked one, and the screen that DID
    # run cannot see the quasi-complete case at all.
    warning(
      paste0(
        "The exact separation check did not run for the STC outcome model, ",
        "because ", exact$reason, ". Only the fitted-value screen was ",
        "applied, and it cannot detect quasi-complete separation: rows on the ",
        "separating hyperplane keep fitted probabilities away from 0 and 1, ",
        "so a fit whose maximum likelihood estimate is infinite can pass it ",
        "with converged = TRUE and finite coefficients. Treat this estimate ",
        "and its interval as unverified. Install detectseparation to run the ",
        "check, or use mlumr(), whose prior makes the posterior proper."
      ),
      call. = FALSE
    )
  }
  if (identical(exact$status, "separated")) {
    stop(
      paste(
        "The STC outcome model is separated: a linear combination of the",
        "covariates separates the outcome, so the maximum likelihood estimate",
        "is infinite even though the fitting reported convergence and every",
        "returned number is finite. This is the quasi-complete case, where",
        "rows on the separating hyperplane keep fitted probabilities away",
        "from 0 and 1, so it cannot be seen in the fitted values. Use",
        "mlumr(), whose prior makes the posterior proper."
      ),
      call. = FALSE
    )
  }
  invisible(exact)
}


#' Exact separation test, when the optional dependency is present
#'
#' Whether a binomial likelihood has a finite maximum is a linear-programming
#' question, not a threshold one: the fit is separated exactly when some linear
#' combination of the covariates perfectly orders the outcome, and a fit that
#' is merely strong can look identical in the coefficients and the fitted
#' values. \pkg{detectseparation} solves that program. It is in Suggests, so
#' when it is absent this returns the `"unknown"` status with the reason, and
#' the caller keeps the fitted-value test as its only screen; that is a weaker
#' guarantee, not a wrong one.
#'
#' An error here is reported as "unknown" rather than as "separated": a refit
#' can fail for reasons that have nothing to do with separation, and turning
#' those into a refusal would reject estimable models. A warning is not an
#' error, and must not be read as one here, because the fit this check exists
#' to catch is the one that warns.
#'
#' The result is a STATUS and not a logical, because `NA` was being read as
#' permission to continue. The caller stopped on `isTRUE()`, so every way of
#' not knowing, an absent dependency most of all, took the same path as a fit
#' that had been checked and cleared. Those are different states and the caller
#' now says which one it is in.
#'
#' @param fit A fitted binomial `glm`.
#' @return A list with `status`, one of `"separated"`, `"not_separated"` or
#'   `"unknown"`, and `reason`, a string explaining an unknown. A warning is
#'   muffled and the outcome used, since a separated refit is the case that
#'   warns.
#' @keywords internal
.stc_separation_status <- function(fit) {
  unknown <- function(reason) list(status = "unknown", reason = reason)
  if (!requireNamespace("detectseparation", quietly = TRUE)) {
    return(unknown(paste("the optional detectseparation package is not",
                         "installed, so the linear program was not run")))
  }
  # Rebuild the fit's own call rather than going through `update()`. That
  # evaluates in its CALLER's frame, which here is this function, so the data
  # argument is looked up from the package namespace outward: it resolves only
  # when the data happens to sit in the global environment, and fails whenever
  # the caller holds it in a local one. The fit already records where its own
  # terms were built, and that is the environment the data was visible in.
  cl <- stats::getCall(fit)
  if (is.null(cl)) {
    return(unknown("the fit records no call, so it could not be re-run"))
  }
  cl$method <- quote(detectseparation::detect_separation)
  env <- environment(stats::formula(fit))
  if (!is.environment(env)) {
    env <- parent.frame()
  }
  # Warnings are MUFFLED, not treated as failure. Fitting a separated model is
  # the case that warns ("fitted probabilities numerically 0 or 1 occurred"),
  # so folding warnings into "unknown" blinded this check exactly when the
  # answer is TRUE, and did so only on the platforms that happen to emit one.
  # An error is a different matter: then there is no outcome to read.
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
#' @keywords internal
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
#' @keywords internal
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
  # Take the boundary correction from the POOLED n, as `.naive_binomial()`
  # does. With each row's own n the answer depends on how one comparator arm
  # was tabulated: 0/100 corrects to 0.5/101, while 0/50 + 0/50 corrects to
  # 0.5/51 twice, so two descriptions of the same data give different standard
  # errors. Interior rows are untouched either way.
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
  # Use the boundary-corrected variance on the absolute scale too. With raw
  # `row_p` a zero-event or all-event comparator arm has p(1 - p) = 0 and
  # contributes no uncertainty: 0/100 gave p_B_se = 0, a degenerate [0, 0]
  # interval, and a risk difference whose SE ignored the comparator entirely,
  # although 0/100 alone is consistent with p up to roughly 0.03. The
  # link-scale effect and the log risk ratio already used the corrected
  # variance; these did not. This mirrors `.naive_binomial()`.
  p_B_se <- .sqrt_variance(var_p_B_effect, "comparator probability variance")
  # The standardized index probability is a model prediction, so its
  # interval is the delta-method one, bounded to [0, 1]; it is asymptotic.
  # The comparator arm is observed directly, and its interval is exact, as
  # in `.naive_binomial()`.
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
#' the comparator grid, and its uncertainty comes through the delta method
#' from the coefficient covariance. The gradients are analytic:
#' `d p / d beta = sum(w_i p_i'(eta_i) X_i) / sum(w_i)`, with `p_i'` the
#' inverse link's derivative, and the link-scale and log-scale functionals
#' follow by the chain rule. A central difference in the coefficient
#' coordinates was used before, with a step proportional to `max(1, |beta|)`;
#' that step is not a property of the model. Multiply a predictor by 1e6 and
#' its coefficient shrinks by 1e6 while the step stays near 6e-6, so the
#' perturbation moved the target linear predictor by about 6, not a local
#' derivative at all, and a comparator probability of 0.75 on 40 subjects
#' at the observed profile reported a standard error of 0.032 instead of the
#' 0.068 the same data give in any other units. Analytic gradients transform
#' with the design, so equivalent units give equivalent uncertainty.
#'
#' Everything is formed on the log scale so that a tail probability outside
#' double precision keeps its digits: see [.stc_binomial_gradients()].
#' @keywords internal
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
#' each grid point (from [.binary_log_probs()]), the standardized log
#' probability is `log(sum(w_i p_i) / sum(w_i))` and its gradient is
#' `sum(c_i (d log p_i / d eta) X_i)` with `c_i = w_i p_i / sum(w p)`, the
#' share of the standardized probability each point carries. The per-point
#' derivative of the log probability is `q_i` under the logit, the inverse
#' Mills ratio `phi(eta) / Phi(eta)` under the probit, and
#' `exp(eta - exp(eta)) / p_i` under the complementary log-log; each is
#' formed from the log probabilities so a point deep in either tail
#' contributes its share rather than a rounded zero. The same for the
#' non-event side, with `-p_i`, `-phi(eta) / Phi(-eta)` and `-exp(eta)`.
#'
#' The link-scale functional is then the chain rule on the two log means:
#' the difference of the two gradients for the logit; for the probit,
#' `d p / phi(z)` at the link value `z`, taken from whichever tail is the
#' smaller one, as [.binary_link_from_logs()] does; for the complementary
#' log-log, `d log q / log q`, or the log-probability gradient once
#' `log q` has rounded to zero and the link is `log p` to double precision.
#'
#' @param X Comparator design, one row per grid point.
#' @param eta Linear predictor at each grid point.
#' @param weights Non-negative weights, one per grid point.
#' @param link The binomial link.
#' @return List of gradient vectors `log_mean`, `log_nonevent_mean`, `mean`
#'   and `link`, one entry per coefficient.
#' @keywords internal
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
  # The log weights are normalized by a shifted log-sum-exp, not by
  # log(sum(weights)): two weights of 1e308 are finite and their sum is not,
  # which sent every log share to -Inf and the gradient of a point mass to 0.
  # [.weighted_log_mean_exp()] normalizes its denominator the same way.
  log_weights <- log(weights)
  m_w <- max(log_weights)
  log_w <- log_weights - (m_w + log(sum(exp(log_weights - m_w))))
  log_p_mean <- .weighted_log_mean_exp(lp$event, weights)
  log_q_mean <- .weighted_log_mean_exp(lp$nonevent, weights)
  # Subtract the mean first, then add the weight. The other order adds a
  # weight of order 1 to a log probability of order 1e17 (the cloglog
  # non-event log probability is -exp(eta), which is -2.4e17 at eta = 40),
  # where the double's spacing is 32 and the weight is lost entirely. Every
  # point then takes the whole share instead of its own: a target that is 64
  # copies of one profile gave a link gradient of 64 where standardizing a
  # point mass cannot change its link at all, so the answer is 1. Centering
  # first cancels the huge common term against itself, exactly, and leaves
  # the weight against a number of order 1.
  share_p <- exp(log_w + (lp$event - log_p_mean))
  share_q <- exp(log_w + (lp$nonevent - log_q_mean))
  grad_log_p <- colSums(share_p * d_log_p * X)
  grad_log_q <- if (link == "cloglog") {
    # The non-event derivative -exp(eta) overflows past eta = 709, where the
    # point's share is 0, and 0 * -Inf is NaN. Formed as one exponent the
    # product underflows to the 0 it is, and a saturated point beside an
    # ordinary one leaves the gradient finite. When every point is
    # saturated the non-event mean is 0 to double precision, the link
    # `.binary_link_from_logs()` reports is +Inf, and the gradient is NaN
    # with it; the finite-variance guard then refuses the fit, as it did
    # before, rather than attach a finite SE to an infinite estimate.
    colSums(-exp(log_w + (lp$nonevent - log_q_mean) + eta) * X)
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
    # `d log q-bar / log q-bar` is the link's own derivative and is exact
    # wherever it can be formed. The non-event log probability is built as
    # -exp(eta) rather than as log(1 - p), so it stays representable until
    # exp(eta) itself underflows below eta = -745; only there is the link
    # log(-log q-bar) equal to log p-bar to double precision. Switching at
    # log p-bar = -18 instead left the point-mass derivative at 1 - p-bar / 2
    # rather than 1, a relative 1e-9 at eta = -20 where the exact form was
    # available.
    grad_log_p
  } else {
    grad_log_q / log_q_mean
  }
  list(log_mean = grad_log_p, log_nonevent_mean = grad_log_q,
       mean = grad_mean, link = grad_link)
}

#' Stable Euclidean norm of two standard errors
#' @keywords internal
.stc_hypot <- function(x, y) {
  if (any(is.infinite(c(x, y)))) return(Inf)
  scale <- max(abs(c(x, y)))
  if (scale == 0) return(0)
  scale * sqrt((x / scale)^2 + (y / scale)^2)
}

#' Normal-outcome STC estimator
#' @keywords internal
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
#' @keywords internal
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

  # Gradient of the standardized RATE itself, not of its logarithm:
  # d/dbeta of sum_i w_norm_i * exp(eta_i) is sum_i w_norm_i * lambda_i * X_i.
  # The log-rate gradient above normalizes by the exponentially weighted
  # `contribution` instead, which is a different weighting, so both are needed.
  w_norm <- weights / sum(weights)
  lambda_comp <- exp(eta_comp)
  grad_rate <- colSums(w_norm * lambda_comp * X_comp_design)
  var_rate_A <- .nonnegative_variance(
    as.numeric(t(grad_rate) %*% V %*% grad_rate),
    "poisson STC rate variance"
  )
  # A fit with no events, or with a direction along which the likelihood
  # rises without bound, never reaches here: .stc_refuse_poisson_recession()
  # refused it, so the delta-method variance is that of an interior maximum.
  # Rate difference on the natural per-unit-exposure scale: the standardized
  # index rate minus the observed comparator rate. The standardized rate's
  # variance is the delta-method one already computed for it, and the two arms
  # are independent, so the variances add.
  rd <- rate_hat_A - rate_B
  # The comparator variance uses the continuity-corrected count, so a
  # zero-event comparator arm still contributes uncertainty rather than
  # collapsing the interval; the log-rate contrast already corrects the same
  # way.
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
#' @keywords internal
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
#' @keywords internal
.stc_survival <- function(data, conf_level, z, distribution, n_boot = 200L,
                          seed = NULL, rmst_horizon = NULL) {
  if (!requireNamespace("flexsurv", quietly = TRUE)) {
    stop("Package 'flexsurv' is required for survival STC. ",
         "Install it or use mlumr() / naive().", call. = FALSE)
  }
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  cov_names <- data$covariates
  # Flexible baselines ("mspline"/"pexp") have no parametric flexsurv analogue.
  # Rather than silently report the requested distribution while actually
  # fitting a Weibull, flag the approximation: warn, and record both the
  # requested `distribution` and the `distribution_fit` actually used.
  # switch() in .stc_flexsurv_dist() used to end in an unnamed default, so any
  # unrecognized name (a typo such as "weibul") fell through to a Weibull fit
  # while the returned object still reported the name the user typed and
  # approximated = FALSE. Wrong model, wrong label, no warning. Validate first.
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
    warning(
      sprintf(
        paste0(
          "Survival STC has no parametric analogue for a '%s' baseline; ",
          "fitting a Weibull G-computation as an approximate RMST benchmark ",
          "(the result reports distribution_fit = \"weibull\"). Request ",
          "distribution = \"weibull\" to silence this, or use mlumr() for ",
          "the flexible-baseline Bayesian fit."
        ),
        distribution
      ),
      call. = FALSE
    )
  }
  # Map the actually-fitted family (dist_fit) to its flexsurv name. dist_fit is
  # the single normalized representation (Weibull for flexible-baseline requests),
  # so the flexsurv lookup never depends on the mspline/pexp fallback entries.
  dist_fs <- .stc_flexsurv_dist(dist_fit)
  # RMST is an integral to a restriction time, so an STC estimate is only
  # comparable with a Bayesian one when both use the same horizon. mlumr() can
  # narrow its default to the follow-up both studies observed, which differs
  # from the pooled maximum used here, so the horizon has to be settable.
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

  # See .stc_survival_point(): a negative fitted shape / Q is outside the
  # parameter space of the Bayesian model carrying the same name.
  out_of_family <- NULL
  if (!is.null(point$family_par_name) && any(point$family_par < 0)) {
    out_of_family <- point$family_par_name
    approximated <- TRUE
    dist_fit <- sprintf("flexsurv %s, unrestricted %s", distribution,
                        point$family_par_name)
    warning(sprintf(
      paste0("The STC '%s' fit has %s = %s, outside the parameter space of ",
             "mlumr()'s '%s' model, which constrains %s > 0. flexsurv admits ",
             "the negative branch, so this benchmark and the Bayesian fit of ",
             "the same name are different distributional families here: a ",
             "difference between them need not be a Bayesian-versus-",
             "frequentist difference. Compare the collapsible RMST estimands, ",
             "or choose a distribution whose parameter spaces agree."),
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
    # Each replicate carries its own family parameter as well as the two
    # estimates: the point-fit check above cannot see a resample that leaves
    # mlumr()'s parameter space while the point estimate stays inside it, and
    # those refits still enter the SE.
    boot <- vapply(seq_len(n_boot), function(b) {
      ib <- ipd[sample(nrow(ipd), replace = TRUE), , drop = FALSE]
      pb <- pseudo[sample(nrow(pseudo), replace = TRUE), , drop = FALSE]
      # A resample can lose every event in an arm. flexsurvreg() then returns
      # optimizer-boundary parameters with a warning rather than an error, so
      # the replicate would be counted as a success and its number would enter
      # the standard error. Treat it as the failed fit it is.
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
    # Count the two quantities separately: a resample can return a finite RMST
    # difference while the cumulative-hazard ratio is undefined at the horizon
    # (a boundary survival), and one shared count would hide that.
    n_boot_ok <- sum(!is.na(boot[1, ]))
    n_boot_ok_chr <- sum(!is.na(boot[2, ]))
    n_boot_failed <- n_boot - n_boot_ok
    n_boot_failed_chr <- n_boot - n_boot_ok_chr
    # Warn on EITHER shortfall. Gating on the RMST count alone left a run in
    # which every RMST difference was finite but several cumulative-hazard
    # ratios were not silently reporting a log-CHR interval built from fewer
    # replicates than the RMST one.
    if (n_boot_failed > 0L || n_boot_failed_chr > 0L) {
      warning(sprintf(
        paste0("Bootstrap successes: RMST difference %d/%d; log cumulative-",
               "hazard ratio %d/%d. Each standard error is based only on its ",
               "own successful resamples. A high failure rate gives an ",
               "over-narrow SE; consider a different `distribution` or a ",
               "larger `n_boot`."),
        n_boot_ok, n_boot, n_boot_ok_chr, n_boot
      ), call. = FALSE)
    }
    # How many resamples left the Bayesian model's parameter space. NA_integer_
    # when the distribution has no such parameter, which is not the same as
    # zero and must not print as though it had been checked.
    n_boot_out_of_family <- if (is.null(point$family_par_name)) {
      NA_integer_
    } else {
      as.integer(sum(apply(boot[3:4, , drop = FALSE], 2L,
                           function(v) any(!is.na(v) & v < 0))))
    }
    if (!is.na(n_boot_out_of_family) && n_boot_out_of_family > 0L) {
      warning(sprintf(
        paste0("%d of %d bootstrap resample(s) fitted %s < 0, outside the ",
               "parameter space of mlumr()'s '%s' model, and those refits are ",
               "included in the standard error. The interval is therefore a ",
               "broader-family flexsurv benchmark rather than a like-for-like ",
               "comparison with the Bayesian fit of the same name."),
        n_boot_out_of_family, n_boot, point$family_par_name, distribution
      ), call. = FALSE)
    }
    # Fewer than two successes leaves sd() undefined; make that explicit rather
    # than letting an NA propagate as though the bootstrap had simply failed.
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
    # Name of the parameter whose sign decides family membership ("shape" for
    # Gompertz, "Q" for the generalized gamma), NULL when the distribution has
    # none. Reported separately from `out_of_family`, which is set only when the
    # POINT fit left the space.
    family_par_name = point$family_par_name,
    horizon = horizon,
    rmst_index_comparator = point$rmst_index,
    rmst_index = point$rmst_index,
    rmst_comparator = point$rmst_comparator,
    # Cumulative-hazard ratio at the horizon (ratio of cumulative hazards
    # H(horizon) = -log S(horizon)), with a bootstrap SE/CI on the log scale.
    # This is not a hazard ratio in general; see .stc_survival_point().
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
    # Resamples whose fitted shape / Q left mlumr()'s parameter space but whose
    # RMST still entered the SE. NA_integer_ when the distribution has no such
    # parameter to leave.
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
#' @keywords internal
.validate_stc_survival_events <- function(ipd, pseudo) {
  n_idx <- sum(ipd$.status == 1L)
  n_cmp <- sum(pseudo$.status == 1L)
  if (n_idx == 0L || n_cmp == 0L) {
    stop("Survival STC needs at least one event in each arm: observed ",
         n_idx, " in the index arm and ", n_cmp, " in the comparator arm. ",
         "With an event-free arm the parametric survival fit has no finite ",
         "interior estimate, so the RMST difference it produces is an ",
         "artifact of where the optimizer stopped.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate survival STC input supported by flexsurv formula construction
#' @keywords internal
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

#' Require a flexsurv fit that actually converged
#'
#' A sparse or nearly separated sample can leave events in both arms and still
#' send the optimizer to a boundary. `flexsurvreg()` warns in that case rather
#' than failing, so the estimates were summarized as an ordinary RMST, and the
#' bootstrap counted such refits among its successes because `tryCatch()` sees
#' only errors. Raising an error here makes a non-converged replicate a failed
#' one, which is what it is.
#' @keywords internal
.validate_flexsurv_fit <- function(fit, arm) {
  conv <- fit$opt$convergence
  if (!is.null(conv) && !identical(as.integer(conv), 0L)) {
    stop("The survival STC fit for the ", arm, " arm did not converge ",
         "(optimizer code ", as.integer(conv), "), so its restricted mean is ",
         "an artifact of where the optimizer stopped.", call. = FALSE)
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
  # A finite covariance is not yet a usable one. The optimizer can report
  # convergence while the Hessian is not positive definite, which is a saddle
  # or boundary point rather than a maximum; flexsurv then returns finite
  # variances that can be zero or negative. Checking only for NA / Inf accepts
  # that fit, and the bootstrap counts it among its successes.
  if (!is.null(fit$cov) && length(fit$cov) > 0L) {
    v <- diag(as.matrix(fit$cov))
    ev <- tryCatch(
      eigen(as.matrix(fit$cov), symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) NA_real_
    )
    if (any(v <= 0) || anyNA(ev) || min(ev) <= 0) {
      stop("The survival STC fit for the ", arm, " arm returned a covariance ",
           "matrix that is not positive definite, so the optimizer stopped at ",
           "a saddle or boundary point rather than a maximum and its ",
           "uncertainty is not usable.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' One STC survival point estimate (RMST_index, RMST_comparator, difference)
#' @keywords internal
.stc_survival_point <- function(ipd, pseudo, cov_names, comp_cov, dist_fs, horizon) {
  ipd$.stc_event <- as.integer(ipd$.status == 1L)
  pseudo$.stc_event <- as.integer(pseudo$.status == 1L)

  # Build the formula from symbols rather than pasting names into a string, as
  # `.stc_formula()` already does for the other families. Backtick-quoting a
  # name that itself contains a backtick produces a formula that does not parse,
  # so a column this function has already accepted as a valid numeric covariate
  # would fail here instead.
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
  # An equal-weight mean IS the comparator-population average here: survival
  # AgD carries exactly one arm-summary row (set_agd_surv() rejects multi-arm
  # comparators), so `comp_cov` is that single row's integration grid and the
  # points are equally weighted by construction. The other families average
  # over several AgD rows and must weight by `agd$.n` / `agd$.E`, which
  # survival AgD does not carry. Weighting has to arrive with multi-row
  # support, not before it.
  rmst_index <- mean(rmst_a_rows$est)

  fit_b <- flexsurv::flexsurvreg(survival::Surv(.time, .stc_event) ~ 1,
                                 data = pseudo, dist = dist_fs)
  .validate_flexsurv_fit(fit_b, "comparator")
  rmst_b <- summary(fit_b, type = "rmst", t = horizon, ci = FALSE,
                    tidy = TRUE)$est[1]

  # Cumulative-hazard ratio at the horizon: the ratio of cumulative hazards
  # H(t) = -log S(t) at t = horizon, for the G-computed index survival
  # (standardized to the comparator covariates) versus the comparator. This is
  # NOT in general a hazard ratio: only when the two separately-fitted survival
  # models happen to be proportional with a common baseline shape does it equal
  # the constant HR. NA if either survival is at a boundary (no events / certain
  # survival), where the log ratio is undefined.
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

  # mlumr's Bayesian Gompertz constrains the shape to be positive, and its
  # generalized gamma is the positive-Q (Lawless k > 0) subfamily. flexsurv
  # admits the negative branch of both, so an STC benchmark can land outside the
  # family its label denotes and would then not be a like-for-like comparison
  # with the Bayesian fit of the same name. Report the parameter so stc() can
  # say so rather than leaving the reader to assume the spaces match.
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
#' @keywords internal
.stc_flexsurv_dist <- function(distribution) {
  switch(distribution,
    exponential = "exp", "exponential-aft" = "exp",
    weibull = "weibull", "weibull-aft" = "weibull",
    gompertz = "gompertz", lognormal = "lnorm", loglogistic = "llogis",
    gamma = "gamma", gengamma = "gengamma",
    # Flexible baselines have no parametric STC analogue; approximate with
    # a Weibull G-computation.
    mspline = "weibull", pexp = "weibull",
    # No unnamed default: an unrecognized name must not fall through to a
    # Weibull fit that the result would then mislabel. stc() validates the name
    # before this point, so reaching here at all is a bug.
    stop("Unsupported survival distribution: ", distribution, call. = FALSE)
  )
}

#' Build comparator-population covariates from AgD means
#' @keywords internal
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
