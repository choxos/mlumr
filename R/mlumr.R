#' Fit ML-UMR Model
#'
#' Fit a Bayesian multilevel unanchored meta-regression model using individual
#' patient data (IPD) and aggregate data (AgD). Supports binary, continuous,
#' and count outcomes.
#'
#' @param data An `mlumr_data` object with integration points (from
#'   [add_integration()])
#' @param model Model type: `"spfa"` (shared prognostic factor assumption) or
#'   `"relaxed"` (treatment-specific coefficients). Default `"spfa"`.
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
#'   poisson: `"log"` (default, only option). If `NULL`, uses the canonical
#'   default for the family.
#' @param prior_intercept Prior for treatment intercepts. Default from
#'   [default_prior_intercept()] (`prior_normal(0, 10)`, weakly informative
#'   on the linear-predictor scale). See [prior_normal()] for guidance.
#' @param prior_beta Prior for regression coefficients. May be a single
#'   prior broadcast to all covariates, or a `list` of priors of length
#'   `n_cov` for per-coefficient specification. All per-coefficient priors
#'   must share the same family and (for Student-t) df. Default from
#'   [default_prior_beta()] (`prior_normal(0, 2.5)`; Gelman et al. 2008;
#'   Stan community prior-choice wiki). Set `autoscale = TRUE` on the
#'   prior to divide the scale by each covariate's empirical SD — useful
#'   when predictors are on very different scales.
#' @param prior_sigma Prior for residual SD (normal family only). Default
#'   from [default_prior_sigma()] (`prior_normal(0, 2.5)`, half-normal via
#'   the Stan `<lower=0>` constraint). [prior_exponential()] is also
#'   supported for sigma.
#' @details
#' The model assumes that all AgD rows come from the same comparator treatment
#' and that, conditional on covariates, there is no between-study heterogeneity.
#' If AgD rows come from multiple studies with different designs or unmeasured
#' confounders, this assumption may not hold. No random effects for study-level
#' heterogeneity are included.
#'
#' **AgD scale assumptions (family = `"normal"`).** The AgD likelihood is
#' `y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
#' `y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`. In both
#' cases `set_agd()` expects `outcome_mean` and `outcome_se` on the
#' **arithmetic (original, untransformed) scale**, not log-scale or
#' geometric. Passing log-scale summaries silently misspecifies the
#' likelihood. See [set_agd()] for details.
#'
#' **The comparator population is the size-weighted mixture of its aggregate
#' rows.** Integrated marginal predictions in the comparator population
#' (`*_comparator` generated quantities) weight each row by the population it
#' represents:
#' \itemize{
#'   \item **binomial**: `n_agd[k]`, the AgD sample size.
#'   \item **normal**: `agd_weight[k]`, from `outcome_n`. This is required for
#'     more than one aggregate row, and is `1` for a single row where the
#'     weighting is irrelevant.
#'   \item **poisson**: `E_agd[k]`, the AgD exposure.
#' }
#' The weights say which population the estimand refers to, and are deliberately
#' separate from the likelihood's own precision weighting, which says how much
#' each row constrains the parameters. Because the parts of a split subgroup sum
#' to the whole, the estimand does not change with how the aggregate evidence
#' happens to be tabulated.
#'
#' **Weakly-identified coefficients in the relaxed model** —
#' `beta_comparator` is identified only through AgD, so the relaxed
#' model needs informative priors (or many AgD rows) to estimate
#' effect modification reliably. [prior_sensitivity()] is the
#' recommended diagnostic.
#'
#' @seealso [prior_sensitivity()] for sensitivity of the posterior
#'   to `prior_beta`; [set_agd()] for AgD scale requirements;
#'   [prior_summary()] for introspection of the priors actually used.
#'
#' @param center Logical (default `TRUE`). Center the covariates about the
#'   pooled IPD and population-weighted declared AgD means before fitting.
#'   The likelihood is unchanged after the intercept is transformed with the
#'   slopes, and centering often improves sampling geometry. Priors specified
#'   independently on the numerical intercept and slopes are not generally
#'   invariant to that transformation, so `center = TRUE` and `FALSE` can imply
#'   different joint priors even when their likelihoods represent the same
#'   regression model. Set `FALSE` to fit on the raw covariate scale.
#' @param qr Logical (default `FALSE`). Apply a thin-QR
#'   reparameterization to the combined (intercepts + covariates) design matrix.
#'   This decorrelates the design columns for more efficient HMC. The Stan model
#'   maps the requested priors to the original regression coefficients before
#'   the QR transform, so this option is intended as a computational
#'   reparameterization. Useful with many correlated or ill-scaled covariates;
#'   for the common few-covariate case the default fused-GLM path (with
#'   `center = TRUE`) is usually faster.
#' @param chains Number of MCMC chains (default 4)
#' @param iter Total iterations per chain (default 2000)
#' @param warmup Number of warmup iterations (default 1000)
#' @param seed Random seed for reproducibility
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
                  ...) {

  model <- match.arg(model)

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(center) || length(center) != 1L || is.na(center)) {
    stop("`center` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(qr) || length(qr) != 1L || is.na(qr)) {
    stop("`qr` must be TRUE or FALSE.", call. = FALSE)
  }
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

  # C.3: autoscale is only consumed by prior_beta. Warn if the user set it
  # on prior_intercept or prior_sigma, because it will be silently ignored.
  if (isTRUE(prior_intercept$autoscale)) {
    warning("`autoscale = TRUE` on prior_intercept is ignored; ",
            "autoscaling is only applied to prior_beta.", call. = FALSE)
  }

  family <- data$family %||% "binomial"
  link_info <- check_link(family, link)

  if (family == "normal") {
    validate_prior(prior_sigma, "sigma")
    if (isTRUE(prior_sigma$autoscale)) {
      warning("`autoscale = TRUE` on prior_sigma is ignored; ",
              "autoscaling is only applied to prior_beta.", call. = FALSE)
    }
  } else if (!is.null(prior_sigma) && !isTRUE(prior_sigma$default)) {
    warning("`prior_sigma` is ignored for non-normal families.",
            call. = FALSE)
  }

  # Warn about weak identifiability in relaxed models with sparse AgD
  if (model == "relaxed") {
    n_agd_rows_check <- nrow(data$agd$data)
    n_cov_check <- data$n_covariates
    if (n_agd_rows_check < 2 * n_cov_check) {
      warning(sprintf(
        paste0("Relaxed model with %d AgD row(s) and %d covariate(s): ",
               "beta_comparator may be weakly identified. ",
               "Consider using more informative priors on beta or the SPFA model."),
        n_agd_rows_check, n_cov_check
      ), call. = FALSE)
    }
  }

  prepared <- .mlumr_build_stan_data(
    data = data,
    family = family,
    link_info = link_info,
    prior_intercept = prior_intercept,
    prior_beta = prior_beta,
    prior_sigma = prior_sigma,
    model = model,
    center = center,
    qr = qr
  )
  stan_data <- prepared$stan_data

  # Select Stan model (family_config is the single source of truth)
  model_name <- paste0(get_family_config(family)$stan_prefix, "_", model)

  .mlumr_log_fit_start(model_name, family, link_info$link, stan_data,
                       engine, verbose)

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
  summary_df <- result$summary_df
  n_divergent <- result$n_divergent
  n_max_td <- result$n_max_td

  # Resolved prior info for prior_summary(): carries both the user-specified
  # priors and the Stan-scale values actually used (post-autoscale), plus the
  # covariate SDs used for autoscaling, so the summary can reconstruct the
  # effective prior that was sampled.
  priors <- .mlumr_prior_metadata(
    data = data,
    family = family,
    prior_intercept = prior_intercept,
    prior_beta = prior_beta,
    prior_sigma = prior_sigma,
    beta_fields = prepared$beta_fields,
    sd_x = prepared$sd_x
  )

  out <- list(
    stanfit = fit,
    draws = draws,
    summary = summary_df,
    diagnostics = list(
      n_divergent = n_divergent,
      n_max_treedepth = n_max_td
    ),
    data = data,
    family = family,
    link = link_info$link,
    link_code = link_info$code,
    model = model,
    model_name = model_name,
    stan_data = stan_data,
    # Design-matrix controls that change the fitted parameterization (the
    # centered intercept, the QR-rotated coefficients). They have to travel
    # with the fit so a refit such as prior_sensitivity() reproduces the same
    # model rather than silently reverting to the defaults.
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
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    )
  )

  class(out) <- c("mlumr_fit", "list")

  check_diagnostics(out)

  mlumr_message("Fitting complete!", verbose = verbose)
  out
}

#' Build the Stan data list for mlumr()
#' @keywords internal
.mlumr_build_stan_data <- function(data, family, link_info, prior_intercept,
                                   prior_beta, prior_sigma,
                                   model = "spfa", center = TRUE, qr = FALSE) {
  ipd_data <- data$ipd$data
  agd_data <- data$agd$data
  X_ipd <- as.matrix(ipd_data[, data$covariates])

  # Autoscaling is based on the IPD covariate scale because the regression
  # coefficients are estimated from individual observations.
  sd_x <- apply(X_ipd, 2, stats::sd)
  intercept_fields <- stan_prior_fields(prior_intercept)
  beta_fields <- stan_prior_fields_beta(
    prior_beta,
    data$n_covariates,
    sd_x = sd_x,
    covariate_names = data$covariates
  )

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
    link = link_info$code
  )

  if (family == "binomial") {
    stan_data$y_ipd <- as.integer(ipd_data$.outcome)
    stan_data$n_agd <- array(as.integer(agd_data$.n))
    stan_data$r_agd <- array(as.integer(agd_data$.r))
  } else if (family == "normal") {
    sigma_fields <- stan_prior_fields(prior_sigma)
    stan_data$y_ipd <- as.numeric(ipd_data$.outcome)
    stan_data$y_agd <- array(as.numeric(agd_data$.y))
    stan_data$se_agd <- array(as.numeric(agd_data$.se))
    # Target-population weights for the comparator estimand. These say which
    # comparator population the standardized effect refers to, and are separate
    # from the likelihood's 1/se^2 precision weights. set_agd() requires
    # outcome_n for more than one row, so the fallback covers single-row data.
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
  } else {
    stan_data$y_ipd <- as.integer(ipd_data$.outcome)
    stan_data$E_ipd <- as.numeric(ipd_data$.exposure)
    stan_data$r_agd <- array(as.integer(agd_data$.r))
    stan_data$E_agd <- array(as.numeric(agd_data$.E))
  }

  # Shared, all-family covariate centering and combined-design QR
  # reparameterization. Centering is likelihood-invariant (the intercept absorbs
  # the shift), so the estimands are unchanged. It is NOT prior-invariant: the
  # intercept prior is placed on the centered intercept, so the center must not
  # depend on tuning controls like `n_int` (see .mlumr_center_covariates(), which
  # weights by AgD rows, not row*point, precisely so the induced prior does not
  # move). QR is an affine reparameterization of the (intercepts + covariates)
  # design that decorrelates the sampling geometry. These leave the likelihood
  # family and response-scale estimands unchanged, but a fixed numerical prior
  # need not represent the same prior after a change of parameterization.
  agd_means <- as.matrix(agd_data[, paste0(data$covariates, "_mean"),
                                  drop = FALSE])
  stan_data <- .mlumr_center_covariates(
    stan_data, center = center, family = family, agd_means = agd_means
  )
  stan_data <- .mlumr_qr_design(stan_data, model = model, qr = qr)

  list(stan_data = stan_data, beta_fields = beta_fields, sd_x = sd_x)
}

#' Population weights for the AgD rows used in covariate centering
#'
#' Returns one weight per aggregate row, taken from the family's comparator
#' weight field (`n_agd`, `agd_weight`, `E_agd`). Falls back to equal weights when no usable field is present.
#' Weights must be positive and finite, and must sum over a split subgroup to
#' the same total as the unsplit one, which is what makes the center invariant
#' to how the aggregate evidence is tabulated.
#'
#' @param stan_data The assembled Stan data list.
#' @param family Outcome family name.
#' @param n_agd_rows Number of aggregate rows.
#' @return Numeric vector of length `n_agd_rows`.
#' @keywords internal
.agd_center_weights <- function(stan_data, family, n_agd_rows) {
  fallback <- rep(1, n_agd_rows)
  cfg <- tryCatch(get_family_config(family), error = function(e) NULL)
  field <- if (is.null(cfg)) NULL else cfg$comp_weight_field
  w <- if (!is.null(field)) stan_data[[field]] else NULL
  if (is.null(w)) return(fallback)
  w <- as.numeric(w)
  if (length(w) == 1L && n_agd_rows > 1L) w <- rep(w / n_agd_rows, n_agd_rows)
  if (length(w) != n_agd_rows || !all(is.finite(w)) || any(w <= 0)) {
    return(fallback)
  }
  w
}

#' Center IPD + integration covariates about their pooled mean (all families)
#'
#' Matches `center = TRUE` default. The intercept then represents the
#' baseline at the average covariate rather than at covariate = 0, removing the
#' intercept<->slope collinearity that forces deep NUTS trajectories on
#' real-scale covariates. The likelihood is invariant because `X_ipd` and the
#' integration grid are shifted by the same `xbar` and the intercept absorbs the
#' shift. A fixed numerical intercept prior is placed on the centered intercept,
#' however, so centering need not leave the posterior unchanged. `cov_center`
#' is always stored (zeros when `center = FALSE`) so predict()/conditional_effects()
#' can map raw-scale covariate values onto the (possibly centered) model scale.
#' @keywords internal
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
    # Use the declared AgD covariate means when the model builder supplies them.
    # Falling back to realized grid means keeps this helper usable in isolation.
    # The production center therefore does not move with the QMC resolution,
    # which would otherwise move the induced raw-scale intercept prior.
    if (is.null(agd_means)) {
      agd_means <- apply(X_int, c(1, 3), mean)
    }
    agd_row_means <- matrix(as.numeric(agd_means), nrow = n_agd_rows,
                            ncol = n_cov)
    # Weight each AgD row by the population it represents, not by 1. Row counts
    # are a property of how the aggregate evidence happens to be TABULATED:
    # splitting one comparator subgroup into two statistically equivalent rows
    # would otherwise change `n_agd_rows`, move `xbar`, and therefore change the
    # induced raw-scale intercept prior even though the likelihood and the
    # target estimand are unchanged. Sample-size weights are invariant under any
    # such split or merge, because the parts sum to the whole.
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
#' @keywords internal
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
    # A thin QR needs full column rank: R is inverted to recover the
    # original-scale coefficients. set_ipd() permits a constant covariate with a
    # warning, and in a relaxed model that column is exactly collinear with its
    # own intercept, so the combined design can be rank deficient even though
    # the model is still estimable from the coefficient priors when qr = FALSE.
    # Fail here with something the user can act on rather than inside solve().
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

#' Validate mlumr() sampler controls before backend dispatch
#' @keywords internal
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
  if (!is.numeric(adapt_delta) || length(adapt_delta) != 1L ||
        !is.finite(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) {
    stop("`adapt_delta` must be a single finite number between 0 and 1.",
         call. = FALSE)
  }
  .validate_mlumr_integer(max_treedepth, "max_treedepth", lower = 1L)
  .validate_mlumr_integer(refresh, "refresh", lower = 0L)
  invisible(TRUE)
}


#' Validate an integer-like mlumr() argument
#' @keywords internal
.validate_mlumr_integer <- function(x, name, lower) {
  valid <- is.numeric(x) &&
    length(x) == 1L &&
    is.finite(x) &&
    x == floor(x) &&
    x >= lower
  if (!valid) {
    stop(sprintf("`%s` must be a single integer >= %d.", name, lower),
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Resolve and validate mlumr() backend engine
#' @keywords internal
.resolve_mlumr_engine <- function(engine) {
  .validate_engine_name(engine %||% get_engine())
}

#' Log mlumr() fit metadata
#' @keywords internal
.mlumr_log_fit_start <- function(model_name, family, link, stan_data,
                                 engine, verbose) {
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
  } else {
    mlumr_message(sprintf("  AgD: %d rows, total exposure = %.1f",
                          stan_data$n_agd_rows, sum(stan_data$E_agd)),
                  verbose = verbose)
  }

  mlumr_message(sprintf("  Covariates: %d, Integration points: %d",
                        stan_data$n_cov, stan_data$n_int),
                verbose = verbose)
  mlumr_message(sprintf("  Engine: %s", engine), verbose = verbose)
}

#' Dispatch mlumr() sampling to the selected backend
#' @keywords internal
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
#' @keywords internal
.mlumr_prior_metadata <- function(data, family, prior_intercept, prior_beta,
                                  prior_sigma, beta_fields, sd_x) {
  priors <- list(
    intercept = prior_intercept,
    beta = prior_beta,
    beta_resolved = list(
      covariate_names = data$covariates,
      mean = beta_fields$mean,
      sd = beta_fields$sd,
      dist = beta_fields$dist,
      df = beta_fields$df,
      autoscale = beta_fields$autoscale,
      sd_x = sd_x
    )
  )

  if (family == "normal") {
    priors$sigma <- prior_sigma
  }

  priors
}
