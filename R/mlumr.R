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
#' **Comparator-population weighting is family-dependent.** Integrated
#' marginal predictions in the comparator population (`*_comparator`
#' generated quantities) are weighted by:
#' \itemize{
#'   \item **binomial**: `n_agd[k]` (AgD sample size), so larger
#'     AgD rows contribute more to the marginal mean.
#'   \item **normal**: equal weights across AgD rows (`/ n_agd_rows`),
#'     reflecting the normal likelihood's treatment of each row as a
#'     single summary statistic.
#'   \item **poisson**: `E_agd[k]` (AgD exposure), matching the
#'     rate-based likelihood.
#' }
#' Each weighting is natural for the corresponding likelihood; users
#' comparing marginal effects across families should be aware they are
#' not identically weighted.
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
    prior_sigma = prior_sigma
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
                                   prior_beta, prior_sigma) {
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

  list(stan_data = stan_data, beta_fields = beta_fields, sd_x = sd_x)
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
                               refresh, ...) {
  if (engine == "cmdstanr") {
    fit_cmdstanr(model_name, stan_data, chains, iter, warmup,
                 seed, adapt_delta, max_treedepth, refresh, ...)
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

