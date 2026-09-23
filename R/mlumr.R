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
