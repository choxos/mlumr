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
#' @param distribution For `family = "survival"` only: the survival
#'   distribution. One of the parametric forms `"exponential"`, `"weibull"`
#'   (default), `"gompertz"` (proportional hazards), `"exponential-aft"`,
#'   `"weibull-aft"`, `"lognormal"`, `"loglogistic"`, `"gamma"`, `"gengamma"`
#'   (accelerated failure time), or the flexible-baseline forms `"mspline"`
#'   and `"pexp"` (piecewise exponential). Must be `NULL` for other families.
#'   Note: `"gengamma"` is the generalized gamma restricted to positive Lawless
#'   shape `Q` (`Q = 1 / sqrt(aux2) > 0`), which nests the Weibull, gamma, and
#'   (as the limit) log-normal; it does not represent negative-`Q` shapes. Use a
#'   flexible `"mspline"` baseline if the data need a hazard shape outside the
#'   positive-`Q` family. `"gengamma"` is also the least numerically robust
#'   option: its likelihood uses Stan's regularized incomplete gamma function,
#'   whose gradient can fail to converge (`grad_reg_lower_inc_gamma: n
#'   (internal counter) exceeded 100000 iterations`). Isolated messages of that
#'   kind are rejected proposals and are harmless, but frequent ones, divergent
#'   transitions, or a chain that fails outright mean the fit should not be
#'   trusted. Always inspect the MCMC diagnostics reported by `summary()` on a
#'   `gengamma` fit, and prefer `"weibull"`, `"gamma"`, `"lognormal"`, or
#'   `"mspline"` when they fit comparably.
#'   Note: `"gompertz"` has a positive shape only (the shape carries a
#'   `<lower=0>` constraint, so the hazard `exp(eta + shape * t)` is
#'   monotonically increasing). Decreasing-hazard Gompertz (negative shape),
#'   available in some survival software, is not supported; use `"mspline"` /
#'   `"pexp"` for a decreasing or non-monotone baseline hazard.
#' @param prior_aux For `family = "survival"` parametric distributions: prior
#'   for the shape/scale parameter(s) (half-normal/half-t/exponential via the
#'   `<lower=0>` constraint). Default [default_prior_aux()].
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
#'   `aux_by`. `".study"` (the default) gives each study its **own** baseline
#'   shape, so the M-spline coefficients (or the parametric shape parameters)
#'   are estimated separately for the index and comparator studies. This matches
#'   `multinma`, where `.study` is always part of the stratification, and it is
#'   the right default: two single-arm trials rarely share a hazard shape, and
#'   assuming they do imposes proportional hazards *across studies*, which no
#'   randomization supports.
#'
#'   `NULL` is accepted and means the same as `".study"`, matching multinma,
#'   where a `NULL` `aux_by` is resolved to `".study"` and `.study` is always
#'   part of the stratification.
#'
#'   `"none"` gives both studies **one** shared shape. multinma has no spelling
#'   for this because it cannot do it; in an unanchored comparison it is a
#'   stronger assumption that buys precision, so it is worth fitting as a
#'   sensitivity analysis when the two Kaplan-Meier curves plainly have the same
#'   shape, but it should be a deliberate choice rather than a default.
#'
#'   **What stratifying assumes, and what it cannot test.** The parity with
#'   `multinma` is a parity of spelling, not of meaning. In an anchored
#'   randomized network each study contributes several arms, so a study-specific
#'   baseline shape is a nuisance parameter and within-study randomization still
#'   identifies the treatment effect. Here each study contributes exactly **one**
#'   arm, so a study-specific baseline shape and a treatment-specific baseline
#'   shape are perfectly aliased: nothing in the data can separate them. Under
#'   `".study"` the fitted shape therefore travels with the treatment when the
#'   effect is transported, which is an additional structural assumption the
#'   data cannot check, not merely the unanchored analogue of stratifying by
#'   study. `"none"` makes the opposite assumption, that the shape belongs to
#'   the disease rather than to the arm, and that one is at least testable
#'   against the two observed curves. Neither is assumption-free; fit both and
#'   report the difference.
#'
#'   With the stratified default the marginal hazard ratio varies with time, so
#'   the scalar `delta_*` reported by [marginal_effects()] is its value at one
#'   time, not a constant; pass `at_time` to choose which. This applies only
#'   where the shapes genuinely differ: the exponential has no shape, so
#'   `aux_by` leaves its closed-form contrast exact. The collapsible RMST
#'   difference does not have this problem and is the better headline estimand.
#'
#'   Identification differs by baseline. For `"mspline"` / `"pexp"` each stratum
#'   gets **its own knots over its own observed support** (as in multinma's
#'   default `type = "quantile"`), and its coefficients are a simplex, which
#'   pins that study's cumulative hazard to 1 at a boundary the study actually
#'   observed. Both parts matter. A single pooled basis spanning the longest
#'   study would leave the shorter study with basis functions it never observes;
#'   scaling its observed coefficients by `c`, moving the surplus simplex mass
#'   into an unobserved column, and replacing its intercept by `mu - log(c)`
#'   would then leave the likelihood exactly unchanged, so the intercept would
#'   be set by the prior rather than by data. Per-study boundaries remove that
#'   flat direction. For parametric baselines there is no such normalization and
#'   none is needed, because shape and scale enter the hazard as different
#'   functions of time; but the comparator shape is then informed only by the
#'   reconstructed comparator curve, so stratifying spends information that a
#'   short or heavily censored aggregate curve may not have. The exponential has
#'   no shape at all, so `aux_by` does not change it.
#'
#'   Reach for `"none"` only when the two arms' Kaplan-Meier curves plainly have
#'   the same shape, and report it as a sensitivity analysis rather than as the
#'   primary result: one shared shape is the stronger assumption and buys
#'   precision, but nothing in an unanchored design justifies it.
#'
#'   **An assumption worth naming.** When a stratified fit predicts the index
#'   treatment in the comparator population, it carries the *index study's*
#'   baseline shape with it, and vice versa. That is coherent only if the
#'   residual time pattern is a property of the treatment that travels across
#'   populations. In an anchored `multinma` network a study-stratified baseline
#'   is a study nuisance, not something attached to a treatment; here each study
#'   contributes exactly one arm, so the data cannot separate a
#'   treatment-specific hazard shape from a study, design, or calendar-time
#'   shape. Stratifying is the safer default for the *contrast*, but absolute
#'   predictions transported across populations rest on this extra assumption.
#'   Where it is doubtful, prefer the RMST estimands, compare against
#'   `aux_by = "none"`, and say which was used.
#' @param mspline_degree For `family = "survival"` flexible baselines: spline
#'   degree override (default derived from `distribution`: 3 for `"mspline"`,
#'   0 for `"pexp"`).
#' @param pred_times For `family = "survival"`: times at which survival,
#'   hazard and cumulative-hazard predictions are produced. If `NULL`, a grid
#'   up to the maximum observed time is used.
#' @param rmst_horizon For `family = "survival"`: the upper time limit for the
#'   restricted mean survival time. If `NULL`, the maximum observed time, except
#'   for a flexible baseline (`"mspline"` / `"pexp"`) stratified by study, where
#'   it defaults to the COMMON follow-up
#'   `min(max(index times), max(comparator times))`. Each study's flexible
#'   baseline is extrapolated as a constant hazard past its own last observed
#'   time, so a pooled-maximum default would make the headline RMST extrapolate
#'   the shorter study by construction. Pass a longer horizon explicitly to
#'   accept that extrapolation; doing so still warns.
#' @param n_rmst_grid For `family = "survival"`: number of equally spaced nodes
#'   (default `100`) on `[0, rmst_horizon]` for the trapezoidal RMST integral.
#'   Increase for sharp early hazards, long horizons, or high-curvature
#'   flexible-baseline tails where 100 points may be too coarse; refit at a
#'   higher value and compare RMST to check convergence.
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

  # Survival distribution + auxiliary/smoothing priors
  surv_info <- NULL
  if (family == "survival") {
    surv_info <- .survival_distribution_info(distribution)
    if (!is.null(knots) && surv_info$kind != "flexible") {
      stop("`knots` can only be supplied with `distribution = \"mspline\"` ",
           "or `distribution = \"pexp\"`.", call. = FALSE)
    }
    if (!is.null(mspline_degree)) {
      # `distribution` names the baseline shape, and the degree defines it:
      # "pexp" IS degree 0 and "mspline" IS degree 3. Silently honoring a
      # contradicting override would fit one model and report the other, so a
      # mismatch is rejected rather than resolved in either direction.
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
    prior_smooth <- prior_smooth %||% default_prior_smooth()
    validate_prior(prior_aux, "prior_aux")
    validate_prior(prior_smooth, "prior_smooth")
    .validate_survival_controls(pred_times, rmst_horizon, mspline_degree,
                                n_knots, n_rmst_grid,
                                distribution = distribution)
    invisible(.resolve_aux_strata(aux_by))   # fail on a bad value before fitting
    .validate_survival_studies(data, aux_by)
  } else {
    if (!is.null(knots)) {
      stop("`knots` is only used for flexible survival models.", call. = FALSE)
    }
    if (!is.null(distribution)) {
      stop("`distribution` is only used for family = 'survival'.", call. = FALSE)
    }
    # `aux_by` now defaults to ".study", so a non-NULL value is not evidence the
    # user asked for it. Only object when they supplied it explicitly.
    if (!missing(aux_by)) {
      stop("`aux_by` is only used for family = 'survival': it stratifies the ",
           "baseline hazard, which the other families do not have.",
           call. = FALSE)
    }
    if (!is.null(prior_aux) || !is.null(prior_smooth)) {
      warning("`prior_aux` / `prior_smooth` are ignored for non-survival families.",
              call. = FALSE)
    }
  }

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

  # Weak identifiability of beta_comparator in relaxed models. It is informed
  # only by the aggregate likelihood, so the note differs by what that
  # likelihood actually is.
  if (model == "relaxed") {
    n_cov_check <- data$n_covariates
    if (family == "normal" && link_info$link == "identity") {
      n_agd_rows_check <- nrow(data$agd$data)
      agd_rank <- .agd_covariate_rank(data)
      if (agd_rank < n_cov_check + 1L) {
        warning(sprintf(
          paste0("Relaxed model with %d AgD row(s), aggregate design rank %d, ",
                 "and %d covariate(s): the identity-link ",
                 "aggregate design needs at least %d independent profiles, so ",
                 "some comparator-parameter combinations are not separated by ",
                 "the likelihood. The most ",
                 "effective fix is to supply the comparator as jointly-defined ",
                 "subgroup rows, one set_agd() row per stratum with its own ",
                 "covariate summaries. Otherwise regularize with ",
                 "an informative `prior_beta` or use model = \"spfa\"."),
          n_agd_rows_check, agd_rank, n_cov_check, n_cov_check + 1L
        ), call. = FALSE)
      }
    } else if (family == "survival") {
      # No count-based warning here, for the reason check_identification()
      # refuses the family outright: the premise that each aggregate row
      # contributes one scalar constraint does not hold for a reconstructed
      # curve. The comparator likelihood is the pseudo-IPD event and censoring
      # times, one term each, so a single arm row can carry several hundred
      # constraints. Counting the arm-summary rows would have reported a
      # 300-patient reconstructed curve as one summary and told the user that
      # two covariate coefficients "cannot all be separated", which is false.
      # What can go wrong here is a genuinely uninformative curve, and a row
      # count cannot see that either way; prior_sensitivity() can.
      NULL
    } else {
      n_agd_rows_check <- nrow(data$agd$data)
      # Not the row count. Mean-profile rank is not available here either,
      # because a nonlinear mean depends on each row's whole covariate
      # distribution and two rows with equal means but different spreads do
      # carry different constraints. Rows built from an identical integration
      # grid are a different matter: they are the identical function of the
      # comparator parameters whatever the link, so the second repeats the
      # first's likelihood term. Counting distinct grids is the bound the raw
      # row count is not, and it is what stops a duplicated set_agd() row from
      # suppressing this warning for the nonlinear families as well.
      n_distinct_check <- .agd_distinct_profiles(data)
      if (n_distinct_check < n_cov_check + 1L) {
        warning(sprintf(
          paste0("Relaxed model with %d AgD row(s), %d of them distinct, and ",
                 "%d covariate(s): the comparator side has %d parameters but ",
                 "only %d independent scalar aggregate outcome summaries. A ",
                 "row repeating another's integration grid contributes an ",
                 "identical likelihood term rather than a new constraint. The ",
                 "comparator coefficients cannot all be separated without ",
                 "prior information. Add jointly-defined subgroup rows, ",
                 "regularize with an informative `prior_beta`, or use ",
                 "model = \"spfa\"."),
          n_agd_rows_check, n_distinct_check, n_cov_check, n_cov_check + 1L,
          n_distinct_check
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
    prior_sigma = prior_sigma,
    surv_info = surv_info,
    prior_aux = prior_aux,
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

  # Select Stan model (family_config is the single source of truth)
  stan_prefix <- if (family == "survival") {
    surv_info$stan_prefix
  } else {
    get_family_config(family)$stan_prefix
  }
  model_name <- paste0(stan_prefix, "_", model)

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
    sd_x = prepared$sd_x,
    prior_aux = prior_aux,
    prior_smooth = prior_smooth,
    surv_info = surv_info
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
    # What was fitted, kept on the fit rather than passed to the sampler.
    # These describe the model, so they belong here: prior_sensitivity() reads
    # `distribution` and `surv_controls` to reproduce a fit, and predict() reads
    # `pred_times`. They were previously handed to .mlumr_fit_backend(), whose
    # signature ends in `...`, so instead of being stored they were forwarded
    # to the sampler: rstan::sampling() rejects unknown argument names outright
    # and CmdStanModel$sample() has no `...` to absorb them, which meant no
    # survival model could be fitted by either engine.
    distribution = if (!is.null(surv_info)) surv_info$distribution else NULL,
    surv_info = surv_info,
    pred_times = stan_data$pred_times,
    # Baseline controls needed to reproduce this fit. Survival-only entries are
    # NULL for other families, because mlumr() rejects them there and replaying
    # a stored formal default into a refit would make it error.
    surv_controls = list(
      n_knots = n_knots,
      knots = if (!is.null(surv_info) && surv_info$kind == "flexible") knots else NULL,
      aux_by = if (family == "survival") aux_by else NULL,
      mspline_degree = if (!is.null(surv_info)) surv_info$mspline_degree else NULL,
      pred_times = stan_data$pred_times,
      rmst_horizon = if (family == "survival")
        max(stan_data$rmst_grid_times) else NULL,
      n_rmst_grid = if (family == "survival")
        length(stan_data$rmst_grid_times) else NULL
    ),
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
                                   surv_info = NULL, prior_aux = NULL,
                                   prior_smooth = NULL, n_knots = 7L,
                                   knots = NULL,
                                   pred_times = NULL, rmst_horizon = NULL,
                                   n_rmst_grid = 100L, aux_by = NULL,
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
  } else if (family == "poisson") {
    stan_data$y_ipd <- as.integer(ipd_data$.outcome)
    stan_data$E_ipd <- as.numeric(ipd_data$.exposure)
    stan_data$r_agd <- array(as.integer(agd_data$.r))
    stan_data$E_agd <- array(as.numeric(agd_data$.E))
  } else {
    stan_data <- .build_stan_data_survival(
      stan_data = stan_data, data = data, surv_info = surv_info,
      pred_times = pred_times, n_knots = n_knots,
      knots = knots,
      rmst_horizon = rmst_horizon, n_rmst_grid = n_rmst_grid,
      prior_aux = prior_aux, prior_smooth = prior_smooth,
      n_strata = .resolve_aux_strata(aux_by)
    )
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
#' @keywords internal
.agd_center_weights <- function(stan_data, family, n_agd_rows) {
  fallback <- rep(1, n_agd_rows)
  cfg <- tryCatch(get_family_config(family), error = function(e) NULL)
  field <- if (is.null(cfg)) NULL else cfg$comp_weight_field
  w <- if (!is.null(field)) stan_data[[field]] else NULL
  # Survival has no comparator weight field; the pseudo-IPD count is the
  # equivalent population size. `stan_data$n_agd` is the TOTAL number of
  # pseudo-individuals, so falling through to the equal-weight fallback would
  # give every AgD row the same weight regardless of how many pseudo-individuals
  # it actually contributes. That is exactly the tabulation dependence this
  # function exists to remove: two comparator arms of 300 and 60 would be
  # centered as if they were 180 each. `agd_arm` maps each pseudo-individual to
  # its row, so tabulating it recovers the true per-row population.
  if (is.null(w) && identical(family, "survival")) {
    arm <- stan_data$agd_arm
    w <- if (!is.null(arm) && n_agd_rows >= 1L) {
      tabulate(as.integer(arm), nbins = n_agd_rows)
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

#' Rank of the AgD comparator covariate design
#'
#' The relaxed model's `mu_comparator` and `beta_comparator` are informed only
#' by the AgD likelihood, which contributes one term per AgD row evaluated at
#' that row's integration grid. The number of comparator parameters those terms
#' can separate is therefore the rank of the per-row mean covariate profiles
#' augmented with an intercept column, NOT the number of rows: rows that repeat
#' the same covariate summaries add likelihood terms but no new direction.
#'
#' Uses the declared aggregate covariate means, which define the identity-link
#' design exactly and do not vary with integration resolution.
#'
#' @param data An `mlumr_data` object with integration points.
#' @return Integer rank, at least 1.
#' @keywords internal
.agd_covariate_rank <- function(data) {
  .profile_rank(.agd_mean_profiles(data))
}

#' Map `aux_by` onto the Stan `n_strata` switch
#'
#' There are only ever two studies in an unanchored comparison, so `".study"`
#' means 2 and no stratification means 1. Named after multinma's argument so the
#' concept transfers, but deliberately not accepting `".trt"`: each study
#' contributes a single arm here, so stratifying by treatment and by study are
#' the same thing.
#' @param aux_by `NULL`, `".study"`, or `"none"`.
#' @return Integer number of baseline strata (1 or 2).
#' @keywords internal
.resolve_aux_strata <- function(aux_by) {
  # NULL means ".study", NOT "share". This follows multinma exactly
  # (multinma/R/nma.R: `if (quo_is_null(aux_by)) aux_by <- ".study"`, and
  # get_aux_id(add_study = TRUE) forces .study into the grouping regardless), so
  # a multinma user writing aux_by = NULL gets the model they expect rather than
  # its opposite. Sharing one baseline across studies has no multinma spelling
  # because multinma cannot do it, so it gets its own explicit name here.
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
#' @keywords internal
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
#' @keywords internal
.build_stan_data_survival <- function(stan_data, data, surv_info, pred_times,
                                      n_knots, knots = NULL, rmst_horizon,
                                      n_rmst_grid = 100L,
                                      prior_aux, prior_smooth, n_strata = 1L) {
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd
  arm_summary <- data$agd$data
  # 1 = one baseline shared by both studies; 2 = one per study (see `aux_by`).
  stan_data$n_strata <- as.integer(n_strata)

  # Covariate centering is now applied for every family by the shared
  # .mlumr_center_covariates() step in .mlumr_build_stan_data() (matching
  # `center = TRUE` default), so it is no longer done here.

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

  # A flexible baseline is extrapolated as a constant hazard past each study's
  # own upper boundary knot, so with unequal follow-up a pooled-maximum default
  # horizon would make the HEADLINE RMST extrapolate the shorter study by
  # construction. A warning is not enough for a default estimand: default
  # instead to the common empirical support, and let a longer horizon be an
  # explicit choice (which still warns).
  horizon <- rmst_horizon
  if (is.null(horizon)) {
    horizon <- if (surv_info$kind == "flexible" && n_strata > 1L) {
      min(max(ipd$.time), max(pseudo$.time))
    } else {
      max_time
    }
  }
  # RMST is the trapezoidal integral of the survival curve on this grid; more
  # points give a finer (more accurate) approximation. Default 100 is adequate
  # for smooth curves; raise `n_rmst_grid` for sharp early hazards, long
  # horizons, or high-curvature flexible-baseline tails.
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
    # The second generalized-gamma shape reuses the prior_aux specification.
    stan_data$prior_aux2_location <- aux_fields$mean
    stan_data$prior_aux2_scale <- aux_fields$sd
    stan_data$prior_aux2_dist <- aux_fields$dist
    stan_data$prior_aux2_df <- aux_fields$df
  } else {
    # One basis per baseline stratum. With `aux_by = ".study"` each study gets
    # its own boundary and internal knots over its OWN observed support, which
    # is what multinma's default `type = "quantile"` does. This is an
    # identification requirement, not a nicety: with a single pooled basis whose
    # upper boundary is the longest study's last time, a shorter study can have
    # basis functions with no support over its observed period. Scaling that
    # study's observed coefficients by c, moving the surplus simplex mass into
    # an unsupported column, and replacing mu by mu - log(c) leaves both
    # h0(t)exp(eta) and H0(t)exp(eta) unchanged at every observed time, so the
    # likelihood is exactly flat along that direction and the intercept is set
    # by the prior rather than the data. Per-study boundaries remove it: every
    # column is supported and H0_s is pinned at a time the study actually
    # observed.
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
        .assert_basis_support(spec_idx, max(ipd$.time), "index")
        .assert_basis_support(spec_cmp, max(pseudo$.time), "comparator")
      }
    } else {
      knots_i <- if (is.null(knots)) {
        make_knots(data, n_knots = n_knots)
      } else {
        .validate_user_knots(knots, max(c(ipd$.time, pseudo$.time)), "shared")
      }
      spec_idx <- spec_cmp <- .build_mspline_basis(knots_i, degree)
    }
    spec <- spec_idx
    if (spec$n_scoef < 2L) {
      # Only reachable at degree 0 (`distribution = "pexp"`), where n_scoef is
      # the number of intervals: with no internal knot that is a single constant
      # hazard, and the random-walk smoothing prior has no increment to be
      # defined on. A cubic M-spline with no internal knots still has
      # degree + 1 = 4 coefficients and is fine.
      stop("Baseline basis has < 2 coefficients, so the random-walk smoothing ",
           "prior has no increments to smooth. A degree-", spec$degree,
           " basis needs at least one internal knot for that; use ",
           "`n_knots >= 1`, or `distribution = \"exponential\"` if a single ",
           "constant hazard is what you want. Got n_scoef = ", spec$n_scoef,
           ".", call. = FALSE)
    }
    # The M-spline baseline is extrapolated with a constant hazard past the
    # upper boundary knot. Warn if predictions/RMST extend past follow-up.
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
    # The IPD is entirely the index study and the pseudo-IPD entirely the
    # comparator, so each likelihood block already uses only its own stratum's
    # basis. The prediction/RMST grids are evaluated on both.
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
    # The constant-hazard centering and RW1 weights are properties of a basis,
    # so they are per-stratum once the bases differ.
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
#' @keywords internal
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

#' Validate survival-specific prediction grids and spline controls
#' @keywords internal
.validate_survival_controls <- function(pred_times, rmst_horizon,
                                        mspline_degree, n_knots,
                                        n_rmst_grid = 100L,
                                        distribution = NULL) {
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
  valid_knots <- is.numeric(n_knots) && length(n_knots) == 1L &&
    is.finite(n_knots) && n_knots >= 0 && n_knots == floor(n_knots) &&
    n_knots <= 50
  if (!valid_knots) {
    stop("`n_knots` must be an integer in [0, 50]. (Flexible baselines rarely ",
         "need more than a handful of internal knots; the cap also blocks ",
         "accidental enormous allocations.)", call. = FALSE)
  }
  # `n_knots = 0` is in range for make_knots(), which is also called directly.
  # Whether it yields a fittable baseline depends on the DEGREE, because
  # `n_scoef = length(internal) + degree + 1`:
  #   pexp    (degree 0) -> 0 + 0 + 1 = 1 coefficient. The random-walk smoothing
  #                         prior is defined on the DIFFERENCES between
  #                         coefficients, so one coefficient has no increments
  #                         and the model cannot be built.
  #   mspline (degree 3) -> 0 + 3 + 1 = 4 coefficients, with three RW1
  #                         increments. That is a perfectly ordinary single-
  #                         interval cubic M-spline and must NOT be rejected.
  # Reject only the case that genuinely cannot work, and leave the general
  # `n_scoef < 2` check to catch anything else.
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

#' Resolve the sampling seed
#'
#' Precedence: an explicit `seed` argument; else a seed drawn from R's RNG when
#' the user has set it (e.g. via set.seed()); else a fixed default of 2026.
#' Returns `list(value, source)` and warns when it falls back to the default, so
#' an unseeded run is still reproducible.
#' @keywords internal

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
                                  prior_sigma, beta_fields, sd_x,
                                  prior_aux = NULL, prior_smooth = NULL,
                                  surv_info = NULL) {
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

  # Surface the survival baseline priors only where they are consumed.
  if (identical(family, "survival")) {
    if (!is.null(surv_info) && surv_info$n_aux > 0) {
      priors$aux <- prior_aux
    }
    if (!is.null(surv_info) && surv_info$kind == "flexible") {
      priors$smooth <- prior_smooth
    }
  }

  if (family == "normal") {
    priors$sigma <- prior_sigma
  }

  priors
}
