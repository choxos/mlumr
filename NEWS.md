# mlumr 0.1.0.9000 (development version)

## Time-to-event outcomes

* Feature: New `"survival"` outcome family. `set_ipd()` accepts time-to-event
  data with right, left, interval and delayed-entry censoring through a
  `survival::Surv()` object or `time`/`status`/`entry_time` columns, and
  `set_agd_surv()` takes the comparator arm as reconstructed pseudo-IPD plus
  its covariate moments. `outcome` is not required for this family.
* Feature: Parametric and flexible baselines through the `distribution`
  argument of `mlumr()`: proportional hazards (`"exponential"`, `"weibull"`,
  `"gompertz"`), accelerated failure time (`"exponential-aft"`,
  `"weibull-aft"`, `"lognormal"`, `"loglogistic"`, `"gamma"`, `"gengamma"`)
  and flexible hazards (`"mspline"`, `"pexp"`) with a random-walk smoothing
  prior. `make_knots()` places spline knots; `knots` accepts a custom set.
* Feature: The baseline hazard is estimated per study by default
  (`aux_by = ".study"`), each stratum with its own knots over its own
  observed support, as `multinma::nma()` does. `aux_by = "none"` shares one
  baseline across studies.
* Feature: New priors `prior_aux`, `prior_aux2` (second generalized-gamma
  auxiliary) and `prior_smooth`, with `default_prior_aux()` and
  `default_prior_smooth()`.
* Feature: `predict()` on a survival fit returns `"survival"`, `"hazard"`,
  `"cumhaz"`, `"rmst"`, `"median"` and `"loghr"` curves. `times` picks
  evaluation times (snapped to the fitted grid, with a message when they
  move), `pred_times` sets the grid, `"median"` carries `p_not_reached`, and
  RMST rows carry their `horizon`. `conditional_effects()` and
  `conditional_predict()` give covariate-conditional versions.
* Feature: `marginal_effects()` reports survival effects on the natural scale:
  the hazard ratio (`HR`) for proportional-hazards fits, the time ratio (`TR`)
  for shared-shape AFT fits, the exponentiated contrast (`EXP_DELTA_ETA`)
  otherwise, plus the RMST difference (`RMSTD`) and ratio (`RMSTR`). Marginal
  hazard ratios carry an `at_time` column because they are time-varying and
  non-collapsible; `predict(type = "loghr")` gives the whole curve.
* Improvement: An effect a fit cannot supply is an error naming the
  alternative, not a substitution: `effect = "tr"` on a proportional-hazards
  fit, `effect = "hr"` on an AFT fit, and either on a relaxed or
  study-specific-shape AFT fit.
* Feature: Survival benchmarks. `naive()` returns the unadjusted Cox log
  hazard ratio and refuses a partial likelihood with no maximum or no
  convergence. `stc()` returns the RMST difference by parametric
  G-computation with `flexsurv`, with a bootstrap interval (`n_boot`,
  default 200) and an `rmst_horizon` argument.
* Feature: `survival_unit` on `calculate_loo()`, `calculate_waic()` and
  `compare_models()` chooses what one held-out unit is for the reconstructed
  comparator: `"observation"` (default, optimistic), `"arm"` or
  `"aggregate"`.
* Improvement: Coefficient rows in `summary()` are labeled by covariate name
  (`beta[age]`, `beta_index[age]`, `beta_comparator[age]`); the stored
  `variable` strings are unchanged.
* Fix: Interval- and left-censored likelihoods under delayed entry are
  evaluated in whichever of the difference, increment or quadrature forms
  keeps its digits, and the exponential, Weibull, Gompertz and log-logistic
  use closed forms. See `?mlumr`.
* Fix: `mlumr()` refuses an M-spline basis column that no subject was at risk
  under, judged over the merged `[entry, exit]` intervals of each study, and
  refuses a shared baseline (`aux_by = "none"`) whose studies never overlap
  on a spline column. See `?mlumr`.
* Improvement: `predict(type = "rmst")` and `predict(type = "median")` warn
  when the prediction grid is too coarse to trust and point at a finer
  `n_rmst_grid` or `pred_times`.
* Improvement: `?set_agd_surv` states that reconstruction uncertainty is not
  propagated and which population the covariate moments must describe under
  delayed entry.

## Transport to any target population

* Feature: A `newdata` argument on `marginal_effects()` and `predict()`
  standardizes effects and absolute predictions to an arbitrary target
  population by Bayesian g-computation, for every family. Standardizing to the
  index covariates reproduces `population = "index"` exactly. Survival RMST
  effects and absolute predictions transport; the marginal hazard ratio is
  reported for a target too, with its `at_time`.
* Improvement: `marginal_effects()` on a relaxed fit queried for the index
  population prints a one-line note with the marginal posterior variance
  change of `beta_comparator` per covariate, descriptive only. Silence it
  with `options(mlumr.quiet_relaxed_index = TRUE)`.

## Plotting

* Feature: `plot()` methods for `marginal_effects()` (forest),
  `predict()` (curves with credible bands, or point-intervals for scalar
  types) and `conditional_effects()` (effects by profile). Each forest draws
  the null its measure implies, uses a log axis for ratio measures, and reads
  its interval level from the result.
* Feature: `geom_km()` overlays the observed Kaplan-Meier curves on a model
  survival plot, colored by treatment, honoring delayed entry, one panel per
  population. It draws right-censored data only.
* Feature: `plot_prior_posterior()` overlays each named parameter's posterior
  on the prior the fit recorded for it.
* Feature: `mlumr_forest()` draws a forest from a plain data frame, for mixed
  comparisons such as `naive()`, `stc()` and both ML-UMR models on one axis;
  a single very wide interval is clipped with an arrow instead of squeezing
  the rest.
* Improvement: `marginal_effects()`, `predict()` and `conditional_effects()`
  return lightweight `data.frame` subclasses so the methods dispatch; data
  frame behavior is unchanged.

## Identification of the relaxed comparator coefficients

* Feature: `check_identification()` reports, before fitting, whether the
  aggregate subgroup rows can identify `beta_comparator` in
  `model = "relaxed"`: the row count against `K + 1`, and the balance and
  spread of the centered subgroup-mean matrix, with a descriptive-only label
  for nonlinear links.
* Feature: `prior_beta_comparator` argument to `mlumr()` sets a separate
  prior on `beta_comparator`, including a different family. Defaults to
  `prior_beta`; ignored for `model = "spfa"`; reported by `prior_summary()`
  and swept by `prior_sensitivity()`.
* Fix: The weak-identifiability warning no longer counts a duplicated
  `set_agd()` row as evidence; it triggers on the rank of the mean design
  under the identity link and on the number of distinct integration grids
  otherwise.

## Data preparation and integration

* Fix: `distr()` honors arguments passed by position. `distr(qnorm, 10, 2)`
  previously integrated a standard normal without any message. Positional
  and abbreviated arguments are now matched against the quantile function's
  formals at construction, and an argument matching nothing is an error.
* Feature: Moment-parameterized `qgamma()`/`pgamma()`/`dgamma()` and
  `qlogitnorm()`/`plogitnorm()`/`dlogitnorm()` accept `mean` and `sd`, so
  `distr(qgamma, mean = age_mean, sd = age_sd)` works as printed in a
  baseline table. Without those arguments they forward to `stats`. See
  `?GammaDist` and `?logitNormal`.
* Fix: `set_agd()` no longer rejects a valid binary covariate SD. The bound
  was the population SD `sqrt(p (1 - p))`; it is now the finite-sample
  maximum with a rounding allowance. See `?set_agd`.
* Improvement: `set_agd()` requires `outcome_n` when normal aggregate data
  have more than one row, and the multi-row comparator estimand for the
  normal family is the sample-size-weighted mean of the strata rather than
  an inverse-variance average. `naive()` and `stc()` use the same weights.
* Improvement: `set_ipd()` warns about a rank-deficient or nearly collinear
  covariate design and names the near-redundant covariates. Autoscaling of a
  covariate with no usable empirical scale falls back to the unscaled prior
  with a warning.
* Fix: `add_integration()` rejects `cor_adjust = "pearson"` with
  non-Gaussian margins, warns about a nonbinary discrete margin (count or
  ordered category) that the copula correction does not cover, and warns when
  a supplied `distr()` grossly contradicts the declared `set_agd()` moments.
* Improvement: `check_integration()` compares realized and target correlations
  on the same method (Pearson or Spearman), reports fidelity of each grid
  margin to the declared mean and SD, lists which correlation pairs it could
  and could not measure (`correlation_pairs`, with `"partial"` and `"review"`
  verdicts), withholds the comparison under `cor_adjust = "none"`, and reports
  a comparison it could not make as `"unavailable"` instead of `"close"`.
* Improvement: Setup functions record an internal `.source_key` per row so
  `compare_models()` can tell two fits were built from one source reordered
  between them. `.source_key` is now a reserved column name.

## Priors and sensitivity

* Fix: `prior_normal(autoscale = TRUE)` rescales the prior location as well
  as the scale, so a nonzero prior mean is on the covariate's own scale.
* Improvement: `prior_sensitivity()` varies the prior and nothing else: every
  model-defining setting is replayed from the fit and `...` may not override
  it. Relaxed fits sweep the comparator prior alongside the index one
  (`prior_beta_comparator_scales`, reported in `scale_comparator`), survival
  rows carry `effect` and `at_time`, and a refit that cannot replay a sampler
  argument the original fit used (such as `thin` or `init`) warns and names
  it.
* Breaking: `prior_sensitivity()` quantile columns are named `q2.5`, `q50`,
  `q97.5`, matching `marginal_effects()`, rather than `q2` and `q98`. Duplicate
  `probs` are refused.
* Improvement: `prior_sensitivity()` no longer claims a scale sweep proves the
  inference is data-driven; its interpretation text points at
  `check_identification()`.
* Fix: `prior_summary()` names constrained priors correctly: a truncated
  normal or t with a nonzero location is not a half-normal or half-t, and an
  exponential is not truncated at all.

## Benchmarks: `naive()` and `stc()`

* Breaking: `stc()` no longer reports an index-population contrast. That
  estimate assumed a constant link-scale effect, which population adjustment
  exists to avoid. `stc()` returns the comparator-population estimand only;
  use `mlumr()` when the index population is the target.
* Breaking: A normal-family `stc()` under a log link reports the log mean
  ratio in `$estimate` and the mean difference in `$md`, `$md_se`,
  `$md_lower`, `$md_upper`. Under the identity link the two coincide.
* Fix: `stc()` refuses a binomial outcome model with complete separation and,
  when `detectseparation` is installed, tests exactly for quasi-complete
  separation; the result records a `separation` status. A Poisson model with
  no events is refused the same way. See `?stc`.
* Fix: Directly observed arm proportions and rates in `naive()`, and the
  observed comparator proportion in a binomial `stc()`, use exact
  Clopper-Pearson and Garwood intervals instead of Wald intervals, which
  excluded the truth at zero counts. Contrasts remain Wald.
* Fix: `naive()` combines several aggregate rows as strata; the comparator
  standard error is that of the size-weighted mean of the row proportions.
* Fix: `naive()` and `stc()` apply a continuity correction only to an
  observed proportion of 0 or 1 instead of clamping every proportion, which
  slightly changes the reported effect for arms with no events or no
  non-events.

## Fitting, diagnostics and model comparison

* Breaking: `mlumr()` gains model-defining arguments ahead of `chains`,
  `iter` and the other sampler controls. Pass sampler settings by name.
* Improvement: `mlumr()` centers covariates by default (`center = TRUE`),
  which removes the intercept-versus-slope collinearity that pushed the
  sampler into deep trajectories on raw-scale covariates. Reported contrasts
  are unchanged; `prior_intercept` then applies at the pooled covariate mean.
  `qr = TRUE` offers a QR-rotated design instead.
* Improvement: The binary, continuous and count IPD likelihoods use Stan's
  fused GLM densities on their canonical links; results agree with 0.1.0 to
  Monte Carlo error.
* Fix: `mlumr()` refuses a normal fit whose outcome is constant or reproduced
  exactly by its covariates, where the posterior for the residual SD is
  improper, and warns when the design is saturated.
* Fix: Marginal probabilities, means, rates and their contrasts are formed on
  the log scale; the `safe_logit()` and `safe_divide()` clamps are gone, so
  ratios with a near-zero comparator are no longer understated and `lor_*`
  is finite where it used to hit the clamp. A ratio beyond double precision
  is `Inf`. See `?mlumr-numerical-evaluation`.
* Breaking: `predict(type = "link")` returns the marginal link
  `g(E[g^-1(eta)])` rather than the mean linear predictor `E[eta]`; the two
  agree for the identity link only. This differs from `multinma` on purpose,
  since every effect mlumr reports is population-standardized.
* Improvement: `seed = NULL` uses the documented default of 2026 with a
  warning instead of drawing an unrecorded seed from the session RNG.
* Improvement: `verbose = FALSE` silences the cmdstanr sampler banner as well.
* Fix: A caller's rstan `control` list is merged with the backend's instead
  of failing argument matching.
* Fix: A cmdstanr run that produced no draws reports how many chains wrote
  nothing and where CmdStan's messages are, instead of failing inside
  `checkmate` on a temporary path.
* Fix: The cmdstanr executable cache key now covers every `#include`, the
  CmdStan version and its build flags; a changed include no longer reuses a
  stale executable. Expect one recompile after upgrading.
* Fix: `check_diagnostics()` and the fit summary no longer drop an infinite
  Rhat before taking the maximum, report missing or non-numeric diagnostics
  as unknown rather than as zero, and compute tail ESS.
* Improvement: Posterior summaries from `predict()`, `marginal_effects()`,
  `conditional_effects()` and `conditional_predict()` carry `n_draws` and
  `n_draws_used`, with one warning per call when draws were dropped.
* Fix: `conditional_predict()` names quantile columns from the requested
  probabilities, so non-round `probs` no longer return `NA`.
* Fix: `calculate_dic()`, `calculate_loo()`, `calculate_waic()` and
  `compare_models()` refuse a saved log-likelihood that does not cover every
  observation the fit was built from, and a cached `mlumr_dic` is checked the
  same way. `compare_models()` refuses fits built on different observations
  and warns when row order cannot be verified. See `?compare_models`.
* Fix: `calculate_loo()` refuses `moment_match = TRUE`, which `loo` ignores
  for a matrix, and both it and `calculate_waic()` refuse arguments the
  installed `loo` does not read.
* Improvement: The `compare_models()` printout no longer presents
  `se_diff > 2` as a decision rule.

## Example data and documentation

* Feature: Bundled datasets derived from published trials replace the
  invented data of 0.1.0: `psoriasis_ipd`/`psoriasis_agd` (binary),
  `shoulder_ipd`/`shoulder_agd` (continuous), `caries_ipd`/`caries_agd`
  (count) and `ndmm_ipd`/`ndmm_agd`/`ndmm_agd_covs` (survival). Provenance
  and known quirks are on each help page;
  `data-raw/prepare_multinma_subsets.R` rebuilds them.
* Improvement: Nine outcome-focused vignettes (`introduction`,
  `data-preparation`, `binary-outcomes`, `continuous-outcomes`,
  `count-outcomes`, `survival-outcomes`, `subgroup-identification`,
  `fitting-and-diagnostics`, `choosing-a-method`), precompiled through
  `R.rsp` so no Stan model runs at check time.
* Improvement: `?set_agd` states what an aggregate Poisson row assumes about
  exposure, and the `shoulder` and `caries` help pages describe their
  reference comparison as an estimate rather than a known truth.
* Feature: New hex-sticker logo with a broken-anchor motif.

## Dependencies

* `splines2` and `survival` added to Imports; `ggplot2` moved from Suggests
  to Imports (`>= 3.4.0`).
* `flexsurv`, `detectseparation`, `multinma`, `ggsurvfit` and `R.rsp` added
  to Suggests.
* `Additional_repositories` is pinned to `https://mc-stan.org/r-packages`
  so that `rstan` and `StanHeaders` resolve from one source. cmdstanr itself
  comes from `https://stan-dev.r-universe.dev`; `mlumr_engine("cmdstanr")`
  offers to install it from there.

# mlumr 0.1.0

Initial CRAN release.

* ML-UMR models for a disconnected two-study comparison: SPFA (shared
  covariate effects) and relaxed SPFA (treatment-specific coefficients), for
  binomial (logit, probit, cloglog), normal (identity, log) and Poisson (log)
  outcomes.
* rstan backend by default, optional cmdstanr through `mlumr_engine()` or the
  `engine` argument.
* `set_ipd()`, `set_agd()`, `combine_data()` and `add_integration()`
  (Sobol quasi-Monte Carlo points with a Gaussian copula), mirroring the
  `multinma` interface.
* Prior constructors `prior_normal()`, `prior_student_t()`, `prior_cauchy()`
  and `prior_exponential()`, per-coefficient priors, `autoscale`,
  `prior_summary()` and `prior_sensitivity()`.
* `predict()`, `marginal_effects()`, `conditional_effects()` and
  `conditional_predict()`.
* `calculate_dic()`, `calculate_loo()`, `calculate_waic()` and
  `compare_models()`.
* `check_diagnostics()` and `check_integration()`.
* Frequentist benchmarks `stc()` (parametric G-computation) and `naive()`.
