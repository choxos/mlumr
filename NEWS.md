# mlumr 0.2.0

Version 0.2.0 adds a time-to-event outcome family, standardization to a
user-supplied target population, a diagnostic for the identification of the
relaxed model's comparator coefficients, `plot()` methods for the
marginal-effect, prediction, and conditional-effect result objects, and
additional vignettes. It also moves the likelihoods and the reported summaries
onto the log scale throughout, which changes some numbers the 0.1.0 families
produced; those changes are listed under Behavior and validation changes to
existing functions, and none of them alters a function's arguments. Data
provenance for the worked examples is described under Example data below.

## Time-to-event (survival) outcomes

* **New `"survival"` outcome family** for unanchored indirect comparison of
  single-arm time-to-event studies, fitted with Stan. The comparator arm is
  supplied as reconstructed pseudo-IPD (event/censoring times digitized from a
  published Kaplan-Meier curve); its likelihood is integrated over the
  comparator covariate distribution, consistent with the package's binary /
  continuous / count families.

* **Parametric and flexible baselines** via the `distribution` argument to
  `mlumr()`:
    - Proportional hazards: `"exponential"`, `"weibull"` (default),
      `"gompertz"` (positive-shape, increasing-hazard parameterization).
    - Accelerated failure time: `"exponential-aft"`, `"weibull-aft"`,
      `"lognormal"`, `"loglogistic"`, `"gamma"`, `"gengamma"` (the positive-`Q`
      Lawless generalized gamma subfamily; negative-`Q` shapes are not covered).
    - Flexible baseline hazard: `"mspline"` (M-spline) and `"pexp"`
      (piecewise exponential), with a random-walk smoothing prior.

* **Censoring support** in `set_ipd()`: right, left, interval, and delayed
  entry (left truncation) via a `survival::Surv()` object. The
  `time`/`status`/`entry_time` column route covers right-censoring (status
  `0`/`1`) and optional delayed entry; supply a `Surv` object for left- or
  interval-censored data. The Bayesian `mlumr()` model uses all censoring types;
  the frequentist benchmarks are right-censored only, so `naive()` (Cox) and
  `geom_km()` reject left-/interval-censored data rather than silently collapse
  it.

* **New `set_agd_surv()`** for comparator pseudo-IPD plus covariate moments,
  and **`make_knots()`** for M-spline knot placement.

* **HTA prediction suite** from `predict()` on a survival fit:
  `type = "survival"`, `"hazard"`, `"cumhaz"`, `"rmst"` (restricted mean
  survival time), `"median"`, and `"loghr"` (the time-varying marginal log
  hazard ratio curve, null 0). `predict(type = "median")` carries a
  `p_not_reached` column reporting the posterior probability that the median is
  beyond follow-up (the other summaries are conditional on the median being
  reached), and interpolates from the known exact point `S(0) = 1` when the
  median falls before the first prediction time. `conditional_effects()` /
  `conditional_predict()` give covariate-conditional contrasts and survival
  curves. The RMST trapezoid resolution is a user control, via the
  `n_rmst_grid` argument to `mlumr()` (default 100).

* **Frequentist benchmarks for survival**: `naive()` returns an unadjusted Cox
  log hazard ratio, and `stc()` performs parametric G-computation of the RMST
  difference using the `flexsurv` package (a suggested dependency). `stc()`
  takes an `rmst_horizon` argument, since its own default is the pooled maximum
  observed time while a stratified flexible `mlumr()` baseline defaults to the
  follow-up both studies observed; the two are different estimands, so the
  horizon must be set explicitly for the results to be comparable. Note that
  `naive()` is on the **log** scale, so it is not directly comparable with
  `marginal_effects(effect = "hr")` unless exponentiated.

* **Survival `stc()` uncertainty is a nonparametric bootstrap**, not the delta
  method the other families use: the RMST is an integral of a fitted survival
  function and has no convenient closed-form variance. `n_boot` (default 200,
  `0` for a point estimate with no interval) and `seed` control it, and the seed
  is restored on exit so the caller's RNG stream is untouched. The RMST
  difference and the log cumulative-hazard ratio are counted separately, because
  a resample can return a finite RMST difference while the cumulative-hazard
  ratio is undefined at the horizon; each interval reports the replicate count
  it actually rests on. Where `flexsurv` admits a wider parameter space than the
  Bayesian model of the same name (a negative Gompertz shape, a negative
  generalized-gamma `Q`), both the point fit and the resamples are checked and
  the result says so, since the benchmark is then a broader-family comparison
  rather than a like-for-like one.

* **Custom knots** for a flexible baseline via the `knots` argument to
  `mlumr()`. Supply one `make_knots()` result for a shared baseline
  (`aux_by = "none"`), or `list(index = ..., comparator = ...)` for the
  study-specific default. `prior_sensitivity()` replays whatever was used, so a
  prior sweep does not silently re-place the knots.

* **New priors** `default_prior_aux()` (shape/scale parameters) and
  `default_prior_smooth()` (M-spline smoothing SD), configurable via the
  `prior_aux` and `prior_smooth` arguments to `mlumr()`.

### The baseline hazard is estimated per study by default

* `mlumr()` gains **`aux_by`, defaulting to `".study"`**: the index and
  comparator studies get their own M-spline coefficients (or their own
  parametric shape parameters) rather than sharing one shape. This is what
  `multinma::nma()` does, where `.study` is always part of the stratification,
  and the semantics match it exactly: `aux_by = NULL` resolves to `".study"`
  rather than meaning "share", so multinma code behaves the same way here.
  Sharing one baseline across both studies is `aux_by = "none"`, which multinma
  has no spelling for because it cannot do it. Two single-arm trials rarely
  share a hazard shape, and assuming they do imposes proportional hazards
  *across studies*, which no randomization supports.

  Each stratum gets **its own knots over its own observed support**, as in
  multinma's default `type = "quantile"`. This is required for identification,
  not a refinement. With one pooled basis spanning the longer study, a shorter
  study can have basis functions it never observes; scaling its observed
  coefficients by `c`, moving the surplus simplex mass into an unobserved
  column, and replacing its intercept by `mu - log(c)` leaves the likelihood
  exactly unchanged, so the intercept is set by the prior rather than the data.
  Per-study boundaries remove that flat direction, and each stratum's simplex
  constraint pins its cumulative hazard to 1 at a boundary it actually observed.

  The choice changes the estimate materially. Fitting the bundled newly-diagnosed
  multiple myeloma example both ways, and evaluating both at the same 64.8-month
  common follow-up, gives an index-population RMST difference of -0.41 months
  under a shared shape and +2.99 months under per-study shapes; the latter agrees
  in sign with the anchored comparisons reported in
  `vignette("survival-outcomes")`. Quoting a common horizon is necessary rather
  than optional here: a shared basis extrapolates nothing, so its default horizon
  is the pooled maximum of 70 months while the stratified default is the common
  support, and the two defaults are different estimands. The vignette fits only
  the stratified default; reproducing the shared-shape figure requires refitting
  with `aux_by = "none"` and `rmst_horizon = 64.8`.

  **What stratifying assumes.** In an anchored randomized network a
  study-specific baseline shape is a study nuisance and within-study
  randomization still identifies the treatment effect. Here each study
  contributes exactly one arm, so a study-specific shape and a
  treatment-specific shape are perfectly aliased: nothing in the data can say
  whether a difference in shape belongs to the treatment or to eligibility,
  ascertainment, follow-up, calendar time, supportive care, or unmeasured
  prognosis. Absolute predictions transported to the other population therefore
  carry that study's shape with them, an assumption the data cannot check, and
  `predict()` says so once per session for such fits. `aux_by = "none"` makes
  the opposite assumption and is at least testable against the two observed
  curves. Neither is assumption-free; fit both, prefer the collapsible RMST
  estimands as the primary reported effect, and report which was used.

  For M-spline and piecewise-exponential baselines, tied event times (the norm
  in pseudo-IPD reconstructed from a digitized Kaplan-Meier curve) can collapse
  duplicated quantile knots in one study only, leaving the two per-study bases
  with different dimensions. The realized knot count is then reduced until both
  studies agree, with a message saying so. How far it may be reduced depends on
  the spline degree: a cubic basis with no internal knots still has
  `degree + 1 = 4` coefficients, so zero is a valid last resort, while a
  degree-0 (piecewise exponential) basis keeps at least one internal knot,
  because zero there is a single constant hazard with nothing to smooth. Every
  basis column is asserted to have support within its own study, and a fit with
  no workable count stops rather than silently returning a likelihood ridge.
  A pooled basis is never used as a fallback.

  Stratified flexible baselines report study-specific spline coefficients
  (`scoef[j,s]`) and smoothing standard deviations (`sigma_smooth[s]`), with
  per-treatment views `scoef_idx[j]` and `scoef_cmp[j]` alongside them.

### What the survival effect measures are, exactly

* **`marginal_effects()` reports natural-scale effects** (null 1, like the rate
  ratio): the hazard ratio (`HR`) for proportional-hazards distributions, the
  time ratio (`TR`) for AFT distributions with one shared shape and one shared
  coefficient vector, or the exponentiated linear-predictor contrast
  (`EXP_DELTA_ETA`) where neither holds. Plus the RMST difference (`RMSTD`,
  null 0) and the RMST ratio (`RMSTR`).

* **A scalar hazard ratio never travels without its evaluation time.** Marginal
  hazard ratios are non-collapsible and are generally time-varying in both the
  SPFA and relaxed models, because the ratio weights the covariate distribution by each
  arm's own survival and the risk sets diverge. `marginal_effects()` therefore
  carries an `at_time` column (and an `at_time` attribute on the raw-draw
  frame), reported as `0` for the closed-form `t -> 0` limit under a shared
  baseline shape and as the evaluation time under study-specific shapes. The
  `at_time` argument names that time explicitly rather than inheriting it from
  the `pred_times` output grid; it is snapped to the nearest fitted prediction
  time, with a message when the match is not exact. `summary()` prints the time
  in its heading. For the whole curve use `predict(type = "loghr")`; the
  RMST-based effects are collapsible and free of this entirely.

* **RMST results carry their restriction time.** RMST is an integral to a
  horizon, so results computed to different horizons are different estimands and
  must not be pooled or plotted together. `predict(type = "rmst")` and the
  `RMSTD` / `RMSTR` rows of `marginal_effects()` report a `horizon` column (a
  `horizon` attribute with `summary = FALSE`), `summary()` prints it in the RMST
  heading, and a forest plot showing an RMST measure captions it. For a flexible baseline stratified by study the default horizon
  is the common follow-up `min(max(index times), max(comparator times))`, not
  the pooled maximum: each study's flexible baseline is extrapolated as a
  constant hazard past its own last observed time, so the pooled maximum would
  guarantee that the primary reported RMST extrapolates the shorter study.

* **An effect that is not available is an error, not a substitution.** With an
  AFT distribution and study-specific shapes, `exp(eta_index - eta_comparator)`
  is not a time ratio: the Weibull quantile ratio picks up
  `[-log S]^(1/a_i - 1/a_c)`, the log-normal `exp(z_p (sigma_i - sigma_c))`, and
  so on. The same applies to any **relaxed** AFT fit even with shared shapes:
  treatment-specific coefficients mean the covariate term does not cancel, so
  the exponentiated contrast is the conditional time ratio at the mean linear
  predictor (equivalently, the geometric mean of profile-specific conditional
  time ratios) rather than one population acceleration factor, and there need be
  no single `a` with `S_index(t) = S_comparator(t / a)` for all `t`. An explicit
  `marginal_effects(effect = "hr")` / `"tr"` or `conditional_effects(effect =
  "hr")` request stops in these cases and names the alternative
  (`"exp_delta_eta"`, `predict(type = "loghr")`, or `aux_by = "none"`) rather
  than returning a differently-named quantity. `effect = "all"` returns the
  contrast under an explicit label: `EXP_DELTA_ETA` from `marginal_effects()`,
  `EXP_ETA_CONTRAST` from `conditional_effects()`.

* **`delta_conditional` is an intercept contrast, and is labeled as one.** It is
  `mu_index - mu_comparator`, which is the linear-predictor contrast evaluated at
  the reference profile. Two conditions matter and are easy to conflate. A
  shared baseline shape is what makes it a conditional log hazard or time ratio
  at all, and it is then that contrast at the reference profile (the pooled
  covariate mean under the default `center = TRUE`), whether or not the
  coefficients are shared. Shared coefficients are what make it constant across
  covariate profiles. Under the default `aux_by = ".study"` neither holds: the
  baseline ratio does not cancel, and with per-study M-spline bases each
  intercept is additionally tied to its own basis normalization. `print()`
  states which case applies rather than listing the quantity beside the
  treatment effects.

* The distinction runs through every surface: `marginal_effects()`,
  `conditional_effects()`, `prior_sensitivity()`, and `summary()` derive the
  label and evaluation time from one shared helper, so they cannot disagree. It
  is applied where the shapes genuinely differ, which excludes the exponential
  and exponential-AFT: they have no shape parameter to stratify, so their
  closed-form contrast stays exact under any `aux_by`.

* **Generalized-gamma survival is evaluated on the log scale throughout.** Its
  survival function is a regularized upper incomplete gamma, whose value
  underflows to zero far into the tail while its logarithm remains an ordinary
  finite number. Computing it through a log-scale continued fraction keeps
  `log S(t)`, the log hazard, and the censored likelihood contributions finite
  wherever they are mathematically representable, so predictions and
  extrapolations at long times are usable rather than infinite.

## Identifying the relaxed model's comparator coefficients

* **New `check_identification()`**, and a vignette answering the question it
  exists for: how much aggregate evidence does `model = "relaxed"` need before
  its index-population estimate is data-driven rather than prior-driven? In the
  relaxed model `beta_comparator` is identified only by the aggregate
  likelihood, and the index-population estimand averages it over the IPD
  covariate distribution, so the answer is fixed by the aggregate subgroup rows
  before any model is fitted.

  Each aggregate row contributes one constraint and the comparator side has
  `K + 1` unknowns (the intercept counts), so `S >= K + 1` rows are necessary.
  They are not sufficient: the rows must also differ in **every** covariate
  direction. `check_identification()` measures that directly, reporting
  `cond_inv` (smallest over largest singular value of the centered, IPD-scaled
  subgroup-mean matrix) and a continuous count of usable directions.

  The simulation study in `vignette("subgroup-identification")` (360 fits across
  the binomial, normal, and poisson families) found that the geometry predicts
  the width of the index-population credible interval better than the row count
  does, and that two realistic tables fail in opposite ways: subgroups reported
  one variable at a time never exceed one usable direction however many are
  published, and a 2x2 categorical cross-tab satisfies `S >= K + 1` while
  leaving a continuous covariate's coefficient prior-driven. Six well-spread
  rows gave intervals less than half the width of eight collinear ones.

  `check_identification()` covers the binomial, normal, and poisson families
  and refuses `family = "survival"`, where its premise does not hold. A
  reconstructed comparator curve is not one scalar summary: it contributes a
  likelihood term at every event and censoring time, so how much of
  `beta_comparator` it determines depends on the survival model and the
  covariate distribution rather than on a row count. One binary covariate under
  exponential proportional hazards gives a known-weight mixture whose component
  rates the curve shape can separate, while several continuous covariates can
  leave the curve nearly invariant to rotations of the coefficient vector that
  hold its norm fixed. A relaxed survival fit therefore warns, and directs the
  reader to the posterior contraction reported by `marginal_effects()` and to
  `prior_sensitivity()`, rather than to a count. Index-population effects remain
  the most exposed, since they transport the comparator coefficients to the IPD
  covariate distribution.

* **New `prior_beta_comparator` argument** to `mlumr()` lets the relaxed model
  use a separate (typically tighter) prior on `beta_comparator`, which
  regularizes the index-population estimand that would otherwise extrapolate
  weakly-identified coefficients over the IPD covariate distribution. Defaults
  to `prior_beta` (so behavior matches earlier versions); ignored for
  `model = "spfa"`. Surfaced separately by `prior_summary()` and reused by
  `prior_sensitivity()`. All five relaxed Stan models take
  `prior_beta_comparator_mean` / `_sd` / `_dist` / `_df`, so the comparator
  coefficients can use a fully independent prior including a different family
  from `beta_index` (for example a heavy-tailed Student-t for regularization).

* **`marginal_effects()` emits a one-line note** when a relaxed fit is queried
  for the index population, reporting the **posterior contraction** of
  `beta_comparator` per covariate, `1 - (posterior sd / prior sd)^2`, and naming
  the weakly-identified ones. A single marginal comparator curve constrains
  `beta'X` but not the direction of `beta`, which is exactly what transporting
  to the index population needs, and an event count cannot detect that. The
  prior SD respects the prior family: Student-t scales are converted via
  `sqrt(df / (df - 2))`, and priors with no finite variance (`df <= 2`,
  including the Cauchy) report `NA` rather than a number that would misstate how
  much was learned. Suppress the note with
  `options(mlumr.quiet_relaxed_index = TRUE)`.

* **The weak-identifiability warning triggers on covariate rank, not row
  count.** Version 0.1.0 warned when `n_agd_rows < 2 * n_cov`, which counted
  duplicate `set_agd()` rows as additional information. Non-survival relaxed
  fits now warn when the rank of the AgD covariate matrix is below `n_cov + 1`,
  so repeated rows that add no new covariate profile no longer suppress the
  warning. Survival fits use the comparator event count against `2 * n_cov`.

* **Relaxed-model identification via subgroup AgD is documented.** Supplying the
  comparator AgD as jointly-defined subgroups (one `set_agd()` row per stratum,
  each with its own covariate summaries) identifies `beta_comparator` from the
  data (`L_AgD = prod_s L_{AgD,s}`) rather than the prior alone, implementing
  the primary relaxed-SPFA strategy of the ML-UMR paper. See `?mlumr`.

## Model comparison for survival fits

* **`survival_unit` for LOO and WAIC.** `calculate_loo()`, `calculate_waic()`,
  and `compare_models()` gain a `survival_unit` argument controlling what one
  pointwise unit is for a survival fit. The comparator arm enters as
  reconstructed pseudo-individuals, so the default `"observation"` holds out one
  pseudo-individual at a time and is optimistic: the pseudo-IPD are a
  digitization of a single published curve, not independent observations.
  `"arm"` groups the comparator pseudo-IPD by arm so each external arm is one
  held-out unit, and `"aggregate"` treats all comparator pseudo-IPD as a single
  external-evidence unit. The index IPD always stay per-individual. Ignored for
  the non-survival families.

## Transportability to arbitrary target populations

* **`newdata` argument** on `marginal_effects()` and `predict.mlumr_fit()`
  transports treatment effects and absolute outcomes to an **arbitrary target
  population** by Bayesian g-computation (model-based standardization over a
  supplied covariate distribution), as in the ML-UMR transportability step
  (Chandler & Ishak). Version 0.1.0 offered only the built-in index and
  comparator populations. Supported for all families' effects/predictions; for
  survival, the collapsible RMST-based effects (`"rmstd"`, `"rmstr"`) and the
  `survival`/`cumhaz`/`rmst`/`median` predictions transport, while the
  non-collapsible time-varying marginal hazard ratio remains tied to the
  built-in populations. Standardizing to the index covariates reproduces
  `population = "index"` exactly.

## Plotting

* **`plot()` methods** for the result objects, following multinma's convention
  that calling `plot()` on an effects or prediction object produces the
  corresponding figure:
    - `plot(marginal_effects(fit))`: forest of population-standardized effects.
    - `plot(predict(fit, type = "survival"))`: a curve with a credible band.
      The `"hazard"`, `"cumhaz"` and `"loghr"` types plot the same way, and
      `"rmst"`, `"median"` and `"response"` plot as point-intervals. Compose
      further layers, such as a Kaplan-Meier overlay, with `+`.
    - `plot(conditional_effects(fit, newdata = ...))`: effects by covariate
      profile.
* Each forest draws the null line implied by the measure it is showing, per
  facet: 0 for differences and log scales, 1 for the risk ratio, rate ratio,
  hazard ratio, time ratio, RMST ratio, and the two exponentiated survival
  contrasts (`EXP_DELTA_ETA`, `EXP_ETA_CONTRAST`).
* **`geom_km()`** overlays the observed Kaplan-Meier curves (from the
  `mlumr_data` object) on a model survival plot, colored by treatment and
  honoring delayed entry.
* **`plot_prior_posterior()`** (exported; the `multinma` name) overlays the
  posterior of named parameters on their prior, read from the fit.
* **`mlumr_forest()`** draws a forest plot from a plain data frame of
  estimates and interval bounds, for comparisons the `plot()` methods do not
  cover because they mix estimators: putting `naive()`, `stc()`, and both
  ML-UMR models on one axis, for instance, as `vignette("choosing-a-method")`
  does. It takes the reference line, axis label, title, and subtitle as
  arguments so the caller sets the measure's null rather than inheriting one.
* `marginal_effects()`, `predict()`, and `conditional_effects()` now return
  lightweight `data.frame` subclasses so these `plot()` methods can dispatch;
  all existing data-frame behavior (indexing, `knitr::kable()`, the reporting
  engine) is unchanged.
* `ggplot2` moved from Suggests to Imports (the plot methods use it at run time).

## Covariate distributions and coefficient labels

* **New moment-parameterized marginal distributions**, mirroring the ones
  `multinma` exports so a published baseline table can be used as printed:
  `qgamma()` / `pgamma()` / `dgamma()` and `qlogitnorm()` / `plogitnorm()` /
  `dlogitnorm()`. All accept a `mean` and `sd` that override the native
  parameters (`shape`/`rate` for the gamma, `mu`/`sigma` on the logit scale for
  the logit-normal), so `distr(qgamma, mean = age_mean, sd = age_sd)` works
  directly in `add_integration()` instead of requiring a hand conversion to
  shape and rate. Without `mean` and `sd` they forward to \pkg{stats}
  unchanged, so they are drop-in safe. The logit-normal is the natural marginal
  for a covariate reported as a proportion, such as percent body surface area.

  The logit-normal moment reparameterization has no closed form and is solved
  numerically, so it validates what it is given: both `mean` and `sd` must be
  supplied together (half a moment specification is an error, not a silent
  fallback to `mu = 0`, `sigma = 1`); the mean must lie strictly inside
  `(0, 1)`; the SD must be positive and satisfy `sd^2 < mean * (1 - mean)`, the
  bound any variable on `(0, 1)` obeys. The search runs on `log(sigma)` so the
  scale cannot go negative, starts from the delta-method approximation on the
  logit scale, and the recovered moments are checked against the target rather
  than the optimizer's convergence flag being trusted on its own.

* **Regression coefficients are labeled by covariate name.** `summary()` on a
  fit now prints `beta[age]` rather than `beta[1]` (and `beta_index[age]` /
  `beta_comparator[age]` for relaxed fits), matching multinma's idiom. The
  underlying `variable` strings in `fit$summary` are unchanged, so code that
  indexes on `beta[1]` keeps working.

## Behavior and validation changes to existing functions

These affect functions and outcome families that already shipped in 0.1.0:

* **Unanchored contrast requires distinct treatments.** `combine_data()` now
  rejects a shared IPD/AgD treatment label with an error. An unanchored contrast
  of a treatment against itself estimates a between-study baseline difference,
  not a treatment effect.
* **Continuous multi-row comparator estimand, and `outcome_n` is now required
  for it.** For the normal family the comparator-population standardized effect
  from several `set_agd()` rows is weighted by `outcome_n` (sample size) rather
  than equally, so splitting one comparator population into subgroup rows no
  longer changes the estimand. Because there is no defensible way to combine
  several population strata without knowing how large they are, `set_agd()` now
  requires `outcome_n` when normal aggregate data have more than one row and
  errors rather than silently falling back to equal weights. Single-row AgD is
  unchanged and still does not require `outcome_n`.
* **Covariate centering no longer depends on how AgD rows are tabulated.** Each
  aggregate row is weighted by the population it represents rather than by 1, so
  splitting one comparator subgroup into statistically equivalent rows leaves
  the center, and therefore the induced raw-scale intercept prior, unchanged.
* **`check_integration()` compares correlations on one scale.** The realized
  integration-point correlation was always measured with Pearson while the
  target, when derived from the IPD, defaults to Spearman. The method that
  defined the target is now carried into the diagnostic and reported in the
  output, so the comparison can no longer warn (or reassure) purely from a
  method mismatch.
* **`add_integration()` states where the copula correction does not apply.** The
  Spearman and Pearson maps branch on continuous versus binary margins. A
  nonbinary discrete margin, such as a count or an ordered category, has no
  branch and is mapped with the continuous-margin formula; an exact mapping
  would depend on that margin's distribution and its category thresholds.
  `add_integration()` warns when it detects such a covariate, and the
  documentation states the limitation. The realized dependence, and the
  finite-grid moments, should be examined with `check_integration()`.
* **`prior_sensitivity()` varies the prior and nothing else.** Every
  model-defining setting is taken from the original fit and replayed: family,
  link, survival distribution and its baseline controls (`n_knots`,
  `mspline_degree`, `pred_times`, `rmst_horizon`, `n_rmst_grid`, `aux_by`), the
  design-matrix controls (`center`, `qr`), the integration points, the shape and
  smoothing priors, and the engine and sampler settings. `...` may not override
  any of them, so movement across the sweep is attributable to the prior alone.
  For relaxed fits the comparator prior is swept alongside the index one, and
  `prior_beta_comparator_scales` pairs a chosen comparator scale with each index
  scale. Both are reported per row, in `scale` and `scale_comparator`, so a
  refit is never labeled by only half of the prior it was fitted under;
  `scale_comparator` is dropped for SPFA fits, which have no comparator
  coefficient prior. Its survival rows carry an `effect` label (`LOG_HR`,
  `LOG_TR`, or `DELTA_ETA`) and an `at_time` where one applies, from the same
  shared helper `marginal_effects()` uses.
* **`verbose = FALSE` now silences the sampler banner too.** cmdstanr writes its
  "Running MCMC with N chains / Chain k finished in ..." lines to stdout rather
  than through the condition system, so neither `refresh = 0` nor
  `suppressMessages()` suppressed them: roughly fifteen lines per fit, which
  buries the output of any loop over more than a handful of models. `mlumr()`
  now passes `verbose` through to the backend, and an explicit `show_messages`
  or `show_exceptions` in `...` still wins.
* **Stricter input validation and diagnostics.** An all-`NA` treatment column
  and out-of-range integer controls (e.g. `seed`) are now rejected;
  `prior_sensitivity()` validates its scale grid, `probs`, and `...`;
  `add_integration()` warns when a supplied `distr()` distribution grossly
  contradicts the declared `set_agd()` moments; `compare_models()` warns when
  DIC is compared across differing observation counts; `n_knots = 0` is rejected
  up front for `distribution = "pexp"`, where it leaves a single spline
  coefficient and the random-walk smoothing prior undefined; and cmdstanr fits
  now report tail-ESS so the tail-ESS convergence check runs.

* **Marginal summaries are no longer clipped to finite reporting bounds.** The
  Stan models previously passed marginal probabilities through `safe_logit()`
  (clamping to `[1e-10, 1 - 1e-10]`) and ratios through `safe_divide()`
  (flooring the denominator at `1e-10`), and the probit and cloglog inverse
  links clamped their output before it reached the likelihood. Both helpers are
  gone. Event and non-event probabilities, marginal means, rates, survival
  probabilities, and cumulative hazards are now formed on the log scale and the
  contrasts are built from those logs, so the reported quantity is the
  mathematical one rather than a finite surrogate. Two consequences: the
  likelihood is no longer biased by a clamp at extreme linear predictors, and
  ratios are no longer biased downward. The old `safe_divide()` substituted a
  denominator LARGER than the true one, so a risk ratio or rate ratio with a
  near-zero comparator was systematically understated; the log-scale contrast
  reports it. Working on the log scale also removes most of what used to trigger
  the clip in the first place, because a marginal probability is no longer
  rounded to 0 or 1 before the contrast is taken: `lor_*` is finite in cases
  where `safe_logit()` previously returned its clip boundary. What remains is
  that a ratio whose true value overflows double precision is now `Inf` rather
  than a large finite surrogate, which needs a draw with an effectively infinite
  linear predictor. Read the log-scale generated quantities when that happens.
  See `?mlumr-numerical-evaluation`.

* **`predict(type = "link")` reports the marginal link, not the mean linear
  predictor.** It previously returned `E[eta]`, the average conditional linear
  predictor. It now returns `g(E[g^-1(eta)])`: the fitted link applied to the
  population-standardized response mean. This is the quantity `marginal_effects()`
  contrasts, so differencing two `type = "link"` predictions now reproduces the
  reported marginal effect, which it did not before. The two definitions agree
  for the identity link and differ for logit, probit, cloglog, and log.
  This is a deliberate divergence from `multinma`, which keeps the two apart:
  its `predict(type = "link")` returns `E[eta]`, and the marginal link-scale
  contrast lives in `marginal_effects(mtype = "link")`. mlumr has no conditional
  population estimand to pair `E[eta]` with, since every effect it reports is
  standardized over a population, so it reports the marginal link under the one
  name rather than offering two link scales that differ silently.

* **Boundary probabilities use a continuity correction instead of a clamp.**
  `bound_probability()` previously clamped every input into
  `[min_count / n, 1 - min_count / n]`. It now leaves interior probabilities
  untouched and replaces only an observed 0 or 1 with the pseudo-count estimate
  `(r + min_count) / (n + 2 * min_count)`. For a zero-event arm with
  `min_count = 0.5` that is `0.5 / (n + 1)` rather than `0.5 / n`, so the link
  contrast, its standard error, and the risk ratio that `naive()` and `stc()`
  report for a binomial arm with no events (or no non-events) change slightly.
  Arms with events on both sides are unaffected.

* **`naive()` combines multiple aggregate rows as strata.** For binomial data
  the comparator standard error was computed as though the pooled comparator
  were a single binomial sample of size `sum(n)`. It is now the variance of the
  sample-size-weighted average of the row proportions,
  `sum(w_k^2 * p_k * (1 - p_k) / n_k)` with `w_k = n_k / sum(n)`, propagated to
  the link scale by the delta method. For normal data the comparator mean was an
  inverse-variance-weighted average, which estimates a common mean efficiently
  but is not the comparator population's mean; it is now the sample-size-weighted
  mean, matching the estimand the Stan models and `stc()` target, with variance
  `sum(w_k^2 * se_k^2)`. Both reduce exactly to the previous formulas for
  single-row aggregate data. The reported comparator event rate is now the
  observed proportion; the continuity correction is applied only inside the
  effect calculation.

* **`prior_normal(autoscale = TRUE)` rescales the prior location as well as its
  scale.** Autoscaling states a prior on the coefficient of a covariate measured
  in standard-deviation units, so recovering it on the original scale divides
  both the location and the scale by that covariate's SD. Only the scale was
  divided before, which left a nonzero prior mean attached to the wrong
  covariate scale. The default `mean = 0` is unaffected, since `0 / sd` is `0`.

* **`add_integration()` rejects a Pearson correlation with non-Gaussian
  margins.** A covariate-scale Pearson correlation is the Gaussian-copula
  correlation only when the margins are themselves Gaussian, so
  `cor_adjust = "pearson"` combined with a non-`qnorm` continuous margin and a
  nonzero off-diagonal entry now errors instead of silently treating the
  supplied matrix as a latent one. Use `cor_adjust = "spearman"`, Gaussian
  margins, or `cor_adjust = "none"` with a matrix already on the latent scale.

* **`check_integration()` also reports fidelity to the declared moments.** The
  existing resolution diagnostic compares two grid sizes and answers "is `n_int`
  large enough". It now additionally compares each realized grid moment against
  the mean and standard deviation declared in `set_agd()`, which answers the
  different question "does the grid represent the population it claims to". A
  `distr()` specification can be numerically well resolved and still target the
  wrong marginal. Two literal `%%` escapes that printed as `%%` in the
  `cat()`-based report are also fixed.

* **`stc()` no longer reports an index-population contrast.** For the binomial,
  normal, and count families `stc()` previously returned an index-population
  effect alongside the comparator-population one, obtained by assuming the
  treatment difference is constant on the link scale. That constancy is an extra
  assumption which is not part of the STC estimand and is not testable from the
  available data, and it does not hold under effect modification, which is the
  situation population adjustment exists to handle. `stc()` is now what its
  design supports: a comparator-population estimand. Use `mlumr()`, which
  standardizes both treatment models and reports both populations without that
  assumption, when the index population is the decision target.

## Example data

* **The worked examples are built on datasets derived from published trials**,
  replacing the freely invented datasets used in version 0.1.0's vignettes.
  Provenance differs by example and is stated in each vignette. The plaque
  psoriasis and newly-diagnosed multiple myeloma data are redistributed from
  `multinma`, where the individual patient data are themselves simulated to
  resemble the published trials and the comparator survival arm is reconstructed
  from a digitized Kaplan-Meier curve. The shoulder pain and dental caries data
  are synthetic, generated with `synthpop` from openly licensed trial data.

* **The survival example uses the McCarthy 2012 lenalidomide arm** (231 rows)
  as `ndmm_ipd`, paired with the Morgan 2012 thalidomide comparator, whose
  baseline covariate distributions are the closer match of the candidate pairs
  considered. `data-raw/prepare_multinma_subsets.R` documents the comparison and
  reproduces every bundled dataset.

## Performance

* The binary, continuous, and count IPD likelihoods now use Stan's fused
  **GLM density functions** (`bernoulli_logit_glm`, `normal_id_glm`,
  `poisson_log_glm`) on their canonical links (logit / identity / log); these
  carry analytic gradients and are faster than the equivalent `_lpdf` forms.
  Non-canonical links (probit, cloglog, log-normal) are unchanged. They are
  statistically equivalent to the 0.1.0 implementation: results match up to
  Monte Carlo error.
* The survival aggregate-data likelihood accumulates its per-pseudo-individual
  contributions into a vector and adds them with a single `sum()` (fewer
  reverse-mode nodes and lower memory than incremental `target +=`).
* Models **center the covariates** by default (matching multinma's
  `center = TRUE`): the IPD design matrix and the comparator integration grid
  are shifted to their pooled covariate mean before fitting. This removes an
  intercept-versus-slope collinearity that, on real-scale covariates (for
  example age in years), could push the NUTS sampler into very deep
  (max-treedepth) trajectories and dramatically slow fits. The shift is
  estimand-invariant for the reported effects (the intercept absorbs it, so all
  population-standardized contrasts and survival predictions are unchanged) and
  is applied transparently to `predict()`, `conditional_effects()`, and
  `conditional_predict()`. It is not prior-invariant: `prior_intercept` then
  applies to the intercept at the pooled covariate mean, so intercepts and
  intercept prior-versus-posterior plots are on a different scale from an
  uncentered fit. `center = FALSE` restores the raw-scale parameterization, and
  `qr = TRUE` offers a QR-rotated design as an alternative conditioning fix.

## Documentation

* Reorganized the vignettes into nine outcome-focused guides: `introduction`,
  `data-preparation`, `binary-outcomes`, `continuous-outcomes`,
  `count-outcomes`, `survival-outcomes`, `subgroup-identification`,
  `fitting-and-diagnostics`, and `choosing-a-method`.
* The modeling vignettes use **bundled example data** and show
  **complete output**: posterior summary and effect tables, forest plots,
  survival / Kaplan-Meier curves, and posterior diagnostic plots. Binary uses
  plaque psoriasis and survival uses newly-diagnosed multiple myeloma (both
  redistributed from the `multinma` package); continuous uses shoulder pain
  (arthroscopic subacromial decompression vs exercise therapy) and count uses
  dental caries (silver diamine fluoride vs nano-silver fluoride), both
  synthetic datasets generated with `synthpop` from openly licensed trials.
* To keep CRAN checks fast, these vignettes are **precompiled** (the multinma
  pattern): the Stan models are fitted once locally via `vignettes/precompile.R`
  and the rendered HTML is shipped through the `R.rsp` vignette engine, so no
  Stan model is run at build/check time. The prebuilt `*.html` and their
  `*.html.asis` registration stubs are tracked and included in the build.
* The binary-outcomes vignette covers **handling quasi-complete separation**
  via a heavy-tailed prior on the coefficients (`prior_student_t()` /
  `prior_cauchy()`), following Gelman et al. (2008).
* **Vignette settings track multinma's own examples.** Covariate marginals
  match `example_plaque_psoriasis.Rmd` and `example_ndmm.Rmd` (gamma for skewed
  continuous covariates, logit-normal for proportions, Bernoulli for binary),
  age stays in years, and weight stays in kilograms. Each vignette fits the
  model **both** ways, prognostic factors only and with treatment interactions,
  and the anchored cross-check does the same with `multinma`, so the
  `~ (covariates) * .trt` specification multinma uses is shown alongside the
  prognostic-only one rather than only described.
* **Self-contained vignettes**, following multinma's example style: each opens
  with a visible `library(mlumr)` and `options(mc.cores = ...)` setup chunk and
  otherwise uses exported functions (the `plot()` methods,
  `plot_prior_posterior()`, `knitr::kable()`, ggplot2); the identification
  simulation additionally calls one internal diagnostic helper. No code is
  sourced from a helper file, so every line producing a displayed result is
  printed in the vignette itself and the visible chunks are intended to run in a
  new R session. The
  survival vignette leads with the M-spline baseline (matching multinma's NDMM
  example) and adds a relaxed-model effect-modification section.

## Dependencies

* Added `splines2` and `survival` to Imports, and `flexsurv` to Suggests
  (the survival STC G-computation uses `flexsurv` when available).
* Moved `ggplot2` from Suggests to Imports: the new `plot()` methods,
  `mlumr_forest()`, `geom_km()`, and `plot_prior_posterior()` build ggplot
  objects at run time rather than only in examples.
* Added `multinma` (vignette example data), `ggsurvfit` (the observed
  Kaplan-Meier displays in the survival vignette), and `R.rsp`
  (precompiled-vignette engine) to Suggests, and `R.rsp` to `VignetteBuilder`.

## Package logo

* **New hex-sticker logo** using a broken-anchor motif, for the unanchored
  comparison. Built with `hexSticker` and the Ubuntu font via
  `tools/build-logo.R`.


# mlumr 0.1.0

Initial CRAN release.

## Core models

* **ML-UMR models**: Bayesian multilevel unanchored meta-regression with
  two model variants:
    - SPFA (Shared Prognostic Factor Assumption): shared covariate effects
      across treatments
    - Relaxed SPFA: treatment-specific covariate coefficients allowing
      effect modification estimation

* **Three outcome families**: binary (binomial), continuous (normal), and
  count (Poisson) outcomes, each with appropriate link functions:
    - Binomial: logit (default), probit, cloglog
    - Normal: identity (default), log
    - Poisson: log

* **Dual Stan backend**: rstan (default, CRAN-compatible) with optional
  cmdstanr support. Switch engines with `mlumr_engine("cmdstanr")`, which
  guides installation of cmdstanr and CmdStan if needed. Per-call override
  via the `engine` argument in `mlumr()`.

* **Simulated Treatment Comparison (STC)**: Frequentist outcome regression
  via parametric G-computation with delta-method standard errors. Supports
  prediction at covariate means or marginalization over full covariate
  distributions using integration points.

* **Naive unadjusted estimate**: Benchmark comparison of crude outcome
  summaries with delta-method confidence intervals.

## Data preparation

* `set_ipd()`, `set_agd()`, and `combine_data()` provide a
  unified interface for preparing IPD and AgD for all three methods.
* `set_ipd()` rejects covariate names that collide with reserved
  internal columns (`.outcome`, `.study`, `.trt`, `.exposure`) so user
  values cannot be silently overwritten by the standardized frame.
* `set_agd()` applies the same check to its covariate mean/SD columns
  (`.n`, `.r`, `.y`, `.se`, `.study`, `.trt`, `.E`) and additionally
  rejects `cov_means` entries that collapse to duplicate names after
  stripping the `_mean` / `_prop` suffix (e.g. `c("age_mean", "age")`).
* `add_integration()` generates Sobol-sequence quasi-Monte Carlo
  integration points with a Gaussian copula to account for covariate
  correlations, enabling accurate marginalization over the AgD covariate
  distribution.
* `mlumr()`, `add_integration()`, `check_integration()`, and
  `prior_sensitivity()` include `verbose` controls so scripts and
  tests can suppress package-level progress output while retaining warnings.
* The public API mirrors the function names used by the related
  `multinma` package for the data-setup, integration, and
  effect-summary workflow (`set_ipd()`, `set_agd()`,
  `add_integration()`, `unnest_integration()`, `distr()`,
  `marginal_effects()`, `qbern()`/`pbern()`/`dbern()`). Users
  familiar with ML-NMR can transfer their muscle memory directly to
  ML-UMR. When both packages are attached in the same R session R
  issues masking warnings on the shared names; disambiguate with
  `mlumr::function()` / `multinma::function()`.

## Prior system

* Prior constructors `prior_normal(mean, sd)`, `prior_student_t(df, mean, sd)`,
  `prior_cauchy(mean, sd)` (alias for `prior_student_t(df = 1, ...)`), and
  `prior_exponential(rate)`. All six Stan models branch on the prior
  family at runtime, so any of these can be supplied to `prior_intercept`,
  `prior_beta`, or (normal family only) `prior_sigma`.
* `prior_beta` accepts either a single prior (broadcast to all covariates)
  or a list of per-coefficient priors. Per-coefficient priors must share
  the same family and df (Stan branches on a single dist code).
* `prior_normal()`, `prior_student_t()`, and `prior_cauchy()` carry an
  `autoscale` argument. When passed as `prior_beta` with
  `autoscale = TRUE`, each coefficient's prior scale is divided by the
  empirical SD of its covariate (Gelman et al., 2008).
  `autoscale = FALSE` by default.
* Default priors follow the Stan community's prior-choice recommendations
  (Vehtari et al., 2025):
    - `prior_intercept`: `prior_normal(0, 10)`
    - `prior_beta`: `prior_normal(0, 2.5)` (weakly informative; Gelman et al.,
      2008)
    - `prior_sigma`: `prior_normal(0, 2.5)` (half-normal via the `<lower=0>`
      constraint in Stan) for the normal family.
* `default_prior_intercept()`, `default_prior_beta()`, and
  `default_prior_sigma()` accessors expose the package defaults. Values
  are tagged with `$default = TRUE` and the package `$version` so
  `prior_summary()` can report whether each prior is a default and which
  mlumr version produced it.
* `prior_summary()` S3 generic + `prior_summary.mlumr_fit()` method for
  human-readable introspection of every prior used in a fit, including
  post-autoscale per-coefficient scales.
* `prior_sensitivity()` refits a model across a grid of `prior_beta`
  scales and returns a posterior-summary table; the workflow recommended
  by Vehtari et al.'s prior-choice wiki for judging data- vs prior-driven
  inference.

## Inference helpers

* `predict.mlumr_fit()` returns population-specific predicted outcomes.
* `marginal_effects()` returns posterior treatment-effect summaries.
* `conditional_effects()` returns covariate-conditional treatment
  effects.
* `conditional_predict()` returns predictions at specific covariate
  values.
* `predict.mlumr_fit()` and `conditional_effects()` document the
  Jensen's-inequality gap on non-identity links: response-scale summaries
  are `E[g^{-1}(eta)]`, not `g^{-1}(E[eta])`.

## Model comparison

* `calculate_dic()` for DIC-based comparison (no extra dependencies).
* `calculate_loo()` and `calculate_waic()` using the optional `loo`
  package for PSIS-LOO and WAIC (Vehtari, Gelman, Gabry, 2017). `loo`
  is in `Suggests`, not `Imports`.
* `compare_models()` accepts `criterion = c("dic", "loo", "waic")`,
  defaulting to `"dic"`.
* All six Stan models produce pointwise log-likelihood vectors
  (`log_lik_ipd`, `log_lik_agd`); the standard contract for
  `loo::loo()` / `loo::waic()`.

## Sampling

* Regression coefficients `beta` (and `beta_comparator` in relaxed
  models) are sampled via an affine (non-centered) reparameterization:
  `z_beta ~ std_* (0, 1)`, `beta = prior_beta_mean + prior_beta_sd .* z_beta`.
  This decouples HMC adaptation from the prior scale and typically
  improves mixing when the prior scale is mis-matched with the
  posterior scale.

## Diagnostics

* Automatic MCMC diagnostic checks (divergences, Rhat, ESS, treedepth)
  via `check_diagnostics()`.
* `check_integration()` provides a `check_joint` argument that
  compares pairwise correlation matrices at the current vs doubled `n_int`
  (and against the user-supplied correlation target when available).

## Stan internals

* Stan prior hyperparameter declarations are shared across all six
  models via `#include include/priors_hyperparameters.stan` and
  `include/priors_sigma_hyperparameters.stan`. Prior log-density
  dispatchers live in `include/priors_functions.stan`.
* Binary-link numerical helpers are shared by the binary SPFA and relaxed
  models via `include/binary_functions.stan`.
* `E_ipd` in the two Poisson Stan models carries `<lower=0>` so
  off-API consumers who assemble `stan_data` manually get a Stan
  validation error rather than `log(0) = -Inf` on a non-positive
  exposure.
* Internal reference page `?mlumr-numerical-guards` documents
  `safe_logit`, `safe_divide`, and the `<lower=0>` Stan guards.

## Documentation

* A package startup message reports the installed mlumr version and GitHub
  repository when the package is attached.
* `?mlumr-package` provides a full overview of the typical workflow
  (data preparation -> integration -> fit -> diagnostics -> inference) and
  points at the alternative methods (`stc()`, `naive()`).
* `@seealso` cross-links across `predict.mlumr_fit()`,
  `marginal_effects()`, `conditional_effects()`,
  `conditional_predict()`, `prior_summary()`, and
  `prior_sensitivity()`.
* Vignettes covering data preparation, the ML-UMR models, STC and
  naive benchmarks, and method comparison. (Reorganized and expanded into
  outcome-specific guides in 0.2.0; see the 0.2.0 notes above.)

## Testing

* Test coverage spans data setup, integration, link functions, priors,
  prior summaries, prior sensitivity, engine selection, diagnostics,
  prediction, conditional effects, ML-UMR validation, fitted-model behavior,
  STC, naive benchmarks, utility functions, and LOO/WAIC/DIC model
  comparison.
* The test suite includes reserved-name guards, duplicate covariate-name
  checks, standardized-frame shape checks, pointwise log-likelihood
  extraction, integration diagnostics, posterior-summary validation, and
  family-specific behavior for binary, normal, and Poisson outcomes.
