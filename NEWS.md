# mlumr 0.1.0.9000 (development version)

## Behavior and validation changes to existing functions

* **`compare_models()` no longer reads a standard error as a threshold, and
  refuses fits built on different observations.** The LOO/WAIC printout said
  that `se_diff > 2` is the conventional threshold for a meaningful difference.
  A large standard error is uncertainty about a difference, not evidence for
  it; the paragraph now says to read `elpd_diff` against `se_diff`, to treat
  any ratio as a heuristic rather than a decision rule, and to check the PSIS
  diagnostics. Every comparison the function makes is also paired, column by
  column, and `loo` can only check that the pointwise matrices have the same
  shape: two fits of different data with the same number of rows, or of the
  same rows in a different order, compared without complaint. The fits carry
  the data they were built from, so the columns that define an observation
  (`.study`, `.trt`, the outcome, exposure, and for survival the times and
  status, and for survival comparators both the aggregate rows with their
  covariate summaries and the reconstructed pseudo-individuals), together with
  every covariate the fits share, are now compared across the fits row for
  row and a mismatch is an error. Covariates
  only one fit uses are not compared, since models of the same outcomes with
  different covariate sets are exactly what gets compared. `calculate_dic()`
  objects carry the same frames, so a DIC comparison is checked too. A model
  whose object carries no data is reported as unverifiable rather than
  assumed to match.

* **The survival `effect` selector is now literal and distribution-specific.**
  `marginal_effects()` accepted `"hr"`, `"tr"` and `"exp_delta_eta"` as
  interchangeable names for one computation, and the label on the returned row
  was the only thing that said which of the three you had been given. Requesting
  `effect = "hr"` from a shared-shape accelerated-failure-time fit therefore
  returned a time ratio, and `effect = "tr"` from a proportional-hazards fit
  returned a hazard ratio. Each fit now accepts exactly one scalar name: `"hr"`
  for proportional hazards, `"tr"` for a shared-shape SPFA AFT fit, and
  `"exp_delta_eta"` for any relaxed AFT fit, or a shape-bearing one with
  `aux_by = ".study"` (`"exponential-aft"` has no shape to stratify, so an SPFA
  fit keeps `"tr"`).
  Asking for a scale the fit cannot supply is an error naming the one it can.
  `conditional_effects()` already worked this way, so the two APIs no longer
  disagree. Code that relied on the aliasing must name the measure it wants.
  The `newdata` route now uses the same selector. It previously offered the
  hazard ratio for a proportional-hazards fit and no scalar at all for an AFT
  fit, so a shared-shape SPFA AFT fit answered `effect = "tr"` without
  `newdata` and refused it with, and `effect = "all"` quietly returned RMST
  effects only as soon as a target was supplied. Supplying a target changes
  which population an effect is standardized to, not which effects exist, so an
  AFT fit now reports its target-standardized location contrast,
  `exp(mean(eta_index) - mean(eta_comparator))` over the target rows. With
  shared coefficients the covariate term cancels draw by draw and the value is
  identical for every target, which is the sense in which a shared-shape time
  ratio is population-invariant; with relaxed coefficients it does not cancel
  and the value genuinely belongs to the target.

* **Requested survival prediction times keep their order and multiplicity.**
  `predict()` selected the nearest fitted grid point for each requested time and
  then sorted and deduplicated the result, so `times = c(10, 2)` came back in
  the other order and `times = c(2, 2)` came back with one row. A caller could
  not line its request up against the result and had to reconstruct the mapping
  by parsing a message. The frame now has one row per requested time, in the
  order requested, and carries a `requested_time` column beside `time` whenever
  `times` is supplied. Two distinct requested times can still land on one grid
  point; both are answered, by the same fitted time, and that is reported.

* **`prior_sensitivity()` validates `probs` with the shared validator.** Its
  local copy of the check omitted the duplicate test that `.validate_probs()`
  applies everywhere else, so two equal probabilities produced two identically
  named `qNN` columns and the second silently overwrote the first: the caller
  asked for n quantiles and received fewer, with no error.

* **An ignored `prior_aux2` is now ignored rather than validated.** For a
  survival fit whose distribution has fewer than two auxiliary parameters, a
  well-formed `prior_aux2` warned and was dropped while a malformed one warned
  and then aborted the fit, so the same argument carried two contracts
  depending on a distribution it does not apply to. It is now discarded before
  validation, matching the non-survival families, which already warned without
  validating.

* **`prior_summary()` names the constrained prior instead of calling every
  positive-constrained prior a "half-distribution".** Two of those labels were
  wrong: an exponential is already supported on the positive half-line, so
  `<lower=0>` truncates nothing, and a normal or t with a nonzero location
  truncated at zero is a truncated normal or t, not a half-normal or half-t.

* **`prior_sensitivity()` names its quantile columns like the rest of the
  package.** They were built with `paste0("q", round(100 * probs))`, which
  labelled the default 2.5th and 97.5th percentiles `q2` and `q98`, and made
  distinct probabilities collide: `probs = c(0.024, 0.025)` produced `q2` twice
  and the second silently overwrote the first. The columns are now `q2.5`,
  `q50`, `q97.5`, matching `marginal_effects()`, so the two can be joined by
  name. Code reading `q2` or `q98` must be updated.

* **The native logit-normal parameterization validates `mu` and `sigma`.** Only
  the moment (`mean` / `sd`) parameterization was checked. A `sigma` of zero
  returned `Inf` from `dlogitnorm()`, `1` from `plogitnorm()` and `0.5` from
  `qlogitnorm()`, all finite and none flagged, while a negative `sigma` returned
  `NA` from the density but `NaN` with a base warning from the other two.
  Non-finite, missing, non-positive and non-numeric parameters are now one clear
  error in all three functions, as they already were for `mean` / `sd`, and
  lengths that would recycle partially are rejected rather than pairing values
  with the wrong parameter.

* **Requested survival prediction times report when they are snapped.**
  `predict(..., times = )` selects the nearest fitted grid time and said
  nothing about it: a 12-month policy horizon could be reported at
  11.8 months, and two requested times landing on one grid point silently
  produced one row instead of two. Both now emit a message naming the requested
  and the used times, and point at `pred_times` for exact evaluation. Asking for
  the same time twice returns it twice and is not an approximation, so it
  produces no message. The returned values are unchanged. Snapping itself
  remains: it is a property of this implementation, not a necessity, since the
  parametric distributions have closed forms at any positive time and the
  flexible bases can be evaluated anywhere inside their support. Evaluating
  requested times directly is still to do.

* **`prior_sensitivity()` no longer claims a scale sweep proves the inference is
  data-driven.** Constant summaries across the tested scales show insensitivity
  to those scales, within one prior family at one location on one model. The
  printed interpretation now says that, and points at `check_identification()`
  for the question a scale sweep cannot answer.

* **Marginal summaries are no longer clipped to finite reporting bounds.** The
  Stan models previously passed marginal probabilities through `safe_logit()`
  (clamping to `[1e-10, 1 - 1e-10]`) and ratios through `safe_divide()`
  (flooring the denominator at `1e-10`). Both helpers are gone. Event and
  non-event probabilities, marginal means, and rates are now formed on the log
  scale and the contrasts are built from those logs, so the reported quantity is
  the mathematical one rather than a finite surrogate. Two consequences: the
  likelihood is no longer biased by a clamp at extreme linear predictors, and
  ratios are no longer biased downward. The old `safe_divide()` substituted a
  denominator LARGER than the true one, so a risk ratio or rate ratio with a
  near-zero comparator was systematically understated; the log-scale contrast
  reports it. Working on the log scale also removes most of what used to trigger
  the clip in the first place, because a marginal probability is no longer
  rounded to 0 or 1 before the contrast is taken: `lor_*` is finite in cases
  where `safe_logit()` previously returned its clip boundary. What remains is
  that a ratio whose true value overflows double precision is now `Inf` rather
  than a large finite surrogate. That needs only a finite log contrast above
  `log(.Machine$double.xmax)`, about 709.78, not an infinite linear predictor.
  Read the log-scale generated quantities when it happens.
  See `?mlumr-numerical-evaluation`.

* **`predict(type = "link")` reports the marginal link, not the mean linear
  predictor.** It previously returned `E[eta]`, the average conditional linear
  predictor. It now returns `g(E[g^-1(eta)])`: the fitted link applied to the
  population-standardized response mean. Differencing two `type = "link"`
  predictions therefore gives a contrast on the fitted link scale, which
  reproduces a reported effect where the two scales coincide (a logit binomial
  fit's `lor_*`) and needs a transformation elsewhere, since
  `marginal_effects()` reports on the scale conventional for the family. The two
  definitions of `type = "link"` agree for the identity link and differ for
  logit, probit, cloglog, and log. This is a deliberate divergence from `multinma`, which keeps the two
  apart: its `predict(type = "link")` returns `E[eta]`, and the marginal
  link-scale contrast lives in `marginal_effects(mtype = "link")`. mlumr has no
  conditional population estimand to pair `E[eta]` with, since every effect it
  reports is standardized over a population, so it reports the marginal link
  under the one name rather than offering two link scales that differ silently.

* **Boundary probabilities use a continuity correction instead of a clamp.**
  `bound_probability()` previously clamped every input into
  `[min_count / n, 1 - min_count / n]`. It now leaves interior probabilities
  untouched and replaces only an observed 0 or 1 with the pseudo-count estimate
  `(r + min_count) / (n + 2 * min_count)`. For a zero-event arm with
  `min_count = 0.5` that is `0.5 / (n + 1)` rather than `0.5 / n`, so the link
  contrast, its standard error, and the risk ratio that `naive()` and `stc()`
  report for a binomial arm with no events (or no non-events) change slightly.
  Arms with events on both sides are unaffected.

* **New arguments are inserted before the sampler controls, so positional calls
  are not preserved.** `mlumr()` gains model-defining arguments ahead of
  `chains`, `iter`, and the rest. A call that passed sampler settings by
  position rather than by name therefore binds them to the wrong parameters and
  stops with a validation error naming the argument it actually received. Call
  `mlumr()` with named arguments.

* **`prior_sensitivity()` varies the prior and nothing else.** Every
  model-defining setting is taken from the original fit and replayed: family,
  link, survival distribution and its baseline controls (`n_knots`,
  `mspline_degree`, `pred_times`, `rmst_horizon`, `n_rmst_grid`, `aux_by`), the
  design-matrix controls (`center`, `qr`), the integration points, the shape and
  smoothing priors, and the engine and sampler settings. `...` may no longer
  override any of them, so movement across the sweep is attributable to the
  prior alone. For relaxed fits the comparator prior is swept alongside the
  index one, and `prior_beta_comparator_scales` pairs a chosen comparator scale
  with each index scale. Both are reported per row, in `scale` and
  `scale_comparator`, so a refit is never labeled by only half of the prior it
  was fitted under. On an SPFA fit, which has no comparator coefficient prior,
  the argument is declined with a warning and the column is dropped. Survival
  rows carry an `effect` label (`LOG_HR`, `LOG_TR`, or `DELTA_ETA`) and an
  `at_time` where one applies, from the same shared helper `marginal_effects()`
  uses.

* **`verbose = FALSE` now silences the sampler banner too.** cmdstanr writes its
  "Running MCMC with N chains / Chain k finished in ..." lines to stdout rather
  than through the condition system, so neither `refresh = 0` nor
  `suppressMessages()` suppressed them: roughly fifteen lines per fit, which
  buries the output of any loop over more than a handful of models. `mlumr()`
  now passes `verbose` through to the backend, and an explicit `show_messages`
  or `show_exceptions` in `...` still wins.

* **`prior_normal(autoscale = TRUE)` rescales the prior location as well as its
  scale.** Autoscaling states a prior on the coefficient of a covariate measured
  in standard-deviation units, so recovering it on the original scale divides
  both the location and the scale by that covariate's SD. Only the scale was
  divided before, which left a nonzero prior mean attached to the wrong
  covariate scale. The default `mean = 0` is unaffected, since `0 / sd` is `0`.

* **`check_integration()` compares correlations on one scale.** The realized
  integration-point correlation was always measured with Pearson while the
  target, when derived from the IPD, defaults to Spearman. The method that
  defined the target is now carried into the diagnostic and reported in the
  output, so the comparison can no longer warn (or reassure) purely from a
  method mismatch.

* **`check_integration()` also reports fidelity to the declared moments.** The
  existing resolution diagnostic compares two grid sizes and answers "is `n_int`
  large enough". It now additionally compares each realized grid moment against
  the mean and standard deviation declared in `set_agd()`, which answers the
  different question "does the grid represent the population it claims to". A
  `distr()` specification can be numerically well resolved and still target the
  wrong marginal.

* **`add_integration()` states where the copula correction does not apply.** The
  Spearman and Pearson maps branch on continuous versus binary margins. A
  nonbinary discrete margin, such as a count or an ordered category, has no
  branch and is mapped with the continuous-margin formula; an exact mapping
  would depend on that margin's distribution and its category thresholds.
  `add_integration()` warns when it detects such a covariate, and the
  documentation states the limitation.

* **`add_integration()` rejects a Pearson correlation with non-Gaussian
  margins.** A covariate-scale Pearson correlation is the Gaussian-copula
  correlation only when the margins are themselves Gaussian, so
  `cor_adjust = "pearson"` combined with a non-`qnorm` continuous margin and a
  nonzero off-diagonal entry now errors instead of silently treating the
  supplied matrix as a latent one. Use `cor_adjust = "spearman"`, Gaussian
  margins, or `cor_adjust = "none"` with a matrix already on the latent scale.

* **`add_integration()` warns when a supplied `distr()` distribution grossly
  contradicts the declared `set_agd()` moments.**

* **Continuous multi-row comparator estimand, and `outcome_n` is now required
  for it.** For the normal family the comparator-population standardized effect
  from several `set_agd()` rows is weighted by `outcome_n` (sample size) rather
  than by inverse variance, so splitting one comparator population into subgroup
  rows no longer changes the estimand. An inverse-variance average estimates a
  common mean efficiently but is not the comparator population's mean, which is
  the size-weighted mixture of its strata. Because there is no defensible way to
  combine several population strata without knowing how large they are,
  `set_agd()` now requires `outcome_n` when normal aggregate data have more than
  one row and errors rather than silently falling back to precision weights.
  `naive()` and `stc()` use the same weighting. Single-row AgD is unchanged and
  still does not require `outcome_n`.

* **`naive()` combines multiple aggregate rows as strata.** For binomial data
  the comparator standard error was computed as though the pooled comparator
  were a single binomial sample of size `sum(n)`. It is now the variance of the
  sample-size-weighted average of the row proportions,
  `sum(w_k^2 * p_k * (1 - p_k) / n_k)` with `w_k = n_k / sum(n)`, propagated to
  the link scale by the delta method. It reduces exactly to the previous formula
  for single-row aggregate data. The reported comparator event rate is now the
  observed proportion; the continuity correction is applied only inside the
  effect calculation.

* **`stc()` no longer reports an index-population contrast.** For the binomial,
  normal, and count families `stc()` previously returned an index-population
  effect alongside the comparator-population one, obtained by assuming the
  treatment difference is constant on the link scale. That constancy is an extra
  assumption which is not part of the STC estimand and is not testable from the
  available data, and it does not hold under effect modification, which is the
  situation population adjustment exists to handle. `stc()` is now what its
  design supports: a comparator-population estimand, labeled as such in the
  returned object. Use `mlumr()`, which standardizes both treatment models and
  reports both populations without that assumption, when the index population is
  the decision target.

* **`seed` defaults to 2026 and says so.** `seed = NULL` previously drew from
  the session RNG whenever `.Random.seed` existed. R initializes that variable
  on demand from the clock and the process id, so its presence never
  established that `set.seed()` had been called: an unseeded fit was silently
  irreproducible, and drawing from the state also advanced the caller's stream.
  `seed = NULL` now uses the documented default of 2026 and warns, and the fit
  banner marks the seed as a default.

* **A difference of two unbounded quantities is undefined, not zero.** The
  log-scale contrast used by the marginal summaries returned an exact zero when
  both logs were `+Inf`, reporting an indeterminate difference as a null
  effect. Both the R helper and its Stan counterpart now leave it undefined.
  Two `-Inf` logs still give zero, because both quantities are zero.

* **Zero exposures and zero aggregate standard errors are refused again.**
  `E_ipd` and `E_agd` in the poisson models and `se_agd` in the normal models
  are declared `<lower=1e-12>`, as they were in 0.1.0. Under a bound of zero an
  exposure of exactly zero reaches `log()` in the linear predictor, and a zero
  aggregate standard error makes the normal likelihood improper.

* **The collinearity guard reports a design that cannot be full rank.** It
  returned quietly whenever there were no more complete IPD rows than
  covariates, which is exactly the case it exists to catch. A covariate whose
  empirical standard deviation is undefined, which is what a single IPD row
  produces, is treated as having no usable scale instead of aborting
  autoscaling.

* **Tail ESS is computed rather than looked for.** `check_diagnostics()` tested
  a column that the default rstan backend never produced, so the check was
  inert on every fit the package makes. Tail ESS is now computed chain-aware
  from the post-warmup draws, and a fit that cannot supply it is reported as
  such rather than passing silently.

* **`check_integration()` no longer passes a comparison it did not make.** An
  all-missing set of differences gave `-Inf` from `max(na.rm = TRUE)`, which
  clears every threshold and printed as "close"; such a comparison now reads
  "unavailable". The declared-target standard deviation falls back to the
  Bernoulli form only for margins declared binary, rather than for any
  covariate whose mean happens to land in `[0, 1]`. A correlation matrix passed
  directly is resolved by name the way `add_integration()` resolves it, so
  reversed dimnames no longer produce a verdict about the wrong pairs. Under
  `cor_adjust = "none"` the supplied matrix is the latent Gaussian copula
  correlation, which the realized covariate-scale correlation does not
  estimate; that comparison is withheld and named instead of scored across the
  two scales.

* **`check_identification()` declines a fitted SPFA object.** Its report is
  headed "relaxed model" and diagnoses `beta_comparator`, which a
  shared-coefficient model does not have. It also compares the realized
  integration design against the declared one through singular-value spectra
  rather than matrix rank, which could not distinguish declared means
  `c(-1, 1)` from realized `c(-1e-10, 1e-10)`; and two aggregate rows built
  from the same integration tuples in a different order now count as one
  profile, since the likelihood averages over a row's points.

* **`n_knots` is validated only when knots have to be generated.** A valid
  custom knot specification was rejected because an argument the fit never
  reads was out of range. The resolved basis is still checked on its own terms.

## Transportability to arbitrary target populations

* **`newdata` argument** on `marginal_effects()` and `predict.mlumr_fit()`
  transports treatment effects and absolute outcomes to an **arbitrary target
  population** by Bayesian g-computation (model-based standardization over a
  supplied covariate distribution), as in the ML-UMR transportability step.
  Version 0.1.0 offered only the built-in index and comparator populations.
  Supported for all families' effects and predictions. For survival, the
  collapsible RMST-based effects (`"rmstd"`, `"rmstr"`) and every absolute
  prediction transport. The marginal hazard ratio is reported for a target
  population too, but it is not a property of that population alone: hazard
  ratios are non-collapsible, and the marginal one weights the covariate
  distribution by each arm's own survival. It follows the same
  evaluation-time convention as the built-in populations, the closed-form
  `t -> 0` limit when the two studies share a baseline shape and the requested
  (or first) fitted time when they do not.

  Standardizing to the index covariates reproduces `population = "index"`
  exactly, for every measure including the hazard ratio, which is the check
  that the transport path and the built-in path are the same calculation.

* **`marginal_effects()` emits a one-line note** when a relaxed fit is queried
  for the index population, reporting the **posterior contraction** of
  `beta_comparator` per covariate, `1 - (posterior sd / prior sd)^2`, and naming
  the weakly-identified ones. A single marginal comparator curve constrains
  `beta'X` but not the direction of `beta`, which is exactly what transporting
  to the index population needs, and an event count cannot detect that. The
  prior SD respects the prior family: Student-t scales are converted via
  `sqrt(df / (df - 2))`, and priors with no finite variance (`df <= 2`,
  including the Cauchy) report `NA` rather than a number that would misstate how
  much was learned. Suppress with
  `options(mlumr.quiet_relaxed_index = TRUE)`.

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
  contrasts. A forest showing only ratio measures is drawn on a log axis, so
  reciprocal effects sit at equal distances from the null. The interval's
  coverage is read from the quantiles the result carries rather than assumed to
  be 95%, and a time-specific marginal hazard ratio is labelled with the
  evaluation time it belongs to.
* **`geom_km()`** overlays the observed Kaplan-Meier curves (from the
  `mlumr_data` object) on a model survival plot, colored by treatment and
  honoring delayed entry. Each curve carries the population its arm was
  measured in, so on a plot faceted by population it appears only in its own
  panel.
* **`plot_prior_posterior()`** (exported; the `multinma` name) overlays the
  posterior of named parameters on the prior the fit records for each of them,
  including the `<lower=0>` truncation for the constrained ones. A parameter
  the fit carries no prior for is refused rather than drawn against another
  parameter's.
* **`mlumr_forest()`** draws a forest plot from a plain data frame of estimates
  and interval bounds, for comparisons the `plot()` methods do not cover because
  they mix estimators: putting `naive()`, `stc()`, and both ML-UMR models on one
  axis, for instance. It takes the reference line, axis label, title, and
  subtitle as arguments so the caller sets the measure's null rather than
  inheriting one.
* `marginal_effects()`, `predict()`, and `conditional_effects()` now return
  lightweight `data.frame` subclasses so these `plot()` methods can dispatch;
  all existing data-frame behavior (indexing, `knitr::kable()`, the reporting
  engine) is unchanged.
* `ggplot2` moved from Suggests to Imports (the plot methods use it at run
  time), at `>= 3.4.0` because they use `linewidth`, which 3.3.x ignores.

## Time-to-event (survival) outcomes

* **New `prior_aux2` argument for the second generalized-gamma auxiliary
  parameter.** The two auxiliaries govern different features of the hazard and
  can need different regularization; both previously took whatever `prior_aux`
  specified. `NULL` (the default) reuses `prior_aux`, so existing fits are
  unchanged, and `prior_summary()` shows the two separately when they can
  differ, naming them as the generalized-gamma `sigma` and `k = 1 / Q^2` for the
  Lawless shape `Q` rather than as anonymous auxiliaries. Supplying it for a
  distribution with fewer than two auxiliary parameters warns rather than being
  silently ignored, and `prior_sensitivity()` refuses to vary it mid-sweep like
  every other scenario-defining argument. `prior_aux`'s documentation now also
  records that one default is reused across auxiliary parameters that do not
  share a scale: the Gompertz shape has units of 1 / time, so the same trial
  expressed in days rather than years gives a half-normal(0, 2) an entirely
  different meaning.

* **New `"survival"` outcome family for data setup.** `set_ipd()` accepts
  time-to-event data, and **`set_agd_surv()`** takes the comparator arm as
  reconstructed pseudo-IPD (event and censoring times digitized from a published
  Kaplan-Meier curve) together with its covariate moments. `combine_data()` and
  `add_integration()` carry the family through.

* **Frequentist benchmarks for survival**: `naive()` returns an unadjusted Cox
  log hazard ratio, and `stc()` performs parametric G-computation of the RMST
  difference using the `flexsurv` package (a suggested dependency). `stc()`
  takes an `rmst_horizon` argument, since its own default is the pooled maximum
  observed time while a stratified flexible `mlumr()` baseline defaults to the
  follow-up both studies observed; the two are different estimands. Note that
  `naive()` is on the **log** scale, so it is not directly comparable with
  `marginal_effects(effect = "hr")` unless exponentiated.

* **Survival `stc()` uncertainty is a nonparametric bootstrap**, not the delta
  method the other families use: the RMST is an integral of a fitted survival
  function and has no convenient closed-form variance. `n_boot` (default 200,
  `0` for a point estimate with no interval) and `seed` control it, and the seed
  is restored on exit so the caller's RNG stream is untouched. `n_boot = 1` is
  rejected, because the standard error of a single resample is undefined and was
  otherwise indistinguishable from every resample having failed.

* **`survival_unit` for LOO and WAIC.** `calculate_loo()`, `calculate_waic()`,
  and `compare_models()` gain a `survival_unit` argument controlling what one
  pointwise unit is for a survival fit. The comparator arm enters as
  reconstructed pseudo-individuals, so the default `"observation"` holds out one
  pseudo-individual at a time and is optimistic: the pseudo-IPD are a
  digitization of a single published curve, not independent observations.
  `"arm"` groups them so each external arm is one held-out unit, and
  `"aggregate"` treats all comparator pseudo-IPD as a single external-evidence
  unit. The index IPD always stay per-individual.

* **Regression coefficients are labeled by covariate name.** `summary()` on a
  fit now prints `beta[age]` rather than `beta[1]` (and `beta_index[age]` /
  `beta_comparator[age]` for relaxed fits). The underlying `variable` strings in
  `fit$summary` are unchanged, so code that indexes on `beta[1]` keeps working.

* **HTA prediction suite** from `predict()` on a survival fit:
  `type = "survival"`, `"hazard"`, `"cumhaz"`, `"rmst"` (restricted mean
  survival time), `"median"`, and `"loghr"` (the time-varying marginal log
  hazard ratio curve, null 0). `predict(type = "median")` carries a
  `p_not_reached` column reporting the posterior probability that the median is
  beyond follow-up. `conditional_effects()` / `conditional_predict()` give
  covariate-conditional contrasts and survival curves.

* **`marginal_effects()` reports natural-scale survival effects** (null 1): the
  hazard ratio (`HR`) for proportional-hazards distributions, the time ratio
  (`TR`) for AFT distributions with one shared shape and one shared coefficient
  vector, or the exponentiated linear-predictor contrast (`EXP_DELTA_ETA`) where
  neither holds. Plus the RMST difference (`RMSTD`, null 0) and RMST ratio
  (`RMSTR`).

* **A scalar hazard ratio never travels without its evaluation time.** Marginal
  hazard ratios are non-collapsible and generally time-varying, so
  `marginal_effects()` carries an `at_time` column, reported as `0` for the
  closed-form `t -> 0` limit under a shared baseline shape and as the evaluation
  time under study-specific shapes. For the whole curve use
  `predict(type = "loghr")`; the RMST-based effects are collapsible and free of
  this entirely.

* **RMST results carry their restriction time.** RMST is an integral to a
  horizon, so results computed to different horizons are different estimands and
  must not be pooled. `predict(type = "rmst")` and the `RMSTD` / `RMSTR` rows of
  `marginal_effects()` report a `horizon` column.

* **An effect that is not available is an error, not a substitution.** With an
  AFT distribution and study-specific shapes, the exponentiated contrast is not
  a time ratio, and the same applies to any relaxed AFT fit even with shared
  shapes. An explicit `effect = "hr"` / `"tr"` request stops in these cases and
  names the alternative rather than returning a differently-named quantity.

* **Parametric and flexible baselines** via the `distribution` argument to
  `mlumr()`:
    - Proportional hazards: `"exponential"`, `"weibull"` (default),
      `"gompertz"` (positive-shape, increasing-hazard parameterization).
    - Accelerated failure time: `"exponential-aft"`, `"weibull-aft"`,
      `"lognormal"`, `"loglogistic"`, `"gamma"`, `"gengamma"` (the positive-`Q`
      Lawless generalized gamma subfamily; negative-`Q` shapes are not covered).
    - Flexible baseline hazard: `"mspline"` (M-spline) and `"pexp"`
      (piecewise exponential), with a random-walk smoothing prior.

* **New priors** `default_prior_aux()` (shape/scale parameters) and
  `default_prior_smooth()` (M-spline smoothing SD), configurable via the
  `prior_aux` and `prior_smooth` arguments to `mlumr()`. **`make_knots()`**
  places M-spline knots, and the `knots` argument accepts a custom placement.

* **The baseline hazard is estimated per study by default.** `mlumr()` gains
  `aux_by`, defaulting to `".study"`: the index and comparator studies get their
  own M-spline coefficients (or their own parametric shape parameters) rather
  than sharing one shape, matching what `multinma::nma()` does. Sharing one
  baseline across both studies is `aux_by = "none"`. Two single-arm trials
  rarely share a hazard shape, and assuming they do imposes proportional hazards
  *across studies*, which no randomization supports.

  Each stratum gets its own knots over its own observed support. This is
  required for identification, not a refinement: with one pooled basis spanning
  the longer study, a shorter study can have basis functions it never observes,
  leaving a flat likelihood direction that the prior rather than the data
  resolves.

* **Censoring support** in `set_ipd()`: right, left, interval, and delayed entry
  (left truncation) via a `survival::Surv()` object. The
  `time`/`status`/`entry_time` column route covers right-censoring (status
  `0`/`1`) and optional delayed entry; supply a `Surv` object for left- or
  interval-censored data.

* `outcome` is no longer required for `family = "survival"`, which uses
  `Surv`/`time`/`status` instead. It is still required for the other families,
  and its absence is now reported as such.

## Identifying the relaxed model's comparator coefficients

* **New `check_identification()`**: how much aggregate evidence does
  `model = "relaxed"` need before its index-population estimate is data-driven
  rather than prior-driven? In the relaxed model `beta_comparator` is identified
  only by the aggregate likelihood, so the answer is fixed by the aggregate
  subgroup rows before any model is fitted.

  Each aggregate row contributes one constraint and the comparator side has
  `K + 1` unknowns (the intercept counts), so `S >= K + 1` rows are necessary.
  They are not sufficient: the rows must also differ in **every** covariate
  direction. `check_identification()` measures that directly, reporting
  `cond_inv` (smallest over largest singular value of the centered, IPD-scaled
  subgroup-mean matrix) and a continuous count of usable directions. Subgroups
  reported one variable at a time never exceed one usable direction however many
  are published.

  For a nonlinear mean model the report is labeled descriptive only: subgroup
  means do not determine the likelihood geometry there, because the
  within-row distributions also affect the integrated response.

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

* **The weak-identifiability warning no longer counts a duplicated row as
  evidence.** Version 0.1.0 warned when `n_agd_rows < 2 * n_cov`, so repeating
  a `set_agd()` row silenced it without adding anything. What replaces the
  count depends on the link, because what a row contributes does. Under an
  identity link the mean profiles are the design, so the warning triggers on
  the rank of that design. Under any other link the integrated response depends
  on each row's whole covariate distribution, and two rows with equal means but
  different spreads do carry different constraints, so mean rank would
  understate the evidence; there the warning triggers on the number of rows
  that do not repeat another's integration grid, a bound that holds under every
  link because a repeated grid gives an identical likelihood term.

## Covariate distributions

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

  Both parameterizations validate what they are given. `mean` and `sd` must be
  supplied together (half a moment specification is an error, not a silent
  fallback to the native defaults). A gamma needs both strictly positive and
  finite: a negative SD is not a typo the conversion can absorb, since both
  `shape` and `rate` square it and `sd = -2` would otherwise return exactly the
  `sd = 2` distribution. A logit-normal mean must lie strictly inside `(0, 1)`
  and its SD must satisfy `sd^2 < mean * (1 - mean)`, the bound any variable on
  `(0, 1)` obeys. Supplying a conflicting `rate` and `scale` is refused rather
  than resolved in favor of one of them.

  The logit-normal moment reparameterization has no closed form and is solved
  numerically. The moments are integrated over the latent normal variable
  rather than over `x` on `(0, 1)`, where a concentrated margin is a narrow
  spike that adaptive quadrature steps over; the search runs on `log(sigma)` so
  the scale cannot go negative, starts from the delta-method approximation on
  the logit scale, and repeats until a restart stops improving, because
  Nelder-Mead reports convergence when its simplex collapses rather than when
  it has arrived. The recovered moments are then checked against the target,
  relative to the target rather than absolutely, instead of the optimizer's
  convergence flag being trusted on its own.

## Example data

* **The worked examples are built on datasets derived from published trials**,
  replacing the freely invented datasets used in version 0.1.0's vignettes.
  Provenance differs by example and is stated in each dataset's help page. The
  plaque psoriasis data are redistributed from `multinma`, where the individual
  patient data are themselves simulated to resemble the published trial. The
  shoulder pain and dental caries data are synthetic, generated with `synthpop`
  from openly licensed trial data.

* `psoriasis_ipd` / `psoriasis_agd` (binary), `shoulder_ipd` / `shoulder_agd`
  (continuous), `caries_ipd` / `caries_agd` (count), and `ndmm_ipd` /
  `ndmm_agd` / `ndmm_agd_covs` (survival, newly diagnosed multiple myeloma,
  also redistributed from `multinma`).
  `data-raw/prepare_multinma_subsets.R` reproduces every bundled dataset.

* `psoriasis_ipd$prevsys` is `integer` 0/1 rather than `logical`, matching
  every other binary covariate in the bundled sets. `set_ipd()` declines a
  logical covariate, so the pair previously could not be used without coercing
  it first. The values are unchanged, and no dataset shipped in 0.1.0.

* Each help page now records the covariates that look wrong but are not:
  `caries_ipd$exposure` is the poisson offset that `set_ipd()` requires,
  constant at 1 because `dmft` is a whole-mouth count with no time at risk;
  `caries_ipd$log_cfu` is bimodal, with 9 of 103 values at exactly zero;
  `psoriasis_ipd$weight` is missing for 2 of 347 rows; and the shoulder and
  caries pairs share one `study` label across their IPD and AgD halves because
  each pair is two arms of one trial, so the `combine_data()` warning about it
  is expected.

## Performance

* The binary, continuous, and count IPD likelihoods now use Stan's fused
  **GLM density functions** (`bernoulli_logit_glm`, `normal_id_glm`,
  `poisson_log_glm`) on their canonical links (logit / identity / log); these
  carry analytic gradients and are faster than the equivalent `_lpdf` forms.
  Non-canonical links (probit, cloglog, log-normal) are unchanged. They are
  statistically equivalent to the 0.1.0 implementation: results match up to
  Monte Carlo error.
* Models **center the covariates** by default: the IPD design matrix and the
  comparator integration grid are shifted to their pooled covariate mean before
  fitting. This removes an intercept-versus-slope collinearity that, on
  real-scale covariates (for example age in years), could push the NUTS sampler
  into very deep (max-treedepth) trajectories and dramatically slow fits. The
  shift is estimand-invariant for the reported effects (the intercept absorbs
  it, so all population-standardized contrasts are unchanged) and is applied
  transparently to `predict()`, `conditional_effects()`, and
  `conditional_predict()`. It is not prior-invariant: `prior_intercept` then
  applies to the intercept at the pooled covariate mean, so intercepts and
  intercept prior-versus-posterior plots are on a different scale from an
  uncentered fit. `center = FALSE` restores the raw-scale parameterization, and
  `qr = TRUE` offers a QR-rotated design as an alternative conditioning fix.

## Documentation

* **The relaxed model's identification claim is corrected.**
  `prior_beta_comparator` said the comparator-population effect "is identified
  directly by the AgD". The AgD likelihood informs the comparator-population
  outcome, but identifying `beta_comparator` or a treatment contrast depends on
  the number and geometry of the independent aggregate summaries, the link, the
  covariate distribution, the outcome precision, and the prior. The
  documentation says so, and points at `check_identification()` and
  `prior_sensitivity()`.

* **`set_agd_surv()` states that reconstruction uncertainty is not propagated.**
  Pseudo-individual records enter the likelihood as observed data, so a survival
  posterior conditions on one digitization of one published curve and carries
  none of the uncertainty in producing it: intervals are narrower than the
  evidence supports, most visibly for flexible baselines, late-tail RMST and
  medians, and weakly identified relaxed comparator coefficients. The help page
  now says this and describes refitting across plausible reconstructions as the
  way to see how much it matters.

* Reorganized the vignettes into nine outcome-focused guides: `introduction`,
  `data-preparation`, `binary-outcomes`, `continuous-outcomes`,
  `count-outcomes`, `survival-outcomes`, `subgroup-identification`,
  `fitting-and-diagnostics`, and `choosing-a-method`.
* The modeling vignettes use the bundled example data and show complete
  output: posterior summary and effect tables, forest plots, survival and
  Kaplan-Meier curves, and posterior diagnostic plots.
* To keep checks fast, those vignettes are precompiled, following multinma's
  pattern: the Stan models are fitted once locally through
  `vignettes/precompile.R` and the rendered HTML ships through the `R.rsp`
  engine, so no Stan model runs at build or check time. The prebuilt `*.html`
  and their `*.html.asis` registration stubs are tracked and included in the
  build.
* The binary-outcomes vignette covers quasi-complete separation, handled with
  a heavy-tailed coefficient prior (`prior_student_t()` / `prior_cauchy()`),
  following Gelman et al. (2008).
* Covariate marginals follow multinma's own examples
  (`example_plaque_psoriasis.Rmd`, `example_ndmm.Rmd`): gamma for skewed
  continuous covariates, logit-normal for proportions, Bernoulli for binary,
  with age in years and weight in kilograms. Each vignette fits both the
  prognostic-only model and the one with treatment interactions, and the
  anchored cross-check does the same with `multinma`.
* The vignettes are self-contained. Each opens with a visible `library(mlumr)`
  and `options(mc.cores = ...)` chunk and otherwise uses exported functions, so
  every line producing a displayed result is printed in the vignette itself and
  the visible chunks are meant to run in a new session. The identification
  vignette additionally calls one internal diagnostic helper. The survival
  vignette leads with the M-spline baseline, matching multinma's NDMM example,
  and adds a relaxed-model effect-modification section.

## Dependencies

* Added `splines2` and `survival` to Imports, and `flexsurv` to Suggests; the
  survival STC G-computation uses `flexsurv` when it is available.
* Moved `ggplot2` from Suggests to Imports. The `plot()` methods,
  `mlumr_forest()`, `geom_km()`, and `plot_prior_posterior()` build ggplot
  objects at run time rather than only in examples.
* Added `multinma` (the anchored cross-check in the vignettes), `ggsurvfit`
  (the observed Kaplan-Meier displays), and `R.rsp` (the precompiled-vignette
  engine) to Suggests, and `R.rsp` to `VignetteBuilder`.

## Package logo

* **New hex-sticker logo** using a broken-anchor motif, for the unanchored
  comparison. Built with `hexSticker` and the Ubuntu font.

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
* Six vignettes covering data preparation, ML-UMR models, STC and
  naive benchmarks, method comparison, and a complete worked example.
  Vignettes run compact examples during package checks; intentionally failing
  demonstrations and longer production-style fits remain non-executed.

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
