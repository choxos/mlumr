# Marginal treatment effects

Extract marginal treatment effects from a fitted ML-UMR model. For
binomial: log odds ratio, risk difference, risk ratio. For normal: mean
difference. For poisson: rate ratio. For survival: the hazard ratio
(proportional-hazards distributions, labeled `HR`), the time ratio
(accelerated-failure-time distributions with one shared shape and one
shared coefficient vector, labeled `TR`), or the exponentiated
linear-predictor contrast (`EXP_DELTA_ETA`, where neither of those
holds); all natural-scale, null 1, like the poisson rate ratio. Plus the
restricted-mean-survival-time difference (`RMSTD`, null 0) and the RMST
ratio (`RMSTR`, null 1), both reported with the restriction time in a
`horizon` column. For the time-varying log hazard ratio curve (null 0)
use `predict(type = "loghr")`.

## Usage

``` r
marginal_effects(
  object,
  population = c("both", "index", "comparator"),
  effect = "all",
  summary = TRUE,
  probs = c(0.025, 0.5, 0.975),
  newdata = NULL,
  at_time = NULL
)
```

## Arguments

- object:

  An `mlumr_fit` object

- population:

  Which population: `"both"` (default), `"index"`, or `"comparator"`.
  The **index** population is normally the decision-relevant target for
  health technology assessment, since cost-effectiveness models are
  built for the population the decision is about; report it as the
  primary estimand and the comparator population alongside. Ignored when
  `newdata` is supplied (the effect is standardized to the `newdata`
  target population instead).

- effect:

  Which effect measure. For binomial: `"all"`, `"lor"`, `"rd"`, or
  `"rr"`. For normal: `"all"` or `"md"` (mean difference). For poisson:
  `"all"` or `"rr"` (rate ratio). For survival: `"all"`, `"hr"` (hazard
  ratio for PH, time ratio for AFT, natural scale, null 1; `"tr"` is an
  accepted alias), `"exp_delta_eta"`, `"rmstd"` (RMST difference, null
  0), or `"rmstr"` (RMST ratio, natural scale, null 1). Requesting an
  effect the fit cannot produce is an error rather than a
  differently-named substitute: with an AFT distribution and
  `aux_by = ".study"` the study shapes differ, so
  `exp(eta_index - eta_comparator)` is not a time ratio and `"hr"` /
  `"tr"` are rejected in favor of the explicit `"exp_delta_eta"`.

- summary:

  Return summary (`TRUE`) or full draws (`FALSE`)

- probs:

  Quantiles for summary

- newdata:

  Optional data frame of covariate profiles defining an arbitrary
  **target population** to transport the effect to (Bayesian
  g-computation / model-based standardization over the supplied
  covariate distribution, as in Chandler & Ishak Eq 9-10). Each row is
  one individual / covariate profile; column names must match the model
  covariates. When `NULL` (default), effects are returned for the
  built-in `index` and/or `comparator` populations from the Stan
  generated quantities. When supplied, the marginal effect is recomputed
  by averaging model-based predictions over these rows at each posterior
  draw, and `population` is ignored. For survival, RMST effects and the
  time-specific target-standardized marginal hazard ratio are available;
  the latter uses `at_time` (the first fitted prediction time by
  default).

- at_time:

  Evaluation time for the scalar marginal hazard ratio, for
  proportional-hazards fits whose two studies have different baseline
  shapes (`aux_by = ".study"` with a distribution that has a shape
  parameter, or either flexible baseline). Marginal hazard ratios are
  non-collapsible and therefore time-varying, so the scalar is only
  meaningful with a time attached; naming it here makes it an estimand
  choice instead of a consequence of the `pred_times` output grid.
  Snapped to the nearest fitted prediction time, with a message when
  that is not exact. `NULL` (default) uses the first prediction time,
  reproducing `delta_*`. An error for a shared baseline (where the
  scalar is the closed-form `t -> 0` limit) and for AFT fits (whose
  scalar is a location contrast with no time).

## Value

A data frame. With `summary = FALSE` the raw posterior draws are
returned as a plain data frame (not plottable; plot methods need
`summary = TRUE`); the column names encode the per-family effect scale
(e.g. poisson `delta_*` is a natural-scale rate ratio, null 1; survival
is the exponentiated HR/TR). With `summary = TRUE` the `effect` column
names the measure; with `summary = FALSE` the scale is carried by the
draw column names themselves (`lor_*`, `rr_*`, `delta_*`, `hr_*` /
`tr_*`, `rmst*`). For survival, RMST-based rows also carry a `horizon`
column (the raw-draw frame, a `horizon` attribute) giving the
restriction time the integral runs to. RMST at different horizons is a
different estimand, so results are only comparable across fits when this
value matches.

For survival, the summary carries an `at_time` column and the raw-draw
frame an `at_time` attribute (one named value per column, `NA` for
measures with no evaluation time), so a time-specific hazard ratio never
travels without its time.

## Details

For survival proportional-hazards models the scalar `"hr"` (the
exponentiated `delta_*`) is always a **marginal** quantity, evaluated at
one time, and the `at_time` column records which. It is never a
conditional coefficient contrast, which is a different estimand and is
what
[`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
returns. Three cases:

- **Shared baseline shape, SPFA.** `delta_*` is the marginal log hazard
  ratio in the `t -> 0` limit (`at_time` is 0). Because SPFA gives both
  treatments the same coefficients, the covariate term cancels and this
  value happens to coincide with the conditional log hazard ratio
  `mu_index - mu_comparator`, which IS constant in time and covariates.
  The two agree here; they are still different estimands, and they part
  company at `t > 0`, where the marginal ratio drifts as the two arms'
  surviving covariate distributions diverge.

- **Shared baseline shape, relaxed.** The coefficients differ by
  treatment, so nothing cancels: `delta_*` is the marginal log hazard
  ratio at `t -> 0` only, and is time-varying thereafter.

- **Study-specific shape-bearing baseline** (the `aux_by = ".study"`
  default). `delta_*` is taken from the time-varying marginal `loghr_*`
  curve at the first prediction time, or at `at_time` when supplied.

Hazard ratios are non-collapsible, so the marginal ratio is time-varying
in every case above except the degenerate `t -> 0` evaluation itself.
For the full curve use `predict(type = "loghr")`. RMST-based effects
(`"rmstd"`, `"rmstr"`) are collapsible within a specified population,
but collapsibility alone does not make them invariant or transportable
across populations.

For accelerated-failure-time distributions the scalar is
`exp(E_X[eta_index(X)] - E_X[eta_comparator(X)])`, and what that is
depends on the fit:

- **One shared shape, SPFA** (`TR`). The coefficients are shared, so
  `eta_index(x) - eta_comparator(x)` is the same constant `a` at every
  covariate profile. Every individual's survival time is accelerated by
  the same factor, so the population-standardized curves satisfy
  `S_index(t) = S_comparator(t / a)` exactly and this IS a population
  time ratio.

- **Otherwise** (`EXP_DELTA_ETA`; differing shapes, or the relaxed
  model). The conditional acceleration varies with `x`, so it is the
  exponentiated average log ratio: equivalently the conditional time
  ratio at the mean linear predictor, or the geometric mean of the
  profile-specific conditional time ratios. It is **not** generally a
  time ratio between the two standardized survival distributions: there
  need be no single `a` with `S_index(t) = S_comparator(t / a)` for all
  `t`, and different survival quantiles can imply different apparent
  acceleration factors. It is labeled `EXP_DELTA_ETA` rather than `TR`
  for that reason.

Neither carries an evaluation time (`at_time` is `NA`). For a population
contrast under differing covariate effects use the RMST-based effects,
which are collapsible and have no such caveat.

For binomial fits the `"lor"` measure is always a logit-scale marginal
odds ratio computed from response-scale population probabilities,
independent of the fitted link. So for a `probit`/`cloglog` fit it is on
a different scale than
[`naive()`](https://choxos.github.io/mlumr/reference/naive.md) /
[`stc()`](https://choxos.github.io/mlumr/reference/stc.md) `$estimate`,
which is the fitted-link (probit / cloglog) difference.

**Relaxed-model index-population estimands.** For `model = "relaxed"`
the index-population estimand averages `beta_comparator` over the IPD
covariate distribution, while `beta_comparator` is identified only by
the (typically single) AgD likelihood term and so is integrated outside
the support it was identified on. The resulting effects are wider and
more prior-sensitive than the comparator-population estimands (which
integrate `beta_comparator` over the AgD support, consistent with
identification). When `population` is `"both"` or `"index"` for a
relaxed fit, `marginal_effects()` emits a one-line note recommending the
comparator population, tightening `prior_beta_comparator` (see
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md)), and
running
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md).
Suppress the note with `options(mlumr.quiet_relaxed_index = TRUE)`.

## See also

[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
for absolute predictions;
[`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
for covariate-conditional effects at specific profiles;
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
to check how strongly the marginal effect depends on `prior_beta`.

## Examples

``` r
if (FALSE) { # \dontrun{
# All effect measures for both populations
marginal_effects(fit)

# Only the log odds ratio in the index population
marginal_effects(fit, population = "index", effect = "lor")

# Transport the effect to an external (e.g. jurisdiction-specific) population
marginal_effects(fit, newdata = target_population_covariates)

# Full posterior draws rather than summary statistics
marginal_effects(fit, summary = FALSE)
} # }
```
