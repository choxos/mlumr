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
  `"rr"`. For normal: `"all"` or `"md"`. For poisson: `"all"` or `"rr"`.
  For survival: `"all"`, `"rmstd"`, `"rmstr"`, and the one scalar name
  the fit's contrast is, `"hr"` for a proportional-hazards fit, `"tr"`
  for a shared-shape SPFA accelerated-failure-time fit and
  `"exp_delta_eta"` otherwise. There are no aliases: requesting a scale
  the fit cannot supply is an error naming the one it can.

- summary:

  Return summary (`TRUE`) or full draws (`FALSE`)

- probs:

  Quantiles for summary

- newdata:

  Optional data frame of covariate profiles defining a target population
  to standardize the effect to by g-computation (Chandler and Ishak, Eq
  9 and 10); `population` is then ignored. Column names must match the
  model covariates. The same survival scalar selector applies as without
  `newdata`.

- at_time:

  Evaluation time for the scalar marginal hazard ratio of a
  proportional-hazards fit whose two studies have different baseline
  shapes. Snapped to the nearest fitted prediction time, with a message.
  `NULL` uses the first prediction time. Under a shared baseline the
  scalar is the `t -> 0` limit, so only `at_time = 0` is accepted; an
  error for AFT fits.

## Value

A data frame. With `summary = FALSE` the raw posterior draws are
returned as a plain data frame whose column names carry the effect scale
(`lor_*`, `rr_*`, `delta_*`, `hr_*` / `tr_*` / `exp_delta_eta_*`,
`rmst*`); with `summary = TRUE` the `effect` column names the measure.
For survival, RMST rows carry a `horizon` column and the scalar rows an
`at_time` column (attributes of the same names on the raw-draw frame).

## Details

For survival proportional-hazards fits the scalar `"hr"` is always a
marginal hazard ratio at one time, recorded in the `at_time` column: the
`t -> 0` limit under a shared baseline shape (where an SPFA fit's value
coincides with the conditional hazard ratio, since the shared
coefficients cancel), and the value at the first prediction time, or at
`at_time`, under study-specific shapes. Hazard ratios are
non-collapsible, so the marginal ratio is time-varying;
`predict(type = "loghr")` gives the curve, and the RMST effects are
collapsible within a population.

For accelerated-failure-time fits the scalar is
`exp(E_X[eta_index(X)] - E_X[eta_comparator(X)])`. With one shared shape
and SPFA coefficients it is a population time ratio (`TR`): every
individual's survival time is accelerated by the same factor. With
differing shapes or treatment-specific coefficients no single
acceleration factor exists and it is labeled `EXP_DELTA_ETA`; use RMST
effects or explicitly indexed survival quantiles there. Neither carries
an evaluation time. See
[`vignette("survival-outcomes", "mlumr")`](https://choxos.github.io/mlumr/articles/survival-outcomes.md).

For binomial fits `"lor"` is always a logit-scale marginal odds ratio,
whatever the fitted link, so for a probit or cloglog fit it is on a
different scale than
[`naive()`](https://choxos.github.io/mlumr/reference/naive.md) and
[`stc()`](https://choxos.github.io/mlumr/reference/stc.md) `$estimate`.

**Relaxed-model index-population estimands** average `beta_comparator`
over the IPD covariate distribution, outside the support it was
identified on, so they are wider and more prior-sensitive than the
comparator-population ones. `marginal_effects()` says so once per call;
suppress with `options(mlumr.quiet_relaxed_index = TRUE)`.

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
