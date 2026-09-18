# Conditional treatment effects

Compute conditional (individual-level) treatment effects at specific
covariate values from a fitted ML-UMR model. Unlike
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md),
which averages over a population's covariate distribution, conditional
effects evaluate the treatment effect at a particular covariate profile.

## Usage

``` r
conditional_effects(
  object,
  newdata = NULL,
  effect = "all",
  summary = TRUE,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- object:

  An `mlumr_fit` object

- newdata:

  Data frame of covariate values at which to compute effects. Each row
  defines one covariate profile. Column names must match the covariates
  used in fitting. If `NULL` (default), uses the covariate means from
  the IPD as a single reference profile.

- effect:

  Which effect measure. For binomial: `"all"`, `"link_effect"`, `"rd"`,
  or `"rr"`. For normal: `"all"` or `"md"`. For Poisson: `"all"` or
  `"rr"`. The legacy value `"lor"` is accepted as an alias for
  `"link_effect"` when the fitted link is logit.

  For **survival**, `exp(eta_index - eta_comparator)` is a conditional
  hazard ratio (`"hr"`, proportional-hazards distributions) or time
  ratio (`"tr"`, accelerated failure time distributions) only when the
  two studies share a baseline shape (`aux_by = "none"`, or any
  exponential fit). The two are different measures: `"tr"` on a
  proportional-hazards fit and `"hr"` on an AFT fit are errors, whose
  message gives the conversion where one exists (`TR = HR^(-1/shape)`
  for a Weibull, applied draw by draw; `TR = 1/HR` for an exponential).
  Under the default study-specific shapes an explicit `"hr"` or `"tr"`
  is an error, since the baseline ratio does not cancel; `"all"` returns
  the contrast under the label `"EXP_ETA_CONTRAST"` with a warning. The
  RMST effects from
  [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
  are the recommended alternative, and `predict(type = "loghr")` gives
  the population-standardized curve.

- summary:

  Return summary statistics (`TRUE`) or full posterior draws (`FALSE`)

- probs:

  Quantiles for summary (default `c(0.025, 0.5, 0.975)`)

## Value

A data frame. If `summary = TRUE`, contains columns `profile`, `effect`,
`mean`, `sd` and quantile columns. If `summary = FALSE`, returns a
single combined data frame of full posterior draws with a `profile`
column indicating which covariate profile each draw belongs to.

## Details

For SPFA models, the conditional link-scale treatment effect is constant
across all covariate values because the shared beta cancels in the
treatment contrast on the fitted link scale. However, risk difference
(RD) and risk ratio (RR) still vary with covariates because they depend
on absolute probability levels. For relaxed models, all conditional
effects vary with covariate values because the index and comparator
treatments have different regression coefficients.

For binomial, normal and Poisson the conditional link-scale effect is
`eta_index - eta_comparator`; for survival the contrast is exponentiated
(null 1) and labeled `"EXP_ETA_CONTRAST"` when the baseline shapes
differ. A conditional effect is evaluated at one profile, so unlike
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
and
[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
there is no averaging over a population and no gap between
`E[g^{-1}(eta)]` and `g^{-1}(E[eta])`.

## See also

[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
for population-averaged treatment effects;
[`conditional_predict()`](https://choxos.github.io/mlumr/reference/conditional_predict.md)
for absolute predictions at specific profiles;
[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
for population-level predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Conditional effects at IPD covariate means (default)
conditional_effects(fit)

# At specific covariate values
conditional_effects(fit, newdata = data.frame(age = 60, sex = 1))

# Multiple profiles
profiles <- data.frame(age = c(50, 60, 70), sex = c(0, 0, 1))
conditional_effects(fit, newdata = profiles)
} # }
```
