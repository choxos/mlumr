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

  For **survival**, what is available depends on whether the two studies
  share a baseline, because `exp(eta_index - eta_comparator)` is a
  conditional hazard ratio only when the baseline factor cancels:

  - **Shared baseline shape** (`aux_by = "none"`, or any exponential
    fit, which has no shape to stratify): `"hr"` returns the exact
    conditional hazard ratio, labeled `"HR"`, for a proportional-hazards
    distribution, and `"tr"` the exact time ratio (`"TR"`) for an
    accelerated failure time one. The two are different estimands and
    `"tr"` is **not** an alias for `"hr"`: a proportional-hazards model
    has no constant time ratio and an AFT model has no constant hazard
    ratio, so `"tr"` on a PH fit and `"hr"` on an AFT fit are both
    errors rather than the other measure returned under the label it
    does have.

  - **Study-specific shape-bearing baseline** (`aux_by = ".study"`, the
    default, with a distribution that has a shape parameter or either
    flexible baseline): an explicit `"hr"` / `"tr"` request is an
    **error**. The conditional hazard ratio is
    `h0_index(t) / h0_comparator(t) * exp(eta_index - eta_comparator)`
    and the baseline ratio does not cancel, so no scalar hazard ratio
    exists to return. Returning the bare exponent under the name `hr`
    would report a different estimand than the one requested.

  - `"all"` still works in that case and returns the exponentiated
    linear-predictor contrast under the honest label
    `"EXP_ETA_CONTRAST"`, with a warning explaining what it is not. The
    warning is emitted whenever the baseline shapes differ, for
    accelerated failure time models as well as proportional-hazards
    ones.

  - The collapsible RMST effects from
    [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
    are unaffected and are the recommended alternative.
    `predict(type = "loghr")` gives the time-varying hazard ratio
    standardized over a **population**, so it is a marginal quantity and
    not the conditional effect at a supplied covariate profile; it
    answers a different question rather than substituting for this one.

- summary:

  Return summary statistics (`TRUE`) or full posterior draws (`FALSE`)

- probs:

  Quantiles for summary (default `c(0.025, 0.5, 0.975)`)

## Value

A data frame. If `summary = TRUE`, contains columns `profile`, `effect`,
`mean`, `sd`, and quantile columns. If `summary = FALSE`, returns a
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

For binomial / normal / Poisson the conditional link-scale effect is
computed directly as `eta_index - eta_comparator`. This avoids numerical
distortion from transforming extreme response-scale probabilities back
through the link function. For **survival** the contrast is reported on
the natural scale, exponentiated from `eta_index - eta_comparator` (null
1, like the rate ratio). That exponent is the conditional hazard ratio
(or AFT time ratio) only when the two studies share a baseline shape;
under the stratified default it is labeled `"EXP_ETA_CONTRAST"` instead,
because the baseline ratio `h0_index(t) / h0_comparator(t)` does not
cancel, and for an accelerated failure time model because differing
shapes add quantile-dependent factors. `predict(type = "loghr")` gives
the time-varying log hazard ratio standardized over a population, which
is a marginal quantity rather than a profile-specific conditional one.

**Conditional vs marginal on non-identity links.** Conditional effects
are evaluated at a single covariate profile, so there is no averaging
over a population and no Jensen's-inequality gap between the conditional
and marginal response. Compare with
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
and
[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md),
which average over either the IPD individuals (index population) or the
AgD integration points (comparator population) and therefore return
`E[g^{-1}(eta)]`, not `g^{-1}(E[eta])`.

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
