# Predictions from ML-UMR model

Generate population-average absolute-outcome predictions in the index
and comparator populations.

## Usage

``` r
# S3 method for class 'mlumr_fit'
predict(
  object,
  population = c("both", "index", "comparator"),
  type = c("response", "link"),
  summary = TRUE,
  probs = c(0.025, 0.5, 0.975),
  ...
)
```

## Arguments

- object:

  An `mlumr_fit` object

- population:

  Which population: `"both"`, `"index"`, or `"comparator"`

- type:

  Prediction type: `"response"` or `"link"`. For `"response"`:
  probabilities (binomial), means (normal), or rates (poisson). For
  `"link"`: mean linear predictor on the fitted link scale (logit,
  probit, cloglog, log, or identity). The link-scale values are computed
  directly from parameter draws as `E[eta]`, not as
  `link(E[g^{-1}(eta)])`, to avoid Jensen's inequality bias.

- summary:

  Return summary statistics (`TRUE`) or full posterior draws (`FALSE`)

- probs:

  Quantiles for summary (default `c(0.025, 0.5, 0.975)`)

- ...:

  Additional arguments (unused)

## Value

A data frame with predictions. When `type = "link"`, values are mean
linear predictors computed directly from parameter draws (avoiding
Jensen's inequality bias).

## Details

**Marginalization on non-identity links.** For `type = "response"` the
reported values are `E[g^{-1}(eta)]` — the posterior expectation of the
inverse-link-transformed linear predictor — *not* `g^{-1}(E[eta])`. The
two differ whenever `g` is non-linear (logit, probit, cloglog, log) by
Jensen's inequality. In the index population the expectation is taken
over IPD individuals; in the comparator population it is taken over the
integration points constructed by
[`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
from the AgD moments. This is the correct population-average prediction
for an individual randomly drawn from that population, and it matches
what the Stan `generated quantities` block computes. For the link scale
(`type = "link"`) the reported value is `E[eta]`, a linear functional,
and the two interpretations coincide.

## See also

[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
for treatment-effect summaries;
[`conditional_predict()`](https://choxos.github.io/mlumr/reference/conditional_predict.md)
and
[`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
for predictions at specific covariate profiles.
