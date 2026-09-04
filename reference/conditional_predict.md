# Conditional predictions

Generate absolute predictions at specific covariate values for both
treatments.

## Usage

``` r
conditional_predict(
  object,
  newdata = NULL,
  type = NULL,
  summary = TRUE,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- object:

  An `mlumr_fit` object

- newdata:

  Data frame of covariate values. If `NULL`, uses IPD covariate means.

- type:

  `"response"` for probabilities, means, or rates; `"link"` for the
  fitted linear-predictor scale. `NULL`, the default, resolves to
  `"response"` for binomial, normal and Poisson fits. Ignored for
  survival fits, which return the conditional survival probability S(t
  \| x) at each fitted prediction time.

- summary:

  Return summary (`TRUE`) or full draws (`FALSE`)

- probs:

  Quantiles for summary

## Value

A data frame with predictions for each treatment at each profile. For
survival fits there is one row per profile, treatment, and time.

## See also

[`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
for covariate-conditional treatment *effects*;
[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
for population-level predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
conditional_predict(fit)
conditional_predict(fit, newdata = data.frame(age = 60, sex = 1))
} # }
```
