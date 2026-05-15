# Marginal treatment effects

Extract marginal treatment effects from a fitted ML-UMR model. For
binomial: log odds ratio, risk difference, risk ratio. For normal: mean
difference. For poisson: rate ratio.

## Usage

``` r
marginal_effects(
  object,
  population = c("both", "index", "comparator"),
  effect = "all",
  summary = TRUE,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- object:

  An `mlumr_fit` object

- population:

  Which population: `"both"`, `"index"`, or `"comparator"`

- effect:

  Which effect measure. For binomial: `"all"`, `"lor"`, `"rd"`, or
  `"rr"`. For normal: `"all"` or `"md"` (mean difference). For poisson:
  `"all"` or `"rr"` (rate ratio).

- summary:

  Return summary (`TRUE`) or full draws (`FALSE`)

- probs:

  Quantiles for summary

## Value

A data frame

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

# Full posterior draws rather than summary statistics
marginal_effects(fit, summary = FALSE)
} # }
```
