# Summarize a single draw vector into mean / sd / quantiles.

Internal helper. Centralizes the (mean, sd, quantile) triplet used by
[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md),
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md),
[`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
and
[`conditional_predict()`](https://choxos.github.io/mlumr/reference/conditional_predict.md)
so that any later change to the canonical posterior summary only needs
to happen in one place.

## Usage

``` r
.summarize_draw_vector(x, probs)
```

## Arguments

- x:

  Numeric vector of posterior draws.

- probs:

  Quantile probabilities.

## Value

Named numeric vector: `c(mean, sd, <named quantiles>)`.
