# Summarize a single draw vector into mean, sd and quantiles

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
.summarize_draw_vector(x, probs, warn = TRUE)
```

## Arguments

- x:

  Numeric vector of posterior draws.

- probs:

  Quantile probabilities.

- warn:

  Whether to report dropped draws. Callers that summarize many vectors
  set this to `FALSE` and report once over the whole set instead.

## Value

Named numeric vector:
`c(mean, sd, <named quantiles>, n_draws, n_draws_used)`. The last two
are the draw accounting: how many draws the summary was offered and how
many it used, so a summary built on a third of its chain says so
wherever it ends up. They differ exactly when NA or NaN draws were
dropped.
