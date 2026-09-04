# Compute links of population-standardized response means

Internal helper for
[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
with `type = "link"`. Uses the log-scale generated quantities that
underlie the marginal response means so the result remains finite when
the natural-scale mean rounds to 0 or overflows.

## Usage

``` r
.compute_marginal_link(object, pred_cols)
```

## Arguments

- object:

  An `mlumr_fit` object.

- pred_cols:

  Character vector of response-scale prediction column names.

## Value

Data frame with the same column names as `pred_cols`, on the marginal
link scale.
