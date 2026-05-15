# Compute mean linear predictors from parameter draws

Internal helper for predict.mlumr_fit with type="link". Computes
mean(eta_i) directly from parameter draws instead of applying the link
to marginalized response-scale draws, avoiding Jensen's inequality bias.

## Usage

``` r
.compute_mean_lp(object, pred_cols)
```

## Arguments

- object:

  An mlumr_fit object

- pred_cols:

  Character vector of column names to compute

## Value

Data frame with the same columns as pred_cols, on the link scale
