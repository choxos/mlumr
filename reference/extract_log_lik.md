# Extract the full pointwise log-likelihood matrix from an mlumr_fit

Combines the IPD and AgD per-observation log-likelihood draws into a
single matrix with one row per posterior draw and one column per
observation. The result is suitable for direct use with
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) /
[`loo::waic()`](https://mc-stan.org/loo/reference/waic.html).

## Usage

``` r
extract_log_lik(object)
```

## Arguments

- object:

  An `mlumr_fit` object.

## Value

A numeric matrix of dimension `n_draws x (n_ipd + n_agd_rows)`. IPD
columns come first, then AgD rows.
