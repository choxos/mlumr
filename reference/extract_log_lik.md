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

A numeric matrix of dimension `n_draws x (n_ipd + n_agd)`. IPD columns
come first, then the AgD columns. The AgD pointwise unit is whatever the
family's Stan model emits as `log_lik_agd`: for binomial / normal /
poisson this is **one column per aggregate row**; for **survival** it is
**one column per reconstructed pseudo-individual** (not per aggregate
row), so survival LOO/WAIC operate at the pseudo-individual level. See
the notes on
[`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md)
/
[`calculate_waic()`](https://choxos.github.io/mlumr/reference/calculate_waic.md).
