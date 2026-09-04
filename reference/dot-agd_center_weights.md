# Population weights for the AgD rows used in covariate centering

Returns one weight per aggregate row, taken from the family's comparator
weight field (`n_agd`, `agd_weight`, `E_agd`), or the pseudo-individual
count for survival. Falls back to equal weights when no usable field is
present. Weights must be positive and finite, and must sum over a split
subgroup to the same total as the unsplit one, which is what makes the
center invariant to how the aggregate evidence is tabulated.

## Usage

``` r
.agd_center_weights(stan_data, family, n_agd_rows)
```

## Arguments

- stan_data:

  The assembled Stan data list.

- family:

  Outcome family name.

- n_agd_rows:

  Number of aggregate rows.

## Value

Numeric vector of length `n_agd_rows`.
