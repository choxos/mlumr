# Label indexed beta rows with covariate names for display

Rewrites `beta[1]` to `beta[age]` (and `beta_index[1]`,
`beta_comparator[1]` likewise) in the printed copy, as multinma does.

## Usage

``` r
.label_beta_rows(df, covariates)
```

## Arguments

- df:

  A slice of `fit$summary`.

- covariates:

  Character vector of covariate names, in model order.

## Value

`df` with its `variable` column relabeled where possible.
