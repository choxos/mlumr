# Label indexed beta rows with covariate names for display

Rewrites `beta[1]` to `beta[age]` (and the relaxed model's
`beta_index[1]` / `beta_comparator[1]` likewise) so printed coefficient
tables name the covariate instead of its position, matching the
`beta[age]` idiom used by `multinma`. Display only: the underlying
`variable` strings in `fit$summary` are unchanged, so code that indexes
on `beta[1]` keeps working.

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
