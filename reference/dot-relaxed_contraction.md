# Marginal posterior variance change for comparator coefficients

For each `beta_comparator` coefficient this reports
`1 - posterior_variance / prior_variance`, a descriptive comparison of
marginal SDs and not an identification test. `prior_sd` is the prior
standard deviation, `NA` for a Student-t prior with `df <= 2`.

## Usage

``` r
.relaxed_contraction(object)
```

## Arguments

- object:

  An `mlumr_fit` from `model = "relaxed"`.

## Value

A data frame with one row per covariate (`covariate`, `prior_sd`,
`posterior_sd`, `contraction`), or `NULL` if unavailable.
