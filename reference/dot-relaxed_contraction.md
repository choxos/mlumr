# Marginal posterior variance change for comparator coefficients

For each `beta_comparator` coefficient this reports
`1 - posterior_variance / prior_variance`. A positive value means the
marginal posterior SD is smaller than the marginal prior SD, zero means
the two SDs are equal, and a negative value means the posterior SD is
larger. This descriptive variance comparison is not a fraction of
information learned, an identification test, or a decomposition of data
and prior influence. It ignores changes in location, shape, and
posterior correlation.

## Usage

``` r
.relaxed_contraction(object)
```

## Arguments

- object:

  An `mlumr_fit` from `model = "relaxed"`.

## Value

A data frame with one row per covariate (`covariate`, `prior_sd`,
`posterior_sd`, `contraction`), or `NULL` if unavailable. The retained
`contraction` column is the marginal variance change defined above and
is `NA` where the prior has no finite variance.

## Details

The ratio is only interpretable against a prior with a finite variance.
`prior_sd` is therefore the prior STANDARD DEVIATION, not the stored
scale: for a Student-t prior they differ by `sqrt(df / (df - 2))`, and
for `df <= 2` (including the `df = 1` Cauchy) no finite variance exists
and `contraction` is `NA` rather than a number that would misstate what
was learned.
