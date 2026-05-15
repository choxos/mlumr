# Translate a scalar prior spec to the Stan data fields

Stan scalar-prior fields: `prior_*_mean`, `prior_*_sd`, `prior_*_dist`
(0 = normal, 1 = student_t, 2 = exponential for sigma only),
`prior_*_df` (used only when dist == 1; positive placeholder otherwise).

## Usage

``` r
stan_prior_fields(prior)
```

## Arguments

- prior:

  A prior list.

## Value

A list with `mean`, `sd`, `dist`, `df`.
