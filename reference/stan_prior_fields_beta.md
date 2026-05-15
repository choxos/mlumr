# Translate a `prior_beta` spec to vector-valued Stan data fields

The Stan models declare `prior_beta_mean` and `prior_beta_sd` as
`vector[n_cov]` so the same data contract supports:

## Usage

``` r
stan_prior_fields_beta(prior, n_cov, sd_x = NULL, covariate_names = NULL)
```

## Arguments

- prior:

  A prior list from
  [`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md)
  /
  [`prior_student_t()`](https://choxos.github.io/mlumr/reference/prior_student_t.md)
  /
  [`prior_cauchy()`](https://choxos.github.io/mlumr/reference/prior_cauchy.md),
  OR a list of such priors of length `n_cov`.

- n_cov:

  Number of covariates.

- sd_x:

  Optional numeric vector of covariate SDs (length `n_cov`). Required
  when any prior has `autoscale = TRUE`; ignored otherwise.

- covariate_names:

  Optional character vector of covariate names (length `n_cov`). Used
  only to produce informative warnings when `autoscale = TRUE` meets a
  zero-SD covariate.

## Value

A list with numeric vectors `mean` and `sd` (length `n_cov`) and scalars
`dist`, `df`, and a logical vector `autoscale` recording which elements
were autoscaled (for
[`prior_summary()`](https://choxos.github.io/mlumr/reference/prior_summary.md)).

## Details

- \(i\) a single prior broadcast to all covariates (scalar user input),

- \(ii\) per-coefficient priors (a list of prior lists, length `n_cov`),

- \(iii\) autoscaling: each covariate's scale is divided by `sd(x_j)` so
  the prior is weakly-informative on the standardized scale.

For a list of per-coefficient priors, all elements must use the same
`distribution` family and `df` (Stan branches on a single dist code).
