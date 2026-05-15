# Add numerical integration points

Generate quasi-Monte Carlo integration points using Sobol sequences and
a Gaussian copula to account for correlations between covariates in the
AgD.

## Usage

``` r
add_integration(
  data,
  n_int = 64,
  cor = NULL,
  cor_adjust = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  An `mlumr_data` object from
  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md)

- n_int:

  Number of integration points (default 64, powers of 2 recommended)

- cor:

  Correlation matrix for covariates. If `NULL`, computed from IPD.

- cor_adjust:

  Adjustment method: `"spearman"`, `"pearson"`, or `"none"`

- verbose:

  Logical; if `FALSE`, suppresses progress messages.

- ...:

  Distribution specifications for each covariate using
  [`distr()`](https://choxos.github.io/mlumr/reference/distr.md)

## Value

An `mlumr_data` object with integration points added

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- add_integration(
  dat,
  n_int = 64,
  x1 = distr(qnorm, mean = x1_mean, sd = x1_sd),
  x2 = distr(qbern, prob = x2_mean)
)
} # }
```
