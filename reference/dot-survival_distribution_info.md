# Resolve survival distribution metadata

Resolve survival distribution metadata

## Usage

``` r
.survival_distribution_info(distribution = NULL)
```

## Arguments

- distribution:

  One of the survival distribution strings (default `"weibull"`).

## Value

A list with the distribution name, `kind` (`"parametric"`/`"flexible"`),
integer `dist_code` (1-9 for parametric, `NA` for flexible),
`mspline_degree`, `is_ph` (proportional hazards flag), `n_aux` (number
of shape parameters), and the Stan model `stan_prefix`.
