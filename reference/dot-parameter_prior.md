# Prior a fitted parameter was actually given

[`plot_prior_posterior()`](https://choxos.github.io/mlumr/reference/plot_prior_posterior.md)
drew `prior_intercept` over every parameter, so `pars = "sigma"` was
overlaid with a symmetric normal that puts mass on impossible negative
values. Each parameter is mapped to the prior the fit records for it,
with the Stan `<lower=0>` constraint carried along so a constrained
parameter gets the truncated density rather than the full one.

## Usage

``` r
.parameter_prior(object, par)
```

## Arguments

- object:

  An `mlumr_fit`.

- par:

  One draw column name.

## Value

A list with `prior` (a prior specification) and `lower` (the support
bound), or `NULL` when the fit records no prior for that parameter.
