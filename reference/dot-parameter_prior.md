# Prior a fitted parameter was actually given

Each parameter is mapped to the prior the fit records for it, with the
Stan `<lower=0>` constraint carried along for the truncated density.

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
