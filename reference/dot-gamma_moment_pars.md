# Gamma shape and rate from a mean and a standard deviation

Returns `NULL` when neither moment was supplied, so the caller falls
through to the native parameterization.

## Usage

``` r
.gamma_moment_pars(no_mean, no_sd, mean, sd)
```

## Arguments

- no_mean, no_sd:

  Whether the caller's `mean` / `sd` were missing.

- mean, sd:

  The supplied moments, or `NULL`.

## Value

A list with `shape` and `rate`, or `NULL`.
