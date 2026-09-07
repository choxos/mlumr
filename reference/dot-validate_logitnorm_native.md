# Validate native logit-normal `mu` / `sigma`

The moment parameterization has always been checked; the native one was
passed through untouched, so one invalid `sigma` produced three
different answers depending on which function saw it. `sigma = 0` gave
`Inf` from
[`dlogitnorm()`](https://choxos.github.io/mlumr/reference/logitNormal.md),
`1` from
[`plogitnorm()`](https://choxos.github.io/mlumr/reference/logitNormal.md)
and `0.5` from
[`qlogitnorm()`](https://choxos.github.io/mlumr/reference/logitNormal.md),
all finite and none flagged, while `sigma = -1` gave `NA` from the
density and `NaN` with a base warning from the other two. A degenerate
spike is not a distribution
[`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
can draw from, so this is an error in both parameterizations rather than
a value that silently propagates.

## Usage

``` r
.validate_logitnorm_native(mu, sigma)
```

## Arguments

- mu, sigma:

  Logit-scale location and scale.

## Value

A list with validated `mu` and `sigma`.
