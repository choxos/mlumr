# Default priors used by [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md)

These accessors return the current default priors used by
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md), tagged
with `$default = TRUE` and the package version.
[`prior_summary()`](https://choxos.github.io/mlumr/reference/prior_summary.md)
prints the version so cross-release reproducibility is diagnosable: if a
later release changes a default, fits produced with an older version
will still carry the correct `$version` tag.

## Usage

``` r
default_prior_intercept()

default_prior_beta()

default_prior_sigma()

default_prior_aux()

default_prior_smooth()
```

## Value

A prior list (see
[`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md)).

## Details

`default_prior_aux()` and `default_prior_smooth()` apply to the survival
family only. `prior_aux` is a half-normal(0, 2) on the shape/scale
parameter(s) of parametric survival distributions
(Weibull/Gompertz/gamma shape, log-normal sdlog, generalized-gamma
shapes). `prior_smooth` is a half-normal(0, 1) on the random-walk
smoothing SD of the M-spline / piecewise-exponential baseline hazard.

## Examples

``` r
default_prior_intercept()
#> $distribution
#> [1] "normal"
#> 
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 10
#> 
#> $df
#> [1] NA
#> 
#> $autoscale
#> [1] FALSE
#> 
#> $default
#> [1] TRUE
#> 
#> $version
#> [1] "0.1.0.9000"
#> 
default_prior_beta()
#> $distribution
#> [1] "normal"
#> 
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 2.5
#> 
#> $df
#> [1] NA
#> 
#> $autoscale
#> [1] FALSE
#> 
#> $default
#> [1] TRUE
#> 
#> $version
#> [1] "0.1.0.9000"
#> 
default_prior_sigma()
#> $distribution
#> [1] "normal"
#> 
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 2.5
#> 
#> $df
#> [1] NA
#> 
#> $autoscale
#> [1] FALSE
#> 
#> $default
#> [1] TRUE
#> 
#> $version
#> [1] "0.1.0.9000"
#> 
```
