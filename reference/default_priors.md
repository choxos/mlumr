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
```

## Value

A prior list (see
[`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md)).

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
#> [1] "0.1.0"
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
#> [1] "0.1.0"
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
#> [1] "0.1.0"
#> 
```
