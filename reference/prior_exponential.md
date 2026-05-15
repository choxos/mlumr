# Specify an exponential prior

Exponential prior on a positive scalar. Currently supported for
[`prior_sigma`](https://choxos.github.io/mlumr/reference/mlumr.md)
(normal-family residual SD) only; rejected for unconstrained intercepts
and regression coefficients. `prior_exponential(rate = 1)` has mean 1
and is a reasonable weakly-informative choice when the outcome is
standardized.

## Usage

``` r
prior_exponential(rate = 1)
```

## Arguments

- rate:

  Rate parameter (default 1). Larger `rate` = tighter prior concentrated
  near zero.

## Value

A list with components `distribution = "exponential"`, `rate`, and
placeholders (`mean = 0`, `sd = 1 / rate`) so the same Stan-field
translation works.

## Examples

``` r
prior_exponential(rate = 1)
#> $distribution
#> [1] "exponential"
#> 
#> $rate
#> [1] 1
#> 
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 1
#> 
#> $df
#> [1] NA
#> 
#> $autoscale
#> [1] FALSE
#> 
```
