# Specify a Student-t prior

Heavier-tailed alternative to
[`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md).
A Student-t with moderate degrees of freedom can be a robust weakly
informative starting family, but its scale still requires calibration to
the link, outcome, and predictor scaling.

## Usage

``` r
prior_student_t(df = 5, mean = 0, sd = 2.5, autoscale = FALSE)
```

## Arguments

- df:

  Degrees of freedom (must be positive).

- mean:

  Prior location (default 0).

- sd:

  Prior scale (default 2.5).

- autoscale:

  See
  [`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md).
  Default `FALSE`.

## Value

A list with components `distribution = "student_t"`, `df`, `mean`, `sd`,
`autoscale`.

## Examples

``` r
# A moderately heavy-tailed coefficient prior
prior_student_t(df = 5, mean = 0, sd = 2.5)
#> $distribution
#> [1] "student_t"
#> 
#> $mean
#> [1] 0
#> 
#> $sd
#> [1] 2.5
#> 
#> $df
#> [1] 5
#> 
#> $autoscale
#> [1] FALSE
#> 
```
