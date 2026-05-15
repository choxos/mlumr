# Specify a Student-t prior

Heavier-tailed alternative to
[`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md).
For logistic-regression coefficients with unit-scale predictors, the
Stan community prior-choice recommendations suggest Student-t with `df`
between 3 and 7 as a robust weakly informative prior (Gelman et al.,
2008).

## Usage

``` r
prior_student_t(df = 5, mean = 0, sd = 2.5, autoscale = FALSE)
```

## Arguments

- df:

  Degrees of freedom (must be positive). Values in `3:7` are recommended
  for logistic-regression coefficients.

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
# Gelman et al. 2008 recommendation for logistic-regression coefficients
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
