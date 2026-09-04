# Specify a Cauchy prior

Cauchy is Student-t with `df = 1`; this constructor is a convenience
wrapper around
[`prior_student_t()`](https://choxos.github.io/mlumr/reference/prior_student_t.md).
It has very heavy tails and should be used with care; modern
recommendations generally prefer `prior_student_t(df in 3:7, ...)` over
Cauchy for regression coefficients to keep sampling well-behaved (see
Piironen & Vehtari on the horseshoe; Ghosh et al. 2015).

## Usage

``` r
prior_cauchy(mean = 0, sd = 2.5, autoscale = FALSE)
```

## Arguments

- mean:

  Prior location (default 0).

- sd:

  Prior scale (default 2.5).

- autoscale:

  See
  [`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md).
  Default `FALSE`.

## Value

A list with components `distribution = "student_t"`, `df = 1`, `mean`,
`sd`, `autoscale`.

## Examples

``` r
prior_cauchy(mean = 0, sd = 2.5)
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
#> [1] 1
#> 
#> $autoscale
#> [1] FALSE
#> 
```
