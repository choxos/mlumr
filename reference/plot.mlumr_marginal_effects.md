# Forest plot of population-standardized marginal effects

Plots the point estimate and credible interval for each effect measure,
grouped by target population (index / comparator). The interval's
coverage is read from the quantile columns present, so it matches the
`probs` the result was summarized with. Panels showing only ratio
measures are drawn on a log axis, where reciprocal effects sit at equal
distances from the null. The mlumr analogue of
`plot(multinma::relative_effects(fit))`.

## Usage

``` r
# S3 method for class 'mlumr_marginal_effects'
plot(x, ref_line = NULL, ...)
```

## Arguments

- x:

  A
  [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
  result.

- ref_line:

  Numeric null-effect reference line. By default 0 for difference/log
  measures and 1 for natural ratio measures (RR), drawn per facet. Pass
  a single value to override for all panels.

- ...:

  Unused.

## Value

A `ggplot` object.

## See also

[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plot(marginal_effects(fit, effect = "all"))
} # }
```
