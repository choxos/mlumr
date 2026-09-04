# Plot covariate-conditional treatment effects

Point-interval of the conditional effect at each covariate profile.

## Usage

``` r
# S3 method for class 'mlumr_conditional_effects'
plot(x, ref_line = NULL, ...)
```

## Arguments

- x:

  A
  [`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
  result.

- ref_line:

  Numeric null-effect reference line. By default it is chosen per facet
  from the effect label: 1 for the natural-ratio measures (RR, HR, TR,
  and the exponentiated contrast reported when the study baselines
  differ) and 0 for the additive ones (RD, MD, LINK_EFFECT). Pass a
  single value to override for all panels. A fixed 0 was previously
  drawn for every effect, which put the null line off-scale on every
  ratio panel.

- ...:

  Unused.

## Value

A `ggplot` object.

## See also

[`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
