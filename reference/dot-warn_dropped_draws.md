# Report posterior draws dropped from a summary

[`.summarize_draw_vector()`](https://choxos.github.io/mlumr/reference/dot-summarize_draw_vector.md)
drops NA and NaN draws with `na.rm = TRUE`, so a summary built on part
of the chain would otherwise read like one built on all of it. An
infinite draw propagates into the mean and is not counted.

## Usage

``` r
.warn_dropped_draws(draws)
```

## Arguments

- draws:

  Numeric vector, matrix or data frame of posterior draws.

## Value

`TRUE` if a warning was issued, `FALSE` otherwise, invisibly.
