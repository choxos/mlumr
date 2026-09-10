# Report posterior draws dropped from a summary

The `na.rm = TRUE` in
[`.summarize_draw_vector()`](https://choxos.github.io/mlumr/reference/dot-summarize_draw_vector.md)
is deliberate: one bad draw should not erase an otherwise usable
summary. It removes those draws without a trace, though, so a mean taken
over a third of the chain reads exactly like a mean taken over all of
it. Say what was dropped and leave the judgment to the reader. The
warning is for the session; the `n_draws` and `n_draws_used` columns on
every summary are what travels with the result.

## Usage

``` r
.warn_dropped_draws(draws)
```

## Arguments

- draws:

  Numeric vector, matrix or data frame of posterior draws.

## Value

`TRUE` if a warning was issued, `FALSE` otherwise, invisibly.

## Details

Only NA and NaN are counted, because only those are what `na.rm`
removes. An infinite draw propagates into the mean and is visible on its
own.
