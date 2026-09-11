# Which correlation pairs were measured, and why the others were not

Which correlation pairs were measured, and why the others were not

## Usage

``` r
.int_cor_pair_status(diff, stats, target_sd)
```

## Arguments

- diff:

  The pair table from
  [`.int_cor_stats()`](https://choxos.github.io/mlumr/reference/dot-int_cor_stats.md).

- stats:

  The marginal statistics of the current grid.

- target_sd:

  Declared target SDs, one per row of `stats`.

## Value

List with `expected`, the number of pairs that have a correlation to
realize; `measured`, how many of those had a finite correlation on the
doubled grid, which is what the target comparison reads;
`measured_resolution`, how many had one on both grids, which is what the
current-versus-doubled comparison reads; `omitted`, a data frame naming
the expected pairs that fell short of either, with a `reason`,
`"constant_on_grid"` when the doubled grid did not vary a margin and
`"constant_on_current_grid"` when only the current one did not, both
resolution failures a larger grid may or may not repair; and
`not_applicable`, the pairs in which a margin is declared with no
variance and so has no correlation to realize at any resolution. Those
are outside `expected`, so a subgroup row with an all-male membership
does not keep every verdict at `partial` forever; and `applicable`, the
logical over the rows of `diff` that the maxima are taken over.
