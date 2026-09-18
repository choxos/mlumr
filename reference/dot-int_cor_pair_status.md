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

List with `expected` (pairs that have a correlation to realize),
`measured` and `measured_resolution` (how many had a finite correlation
on the doubled grid, and on both grids), `omitted` (the expected pairs
that fell short, with a reason), `not_applicable` (pairs in which a
margin is declared with no variance) and `applicable`, the logical
vector the maxima are taken over.
