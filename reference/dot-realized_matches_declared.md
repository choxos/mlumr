# Does the realized integration design reproduce the declared one?

Compares each row's realized integration means with its declared
`<covariate>_mean` values, in reference SDs. A finite grid misses its
declared mean by a few hundredths of an SD, while a
[`distr()`](https://choxos.github.io/mlumr/reference/distr.md) that
ignores its row misses by the whole distance to whatever it was given,
so a quarter of an SD separates the two. `TRUE` when there is nothing to
compare, or when the two cannot be compared (with a warning).

## Usage

``` r
.realized_matches_declared(
  declared,
  realized,
  ref_sd = NULL,
  max_location_gap = 0.25
)
```

## Arguments

- declared:

  Matrix of declared mean profiles, rows by covariates.

- realized:

  Matrix of realized integration means, or `NULL`.

- ref_sd:

  Reference SD per covariate; `NULL` falls back to each declared
  column's range.

- max_location_gap:

  Largest per-row distance, in reference SDs, that still counts as a
  match.
