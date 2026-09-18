# Split a diagnostic column into usable values and missing ones

Keeps an infinite value, which is a diagnostic that came out as bad as
it can, and counts a missing one rather than dropping it. An absent or
non-numeric column counts as `n_expected` missing diagnostics.

## Usage

``` r
.usable_diagnostic_values(x, n_expected = length(x))
```

## Arguments

- x:

  A summary column, possibly `NULL`.

- n_expected:

  How many parameters should have had a diagnostic.

## Value

A list with `values`, `n_missing`, `n_total` and `missing_idx`.
