# Split a diagnostic column into usable values and missing ones

Unlike
[`.finite_numeric_values()`](https://choxos.github.io/mlumr/reference/dot-finite_numeric_values.md)
this KEEPS an infinite value. The two cases it separates are not the
same thing: `Inf` is a diagnostic that was computed and came out as bad
as it can be, while `NA` or `NaN` is a parameter that has no diagnostic
at all, which happens legitimately for a quantity that is constant
across every draw. Filtering both away left a worst-case statistic that
could not report the worst case, and reported a benign number in its
place.

## Usage

``` r
.usable_diagnostic_values(x, n_expected = length(x))
```

## Arguments

- x:

  A summary column, possibly `NULL`.

- n_expected:

  How many parameters should have had a diagnostic. Defaults to the
  length of `x`, which is right whenever the column is present.

## Value

A list with `values` (every number, infinities included) and `n_missing`
/ `n_total` counts.

## Details

A column that is absent, or present but not numeric, is not zero
diagnostics either. `c(NA, NA)` is a LOGICAL vector in R, so a backend
that wrote missing values into a column it never filled produced two
unavailable diagnostics, and this reported none: `n_total` came back 0,
the reporter below says nothing when the total is 0, and the summary
printed no line at all. `n_expected` is what the caller knows the count
should be, normally the number of rows in the summary, so an absent
column is reported as entirely missing rather than as an empty
population of parameters.
