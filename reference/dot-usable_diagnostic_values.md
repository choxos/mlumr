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
.usable_diagnostic_values(x)
```

## Arguments

- x:

  A summary column.

## Value

A list with `values` (every number, infinities included) and `n_missing`
/ `n_total` counts.
