# The stretches of time a study actually had someone under observation

The union of each subject's `[entry, exit]`, merged, so a gap with an
empty risk set is not treated as observed. Without entry times this is
one interval from zero.

## Usage

``` r
.risk_intervals(entry, exit, observed_max)
```

## Arguments

- entry:

  Entry times, or `NULL`.

- exit:

  Exit times, or `NULL`.

- observed_max:

  Last observed time, used when `exit` is absent.

## Value

A list of `c(lo, hi)` intervals, in increasing order.
