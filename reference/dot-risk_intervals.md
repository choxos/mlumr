# The stretches of time a study actually had someone under observation

The union of each subject's `[entry, exit]`, merged. Reducing this to a
single span from the earliest entry to the last exit would treat a gap
with an empty risk set as observed: subjects seen on `[1, 2]` and
`[8, 9]` leave `(2, 8)` contributing no event hazard and no
cumulative-hazard exposure, and a basis column living only there is as
unidentified as one before the first entry. Without entry times this is
a single interval from zero, which is what keeps the check unchanged for
ordinary data.

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
