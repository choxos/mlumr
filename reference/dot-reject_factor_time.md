# Convert a time column, refusing a factor

[`as.numeric()`](https://rdrr.io/r/base/numeric.html) on a factor
returns its internal level codes, not the printed numbers, and the small
positive integers that produces look like plausible times: they pass
every later check. The column route guarded its own times this way; the
`Surv` route coerced `entry_time` directly, so a factor entry column
became left-truncation times of 1, 2, 3 and the likelihood was
conditioned on the wrong risk sets.

## Usage

``` r
.reject_factor_time(x, nm)
```

## Arguments

- x:

  The column.

- nm:

  Its name, for the message.

## Value

A numeric vector.
