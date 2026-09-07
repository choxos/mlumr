# Coerce a validated count to integer without truncating

[`as.integer()`](https://rdrr.io/r/base/integer.html) truncates toward
zero, while the count validators accept any value within
`sqrt(.Machine$double.eps)` of a whole number. Those two rules disagree:
`0.999999999` is accepted as the count 1 and then coerced to 0, so the
package silently changed an event count it had just approved. Rounding
first maps an accepted value onto the integer it was accepted FOR, and
is a no-op for values that were already exact.

## Usage

``` r
.as_count_integer(x)
```

## Arguments

- x:

  A numeric vector that has passed a whole-number count check.

## Value

An integer vector.
