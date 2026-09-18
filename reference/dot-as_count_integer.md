# Coerce a validated count to integer without truncating

The count validators accept values within `sqrt(.Machine$double.eps)` of
a whole number, and
[`as.integer()`](https://rdrr.io/r/base/integer.html) truncates, so
round first.

## Usage

``` r
.as_count_integer(x)
```

## Arguments

- x:

  A numeric vector that has passed a whole-number count check.

## Value

An integer vector.
