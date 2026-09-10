# Exact keys for design rows

The binary representation of every element, so two rows compare equal
exactly when they are the same numbers.
[`paste()`](https://rdrr.io/r/base/paste.html) on doubles keeps fifteen
digits and could merge two rows that differ. A negative zero is the same
number as a positive one, and every fit treats it so, but `%a` spells
the two differently; adding zero to each element folds them together and
changes nothing else.

## Usage

``` r
.row_keys(X)
```

## Arguments

- X:

  Design matrix.

## Value

Character vector, one key per row.
