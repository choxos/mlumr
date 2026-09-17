# Refuse a missing grouping identifier

A missing identifier matches no rows, so the arm summary would be all
NA.

## Usage

``` r
.require_identity(x, nm, as_char = TRUE)
```

## Arguments

- as_char:

  Return `as.character(x)` rather than `x`.
