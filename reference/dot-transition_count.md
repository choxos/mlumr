# Read a transition count that the backend may not have supplied

Zero is the count that says the sampler behaved, so an unreported,
fractional or out-of-range value is `NA` rather than 0.

## Usage

``` r
.transition_count(x)
```

## Arguments

- x:

  The recorded count.

## Value

A non-negative integer, or `NA_integer_` when unknown.
