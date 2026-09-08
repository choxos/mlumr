# Read a transition count that the backend may not have supplied

[`.diagnostic_count()`](https://choxos.github.io/mlumr/reference/dot-diagnostic_count.md)
maps anything unusable to 0 and its callers guard that separately.
Divergence and treedepth counts have no such guard, and 0 is the answer
that says the sampler behaved, so an unreported count has to stay
unknown instead.

## Usage

``` r
.transition_count(x)
```

## Arguments

- x:

  The recorded count.

## Value

A non-negative integer, or `NA_integer_` when unknown.
