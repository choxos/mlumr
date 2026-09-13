# The sign of an exact sum of doubles

The same expansion as
[`.exact_sum_is_zero()`](https://choxos.github.io/mlumr/reference/dot-exact_sum_is_zero.md),
answering which side of zero the sum falls on rather than only whether
it is zero. The components come out non-overlapping and in increasing
magnitude, so the LAST nonzero one carries the sign: everything below it
is too small to reach its bits, let alone cross zero.

## Usage

``` r
.exact_sum_sign(terms)
```

## Arguments

- terms:

  As for
  [`.exact_sum_is_zero()`](https://choxos.github.io/mlumr/reference/dot-exact_sum_is_zero.md).

## Value

`-1`, `0` or `1`, or `NA` where any term is not finite.

## Details

This is what a comparison between two exact quantities needs. Deciding
whether a candidate slope `p / q` satisfies `c <= beta * d` by computing
the division and comparing is a floating-point solve, and a solve cannot
certify anything; cross-multiplying turns it into the sign of
`p * d - c * q`, which this answers exactly.
