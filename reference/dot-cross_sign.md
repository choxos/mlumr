# The exact sign of a two-by-two cross difference, or NA

`c1 d2 - c2 d1` where each operand carries the error its own subtraction
left, so the quantity whose sign is wanted is
`(c1 + e_c1)(d2 + e_d2) - (c2 + e_c2)(d1 + e_d1)`. Expanding that
exactly is sixteen terms; the cheap four-term sign is taken instead and
accepted only where the four-term value provably dominates what the
errors can contribute. Genuine cancellation at the last bits is left
undecided.

## Usage

``` r
.cross_sign(c1, d1, c2, d2, e1 = 0, f1 = 0, e2 = 0, f2 = 0)
```

## Arguments

- c1, d1, c2, d2:

  The four operands.

- e1, f1, e2, f2:

  Their respective errors, `0` where a value is exact.

## Value

`-1`, `0`, `1`, or `NA` where the arithmetic could not settle it.
