# The rounding error of a floating-point sum, exactly

Knuth's TwoSum. For `s = a + b` the returned `e` satisfies
`a + b = s + e` exactly, with no assumption about the relative
magnitudes. `e == 0` says the addition was exact, which is the only
thing this file asks of it.

## Usage

``` r
.two_sum_err(a, b, s)
```

## Arguments

- a, b:

  The operands.

- s:

  Their computed sum.

## Value

The exact rounding error.
