# Modular exponentiation, vectorized over the exponent

Modular exponentiation, vectorized over the exponent

## Usage

``` r
.mod_pow(base, e, p)
```

## Arguments

- base:

  A single non-negative integer below `p`.

- e:

  Non-negative integer exponents.

- p:

  The modulus, below `2^26`.

## Value

`base^e mod p`, one value per exponent.
