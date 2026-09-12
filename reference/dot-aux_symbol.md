# Name the auxiliary parameter as a bare symbol

[`.aux_name()`](https://choxos.github.io/mlumr/reference/dot-aux_name.md)
returns a noun phrase, which reads correctly in a sentence and not
inside a formula: "the marginal behaves as `(1 / the Weibull shape)^1`".
This is the same parameter written as the symbol a formula needs.

## Usage

``` r
.aux_symbol(distribution)
```

## Arguments

- distribution:

  The resolved survival distribution.

## Value

A single string.
