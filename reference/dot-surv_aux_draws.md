# Auxiliary (shape) draws for one treatment

Reads the named per-treatment view (`aux_val`, `aux_val_cmp`) or the raw
matrix `aux_raw[1,s]` it is derived from. With one stratum either view
serves either treatment; with two they are different studies' shapes and
are never crossed. A shape of 1 is returned only where Stan fixes it at
1, never as a substitute for one that could not be found.

## Usage

``` r
.surv_aux_draws(object, base, treatment, n)
```

## Arguments

- object:

  A fitted `mlumr_fit`.

- base:

  `"aux_val"` or `"aux2_val"`.

- treatment:

  `"index"` or `"comparator"`.

- n:

  Number of draws, for the fixed-at-one case.

## Value

Numeric vector of `n` draws.
