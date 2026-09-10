# Auxiliary (shape) draws for one treatment

Layouts in order, in the same spirit as
[`.surv_scoef_draws()`](https://choxos.github.io/mlumr/reference/dot-surv_scoef_draws.md):
`aux_val` / `aux_val_cmp` the named per-treatment views `aux_raw[1,s]`
the underlying parameter matrix The second exists because the views are
TRANSFORMED parameters, so a fit made with
`pars = c("aux_val", "aux_val_cmp"), include = FALSE` drops them and
keeps the raw matrix they are read off. Both are consulted for the
treatment asked about before anything else is: the comparator falls back
to the index view only when there is a single stratum, where Stan makes
them the same number. Under `aux_by = ".study"` they are different
studies' shapes.

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

## Details

A shape of 1 is returned only where Stan itself fixes it at 1: a
distribution that has no such shape, where the raw matrix has no rows at
all. That is `dist` 1 and 4 for the first shape and every `dist` but 9
for the second. Substituting 1 for a shape that merely could not be
found gives a different distribution without saying so: at `dist` 2 it
turns a Weibull into an exponential.
