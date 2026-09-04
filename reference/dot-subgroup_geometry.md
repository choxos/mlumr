# Singular-value geometry of a set of subgroup mean vectors

Singular-value geometry of a set of subgroup mean vectors

## Usage

``` r
.subgroup_geometry(means, ref_sd)
```

## Arguments

- means:

  Matrix of aggregate subgroup covariate means, rows by covariates.

- ref_sd:

  Reference SD per covariate (the IPD SDs), used to put the columns on a
  common scale. Scaling by the spread of the MEANS instead would rescale
  a covariate whose subgroup means barely move up to the same footing as
  one that swings from 0 to 1, hiding the very collapse being measured.

## Value

A list with `cond_inv`, `eff_dim`, `spread`, `singular_values`, `means`.
