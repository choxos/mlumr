# Can the aggregate data identify the comparator coefficients?

In the relaxed model the comparator coefficients `beta_comparator` are
informed only by the aggregate rows. With `K` covariates there are
`K + 1` comparator parameters, so at least `K + 1` distinct aggregate
rows are needed, and under an identity link the rows must also differ in
every covariate direction.

## Usage

``` r
check_identification(x, verbose = TRUE, link = NULL)
```

## Arguments

- x:

  An `mlumr_data` object or a fitted relaxed `mlumr_fit`.

- verbose:

  Print a readable report (default `TRUE`).

- link:

  Planned link for an unfitted data object. Defaults to the family
  default. A fitted object always uses its stored link.

## Value

Invisibly, a list with `n_rows`, `n_distinct` (rows that do not repeat
another's integration grid), `n_cov`, `n_rows_needed` (`K + 1`),
`cond_inv`, `eff_dim`, `spread`, `singular_values`, `means` (the scaled,
centered subgroup mean matrix), `diagnostic_scope` (`"identity"` or
`"descriptive"`) and `flagged`.

## Details

The subgroup mean profiles are centered, divided by the IPD covariate
SDs and decomposed. `cond_inv` is the ratio of the smallest to the
largest singular value and goes to 0 as the rows collapse onto a
lower-dimensional set. `eff_dim` is the participation ratio of the
squared singular values, the number of directions the rows effectively
spread along, from 1 to `K`; it is 0 when the rows do not vary or cannot
be decomposed. `spread` is the RMS distance of the rows from their
center along the dominant direction, in IPD SDs; it supplies the
absolute scale `cond_inv` lacks. For a normal identity-link model the
subgroup means are the aggregate design and the screen flags
`cond_inv < 0.2` or `spread < 0.05`, which are package heuristics. For
other links the integrated response also depends on each row's covariate
distribution, so the geometry is descriptive only and `flagged` is `NA`
unless there are too few rows. Reconstructed survival curves are
refused, since a curve is not one scalar summary per row. Neither
measure sees subgroup sizes or outcome precision, so confirm any verdict
with the coefficient posterior and
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md).
The subgroup-identification vignette works through the cases.

## See also

[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) for
`model = "relaxed"`;
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md).

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- add_integration(combine_data(ipd, agd), n_int = 64, ...)
check_identification(dat)
} # }
```
