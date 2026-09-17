# Singular-value geometry of the subgroup mean profiles

Rows are centered and divided by the IPD SDs, so a covariate measured in
large units cannot dominate by units alone. Returns `cond_inv` (smallest
over largest singular value), `spread` (RMS distance of the rows from
their center along the dominant direction, in IPD SDs),
`singular_values` and the scaled `means`. A design that cannot be
decomposed reports zero geometry, as
[`.profile_rank()`](https://choxos.github.io/mlumr/reference/dot-profile_rank.md)
does.

## Usage

``` r
.subgroup_geometry(means, ref_sd)
```
