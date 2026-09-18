# Refuse a shared baseline whose weights float against the study intercepts

With `aux_by = "none"` the model carries one weight simplex and a
separate intercept per study, so if the studies' exposure falls on
disjoint sets of basis columns, mass can be moved between the sets and
absorbed exactly by the intercepts while the treatment contrast moves
freely. What rules it out is that the studies and the columns they touch
form one connected component. Connectivity is exact for a degree-0 basis
and necessary rather than sufficient above it.

## Usage

``` r
.assert_shared_basis_identified(spec, studies)
```

## Arguments

- spec:

  A basis spec from
  [`.build_mspline_basis()`](https://choxos.github.io/mlumr/reference/dot-build_mspline_basis.md).

- studies:

  A named list; each element a list with `observed_max`, `entry`, `exit`
  and `event`.

## Value

`TRUE`, invisibly.
