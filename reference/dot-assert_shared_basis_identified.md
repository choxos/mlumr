# Refuse a shared baseline whose weights float against the study intercepts

[`.assert_basis_support()`](https://choxos.github.io/mlumr/reference/dot-assert_basis_support.md)
asks whether every column is live SOMEWHERE in the pooled risk set. That
is necessary and it is not sufficient. With `aux_by = "none"` the model
carries ONE weight simplex and a SEPARATE intercept per study:

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

## Details

    h_s(t | x) = exp(mu_s + beta * x) * sum_k w_k M_k(t)

so if the studies' observed exposure falls on disjoint sets of basis
columns, mass can be moved between those sets and absorbed exactly by
the intercepts. Take a degree-0 basis with boundary knots 0 and 3 and an
internal knot at 1, the index study observed on `[0, 1]` and the
comparator on `[2, 3]`. Every column is live in the pooled risk set, so
the support check passes. But replacing `w` by any `w'` in (0, 1) and
setting `mu_index' = mu_index + log(w / w')` and
`mu_comparator' = mu_comparator + log((1 - w) / (1 - w'))` leaves every
observed hazard, every cumulative-hazard increment and therefore every
likelihood term unchanged, while the conditional hazard ratio moves from
1 to 3. That is an exact likelihood-preserving transformation, not poor
conditioning: a proper prior still gives a usable posterior, but the
treatment contrast along that direction is coming from the prior.

What rules it out is that the studies and the columns they touch form
ONE connected component. Then no subset of the weights can be rescaled
without changing a hazard some study observes.

Connectivity is necessary, not a proof of identification. It is exact
for a degree-0 basis, whose columns have disjoint supports, so a column
belongs to a study or it does not. Above degree 0 the supports overlap,
which makes disconnection harder to reach and makes a connected graph
correspondingly weaker evidence: it rules out this failure mode and says
nothing about the conditioning of the constrained
parameter-to-likelihood map.
