# Evaluate an M-spline (or integrated I-spline) basis at given times

Values outside the boundary knots are extrapolated with constant
boundary hazards. For the integrated basis, this adds a linear tail
beyond the boundary so cumulative hazards continue increasing. Times at
or before zero contribute no cumulative hazard.

## Usage

``` r
.eval_basis(spec, times, integral = FALSE)
```

## Arguments

- spec:

  A basis spec from
  [`.build_mspline_basis()`](https://choxos.github.io/mlumr/reference/dot-build_mspline_basis.md).

- times:

  Numeric vector of evaluation times.

- integral:

  If `TRUE`, return the integrated (I-spline) basis; otherwise the
  M-spline basis.

## Value

A numeric matrix with `length(times)` rows and `spec$n_scoef` columns.
