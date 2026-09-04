# Build an M-spline basis specification

Build an M-spline basis specification

## Usage

``` r
.build_mspline_basis(knots, degree)
```

## Arguments

- knots:

  A list from
  [`make_knots()`](https://choxos.github.io/mlumr/reference/make_knots.md)
  (`internal`, `boundary`).

- degree:

  Spline degree: 3 (cubic M-spline) or 0 (piecewise exponential).

## Value

A basis spec list with `internal`, `boundary`, `degree`, `n_scoef`.
