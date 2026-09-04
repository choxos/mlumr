# Stop if any basis column has no support over a study's observed period

An unsupported column is exactly the nonidentification condition: its
coefficient cannot be moved by the likelihood, so simplex mass can be
parked there and traded against the study intercept at no cost in fit.

## Usage

``` r
.assert_basis_support(spec, observed_max, label)
```

## Arguments

- spec:

  A basis spec from
  [`.build_mspline_basis()`](https://choxos.github.io/mlumr/reference/dot-build_mspline_basis.md).

- observed_max:

  The largest time that study actually observed.

- label:

  Study label used in the error message.

## Value

`TRUE`, invisibly.
