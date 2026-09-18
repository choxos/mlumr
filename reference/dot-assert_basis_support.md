# Stop if any basis column has no support over a study's observed period

An unsupported column is the nonidentification condition: simplex mass
can be parked on it and traded against the study intercept at no cost in
fit.

## Usage

``` r
.assert_basis_support(
  spec,
  observed_max,
  label,
  entry = NULL,
  exit = NULL,
  event = NULL
)
```

## Arguments

- spec:

  A basis spec from
  [`.build_mspline_basis()`](https://choxos.github.io/mlumr/reference/dot-build_mspline_basis.md).

- observed_max:

  The largest time that study actually observed.

- label:

  Study label used in the error message.

- entry, exit:

  The study's per-subject entry and exit times; omit both for data with
  no delayed entry.

- event:

  The study's event times, or `NULL`.

## Value

`TRUE`, invisibly.
