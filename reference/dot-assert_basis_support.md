# Stop if any basis column has no support over a study's observed period

An unsupported column is exactly the nonidentification condition: its
coefficient cannot be moved by the likelihood, so simplex mass can be
parked there and traded against the study intercept at no cost in fit.

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

  The study's per-subject entry and exit times, whose merged union is
  the period it had someone under observation. Omit both for data with
  no delayed entry, which is treated as one interval from zero.

- event:

  The study's event times, or `NULL`. The cumulative hazard integrates
  over the risk intervals and cannot see an isolated instant, but the
  event term evaluates the hazard AT each event time, so a column
  positive only there is supported after all.

## Value

`TRUE`, invisibly.
