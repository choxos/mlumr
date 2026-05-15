# Check integration point adequacy

Compare integration results at the current `n_int` against a doubled
resolution to assess numerical accuracy. Large discrepancies indicate
that `n_int` should be increased.

## Usage

``` r
check_integration(
  data,
  ...,
  cor = NULL,
  cor_adjust = NULL,
  check_joint = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  An `mlumr_data` object with integration points

- ...:

  Distribution specifications (same as passed to
  [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md))

- cor:

  Correlation matrix (same as passed to
  [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md))

- cor_adjust:

  Adjustment method (same as passed to
  [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md))

- check_joint:

  If `TRUE` (default), also compare pairwise correlation matrices
  between the current and doubled `n_int`, and the maximum per-AgD-row
  absolute deviation from the user-supplied `cor`. The pairwise
  comparison catches cases where marginals converge but joint dependence
  structure does not (rare in practice for QMC with sensible
  `cor_adjust` but worth flagging when `n_int` is small).

- verbose:

  Logical; if `FALSE`, suppresses printed diagnostic messages.

## Value

A list with components `marginals` (the original data frame returned by
previous versions) and, if `check_joint = TRUE`, `correlations` – a data
frame of pairwise covariate correlations at the current and doubled
`n_int` for each AgD row. Printed with a pass/warn verdict.
