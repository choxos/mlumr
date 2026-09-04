# Check integration point adequacy

Compare integration results at the current `n_int` against a doubled
resolution to assess numerical accuracy. Large discrepancies indicate
that `n_int` should be increased. Because the Sobol sequence is nested
(the doubled set contains the current set), this current-vs-doubled
difference is a convergence heuristic, not an error bound. Agreement
between the two grids does not establish accuracy for rare discrete
margins or for a final treatment-effect estimand.

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
previous versions) and, if `check_joint = TRUE`, `correlations`, a data
frame of pairwise covariate correlations at the current and doubled
`n_int` for each AgD row. Printed with a pass/warn verdict.

The `verdict` component reports `"stable"` / `"close"` when a comparison
was made and met the heuristic, `"review"` when it did not, and
`"unavailable"` when there was nothing finite to compare. A declared
target the AgD does not supply, or a latent Gaussian-copula correlation
(`cor_adjust = "none"`), gives `"unavailable"` rather than a pass.
