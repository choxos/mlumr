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

A list with `marginals`, a data frame of grid means and SDs at the
current and doubled `n_int` against the declared targets, and `verdict`,
whose entries are `"stable"` or `"close"` when a comparison met the
heuristic, `"review"` when it did not, `"partial"` when the measured
correlation pairs passed but some pair could not be measured, and
`"unavailable"` when there was nothing finite to compare (a latent
matrix under `cor_adjust = "none"` is never compared). With
`check_joint = TRUE` and two or more covariates it also holds
`correlations`, the pairwise correlations per AgD row on both grids, and
`correlation_pairs`, which counts the pairs measured and names the rest.
A binary margin's target SD is `sqrt(p * (1 - p))` from the declared
mean, and grid SDs are population SDs.
