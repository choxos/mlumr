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
`n_int` for each AgD row. Its `covariate_1` and `covariate_2` columns
name the two margins; `pair` is a label built from them for reading.
Printed with a pass/warn verdict.

The `verdict` component reports `"stable"` / `"close"` when a comparison
was made and met the heuristic, `"review"` when it did not, and
`"unavailable"` when there was nothing finite to compare. A declared
target the AgD does not supply, or a latent Gaussian-copula correlation
(`cor_adjust = "none"`), gives `"unavailable"` rather than a pass. The
correlation verdicts (`target_correlation`, `resolution_correlation`)
are `"partial"` when the measured pairs pass but some pair with a
correlation to realize could not be measured, since a maximum over the
measured pairs says nothing about the rest; a measured pair that misses
the heuristic is `"review"` regardless. `correlation_pairs` counts the
pairs expected and the pairs measured: `measured` is the number with a
finite correlation on the doubled grid, the correlation the target
comparison uses when there is a target to compare it with, and
`measured_resolution` the number with a finite correlation on both
grids. Neither count says whether a target comparison was made; with
`cor_adjust = "none"` none is. It names the omitted pairs with a reason,
and lists separately the pairs in which a margin is declared with no
variance, which have no correlation to realize and are outside the
count.

For a binary margin the declared-target SD is the distribution's,
`sqrt(p * (1 - p))` from the declared mean, whatever `_sd` column the
AgD carries: a sample SD of the source data has a size correction no
grid can reproduce. Grid SDs are population SDs for the same reason.
