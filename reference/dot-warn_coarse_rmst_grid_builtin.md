# The same check for the fit's own populations

The `rmst_*` draws are integrated in Stan on the same grid, by the same
trapezoid rule, so the exponential example above distorts them just as
badly, and `predict(type = "rmst")` and the RMST effects of
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
read them straight from the draws with nothing in the way. This
evaluates the standardized curve on the whole RMST grid, for both
treatments in the requested populations, over exactly the rows Stan
averaged: every IPD row for the index population and every integration
point for the comparator one, equally weighted, un-centered here because
[`.conditional_profiles()`](https://choxos.github.io/mlumr/reference/dot-conditional_profiles.md)
centers again. The share is judged on the rows' own curves, not on their
average, for the reason given at
[`.standardize_target_survival_s()`](https://choxos.github.io/mlumr/reference/dot-standardize_target_survival_s.md).

## Usage

``` r
.warn_coarse_rmst_grid_builtin(object, pops = c("index", "comparator"))
```

## Arguments

- object:

  A survival `mlumr_fit`.

- pops:

  The populations whose RMST is being returned, `"index"`,
  `"comparator"` or both. Only their curves are judged: the comparator
  population under a strong covariate effect can decay ahead of the grid
  while the index population, the one asked for, is resolved fine, and a
  warning about curves that contribute nothing to the result would tell
  the user to refit for no reason. The fraction is measured on a
  deterministic, evenly spaced subset of at most 200 draws and 60 rows
  per population, not on every draw and every row, so it is an estimate
  of the share rather than a census of it. The subset is taken by
  position rather than at random, so the same fit reports the same
  number every time.

## Value

`NULL`, invisibly; called for the warning.
