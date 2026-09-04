# Label and evaluation time for the scalar survival treatment effect

`delta_*` means three different things depending on the fit, and every
user-facing surface that reports it must say which. Deriving that in one
place is the point:
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
and
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
previously decided independently, which is exactly how one of them came
to print a generic "log hazard ratio / log time ratio" for a quantity
that was neither.

## Usage

``` r
.surv_scalar_label(object, log_scale = FALSE)
```

## Arguments

- object:

  An `mlumr_fit` (survival family).

- log_scale:

  `TRUE` for the log-scale name (as stored in `delta_*`), `FALSE` for
  the natural-scale name
  [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
  reports.

## Value

A list with `label` and `at_time` (`NA` when the measure has no
evaluation time).

## Details

- **Proportional hazards.** A marginal log hazard ratio. Marginal hazard
  ratios are non-collapsible, so it always carries a time: the `t -> 0`
  limit when the baseline shapes are shared, otherwise the first
  prediction time.

- **AFT, shared shapes, SPFA.** A genuine log time ratio. Both arms
  share coefficients, so the covariate term cancels from
  `mean(eta_index) - mean(eta_comparator)` and the contrast is constant
  in both time and covariates.

- **AFT otherwise.** Not a time ratio. If the shapes differ there is no
  constant acceleration factor at all. If the model is `relaxed`, the
  coefficients differ by treatment, so the covariate term does NOT
  cancel and the contrast is the average of covariate-specific log time
  ratios over the population (a geometric mean once exponentiated), not
  one population-level acceleration factor. Either way the honest name
  is the location contrast.
