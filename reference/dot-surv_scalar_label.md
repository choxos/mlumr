# Label and evaluation time for the scalar survival treatment effect

`delta_*` is a marginal log hazard ratio under proportional hazards (at
`t -> 0` when the shapes are shared, otherwise at the first prediction
time), a log time ratio for a shared-shape SPFA AFT fit, and otherwise a
location contrast that is not a time ratio: with different shapes there
is no constant acceleration factor, and in a relaxed fit the covariate
term does not cancel.
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
and
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
both read this.

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
