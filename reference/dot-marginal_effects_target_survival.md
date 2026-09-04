# Marginal survival effects standardized to an arbitrary target population

Internal dispatch for
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
(survival) when `newdata` is supplied. RMST effects are computed from
the target-standardized survival curve. For proportional-hazards fits,
`"hr"` is the time-specific marginal hazard ratio obtained from the
survival-weighted hazards in that same target. RMST differences are
directly collapsible, but no effect measure is assumed to be invariant
across populations merely because it is collapsible.

## Usage

``` r
.marginal_effects_target_survival(
  object,
  newdata,
  effect,
  summary,
  probs,
  at_time = NULL
)
```
