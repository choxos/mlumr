# Message for an `effect` this survival fit cannot supply

Says which scalar estimand the fit does have and why the requested one
does not exist for it.

## Usage

``` r
.surv_effect_scale_error(
  effect,
  label,
  scalar_effect,
  stratified,
  valid_effects
)
```

## Arguments

- effect:

  The requested selector.

- label, scalar_effect:

  The fit's natural-scale label and its selector.

- stratified:

  `TRUE` when the baseline shapes differ by study.

- valid_effects:

  The accepted selectors for this fit.
