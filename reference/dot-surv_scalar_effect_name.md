# The one scalar `effect` name a survival fit can legitimately supply

Turns the natural-scale label into the `effect` selector that names it.
Every fit has exactly one: a proportional-hazards fit supplies a hazard
ratio, a shared-shape SPFA AFT fit supplies a time ratio, and anything
else supplies only the exponentiated location contrast. Keeping this
derivation in one place is what stops the selector and the label from
disagreeing.

## Usage

``` r
.surv_scalar_effect_name(label)
```

## Arguments

- label:

  The `label` from
  [`.surv_scalar_label()`](https://choxos.github.io/mlumr/reference/dot-surv_scalar_label.md)
  (natural scale).

## Value

One of `"hr"`, `"tr"`, `"exp_delta_eta"`.
