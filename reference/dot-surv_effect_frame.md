# Assemble a survival marginal-effects frame from per-column draw records

`spec` has one entry per effect column with `variable`, `effect`,
`population`, `at_time`, `horizon` and `draws`; both routes reduce to
it, so the layout is written once.

## Usage

``` r
.surv_effect_frame(spec, summary, probs, rmst_horizon)
```
