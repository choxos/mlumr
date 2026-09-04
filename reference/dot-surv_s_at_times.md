# Survival S(t \| x) at arbitrary times for one linear-predictor draw vector

Generalizes
[`.surv_eval_curve()`](https://choxos.github.io/mlumr/reference/dot-surv_eval_curve.md)
to an arbitrary `times` grid with matching I-spline integral basis
`ibasis` (used for the M-spline/piecewise baseline; ignored for
parametric distributions, which evaluate `S` analytically).

## Usage

``` r
.surv_s_at_times(
  object,
  eta,
  times,
  ibasis,
  treatment = c("index", "comparator"),
  log_scale = FALSE
)
```
