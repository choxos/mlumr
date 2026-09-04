# Name for the conditional survival contrast

`exp(eta_index - eta_comparator)` is a conditional hazard ratio only
when the two studies share a baseline hazard, and a time ratio only when
the AFT shape/scale parameters are shared. The test is whether the
auxiliary shape/scale draws actually differ, not whether `aux_by` asked
for strata: an exponential has no shape to stratify, so
`aux_by = ".study"` leaves its baseline shared and the exact `hr` label
stands.

## Usage

``` r
.surv_contrast_name(object)
```

## Arguments

- object:

  An `mlumr_fit` (survival).

## Value

`"hr"`, `"tr"`, or `"exp_eta_contrast"` when the two baselines'
shape/scale parameters differ.
