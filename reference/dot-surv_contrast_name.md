# Name for the conditional survival contrast

`exp(eta_index - eta_comparator)` is a hazard ratio or time ratio only
when the two studies' shape parameters agree. An exponential has no
shape to stratify, so `aux_by = ".study"` leaves its label exact.

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
