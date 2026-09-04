# Does the baseline shape actually differ between strata?

`n_strata > 1` alone is not the question. `aux_by = ".study"` allocates
one auxiliary column per study, but a distribution with no shape
parameter has nothing to allocate: for the exponential (PH) and
exponential-AFT the hazard is `exp(eta)` and the baseline is carried
entirely by the study intercept, which is study-specific in every
unanchored fit. Stratifying an exponential therefore changes nothing,
and the closed-form contrasts stay exact.

## Usage

``` r
.aux_shapes_differ(object)
```

## Arguments

- object:

  An `mlumr_fit` (survival family).

## Value

`TRUE` when the two studies genuinely have different baseline shapes,
`FALSE` otherwise.

## Details

This mirrors the Stan gate exactly. `mlumr_survival_{spfa,relaxed}.stan`
swaps `delta_*` for the time-varying `loghr_*[1]` under
`n_strata > 1 && nonexp && dist <= 3`, where `nonexp` excludes precisely
the two exponential codes; the flexible models use `n_strata > 1`
unconditionally because their per-stratum spline simplex IS the
baseline. Reading the gate from one helper is what stops the R layer
attaching a prediction time to a number Stan computed as the `t -> 0`
limit.
