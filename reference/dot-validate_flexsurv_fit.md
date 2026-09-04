# Require a flexsurv fit that actually converged

A sparse or nearly separated sample can leave events in both arms and
still send the optimizer to a boundary. `flexsurvreg()` warns in that
case rather than failing, so the estimates were summarized as an
ordinary RMST, and the bootstrap counted such refits among its successes
because [`tryCatch()`](https://rdrr.io/r/base/conditions.html) sees only
errors. Raising an error here makes a non-converged replicate a failed
one, which is what it is.

## Usage

``` r
.validate_flexsurv_fit(fit, arm)
```
