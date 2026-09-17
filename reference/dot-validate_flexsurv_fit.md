# Require a flexsurv fit that converged to a maximum

`flexsurvreg()` warns rather than fails at an optimizer boundary, so
this raises an error and the bootstrap counts such a replicate as
failed.

## Usage

``` r
.validate_flexsurv_fit(fit, arm)
```
