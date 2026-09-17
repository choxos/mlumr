# A caller's `...` merged into the arguments that replay a fit

A caller's settings refine the recorded `control` rather than replace
it, and the engine is resolved the way
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) resolves
it.

## Usage

``` r
.prior_sensitivity_merge_dots(call_args, dots)
```
