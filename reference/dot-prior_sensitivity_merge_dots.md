# A caller's `...` merged into the arguments that replay a fit

The recorded `control` describes the original fit, so a caller's
settings refine it rather than replace it, and the engine that will
actually run has to be resolved the way
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) resolves
it rather than read off the call. Split out from the refit loop so the
merge can be checked without sampling: written out at the call site it
could only be tested by a copy, and a copy passes whatever the loop
itself then does.

## Usage

``` r
.prior_sensitivity_merge_dots(call_args, dots)
```

## Arguments

- call_args:

  The replay arguments built from the original fit.

- dots:

  The caller's `...`, as a list.

## Value

`call_args`, with the caller's settings merged in.
