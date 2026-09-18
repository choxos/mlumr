# Check the sampler diagnostics of a fit

Warns about divergent transitions, iterations that hit the maximum tree
depth, chains that did not come back, split-Rhat above 1.01 or 1.05, and
bulk or tail effective sample sizes below 400. A diagnostic the backend
did not supply is reported as unavailable rather than read as clean.
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) runs this
check after sampling; call it again on a stored fit to see the same
verdict.

## Usage

``` r
check_diagnostics(fit)
```

## Arguments

- fit:

  An `mlumr_fit` object.

## Value

`NULL`, invisibly; called for its warnings and messages.

## See also

[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md),
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md).
