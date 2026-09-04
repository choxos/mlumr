# Marginal log hazard ratio in a target population at the origin

With a shared baseline shape the shape cancels from the ratio of
marginal hazards as `t -> 0`, leaving
`log E[exp(eta_index)] - log E[exp(eta_comparator)]` over the target
rows. This is the same closed form the Stan models use for `delta_*`, so
a target equal to the IPD covariates reproduces `population = "index"`
exactly. The equal row counts cancel, so no `1/n` appears.

## Usage

``` r
.target_loghr_origin(object, newdata)
```
