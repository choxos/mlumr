# Squared relative distance between a logit-normal's moments and a target

`est` is `(mu, log sigma)`. Optimizing the log keeps `sigma` strictly
positive without a constrained optimizer; an unconstrained search over
`sigma` itself can step to a negative scale, where the density is `NaN`
and the objective is meaningless.

## Usage

``` r
.lndiff(est, m, s)
```

## Details

The residuals are divided by their targets. An absolute objective is
meaningless for a small margin: at `mean = 0.0005` a solution three
times too large scores 1e-6, which any convergence rule reads as a fit.
