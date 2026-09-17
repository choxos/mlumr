# Estimate logit-normal mu / sigma from a mean and SD on (0, 1)

A variable on `(0, 1)` has `Var(X) < mean * (1 - mean)`, so an
impossible pair is refused before the optimizer is asked.

## Usage

``` r
.pars_logitnorm(m, s)
```
