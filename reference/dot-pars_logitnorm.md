# Estimate logit-normal mu / sigma from a mean and SD on (0, 1)

The feasibility check is not decoration. A variable supported on
`(0, 1)` has `Var(X) <= mean * (1 - mean)`, with equality only for a
two-point distribution on the boundaries, which no logit-normal can
represent. Given an impossible pair the optimizer still returns
something, so without this the caller would silently integrate over a
distribution that has neither the requested mean nor the requested SD.

## Usage

``` r
.pars_logitnorm(m, s)
```
