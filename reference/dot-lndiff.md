# Squared relative distance between a logit-normal's moments and a target

`est` is `(mu, log sigma)`; the residuals are relative to their targets
so a small margin is judged on its own scale.

## Usage

``` r
.lndiff(est, m, s)
```
