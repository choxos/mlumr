# Resolve logit-normal parameters from either parameterization

Supplying only one of `mean` / `sd` used to fall through to the `mu` /
`sigma` defaults, so `distr(qlogitnorm, mean = bsa_mean)` silently
integrated over a standard logit-normal instead of the requested
distribution. Half a moment specification is a mistake, not a
parameterization.

## Usage

``` r
.logitnorm_pars(mu, sigma, mean, sd, has_mean, has_sd)
```
