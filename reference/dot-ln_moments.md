# Moments of a logit-normal, by numerical integration

Integrates over the latent normal variable, splitting the range at
`z0 = -mu / sigma`, clamped to \[-8, 8\], where the logistic transition
sits, with `abs.tol = 0` because the variance of a concentrated margin
is far below the default absolute tolerance. Returns `NULL` when the
quadrature fails.

## Usage

``` r
.ln_moments(mu, sigma)
```
