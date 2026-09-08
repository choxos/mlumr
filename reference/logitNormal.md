# The logit-Normal distribution

Density, distribution, and quantile functions for the logit-normal
distribution: the distribution of `plogis(z)` where `z` is normal with
mean `mu` and standard deviation `sigma`. It is the natural marginal for
a covariate reported as a proportion on `(0, 1)`, such as percent body
surface area.

## Usage

``` r
dlogitnorm(x, mu = 0, sigma = 1, log = FALSE, ..., mean, sd)

plogitnorm(q, mu = 0, sigma = 1, ..., mean, sd)

qlogitnorm(p, mu = 0, sigma = 1, ..., mean, sd)
```

## Arguments

- x, q:

  Vector of quantiles, in `(0, 1)`.

- mu, sigma:

  Location and scale, on the logit scale.

- log:

  Return the log density. Positional, as in
  [`stats::dnorm()`](https://rdrr.io/r/stats/Normal.html).

- ...:

  For `plogitnorm()` and `qlogitnorm()`, passed to the underlying stats
  normal function
  ([`stats::pnorm()`](https://rdrr.io/r/stats/Normal.html),
  [`stats::qnorm()`](https://rdrr.io/r/stats/Normal.html)), so
  `lower.tail` and `log.p` work as usual. `dlogitnorm()` builds its
  density from [`stats::dnorm()`](https://rdrr.io/r/stats/Normal.html)
  and a Jacobian rather than delegating, so it has nothing to forward
  and refuses anything passed here; in its signature `...` serves only
  to keep `mean` and `sd` from matching positionally.

- mean, sd:

  Mean and standard deviation on the `(0, 1)` scale, overriding `mu` and
  `sigma` when both are supplied.

- p:

  Vector of probabilities.

## Value

A numeric vector.

## Details

For convenience the distribution may be given by its `mean` and `sd` on
the natural `(0, 1)` scale instead of `mu` and `sigma` on the logit
scale. There is no closed form for that reparameterization, so `mu` and
`sigma` are found numerically; supply `mu` / `sigma` directly if you
have them.

## See also

[`distr()`](https://choxos.github.io/mlumr/reference/distr.md),
[`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md),
[GammaDist](https://choxos.github.io/mlumr/reference/GammaDist.md)

## Examples

``` r
qlogitnorm(0.5, mean = 0.34, sd = 0.19)
#> [1] 0.310708
```
