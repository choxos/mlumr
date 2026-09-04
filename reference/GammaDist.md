# The Gamma distribution, parameterized by mean and standard deviation

Density, distribution, and quantile functions for the gamma
distribution, accepting either the native `shape` / `rate` (or `scale`)
parameters or a `mean` and `sd`, which override them. Useful with
[`distr()`](https://choxos.github.io/mlumr/reference/distr.md) and
[`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md),
where covariate moments come straight from a published baseline table.
The moment parameterization uses `shape = (mean / sd)^2` and
`rate = mean / sd^2`.

## Usage

``` r
qgamma(
  p,
  shape,
  rate = 1,
  scale = 1/rate,
  lower.tail = TRUE,
  log.p = FALSE,
  ...,
  mean,
  sd
)

pgamma(
  q,
  shape,
  rate = 1,
  scale = 1/rate,
  lower.tail = TRUE,
  log.p = FALSE,
  ...,
  mean,
  sd
)

dgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE, ..., mean, sd)
```

## Arguments

- p:

  Vector of probabilities.

- shape, rate, scale:

  See [stats::GammaDist](https://rdrr.io/r/stats/GammaDist.html).

- lower.tail, log.p, log:

  See [stats::GammaDist](https://rdrr.io/r/stats/GammaDist.html).

- ...:

  Must be empty. It exists only to hold `mean` and `sd` back from
  partial matching, and is checked so that a misspelled argument is
  refused rather than silently ignored.

- mean, sd:

  Mean and standard deviation, overriding `shape` and `rate` / `scale`
  when both are supplied. Both must be named in full: they sit behind
  `...` so that adding them cannot make `s` ambiguous between `scale`
  and `sd`, which would break the abbreviation
  [`stats::qgamma()`](https://rdrr.io/r/stats/GammaDist.html) accepts.

- x, q:

  Vector of quantiles.

## Value

A numeric vector, as the corresponding stats function.

## See also

[`distr()`](https://choxos.github.io/mlumr/reference/distr.md),
[`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md),
[`qbern()`](https://choxos.github.io/mlumr/reference/qbern.md)

## Examples

``` r
# Equivalent specifications
qgamma(0.5, mean = 65, sd = 8)
#> [1] 64.67209
qgamma(0.5, shape = (65 / 8)^2, rate = 65 / 8^2)
#> [1] 64.67209
```
