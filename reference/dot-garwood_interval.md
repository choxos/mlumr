# Exact interval for a directly observed Poisson rate

Garwood's interval, the one
[`stats::poisson.test()`](https://rdrr.io/r/stats/poisson.test.html)
reports: the lower bound is the `alpha / 2` quantile of `Gamma(x, 1)`
over the exposure and the upper the `1 - alpha / 2` quantile of
`Gamma(x + 1, 1)` over it, with the lower bound at 0 when the count is.
Coverage is at least the nominal level for every true rate; the bounded
Wald interval it replaces ended at 0.0139 for 0 events over an exposure
of 100 and covered a true rate of 0.02 about 86% of the time.

## Usage

``` r
.garwood_interval(x, exposure, conf_level)
```

## Arguments

- x:

  Event count, non-negative.

- exposure:

  Total exposure, positive.

- conf_level:

  Confidence level.

## Value

List with `lower` and `upper`.
