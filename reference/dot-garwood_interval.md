# Exact interval for a directly observed Poisson rate

Garwood's interval, as
[`stats::poisson.test()`](https://rdrr.io/r/stats/poisson.test.html)
reports it: gamma quantiles over the exposure, with the lower bound at 0
when the count is 0. Coverage is at least nominal for every true rate.

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
