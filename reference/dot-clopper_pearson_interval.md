# Exact interval for a directly observed binomial proportion

Clopper and Pearson's interval, as
[`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html) reports
it but without the integer check: beta quantiles, with the lower bound
at 0 when the count is 0 and the upper bound at 1 when the count equals
`n`. For integer counts its coverage is at least nominal for every true
probability, which the bounded Wald interval it replaced lacked; a
fractional count has no such guarantee.

## Usage

``` r
.clopper_pearson_interval(r, n, conf_level)
```

## Arguments

- r:

  Event count, in `[0, n]`.

- n:

  Number of trials, positive.

- conf_level:

  Confidence level.

## Value

List with `lower` and `upper`.
