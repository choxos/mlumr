# Exact interval for a directly observed binomial proportion

Clopper and Pearson's interval, the one
[`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html)
reports: the lower bound is the `alpha / 2` quantile of
`Beta(r, n - r + 1)` and the upper the `1 - alpha / 2` quantile of
`Beta(r + 1, n - r)`, with the bound at 0 or 1 when the count is. Its
coverage is at least the nominal level for every true probability, which
the bounded Wald interval it replaces did not have: at 0 events of 100
that interval ended at 0.0138, and enumerating every count at a true
probability of 0.014 put its coverage at 75.5%, since the zero-count
outcome alone has probability 0.24 and excludes the truth. The exact
interval is conservative rather than shortest; it is used for arms that
are observed directly, not for model predictions or contrasts.

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

## Details

Formed from the same beta quantiles
[`binom.test()`](https://rdrr.io/r/stats/binom.test.html) uses, without
its integer check, so a count is taken as given.
