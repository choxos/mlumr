# Naive unadjusted indirect comparison

Compute an unadjusted (naive) indirect treatment comparison by comparing
crude outcomes from the IPD and AgD without any covariate adjustment.
The index outcome remains marginal over the index-study population and
the comparator outcome remains marginal over the comparator population.
The contrast therefore has no single standardized target population. It
returns the link-scale contrast plus the two observed marginal outcomes
and available natural-scale contrasts.

## Usage

``` r
naive(data, link = NULL, conf_level = 0.95)
```

## Arguments

- data:

  An `mlumr_data` object from
  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md)

- link:

  Link function. For binomial: `"logit"` (default), `"probit"`, or
  `"cloglog"`. For normal/poisson: ignored (identity/log always used).
  For survival: ignored (an unadjusted Cox proportional-hazards log
  hazard ratio is returned). The naive Cox benchmark accepts only
  right-censored / event data (optionally with delayed entry); left- or
  interval-censored data (which the Bayesian
  [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) model
  supports) are rejected. If `NULL`, uses the canonical default.

- conf_level:

  Confidence level for the interval (default 0.95)

## Value

An object of class `mlumr_naive`

## Details

The two arms are observed directly, so their intervals are exact: the
Clopper-Pearson interval for a binomial proportion and the Garwood
interval for a Poisson rate, the ones
[`binom.test()`](https://rdrr.io/r/stats/binom.test.html) and
[`poisson.test()`](https://rdrr.io/r/stats/poisson.test.html) report,
whose coverage is at least the nominal level for every true value (they
are conservative rather than shortest). A comparator built from several
aggregate rows is pooled for its interval: independent Poisson counts
add, so the pooled rate interval is exact for the exposure-weighted
mean, and the pooled binomial interval is conservative for the
size-weighted mean of its strata. The arm standard errors are still
reported, and they and the contrasts use the boundary pseudo-count
`(r + 0.5) / (n + 1)` when an arm has zero or all events, or 0.5 events
when a Poisson count is zero; the reported crude proportions and rates
are unchanged. The contrasts' intervals are Wald intervals on the link
scale around those corrected quantities. Enumerating every pair of
counts at 100 per arm, their coverage of the true log odds ratio, log
risk ratio and risk difference ranges from 0.939 to 0.9999 over true
probabilities from 0.014 to 0.986, with the risk difference lowest at
0.939 beside a boundary; they are approximate, not exact.

Scale note: `$estimate` (and the binomial `$log_rr`) is on the link /
log scale, where the null is 0. To compare against the natural-scale
risk ratio or rate ratio from
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
(where the null is 1), exponentiate it (e.g. `exp(result$estimate)`).

## Normal-family weighting

Across multiple AgD rows the normal-family comparator mean here is
population weighted using `outcome_n`, matching the Bayesian ML-UMR
comparator-population estimand. `outcome_n` is required when there is
more than one row; a single row has weight one. The comparator-mean
variance combines independent, mutually exclusive strata as
`sum(w^2 * se^2)` using normalized population weights. The same
weighting applies to
[`stc()`](https://choxos.github.io/mlumr/reference/stc.md).

## Examples

``` r
if (FALSE) { # \dontrun{
result <- naive(dat)
print(result)
} # }
```
