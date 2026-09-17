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
interval for a Poisson rate, pooled across aggregate rows. The arm
standard errors and the contrasts use the boundary pseudo-count
`(r + 0.5) / (n + 1)` when an arm has zero or all events, or 0.5 events
for a zero Poisson count; the reported crude proportions and rates are
unchanged. The link-scale contrast, the log risk ratio and the risk
difference get Wald intervals, whose coverage is approximate.

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
