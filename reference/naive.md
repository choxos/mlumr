# Naive unadjusted indirect comparison

Compute an unadjusted (naive) indirect treatment comparison by comparing
crude outcomes from the IPD and AgD without any covariate adjustment.
Returns treatment effect on the link scale plus absolute predictions
(event probabilities, risk difference, risk ratio) for both populations.

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
  If `NULL`, uses the canonical default.

- conf_level:

  Confidence level for the interval (default 0.95)

## Value

An object of class `mlumr_naive`

## Details

For binomial outcomes, event-probability intervals use Wald standard
errors and are bounded to `[0, 1]`. For Poisson outcomes, the log-rate
contrast uses a 0.5 continuity correction when an observed event count
is zero.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- naive(dat)
print(result)
} # }
```
