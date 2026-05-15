# Simulated treatment comparison via G-computation

Perform an unanchored simulated treatment comparison (STC) using
parametric G-computation. Fits a regression on IPD, predicts
counterfactual outcomes in both the index and comparator populations,
and computes marginal treatment effects with delta-method standard
errors.

## Usage

``` r
stc(data, link = NULL, conf_level = 0.95)
```

## Arguments

- data:

  An `mlumr_data` object from
  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md).
  Integration points are not required for STC but covariate information
  from the AgD is used.

- link:

  Link function. For binomial: `"logit"` (default), `"probit"`, or
  `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
  poisson: `"log"` (default). If `NULL`, uses the canonical default.

- conf_level:

  Confidence level for the interval (default 0.95)

## Value

An object of class `mlumr_stc`

## Details

For binomial outcomes, returns the treatment effect on the link scale
plus event probabilities, risk difference, and log risk ratio with SEs
and CIs for both populations. Event-probability intervals use Wald
standard errors and are bounded to `[0, 1]`. For Poisson outcomes, the
comparator log rate uses a 0.5 continuity correction when the observed
event count is zero.

The STC procedure is:

1.  Fit a GLM on IPD (binomial/gaussian/poisson as appropriate).

2.  Predict on comparator-population covariates (from integration points
    or AgD covariate means).

3.  Marginalize predictions over the comparator population.

4.  Predict on index-population covariates (IPD).

5.  Compute treatment effects and SEs via the delta method.

STC is a parametric G-computation benchmark. It relies on the IPD
outcome model being correctly specified and transportable to the
comparator population. It does not model posterior uncertainty in
population covariate distributions or relax treatment-specific covariate
effects. When clinically meaningful effect modification is plausible,
prefer `mlumr(..., model = "relaxed")` as the primary analysis and use
STC as a sensitivity or benchmarking analysis.

For binomial outcomes, the comparator-population treatment contrast is
transported to the index population **assuming the treatment contrast is
constant on the fitted link scale** (i.e., no effect modification on
that scale). Under this assumption, the index-population comparator
probability is computed as
`inv_link(link(p_A_index) - (link(p_A_comp) - link(p_B)))`, and its
uncertainty is propagated through the delta method. If effect
modification is expected, fit a Bayesian relaxed model with
`mlumr(..., model = "relaxed")` and use
`predict(..., population = "index")` instead, which does not require
this assumption.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- stc(dat)
print(result)
} # }
```
