# Simulated treatment comparison via G-computation

Perform an unanchored simulated treatment comparison (STC) with marginal
standardization. Fits an outcome regression to the single IPD treatment
arm, standardizes its predictions over the comparator-population
covariate distribution, and contrasts that outcome with the reported
comparator-arm outcome in the same population. It does not standardize
either treatment to the index population.

## Usage

``` r
stc(
  data,
  link = NULL,
  conf_level = 0.95,
  distribution = "weibull",
  n_boot = 200L,
  seed = NULL,
  rmst_horizon = NULL
)
```

## Arguments

- data:

  An `mlumr_data` object from
  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md).
  Integration points from
  [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
  are required whenever the outcome model uses a nonlinear link
  (binomial, Poisson, normal-log, or survival). Substitution of
  aggregate means is exact only for a normal identity-link model.

- link:

  Link function. For binomial: `"logit"` (default), `"probit"`, or
  `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
  poisson: `"log"` (default). Ignored for survival. If `NULL`, uses the
  canonical default.

- conf_level:

  Confidence level for the interval (default 0.95)

- distribution:

  For `family = "survival"`: the parametric distribution used for the
  package-specific survival G-computation (default `"weibull"`).
  Requires the `flexsurv` package. The STC estimand is the
  restricted-mean-survival-time difference. The flexible baselines
  `"mspline"` and `"pexp"` have no parametric `flexsurv` analogue:
  requesting either fits a Weibull G-computation as an approximate
  benchmark, emits a warning, and records `distribution_fit = "weibull"`
  and `approximated = TRUE` in the result (`$distribution` keeps the
  requested value). Survival STC currently supports right-censored data
  without delayed entry; use
  [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) for
  left-censored, interval-censored, or delayed-entry survival data.

- n_boot:

  For `family = "survival"` only: number of nonparametric bootstrap
  resamples used for the RMST-difference standard error (default `200`).
  Set `n_boot = 0` for a fast point estimate with no interval (`se`/CI
  returned as `NA`). Must be 0 or at least 2, since a single resample
  has no standard error; several hundred are needed before the interval
  is usable, so treat anything below the default as exploratory. Ignored
  for other families, which use the delta method.

- seed:

  For `family = "survival"` only: optional integer seed for the
  bootstrap, making the standard error reproducible. The global random
  number stream is restored on exit. Ignored for other families.

- rmst_horizon:

  For `family = "survival"` only: the restriction time the RMST
  difference is integrated to. Defaults to the largest observed time
  across both arms. RMST at a different horizon is a different estimand,
  so set this explicitly whenever the result is to be compared with an
  [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) fit,
  whose own default can be the follow-up both studies observed rather
  than the pooled maximum; read that fit's horizon from the `horizon`
  column of `predict(type = "rmst")`. A value beyond the observed range
  extrapolates the fitted parametric survival function and warns.
  Ignored for other families.

## Value

An object of class `mlumr_stc`. Its `separation` component records the
outcome of the binomial separation check: `status` is `"not_separated"`
when the exact check ran and found none, `"unknown"` when only the
fitted-value screen ran (it cannot see quasi-complete separation), and
`"not_applicable"` for other outcome models. A separated fit, and a
Poisson fit with no events, are refused rather than returned.

## Details

For binomial outcomes, returns the treatment effect on the link scale
plus event probabilities, risk difference, and log risk ratio with SEs
and CIs in the comparator population. The observed comparator proportion
gets the exact Clopper-Pearson interval, as in
[`naive()`](https://choxos.github.io/mlumr/reference/naive.md). The
standardized index probability is a model prediction, and its interval
is the delta-method Wald interval bounded to `[0, 1]`: an asymptotic
approximation whose calibration has not been studied here, as are the
intervals of the contrasts. When the observed comparator arm has zero or
all events, transformed effect measures use the boundary-only
pseudo-count `(r + 0.5) / (n + 1)`; model predictions are never
corrected. For Poisson outcomes, the comparator log rate uses a 0.5
continuity correction when the observed event count is zero, and `$rd`
is a RATE difference on the per-unit-exposure scale, the standardized
index rate minus the observed comparator rate, rather than the risk
difference the binomial `$rd` is. `$estimate` stays the log rate ratio.

Scale note: `$estimate` (and the binomial `$log_rr`) is on the link /
log scale, where the null is 0. To compare against the natural-scale
risk ratio or rate ratio from
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
(where the null is 1), exponentiate it (e.g. `exp(result$estimate)`).

Normal-family weighting note: across multiple AgD rows, the normal STC
comparator-population prediction and observed mean use sample-size
(`outcome_n`) weights, matching the Bayesian ML-UMR
comparator-population estimand. `outcome_n` is required when there is
more than one row; a single row has weight one. The observed
comparator-mean variance combines independent, mutually exclusive strata
as `sum(w^2 * se^2)` using normalized population weights.

The STC procedure is:

1.  Fit a GLM on IPD (binomial/gaussian/poisson as appropriate).

2.  Predict on comparator-population covariates (from integration points
    or AgD covariate means for the identity-link normal special case).

3.  Marginalize predictions over the comparator population.

4.  Contrast with the reported comparator outcome in that population.

5.  Compute first-order, fixed-integration-grid delta-method standard
    errors.

The response-scale standardization predicts each target profile,
averages the natural-scale outcomes, then transforms the average, as in
Ren et al.'s unanchored STC. Only the index-treatment outcome model is
fitted, since comparator IPD are unavailable; Remiro-Azocar et al.
describe the two-arm G-computation that a different data design allows.

The non-survival standard error is conditional on the integration grid
and the reported comparator summaries: it propagates coefficient and
comparator-outcome uncertainty, not uncertainty in reconstructing the
comparator covariate distribution. The estimator relies on a correctly
specified index outcome model that applies across the comparator
covariate distribution, adequate overlap and no unmeasured confounding;
it does not need the two treatments to share covariate effects.

The estimand is `E_B[m_A(X)] - E_B[Y_B]` on the response scale, which is
`$rd` for a binomial outcome and `$md` for a normal one. `$estimate` is
the same mean difference under the normal identity link and otherwise
the link-scale contrast of the two standardized quantities: a marginal
log odds ratio under a binomial logit, a log rate ratio under Poisson, a
log mean ratio under a log link.

Survival STC contrasts the index RMST standardized to the comparator
covariates with the RMST of an intercept-only
[`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md)
fit to the reconstructed pseudo-IPD.

The effect is defined in the comparator population and is not
transported to the index population; `mlumr(..., model = "relaxed")` is
what estimates comparator-specific covariate effects for any other
target.

## References

Ren S, Ren S, Welton NJ, Strong M (2024). Advancing unanchored simulated
treatment comparisons: A novel implementation and simulation study.
*Research Synthesis Methods*, 15(4), 657-670.
[doi:10.1002/jrsm.1718](https://doi.org/10.1002/jrsm.1718)

Remiro-Azocar A, Heath A, Baio G (2022). Parametric G-computation for
compatible indirect treatment comparisons with limited individual
patient data. *Research Synthesis Methods*, 13(6), 716-744.
[doi:10.1002/jrsm.1565](https://doi.org/10.1002/jrsm.1565)

## Examples

``` r
if (FALSE) { # \dontrun{
result <- stc(dat)
print(result)
} # }
```
