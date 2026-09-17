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

  For `family = "survival"`: the parametric distribution of the survival
  G-computation (default `"weibull"`), fitted with flexsurv; the
  estimand is the restricted mean survival time difference. `"mspline"`
  and `"pexp"` have no parametric analogue and fall back to a Weibull
  fit with a warning, recorded as `approximated = TRUE`. Survival STC
  supports right-censored data without delayed entry.

- n_boot:

  For `family = "survival"` only: number of bootstrap resamples for the
  RMST-difference standard error (default `200`; 0 gives a point
  estimate with no interval). Other families use the delta method.

- seed:

  For `family = "survival"` only: optional integer seed for the
  bootstrap, making the standard error reproducible. The global random
  number stream is restored on exit. Ignored for other families.

- rmst_horizon:

  For `family = "survival"` only: the restriction time of the RMST
  difference. Defaults to the largest observed time across both arms;
  set it explicitly to match an
  [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) fit,
  whose default can differ. A horizon beyond the observed range
  extrapolates and warns.

## Value

An object of class `mlumr_stc`. Its `separation` component records the
outcome of the binomial separation check: `status` is `"not_separated"`
when the exact check ran and found none, `"unknown"` when only the
fitted-value screen ran (it cannot see quasi-complete separation), and
`"not_applicable"` for other outcome models. A separated fit, and a
Poisson fit with no events, are refused rather than returned.

## Details

For binomial outcomes the result carries the link-scale effect, the
standardized and observed event probabilities, the risk difference and
the log risk ratio, with delta-method Wald intervals; the observed
comparator proportion gets the exact Clopper-Pearson interval, as in
[`naive()`](https://choxos.github.io/mlumr/reference/naive.md). An
observed comparator arm with zero or all events uses the pseudo-count
`(r + 0.5) / (n + 1)` in transformed measures. For Poisson outcomes
`$rd` is a rate difference per unit exposure and `$estimate` the log
rate ratio, with a 0.5 continuity correction for a zero comparator
count.

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

A GLM is fitted to the IPD, its predictions are averaged over the
comparator covariate distribution (the integration points, or the AgD
means for the identity-link normal case) on the response scale, and the
average is contrasted with the reported comparator outcome, as in Ren et
al.'s unanchored STC. Standard errors are first-order delta-method
values, conditional on the integration grid and the reported comparator
summaries. The estimand is `E_B[m_A(X)] - E_B[Y_B]` in the comparator
population (`$rd` for binomial, `$md` for normal); `$estimate` is the
link-scale contrast of the two standardized quantities. Survival STC
contrasts the index RMST standardized to the comparator covariates with
the RMST of an intercept-only
[`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md)
fit to the pseudo-IPD. The effect is not transported to the index
population; `mlumr(model = "relaxed")` estimates comparator-specific
covariate effects for any other target.

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
