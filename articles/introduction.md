# Introduction to mlumr

## What mlumr does

**mlumr** implements **multilevel unanchored meta-regression (ML-UMR)**
for population-adjusted indirect treatment comparisons. It is for the
situation where two treatments have never been compared in the same
trial and there is **no common comparator arm** to anchor the comparison
(a *disconnected* or *single-arm* evidence base), you hold individual
patient data (IPD) for one treatment and only published aggregate data
(AgD) for the other.

ML-UMR fits an outcome model to the IPD and **integrates it over the
comparator study’s covariate distribution**, transporting the comparison
to a decision-relevant population while adjusting for cross-trial
covariate imbalance. It extends multilevel network meta-regression
(ML-NMR) ([Phillippo et al. 2020](#ref-Phillippo2020)) to the
unanchored, two-treatment case; the method and its assumptions are
developed by Chandler and Ishak for binary ([Chandler and Ishak
2025](#ref-ChandlerIshak2025)) and survival ([Chandler and Ishak
2026](#ref-ChandlerIshak2026)) outcomes.

The current package is two-treatment: one index treatment (IPD) versus
one comparator (AgD). Multi-treatment unanchored networks are out of
scope.

## Supported outcome families

| Family | Outcome | Worked-example vignette |
|----|----|----|
| binomial | binary (0/1) response | [`vignette("binary-outcomes")`](https://choxos.github.io/mlumr/articles/binary-outcomes.md) |
| normal | continuous | [`vignette("continuous-outcomes")`](https://choxos.github.io/mlumr/articles/continuous-outcomes.md) |
| poisson | counts with exposure | [`vignette("count-outcomes")`](https://choxos.github.io/mlumr/articles/count-outcomes.md) |
| survival | time-to-event | [`vignette("survival-outcomes")`](https://choxos.github.io/mlumr/articles/survival-outcomes.md) |

## The four methods

mlumr provides four estimators behind one data interface, so you can run
them side by side as a sensitivity analysis:

| Method | Adjusts covariates? | Effect modification? | Inference |
|----|----|----|----|
| ML-UMR SPFA | yes | no (shared coefficients) | Bayesian |
| ML-UMR relaxed | yes | yes (treatment-specific) | Bayesian |
| STC | yes (outcome model) | no | frequentist |
| Naive | no | n/a | frequentist (benchmark) |

## What “unanchored” does and does not buy you

Removing the common-arm requirement is what makes single-arm comparisons
possible, but it does **not** remove the need for assumptions.
Unanchored ML-UMR still relies on **conditional constancy of the
absolute outcome**: that, conditional on the modelled covariates, the
index outcome model transports to the comparator population (and, under
the default SPFA, that prognostic effects are shared across treatments).
These assumptions are strong and largely untestable, so always report
the naive benchmark and sensitivity analyses
([`vignette("choosing-a-method")`](https://choxos.github.io/mlumr/articles/choosing-a-method.md)).

## Installation

Install the released version from CRAN, or the development version from
GitHub:

``` r

install.packages("mlumr")            # from CRAN
# remotes::install_github("choxos/mlumr")   # development version
```

## The workflow at a glance

Every analysis follows the same four steps, then a fit:

``` r

library(mlumr)

ipd <- set_ipd(...)                  # individual patient data (index)
agd <- set_agd(...)                  # aggregate data (comparator)
dat <- combine_data(ipd, agd)        # combine
dat <- add_integration(dat, ...)     # quasi-Monte Carlo integration points
fit <- mlumr(dat, model = "spfa")    # fit the Bayesian model
```

The exact
[`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md) /
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
arguments depend on the outcome family; each per-outcome vignette shows
a complete, runnable example.

## Where to go next

1.  [`vignette("data-preparation")`](https://choxos.github.io/mlumr/articles/data-preparation.md),
    the four-step pipeline and the numerical integration that powers
    population adjustment (covariate distributions, correlation,
    diagnostics).
2.  **One outcome-family worked example**,
    [`vignette("binary-outcomes")`](https://choxos.github.io/mlumr/articles/binary-outcomes.md),
    [`vignette("continuous-outcomes")`](https://choxos.github.io/mlumr/articles/continuous-outcomes.md),
    [`vignette("count-outcomes")`](https://choxos.github.io/mlumr/articles/count-outcomes.md),
    or
    [`vignette("survival-outcomes")`](https://choxos.github.io/mlumr/articles/survival-outcomes.md).
3.  [`vignette("fitting-and-diagnostics")`](https://choxos.github.io/mlumr/articles/fitting-and-diagnostics.md),
    sampler control, priors, and MCMC diagnostics.
4.  [`vignette("choosing-a-method")`](https://choxos.github.io/mlumr/articles/choosing-a-method.md),
    assumptions, model comparison, and a decision guide for which
    estimate to report.

Chandler, C., and K. J. Ishak. 2025. *Anchors Away: Navigating
Unanchored Indirect Comparisons with Multilevel Unanchored
Meta-Regression*. ISPOR Europe, Glasgow, UK; abstract MSR28.
<https://www.valueinhealthjournal.com/article/S1098-3015(25)05944-3/abstract>.

Chandler, C., and K. J. Ishak. 2026. *Surviving Unanchored Indirect
Comparisons: An Extension of Multilevel Unanchored Meta-Regression
(ML-UMR) for Survival Analyses*. ISPOR, Philadelphia, PA, USA; abstract
MSR131; Value in Health.
<https://www.ispor.org/heor-resources/presentations-database/presentation-cti/ispor-2026/poster-session-3-3/surviving-unanchored-indirect-comparisons-an-extension-of-multilevel-unanchored-meta-regression-ml-umr-for-survival-analyses>.

Phillippo, D. M., S. Dias, A. E. Ades, et al. 2020. “Multilevel Network
Meta-Regression for Population-Adjusted Treatment Comparisons.” *Journal
of the Royal Statistical Society: Series A (Statistics in Society)* 183
(3): 1189–210. <https://doi.org/10.1111/rssa.12579>.
