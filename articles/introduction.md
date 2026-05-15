# Introduction to mlumr

## Overview

**mlumr** implements multilevel unanchored meta-regression (ML-UMR) for
indirect treatment comparisons using individual patient data (IPD) and
aggregate data (AgD). It supports binary, continuous (normal), and count
(Poisson) outcomes. It provides:

- **ML-UMR** (Bayesian): SPFA and Relaxed SPFA models via Stan (`rstan`
  by default, with optional `cmdstanr`)
- **STC**: Simulated treatment comparison via parametric G-computation
- **Naive**: Unadjusted comparison as a benchmark

Version 0.1.0 is a two-treatment package: one treatment is represented
by IPD (the index treatment) and one by AgD (the comparator treatment).
Multi-treatment unanchored networks are outside the current package
scope.

## When to use mlumr

Use mlumr when you need to compare two treatments that have never been
directly compared in the same trial (disconnected network), and you
have:

- **IPD** from one trial (index treatment)
- **AgD** from another trial (comparator treatment)
- Shared covariates between populations
- Binary, continuous, or count outcomes

## Installation

``` r

# From GitHub (development version)
# install.packages("remotes")
remotes::install_github("choxos/mlumr")
```

## Quick start

The pipeline below runs on toy data so every chunk is self-contained.
For a more detailed worked analysis, see
[`vignette("worked-example")`](https://choxos.github.io/mlumr/articles/worked-example.md).

``` r

library(mlumr)
set.seed(2026)

# Toy IPD: trial A (index treatment, binary outcome)
n_a <- 300
trial_a_data <- data.frame(
  trt      = "Drug_A",
  response = rbinom(n_a, 1, 0.55),
  age_cat  = rbinom(n_a, 1, 0.40),
  sex      = rbinom(n_a, 1, 0.55)
)

# Toy AgD: trial B (comparator treatment)
trial_b_data <- data.frame(
  trt           = "Drug_B",
  n_total       = 400,
  n_events      = 160,
  age_cat_mean  = 0.35,
  sex_mean      = 0.50
)
```

``` r

# 1. Prepare IPD
ipd <- set_ipd(
  data = trial_a_data,
  treatment = "trt",
  outcome = "response",
  covariates = c("age_cat", "sex")
)

# 2. Prepare AgD
agd <- set_agd(
  data = trial_b_data,
  treatment = "trt",
  outcome_n = "n_total",
  outcome_r = "n_events",
  cov_means = c("age_cat_mean", "sex_mean"),
  cov_types = c("binary", "binary")
)

# 3. Combine
dat <- combine_data(ipd, agd)

# 4. Add integration points (needed for ML-UMR)
dat <- add_integration(
  dat,
  n_int = 64,
  age_cat = distr(qbern, prob = age_cat_mean),
  sex = distr(qbern, prob = sex_mean)
)
```

``` r

# 5. Fit ML-UMR
fit_spfa <- mlumr(
  dat, model = "spfa",
  chains = 2, iter = 500, warmup = 250,
  seed = 42, refresh = 0, verbose = FALSE
)
fit_relaxed <- mlumr(
  dat, model = "relaxed",
  chains = 2, iter = 500, warmup = 250,
  seed = 43, refresh = 0, verbose = FALSE
)

# 6. Compare
summary(fit_spfa)
#> ML-UMR Model Summary
#> ====================
#> 
#> Model: SPFA 
#> Family: Binary 
#> Link: logit 
#> Engine: rstan 
#> Treatments: Drug_A (IPD) vs Drug_B (AgD)
#> 
#> MCMC Diagnostics:
#>   Divergent transitions: 0 
#>   Max treedepth hits: 0 
#>   Max Rhat: 1.016 
#>   Min ESS: 149 
#> 
#> Intercepts (logit scale):
#>       variable        mean        sd       2.5%      97.5%     Rhat
#>       mu_index  0.01040724 0.1915921 -0.3307466  0.3815756 1.007205
#>  mu_comparator -0.63607867 0.1700693 -0.9595353 -0.3209510 1.015403
#> 
#> Regression Coefficients:
#>  variable       mean        sd       2.5%     97.5%      Rhat
#>   beta[1] -0.1538366 0.2480568 -0.6437745 0.3240322 0.9976325
#>   beta[2]  0.5748965 0.2306527  0.1285506 1.0311792 1.0164810
#> 
#> Marginal Treatment Effects:
#>   Log Odds Ratios:
#>        variable      mean        sd      2.5%     97.5%
#>       lor_index 0.6293387 0.1529275 0.3198898 0.9268537
#>  lor_comparator 0.6296880 0.1530364 0.3199753 0.9284606
#>   Risk Differences:
#>       variable      mean         sd       2.5%     97.5%
#>       rd_index 0.1553421 0.03716764 0.07954897 0.2274495
#>  rd_comparator 0.1553186 0.03719440 0.07955894 0.2276036
#>   Risk Ratios:
#>       variable     mean        sd     2.5%    97.5%
#>       rr_index 1.390010 0.1108687 1.182237 1.622135
#>  rr_comparator 1.394113 0.1115285 1.182954 1.627734
compare_models(fit_spfa, fit_relaxed)
#> 
#> Model Comparison (DIC)
#> ======================
#> 
#>         Model    DIC   pD Delta_DIC
#>  Relaxed SPFA 419.68 3.88      0.00
#>          SPFA 420.17 4.05      0.48
#> 
#> Lower DIC = better fit. Delta_DIC > 5 is a rough heuristic for
#> meaningful difference, not a formally calibrated threshold.
#> DIC should not be the sole basis for model selection.

# 7. Frequentist benchmarks
naive_result <- naive(dat)
stc_result <- stc(dat)
print(naive_result)
#> Naive Unadjusted Indirect Comparison
#> =====================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Event rates:
#>   Index (IPD):      0.560 (168/300)
#>   Comparator (AgD): 0.400 (160/400)
#> 
#> Log Odds Ratio: 0.6466 (SE: 0.1547)
#> 95% CI: [0.3433, 0.9499]
print(stc_result)
#> Simulated Treatment Comparison (G-computation)
#> ===============================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Marginalized P(Y=1|index trt, comp pop): 0.5555
#> Observed P(Y=1|comp trt, comp pop):      0.4000
#> 
#> Log Odds Ratio: 0.6285 (SE: 0.1549)
#> 95% CI: [0.3250, 0.9321]
#> 
#> Outcome model coefficients:
#> (Intercept)     age_cat         sex 
#>      0.0133     -0.1527      0.5697
```

Before using results in a real analysis, inspect integration quality and
posterior diagnostics:

``` r

check_integration(
  dat,
  age_cat = distr(qbern, prob = age_cat_mean),
  sex = distr(qbern, prob = sex_mean)
)
#> Integration check: n_int = 64 vs 128
#> Marginals -- max relative difference: 0.0476
#> CAUTION: 1-5%% marginal relative difference. Consider increasing n_int.
#> Joint -- max |cor(current) - cor(doubled)|: 0.0012
#> OK joint: pairwise correlations agree within 0.05.
prior_summary(fit_spfa)
#> Priors for ML-UMR Fit
#> =====================
#> 
#> Intercepts (mu_index, mu_comparator):
#>   normal(0, 10)
#>   (package default, mlumr 0.1.0)
#> 
#> Regression coefficients (beta):
#>   normal(0, 2.5) applied to all 2 covariate(s)
#>   (package default, mlumr 0.1.0)
fit_spfa$diagnostics
#> $n_divergent
#> [1] 0
#> 
#> $n_max_treedepth
#> [1] 0
max(fit_spfa$summary$Rhat, na.rm = TRUE)
#> [1] 1.016481
min(fit_spfa$summary$n_eff, na.rm = TRUE)
#> [1] 148.6844
```

## Choosing a method

| Method | Adjusts for covariates? | Effect modification? | Inference |
|----|----|----|----|
| ML-UMR SPFA | Yes | No (shared betas) | Bayesian |
| ML-UMR Relaxed | Yes | Yes (treatment-specific betas) | Bayesian |
| STC | Yes (outcome model only) | No | Frequentist |
| Naive | No | N/A | Frequentist |

- **ML-UMR SPFA** is appropriate when prognostic factors affect both
  treatments equally.
- **ML-UMR Relaxed** is appropriate when you suspect
  treatment-by-covariate interactions (effect modification).
- **STC** adjusts for imbalances through an outcome model and uses
  integration points when available; best when treatment effects do not
  vary with covariates.
- **Naive** serves as a benchmark only; do not use for inference in the
  presence of cross-trial differences.
