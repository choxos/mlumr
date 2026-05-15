# Comparing ML-UMR, STC, and Naive Methods

## Why compare methods?

Unanchored indirect treatment comparisons (ITCs) rely on strong
assumptions that cannot be fully verified from data alone. Running
multiple methods and comparing their results is an important part of any
ITC analysis: a sensitivity check that reveals how conclusions depend on
modeling choices.

mlumr provides three methods in a single package with a unified data
interface, making side-by-side comparison straightforward.

| Method | Adjustment | Framework | Key assumption |
|----|----|----|----|
| Naive | None | Frequentist | Populations are exchangeable |
| STC | Outcome regression | Frequentist | Correct outcome model specification |
| ML-UMR SPFA | Joint Bayesian model | Bayesian | Shared prognostic effects (SPFA) |
| ML-UMR Relaxed | Joint Bayesian model | Bayesian | Correct specification and enough information/prior support for treatment-specific effects |

## Setup: shared data preparation

All three methods start from the same `mlumr_data` object. STC and
ML-UMR benefit from integration points; the naive method ignores them.

``` r

library(mlumr)
set.seed(2026)

# --- Simulate IPD (index treatment) ---
n_A <- 500
age_A <- rbinom(n_A, 1, 0.40)
sex_A <- rbinom(n_A, 1, 0.55)

# True DGP: logit(p) = -0.5 + 0.8*age - 0.3*sex
logit_p_A <- -0.5 + 0.8 * age_A - 0.3 * sex_A
y_A <- rbinom(n_A, 1, plogis(logit_p_A))

ipd_df <- data.frame(
  trt = "Drug_A", outcome = y_A,
  age_group = age_A, sex = sex_A
)

# --- Simulate AgD (comparator) ---
# Same covariate effects, different intercept and covariate distribution
n_B <- 400
r_B <- 148  # pre-computed from true model with different covariates

agd_df <- data.frame(
  trt = "Drug_B", n_total = n_B, n_events = r_B,
  age_group_mean = 0.35, sex_mean = 0.50
)

# --- Prepare data ---
ipd <- set_ipd(ipd_df, treatment = "trt", outcome = "outcome",
               covariates = c("age_group", "sex"))
agd <- set_agd(agd_df, treatment = "trt",
               outcome_n = "n_total", outcome_r = "n_events",
               cov_means = c("age_group_mean", "sex_mean"),
               cov_types = c("binary", "binary"))

dat <- combine_data(ipd, agd)

# Add integration points (used by STC and ML-UMR)
dat <- add_integration(
  dat, n_int = 64,
  age_group = distr(qbern, prob = age_group_mean),
  sex = distr(qbern, prob = sex_mean)
)
```

## Running all three methods

### Naive estimate

``` r

res_naive <- naive(dat)
print(res_naive)
#> Naive Unadjusted Indirect Comparison
#> =====================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Event rates:
#>   Index (IPD):      0.410 (205/500)
#>   Comparator (AgD): 0.370 (148/400)
#> 
#> Log Odds Ratio: 0.1683 (SE: 0.1378)
#> 95% CI: [-0.1019, 0.4384]
```

The naive method ignores covariate differences between populations. It
serves as a reference: if naive and adjusted estimates agree, covariate
imbalance has little practical impact.

### STC estimate

``` r

res_stc <- stc(dat)
print(res_stc)
#> Simulated Treatment Comparison (G-computation)
#> ===============================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Marginalized P(Y=1|index trt, comp pop): 0.4013
#> Observed P(Y=1|comp trt, comp pop):      0.3700
#> 
#> Log Odds Ratio: 0.1321 (SE: 0.1375)
#> 95% CI: [-0.1373, 0.4015]
#> 
#> Outcome model coefficients:
#> (Intercept)   age_group         sex 
#>     -0.6545      0.8477     -0.1063
```

STC fits a logistic regression on IPD and predicts counterfactual
outcomes for the comparator population via G-computation. The delta
method provides frequentist standard errors. STC is fast (sub-second)
and a good default when covariate adjustment is needed but a full
Bayesian model is not warranted.

### ML-UMR SPFA

``` r

fit_spfa <- mlumr(
  dat, model = "spfa",
  prior_intercept = prior_normal(0, 10),
  prior_beta = prior_normal(0, 2.5),
  chains = 2, iter = 500, warmup = 250,
  seed = 42, refresh = 0, verbose = FALSE
)
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
#>   Max Rhat: 1.008 
#>   Min ESS: 123 
#> 
#> Intercepts (logit scale):
#>       variable       mean        sd       2.5%      97.5%      Rhat
#>       mu_index -0.6504984 0.1562830 -0.9497378 -0.3353501 0.9990681
#>  mu_comparator -0.8007299 0.1538053 -1.1324019 -0.4791986 1.0054141
#> 
#> Regression Coefficients:
#>  variable       mean        sd       2.5%     97.5%     Rhat
#>   beta[1]  0.8437293 0.2053132  0.4392676 1.2323737 1.006647
#>   beta[2] -0.1129692 0.1849058 -0.4669804 0.2337334 1.003014
#> 
#> Marginal Treatment Effects:
#>   Log Odds Ratios:
#>        variable      mean        sd       2.5%     97.5%
#>       lor_index 0.1437993 0.1368440 -0.1022970 0.4121727
#>  lor_comparator 0.1441575 0.1371719 -0.1024831 0.4129336
#>   Risk Differences:
#>       variable       mean         sd        2.5%      97.5%
#>       rd_index 0.03411178 0.03236864 -0.02436672 0.09788732
#>  rd_comparator 0.03393758 0.03220645 -0.02410684 0.09729075
#>   Risk Ratios:
#>       variable     mean         sd      2.5%    97.5%
#>       rr_index 1.095557 0.09293487 0.9396591 1.294386
#>  rr_comparator 1.097164 0.09443462 0.9383092 1.297817
```

The SPFA model assumes shared covariate effects across treatments. It
jointly models both data sources and produces posterior distributions
for all parameters.

### ML-UMR Relaxed

``` r

fit_relaxed <- mlumr(
  dat, model = "relaxed",
  prior_intercept = prior_normal(0, 10),
  prior_beta = prior_normal(0, 2.5),
  chains = 2, iter = 500, warmup = 250,
  seed = 43, refresh = 0, verbose = FALSE
)
summary(fit_relaxed)
#> ML-UMR Model Summary
#> ====================
#> 
#> Model: Relaxed SPFA 
#> Family: Binary 
#> Link: logit 
#> Engine: rstan 
#> Treatments: Drug_A (IPD) vs Drug_B (AgD)
#> 
#> MCMC Diagnostics:
#>   Divergent transitions: 0 
#>   Max treedepth hits: 0 
#>   Max Rhat: 1.009 
#>   Min ESS: 151 
#> 
#> Intercepts (logit scale):
#>       variable       mean        sd       2.5%      97.5%     Rhat
#>       mu_index -0.6506521 0.1552833 -0.9733024 -0.3444626 1.003992
#>  mu_comparator -0.9916549 1.6189626 -4.4117344  1.7617439 1.002768
#> 
#> Regression Coefficients:
#>            variable       mean        sd       2.5%     97.5%      Rhat
#>       beta_index[1]  0.8445558 0.1998645  0.4549557 1.2394099 0.9994025
#>       beta_index[2] -0.1140334 0.1987585 -0.4830300 0.2641751 0.9993862
#>  beta_comparator[1]  0.1762647 2.8346721 -5.2196499 5.4027178 1.0008780
#>  beta_comparator[2]  0.1190578 2.6669410 -4.4449794 5.2766561 1.0039737
#> 
#> Marginal Treatment Effects:
#>   Log Odds Ratios:
#>        variable      mean        sd       2.5%     97.5%
#>       lor_index 0.1432492 0.1953916 -0.2070638 0.5380411
#>  lor_comparator 0.1244781 0.1363172 -0.1377880 0.3891701
#>   Risk Differences:
#>       variable       mean         sd        2.5%      97.5%
#>       rd_index 0.03339867 0.04570687 -0.05061295 0.12466173
#>  rd_comparator 0.02939591 0.03218000 -0.03257277 0.09120433
#>   Risk Ratios:
#>       variable     mean         sd      2.5%    97.5%
#>       rr_index 1.102062 0.13685994 0.8868852 1.411378
#>  rr_comparator 1.083395 0.09086321 0.9182389 1.272522
```

The Relaxed model allows treatment-specific covariate coefficients,
capturing potential effect modification. Compare it with SPFA to assess
whether assuming shared effects is reasonable. With sparse AgD,
relaxed-model comparator coefficients can be prior-sensitive, so inspect
`delta_beta` and run prior-sensitivity checks when the relaxed model is
central to the interpretation.

## Building a comparison table

``` r

# Extract ML-UMR marginal effects (LOR in comparator population)
me_spfa <- marginal_effects(fit_spfa, effect = "lor")
me_relaxed <- marginal_effects(fit_relaxed, effect = "lor")

# Comparator-population LORs from ML-UMR
lor_spfa <- me_spfa[me_spfa$population == "Comparator", ]
lor_relaxed <- me_relaxed[me_relaxed$population == "Comparator", ]

# Assemble results
comparison <- data.frame(
  Method = c("Naive", "STC", "ML-UMR SPFA", "ML-UMR Relaxed"),
  LOR = c(res_naive$link_effect, res_stc$link_effect, lor_spfa$mean, lor_relaxed$mean),
  SE = c(res_naive$se, res_stc$se, lor_spfa$sd, lor_relaxed$sd),
  CI_lower = c(res_naive$ci_lower, res_stc$ci_lower,
               lor_spfa$q2.5, lor_relaxed$q2.5),
  CI_upper = c(res_naive$ci_upper, res_stc$ci_upper,
               lor_spfa$q97.5, lor_relaxed$q97.5),
  stringsAsFactors = FALSE
)

# Add odds ratios for clinical interpretation
comparison$OR <- exp(comparison$LOR)
comparison$OR_lower <- exp(comparison$CI_lower)
comparison$OR_upper <- exp(comparison$CI_upper)

print(comparison, digits = 3)
#>           Method   LOR    SE CI_lower CI_upper   OR OR_lower OR_upper
#> 1          Naive 0.168 0.138   -0.102    0.438 1.18    0.903     1.55
#> 2            STC 0.132 0.137   -0.137    0.402 1.14    0.872     1.49
#> 3    ML-UMR SPFA 0.144 0.137   -0.102    0.413 1.16    0.903     1.51
#> 4 ML-UMR Relaxed 0.124 0.136   -0.138    0.389 1.13    0.871     1.48
```

## Interpreting differences between methods

### Naive vs STC

A large gap between naive and STC estimates indicates that covariate
imbalance is influencing the unadjusted comparison. The direction of the
shift reveals which population’s covariates favor the outcome.

``` r

bias_naive <- res_naive$link_effect - res_stc$link_effect
cat(sprintf("Apparent bias from covariate imbalance: %.3f (log OR scale)\n",
            bias_naive))
#> Apparent bias from covariate imbalance: 0.036 (log OR scale)
```

### STC vs ML-UMR SPFA

Under the shared prognostic factor assumption, STC and ML-UMR SPFA
should give similar point estimates because both adjust for the same
covariates. Differences arise because:

1.  **STC** uses maximum likelihood; **ML-UMR** uses Bayesian inference
    with priors. With large samples, this difference is small.
2.  **ML-UMR** jointly models both data sources; **STC** only uses AgD
    for prediction targets, not as a likelihood contribution.
3.  ML-UMR uncertainty intervals tend to be wider because they account
    for more sources of uncertainty (prior, integration, joint
    modeling).

### SPFA vs Relaxed

``` r

dic_comparison <- compare_models(fit_spfa, fit_relaxed)
#> 
#> Model Comparison (DIC)
#> ======================
#> 
#>         Model    DIC   pD Delta_DIC
#>  Relaxed SPFA 670.58 3.89      0.00
#>          SPFA 670.74 4.02      0.17
#> 
#> Lower DIC = better fit. Delta_DIC > 5 is a rough heuristic for
#> meaningful difference, not a formally calibrated threshold.
#> DIC should not be the sole basis for model selection.
print(dic_comparison)
#>          Model    DIC   pD Delta_DIC
#> 1 Relaxed SPFA 670.58 3.89      0.00
#> 2         SPFA 670.74 4.02      0.17
```

If the Relaxed model gives markedly different LORs or substantially
better DIC, this *may* suggest effect modification – covariate effects
differ by treatment. However, DIC is a rough metric with known
limitations: it should not be the sole basis for claiming effect
modification. Always inspect `delta_beta` credible intervals, prior
sensitivity, and clinical plausibility. If SPFA and Relaxed agree, the
simpler SPFA model is preferred.

``` r

# Directly compare delta_beta from the Relaxed model
# Non-zero delta_beta indicates effect modification
relaxed_summary <- fit_relaxed$summary
delta_rows <- grepl("^delta_beta", relaxed_summary$variable)
if (any(delta_rows)) {
  cat("Effect modification parameters (delta_beta):\n")
  print(relaxed_summary[delta_rows, c("variable", "mean", "2.5%", "97.5%")],
        row.names = FALSE)
}
#> Effect modification parameters (delta_beta):
#>       variable       mean      2.5%    97.5%
#>  delta_beta[1]  0.6682911 -4.721495 6.144315
#>  delta_beta[2] -0.2330913 -5.506909 4.368791
```

``` r

prior_sensitivity(fit_relaxed, prior_beta_scales = c(1, 2.5, 5, 10))
```

## Decision guide: which method to report?

The choice of primary analysis depends on the clinical and regulatory
context. This flowchart summarizes the main considerations:

1.  **Are covariate distributions similar across trials?**
    - Yes: Naive estimate may suffice as primary, with STC as
      sensitivity.
    - No: Adjustment is needed. Proceed to step 2.
2.  **Is the SPFA assumption plausible?**
    - Yes: ML-UMR SPFA is the recommended primary analysis. Report STC
      and Naive as sensitivity analyses.
    - Uncertain: Run both SPFA and Relaxed. If they agree, report SPFA.
      If they disagree, report Relaxed as primary and discuss the
      evidence for effect modification.
    - No (known effect modification): ML-UMR Relaxed as primary.
3.  **Is Bayesian analysis acceptable to the audience?**
    - No: Report STC as primary with ML-UMR as sensitivity.
    - Yes: Report ML-UMR as primary with STC as sensitivity.

In all cases, report the naive estimate as a benchmark to quantify the
impact of covariate adjustment.

## Presenting results

For a complete ITC report, present:

1.  **A summary table** (as above) with LOR, SE, and CI for each method
2.  **Model diagnostics** for ML-UMR (divergences, Rhat, ESS)
3.  **DIC comparison** between SPFA and Relaxed (if both were fit)
4.  **A narrative** explaining which method is primary and why, with
    sensitivity analyses supporting the conclusions

``` r

# Example narrative output
cat("Primary analysis: ML-UMR SPFA\n")
#> Primary analysis: ML-UMR SPFA
cat(sprintf("  LOR = %.3f (95%% CrI: %.3f to %.3f)\n",
            lor_spfa$mean, lor_spfa$q2.5, lor_spfa$q97.5))
#>   LOR = 0.144 (95% CrI: -0.102 to 0.413)
cat(sprintf("  OR  = %.3f (95%% CrI: %.3f to %.3f)\n\n",
            exp(lor_spfa$mean), exp(lor_spfa$q2.5), exp(lor_spfa$q97.5)))
#>   OR  = 1.155 (95% CrI: 0.903 to 1.511)

cat("Sensitivity analyses:\n")
#> Sensitivity analyses:
cat(sprintf("  STC:    LOR = %.3f (95%% CI: %.3f to %.3f)\n",
            res_stc$link_effect, res_stc$ci_lower, res_stc$ci_upper))
#>   STC:    LOR = 0.132 (95% CI: -0.137 to 0.402)
cat(sprintf("  Naive:  LOR = %.3f (95%% CI: %.3f to %.3f)\n",
            res_naive$link_effect, res_naive$ci_lower, res_naive$ci_upper))
#>   Naive:  LOR = 0.168 (95% CI: -0.102 to 0.438)
cat(sprintf("  Relaxed: LOR = %.3f (95%% CrI: %.3f to %.3f)\n",
            lor_relaxed$mean, lor_relaxed$q2.5, lor_relaxed$q97.5))
#>   Relaxed: LOR = 0.124 (95% CrI: -0.138 to 0.389)
```

## Summary

Running all three methods and comparing their results strengthens any
unanchored ITC analysis. mlumr’s unified data interface makes this
comparison straightforward – the same `mlumr_data` object feeds all
methods, ensuring consistency in the inputs.

Key takeaways:

- **Always report the naive estimate** as a baseline to show the impact
  of adjustment.
- **STC is a fast, frequentist alternative** that adjusts for measured
  covariates via outcome regression.
- **ML-UMR SPFA** is the recommended primary analysis under the shared
  prognostic factor assumption, offering fully Bayesian uncertainty
  quantification.
- **ML-UMR Relaxed** is a useful sensitivity analysis when effect
  modification is plausible.
- **Compare SPFA and Relaxed via DIC** as a rough sensitivity check. DIC
  is a legacy metric with known limitations; treat large differences as
  suggestive rather than definitive evidence for effect modification.
