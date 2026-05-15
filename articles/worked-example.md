# Worked Example: Complete Analysis

## Scenario

We have IPD from a trial of **Drug A** (index treatment, N=500) and
published AgD from a trial of **Drug B** (comparator, N=400). We want to
estimate the treatment effect of Drug A vs Drug B on a binary endpoint.

Two binary covariates (age group and sex) are available in both trials
but their distributions differ:

- Drug A trial: 40% elderly, 55% female
- Drug B trial: 35% elderly, 50% female

## Step 1: Simulate example data

``` r

library(mlumr)
set.seed(2026)

# --- IPD for Drug A ---
n_A <- 500
age_A <- rbinom(n_A, 1, 0.40)
sex_A <- rbinom(n_A, 1, 0.55)

# True model: logit(p) = -0.5 + 0.8*age - 0.3*sex
logit_p_A <- -0.5 + 0.8 * age_A - 0.3 * sex_A
y_A <- rbinom(n_A, 1, plogis(logit_p_A))

ipd_df <- data.frame(
  trt = "Drug_A",
  outcome = y_A,
  age_group = age_A,
  sex = sex_A
)

# --- AgD for Drug B ---
# True comparator model: logit(p) = -0.8 + 0.8*age - 0.3*sex
# (same covariate effects, different intercept)
n_B <- 400
age_B <- rbinom(n_B, 1, 0.35)
sex_B <- rbinom(n_B, 1, 0.50)
logit_p_B <- -0.8 + 0.8 * age_B - 0.3 * sex_B
y_B <- rbinom(n_B, 1, plogis(logit_p_B))

agd_df <- data.frame(
  trt = "Drug_B",
  n_total = n_B,
  n_events = sum(y_B),
  age_group_mean = mean(age_B),
  sex_mean = mean(sex_B)
)
```

## Step 2: Prepare data objects

``` r

ipd <- set_ipd(ipd_df, treatment = "trt", outcome = "outcome",
               covariates = c("age_group", "sex"))

agd <- set_agd(agd_df, treatment = "trt",
               outcome_n = "n_total", outcome_r = "n_events",
               cov_means = c("age_group_mean", "sex_mean"),
               cov_types = c("binary", "binary"))

dat <- combine_data(ipd, agd)
print(dat)
#> Unanchored Comparison Data (Binary)
#> ====================================
#> 
#> Index treatment (IPD): Drug_A 
#>   N = 500 
#>   Events = 205 (41.0%) 
#> 
#> Comparator treatment (AgD): Drug_B 
#>   N = 400 
#>   Events = 145 (36.2%) 
#> 
#> Covariates ( 2 ): age_group, sex 
#> Integration points: not yet added (use add_integration())
```

## Step 3: Add integration points

``` r

dat <- add_integration(
  dat,
  n_int = 64,
  age_group = distr(qbern, prob = age_group_mean),
  sex = distr(qbern, prob = sex_mean)
)

check_integration(
  dat,
  age_group = distr(qbern, prob = age_group_mean),
  sex = distr(qbern, prob = sex_mean)
)
#> Integration check: n_int = 64 vs 128
#> Marginals -- max relative difference: 0.0154
#> CAUTION: 1-5%% marginal relative difference. Consider increasing n_int.
#> Joint -- max |cor(current) - cor(doubled)|: 0.0216
#> OK joint: pairwise correlations agree within 0.05.
```

## Step 4: Naive estimate (benchmark)

``` r

naive_result <- naive(dat)
print(naive_result)
#> Naive Unadjusted Indirect Comparison
#> =====================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Event rates:
#>   Index (IPD):      0.410 (205/500)
#>   Comparator (AgD): 0.362 (145/400)
#> 
#> Log Odds Ratio: 0.2006 (SE: 0.1382)
#> 95% CI: [-0.0702, 0.4713]
```

## Step 5: STC estimate

``` r

stc_result <- stc(dat)
print(stc_result)
#> Simulated Treatment Comparison (G-computation)
#> ===============================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Marginalized P(Y=1|index trt, comp pop): 0.4006
#> Observed P(Y=1|comp trt, comp pop):      0.3625
#> 
#> Log Odds Ratio: 0.1614 (SE: 0.1375)
#> 95% CI: [-0.1081, 0.4308]
#> 
#> Outcome model coefficients:
#> (Intercept)   age_group         sex 
#>     -0.6545      0.8477     -0.1063
```

## Step 6: ML-UMR SPFA model

``` r

fit_spfa <- mlumr(
  dat,
  model = "spfa",
  prior_intercept = prior_normal(0, 10),
  prior_beta = prior_normal(0, 2.5),
  chains = 2,
  iter = 500,
  warmup = 250,
  seed = 42,
  refresh = 0,
  verbose = FALSE
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
#>   Max Rhat: 1.023 
#>   Min ESS: 131 
#> 
#> Intercepts (logit scale):
#>       variable       mean        sd       2.5%      97.5%     Rhat
#>       mu_index -0.6719067 0.1462933 -0.9292307 -0.3554525 1.022857
#>  mu_comparator -0.8430577 0.1534397 -1.1187600 -0.5327863 1.019547
#> 
#> Regression Coefficients:
#>  variable        mean        sd       2.5%     97.5%     Rhat
#>   beta[1]  0.85636206 0.1875140  0.4816783 1.2066139 1.014797
#>   beta[2] -0.08643791 0.1883094 -0.4690963 0.2371305 1.019971
#> 
#> Marginal Treatment Effects:
#>   Log Odds Ratios:
#>        variable      mean        sd        2.5%     97.5%
#>       lor_index 0.1633394 0.1342548 -0.09609159 0.4281837
#>  lor_comparator 0.1636748 0.1345271 -0.09633530 0.4289090
#>   Risk Differences:
#>       variable       mean         sd        2.5%      97.5%
#>       rd_index 0.03870014 0.03179979 -0.02309618 0.10041522
#>  rd_comparator 0.03841876 0.03157799 -0.02294458 0.09978104
#>   Risk Ratios:
#>       variable     mean         sd      2.5%    97.5%
#>       rr_index 1.108785 0.09111798 0.9439559 1.306502
#>  rr_comparator 1.110864 0.09285184 0.9428912 1.312051
prior_summary(fit_spfa)
#> Priors for ML-UMR Fit
#> =====================
#> 
#> Intercepts (mu_index, mu_comparator):
#>   normal(0, 10)
#> 
#> Regression coefficients (beta):
#>   normal(0, 2.5) applied to all 2 covariate(s)
```

## Step 7: ML-UMR Relaxed model

``` r

fit_relaxed <- mlumr(
  dat,
  model = "relaxed",
  prior_intercept = prior_normal(0, 10),
  prior_beta = prior_normal(0, 2.5),
  chains = 2,
  iter = 500,
  warmup = 250,
  seed = 43,
  refresh = 0,
  verbose = FALSE
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
#>   Max Rhat: 1.019 
#>   Min ESS: 129 
#> 
#> Intercepts (logit scale):
#>       variable       mean        sd       2.5%      97.5%     Rhat
#>       mu_index -0.6767692 0.1518303 -0.9579614 -0.3771797 1.005002
#>  mu_comparator -1.1563243 1.7339913 -4.3897076  1.6697488 1.001115
#> 
#> Regression Coefficients:
#>            variable        mean        sd       2.5%     97.5%      Rhat
#>       beta_index[1]  0.85737150 0.1847564  0.4910975 1.1893471 0.9999757
#>       beta_index[2] -0.08417422 0.1938599 -0.4530986 0.2956108 0.9972565
#>  beta_comparator[1]  0.39049689 3.0338891 -5.1242111 6.0855273 1.0005843
#>  beta_comparator[2]  0.08080408 2.7417192 -5.1745280 5.2409806 1.0006716
#> 
#> Marginal Treatment Effects:
#>   Log Odds Ratios:
#>        variable      mean        sd       2.5%     97.5%
#>       lor_index 0.1709452 0.1864900 -0.1901254 0.5294903
#>  lor_comparator 0.1509587 0.1450953 -0.1435969 0.4460422
#>   Risk Differences:
#>       variable       mean         sd        2.5%     97.5%
#>       rd_index 0.03997569 0.04374924 -0.04620561 0.1226960
#>  rd_comparator 0.03540224 0.03406476 -0.03416202 0.1058761
#>   Risk Ratios:
#>       variable     mean         sd      2.5%    97.5%
#>       rr_index 1.119922 0.12946489 0.8955887 1.392123
#>  rr_comparator 1.102856 0.09946903 0.9162735 1.314851
prior_summary(fit_relaxed)
#> Priors for ML-UMR Fit
#> =====================
#> 
#> Intercepts (mu_index, mu_comparator):
#>   normal(0, 10)
#> 
#> Regression coefficients (beta):
#>   normal(0, 2.5) applied to all 2 covariate(s)
```

## Step 8: Model comparison

``` r

compare_models(fit_spfa, fit_relaxed)
#> 
#> Model Comparison (DIC)
#> ======================
#> 
#>         Model    DIC   pD Delta_DIC
#>          SPFA 669.82 3.38      0.00
#>  Relaxed SPFA 671.23 4.38      1.41
#> 
#> Lower DIC = better fit. Delta_DIC > 5 is a rough heuristic for
#> meaningful difference, not a formally calibrated threshold.
#> DIC should not be the sole basis for model selection.
```

## Step 9: Extract and compare results

``` r

# ML-UMR marginal effects
me_spfa <- marginal_effects(fit_spfa, effect = "lor")
me_relaxed <- marginal_effects(fit_relaxed, effect = "lor")

# Build comparison table
results <- data.frame(
  Method = c("Naive", "STC",
             "ML-UMR SPFA (Index)", "ML-UMR SPFA (Comparator)",
             "ML-UMR Relaxed (Index)", "ML-UMR Relaxed (Comparator)"),
  LOR = c(
    naive_result$link_effect,
    stc_result$link_effect,
    me_spfa$mean[me_spfa$population == "Index"],
    me_spfa$mean[me_spfa$population == "Comparator"],
    me_relaxed$mean[me_relaxed$population == "Index"],
    me_relaxed$mean[me_relaxed$population == "Comparator"]
  ),
  SE = c(
    naive_result$se,
    stc_result$se,
    me_spfa$sd[me_spfa$population == "Index"],
    me_spfa$sd[me_spfa$population == "Comparator"],
    me_relaxed$sd[me_relaxed$population == "Index"],
    me_relaxed$sd[me_relaxed$population == "Comparator"]
  )
)
results$CI_lower <- c(
  naive_result$ci_lower,
  stc_result$ci_lower,
  me_spfa$q2.5[me_spfa$population == "Index"],
  me_spfa$q2.5[me_spfa$population == "Comparator"],
  me_relaxed$q2.5[me_relaxed$population == "Index"],
  me_relaxed$q2.5[me_relaxed$population == "Comparator"]
)
results$CI_upper <- c(
  naive_result$ci_upper,
  stc_result$ci_upper,
  me_spfa$q97.5[me_spfa$population == "Index"],
  me_spfa$q97.5[me_spfa$population == "Comparator"],
  me_relaxed$q97.5[me_relaxed$population == "Index"],
  me_relaxed$q97.5[me_relaxed$population == "Comparator"]
)

print(results, digits = 3)
#>                        Method   LOR    SE CI_lower CI_upper
#> 1                       Naive 0.201 0.138  -0.0702    0.471
#> 2                         STC 0.161 0.137  -0.1081    0.431
#> 3         ML-UMR SPFA (Index) 0.163 0.134  -0.0961    0.428
#> 4    ML-UMR SPFA (Comparator) 0.164 0.135  -0.0963    0.429
#> 5      ML-UMR Relaxed (Index) 0.171 0.186  -0.1901    0.529
#> 6 ML-UMR Relaxed (Comparator) 0.151 0.145  -0.1436    0.446
```

## Step 10: Predicted probabilities

``` r

# Predicted event probabilities for each treatment in each population
preds <- predict(fit_spfa, population = "both")
print(preds)
#>   treatment population      mean         sd      q2.5       q50     q97.5
#> 1    Drug_A      Index 0.4096603 0.02072165 0.3711605 0.4095501 0.4502921
#> 2    Drug_B      Index 0.3709601 0.02313690 0.3285836 0.3696950 0.4171254
#> 3    Drug_A Comparator 0.4000036 0.02073565 0.3614734 0.3994409 0.4414920
#> 4    Drug_B Comparator 0.3615848 0.02298574 0.3195145 0.3603637 0.4065253
```

## Step 11: Conditional effects at covariate profiles

Population-level effects average over the covariate distribution.
Conditional effects evaluate the treatment effect at specific covariate
values.

``` r

profiles <- data.frame(
  age_group = c(0, 1),
  sex = c(0, 1)
)

conditional_predict(fit_spfa, newdata = profiles)
#>   profile treatment      mean         sd      q2.5       q50     q97.5
#> 1       1    Drug_A 0.3388252 0.03286710 0.2830808 0.3365797 0.4120608
#> 2       1    Drug_B 0.3018660 0.03225646 0.2462414 0.3017908 0.3698674
#> 3       2    Drug_A 0.5243367 0.03842272 0.4505354 0.5222893 0.6020464
#> 4       2    Drug_B 0.4818548 0.04071288 0.4027168 0.4837743 0.5635341
conditional_effects(fit_spfa, newdata = profiles)
#>   profile      effect       mean         sd        q2.5        q50      q97.5
#> 1       1 LINK_EFFECT 0.17115101 0.14063915 -0.10022090 0.17678926 0.44671644
#> 2       1          RD 0.03695916 0.03057094 -0.02232146 0.03835031 0.09629932
#> 3       1          RR 1.12878013 0.10802740  0.93352670 1.12723757 1.35942920
#> 4       2 LINK_EFFECT 0.17115101 0.14063915 -0.10022090 0.17678926 0.44671644
#> 5       2          RD 0.04248188 0.03487628 -0.02503725 0.04413270 0.11086334
#> 6       2          RR 1.09189975 0.07760632  0.95295354 1.09146159 1.25566243
```

## Interpretation

In this simulated example:

- The **true LOR** is approximately `logit(p_A) - logit(p_B)` where
  treatment effects are captured by the intercept difference (0.3 on
  logit scale).
- The **naive estimate** is biased because of covariate imbalance
  between populations.
- **STC** partially corrects for this by adjusting via outcome
  regression.
- **ML-UMR SPFA** provides population-specific treatment effects in both
  the index and comparator populations, fully accounting for covariate
  differences through QMC integration.
- **ML-UMR Relaxed** additionally allows for effect modification, though
  in this simulated case (same covariate effects), SPFA and Relaxed
  should give similar results.
