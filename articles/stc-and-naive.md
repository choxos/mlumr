# STC and Naive Benchmarks

## Overview

In addition to ML-UMR, mlumr provides two frequentist methods:

- **STC** (Simulated Treatment Comparison): adjusts for cross-trial
  covariate differences using parametric G-computation
- **Naive**: unadjusted comparison of crude outcome summaries

These serve as important benchmarks alongside the Bayesian ML-UMR
approach.

## Naive estimate

For binary outcomes, the naive method compares crude event rates without
any covariate adjustment:

``` math
\text{LOR}_{\text{naive}} = \text{logit}(\hat{p}_A) - \text{logit}(\hat{p}_B)
```

where $`\hat{p}_A = \bar{Y}_{\text{IPD}}`$ and $`\hat{p}_B = r_B / n_B`$
from the AgD. The standard error is computed via the delta method:

``` math
\text{SE} = \sqrt{\frac{1}{n_A \hat{p}_A(1-\hat{p}_A)} + \frac{1}{n_B \hat{p}_B(1-\hat{p}_B)}}
```

### Usage

The chunks below operate on a small toy `dat`. Run this setup once first
so [`naive()`](https://choxos.github.io/mlumr/reference/naive.md),
[`stc()`](https://choxos.github.io/mlumr/reference/stc.md), and the
comparison block all have data to work with.

``` r

library(mlumr)
set.seed(2026)

trial_a_data <- data.frame(
  trt      = "Drug_A",
  response = rbinom(300, 1, 0.55),
  age_cat  = rbinom(300, 1, 0.40),
  sex      = rbinom(300, 1, 0.55)
)
trial_b_data <- data.frame(
  trt           = "Drug_B",
  n_total       = 400,
  n_events      = 160,
  age_cat_mean  = 0.35,
  sex_mean      = 0.50
)

ipd <- set_ipd(trial_a_data, treatment = "trt", outcome = "response",
               covariates = c("age_cat", "sex"))
agd <- set_agd(trial_b_data, treatment = "trt",
               outcome_n = "n_total", outcome_r = "n_events",
               cov_means = c("age_cat_mean", "sex_mean"),
               cov_types = c("binary", "binary"))
```

``` r

# Prepare data (no integration points needed)
dat <- combine_data(ipd, agd)

result <- naive(dat)
print(result)
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
```

### Interpreting the naive estimate

The naive LOR is **biased** when covariate distributions differ between
the IPD and AgD populations. It provides a useful reference point:

- If the naive and adjusted estimates agree, covariate imbalance has
  little impact.
- If they disagree substantially, adjustment is important and the naive
  estimate should not be used for inference.

### Extreme rates

The function includes continuity-correction-style boundary guards for
extreme proportions (0% or 100% event rates), ensuring finite log odds
ratios even at the boundaries. Event-probability confidence intervals
use Wald standard errors and are bounded to `[0, 1]`.

## Simulated Treatment Comparison (STC)

STC uses parametric G-computation to adjust for covariate differences:

1.  **Fit an outcome model** on IPD:
    $`\text{logit}(P(Y_i = 1)) = \alpha + \mathbf{x}_i^T \boldsymbol{\beta}`$
2.  **Predict** counterfactual outcomes for the comparator population
3.  **Marginalize**:
    $`\hat{p}_{A|B} = \frac{1}{N} \sum_j \hat{P}(Y = 1 | \mathbf{x}_j)`$
4.  **Compute LOR**:
    $`\text{LOR} = \text{logit}(\hat{p}_{A|B}) - \text{logit}(\hat{p}_B)`$

The standard error uses the delta method, propagating parameter
uncertainty through the logit-of-mean transformation.

### Usage

``` r

# Without integration points (uses AgD means)
result_stc <- stc(dat)

# With integration points (better marginalization)
dat <- add_integration(dat, n_int = 64,
                       age_cat = distr(qbern, prob = age_cat_mean),
                       sex = distr(qbern, prob = sex_mean))
result_stc <- stc(dat)

print(result_stc)
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

### With vs without integration points

- **Without integration points**: STC predicts only at the AgD covariate
  **means** (a single point). This can introduce bias for non-linear
  models (Jensen’s inequality).
- **With integration points**: STC marginalizes over the full covariate
  distribution, giving a more accurate estimate. We recommend always
  using integration points.

For binary outcomes, event-probability confidence intervals are bounded
to `[0, 1]`. For Poisson outcomes, STC uses a 0.5 continuity correction
for the comparator log rate when the AgD event count is zero; the
reported comparator rate remains the observed rate.

### Accessing the GLM fit

The underlying logistic regression model is stored in the result:

``` r

# Coefficients
coef(result_stc$glm_fit)
#> (Intercept)     age_cat         sex 
#>   0.0133087  -0.1527204   0.5696630

# Full GLM summary
summary(result_stc$glm_fit)
#> 
#> Call:
#> glm(formula = .stc_formula(cov_names, family), family = glm_family, 
#>     data = ipd)
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)  
#> (Intercept)  0.01331    0.18715   0.071   0.9433  
#> age_cat     -0.15272    0.24043  -0.635   0.5253  
#> sex          0.56966    0.23615   2.412   0.0159 *
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 411.56  on 299  degrees of freedom
#> Residual deviance: 405.50  on 297  degrees of freedom
#> AIC: 411.5
#> 
#> Number of Fisher Scoring iterations: 4

# Predicted probabilities
fitted(result_stc$glm_fit)
#>         1         2         3         4         5         6         7         8 
#> 0.6417509 0.5033271 0.6059337 0.5033271 0.6059337 0.6417509 0.4652034 0.5033271 
#>         9        10        11        12        13        14        15        16 
#> 0.6059337 0.4652034 0.6059337 0.6059337 0.6417509 0.4652034 0.5033271 0.4652034 
#>        17        18        19        20        21        22        23        24 
#> 0.5033271 0.5033271 0.4652034 0.6059337 0.6059337 0.6417509 0.6417509 0.4652034 
#>        25        26        27        28        29        30        31        32 
#> 0.5033271 0.6059337 0.4652034 0.4652034 0.6417509 0.4652034 0.6417509 0.6059337 
#>        33        34        35        36        37        38        39        40 
#> 0.6059337 0.4652034 0.6417509 0.4652034 0.5033271 0.5033271 0.5033271 0.6059337 
#>        41        42        43        44        45        46        47        48 
#> 0.5033271 0.5033271 0.5033271 0.6417509 0.6059337 0.5033271 0.5033271 0.6417509 
#>        49        50        51        52        53        54        55        56 
#> 0.5033271 0.6417509 0.5033271 0.4652034 0.5033271 0.6417509 0.6059337 0.6059337 
#>        57        58        59        60        61        62        63        64 
#> 0.4652034 0.6417509 0.5033271 0.4652034 0.6417509 0.6417509 0.6059337 0.6417509 
#>        65        66        67        68        69        70        71        72 
#> 0.6059337 0.6417509 0.6059337 0.4652034 0.6417509 0.4652034 0.4652034 0.5033271 
#>        73        74        75        76        77        78        79        80 
#> 0.6059337 0.6417509 0.6059337 0.5033271 0.4652034 0.5033271 0.5033271 0.5033271 
#>        81        82        83        84        85        86        87        88 
#> 0.4652034 0.6059337 0.5033271 0.6417509 0.4652034 0.6059337 0.6059337 0.6417509 
#>        89        90        91        92        93        94        95        96 
#> 0.6417509 0.6417509 0.4652034 0.6059337 0.5033271 0.6417509 0.5033271 0.4652034 
#>        97        98        99       100       101       102       103       104 
#> 0.6417509 0.6059337 0.5033271 0.6417509 0.5033271 0.6417509 0.6059337 0.6417509 
#>       105       106       107       108       109       110       111       112 
#> 0.5033271 0.6417509 0.6417509 0.5033271 0.6417509 0.6059337 0.6059337 0.5033271 
#>       113       114       115       116       117       118       119       120 
#> 0.5033271 0.6059337 0.6417509 0.5033271 0.6417509 0.6417509 0.6059337 0.5033271 
#>       121       122       123       124       125       126       127       128 
#> 0.5033271 0.6417509 0.6059337 0.5033271 0.6417509 0.5033271 0.6417509 0.6417509 
#>       129       130       131       132       133       134       135       136 
#> 0.5033271 0.6059337 0.4652034 0.6417509 0.6059337 0.6417509 0.6059337 0.5033271 
#>       137       138       139       140       141       142       143       144 
#> 0.4652034 0.6417509 0.4652034 0.5033271 0.5033271 0.6417509 0.5033271 0.6417509 
#>       145       146       147       148       149       150       151       152 
#> 0.5033271 0.6059337 0.6417509 0.5033271 0.5033271 0.5033271 0.6059337 0.6417509 
#>       153       154       155       156       157       158       159       160 
#> 0.5033271 0.4652034 0.6417509 0.6417509 0.6059337 0.6059337 0.4652034 0.6059337 
#>       161       162       163       164       165       166       167       168 
#> 0.5033271 0.6059337 0.4652034 0.5033271 0.6059337 0.6417509 0.4652034 0.5033271 
#>       169       170       171       172       173       174       175       176 
#> 0.6417509 0.6417509 0.6059337 0.5033271 0.5033271 0.6417509 0.6059337 0.6059337 
#>       177       178       179       180       181       182       183       184 
#> 0.6059337 0.5033271 0.4652034 0.4652034 0.6059337 0.5033271 0.5033271 0.6417509 
#>       185       186       187       188       189       190       191       192 
#> 0.6059337 0.4652034 0.4652034 0.6417509 0.6417509 0.6059337 0.6417509 0.6059337 
#>       193       194       195       196       197       198       199       200 
#> 0.5033271 0.6059337 0.6417509 0.5033271 0.4652034 0.4652034 0.6417509 0.6059337 
#>       201       202       203       204       205       206       207       208 
#> 0.5033271 0.6059337 0.4652034 0.5033271 0.6417509 0.5033271 0.6417509 0.4652034 
#>       209       210       211       212       213       214       215       216 
#> 0.6059337 0.5033271 0.6417509 0.5033271 0.5033271 0.5033271 0.4652034 0.5033271 
#>       217       218       219       220       221       222       223       224 
#> 0.5033271 0.6059337 0.6417509 0.5033271 0.4652034 0.6059337 0.6059337 0.6059337 
#>       225       226       227       228       229       230       231       232 
#> 0.5033271 0.4652034 0.5033271 0.6417509 0.5033271 0.6417509 0.6417509 0.6417509 
#>       233       234       235       236       237       238       239       240 
#> 0.5033271 0.5033271 0.4652034 0.5033271 0.4652034 0.5033271 0.6417509 0.5033271 
#>       241       242       243       244       245       246       247       248 
#> 0.4652034 0.4652034 0.5033271 0.4652034 0.5033271 0.5033271 0.6059337 0.5033271 
#>       249       250       251       252       253       254       255       256 
#> 0.6059337 0.6417509 0.4652034 0.6417509 0.6059337 0.6417509 0.6417509 0.5033271 
#>       257       258       259       260       261       262       263       264 
#> 0.5033271 0.4652034 0.6417509 0.6417509 0.6059337 0.6059337 0.4652034 0.6417509 
#>       265       266       267       268       269       270       271       272 
#> 0.5033271 0.5033271 0.6417509 0.6417509 0.4652034 0.5033271 0.6417509 0.4652034 
#>       273       274       275       276       277       278       279       280 
#> 0.6417509 0.6417509 0.6417509 0.5033271 0.6059337 0.5033271 0.5033271 0.6059337 
#>       281       282       283       284       285       286       287       288 
#> 0.4652034 0.4652034 0.5033271 0.6417509 0.6059337 0.6417509 0.6059337 0.6059337 
#>       289       290       291       292       293       294       295       296 
#> 0.5033271 0.6417509 0.6059337 0.6417509 0.5033271 0.6417509 0.6417509 0.5033271 
#>       297       298       299       300 
#> 0.5033271 0.6059337 0.6417509 0.6059337
```

## Delta method variance

The delta method SE for STC accounts for uncertainty in:

1.  **Outcome model parameters** (via the GLM’s variance-covariance
    matrix)
2.  **AgD event rate** (binomial sampling variance)

The gradient of
$`\text{logit}(\text{mean}(\sigma(\mathbf{X}\boldsymbol{\beta})))`$ with
respect to $`\boldsymbol{\beta}`$ is computed analytically:

``` math
\frac{\partial}{\partial \boldsymbol{\beta}} \text{logit}\left(\frac{1}{N}\sum_j \sigma(\mathbf{x}_j^T\boldsymbol{\beta})\right) = \frac{1}{\bar{p}(1-\bar{p})} \cdot \frac{1}{N} \sum_j \sigma'(\eta_j) \mathbf{x}_j
```

where $`\sigma(\eta) = 1/(1 + e^{-\eta})`$ and
$`\sigma'(\eta) = \sigma(\eta)(1 - \sigma(\eta))`$.

## Continuous and count outcomes

The same [`naive()`](https://choxos.github.io/mlumr/reference/naive.md)
and [`stc()`](https://choxos.github.io/mlumr/reference/stc.md) functions
support normal and Poisson outcomes. For normal outcomes,
[`naive()`](https://choxos.github.io/mlumr/reference/naive.md) compares
the IPD mean against the inverse- variance weighted AgD mean, while
[`stc()`](https://choxos.github.io/mlumr/reference/stc.md) fits a
Gaussian GLM and G-computes the index-treatment mean in the comparator
population. For Poisson outcomes, both methods report log rate ratios;
zero observed event counts use a 0.5 continuity correction on the
log-rate scale so estimates remain finite.

``` r

# Normal-family benchmark
ipd_normal <- set_ipd(
  data.frame(
    trt = "Drug_A",
    score = rnorm(120, mean = 3.0, sd = 1.0),
    age_cat = rbinom(120, 1, 0.40)
  ),
  treatment = "trt",
  outcome = "score",
  covariates = "age_cat",
  family = "normal"
)
agd_normal <- set_agd(
  data.frame(trt = "Drug_B", y_mean = 2.7, se = 0.12, age_cat_mean = 0.35),
  treatment = "trt",
  family = "normal",
  outcome_mean = "y_mean",
  outcome_se = "se",
  cov_means = "age_cat_mean",
  cov_types = "binary"
)
dat_normal <- combine_data(ipd_normal, agd_normal)
naive(dat_normal)
#> Naive Unadjusted Indirect Comparison
#> =====================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Mean outcomes:
#>   Index (IPD):      3.1649
#>   Comparator (AgD): 2.7000
#> 
#> Mean Difference: 0.4649 (SE: 0.1532)
#> 95% CI: [0.1647, 0.7652]
stc(dat_normal)
#> Simulated Treatment Comparison (G-computation)
#> ===============================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Marginalized E[Y|index trt, comp pop]: 3.1777
#> Observed E[Y|comp trt, comp pop]:      2.7000
#> 
#> Mean Difference: 0.4777 (SE: 0.1536)
#> 95% CI: [0.1768, 0.7787]
#> 
#> Outcome model coefficients:
#> (Intercept)     age_cat 
#>      3.2545     -0.2194
```

``` r

# Poisson-family benchmark
exposure <- runif(120, 0.5, 2.0)
ipd_poisson <- set_ipd(
  data.frame(
    trt = "Drug_A",
    events = rpois(120, exp(0.2) * exposure),
    person_years = exposure,
    age_cat = rbinom(120, 1, 0.40)
  ),
  treatment = "trt",
  outcome = "events",
  covariates = "age_cat",
  family = "poisson",
  exposure = "person_years"
)
agd_poisson <- set_agd(
  data.frame(trt = "Drug_B", n_events = 40, person_years = 180,
             age_cat_mean = 0.35),
  treatment = "trt",
  family = "poisson",
  outcome_r = "n_events",
  outcome_E = "person_years",
  cov_means = "age_cat_mean",
  cov_types = "binary"
)
dat_poisson <- combine_data(ipd_poisson, agd_poisson)
naive(dat_poisson)
#> Naive Unadjusted Indirect Comparison
#> =====================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Rates:
#>   Index (IPD):      1.2726
#>   Comparator (AgD): 0.2222
#> 
#> Log Rate Ratio: 1.7451 (SE: 0.1747)
#> 95% CI: [1.4027, 2.0875]
stc(dat_poisson)
#> Simulated Treatment Comparison (G-computation)
#> ===============================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Marginalized rate (index trt, comp pop): 1.2667
#> Observed rate (comp trt, comp pop):      0.2222
#> 
#> Log Rate Ratio: 1.7405 (SE: 0.1751)
#> 95% CI: [1.3972, 2.0837]
#> 
#> Outcome model coefficients:
#> (Intercept)     age_cat 
#>      0.2142      0.0633
```

## Comparing all methods

``` r

# Fit all three methods
naive_result <- naive(dat)
stc_result <- stc(dat)
mlumr_result <- mlumr(
  dat, model = "spfa",
  chains = 2, iter = 500, warmup = 250,
  seed = 42, refresh = 0, verbose = FALSE
)

# Extract LORs for comparison
le_naive <- naive_result$link_effect
le_stc <- stc_result$link_effect
lor_mlumr <- marginal_effects(mlumr_result, effect = "lor",
                               population = "comparator")

cat("Method comparison (LOR in comparator population):\n")
#> Method comparison (LOR in comparator population):
cat(sprintf("  Naive:  %.3f [%.3f, %.3f]\n",
            naive_result$link_effect, naive_result$ci_lower, naive_result$ci_upper))
#>   Naive:  0.647 [0.343, 0.950]
cat(sprintf("  STC:    %.3f [%.3f, %.3f]\n",
            stc_result$link_effect, stc_result$ci_lower, stc_result$ci_upper))
#>   STC:    0.629 [0.325, 0.932]
cat(sprintf("  ML-UMR: %.3f [%.3f, %.3f]\n",
            lor_mlumr$mean, lor_mlumr$q2.5, lor_mlumr$q97.5))
#>   ML-UMR: 0.630 [0.320, 0.928]
```

## Confidence level

Both [`naive()`](https://choxos.github.io/mlumr/reference/naive.md) and
[`stc()`](https://choxos.github.io/mlumr/reference/stc.md) accept a
`conf_level` parameter:

``` r

# 90% confidence intervals
naive_90 <- naive(dat, conf_level = 0.90)
stc_90 <- stc(dat, conf_level = 0.90)
print(naive_90)
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
#> 90% CI: [0.3921, 0.9012]
print(stc_90)
#> Simulated Treatment Comparison (G-computation)
#> ===============================================
#> 
#> Treatments: Drug_A vs Drug_B 
#> 
#> Marginalized P(Y=1|index trt, comp pop): 0.5555
#> Observed P(Y=1|comp trt, comp pop):      0.4000
#> 
#> Log Odds Ratio: 0.6285 (SE: 0.1549)
#> 90% CI: [0.3738, 0.8833]
#> 
#> Outcome model coefficients:
#> (Intercept)     age_cat         sex 
#>      0.0133     -0.1527      0.5697
```

## Key differences from ML-UMR

| Feature | STC | Naive | ML-UMR |
|----|----|----|----|
| Covariate adjustment | Outcome model | None | Joint model |
| Population weighting | G-computation | None | QMC integration |
| Uncertainty | Delta method | Delta method | Posterior |
| Effect modification | Not captured | N/A | Relaxed model |
| Speed | Instant | Instant | Minutes |
| No. parameters | p+1 | 0 | 2+p (SPFA) or 2+2p (Relaxed) |

STC is faster but makes stronger modeling assumptions. ML-UMR jointly
models both data sources and naturally propagates all sources of
uncertainty through the posterior.
