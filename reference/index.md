# Package index

## Data setup

Assemble the index IPD and comparator AgD into one `mlumr_data` object
and build the quasi-Monte Carlo integration points for population
adjustment.

- [`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md) :
  Set up individual patient data (IPD)
- [`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) :
  Set up aggregate data (AgD)
- [`set_agd_surv()`](https://choxos.github.io/mlumr/reference/set_agd_surv.md)
  : Set up aggregate survival data (reconstructed pseudo-IPD)
- [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md)
  : Combine IPD and AgD for unanchored comparison
- [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
  : Add numerical integration points
- [`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md)
  : Check integration point adequacy
- [`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md)
  : Can the aggregate data identify the comparator coefficients?
- [`unnest_integration()`](https://choxos.github.io/mlumr/reference/unnest_integration.md)
  : Expand integration points into a long-format data frame
- [`make_knots()`](https://choxos.github.io/mlumr/reference/make_knots.md)
  : Choose M-spline knots for a flexible-baseline survival model

## Covariate distributions

Marginal-distribution helpers for the comparator integration points.

- [`distr()`](https://choxos.github.io/mlumr/reference/distr.md) :
  Specify a marginal distribution
- [`qbern()`](https://choxos.github.io/mlumr/reference/qbern.md) :
  Bernoulli quantile function
- [`pbern()`](https://choxos.github.io/mlumr/reference/pbern.md) :
  Bernoulli CDF
- [`dbern()`](https://choxos.github.io/mlumr/reference/dbern.md) :
  Bernoulli PMF
- [`qgamma()`](https://choxos.github.io/mlumr/reference/GammaDist.md)
  [`pgamma()`](https://choxos.github.io/mlumr/reference/GammaDist.md)
  [`dgamma()`](https://choxos.github.io/mlumr/reference/GammaDist.md) :
  The Gamma distribution, parameterized by mean and standard deviation
- [`dlogitnorm()`](https://choxos.github.io/mlumr/reference/logitNormal.md)
  [`plogitnorm()`](https://choxos.github.io/mlumr/reference/logitNormal.md)
  [`qlogitnorm()`](https://choxos.github.io/mlumr/reference/logitNormal.md)
  : The logit-Normal distribution

## Model fitting

Fit the Bayesian ML-UMR model (SPFA / relaxed) and select the Stan
backend.

- [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) : Fit
  ML-UMR Model
- [`mlumr_engine()`](https://choxos.github.io/mlumr/reference/mlumr_engine.md)
  : Get or Set the Stan Engine

## Priors

Prior constructors, the prior summary, and prior-sensitivity analysis.

- [`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md)
  : Specify a normal prior

- [`prior_student_t()`](https://choxos.github.io/mlumr/reference/prior_student_t.md)
  : Specify a Student-t prior

- [`prior_cauchy()`](https://choxos.github.io/mlumr/reference/prior_cauchy.md)
  : Specify a Cauchy prior

- [`prior_exponential()`](https://choxos.github.io/mlumr/reference/prior_exponential.md)
  : Specify an exponential prior

- [`prior_summary()`](https://choxos.github.io/mlumr/reference/prior_summary.md)
  : Summary of priors used by a fitted ML-UMR model

- [`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
  : Prior sensitivity analysis for an ML-UMR fit

- [`default_prior_intercept()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  [`default_prior_beta()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  [`default_prior_sigma()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  [`default_prior_aux()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  [`default_prior_smooth()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  :

  Default priors used by
  [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md)

## Treatment effects and predictions

Population-standardized effects and absolute predictions in either
population.

- [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
  : Marginal treatment effects
- [`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
  : Conditional treatment effects
- [`predict(`*`<mlumr_fit>`*`)`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
  : Predictions from ML-UMR model
- [`conditional_predict()`](https://choxos.github.io/mlumr/reference/conditional_predict.md)
  : Conditional predictions

## Frequentist benchmarks

Unadjusted (naive) and simulated-treatment-comparison (STC) reference
estimators.

- [`naive()`](https://choxos.github.io/mlumr/reference/naive.md) : Naive
  unadjusted indirect comparison
- [`stc()`](https://choxos.github.io/mlumr/reference/stc.md) : Simulated
  treatment comparison via G-computation

## Model comparison and diagnostics

Compare SPFA against relaxed, and the information criteria behind it.

- [`compare_models()`](https://choxos.github.io/mlumr/reference/compare_models.md)
  : Compare fitted ML-UMR models
- [`calculate_dic()`](https://choxos.github.io/mlumr/reference/calculate_dic.md)
  : Calculate DIC for model comparison
- [`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md)
  : Calculate LOO-CV for an mlumr_fit
- [`calculate_waic()`](https://choxos.github.io/mlumr/reference/calculate_waic.md)
  : Calculate WAIC for an mlumr_fit

## Plots

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods,
the Kaplan-Meier overlay, the method-comparison forest, and the
prior-versus-posterior overlay.

- [`plot(`*`<mlumr_prediction>`*`)`](https://choxos.github.io/mlumr/reference/plot.mlumr_prediction.md)
  : Plot absolute predictions from a fitted ML-UMR model
- [`plot(`*`<mlumr_marginal_effects>`*`)`](https://choxos.github.io/mlumr/reference/plot.mlumr_marginal_effects.md)
  : Forest plot of population-standardized marginal effects
- [`plot(`*`<mlumr_conditional_effects>`*`)`](https://choxos.github.io/mlumr/reference/plot.mlumr_conditional_effects.md)
  : Plot covariate-conditional treatment effects
- [`geom_km()`](https://choxos.github.io/mlumr/reference/geom_km.md) :
  Observed Kaplan-Meier layer for survival overlays
- [`mlumr_forest()`](https://choxos.github.io/mlumr/reference/mlumr_forest.md)
  : Forest plot of a small set of estimates
- [`plot_prior_posterior()`](https://choxos.github.io/mlumr/reference/plot_prior_posterior.md)
  : Prior-versus-posterior overlay

## Example datasets

Bundled index IPD and comparator AgD for the worked examples, one pair
per outcome family (binary, continuous, count, survival).

- [`psoriasis_ipd`](https://choxos.github.io/mlumr/reference/psoriasis_ipd.md)
  : Plaque psoriasis: index individual patient data (binary)
- [`psoriasis_agd`](https://choxos.github.io/mlumr/reference/psoriasis_agd.md)
  : Plaque psoriasis: comparator aggregate data (binary)
- [`shoulder_ipd`](https://choxos.github.io/mlumr/reference/shoulder_ipd.md)
  : Shoulder pain: simulated index IPD (continuous)
- [`shoulder_agd`](https://choxos.github.io/mlumr/reference/shoulder_agd.md)
  : Shoulder pain: simulated comparator aggregate data (continuous)
- [`caries_ipd`](https://choxos.github.io/mlumr/reference/caries_ipd.md)
  : Dental caries: simulated index IPD (count)
- [`caries_agd`](https://choxos.github.io/mlumr/reference/caries_agd.md)
  : Dental caries: simulated comparator aggregate data (count)
- [`ndmm_ipd`](https://choxos.github.io/mlumr/reference/ndmm_ipd.md) :
  Newly diagnosed multiple myeloma: index individual patient data
  (survival)
- [`ndmm_agd`](https://choxos.github.io/mlumr/reference/ndmm_agd.md) :
  Newly diagnosed multiple myeloma: comparator pseudo-IPD (survival)
- [`ndmm_agd_covs`](https://choxos.github.io/mlumr/reference/ndmm_agd_covs.md)
  : Newly diagnosed multiple myeloma: comparator covariate summaries
  (survival)
