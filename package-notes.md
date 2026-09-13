# Package companion: ML-UMR in the developing mlumr release

This lesson follows the development version of mlumr, **0.1.0.9000**, which is heading toward 0.2.0. It was checked against commit [`4cfd366`](https://github.com/choxos/mlumr/tree/4cfd3660f56e22668ae357bde3df4b30cacb23a5) of `main`. It describes that implementation, not a published 0.2.0. The interactive charts are teaching calculations. The code cells run real R in the browser, and the mlumr cell runs mlumr's own R code and its compiled binomial Stan models. The separate [workflow.R](workflow.R) runs the installed R package and, with `--fit`, real Stan inference. Every dataset and number in it is simulated.

## Run the companion script

From the lesson branch root, with mlumr and its dependencies installed:

```sh
Rscript workflow.R --help
Rscript workflow.R
Rscript workflow.R --fit
```

To load a source checkout instead of the installed package, pass its path. The checkout needs a built package DLL in `src/`, because `pkgload::load_all(compile = FALSE)` does not compile it. The script uses rstan by default; `--engine=cmdstanr` uses an installed CmdStan. The script installs nothing.

```sh
Rscript workflow.R --source=/path/to/mlumr --fit --engine=cmdstanr
```

The script simulates 300 patients for treatment A and three **non-overlapping** subgroup rows for treatment B, with 150, 180 and 170 patients. A hidden patient-level simulation for B is used only to build those published summaries and is then deleted. Both true logit models have slope 0.8, with intercepts -0.8 and -0.25, so shared slopes hold and the relaxed model can still be fitted. The script uses 512 Sobol points per row and checks against a grid twice as large. It has one continuous covariate, so no correlation between covariates has to be carried from trial A to trial B.

`--fit` runs both models with four chains of 1000 warmup and 1000 kept iterations, explicit priors and seed 2026. That is a teaching budget, not a recommendation. The script prints the sampler checks, predictions and effects in both built-in populations, conditional effects at three covariate values, and predictions and effects over a 400-row synthetic target population. Its assertions check the setup, finite benchmark and target results, and that averaging risks differs from the risk at the average covariate value. It writes no files.

## What the API means

| Task | Current API | Meaning |
| --- | --- | --- |
| Trial A's patient data | `set_ipd(..., family = "binomial")` | One index treatment, patient outcomes, measured covariates. |
| Trial B's summaries | `set_agd(..., outcome_n, outcome_r, cov_means, cov_sds)` | Events and sample sizes plus covariate summaries. Column names are strings. A name such as `x_mean` maps to covariate `x`. |
| Join and integrate | `combine_data()`, then `add_integration(..., x = distr(qnorm, mean = x_mean, sd = x_sd))` | The covariate distributions you assume enter the likelihood and the predictions. Means and SDs alone do not fix a distribution. `qbern()`, `qlogitnorm()` and `qgamma()` suit yes or no, proportion and positive covariates. `cor` and `cor_adjust` set how covariates move together; by default the correlation is estimated from the IPD. `n_int` defaults to 64. |
| Shared slopes | `mlumr(..., model = "spfa")` | Separate intercepts, one shared coefficient vector on the link scale. |
| Separate slopes | `mlumr(..., model = "relaxed")` | Separate coefficient vectors; trial B's coefficients depend on its summaries and priors. `prior_beta_comparator` defaults to `prior_beta`. |
| Scaled priors | `prior_normal(0, 1, autoscale = TRUE)` | For `prior_beta`, the scale is divided by each covariate's SD. |
| Sampler checks | `summary(fit)` | `mlumr()` checks chains, divergences, treedepth, R-hat and ESS when it finishes and warns. `summary()` prints those checks again. |
| Prior checks | `prior_summary(fit)`, `plot_prior_posterior(fit, pars = "beta_comparator[1]")`, `prior_sensitivity(fit)` | `plot_prior_posterior()` shows only the intercepts unless `pars` names more. `prior_sensitivity()` refits at scales `c(0.5, 1, 2.5, 5, 10)`, so it is slow. |
| Population outcomes | `predict(fit, type = "response")` | Standardized probability, mean or rate, depending on family. |
| Marginal effects | `marginal_effects(fit, population = "comparator")` | Both treatments in the same population. Binary selectors: `"lor"` (log odds ratio), `"rd"`, `"rr"`; `"or"` is not a selector. Normal: `"md"`. Poisson: `"rr"`. Survival: `"hr"` with `at_time`, `"tr"`, `"rmstd"`, `"rmstr"`. `summary = FALSE` returns draws. |
| Conditional effects | `conditional_effects(fit, newdata = profiles)`, `conditional_predict(fit, newdata = profiles)` | One contrast or prediction per covariate profile. Binary contrasts: `"link_effect"`, `"rd"`, `"rr"`; `"lor"` aliases `"link_effect"` only under logit. |
| Your own target | `predict(fit, newdata = target)`, `marginal_effects(fit, newdata = target)` | Every row counts equally. Both treatments are standardized over the rows within each draw, and `population` is ignored. This differs from conditional prediction at each row. |
| Figures | `plot()` on predictions, marginal effects and conditional effects; `mlumr_forest()`; `geom_km()` | Exported plot methods and helpers. |

For trial B's own population, rows are mixed by sample size for binomial and normal outcomes and by exposure for Poisson outcomes. These population weights are not likelihood precision weights. External `newdata` rows get equal weights.

Differences are index minus comparator and ratios are index divided by comparator. For a harmful event, a negative risk difference favors the index treatment. For odds ratio summaries, exponentiate each log odds ratio draw and then summarize; the exponentiated posterior mean of the log odds ratio is a different number.

For non-identity links, `predict(type = "link")` returns g(E[g⁻¹(η)]), the link of each draw's standardized mean, not E[η]. The binary `"lor"` effect is always a logit contrast of the averaged probabilities, even for probit or cloglog models, while the naive and STC `$estimate` use the fitted link. Do not compare differently labeled scales as if they were the same.

## Checks answer different questions

`check_integration()` compares moments, and with several covariates their pairwise associations, between the grid and one twice as large and against the declared targets. These are numerical heuristics, not bounds on the final effect's error. Read its separate `resolution` and `target_moments` verdicts; a stable grid can still miss its declared distribution. Refit with more points and compare the effect you report. Carrying the IPD correlation to trial B is an extra assumption, and binary correlation mappings are heuristic.

`check_identification()` is about the **relaxed model**. With K covariates, trial B needs at least K + 1 scalar summaries to identify its intercept and K slopes. More rows alone do not guarantee useful information. A normal identity-link model has an exact linear mean design; for other families the whole within-row distribution matters. With enough rows in a nonlinear model, `flagged` is **NA**, not FALSE, and with too few it is TRUE. Reconstructed survival data are refused.

For normal identity-link models, `target_in_span` asks whether trial A's covariate means lie in the span of the realized integration profiles. It can identify one target without identifying every coefficient, and it certifies nothing about precision, support or confounding. `target_in_declared_span` repeats the question for the declared mean columns, and `target_span_gap` and outcome precision still matter. Nothing comparable follows for nonlinear models.

Good R-hat, ESS and no divergences do not establish identification, a correct outcome model, adequate integration or comparable trials. Check outcome fit and prior sensitivity separately, especially for trial B's slopes and for any extrapolation to trial A's population or an external target. Tight priors can hide weak information rather than repair it.

## Benchmarks are different estimands

`naive()` compares the outcome observed in A's trial with the outcome observed in B's trial, with no common population. For GLM outcomes, `stc()` fits only trial A's regression, averages its predictions over trial B's covariates, and contrasts that with B's observed outcome. It does **not** fit a regression for B and does **not** need equal slopes for that comparator-population question. It still needs a correct and transportable model for A, overlap, and no unmeasured differences, and its answer is not an index-population effect. For survival, `naive()` returns a Cox log hazard ratio, and `stc()` returns an RMST difference through flexsurv (`n_boot`, `seed`, `rmst_horizon`), with mspline and pexp falling back to Weibull. The relaxed ML-UMR model also models B's coefficients, which supports conditional effects and other targets when the data and assumptions allow.

## Outcome types and limits

Normal aggregate inputs are arithmetic means with their standard errors on the original scale, even with a log link, and several rows need `outcome_n`. For Poisson outcomes, trial B's covariate summaries should describe covariates **weighted by exposure**, unless exposure is equal within rows; weighting across rows cannot fix exposure dependence inside a row.

Survival comparators use `set_agd_surv()` with reconstructed pseudo-IPD, for one comparator arm only. The default distribution is Weibull. Baselines include exponential, Weibull, Gompertz (positive shape only), several AFT distributions (gengamma with positive Lawless Q only), M-splines (`make_knots()`, `n_knots`, `knots`) and piecewise exponential. `aux_by = ".study"`, the default, gives each trial its own shape, which is an extra transport assumption in this design; `aux_by = "none"` shares it. Many tied reconstructed event times inside the integration grid's reach make mlumr refuse a log-normal fit and warn for the shape families. A marginal hazard ratio needs a time and an RMST needs a horizon (`pred_times`, `rmst_horizon`, `n_rmst_grid`). `predict()` survival types are survival, hazard, cumhaz, rmst, median and loghr. Conditional HR and TR selectors are refused when different baseline shapes prevent the needed cancellation. Reconstruction uncertainty is not propagated, and delayed-entry comparator distributions must describe patients at entry.

Aggregate rows must partition the patients. Do not stack overlapping age, sex and severity tables from one publication: the likelihood treats every row as independent evidence, and this cannot be detected from the summaries. The package does not supply randomization, estimate a missing common control, or fit random study effects.

The package ships four example datasets: `psoriasis_ipd`/`psoriasis_agd`, `ndmm_ipd`/`ndmm_agd` (survival), `shoulder_ipd`/`shoulder_agd` (continuous) and `caries_ipd`/`caries_agd` (counts). Some function names resemble `multinma`, but the models and fitted objects are not interchangeable.

## Source anchors

Checked against `4cfd366`: [set_agd](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/set_agd.Rd), [add_integration](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/add_integration.Rd), [mlumr](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/mlumr.Rd), [predict](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/predict.mlumr_fit.Rd), [marginal_effects](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/marginal_effects.Rd), [conditional_effects](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/conditional_effects.Rd), [check_identification](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/check_identification.Rd), [stc](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/stc.Rd), [set_agd_surv](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/set_agd_surv.Rd), [prior_sensitivity](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/prior_sensitivity.Rd), [plot_prior_posterior](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/plot_prior_posterior.Rd) and [prior_normal](https://github.com/choxos/mlumr/blob/4cfd3660f56e22668ae357bde3df4b30cacb23a5/man/prior_normal.Rd). The R source at the same commit is the implementation.

## Execution record, September 12, 2026

`Rscript workflow.R --source=<mlumr checkout at 4cfd366> --fit --engine=cmdstanr` completed with exit code 0, using R 4.6.0, cmdstanr 0.9.0, CmdStan 2.39.0 and mlumr 0.1.0.9000. The checkout differed from `4cfd366` only in the pkgdown configuration. Both fits, all population and target predictions, all conditional effects and all assertions completed. RStan fitting was not run for this record.

At 512 versus 1024 integration points, the largest relative resolution difference was 0.0020 and the largest declared-target moment difference was 0.0040, giving the verdicts `stable` and `close`. That is numerical evidence about the grid, not a refit of the effect at a larger grid.

| Simulated fit | Kept draws | Divergences | Treedepth hits | Max R-hat | Min ESS | Synthetic target RD, posterior mean (95% interval) |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| SPFA | 4000 | 0 | 0 | 1.003 | 1960 | -0.09132 (-0.16660, -0.01359) |
| Relaxed | 4000 | 0 | 0 | 1.006 | 1745 | -0.09105 (-0.17276, -0.01359) |

The true target risk difference used to simulate the data is -0.12408, and both intervals contain it. The benchmarks on the same data were: naive log odds ratio -0.8489 (95% CI -1.1522 to -0.5457), a comparison of different populations; STC log odds ratio in trial B's population -0.3477 (-0.6592 to -0.0362). These numbers show that the code runs on simulated data. They say nothing about clinical effectiveness. Prior sensitivity, other outcome models, repeated simulation, survival fits and an integration refit were not run.

The executed script has SHA-256 `e01612266acdaf14e3dc1f4e677b0714dd49f62b46efd599ef7afdc309f5e3b2`. It prints its own results, so anyone can recreate this record without a saved fit.
