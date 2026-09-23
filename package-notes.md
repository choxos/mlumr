# Package companion: ML-UMR in the developing mlumr release

This lesson follows the development version of mlumr, **0.1.0.9000**, which is heading toward 0.2.0. It was checked against commit [`965dfc5`](https://github.com/choxos/mlumr/tree/965dfc5c2605c07afee47270c2a5da9a69df368f) of `main`. It describes that implementation, not a published 0.2.0. The interactive charts are teaching calculations. The code cells run real R in the browser, and the mlumr cell runs mlumr's own R code and its compiled binomial Stan models. The separate [workflow.R](workflow.R) runs the installed R package and, with `--fit`, real Stan inference. Every dataset and number in it is simulated.

## Run the companion script

From the lesson branch root, with mlumr and its dependencies installed:

```sh
Rscript workflow.R --help
Rscript workflow.R
Rscript workflow.R --fit
```

To load a source checkout instead of the installed package, pass its path. The script loads it with `pkgload::load_all(compile = NA)`, which compiles the package DLL when any source under `src/` is newer than it (the Stan models take a while), and stops if the compiled code is still older than the sources. The script uses rstan by default; `--engine=cmdstanr` uses an installed CmdStan. The script installs nothing.

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
| Scaled priors | `prior_normal(0, 1, autoscale = TRUE)` | For `prior_beta`, the scale is divided by each covariate's SD. For a normal outcome with an identity link it is also multiplied by the IPD outcome SD, since the slopes are then in outcome units; the normal family's default priors are multiples of that SD wherever the parameter is in outcome units. |
| Sampler checks | `summary(fit)` | `mlumr()` checks chains, divergences, treedepth, R-hat and ESS when it finishes and warns. `summary()` prints those checks again. |
| Prior checks | `prior_summary(fit)`, `plot_prior_posterior(fit, pars = "beta_comparator[1]")`, `prior_sensitivity(fit)` | `plot_prior_posterior()` shows only the intercepts unless `pars` names more. `prior_sensitivity()` refits at scales `c(0.5, 1, 2.5, 5, 10)`, so it is slow. |
| Population outcomes | `predict(fit, type = "response")` | Standardized probability, mean or rate, depending on family. |
| Marginal effects | `marginal_effects(fit, population = "comparator")` | Both treatments in the same population. Binary selectors: `"lor"` (log odds ratio), `"rd"`, `"rr"`; `"or"` is not a selector. Normal: `"md"`. Poisson: `"rr"`. Survival: one literal ratio, `"hr"` for proportional hazards, `"tr"` for an AFT fit with shared slopes and one shared shape, or `"exp_delta_eta"` for other AFT fits, which is not generally a time ratio; plus `"rmstd"` and `"rmstr"`. A hazard ratio carries its evaluation time in the `at_time` column. `summary = FALSE` returns draws. |
| Conditional effects | `conditional_effects(fit, newdata = profiles)`, `conditional_predict(fit, newdata = profiles)` | One contrast or prediction per covariate profile. Binary contrasts: `"link_effect"`, `"rd"`, `"rr"`; `"lor"` aliases `"link_effect"` only under logit. |
| Your own target | `predict(fit, newdata = target)`, `marginal_effects(fit, newdata = target)` | Every row counts equally. Both treatments are standardized over the rows within each draw, and `population` is ignored. This differs from conditional prediction at each row. |
| Figures | `plot()` on predictions, marginal effects and conditional effects; `mlumr_forest()`; `geom_km()` | Exported plot methods and helpers. |

For trial B's own population, rows are mixed by sample size for binomial and normal outcomes and by exposure for Poisson outcomes. These population weights are not likelihood precision weights. External `newdata` rows get equal weights.

Differences are index minus comparator and ratios are index divided by comparator. For a harmful event, a negative risk difference favors the index treatment. For odds ratio summaries, exponentiate each log odds ratio draw and then summarize; the exponentiated posterior mean of the log odds ratio is a different number.

For non-identity links, `predict(type = "link")` returns g(E[g⁻¹(η)]), the link of each draw's standardized mean, not E[η]. The binary `"lor"` effect is always a logit contrast of the averaged probabilities, even for probit or cloglog models, while the naive and STC `$estimate` use the fitted link. Do not compare differently labeled scales as if they were the same.

## Checks answer different questions

`check_integration()` compares moments, and with several covariates their pairwise associations, between the grid and one twice as large and against the declared targets. These are numerical heuristics, not bounds on the final effect's error. Read its separate `resolution` and `target_moments` verdicts; a stable grid can still miss its declared distribution. Refit with more points and compare the effect you report. Carrying the IPD correlation to trial B is an extra assumption, and binary correlation mappings are heuristic.

`check_identification()` is about the **relaxed model**. With K covariates, trial B needs at least K + 1 scalar summaries to identify its intercept and K slopes. More rows alone do not guarantee useful information. A normal identity-link model has an exact linear mean design; for other families the whole within-row distribution matters. With enough rows in a nonlinear model, `flagged` is **NA**, not FALSE, and with too few it is TRUE. Reconstructed survival data are refused.

Besides the row count, it reports the balance (`cond_inv`, smallest over largest singular value of the centered subgroup means in IPD SDs), the spectral dimension (`eff_dim`) and the spread. Under an identity link it flags `cond_inv < 0.2` or `spread < 0.05`, package heuristics, and `NOT FLAGGED` does not establish identification. It screens the coefficients, not a target: a target can be pinned down by the rows even when a coefficient is not, and a coefficient the rows barely separate can still move a far target, so check the target you report with the posterior and prior sensitivity.

Good R-hat, ESS and no divergences do not establish identification, a correct outcome model, adequate integration or comparable trials. Check outcome fit and prior sensitivity separately, especially for trial B's slopes and for any extrapolation to trial A's population or an external target. Tight priors can hide weak information rather than repair it.

## Benchmarks are different estimands

`naive()` compares the outcome observed in A's trial with the outcome observed in B's trial, with no common population. For GLM outcomes, `stc()` fits only trial A's regression, averages its predictions over trial B's covariates, and contrasts that with B's observed outcome. It does **not** fit a regression for B and does **not** need equal slopes for that comparator-population question. It still needs a correct and transportable model for A, overlap, and no unmeasured differences, and its answer is not an index-population effect. For survival, `naive()` returns a Cox log hazard ratio, and `stc()` returns an RMST difference through flexsurv (`n_boot`, `seed`, `rmst_horizon`), with mspline and pexp falling back to Weibull. The relaxed ML-UMR model also models B's coefficients, which supports conditional effects and other targets when the data and assumptions allow.

## Outcome types and limits

Normal aggregate inputs are arithmetic means with their standard errors on the original scale, even with a log link, and several rows need `outcome_n`. The SD of individual outcomes divided by the square root of n gives that standard error only for a simple mean of independent patients; a reported SE is used as it is, and an adjusted, weighted, clustered or repeated-measure estimate needs the standard error of its own analysis. The mean must describe what the row's model predicts: an adjusted mean standardized to another population is a different estimand from the row's raw mean, and a correct SE does not repair that mismatch. Rows that share patients or an adjustment model are not independent evidence. For Poisson outcomes, trial B's covariate summaries should describe covariates **weighted by exposure**, unless exposure is equal within rows; weighting across rows cannot fix exposure dependence inside a row.

Survival comparators use `set_agd_surv()` with reconstructed pseudo-IPD, for one comparator arm only. The default distribution is Weibull. Baselines include exponential, Weibull, Gompertz (positive shape only), several AFT distributions (gengamma with positive Lawless Q only), M-splines (`make_knots()`, `n_knots`, `knots`) and piecewise exponential. `aux_by = ".study"`, the default, gives each trial its own shape, which is an extra transport assumption in this design; `aux_by = "none"` shares it. mlumr does not screen the reconstructed event times before sampling; many tied times, like any hard posterior, show up in the sampler's checks. A marginal hazard ratio needs an evaluation time, and the route depends on the fit. With one shared baseline shape, the scalar from `marginal_effects()` is its limit at time zero and any `at_time` other than 0 is refused. With study-specific shapes or a flexible baseline, `at_time` is rounded to the nearest fitted prediction time, and the `at_time` column records the time used. `newdata` targets have their own documented time handling, and `predict(type = "loghr")` gives the whole log hazard ratio curve. An RMST needs a horizon (`pred_times`, `rmst_horizon`, `n_rmst_grid`) and its time units. Correct censoring codes do not show that censoring is unrelated to the outcome. `predict()` survival types are survival, hazard, cumhaz, rmst, median and loghr. Conditional HR and TR selectors are refused when different baseline shapes prevent the needed cancellation. Reconstruction uncertainty is not propagated, and delayed-entry comparator distributions must describe patients at entry.

Aggregate rows must partition the patients. Do not stack overlapping age, sex and severity tables from one publication: the likelihood treats every row as independent evidence, and this cannot be detected from the summaries. The package does not supply randomization, estimate a missing common control, or fit random study effects.

The package ships four example datasets: `psoriasis_ipd`/`psoriasis_agd`, `ndmm_ipd`/`ndmm_agd` (survival), `shoulder_ipd`/`shoulder_agd` (continuous) and `caries_ipd`/`caries_agd` (counts). Some function names resemble `multinma`, but the models and fitted objects are not interchangeable.

## Source anchors

Checked against `965dfc5`: [set_agd](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/set_agd.Rd), [add_integration](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/add_integration.Rd), [mlumr](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/mlumr.Rd), [predict](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/predict.mlumr_fit.Rd), [marginal_effects](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/marginal_effects.Rd), [conditional_effects](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/conditional_effects.Rd), [check_identification](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/check_identification.Rd), [stc](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/stc.Rd), [set_agd_surv](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/set_agd_surv.Rd), [prior_sensitivity](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/prior_sensitivity.Rd), [plot_prior_posterior](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/plot_prior_posterior.Rd) and [prior_normal](https://github.com/choxos/mlumr/blob/965dfc5c2605c07afee47270c2a5da9a69df368f/man/prior_normal.Rd). The R source at the same commit is the implementation.

## Sensitivity loop for the prespecified target

`Rscript workflow.R --fit --sensitivity --record=FILE` runs the analyst loop that the lesson's priors and diagnostics chapters describe, and writes its results as JSON. The integration and prior scenarios refit the model and extract the risk difference, A minus B, in the same prespecified 400-row target again; the transport scenario keeps the base fits and evaluates them in three targets. `prior_sensitivity()` is not used for this: it summarizes the built-in populations and, for a relaxed fit, forwards its extra arguments to `mlumr()`, not to `marginal_effects()`, so an external target has to be re-extracted from each refit explicitly, which is what the script does.

- **Integration refit.** Both models refitted with `n_int = 2048` against the base 512, same priors. Every scenario uses its own seed (2026 plus the scenario number; the two models within a scenario share it, and the base fits keep 2026 so they equal the `--fit` fits), so a refit and the base fit of the same model have independent Monte Carlo errors and the MCSE of their difference is the square root of the sum of the two squared MCSEs. Rows of different models are not compared this way.
- **Comparator slope prior.** The relaxed model refitted with `prior_beta_comparator` scales 0.25, 0.5, 2.5 and 5 while `prior_beta` stays at `normal(0, 1)`, so only the comparator prior moves.
- **Transport.** The base fits evaluated in the prespecified target, a shifted target (mean 1.0, SD 0.8) and an extrapolating target (mean 2.2, SD 0.5), with the share of each target's rows inside trial A's central 95% covariate range as an overlap measure. With one covariate there is no correlation to carry from trial A to trial B, so no dependence scenario applies.

The MCSE column is the Monte Carlo standard error of the posterior mean, from the `posterior` package on the draws folded into an iterations by chains matrix by the fit's chain labels. Read a difference between two rows against the MCSEs of both, not as an exact number: independent chains differ by Monte Carlo noise even when nothing else changed. `--record` needs both `--fit` and `--sensitivity`, and `dist-manifest.mjs verify` refuses a record without the fits or the table, so the lesson never reads a partial record.

### Execution record, September 23, 2026

`Rscript workflow.R --source=<mlumr checkout at 965dfc5> --fit --sensitivity --record=scenes/native-record.json --engine=cmdstanr` completed with exit code 0, using R 4.6.0, cmdstanr 0.9.0, CmdStan 2.39.0 and mlumr 0.1.0.9000 at `965dfc5`, a clean tree whose compiled code was current. It took about 55 minutes because other jobs kept the machine fully loaded; the fits are the same size as before. Every fitted number in the record equals the September 15 record to its last recorded digit: both fits, the fourteen sensitivity rows in the table below, the benchmarks and the overlap shares. The binomial Stan programs and the Stan data are identical at `4cfd366` and `965dfc5`, and the seeds did not change, so this is the expected result; only the commit, the hash of the compiled code and the run time differ. Every refit again had 0 divergences and a largest R-hat of at most 1.007. The script now passes `prior_beta_comparator` to the relaxed model only: the SPFA refits used to receive it too, and the package ignored it with a warning, so the two warnings at the end of the run are gone and no number changed.

### Execution record, September 15, 2026

`Rscript workflow.R --source=<mlumr checkout at 4cfd366> --fit --sensitivity --record=scenes/native-record.json --engine=cmdstanr` completed with exit code 0 in about four minutes, using R 4.6.0, cmdstanr 0.9.0, CmdStan 2.39.0 and mlumr 0.1.0.9000. The record it wrote is `scenes/native-record.json` on the lesson branch, and the lesson reads the report chart and the sensitivity panel from it. The record names the checkout's commit, whether its tree was clean, and the SHA-256 of its compiled code together with whether that code was at least as new as every source under `src/` (the script loads the checkout with `pkgload::load_all(compile = NA)`, which rebuilds an older DLL, and stops if one still is), because the development version number alone cannot tell one checkout from another; `dist-manifest.mjs verify` refuses a build whose record names a commit other than the pin in `lesson.sh`, a tree with local changes, or compiled code older than its sources, and one whose script SHA-256 differs from the committed `workflow.R`, so the fitted numbers on the page always come from the analysis code in the same commit. The MCSE and ESS fold the draws by the fit's own chain labels, and a fit that lost a chain stops the script instead of being recorded. The base fits reproduce the September 12 record exactly (same seed). Every refit had 0 divergences and a largest R-hat of at most 1.007. The true target risk difference behind the simulated data is -0.12408.

| Scenario | Model | n_int | Comparator prior scale | Seed | Target | Mean | MCSE | Bulk ESS | 95% posterior interval |
| --- | --- | ---: | ---: | ---: | --- | ---: | ---: | ---: | --- |
| base | SPFA | 512 | 1 | 2026 | prespecified | -0.09132 | 0.00070 | 3061 | -0.1666 to -0.0136 |
| base | relaxed | 512 | 1 | 2026 | prespecified | -0.09105 | 0.00062 | 4281 | -0.1728 to -0.0136 |
| integration | SPFA | 2048 | 1 | 2027 | prespecified | -0.09072 | 0.00073 | 2907 | -0.1675 to -0.0148 |
| integration | relaxed | 2048 | 1 | 2027 | prespecified | -0.09125 | 0.00063 | 3874 | -0.1679 to -0.0138 |
| comparator prior | relaxed | 512 | 0.25 | 2028 | prespecified | -0.08974 | 0.00058 | 4639 | -0.1662 to -0.0123 |
| comparator prior | relaxed | 512 | 0.5 | 2029 | prespecified | -0.09165 | 0.00058 | 5025 | -0.1706 to -0.0119 |
| comparator prior | relaxed | 512 | 2.5 | 2030 | prespecified | -0.09259 | 0.00064 | 3806 | -0.1688 to -0.0121 |
| comparator prior | relaxed | 512 | 5 | 2031 | prespecified | -0.09127 | 0.00064 | 3805 | -0.1690 to -0.0122 |
| transport | SPFA | 512 | 1 | 2026 | prespecified | -0.09132 | 0.00070 | 3061 | -0.1666 to -0.0136 |
| transport | relaxed | 512 | 1 | 2026 | prespecified | -0.09105 | 0.00062 | 4281 | -0.1728 to -0.0136 |
| transport | SPFA | 512 | 1 | 2026 | shifted, mean 1.0 | -0.09170 | 0.00072 | 3044 | -0.1690 to -0.0134 |
| transport | relaxed | 512 | 1 | 2026 | shifted, mean 1.0 | -0.09245 | 0.00084 | 4281 | -0.2031 to 0.0123 |
| transport | SPFA | 512 | 1 | 2026 | extrapolating, mean 2.2 | -0.06775 | 0.00060 | 2904 | -0.1348 to -0.0089 |
| transport | relaxed | 512 | 1 | 2026 | extrapolating, mean 2.2 | -0.07098 | 0.00122 | 4139 | -0.2355 to 0.0788 |

Share of target rows inside trial A's central 95% covariate range: prespecified 0.945, shifted 0.803, extrapolating 0.150.

How to read it:

- **Integration.** Going from 512 to 2048 points, with its own seed, moved the SPFA target mean by 0.0006, about 0.6 times the MCSE of the difference, and the relaxed mean by 0.0002, about 0.2 times. Both differences are consistent with Monte Carlo noise and show no grid effect; the comparison cannot see an effect smaller than about one MCSE, so a tighter bound would need more draws or a tolerance set in advance.
- **Comparator prior.** Across comparator prior scales from 0.25 to 5, with the index prior fixed, the relaxed target mean spans -0.0897 to -0.0926, a range of about four MCSEs and 2% of the interval width. Trial B's three rows inform its slope here, so the prior does little. A wide spread in this row would mean the target depends on an assumption.
- **Transport.** In the shifted target, where a fifth of the rows leave trial A's central range, the relaxed interval already reaches past zero. In the extrapolating target, where 85% of the rows lie outside, the relaxed interval runs from -0.24 to 0.08 and the SPFA and relaxed means separate. For that target the defensible conclusion is that the evidence does not support a headline number, whichever interval is narrower.
- **Dependence.** Not applicable: one covariate.

Not run: rstan fits, the survival routes, repeated simulation and other outcome models. The benchmarks on the same data were unchanged: naive log odds ratio -0.8489 (95% confidence interval -1.1522 to -0.5457), STC -0.3477 (-0.6592 to -0.0362).

## Execution record, September 12, 2026

`Rscript workflow.R --source=<mlumr checkout at 4cfd366> --fit --engine=cmdstanr` completed with exit code 0, using R 4.6.0, cmdstanr 0.9.0, CmdStan 2.39.0 and mlumr 0.1.0.9000. The checkout differed from `4cfd366` only in the pkgdown configuration. Both fits, all population and target predictions, all conditional effects and all assertions completed. RStan fitting was not run for this record.

At 512 versus 1024 integration points, the largest relative resolution difference was 0.0020 and the largest declared-target moment difference was 0.0040, giving the verdicts `stable` and `close`. That is numerical evidence about the grid, not a refit of the effect at a larger grid.

| Simulated fit | Kept draws | Divergences | Treedepth hits | Max R-hat | Min ESS | Synthetic target RD, posterior mean (95% interval) |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| SPFA | 4000 | 0 | 0 | 1.003 | 1960 | -0.09132 (-0.16660, -0.01359) |
| Relaxed | 4000 | 0 | 0 | 1.006 | 1745 | -0.09105 (-0.17276, -0.01359) |

The true target risk difference used to simulate the data is -0.12408, and both intervals contain it. The benchmarks on the same data were: naive log odds ratio -0.8489 (95% CI -1.1522 to -0.5457), a comparison of different populations; STC log odds ratio in trial B's population -0.3477 (-0.6592 to -0.0362). These numbers show that the code runs on simulated data. They say nothing about clinical effectiveness. Prior sensitivity, other outcome models, repeated simulation, survival fits and an integration refit were not run.

The executed script has SHA-256 `e01612266acdaf14e3dc1f4e677b0714dd49f62b46efd599ef7afdc309f5e3b2`. It prints its own results, so anyone can recreate this record without a saved fit.
