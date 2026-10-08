# Simulation study

The programs and results of the simulation study in

> Chandler, C. & Ishak, J. (2026). "Anchors Away: Navigating Unanchored Indirect Comparisons with Multilevel
> Unanchored Meta-Regression (ML-UMR)." Preprint, arXiv:2606.20341 [stat.ME], Appendix C.

run with the current mlumr. The study compares ML-UMR under the shared prognostic factor assumption (SPFA), ML-UMR
with SPFA relaxed, unanchored STC, MAIC and a naive comparison, on the published n = 150 design: 12 scenarios,
500 replicates each. `results/main` reproduces the published metrics (bias within 1e-4, coverage within 0.2
points, true values within 1e-15).

## Design

* Two single-arm studies of 150 patients: the index study (treatment A) with individual patient data and the
  comparator study (treatment B) with aggregate data.
* Two correlated binary covariates; logit P(Y = 1) = alpha_k + X' beta_k with alpha_A = 1, alpha_B = 0.25,
  beta_A = (-1, -2) and beta_B = (-1, -2 + delta). X1 is a prognostic factor only; X2 is a prognostic factor and,
  when delta is not 0, an effect modifier.
* Scenarios: effect modification none, weak or strong (beta_B2 = -2, -1.75 or -1); imbalance moderate
  (P(X = 1) 0.6 in the index population, 0.4 in the comparator) or high (0.8 and 0.2); covariate correlation 0.5
  in the index population and 0.5 or 0.25 in the comparator. `results/scenarios.csv` lists them by number.
* Estimands: the marginal log odds ratio, log risk ratio, risk difference and event probabilities of A against B
  in each population. True values are computed on populations of 10 million covariate vectors.
* Seeds: replicate r of scenario s uses `20241212 + 10000 s + r` (the relaxed fit adds 50000).
* ML-UMR: normal(0, 10) priors, 3 chains of 1,000 warmup and 1,000 sampling iterations, `adapt_delta` 0.95,
  `max_treedepth` 15, 512 integration points (SPFA) or 128 per subgroup (relaxed), `center = FALSE`.

### An omitted prognostic factor

`--omit=x1` reruns the analyses as if X1 had not been identified as a prognostic factor or had not been collected:
ML-UMR, STC and MAIC adjust for X2 only, and relaxed SPFA uses the two X2 subgroups instead of the four X1 by X2
cells. The datasets, seeds, scenario numbers and true values are those of the main analysis, so the two sets of
results pair replicate by replicate, and the naive comparison is the same in both. Results go to
`results/omit_x1`. (`--omit=x2` also runs; X2 is an effect modifier as well as a prognostic factor.)

## Contents

| Path | What it is |
|---|---|
| `R/00_config.R` | Study settings and the scenario grid. |
| `R/01_data_generation.R` | Data generation and true effects. |
| `R/02_methods.R` | ML-UMR (SPFA, relaxed) with mlumr; the naive, STC and MAIC benchmarks. |
| `R/03_metrics.R` | Performance measures (bias, empirical SE, RMSE, coverage, interval width, Monte Carlo SEs). |
| `R/04_figures.R` | Bias and coverage figures in the layout of the paper's Figs. 1 to 4. |
| `run_simulation.R` | Runs the study; one file per replicate and method, so runs resume and can be split. |
| `summarize_results.R` | Writes the tables and figures below from the saved replicates. |
| `results/scenarios.csv` | The 12 scenarios. |
| `results/main/` | `metrics_detailed_n150.csv`, `true_values.csv` and `figures/` for the published analysis. |
| `results/omit_x1/` | The same for the analysis that omits X1. |

The per-replicate files (`results/*/reps/`) are not kept in the repository; `run_simulation.R` regenerates them.

## Running

R 4.0 or later, mlumr, cmdstanr with CmdStan, and randtoolbox, copula, sandwich, mvtnorm and ggplot2. From this
folder:

```
Rscript run_simulation.R --methods=bench --workers=3                   # naive, STC, MAIC: minutes
Rscript run_simulation.R --methods=spfa,relaxed --workers=6            # 12,000 ML-UMR fits: 11 to 16 hours
Rscript run_simulation.R --scenarios=6 --methods=spfa,relaxed          # one scenario
Rscript run_simulation.R --methods=bench,spfa,relaxed --workers=6 --omit=x1
Rscript summarize_results.R
```

`--scenarios` and `--reps` take R expressions (`--scenarios=c(4,6)`, `--reps=1:100`). A run skips the
replicates already saved. Each fit's CmdStan output files are deleted after it is summarized, so long runs do not
fill the disk. The hours above were measured on 6 workers of an Apple M2.
