# mlumr 0.1.0.9000 (development version)

## Package logo

* **New hex-sticker logo** using a broken-anchor motif, for the unanchored
  comparison. Built with `hexSticker` and the Ubuntu font via
  `tools/build-logo.R`.

# mlumr 0.1.0

Initial CRAN release.

## Core models

* **ML-UMR models**: Bayesian multilevel unanchored meta-regression with
  two model variants:
    - SPFA (Shared Prognostic Factor Assumption): shared covariate effects
      across treatments
    - Relaxed SPFA: treatment-specific covariate coefficients allowing
      effect modification estimation

* **Three outcome families**: binary (binomial), continuous (normal), and
  count (Poisson) outcomes, each with appropriate link functions:
    - Binomial: logit (default), probit, cloglog
    - Normal: identity (default), log
    - Poisson: log

* **Dual Stan backend**: rstan (default, CRAN-compatible) with optional
  cmdstanr support. Switch engines with `mlumr_engine("cmdstanr")`, which
  guides installation of cmdstanr and CmdStan if needed. Per-call override
  via the `engine` argument in `mlumr()`.

* **Simulated Treatment Comparison (STC)**: Frequentist outcome regression
  via parametric G-computation with delta-method standard errors. Supports
  prediction at covariate means or marginalization over full covariate
  distributions using integration points.

* **Naive unadjusted estimate**: Benchmark comparison of crude outcome
  summaries with delta-method confidence intervals.

## Data preparation

* `set_ipd()`, `set_agd()`, and `combine_data()` provide a
  unified interface for preparing IPD and AgD for all three methods.
* `set_ipd()` rejects covariate names that collide with reserved
  internal columns (`.outcome`, `.study`, `.trt`, `.exposure`) so user
  values cannot be silently overwritten by the standardized frame.
* `set_agd()` applies the same check to its covariate mean/SD columns
  (`.n`, `.r`, `.y`, `.se`, `.study`, `.trt`, `.E`) and additionally
  rejects `cov_means` entries that collapse to duplicate names after
  stripping the `_mean` / `_prop` suffix (e.g. `c("age_mean", "age")`).
* `add_integration()` generates Sobol-sequence quasi-Monte Carlo
  integration points with a Gaussian copula to account for covariate
  correlations, enabling accurate marginalization over the AgD covariate
  distribution.
* `mlumr()`, `add_integration()`, `check_integration()`, and
  `prior_sensitivity()` include `verbose` controls so scripts and
  tests can suppress package-level progress output while retaining warnings.
* The public API mirrors the function names used by the related
  `multinma` package for the data-setup, integration, and
  effect-summary workflow (`set_ipd()`, `set_agd()`,
  `add_integration()`, `unnest_integration()`, `distr()`,
  `marginal_effects()`, `qbern()`/`pbern()`/`dbern()`). Users
  familiar with ML-NMR can transfer their muscle memory directly to
  ML-UMR. When both packages are attached in the same R session R
  issues masking warnings on the shared names; disambiguate with
  `mlumr::function()` / `multinma::function()`.

## Prior system

* Prior constructors `prior_normal(mean, sd)`, `prior_student_t(df, mean, sd)`,
  `prior_cauchy(mean, sd)` (alias for `prior_student_t(df = 1, ...)`), and
  `prior_exponential(rate)`. All six Stan models branch on the prior
  family at runtime, so any of these can be supplied to `prior_intercept`,
  `prior_beta`, or (normal family only) `prior_sigma`.
* `prior_beta` accepts either a single prior (broadcast to all covariates)
  or a list of per-coefficient priors. Per-coefficient priors must share
  the same family and df (Stan branches on a single dist code).
* `prior_normal()`, `prior_student_t()`, and `prior_cauchy()` carry an
  `autoscale` argument. When passed as `prior_beta` with
  `autoscale = TRUE`, each coefficient's prior scale is divided by the
  empirical SD of its covariate (Gelman et al., 2008).
  `autoscale = FALSE` by default.
* Default priors follow the Stan community's prior-choice recommendations
  (Vehtari et al., 2025):
    - `prior_intercept`: `prior_normal(0, 10)`
    - `prior_beta`: `prior_normal(0, 2.5)` (weakly informative; Gelman et al.,
      2008)
    - `prior_sigma`: `prior_normal(0, 2.5)` (half-normal via the `<lower=0>`
      constraint in Stan) for the normal family.
* `default_prior_intercept()`, `default_prior_beta()`, and
  `default_prior_sigma()` accessors expose the package defaults. Values
  are tagged with `$default = TRUE` and the package `$version` so
  `prior_summary()` can report whether each prior is a default and which
  mlumr version produced it.
* `prior_summary()` S3 generic + `prior_summary.mlumr_fit()` method for
  human-readable introspection of every prior used in a fit, including
  post-autoscale per-coefficient scales.
* `prior_sensitivity()` refits a model across a grid of `prior_beta`
  scales and returns a posterior-summary table — the workflow recommended
  by Vehtari et al.'s prior-choice wiki for judging data- vs prior-driven
  inference.

## Inference helpers

* `predict.mlumr_fit()` returns population-specific predicted outcomes.
* `marginal_effects()` returns posterior treatment-effect summaries.
* `conditional_effects()` returns covariate-conditional treatment
  effects.
* `conditional_predict()` returns predictions at specific covariate
  values.
* `predict.mlumr_fit()` and `conditional_effects()` document the
  Jensen's-inequality gap on non-identity links: response-scale summaries
  are `E[g^{-1}(eta)]`, not `g^{-1}(E[eta])`.

## Model comparison

* `calculate_dic()` for DIC-based comparison (no extra dependencies).
* `calculate_loo()` and `calculate_waic()` using the optional `loo`
  package for PSIS-LOO and WAIC (Vehtari, Gelman, Gabry, 2017). `loo`
  is in `Suggests`, not `Imports`.
* `compare_models()` accepts `criterion = c("dic", "loo", "waic")`,
  defaulting to `"dic"`.
* All six Stan models produce pointwise log-likelihood vectors
  (`log_lik_ipd`, `log_lik_agd`) — the standard contract for
  `loo::loo()` / `loo::waic()`.

## Sampling

* Regression coefficients `beta` (and `beta_comparator` in relaxed
  models) are sampled via an affine (non-centered) reparameterization:
  `z_beta ~ std_* (0, 1)`, `beta = prior_beta_mean + prior_beta_sd .* z_beta`.
  This decouples HMC adaptation from the prior scale and typically
  improves mixing when the prior scale is mis-matched with the
  posterior scale.

## Diagnostics

* Automatic MCMC diagnostic checks (divergences, Rhat, ESS, treedepth)
  via `check_diagnostics()`.
* `check_integration()` provides a `check_joint` argument that
  compares pairwise correlation matrices at the current vs doubled `n_int`
  (and against the user-supplied correlation target when available).

## Stan internals

* Stan prior hyperparameter declarations are shared across all six
  models via `#include include/priors_hyperparameters.stan` and
  `include/priors_sigma_hyperparameters.stan`. Prior log-density
  dispatchers live in `include/priors_functions.stan`.
* Binary-link numerical helpers are shared by the binary SPFA and relaxed
  models via `include/binary_functions.stan`.
* `E_ipd` in the two Poisson Stan models carries `<lower=0>` so
  off-API consumers who assemble `stan_data` manually get a Stan
  validation error rather than `log(0) = -Inf` on a non-positive
  exposure.
* Internal reference page `?mlumr-numerical-guards` documents
  `safe_logit`, `safe_divide`, and the `<lower=0>` Stan guards.

## Documentation

* A package startup message reports the installed mlumr version and GitHub
  repository when the package is attached.
* `?mlumr-package` provides a full overview of the typical workflow
  (data preparation -> integration -> fit -> diagnostics -> inference) and
  points at the alternative methods (`stc()`, `naive()`).
* `@seealso` cross-links across `predict.mlumr_fit()`,
  `marginal_effects()`, `conditional_effects()`,
  `conditional_predict()`, `prior_summary()`, and
  `prior_sensitivity()`.
* Six vignettes covering data preparation, ML-UMR models, STC and
  naive benchmarks, method comparison, and a complete worked example.
  Vignettes run compact examples during package checks; intentionally failing
  demonstrations and longer production-style fits remain non-executed.

## Testing

* Test coverage spans data setup, integration, link functions, priors,
  prior summaries, prior sensitivity, engine selection, diagnostics,
  prediction, conditional effects, ML-UMR validation, fitted-model behavior,
  STC, naive benchmarks, utility functions, and LOO/WAIC/DIC model
  comparison.
* The test suite includes reserved-name guards, duplicate covariate-name
  checks, standardized-frame shape checks, pointwise log-likelihood
  extraction, integration diagnostics, posterior-summary validation, and
  family-specific behavior for binary, normal, and Poisson outcomes.
