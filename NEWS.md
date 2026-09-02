# mlumr 0.1.0.9000 (development version)

## Behavior and validation changes to existing functions

* **Marginal summaries are no longer clipped to finite reporting bounds.** The
  Stan models previously passed marginal probabilities through `safe_logit()`
  (clamping to `[1e-10, 1 - 1e-10]`) and ratios through `safe_divide()`
  (flooring the denominator at `1e-10`). Both helpers are gone. Event and
  non-event probabilities, marginal means, and rates are now formed on the log
  scale and the contrasts are built from those logs, so the reported quantity is
  the mathematical one rather than a finite surrogate. Two consequences: the
  likelihood is no longer biased by a clamp at extreme linear predictors, and
  ratios are no longer biased downward. The old `safe_divide()` substituted a
  denominator LARGER than the true one, so a risk ratio or rate ratio with a
  near-zero comparator was systematically understated; the log-scale contrast
  reports it. Working on the log scale also removes most of what used to trigger
  the clip in the first place, because a marginal probability is no longer
  rounded to 0 or 1 before the contrast is taken: `lor_*` is finite in cases
  where `safe_logit()` previously returned its clip boundary. What remains is
  that a ratio whose true value overflows double precision is now `Inf` rather
  than a large finite surrogate. That needs only a finite log contrast above
  `log(.Machine$double.xmax)`, about 709.78, not an infinite linear predictor.
  Read the log-scale generated quantities when it happens.
  See `?mlumr-numerical-evaluation`.

* **`predict(type = "link")` reports the marginal link, not the mean linear
  predictor.** It previously returned `E[eta]`, the average conditional linear
  predictor. It now returns `g(E[g^-1(eta)])`: the fitted link applied to the
  population-standardized response mean. Differencing two `type = "link"`
  predictions therefore gives a contrast on the fitted link scale, which
  reproduces a reported effect where the two scales coincide (a logit binomial
  fit's `lor_*`) and needs a transformation elsewhere, since
  `marginal_effects()` reports on the scale conventional for the family. The two
  definitions of `type = "link"` agree for the identity link and differ for
  logit, probit, cloglog, and log. This is a deliberate divergence from `multinma`, which keeps the two
  apart: its `predict(type = "link")` returns `E[eta]`, and the marginal
  link-scale contrast lives in `marginal_effects(mtype = "link")`. mlumr has no
  conditional population estimand to pair `E[eta]` with, since every effect it
  reports is standardized over a population, so it reports the marginal link
  under the one name rather than offering two link scales that differ silently.

* **Boundary probabilities use a continuity correction instead of a clamp.**
  `bound_probability()` previously clamped every input into
  `[min_count / n, 1 - min_count / n]`. It now leaves interior probabilities
  untouched and replaces only an observed 0 or 1 with the pseudo-count estimate
  `(r + min_count) / (n + 2 * min_count)`. For a zero-event arm with
  `min_count = 0.5` that is `0.5 / (n + 1)` rather than `0.5 / n`, so the link
  contrast, its standard error, and the risk ratio that `naive()` and `stc()`
  report for a binomial arm with no events (or no non-events) change slightly.
  Arms with events on both sides are unaffected.

* **New arguments are inserted before the sampler controls, so positional calls
  are not preserved.** `mlumr()` gains model-defining arguments ahead of
  `chains`, `iter`, and the rest. A call that passed sampler settings by
  position rather than by name therefore binds them to the wrong parameters and
  stops with a validation error naming the argument it actually received. Call
  `mlumr()` with named arguments.

* **`check_integration()` compares correlations on one scale.** The realized
  integration-point correlation was always measured with Pearson while the
  target, when derived from the IPD, defaults to Spearman. The method that
  defined the target is now carried into the diagnostic and reported in the
  output, so the comparison can no longer warn (or reassure) purely from a
  method mismatch.

* **`check_integration()` also reports fidelity to the declared moments.** The
  existing resolution diagnostic compares two grid sizes and answers "is `n_int`
  large enough". It now additionally compares each realized grid moment against
  the mean and standard deviation declared in `set_agd()`, which answers the
  different question "does the grid represent the population it claims to". A
  `distr()` specification can be numerically well resolved and still target the
  wrong marginal.

* **`add_integration()` states where the copula correction does not apply.** The
  Spearman and Pearson maps branch on continuous versus binary margins. A
  nonbinary discrete margin, such as a count or an ordered category, has no
  branch and is mapped with the continuous-margin formula; an exact mapping
  would depend on that margin's distribution and its category thresholds.
  `add_integration()` warns when it detects such a covariate, and the
  documentation states the limitation.

* **`add_integration()` rejects a Pearson correlation with non-Gaussian
  margins.** A covariate-scale Pearson correlation is the Gaussian-copula
  correlation only when the margins are themselves Gaussian, so
  `cor_adjust = "pearson"` combined with a non-`qnorm` continuous margin and a
  nonzero off-diagonal entry now errors instead of silently treating the
  supplied matrix as a latent one. Use `cor_adjust = "spearman"`, Gaussian
  margins, or `cor_adjust = "none"` with a matrix already on the latent scale.

* **`add_integration()` warns when a supplied `distr()` distribution grossly
  contradicts the declared `set_agd()` moments.**

* **Continuous multi-row comparator estimand, and `outcome_n` is now required
  for it.** For the normal family the comparator-population standardized effect
  from several `set_agd()` rows is weighted by `outcome_n` (sample size) rather
  than by inverse variance, so splitting one comparator population into subgroup
  rows no longer changes the estimand. An inverse-variance average estimates a
  common mean efficiently but is not the comparator population's mean, which is
  the size-weighted mixture of its strata. Because there is no defensible way to
  combine several population strata without knowing how large they are,
  `set_agd()` now requires `outcome_n` when normal aggregate data have more than
  one row and errors rather than silently falling back to precision weights.
  `naive()` and `stc()` use the same weighting. Single-row AgD is unchanged and
  still does not require `outcome_n`.

* **`naive()` combines multiple aggregate rows as strata.** For binomial data
  the comparator standard error was computed as though the pooled comparator
  were a single binomial sample of size `sum(n)`. It is now the variance of the
  sample-size-weighted average of the row proportions,
  `sum(w_k^2 * p_k * (1 - p_k) / n_k)` with `w_k = n_k / sum(n)`, propagated to
  the link scale by the delta method. It reduces exactly to the previous formula
  for single-row aggregate data. The reported comparator event rate is now the
  observed proportion; the continuity correction is applied only inside the
  effect calculation.

* **`stc()` no longer reports an index-population contrast.** For the binomial,
  normal, and count families `stc()` previously returned an index-population
  effect alongside the comparator-population one, obtained by assuming the
  treatment difference is constant on the link scale. That constancy is an extra
  assumption which is not part of the STC estimand and is not testable from the
  available data, and it does not hold under effect modification, which is the
  situation population adjustment exists to handle. `stc()` is now what its
  design supports: a comparator-population estimand, labeled as such in the
  returned object. Use `mlumr()`, which standardizes both treatment models and
  reports both populations without that assumption, when the index population is
  the decision target.

## Performance

* The binary, continuous, and count IPD likelihoods now use Stan's fused
  **GLM density functions** (`bernoulli_logit_glm`, `normal_id_glm`,
  `poisson_log_glm`) on their canonical links (logit / identity / log); these
  carry analytic gradients and are faster than the equivalent `_lpdf` forms.
  Non-canonical links (probit, cloglog, log-normal) are unchanged. They are
  statistically equivalent to the 0.1.0 implementation: results match up to
  Monte Carlo error.
* Models **center the covariates** by default: the IPD design matrix and the
  comparator integration grid are shifted to their pooled covariate mean before
  fitting. This removes an intercept-versus-slope collinearity that, on
  real-scale covariates (for example age in years), could push the NUTS sampler
  into very deep (max-treedepth) trajectories and dramatically slow fits. The
  shift is estimand-invariant for the reported effects (the intercept absorbs
  it, so all population-standardized contrasts are unchanged) and is applied
  transparently to `predict()`, `conditional_effects()`, and
  `conditional_predict()`. It is not prior-invariant: `prior_intercept` then
  applies to the intercept at the pooled covariate mean, so intercepts and
  intercept prior-versus-posterior plots are on a different scale from an
  uncentered fit. `center = FALSE` restores the raw-scale parameterization, and
  `qr = TRUE` offers a QR-rotated design as an alternative conditioning fix.

## Package logo

* **New hex-sticker logo** using a broken-anchor motif, for the unanchored
  comparison. Built with `hexSticker` and the Ubuntu font.

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
  scales and returns a posterior-summary table; the workflow recommended
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
  (`log_lik_ipd`, `log_lik_agd`); the standard contract for
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
