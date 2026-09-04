# Fit ML-UMR Model

Fit a Bayesian multilevel unanchored meta-regression model using
individual patient data (IPD) and aggregate data (AgD). Supports binary,
continuous, count, and time-to-event outcomes.

## Usage

``` r
mlumr(
  data,
  model = c("spfa", "relaxed"),
  link = NULL,
  prior_intercept = default_prior_intercept(),
  prior_beta = default_prior_beta(),
  prior_sigma = default_prior_sigma(),
  distribution = NULL,
  prior_aux = NULL,
  prior_smooth = NULL,
  n_knots = 7L,
  knots = NULL,
  mspline_degree = NULL,
  aux_by = ".study",
  pred_times = NULL,
  rmst_horizon = NULL,
  n_rmst_grid = 100L,
  center = TRUE,
  qr = FALSE,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = NULL,
  adapt_delta = 0.95,
  max_treedepth = 15,
  refresh = 200,
  engine = NULL,
  verbose = TRUE,
  prior_beta_comparator = NULL,
  ...
)
```

## Arguments

- data:

  An `mlumr_data` object with integration points (from
  [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md))

- model:

  Model type: `"spfa"` (shared prognostic factor assumption) or
  `"relaxed"` (treatment-specific coefficients). Default `"spfa"`.

- link:

  Link function. For binomial: `"logit"` (default), `"probit"`, or
  `"cloglog"`. For normal: `"identity"` (default) or `"log"`. For
  poisson and survival: `"log"` (default, only option). If `NULL`, uses
  the canonical default for the family.

- prior_intercept:

  Prior for treatment intercepts. Default from
  [`default_prior_intercept()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  (`prior_normal(0, 10)`). This is a generic starting value on the
  linear-predictor scale, not a calibrated choice for every family or
  outcome scale. See
  [`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md)
  for guidance.

- prior_beta:

  Prior for regression coefficients. May be a single prior broadcast to
  all covariates, or a `list` of priors of length `n_cov` for
  per-coefficient specification. All per-coefficient priors must share
  the same family and (for Student-t) df. Default from
  [`default_prior_beta()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  (`prior_normal(0, 2.5)`). Gelman et al. (2008) motivate a Cauchy prior
  after a particular predictor scaling, not this normal prior as a
  universal default. Set `autoscale = TRUE` on the prior to divide the
  scale by each covariate's empirical SD: useful when predictors are on
  very different scales. For `model = "spfa"` the single coefficient
  vector `beta` uses this prior; for `model = "relaxed"` the index-arm
  coefficients `beta_index` use it while `beta_comparator` uses
  `prior_beta_comparator` (see below).

- prior_sigma:

  Prior for residual SD (normal family only). Default from
  [`default_prior_sigma()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  (`prior_normal(0, 2.5)`, half-normal via the Stan `<lower=0>`
  constraint).
  [`prior_exponential()`](https://choxos.github.io/mlumr/reference/prior_exponential.md)
  is also supported for sigma.

- distribution:

  For `family = "survival"` only: the survival distribution. One of the
  parametric forms `"exponential"`, `"weibull"` (default), `"gompertz"`
  (proportional hazards), `"exponential-aft"`, `"weibull-aft"`,
  `"lognormal"`, `"loglogistic"`, `"gamma"`, `"gengamma"` (accelerated
  failure time), or the flexible-baseline forms `"mspline"` and `"pexp"`
  (piecewise exponential). Must be `NULL` for other families. Note:
  `"gengamma"` is the generalized gamma restricted to positive Lawless
  shape `Q` (`Q = 1 / sqrt(aux2) > 0`), which nests the Weibull, gamma,
  and (as the limit) log-normal; it does not represent negative-`Q`
  shapes. Use a flexible `"mspline"` baseline if the data need a hazard
  shape outside the positive-`Q` family. `"gengamma"` is also the least
  numerically robust option: its likelihood uses Stan's regularized
  incomplete gamma function, whose gradient can fail to converge
  (`grad_reg_lower_inc_gamma: n (internal counter) exceeded 100000 iterations`).
  Isolated messages of that kind are rejected proposals and are
  harmless, but frequent ones, divergent transitions, or a chain that
  fails outright mean the fit should not be trusted. Always inspect the
  MCMC diagnostics reported by
  [`summary()`](https://rdrr.io/r/base/summary.html) on a `gengamma`
  fit, and prefer `"weibull"`, `"gamma"`, `"lognormal"`, or `"mspline"`
  when they fit comparably. Note: `"gompertz"` has a positive shape only
  (the shape carries a `<lower=0>` constraint, so the hazard
  `exp(eta + shape * t)` is monotonically increasing). Decreasing-hazard
  Gompertz (negative shape), available in some survival software, is not
  supported; use `"mspline"` / `"pexp"` for a decreasing or non-monotone
  baseline hazard.

- prior_aux:

  For `family = "survival"` parametric distributions: prior for the
  shape/scale parameter(s) (half-normal/half-t/exponential via the
  `<lower=0>` constraint). Default
  [`default_prior_aux()`](https://choxos.github.io/mlumr/reference/default_priors.md).

- prior_smooth:

  For `family = "survival"` flexible baselines (`"mspline"`/`"pexp"`):
  prior for the random-walk smoothing SD. Default
  [`default_prior_smooth()`](https://choxos.github.io/mlumr/reference/default_priors.md).

- n_knots:

  For `family = "survival"` flexible baselines: number of internal
  spline knots (default 7). See
  [`make_knots()`](https://choxos.github.io/mlumr/reference/make_knots.md).

- knots:

  Optional custom knots for a flexible survival baseline. With a shared
  baseline (`aux_by = "none"`), supply one
  [`make_knots()`](https://choxos.github.io/mlumr/reference/make_knots.md)
  result. With study-specific baselines, supply
  `list(index = ..., comparator = ...)`, where each element has the same
  structure and coefficient count.

- mspline_degree:

  For `family = "survival"` flexible baselines: spline degree override
  (default derived from `distribution`: 3 for `"mspline"`, 0 for
  `"pexp"`).

- aux_by:

  For `family = "survival"`: how the baseline hazard is shared between
  the two studies, the unanchored analogue of
  [`multinma::nma()`](https://dmphillippo.github.io/multinma/reference/nma.html)'s
  `aux_by`. `".study"` (the default) gives each study its **own**
  baseline shape, so the M-spline coefficients (or the parametric shape
  parameters) are estimated separately for the index and comparator
  studies. This matches `multinma`, where `.study` is always part of the
  stratification, and it is the right default: two single-arm trials
  rarely share a hazard shape, and assuming they do imposes proportional
  hazards *across studies*, which no randomization supports.

  `NULL` is accepted and means the same as `".study"`, matching
  multinma, where a `NULL` `aux_by` is resolved to `".study"` and
  `.study` is always part of the stratification.

  `"none"` gives both studies **one** shared shape. multinma has no
  spelling for this because it cannot do it; in an unanchored comparison
  it is a stronger assumption that buys precision, so it is worth
  fitting as a sensitivity analysis when the two Kaplan-Meier curves
  plainly have the same shape, but it should be a deliberate choice
  rather than a default.

  **What stratifying assumes, and what it cannot test.** The parity with
  `multinma` is a parity of spelling, not of meaning. In an anchored
  randomized network each study contributes several arms, so a
  study-specific baseline shape is a nuisance parameter and within-study
  randomization still identifies the treatment effect. Here each study
  contributes exactly **one** arm, so a study-specific baseline shape
  and a treatment-specific baseline shape are perfectly aliased: nothing
  in the data can separate them. Under `".study"` the fitted shape
  therefore travels with the treatment when the effect is transported,
  which is an additional structural assumption the data cannot check,
  not merely the unanchored analogue of stratifying by study. `"none"`
  makes the opposite assumption, that the shape belongs to the disease
  rather than to the arm, and that one is at least testable against the
  two observed curves. Neither is assumption-free; fit both and report
  the difference.

  With the stratified default the marginal hazard ratio varies with
  time, so the scalar `delta_*` reported by
  [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
  is its value at one time, not a constant; pass `at_time` to choose
  which. This applies only where the shapes genuinely differ: the
  exponential has no shape, so `aux_by` leaves its closed-form contrast
  exact. The collapsible RMST difference does not have this problem and
  is the better headline estimand.

  Identification differs by baseline. For `"mspline"` / `"pexp"` each
  stratum gets **its own knots over its own observed support** (as in
  multinma's default `type = "quantile"`), and its coefficients are a
  simplex, which pins that study's cumulative hazard to 1 at a boundary
  the study actually observed. Both parts matter. A single pooled basis
  spanning the longest study would leave the shorter study with basis
  functions it never observes; scaling its observed coefficients by `c`,
  moving the surplus simplex mass into an unobserved column, and
  replacing its intercept by `mu - log(c)` would then leave the
  likelihood exactly unchanged, so the intercept would be set by the
  prior rather than by data. Per-study boundaries remove that flat
  direction. For parametric baselines there is no such normalization and
  none is needed, because shape and scale enter the hazard as different
  functions of time; but the comparator shape is then informed only by
  the reconstructed comparator curve, so stratifying spends information
  that a short or heavily censored aggregate curve may not have. The
  exponential has no shape at all, so `aux_by` does not change it.

  Reach for `"none"` only when the two arms' Kaplan-Meier curves plainly
  have the same shape, and report it as a sensitivity analysis rather
  than as the primary result: one shared shape is the stronger
  assumption and buys precision, but nothing in an unanchored design
  justifies it.

  **An assumption worth naming.** When a stratified fit predicts the
  index treatment in the comparator population, it carries the *index
  study's* baseline shape with it, and vice versa. That is coherent only
  if the residual time pattern is a property of the treatment that
  travels across populations. In an anchored `multinma` network a
  study-stratified baseline is a study nuisance, not something attached
  to a treatment; here each study contributes exactly one arm, so the
  data cannot separate a treatment-specific hazard shape from a study,
  design, or calendar-time shape. Stratifying is the safer default for
  the *contrast*, but absolute predictions transported across
  populations rest on this extra assumption. Where it is doubtful,
  prefer the RMST estimands, compare against `aux_by = "none"`, and say
  which was used.

- pred_times:

  For `family = "survival"`: times at which survival, hazard and
  cumulative-hazard predictions are produced. If `NULL`, a grid up to
  the maximum observed time is used.

- rmst_horizon:

  For `family = "survival"`: the upper time limit for the restricted
  mean survival time. If `NULL`, the maximum observed time, except for a
  flexible baseline (`"mspline"` / `"pexp"`) stratified by study, where
  it defaults to the COMMON follow-up
  `min(max(index times), max(comparator times))`. Each study's flexible
  baseline is extrapolated as a constant hazard past its own last
  observed time, so a pooled-maximum default would make the headline
  RMST extrapolate the shorter study by construction. Pass a longer
  horizon explicitly to accept that extrapolation; doing so still warns.

- n_rmst_grid:

  For `family = "survival"`: number of equally spaced nodes (default
  `100`) on `[0, rmst_horizon]` for the trapezoidal RMST integral.
  Increase for sharp early hazards, long horizons, or high-curvature
  flexible-baseline tails where 100 points may be too coarse; refit at a
  higher value and compare RMST to check convergence.

- center:

  Logical (default `TRUE`). Center the covariates about the pooled IPD
  and population-weighted declared AgD means before fitting. The
  likelihood is unchanged after the intercept is transformed with the
  slopes, and centering often improves sampling geometry. Priors
  specified independently on the numerical intercept and slopes are not
  generally invariant to that transformation, so `center = TRUE` and
  `FALSE` can imply different joint priors even when their likelihoods
  represent the same regression model. Set `FALSE` to fit on the raw
  covariate scale.

- qr:

  Logical (default `FALSE`). Apply a thin-QR reparameterization to the
  combined (intercepts + covariates) design matrix. This decorrelates
  the design columns for more efficient HMC. The Stan model maps the
  requested priors to the original regression coefficients before the QR
  transform, so this option is intended as a computational
  reparameterization. Useful with many correlated or ill-scaled
  covariates; for the common few-covariate case the default fused-GLM
  path (with `center = TRUE`) is usually faster.

- chains:

  Number of MCMC chains (default 4)

- iter:

  Total iterations per chain (default 2000)

- warmup:

  Number of warmup iterations (default 1000)

- seed:

  Random seed for reproducibility. If `NULL` (default), the fixed seed
  2026 is used and a warning says so, so an unseeded fit still
  reproduces. The seed actually used is reported in the fitting
  messages.

- adapt_delta:

  Target acceptance rate (default 0.95)

- max_treedepth:

  Maximum tree depth for NUTS (default 15)

- refresh:

  How often to print progress (0 = silent, default 200)

- engine:

  Stan backend: `"rstan"` (default) or `"cmdstanr"`. If `NULL`, uses the
  engine set by
  [`mlumr_engine()`](https://choxos.github.io/mlumr/reference/mlumr_engine.md).
  See
  [`mlumr_engine()`](https://choxos.github.io/mlumr/reference/mlumr_engine.md)
  for setup.

- verbose:

  Logical; if `FALSE`, suppresses mlumr progress messages. Stan sampler
  progress is still controlled by `refresh`.

- prior_beta_comparator:

  (Relaxed model only.) Prior for the comparator-arm regression
  coefficients `beta_comparator`. Same specification rules as
  `prior_beta` (single prior or per-coefficient list, any supported
  family); a different family from `prior_beta` is allowed (for example
  a heavy-tailed Student-t). If `NULL` (the default) `prior_beta` is
  used (matching the default symmetric behavior). This is a secondary,
  targeted regularization tool: for reliable relaxed-model estimates
  first ensure adequate integration points
  ([`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
  `n_int`) and post-warmup iterations. The comparator-population effect
  is identified directly by the AgD; the index-population effect
  additionally averages `beta_comparator` over the IPD covariate
  distribution (an extrapolation, since `beta_comparator` is informed
  only by the AgD likelihood), so its residual width is
  identification-driven. Tightening this prior (for example a smaller
  `prior_normal(0, 1)`) regularizes that residual width. Ignored for
  `model = "spfa"` (which has a single shared `beta`).

- ...:

  Additional arguments passed to the Stan sampling function
  ([`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  or cmdstanr's `$sample()` method)

## Value

An object of class `mlumr_fit`

## Details

The model assumes that all AgD rows come from the same comparator
treatment and that, conditional on covariates, there is no between-study
heterogeneity. If AgD rows come from multiple studies with different
designs or unmeasured confounders, this assumption may not hold. No
random effects for study-level heterogeneity are included.

**AgD scale assumptions (family = `"normal"`).** The AgD likelihood is
`y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
`y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`. In both
cases [`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
expects `outcome_mean` and `outcome_se` on the **arithmetic (original,
untransformed) scale**, not log-scale or geometric. Passing log-scale
summaries silently misspecifies the likelihood. See
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) for
details.

**The comparator population is the size-weighted mixture of its
aggregate rows.** Integrated marginal predictions in the comparator
population (`*_comparator` generated quantities) weight each row by the
population it represents:

- **binomial**: `n_agd[k]`, the AgD sample size.

- **normal**: `agd_weight[k]`, from `outcome_n`. This is required for
  more than one aggregate row, and is `1` for a single row where the
  weighting is irrelevant.

- **poisson**: `E_agd[k]`, the AgD exposure.

The weights say which population the estimand refers to, and are
deliberately separate from the likelihood's own precision weighting,
which says how much each row constrains the parameters. Because the
parts of a split subgroup sum to the whole, the estimand does not change
with how the aggregate evidence happens to be tabulated.

**Weakly-identified coefficients in the relaxed model**:
`beta_comparator` is identified only through AgD, so the relaxed model
needs informative priors (or many AgD rows) to estimate effect
modification reliably.
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
is the recommended diagnostic.

The model assumes that all AgD rows come from the same comparator
treatment and that, conditional on covariates, there is no between-study
heterogeneity. If AgD rows come from multiple studies with different
designs or unmeasured confounders, this assumption may not hold. No
random effects for study-level heterogeneity are included.

**AgD scale assumptions (family = `"normal"`).** The AgD likelihood is
`y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
`y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`. In both
cases [`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
expects `outcome_mean` and `outcome_se` on the **arithmetic (original,
untransformed) scale**, not log-scale or geometric. Passing log-scale
summaries silently misspecifies the likelihood. See
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) for
details.

**Comparator-population weighting is family-dependent.** Integrated
marginal predictions in the comparator population (`*_comparator`
generated quantities) are weighted by:

- **binomial**: `n_agd[k]` (AgD sample size), so larger AgD rows
  contribute more to the marginal mean.

- **normal**: `outcome_n[k]` (AgD sample size), which is required for
  multiple rows; a single row has weight one when `outcome_n` is
  omitted. These are the estimand's mixing weights, not the likelihood's
  `1 / se^2` precision weights, so splitting one comparator population
  into subgroup rows does not change the target population.

- **poisson**: `E_agd[k]` (AgD exposure), matching the rate-based
  likelihood.

Each weighting is natural for the corresponding likelihood; users
comparing marginal effects across families should be aware they are not
identically weighted.

**Weakly-identified coefficients in the relaxed model.**
`beta_comparator` is identified only through AgD, so the relaxed model
needs informative priors (or many AgD rows) to estimate effect
modification reliably.
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
is the recommended diagnostic.

**Identifying the relaxed model with subgroup AgD.** The strongest way
to identify `beta_comparator` from data (rather than the prior) is to
supply the comparator AgD as **joint subgroups**: mutually exclusive,
collectively exhaustive strata of the comparator population, one
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) row
per subgroup, each with its own covariate summaries and outcome. Each
subgroup contributes a separate marginal likelihood term
(`L_AgD = prod_s L_{AgD,s}`), and the variation in covariate means
across subgroups identifies the treatment-specific covariate effects
`beta_comparator` (the primary relaxed-SPFA strategy of Chandler &
Ishak, Section 2.2.1). With only a single overall comparator AgD row,
`beta_comparator` is identified by the prior alone; with subgroups it is
informed by the data. (Marginal, overlapping subgroups would
double-count patients and understate uncertainty; supply jointly-defined
subgroups.)

## See also

[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
for sensitivity of the posterior to `prior_beta`;
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) for
AgD scale requirements;
[`prior_summary()`](https://choxos.github.io/mlumr/reference/prior_summary.md)
for introspection of the priors actually used.

## Examples

``` r
if (FALSE) { # \dontrun{
# Binary SPFA model
fit_spfa <- mlumr(dat, model = "spfa")

# Relaxed SPFA (allows effect modification)
fit_relaxed <- mlumr(dat, model = "relaxed")
} # }
```
