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
  prior_aux2 = NULL,
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
  (`prior_normal(0, 10)`), on the linear-predictor scale. See
  [`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md).

- prior_beta:

  Prior for regression coefficients. A single prior broadcast to all
  covariates, or a `list` of priors of length `n_cov` sharing one family
  (and, for Student-t, one df). Default from
  [`default_prior_beta()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  (`prior_normal(0, 2.5)`). Set `autoscale = TRUE` on the prior to
  divide the scale by each covariate's empirical SD. For
  `model = "spfa"` this is the prior on the shared `beta`; for
  `model = "relaxed"` on `beta_index`, with `beta_comparator` taking
  `prior_beta_comparator`.

- prior_sigma:

  Prior for residual SD (normal family only). Default from
  [`default_prior_sigma()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  (`prior_normal(0, 2.5)`, half-normal via the Stan `<lower=0>`
  constraint).
  [`prior_exponential()`](https://choxos.github.io/mlumr/reference/prior_exponential.md)
  is also supported for sigma.

- distribution:

  For `family = "survival"` only: the survival distribution.
  Proportional hazards: `"exponential"`, `"weibull"` (default),
  `"gompertz"` (positive shape, so an increasing hazard). Accelerated
  failure time: `"exponential-aft"`, `"weibull-aft"`, `"lognormal"`,
  `"loglogistic"`, `"gamma"`, `"gengamma"` (the positive Lawless `Q`
  subfamily). Flexible baseline hazard: `"mspline"` and `"pexp"`
  (piecewise exponential). `"gengamma"` is the least numerically robust
  option, so inspect its MCMC diagnostics. Must be `NULL` for other
  families.

- prior_aux:

  For `family = "survival"` parametric distributions: prior for the
  shape or scale parameter(s), half-normal, half-t or exponential
  through the `<lower=0>` constraint. Default
  [`default_prior_aux()`](https://choxos.github.io/mlumr/reference/default_priors.md).
  The Gompertz shape has units of 1 / time, so set this explicitly for a
  Gompertz baseline and check it against the time unit.

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
  `aux_by`. `".study"` (the default, and what `NULL` means) gives each
  study its own baseline shape, with its own knots over its own observed
  support for a flexible baseline. `"none"` gives both studies one
  shared shape, a stronger assumption that buys precision; fit it as a
  sensitivity analysis when the two Kaplan-Meier curves plainly share a
  shape. Each study contributes one arm, so a study-specific and a
  treatment-specific baseline shape cannot be told apart, and a
  stratified fit carries each study's shape with its treatment when
  predictions are transported; where that is doubtful, prefer the RMST
  estimands and report both settings. With the stratified default the
  marginal hazard ratio varies with time, so
  [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
  reports it at one `at_time`.

- pred_times:

  For `family = "survival"`: times at which survival, hazard and
  cumulative-hazard predictions are produced. If `NULL`, a grid up to
  the maximum observed time is used.

- rmst_horizon:

  For `family = "survival"`: the upper time limit for the restricted
  mean survival time. If `NULL`, the maximum observed time, except for a
  flexible baseline stratified by study, where it defaults to the
  follow-up both studies observed so the headline RMST does not
  extrapolate the shorter study. A longer horizon warns.

- n_rmst_grid:

  For `family = "survival"`: number of equally spaced nodes (default
  `100`) on `[0, rmst_horizon]` for the trapezoidal RMST integral.
  Increase for sharp early hazards or long horizons.

- center:

  Logical (default `TRUE`). Center the covariates about the pooled IPD
  and population-weighted declared AgD means before fitting. The
  likelihood is unchanged and sampling is usually easier, but
  `prior_intercept` then applies to the intercept at the pooled
  covariate mean. Set `FALSE` to fit on the raw covariate scale. A fit
  whose centering rounds two integration points onto one is refused.

- qr:

  Logical (default `FALSE`). Apply a thin-QR reparameterization to the
  combined design matrix, which decorrelates its columns for HMC. The
  priors still apply to the original coefficients. Useful with many
  correlated or ill-scaled covariates.

- chains:

  Number of MCMC chains (default 4)

- iter:

  Total iterations per chain (default 2000)

- warmup:

  Number of warmup iterations (default 1000)

- seed:

  Random seed for reproducibility. If `NULL` (default), the fixed seed
  2026 is used and a warning says so. The seed used is reported in the
  fitting messages.

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

  Relaxed model only: prior for the comparator-arm coefficients
  `beta_comparator`, with the same specification rules as `prior_beta`
  and any supported family. `NULL` (the default) reuses `prior_beta`.
  `beta_comparator` is informed only by the aggregate likelihood, so a
  tighter prior here regularizes the index-population estimand; see
  [`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md)
  and
  [`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md).
  Ignored for `model = "spfa"`.

- prior_aux2:

  For `distribution = "gengamma"` only: prior for the second auxiliary
  parameter. `NULL` (the default) reuses `prior_aux`. Supplying it for
  any other distribution warns and has no effect.

- ...:

  Additional arguments passed to the Stan sampling function
  ([`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  or cmdstanr's `$sample()` method)

## Value

An object of class `mlumr_fit`

## Details

The model assumes that all AgD rows come from the same comparator
treatment and that, conditional on covariates, there is no between-study
heterogeneity. No random effects for study-level heterogeneity are
included.

**AgD scale (family = `"normal"`).** The AgD likelihood is
`y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
`y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`; in both
cases [`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
expects `outcome_mean` and `outcome_se` on the arithmetic scale.

**The comparator population is the size-weighted mixture of its
aggregate rows.** Comparator-population predictions weight each row by
the population it represents: `n_agd` for binomial, `outcome_n` for
normal (required for more than one row) and `E_agd` for poisson. These
are mixing weights, separate from the likelihood's precision weights, so
splitting a comparator population into subgroup rows leaves the estimand
unchanged.

**Identifying the relaxed model.** `beta_comparator` is informed only by
the aggregate likelihood. Jointly defined subgroup rows, one
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) row
per stratum, are what can separate it from the comparator intercept; a
single aggregate summary constrains one combination of them. Use
[`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md)
before fitting and
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
after; the subgroup-identification vignette works through the geometry.

**Normal outcomes.** A normal fit whose IPD covariates reproduce the
outcome exactly has an improper posterior for the residual SD, so
`mlumr()` refuses a constant outcome and a least-squares fit that is
exact to numerical precision (residual sum of squares at most 1e-12 of
the total) before sampling, and warns when the design is saturated.
Everything else is left to the sampler and its diagnostics.

## See also

[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md),
[`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md),
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md),
[`prior_summary()`](https://choxos.github.io/mlumr/reference/prior_summary.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Binary SPFA model
fit_spfa <- mlumr(dat, model = "spfa")

# Relaxed SPFA (allows effect modification)
fit_relaxed <- mlumr(dat, model = "relaxed")
} # }
```
