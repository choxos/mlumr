# Fit ML-UMR Model

Fit a Bayesian multilevel unanchored meta-regression model using
individual patient data (IPD) and aggregate data (AgD). Supports binary,
continuous, and count outcomes.

## Usage

``` r
mlumr(
  data,
  model = c("spfa", "relaxed"),
  link = NULL,
  prior_intercept = default_prior_intercept(),
  prior_beta = default_prior_beta(),
  prior_sigma = default_prior_sigma(),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = NULL,
  adapt_delta = 0.95,
  max_treedepth = 15,
  refresh = 200,
  engine = NULL,
  verbose = TRUE,
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
  poisson: `"log"` (default, only option). If `NULL`, uses the canonical
  default for the family.

- prior_intercept:

  Prior for treatment intercepts. Default from
  [`default_prior_intercept()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  (`prior_normal(0, 10)`, weakly informative on the linear-predictor
  scale). See
  [`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md)
  for guidance.

- prior_beta:

  Prior for regression coefficients. May be a single prior broadcast to
  all covariates, or a `list` of priors of length `n_cov` for
  per-coefficient specification. All per-coefficient priors must share
  the same family and (for Student-t) df. Default from
  [`default_prior_beta()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  (`prior_normal(0, 2.5)`; Gelman et al. 2008; Stan community
  prior-choice wiki). Set `autoscale = TRUE` on the prior to divide the
  scale by each covariate's empirical SD — useful when predictors are on
  very different scales.

- prior_sigma:

  Prior for residual SD (normal family only). Default from
  [`default_prior_sigma()`](https://choxos.github.io/mlumr/reference/default_priors.md)
  (`prior_normal(0, 2.5)`, half-normal via the Stan `<lower=0>`
  constraint).
  [`prior_exponential()`](https://choxos.github.io/mlumr/reference/prior_exponential.md)
  is also supported for sigma.

- chains:

  Number of MCMC chains (default 4)

- iter:

  Total iterations per chain (default 2000)

- warmup:

  Number of warmup iterations (default 1000)

- seed:

  Random seed for reproducibility

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

**Comparator-population weighting is family-dependent.** Integrated
marginal predictions in the comparator population (`*_comparator`
generated quantities) are weighted by:

- **binomial**: `n_agd[k]` (AgD sample size), so larger AgD rows
  contribute more to the marginal mean.

- **normal**: equal weights across AgD rows (`/ n_agd_rows`), reflecting
  the normal likelihood's treatment of each row as a single summary
  statistic.

- **poisson**: `E_agd[k]` (AgD exposure), matching the rate-based
  likelihood.

Each weighting is natural for the corresponding likelihood; users
comparing marginal effects across families should be aware they are not
identically weighted.

**Weakly-identified coefficients in the relaxed model** —
`beta_comparator` is identified only through AgD, so the relaxed model
needs informative priors (or many AgD rows) to estimate effect
modification reliably.
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
is the recommended diagnostic.

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
