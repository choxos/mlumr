# Numerical guards in the Stan models

`mlumr`'s Stan models clip a few quantities to avoid undefined
arithmetic in `generated quantities`. This page documents the exact
behavior so users can assess when the guard matters for their analysis.

## Guards currently in place

- `safe_logit(p)` (binomial models):

  Returns `logit(clamp(p, 1e-10, 1 - 1e-10))`. The logit and
  log-odds-ratio posterior summaries use this rather than the raw
  `logit(p)` so that finite samples with `p` at the boundaries do not
  propagate `Inf`/`NaN` into the posterior.

- `safe_divide(num, denom)` (binomial and Poisson models):

  Returns `num / max(denom, 1e-10)`. Used for risk ratios and rate
  ratios to avoid division-by-zero when a comparator posterior draw has
  a near-zero mean outcome.

- Stan `<lower=0>` constraints on `sigma` (normal family):

  Truncate the prior to the positive half-line — the reason why
  `prior_sigma = prior_normal(0, 2.5)` is interpreted as a half-normal
  and why
  [`prior_exponential()`](https://choxos.github.io/mlumr/reference/prior_exponential.md)
  is also valid.

## When the guards introduce bias

The guards are one-sided: they replace invalid values with extreme but
finite ones rather than with a symmetric distribution. In practice:

- For binomial outcomes with event rates in the range `[1e-4, 1 - 1e-4]`
  the clipped output is indistinguishable from the unclipped value at
  double precision.

- For event rates outside that range — or Poisson models with expected
  rates below `1e-4` events per unit exposure — the summary of the
  posterior for LOR, RR, and related ratios may be biased toward the
  clip boundary. The point estimate for the treatment effect on the link
  scale (`delta_index` on log-odds, log-rate, or mean-difference scale)
  is *not* affected since it does not go through the guards.

- **Direction of `safe_divide` bias.** When the comparator rate (the
  denominator) is smaller than `1e-10`, the ratio is clipped to
  `num / 1e-10`. Because rates and probabilities in mlumr are
  non-negative, the clipped value is always *larger* (more positive)
  than the mathematical limit — so risk ratios and rate ratios in
  near-zero-comparator regimes are *upward-biased*, never downward.
  Expect the clipped posterior summary to overstate, not understate, the
  relative treatment effect in those draws.

- For very-rare-event data, consider reframing the model in terms of
  rates: an event count with exposure via
  [`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
  `family = "poisson"` is numerically better behaved than Bernoulli /
  Binomial at extreme probabilities.

## How to diagnose guard activation

Posterior draws of `p_*_index` / `p_*_comparator` or `rate_*_*` are
returned by the Stan models and are available in `fit$draws`. If any
posterior draw sits below `5e-10` or above `1 - 5e-10` for binomial
probabilities (respectively below `5e-10` for Poisson rates), the guards
are active for that draw. A quick check:


      any(fit$draws[, "p_comparator_comparator"] < 5e-10)
