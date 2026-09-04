# Predictions from ML-UMR model

Generate population-average absolute-outcome predictions in the index
and comparator populations.

## Usage

``` r
# S3 method for class 'mlumr_fit'
predict(
  object,
  population = c("both", "index", "comparator"),
  type = NULL,
  summary = TRUE,
  probs = c(0.025, 0.5, 0.975),
  times = NULL,
  newdata = NULL,
  ...
)
```

## Arguments

- object:

  An `mlumr_fit` object

- population:

  Which population: `"both"`, `"index"`, or `"comparator"`

- type:

  Prediction type. For binomial/normal/poisson: `"response"` (default)
  or `"link"`. For survival: `"survival"` (default), `"hazard"`,
  `"cumhaz"`, `"rmst"` (restricted mean survival time), `"median"`
  (median survival, obtained by linear interpolation on the fitted
  `pred_times` grid; if the true median precedes the first grid point it
  is interpolated between the known exact point `S(0) = 1` and
  `(pred_times[1], S(pred_times[1]))`, so it is never reported as later
  than the first grid time, though a denser `pred_times` near zero still
  resolves very early medians better), or `"loghr"` (time-varying
  marginal log hazard ratio of index vs comparator at each fitted time,
  per population). For `"response"`: probabilities (binomial), means
  (normal), or rates (poisson). For `"link"`: the fitted link applied to
  the population-standardized response mean, `g(E[g^{-1}(eta)])`. This
  is the marginal link-scale prediction used by G-computation; it is
  generally not the mean conditional linear predictor `E[eta]`.

- summary:

  Return summary statistics (`TRUE`) or full posterior draws (`FALSE`)

- probs:

  Quantiles for summary (default `c(0.025, 0.5, 0.975)`)

- times:

  For survival fits, an optional vector of times at which to report
  curve predictions; each is matched to the nearest fitted `pred_times`
  grid point. If `NULL`, all fitted times are returned.

- newdata:

  Optional data frame of covariate profiles defining an arbitrary
  **target population**. When supplied, per-treatment absolute
  predictions are standardized to this population by g-computation
  (averaging model-based predictions over the rows at each posterior
  draw), and `population` is ignored. Supports
  `type = "response"`/`"link"` (binomial/normal/poisson) and
  `type = "survival"`/`"hazard"`/`"cumhaz"`/`"rmst"`/`"median"`/`"loghr"`
  (survival). Survival hazards use the target-specific survival-weighted
  definition, so `"loghr"` is population-specific and time-varying. Rows
  outside the covariate support used to fit a treatment model are
  accepted as model-based extrapolation; the function does not certify
  overlap or transportability, so users must assess support and run
  sensitivity analyses.

- ...:

  Additional arguments (unused)

## Value

A data frame with predictions. When `type = "link"`, values are the
fitted link applied to each draw's population-standardized response
mean. `type = "rmst"` adds a `horizon` column: RMST is an integral to a
restriction time, so values computed to different horizons are different
estimands and must not be compared. The horizon reported is the one
actually integrated to. That is the `rmst_horizon` given to
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) when one
was supplied; when it was left `NULL` it is the default, which for a
study-stratified flexible baseline is the follow-up both studies
observed rather than the pooled maximum. The plot methods require
`summary = TRUE`; with `summary = FALSE` the raw posterior draws are
returned as a plain data frame.

## Details

**Marginalization on non-identity links.** For `type = "response"` the
reported values are `E[g^{-1}(eta)]`, the posterior expectation of the
inverse-link-transformed linear predictor, *not* `g^{-1}(E[eta])`. The
two differ whenever `g` is non-linear (logit, probit, cloglog, log) by
Jensen's inequality. In the index population the expectation is taken
over IPD individuals; in the comparator population it is taken over the
integration points constructed by
[`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
from the AgD moments. This is the population-average prediction for an
individual randomly drawn from that population, and it matches what the
Stan `generated quantities` block computes. `type = "link"` applies the
fitted link only after that marginalization. It coincides with `E[eta]`
for an identity link but not generally for logit, probit, cloglog, or
log links.

## See also

[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
for treatment-effect summaries;
[`conditional_predict()`](https://choxos.github.io/mlumr/reference/conditional_predict.md)
and
[`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md)
for predictions at specific covariate profiles.

## Examples

``` r
if (FALSE) { # \dontrun{
# Absolute predictions for both populations:
predict(fit, population = "both")
# Survival RMST, and transport to a target covariate distribution:
predict(fit, type = "rmst")
predict(fit, newdata = target_population)
} # }
```
