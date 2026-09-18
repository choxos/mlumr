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

  When supplied, the result has one row per requested time for each
  treatment and population cell, in the order requested and including
  repeats, with a `requested_time` column beside `time`. With
  `summary = FALSE` the mapping is carried as the `requested_time` and
  `used_time` attributes instead, one entry per time column. Refit with
  `pred_times` containing the exact times to avoid the approximation.

- newdata:

  Optional data frame of covariate profiles defining an arbitrary
  **target population**. When supplied, per-treatment absolute
  predictions are standardized to this population by g-computation
  (averaging model-based predictions over the rows at each posterior
  draw), and `population` is ignored. Supports
  `type = "response"`/`"link"` (binomial/normal/poisson) and
  `type = "survival"`/`"hazard"`/`"cumhaz"`/`"rmst"`/`"median"`/`"loghr"`
  (survival). Rows outside the fitted covariate support are model-based
  extrapolation; overlap is not checked.

- ...:

  Additional arguments (unused)

## Value

A data frame with predictions. `type = "rmst"` adds a `horizon` column
with the restriction time actually integrated to, since RMST at
different horizons is a different estimand. The plot methods require
`summary = TRUE`; with `summary = FALSE` the raw posterior draws are
returned as a plain data frame. For `type = "median"` the summary is
conditional on the median being reached, and `p_not_reached` gives the
posterior probability that it is not.

## Details

**Marginalization on non-identity links.** For `type = "response"` the
reported values are `E[g^{-1}(eta)]`, the population-average prediction
for an individual drawn from that population, not `g^{-1}(E[eta])`; the
expectation runs over the IPD individuals for the index population and
over the integration points for the comparator one. `type = "link"`
applies the fitted link after that marginalization.

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
