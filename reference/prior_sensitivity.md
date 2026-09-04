# Prior sensitivity analysis for an ML-UMR fit

Refit an [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md)
model across a grid of `prior_beta` scales (keeping the family, mean,
and df fixed) and summarize how the posterior for the marginal treatment
effects (`delta_index`, `delta_comparator`) moves. This is the workflow
recommended by Vehtari et al.'s prior-choice wiki for judging how much
of the posterior is driven by the data versus the prior.

## Usage

``` r
prior_sensitivity(
  fit,
  prior_beta_scales = c(0.5, 1, 2.5, 5, 10),
  prior_beta_comparator_scales = NULL,
  probs = c(0.025, 0.5, 0.975),
  verbose = TRUE,
  ...
)
```

## Arguments

- fit:

  A fitted `mlumr_fit` object to re-fit under alternative priors.

- prior_beta_scales:

  Numeric vector of scales for `prior_beta`. Default
  `c(0.5, 1, 2.5, 5, 10)`.

- prior_beta_comparator_scales:

  (Relaxed fits only.) Numeric vector of scales for
  `prior_beta_comparator`, same length as `prior_beta_scales` and paired
  with it elementwise. If `NULL` (default), the comparator prior is
  swept in parallel using `prior_beta_scales`. This matters because the
  relaxed index-population estimand is driven by the comparator
  coefficients, so a faithful sensitivity sweep must vary their prior.
  Whichever scale is used is reported in the `scale_comparator` column,
  so a refit that paired, say, an index scale of 0.5 with a comparator
  scale of 2.5 is not labeled as though only the index prior had been
  set. Ignored, with a warning, for SPFA fits, which have no
  comparator-specific coefficients.

- probs:

  Quantiles for summarizing each posterior (default
  `c(0.025, 0.5, 0.975)`).

- verbose:

  Logical; if `FALSE`, suppresses progress messages and final printed
  summary table.

- ...:

  Additional arguments forwarded to
  [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) on each
  refit (e.g. `chains`, `iter`, `refresh`). Sampling defaults otherwise
  inherit from the original fit.

## Value

A data frame (tibble-style) with one row per (scale, population,
quantile) combination and columns `scale`, `parameter`, `mean`, `sd`,
and the requested quantiles. Side effect: prints a summary table at the
end.

## Details

The design-matrix controls (`center`, `qr`) are taken from the original
fit and replayed, so a refit reproduces the original parameterization
instead of reverting to the defaults. A fit made with `center = FALSE`
or `qr = TRUE` is a different parameterization, and replaying the
defaults would vary the model as well as the prior.

Only the scale of the `prior_beta` family is varied; its distribution
(normal / student_t) and mean are preserved so comparisons are apples to
apples. `prior_intercept` and `prior_sigma` are carried through
unchanged from the original fit. Each value in `prior_beta_scales` is
used as the **absolute** scale for every coefficient at that refit — if
the original fit used per-coefficient priors, all coefficients are set
to the same scale (the sweep is deliberately homogeneous so the grid
reflects a single level of prior informativeness per refit, not a
rescaling of existing relative differences). If the original
`prior_beta` used an exponential family, it is swapped for a
`prior_normal(0, scale)` at each grid point since exponential has no
scale parameter to vary.

For a relaxed fit the comparator prior is swept alongside the index
prior. By default the two move together, which is a single-factor sweep
of overall prior informativeness; supplying
`prior_beta_comparator_scales` pairs a different comparator scale with
each index scale, so the two are then separate factors moved in lockstep
rather than one. Sweeping the comparator prior is what makes the result
meaningful: the relaxed index-population estimand is driven by the
comparator coefficients, so holding their prior fixed would report a
flat, reassuring curve for exactly the quantity most exposed to the
prior. Both scales are recorded per row (`scale` and
`scale_comparator`), so a row is never labeled by only half of the prior
it was fitted under.

## See also

[`prior_summary()`](https://choxos.github.io/mlumr/reference/prior_summary.md)
for a one-shot description of the priors on a fit;
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
for the posterior summary quantities this sweep tracks.

## Examples

``` r
if (FALSE) { # \dontrun{
sens <- prior_sensitivity(fit_spfa, prior_beta_scales = c(1, 2.5, 5))
} # }
```
