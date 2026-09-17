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
  `prior_beta_comparator`, paired elementwise with `prior_beta_scales`.
  `NULL` (default) sweeps the comparator prior in parallel with
  `prior_beta_scales`; the scale used is reported in the
  `scale_comparator` column. Ignored, with a warning, for SPFA fits.

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

A data frame with one row per (prior scale, summarized parameter) pair,
and columns `scale`, `scale_comparator` (dropped when the model has no
comparator coefficient prior), `parameter`, `effect`, `at_time` (present
only when the summarized effect has an evaluation time, so absent for
every non-survival family and for survival scalars that carry none),
`mean`, `sd`, and one column per requested quantile, named `q` followed
by the percentage (the default `probs` give `q2.5`, `q50`, `q97.5`),
matching
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md).
Quantiles are columns, not a row dimension. Side effect: prints a
summary table at the end when `verbose = TRUE`, which is the default;
`verbose = FALSE` returns the same data frame and prints nothing.

## Details

The design-matrix controls (`center`, `qr`) are taken from the original
fit and replayed, so a refit reproduces the original parameterization
instead of reverting to the defaults. A fit made with `center = FALSE`
or `qr = TRUE` is a different parameterization, and replaying the
defaults would vary the model as well as the prior.

Only the scale of `prior_beta` is varied; its family and mean, and every
other prior and setting, come from the original fit. Each scale is
applied to every coefficient, so the sweep reflects one level of prior
informativeness per refit; an exponential `prior_beta` is swapped for
`prior_normal(0, scale)`. For a relaxed fit the comparator prior is
swept alongside, because the index-population estimand is driven by the
comparator coefficients; holding their prior fixed would report a flat
curve for exactly the quantity most exposed to the prior.

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
