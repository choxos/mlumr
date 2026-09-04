# Add numerical integration points

Generate quasi-Monte Carlo integration points using Sobol sequences and
a Gaussian copula to account for correlations between covariates in the
AgD.

## Usage

``` r
add_integration(
  data,
  n_int = 64,
  cor = NULL,
  cor_adjust = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  An `mlumr_data` object from
  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md)

- n_int:

  Number of integration points (default 64; use powers of 2). More
  points can improve quasi-Monte Carlo integration of the AgD likelihood
  and comparator-population estimands. Increase it when
  [`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md)
  shows numerical sensitivity. Wider posterior intervals alone indicate
  neither an inadequate grid nor a need for more points. Larger values
  cost more sampling time.

- cor:

  Correlation matrix for covariates, on the covariate scale. If `NULL`
  (the default) it is estimated from the IPD; see the
  correlation-transport note in Details.

- cor_adjust:

  Adjustment method: `"spearman"`, `"pearson"`, or `"none"`

- verbose:

  Logical; if `FALSE`, suppresses progress messages.

- ...:

  Distribution specifications for each covariate using
  [`distr()`](https://choxos.github.io/mlumr/reference/distr.md)

## Value

An `mlumr_data` object with integration points added

## Details

**The correlation structure is assumed to transport.** Published
aggregate data report marginal covariate summaries (means, SDs,
proportions) but never the joint distribution, so comparator within-row
dependence cannot be estimated from the AgD. With `cor = NULL` this
function estimates one matrix from the **index (IPD)** population and
applies it as a common within-row copula to every comparator subgroup.
It is not generally the pooled comparator correlation because
between-subgroup means also contribute to pooled covariance. That
assumption is untestable from the data at hand and is inherited from
ML-NMR (Phillippo et al. 2020); it is additional to the
shared-prognostic-factor and no-unmeasured-effect-modifier assumptions
of the unanchored comparison itself, and it should be stated in any
submission that uses these results.

The levers are: supply `cor` directly when an external source (a
registry, a similar trial, a publication reporting a correlation matrix)
gives a better estimate for the comparator population; vary it to check
sensitivity; and use
[`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md)
to confirm the realized integration points reproduce the AgD moments and
pairwise correlations you intended. Only the marginal moments are pinned
by [`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md);
the dependence structure is your choice.

`cor_adjust` controls how the covariate-scale correlation is mapped onto
the Gaussian copula. The Spearman map is exact for continuous monotone
margins; the Pearson map is accepted only for Gaussian continuous
margins. The binary-binary and continuous-binary corrections are
prevalence-independent heuristics, while the true latent-Gaussian
correlation for a discrete margin depends on its thresholds. Treat the
realized association as close to, not equal to, the target, and check it
with
[`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md),
passing the same `cor` matrix so the realized-versus-target deviation is
reported. `"none"` passes an explicitly supplied latent Gaussian-copula
matrix through unchanged and can be used with any margins.

Both corrections branch on continuous versus **binary**, and there is no
branch for a nonbinary discrete margin (a count such as Poisson or
negative binomial, or an ordered category). Such a covariate is mapped
as if it were continuous, which understates the attenuation its
discreteness causes; and because many latent Gaussian correlations
induce the same observed ranks, there is no unique value to map to in
the first place. `add_integration()` warns when it detects one. A finite
Sobol grid approximates both marginal moments and dependence. Verify
with
[`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md),
which reports the realized correlation and names the scale
(`cor_method`) it was measured on.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- add_integration(
  dat,
  n_int = 64,
  x1 = distr(qnorm, mean = x1_mean, sd = x1_sd),
  x2 = distr(qbern, prob = x2_mean)
)
} # }
```
