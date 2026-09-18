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

**The correlation structure is assumed to transport.** Aggregate data
report marginal summaries only, so with `cor = NULL` the within-row
correlation is estimated from the IPD and applied to every comparator
row, as in ML-NMR (Phillippo et al. 2020). The assumption is untestable
from the data; supply `cor` from an external source to vary it, and use
[`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md)
to confirm the realized moments and correlations.

`cor_adjust` maps the covariate-scale correlation onto the Gaussian
copula: the Spearman map is exact for continuous margins, the Pearson
map holds for Gaussian margins, and pairs involving a binary margin use
prevalence-independent heuristics. A nonbinary discrete margin (a count
or an ordered category) is treated as continuous and its realized
association need not match the target; `add_integration()` warns when it
sees one. `"none"` passes a latent Gaussian-copula matrix through
unchanged.

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
