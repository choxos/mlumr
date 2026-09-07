# Can the aggregate data identify the comparator coefficients?

In the relaxed model the comparator coefficients `beta_comparator` are
informed only by the aggregate likelihood. This function describes how
the aggregate subgroup mean profiles span the covariate space.

## Usage

``` r
check_identification(x, verbose = TRUE, link = NULL)
```

## Arguments

- x:

  An `mlumr_data` object or a fitted `mlumr_fit`.

- verbose:

  Print a readable report (default `TRUE`).

- link:

  Planned link for an unfitted data object. Defaults to the family
  default. A fitted object always uses its stored link.

## Value

Invisibly, a list with `n_rows` (aggregate rows), `n_distinct` (those
that do not repeat another's integration grid), `n_cov`, `n_rows_needed`
(`n_cov + 1`), `cond_inv`, `eff_dim` (the participation ratio of the
squared singular-value spectrum, which summarizes how evenly the
spectral variation is spread across directions; it is not a count of
identified coefficients), `spread`, `singular_values`, `means` (the
scaled, centered subgroup mean matrix), `diagnostic_scope`,
`target_in_span`, `target_span_gap`, `target_in_declared_span` (the same
exact statement about the declared `<covariate>_mean` rows, which says
whether a gap is the integration grid's or the design's), and `flagged`:
`TRUE` for too few rows, otherwise the identity-link screen result, or
`NA` when nonlinear mean-profile geometry is descriptive only.

## Details

For a normal identity-link model, the subgroup means are the exact
design matrix for the aggregate mean and the reported screen is an
identification diagnostic. For nonlinear mean models, the integrated
response also depends on each row's full covariate distribution. The
mean-profile spectrum is then descriptive only: `flagged` is `TRUE` when
there are fewer than `K + 1` scalar outcome summaries and `NA`
otherwise. Reconstructed survival curves are refused because they
contribute repeated event and censoring likelihood terms rather than one
scalar outcome summary per row.

With `K` covariates there are `K + 1` comparator parameters (the
intercept and `K` coefficients), so at least `K + 1` scalar aggregate
outcome summaries are necessary. For the identity-link mean, the rows
must also differ in every covariate direction.

The function summarizes the spread of the subgroup mean vectors. They
are centered, divided by each covariate's SD in the IPD (so a covariate
measured in large units cannot dominate a proportion by units alone),
and decomposed. `cond_inv`, the ratio of the smallest to the largest
singular value, goes to 0 as the rows collapse onto a lower-dimensional
set. Centering costs one dimension, which is the same `K + 1` fact seen
from the other side.

The identity-link screen uses `cond_inv < 0.2` and `spread < 0.05` as
package heuristics, not validated universal cutoffs. `cond_inv` compares
directions with one another, so it cannot see whether any of them
carries information: with a single covariate there is one singular value
and the ratio is 1 for every nonzero separation, including subgroup
means that differ by 1e-12. `spread` supplies the absolute scale it
lacks, as the RMS distance of the profiles from their center along the
dominant direction, in IPD standard deviations. Neither sees subgroup
sample sizes or outcome precision, and a result above both cutoffs does
not establish identification. Confirm conclusions with
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md).

Identifying every comparator coefficient is sufficient for identifying
the index-population estimand, not necessary. Under an identity link
that estimand is `mu_c + m_ipd' beta_c`, one linear functional of the
comparator parameters, and the aggregate rows pin down every functional
in their row space. A single row whose covariate means equal the IPD
means identifies it exactly while separating neither the intercept nor
the slope. `target_in_span` reports that case, so a coefficient verdict
is not read as one about the estimand. It is an exact statement about
the integration profiles the likelihood actually uses, not about the
declared `<covariate>_mean` columns: a realized grid can sit a fifth of
an SD from its declared mean and still pass the declared-versus-realized
check, and the declared row would then certify a target the fitted row's
span does not contain. Being exact, it says nothing about how well the
estimand is pinned down. `target_span_gap` carries the practical
complement: the distance, in IPD standard deviations, from the target to
the affine span of the directions along which the rows spread by at
least 0.05 SD. Zero means the estimand rests on directions the rows
genuinely move along; a small positive value with `target_in_span` FALSE
means the estimand contains that much of a coefficient the rows do not
pin down, which a finite integration grid can produce on its own; a
large value with `target_in_span` TRUE means the estimand is identified
only through directions the rows barely move along, where precision
depends on the outcome standard errors and row sizes.

This diagnostic concerns `model = "relaxed"` only. Under SPFA both
treatments share one coefficient vector, which the IPD identifies, so a
single aggregate row is not a problem.

## See also

[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) for
`model = "relaxed"`;
[`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md)
for the numerical-integration diagnostic;
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
to confirm how far an estimate moves with the prior.

## Examples

``` r
if (FALSE) { # \dontrun{
dat <- add_integration(combine_data(ipd, agd), n_int = 64, ...)
check_identification(dat)
} # }
```
