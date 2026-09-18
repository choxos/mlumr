# Set up aggregate survival data (reconstructed pseudo-IPD)

Prepare comparator aggregate survival data for an unanchored indirect
comparison. The comparator arm is supplied as **reconstructed
pseudo-IPD** (event/censoring times digitized from a published
Kaplan-Meier curve, e.g. via the Guyot algorithm) together with summary
covariate moments (means/SDs). The Stan model integrates the comparator
likelihood over the covariate distribution implied by those moments.

## Usage

``` r
set_agd_surv(
  data,
  treatment,
  Surv = NULL,
  time = NULL,
  status = NULL,
  entry_time = NULL,
  cov_means,
  cov_sds = NULL,
  cov_types = NULL,
  study = NULL,
  arm = NULL
)
```

## Arguments

- data:

  Data frame of reconstructed pseudo-IPD (one row per
  pseudo-individual).

- treatment:

  Column name for the (single) comparator treatment.

- Surv:

  Optional
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object describing the outcome. Use this for left/interval censoring or
  delayed entry.

- time, status, entry_time:

  Character column names as an alternative to `Surv` (right-censoring
  with status `0`/`1`, plus optional delayed entry).

- cov_means:

  Character vector of covariate mean/proportion column names (constant
  within each arm). Suffixes `_mean`/`_prop` are stripped to match the
  IPD covariate names.

- cov_sds:

  Character vector of covariate SD column names (`NA` for binary
  covariates). `NULL` treats all covariates as binary.

- cov_types:

  Character vector of `"continuous"`/`"binary"` per covariate. If
  `NULL`, inferred from the presence of an SD column.

- study:

  Optional study identifier column.

- arm:

  Optional arm identifier column. Only a single comparator arm is
  supported; if supplied, it must have one unique value. Multi-arm
  reconstructed survival comparators are rejected until a weighting
  estimand is implemented. Defaults to a single arm.

## Value

An object of class `mlumr_agd_surv` (also inheriting `mlumr_agd`). The
internal column names cannot be used as column names in `data`.

## Details

Under delayed entry the comparator likelihood conditions each
integration point on survival to its entry time and then averages, so
`cov_means`, `cov_sds` and the distributions
[`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
builds must describe the population observed at entry (those in the risk
set), not a baseline population before selection. With varying entry
times that population can differ by entry time while the model has one
distribution per arm; pooled summaries are right only under a common
entry time or when the covariate distribution among those observed at
entry is the same at every entry time. Neither condition is checkable
from the summaries supplied. Delayed entry in the individual arm is
unaffected.

## Reconstruction uncertainty is not propagated

The pseudo-individual records enter the likelihood as observed data, so
credible intervals are conditional on this one reconstruction and
narrower than the evidence supports. Treat the reconstruction as an
analysis choice: digitize the curve more than once or perturb the points
within their reading error, refit, and report the spread across refits
beside the within-fit interval.

## See also

[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) for
non-survival aggregate data;
[`multinma::set_agd_surv()`](https://dmphillippo.github.io/multinma/reference/set_agd_surv.html)
is the ML-NMR equivalent.

## Examples

``` r
if (FALSE) { # \dontrun{
agd <- set_agd_surv(
  data = comparator_km,
  treatment = "trt",
  time = "time", status = "status",
  cov_means = c("age_mean", "male_prop"),
  cov_sds = c("age_sd", NA),
  cov_types = c("continuous", "binary")
)
} # }
```
