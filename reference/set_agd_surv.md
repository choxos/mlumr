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

An object of class `mlumr_agd_surv` (also inheriting `mlumr_agd`).

## See also

[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) for
non-survival aggregate data.
[`multinma::set_agd_surv()`](https://dmphillippo.github.io/multinma/reference/set_agd_surv.html)
is the ML-NMR equivalent; the name is given as code rather than as a
link because multinma is not a dependency here, and an anchored link
into a package the check environment need not have is reported as an
unresolved cross-reference.

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
