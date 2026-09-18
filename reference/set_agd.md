# Set up aggregate data (AgD)

Prepare AgD from the comparator treatment for an unanchored indirect
comparison.

## Usage

``` r
set_agd(
  data,
  treatment,
  family = c("binomial", "normal", "poisson"),
  outcome_n = NULL,
  outcome_r = NULL,
  outcome_mean = NULL,
  outcome_se = NULL,
  outcome_E = NULL,
  cov_means,
  cov_sds = NULL,
  cov_types = NULL,
  study = NULL
)
```

## Arguments

- data:

  Data frame containing AgD summary statistics

- treatment:

  Column name for treatment variable

- family:

  Outcome family: `"binomial"`, `"normal"`, or `"poisson"`.
  Time-to-event comparator data go to
  [`set_agd_surv()`](https://choxos.github.io/mlumr/reference/set_agd_surv.md)
  instead, which takes reconstructed pseudo-IPD rather than a scalar
  outcome summary.

- outcome_n:

  Column name for sample size. Required for binomial. For normal,
  required when there is more than one aggregate row, because the
  comparator-population estimand is the size-weighted mixture of those
  rows and they cannot be combined without knowing how large each is;
  optional for a single row, where the weighting is irrelevant.

- outcome_r:

  Column name for number of events (required for binomial and poisson)

- outcome_mean:

  Column name for mean outcome (required for normal)

- outcome_se:

  Column name for standard error of outcome (required for normal)

- outcome_E:

  Column name for total exposure (required for poisson)

- cov_means:

  Character vector of column names for covariate means/proportions

- cov_sds:

  Character vector of column names for covariate SDs (`NA` for binary
  covariates)

- cov_types:

  Character vector specifying `"continuous"` or `"binary"` for each
  covariate. If `NULL`, inferred from presence of SD.

- study:

  Column name for study identifier (optional)

## Value

An object of class `mlumr_agd`. As for
[`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md), the
internal column names cannot be used as column names in `data`.

## Details

**Rows must partition the aggregate sample.** Each row contributes its
own likelihood factor, as if the rows were disjoint sets of patients.
One arm, or one set of mutually exclusive subgroup cells, is right;
several overlapping subgroup tables of the same participants count every
patient once per table and overstate the precision. Nothing in the data
reveals the overlap, so it is not checked. See
[`vignette("subgroup-identification", "mlumr")`](https://choxos.github.io/mlumr/articles/subgroup-identification.md)
for how many rows the relaxed model needs.

**Scales.** For `family = "normal"`, `outcome_mean` and `outcome_se` are
on the arithmetic scale under both links; a geometric mean or a
log-scale summary is a different quantity and cannot be converted by the
delta method. For `family = "poisson"`, `outcome_r` is the total count
and `outcome_E` the total person-time, and the covariate distribution
the rate is averaged over has to describe the covariates weighted by
exposure; person-level moments stand in for that only when exposure
carries no information about the rate within the row. For
`family = "binomial"`, `outcome_r` and `outcome_n` are counts of events
and trials.

## Examples

``` r
if (FALSE) { # \dontrun{
# Binary outcome
agd <- set_agd(
  data = trial_b,
  treatment = "trt",
  outcome_n = "n_total",
  outcome_r = "n_events",
  cov_means = c("age_mean", "sex_prop"),
  cov_sds = c("age_sd", NA),
  cov_types = c("continuous", "binary")
)

# Continuous outcome
agd <- set_agd(
  data = trial_b,
  treatment = "trt",
  family = "normal",
  outcome_mean = "mean_score",
  outcome_se = "se_score",
  outcome_n = "n_total",
  cov_means = c("age_mean", "sex_prop")
)

# Count outcome
agd <- set_agd(
  data = trial_b,
  treatment = "trt",
  family = "poisson",
  outcome_r = "n_events",
  outcome_E = "person_years",
  cov_means = c("age_mean", "sex_prop")
)
} # }
```
