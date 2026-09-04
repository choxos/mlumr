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

An object of class `mlumr_agd`

## Details

**Rows must partition the aggregate sample, not overlap it.** Every row
contributes its own factor to the aggregate likelihood, which multiplies
them as if they came from disjoint sets of patients. That is correct
when the rows are one arm, or a set of mutually exclusive, jointly
defined subgroup cells (for example the four cells of sex crossed with
prior therapy). It is wrong when a publication reports several
*overlapping* subgroup tables over the same participants, as when age
bands, sex, and disease severity are each tabulated separately.
Supplying those together counts every patient once per table, and the
posterior becomes correspondingly overconfident: the intervals shrink
because the model believes it has seen several independent studies.
Nothing in the data identifies the overlap, so `set_agd()` cannot detect
this and does not try. Choose one partition of the comparator sample and
use only its rows. See
[`vignette("subgroup-identification", "mlumr")`](https://choxos.github.io/mlumr/articles/subgroup-identification.md)
for how many such rows the relaxed model needs.

**Scale assumptions for `family = "normal"`.** The AgD likelihood is
`y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
`y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`. In both
cases `outcome_mean` and `outcome_se` must be on the **arithmetic
(original, untransformed) scale**. If a publication reports only a
log-scale mean / SD or a geometric mean, back-transform before calling
`set_agd()` and propagate uncertainty via the delta method; passing
log-scale or geometric summaries silently misspecifies the likelihood
and biases the posterior.

**Scale assumptions for `family = "poisson"`.** `outcome_r` is the total
count in each AgD row and `outcome_E` is the total person-time (or other
exposure). The Stan likelihood uses `log(E_agd)` as an offset, so rates
are modeled on the log scale regardless of how `outcome_r` is tabulated.

**Scale assumptions for `family = "binomial"`.** `outcome_r` /
`outcome_n` are counts of events and trials. The log-odds (or probit /
cloglog under alternative links) are formed from
`outcome_r / outcome_n`, so no scale conversion is required.

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
