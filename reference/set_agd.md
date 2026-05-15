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

  Outcome family: `"binomial"`, `"normal"`, or `"poisson"`

- outcome_n:

  Column name for sample size (required for binomial, optional for
  normal)

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
