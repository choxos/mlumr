# Set up individual patient data (IPD)

Prepare IPD from the index treatment for an unanchored indirect
comparison.

## Usage

``` r
set_ipd(
  data,
  treatment,
  outcome,
  covariates,
  family = c("binomial", "normal", "poisson"),
  exposure = NULL,
  study = NULL
)
```

## Arguments

- data:

  Data frame containing IPD

- treatment:

  Column name for treatment variable

- outcome:

  Column name for outcome variable. For `family = "binomial"`, must be
  binary (0/1). For `family = "normal"`, any numeric. For
  `family = "poisson"`, non-negative integer counts.

- covariates:

  Character vector of covariate column names

- family:

  Outcome family: `"binomial"`, `"normal"`, or `"poisson"`

- exposure:

  Column name for exposure/time-at-risk (required when
  `family = "poisson"`)

- study:

  Column name for study identifier (optional)

## Value

An object of class `mlumr_ipd`

## Examples

``` r
if (FALSE) { # \dontrun{
# Binary outcome
ipd <- set_ipd(
  data = trial_a,
  treatment = "trt",
  outcome = "response",
  covariates = c("age", "sex")
)

# Continuous outcome
ipd <- set_ipd(
  data = trial_a,
  treatment = "trt",
  outcome = "score",
  covariates = c("age", "sex"),
  family = "normal"
)

# Count outcome with exposure
ipd <- set_ipd(
  data = trial_a,
  treatment = "trt",
  outcome = "events",
  covariates = c("age", "sex"),
  family = "poisson",
  exposure = "person_years"
)
} # }
```
