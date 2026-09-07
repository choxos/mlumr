# Set up individual patient data (IPD)

Prepare IPD from the index treatment for an unanchored indirect
comparison.

## Usage

``` r
set_ipd(
  data,
  treatment,
  outcome = NULL,
  covariates,
  family = c("binomial", "normal", "poisson", "survival"),
  exposure = NULL,
  study = NULL,
  Surv = NULL,
  time = NULL,
  status = NULL,
  entry_time = NULL
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
  `family = "poisson"`, non-negative integer counts. Not used (leave
  `NULL`) for `family = "survival"`, which uses `Surv`/`time`/`status`
  instead.

- covariates:

  Character vector of covariate column names

- family:

  Outcome family: `"binomial"`, `"normal"`, `"poisson"`, or `"survival"`
  (time-to-event)

- exposure:

  Column name for exposure/time-at-risk (required when
  `family = "poisson"`)

- study:

  Column name for study identifier (optional)

- Surv:

  For `family = "survival"`, an optional
  [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html)
  object describing the outcome (use for left/interval censoring or
  delayed entry).

- time, status, entry_time:

  For `family = "survival"`, character column names as an alternative to
  `Surv` (right-censoring with status `0`/`1`, plus optional delayed
  entry).

## Value

An object of class `mlumr_ipd`. Its `$data` holds the treatment, study,
outcome and covariate columns under internal names, and one more,
`.source_key`: a digest of the whole of `data` together with the row's
rank within a canonical ordering of it, which lets
[`compare_models()`](https://choxos.github.io/mlumr/reference/compare_models.md)
tell fits of one source apart from fits of that source reordered,
whatever covariates they use. Nothing of `data`'s content is kept in it.
The internal names, `.source_key` among them, cannot be used as column
names in `data`.

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
