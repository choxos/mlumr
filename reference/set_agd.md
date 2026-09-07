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
[`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md), its
`$data` carries a `.source_key` column, a digest of the whole of `data`
with the row's rank within a canonical ordering of it, holding nothing
of the content; it lets
[`compare_models()`](https://choxos.github.io/mlumr/reference/compare_models.md)
recognize one source reordered between two fits. The internal names,
`.source_key` among them, cannot be used as column names in `data`.

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
(original, untransformed) scale**.

A geometric mean is ALREADY on the original measurement scale, and it is
a different quantity from the arithmetic mean: exponentiating a
log-scale mean returns the geometric mean, not the arithmetic one. For a
lognormal outcome with log-scale mean 0 and log-scale SD 1 the geometric
mean is 1 while the arithmetic mean is `exp(0.5) = 1.65`, so
substituting one for the other is a 39% error in the reported level. No
amount of standard-error propagation repairs that: the delta method
rescales uncertainty about a transformation, it does not convert one
estimand into another.

So do not "back-transform and apply the delta method". Ask for, or
compute from the individual data, the arithmetic mean and its standard
error. If you have only log-scale summaries and are willing to assume
the outcome is lognormal, the arithmetic mean is `exp(m + s^2 / 2)`
where `m` is the log-scale mean and `s` is the log-scale **SD** of the
outcome, not the standard error of `m`; propagate uncertainty in `m` and
`s` jointly through that expression. A change score on a transformed
scale generally cannot be reversed from a published mean alone at all.
Passing log-scale or geometric summaries silently misspecifies the
likelihood and biases the posterior.

**Scale assumptions for `family = "poisson"`.** `outcome_r` is the total
count in each AgD row and `outcome_E` is the total person-time (or other
exposure). The Stan likelihood uses `log(E_agd)` as an offset, so rates
are modeled on the log scale regardless of how `outcome_r` is tabulated.

**What the aggregate Poisson row assumes about exposure.** The
likelihood for a row is the total exposure multiplied by the rate
averaged over the covariate distribution you supply, while the quantity
it stands in for is the sum over people of each person's own exposure
times that person's rate. The two agree exactly when the supplied
distribution is the one **weighted by exposure**, whatever the
dependence between exposure and the covariates: two people with rates 1
and 3 and exposures 9 and 1 contribute `9 * 1 + 1 * 3 = 12` expected
events, and `10 * (0.9 * 1 + 0.1 * 3)` is 12 as well. With the
person-level distribution instead, the same row gives a total exposure
of 10 times the mean rate of 2, which is 20. Person-level moments
reproduce the sum only when exposure carries no information about the
covariate-specific rate within the row.

The assumption is therefore about person-time, not about people, and it
is about the whole distribution the rate is averaged over, not only the
moments: the marginal shape that each
[`distr()`](https://choxos.github.io/mlumr/reference/distr.md) in
[`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)
assumes around `cov_means` and `cov_sds`, and with two or more
covariates the correlation it combines them with, whose default is
estimated from the index sample. All of it has to describe the
covariates weighted by exposure. Exposure can leave every mean and
standard deviation where it was and still change a covariate's skewness
or tails, or how the covariates go together when it varies with their
combination rather than with each on its own, and the averaged rate
moves with any of these. Person-level summaries stand in for
exposure-weighted ones when exposure carries no information about the
covariate-specific rate within the row. Mean exposure that does not vary
with the covariates is a sufficient condition that does not depend on
the model, because it makes the two distributions coincide, shape and
dependence included, and equal individual exposure is its simplest case.
Published subgroup tables almost always report person-level moments, and
those do not identify the person-time distribution. Where follow-up
varies with a prognostic covariate, prefer rows defined so that exposure
is close to constant inside each one, and say which reading the reported
moments support. Weighting across rows does not repair a dependence
inside a row, and nothing in the supplied summaries reveals it, so this
is not checked.

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
