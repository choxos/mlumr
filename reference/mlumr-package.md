# mlumr: Multilevel Unanchored Meta-Regression for Indirect Treatment Comparisons

Bayesian multilevel unanchored meta-regression (ML-UMR) for population-
adjusted indirect comparisons between a single-arm individual patient
data (IPD) study on the index treatment and aggregate data (AgD) on a
comparator. Two model variants are provided: a shared-prognostic-factor
(SPFA) model and a relaxed-SPFA model that allows treatment-specific
covariate coefficients. Supports binary (binomial), continuous (normal),
count (Poisson), and time-to-event (survival) outcomes, each with
appropriate link functions. Frequentist outcome regression (Simulated
Treatment Comparison) and a naive unadjusted benchmark are included for
side-by-side comparison.

## Typical workflow

1.  [`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md):
    declare the individual patient data and its covariates.

2.  [`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md):
    declare the aggregate-data arm.

3.  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md):
    join the two into an `mlumr_data` object.

4.  [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md):
    build the Gaussian-copula QMC integration points used to marginalize
    over the AgD covariate distribution.

5.  [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md): fit
    the Bayesian ML-UMR model (SPFA or relaxed).

6.  [`prior_summary()`](https://choxos.github.io/mlumr/reference/prior_summary.md),
    [`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md):
    sanity-check the model before inferring from it.

7.  [predict()](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md),
    [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md),
    [`conditional_effects()`](https://choxos.github.io/mlumr/reference/conditional_effects.md),
    [`conditional_predict()`](https://choxos.github.io/mlumr/reference/conditional_predict.md):
    extract population-level and profile-specific quantities.

8.  [`calculate_dic()`](https://choxos.github.io/mlumr/reference/calculate_dic.md),
    [`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md),
    [`calculate_waic()`](https://choxos.github.io/mlumr/reference/calculate_waic.md),
    [`compare_models()`](https://choxos.github.io/mlumr/reference/compare_models.md):
    compare SPFA vs relaxed or competing specifications.

9.  [`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md):
    examine robustness over specified prior choices.

## Priors

The prior constructors
[`prior_normal()`](https://choxos.github.io/mlumr/reference/prior_normal.md),
[`prior_student_t()`](https://choxos.github.io/mlumr/reference/prior_student_t.md),
[`prior_cauchy()`](https://choxos.github.io/mlumr/reference/prior_cauchy.md),
and
[`prior_exponential()`](https://choxos.github.io/mlumr/reference/prior_exponential.md)
all plug into
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) via
`prior_intercept`, `prior_beta`, and (normal family only) `prior_sigma`.
Survival models add two further knobs: `prior_aux` for the parametric
shape or scale parameter(s) (Weibull/Gompertz shape, log-normal sdlog,
generalized gamma shapes; see
[`default_prior_aux()`](https://choxos.github.io/mlumr/reference/default_priors.md))
and `prior_smooth` for the M-spline / piecewise-exponential baseline
smoothing standard deviation (see
[`default_prior_smooth()`](https://choxos.github.io/mlumr/reference/default_priors.md)).
The package defaults are generic starting values; calibrate them using
prior predictive checks and subject-matter knowledge.

## Quiet mode

Set `options(mlumr.quiet = TRUE)` to suppress the package startup banner
in scripted sessions (in addition to the standard
[`suppressPackageStartupMessages()`](https://rdrr.io/r/base/message.html)).

## Alternative methods

[`stc()`](https://choxos.github.io/mlumr/reference/stc.md) performs
G-computation by fitting a one-arm outcome model and standardizing
index-treatment predictions to the comparator population before
contrasting them with the observed comparator outcome there. It does not
standardize to the index population.
[`naive()`](https://choxos.github.io/mlumr/reference/naive.md) computes
an unadjusted contrast between the observed index study and comparator
study, so it does not have a single common target population. Both
consume the same `mlumr_data` object as
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md), but
their estimands must be interpreted before numerical results are
compared.

## See also

Useful links:

- <https://github.com/choxos/mlumr>

- <https://choxos.github.io/mlumr/>

- Report bugs at <https://github.com/choxos/mlumr/issues>

## Author

**Maintainer**: Ahmad Sofi-Mahmudi <a.sofimahmudi@gmail.com>
([ORCID](https://orcid.org/0000-0001-6829-0823))

Authors:

- Ahmad Sofi-Mahmudi <a.sofimahmudi@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-6829-0823))

- Conor Chandler ([ORCID](https://orcid.org/0000-0002-1365-9002))
