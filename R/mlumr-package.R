#' mlumr: Multilevel Unanchored Meta-Regression for Indirect Treatment
#' Comparisons
#'
#' Bayesian multilevel unanchored meta-regression (ML-UMR) for population-
#' adjusted indirect comparisons between a single-arm individual patient
#' data (IPD) study on the index treatment and aggregate data (AgD) on a
#' comparator. Two model variants are provided: a shared-prognostic-factor
#' (SPFA) model and a relaxed-SPFA model that allows treatment-specific
#' covariate coefficients. Supports binary (binomial), continuous (normal),
#' and count (Poisson) outcomes, each with appropriate link functions.
#' Frequentist outcome regression (Simulated Treatment Comparison) and a
#' naive unadjusted benchmark are included for side-by-side comparison.
#'
#' @section Typical workflow:
#' \enumerate{
#'   \item [set_ipd()] — declare the individual patient data and its
#'     covariates.
#'   \item [set_agd()] — declare the aggregate-data arm.
#'   \item [combine_data()] — join the two into an `mlumr_data` object.
#'   \item [add_integration()] — build the Gaussian-copula QMC
#'     integration points used to marginalize over the AgD covariate
#'     distribution.
#'   \item [mlumr()] — fit the Bayesian ML-UMR model (SPFA or relaxed).
#'   \item [prior_summary()], [check_integration()] — sanity-check the
#'     model before inferring from it.
#'   \item [predict()][predict.mlumr_fit()], [marginal_effects()],
#'     [conditional_effects()], [conditional_predict()] — extract
#'     population-level and profile-specific quantities.
#'   \item [calculate_dic()], [calculate_loo()], [calculate_waic()],
#'     [compare_models()] — compare SPFA vs relaxed or competing
#'     specifications.
#'   \item [prior_sensitivity()] — verify the posterior is data-driven
#'     rather than prior-driven.
#' }
#'
#' @section Priors:
#' The prior constructors [prior_normal()], [prior_student_t()],
#' [prior_cauchy()], and [prior_exponential()] all plug into [mlumr()] via
#' `prior_intercept`, `prior_beta`, and (normal family only) `prior_sigma`.
#' Defaults follow the Stan community's prior-choice recommendations
#' (Vehtari et al., 2025).
#'
#' @section Alternative methods:
#' [stc()] runs frequentist parametric G-computation (Simulated Treatment
#' Comparison). [naive()] computes an unadjusted crude-rate benchmark. Both
#' consume the same `mlumr_data` object as [mlumr()] so their estimands can
#' be compared directly.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib mlumr, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stats as.formula binomial coef complete.cases cor dbinom glm pbinom plogis predict qbinom qlogis qnorm quantile sd setNames var vcov weighted.mean
## usethis namespace: end
NULL
