#' mlumr: Multilevel Unanchored Meta-Regression for Indirect Treatment
#' Comparisons
#'
#' Bayesian multilevel unanchored meta-regression (ML-UMR) for population-
#' adjusted indirect comparisons between a single-arm individual patient
#' data (IPD) study on the index treatment and aggregate data (AgD) on a
#' comparator. Two model variants are provided: a shared-prognostic-factor
#' (SPFA) model and a relaxed-SPFA model that allows treatment-specific
#' covariate coefficients. Supports binary (binomial), continuous (normal),
#' count (Poisson), and time-to-event (survival) outcomes, each with
#' appropriate link functions. Frequentist outcome regression (Simulated
#' Treatment Comparison) and a naive unadjusted benchmark are included for
#' side-by-side comparison.
#'
#' @section Typical workflow:
#' \enumerate{
#'   \item [set_ipd()]: declare the individual patient data and its
#'     covariates.
#'   \item [set_agd()]: declare the aggregate-data arm.
#'   \item [combine_data()]: join the two into an `mlumr_data` object.
#'   \item [add_integration()]: build the Gaussian-copula QMC
#'     integration points used to marginalize over the AgD covariate
#'     distribution.
#'   \item [mlumr()]: fit the Bayesian ML-UMR model (SPFA or relaxed).
#'   \item [prior_summary()], [check_integration()]: sanity-check the
#'     model before inferring from it.
#'   \item [predict()][predict.mlumr_fit()], [marginal_effects()],
#'     [conditional_effects()], [conditional_predict()]: extract
#'     population-level and profile-specific quantities.
#'   \item [calculate_dic()], [calculate_loo()], [calculate_waic()],
#'     [compare_models()]: compare SPFA vs relaxed or competing
#'     specifications.
#'   \item [prior_sensitivity()]: examine robustness over specified prior
#'     choices.
#' }
#'
#' @section Priors:
#' The prior constructors [prior_normal()], [prior_student_t()],
#' [prior_cauchy()], and [prior_exponential()] all plug into [mlumr()] via
#' `prior_intercept`, `prior_beta`, and (normal family only) `prior_sigma`.
#' Survival models add two further knobs: `prior_aux` for the parametric shape or
#' scale parameter(s) (Weibull/Gompertz shape, log-normal sdlog, generalized
#' gamma shapes; see [default_prior_aux()]) and `prior_smooth` for the M-spline /
#' piecewise-exponential baseline smoothing standard deviation (see
#' [default_prior_smooth()]). The package defaults are generic starting values;
#' calibrate them using prior predictive checks and subject-matter knowledge.
#'
#' @section Quiet mode:
#' Set `options(mlumr.quiet = TRUE)` to suppress the package startup banner in
#' scripted sessions (in addition to the standard
#' `suppressPackageStartupMessages()`).
#'
#' @section Alternative methods:
#' [stc()] performs G-computation by fitting a one-arm outcome model and
#' standardizing index-treatment
#' predictions to the comparator population before contrasting them with the
#' observed comparator outcome there. It does not standardize to the index
#' population. [naive()] computes an unadjusted contrast between the observed
#' index study and comparator study, so it does not have a single common target
#' population. Both consume the same `mlumr_data` object as [mlumr()], but their
#' estimands must be interpreted before numerical results are compared.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib mlumr, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom stats as.formula binomial coef complete.cases cor dbinom glm
#' @importFrom stats pbinom plogis predict qbinom qlogis qnorm quantile sd
#' @importFrom stats setNames var vcov weighted.mean
## usethis namespace: end
NULL
