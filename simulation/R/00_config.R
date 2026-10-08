# Settings of the ML-UMR simulation study (Chandler and Ishak, arXiv:2606.20341,
# Appendix C). Every value here reproduces the published results.

config <- list(
  n_replications = 500,
  sample_sizes = 150,
  effect_modification = c("none", "weak", "strong"),
  # "low" is the paper's "moderate" imbalance; see get_imbalance_params() for
  # the prevalences.
  population_imbalance = c("low", "high"),
  covariate_correlation_index = 0.5,
  covariate_correlation_comparator = c(0.5, 0.25),
  n_covariates = 2,

  # MCMC: 3 chains of 1,000 warmup and 1,000 sampling iterations.
  chains = 3,
  iter = 2000,
  warmup = 1000,
  adapt_delta = 0.95,
  max_treedepth = 15,
  # Normal(0, 10) priors on every intercept and coefficient.
  prior_sd = 10,

  # Integration points: the overall AgD row (SPFA, and STC) and each of the
  # subgroup AgD rows (relaxed SPFA).
  n_int_spfa = 512,
  n_int_relaxed = 128,

  # Replicate r of scenario s uses seed base_seed + 10000 s + r (the relaxed
  # fit adds 50000).
  base_seed = 20241212,

  # Size of the populations the true effects are computed on.
  truth_n = 1e7,

  # Covariates the analyses leave out (ML-UMR, STC and MAIC; the data are
  # unchanged): character(0) as published, or "x1" to omit the prognostic
  # factor X1. The datasets, seeds and scenario numbers are the same either
  # way.
  omit = character(0)
)

#' The scenario grid, numbered as in the published results (effect
#' modification varies fastest, then imbalance, then comparator correlation).
scenario_grid <- function(config) {
  sc <- expand.grid(
    sample_size = config$sample_sizes,
    effect_modification = config$effect_modification,
    population_imbalance = config$population_imbalance,
    covariate_correlation_index = config$covariate_correlation_index,
    covariate_correlation_comparator = config$covariate_correlation_comparator,
    covariate_type = "binary",
    stringsAsFactors = FALSE
  )
  sc$n_covariates <- config$n_covariates
  sc$scenario_id <- seq_len(nrow(sc))
  sc$scenario_name <- with(sc, paste0(
    "n", sample_size, "_em", substr(effect_modification, 1, 4),
    "_imb", substr(population_imbalance, 1, 3),
    "_corI", covariate_correlation_index, "_corC", covariate_correlation_comparator,
    "_", substr(covariate_type, 1, 3)
  ))
  sc
}
