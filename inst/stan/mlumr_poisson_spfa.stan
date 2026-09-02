// ML-UMR: Count outcomes with SPFA
// Poisson likelihood with shared prognostic factors

functions {
#include include/priors_functions.stan
#include include/numerical_functions.stan
}

data {
  // IPD (Index treatment)
  int<lower=0> n_ipd;
  array[n_ipd] int<lower=0> y_ipd;
  // Exposure is a log offset in the IPD likelihood.
  vector<lower=1e-12>[n_ipd] E_ipd;
  int<lower=1> n_cov;
  matrix[n_ipd, n_cov] X_ipd;

  // AgD (Comparator treatment)
  int<lower=1> n_agd_rows;
  array[n_agd_rows] int<lower=0> r_agd;
  // AgD exposure scales the marginal rate into an expected total count.
  array[n_agd_rows] real<lower=1e-12> E_agd;

  // Integration points for AgD
  int<lower=1> n_int;
  array[n_agd_rows] matrix[n_int, n_cov] X_int;

#include include/priors_hyperparameters.stan

  int<lower=1,upper=1> link;
}

parameters {
  real mu_index;
  real mu_comparator;
  // Affine (non-centered) reparameterization: see priors_functions.stan.
  vector[n_cov] z_beta;
}

transformed parameters {
  // SPFA uses one prognostic coefficient vector for both treatments.
  // NOTE: SPFA imposes a shared beta across treatments.
  // Use the relaxed model as a sensitivity analysis if effect modification is plausible.
  vector[n_cov] beta = prior_beta_mean + prior_beta_sd .* z_beta;
  vector[n_ipd] log_lambda_ipd;

  // IPD log-rate
  log_lambda_ipd = mu_index + X_ipd * beta + log(E_ipd);
}

model {
  // Priors (dispatched on dist code)
  target += log_prior_scalar(mu_index, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_scalar(mu_comparator, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_std_vector(z_beta, prior_beta_dist, prior_beta_df);

  // IPD likelihood
  y_ipd ~ poisson_log(log_lambda_ipd);

  // AgD likelihood for total counts after marginalizing over covariates.
  for (k in 1:n_agd_rows) {
    // AgD log-rate (integrated over covariate distribution) - vectorized with log-sum-exp
    vector[n_int] log_lambda_agd_int = mu_comparator + X_int[k] * beta;
    // Use log-sum-exp to avoid overflow: log(mean(exp(x))) = log_sum_exp(x) - log(n)
    real log_lambda_agd_bar = log(E_agd[k]) + log_sum_exp(log_lambda_agd_int) - log(n_int);

    r_agd[k] ~ poisson_log(log_lambda_agd_bar);
  }
}

generated quantities {
  // Marginal effects in both populations
  // Generated quantities evaluate both treatments in each target population.
  // Population suffixes follow treatment_population naming.
  real delta_index;
  real delta_comparator;

  // Absolute predictions (rates per unit exposure)
  real rate_index_index;
  real rate_comparator_index;
  real rate_index_comparator;
  real rate_comparator_comparator;
  real log_rate_index_index;
  real log_rate_comparator_index;
  real log_rate_index_comparator;
  real log_rate_comparator_comparator;

  // Pointwise log-likelihoods keep IPD observations and AgD rows separate.
  // This is the contract used by loo(), waic(), and DIC helpers.
  vector[n_ipd] log_lik_ipd;
  vector[n_agd_rows] log_lik_agd;

  // Marginal predictions in index population - vectorized
  {
    log_rate_index_index = log_mean_exp_vec(mu_index + X_ipd * beta);
    log_rate_comparator_index = log_mean_exp_vec(mu_comparator + X_ipd * beta);
    rate_index_index = exp(log_rate_index_index);
    rate_comparator_index = exp(log_rate_comparator_index);
  }

  // Marginal predictions in comparator population - vectorized
  {
    vector[n_agd_rows] log_rate_index_row;
    vector[n_agd_rows] log_rate_comparator_row;
    vector[n_agd_rows] exposure;

    for (k in 1:n_agd_rows) {
      log_rate_index_row[k] = log_mean_exp_vec(mu_index + X_int[k] * beta);
      log_rate_comparator_row[k] = log_mean_exp_vec(mu_comparator + X_int[k] * beta);
      exposure[k] = E_agd[k];
    }

    log_rate_index_comparator = log_weighted_mean_exp_vec(log_rate_index_row,
                                                          exposure);
    log_rate_comparator_comparator = log_weighted_mean_exp_vec(
      log_rate_comparator_row, exposure);
    rate_index_comparator = exp(log_rate_index_comparator);
    rate_comparator_comparator = exp(log_rate_comparator_comparator);
  }

  delta_index = exp(log_rate_index_index - log_rate_comparator_index);
  delta_comparator = exp(log_rate_index_comparator -
                         log_rate_comparator_comparator);

  // Per-observation log-likelihoods
  for (i in 1:n_ipd)
    log_lik_ipd[i] = poisson_log_lpmf(y_ipd[i] | log_lambda_ipd[i]);

  // AgD log-likelihood mirrors the model block for row-level diagnostics.
  for (k in 1:n_agd_rows) {
    vector[n_int] log_lambda_agd_int = mu_comparator + X_int[k] * beta;
    real log_lambda_agd_bar = log(E_agd[k]) + log_sum_exp(log_lambda_agd_int) - log(n_int);
    log_lik_agd[k] = poisson_log_lpmf(r_agd[k] | log_lambda_agd_bar);
  }
}
