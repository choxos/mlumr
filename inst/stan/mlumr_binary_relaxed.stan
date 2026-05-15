// ML-UMR: Binary outcomes with Relaxed SPFA
// Bernoulli/binomial likelihood with treatment-specific prognostic factors

functions {
#include include/priors_functions.stan
#include include/numerical_functions.stan
#include include/binary_functions.stan
}

data {
  // IPD contributes individual Bernoulli observations for the index treatment.
  int<lower=0> n_ipd;
  array[n_ipd] int<lower=0,upper=1> y_ipd;
  int<lower=1> n_cov;
  matrix[n_ipd, n_cov] X_ipd;

  // AgD contributes binomial summaries for the comparator treatment.
  int<lower=1> n_agd_rows;
  array[n_agd_rows] int<lower=1> n_agd;
  array[n_agd_rows] int<lower=0> r_agd;

  // Row-specific integration points approximate each AgD covariate distribution.
  int<lower=1> n_int;
  array[n_agd_rows] matrix[n_int, n_cov] X_int;

#include include/priors_hyperparameters.stan

  int<lower=1,upper=3> link;
}

parameters {
  // Treatment-specific intercepts encode the baseline treatment contrast.
  real mu_index;
  real mu_comparator;
  // Affine (non-centered) reparameterization: sample on a standardized scale
  // and recover beta_* in transformed parameters. See priors_functions.stan.
  vector[n_cov] z_beta_index;
  vector[n_cov] z_beta_comparator;
  // NOTE: beta_comparator is identified only through AgD data.
  // With sparse AgD (few rows), this creates weak identifiability;
  // use informative priors on beta to regularize.
}

transformed parameters {
  // Relaxed SPFA allows treatment-specific prognostic coefficients.
  vector[n_cov] beta_index      = prior_beta_mean + prior_beta_sd .* z_beta_index;
  vector[n_cov] beta_comparator = prior_beta_mean + prior_beta_sd .* z_beta_comparator;
  vector[n_ipd] eta_ipd = mu_index + X_ipd * beta_index;
}

model {
  // Priors (dispatched on dist code)
  target += log_prior_scalar(mu_index, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_scalar(mu_comparator, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_std_vector(z_beta_index, prior_beta_dist, prior_beta_df);
  target += log_prior_std_vector(z_beta_comparator, prior_beta_dist, prior_beta_df);

  // The same binary link is used for the IPD likelihood and AgD integration.
  if (link == 1)
    y_ipd ~ bernoulli_logit(eta_ipd);
  else
    y_ipd ~ bernoulli(inv_link_binary_vec(eta_ipd, link));

  // The binomial AgD likelihood uses average probability, not average linear predictor.
  for (k in 1:n_agd_rows) {
    // Integrate the comparator event probability over the AgD covariates.
    vector[n_int] p_int = inv_link_binary_vec(mu_comparator + X_int[k] * beta_comparator, link);
    real p_mean = mean(p_int);
    r_agd[k] ~ binomial(n_agd[k], p_mean);
  }
}

generated quantities {
  // Estimands are marginal, response-scale population averages.
  real lor_index;
  real lor_comparator;
  real rd_index;
  real rd_comparator;
  real rr_index;
  real rr_comparator;
  real p_index_index;
  real p_comparator_index;
  real p_index_comparator;
  real p_comparator_comparator;
  // Effect modification parameters (difference in regression coefficients)
  vector[n_cov] delta_beta;
  vector[n_ipd] log_lik_ipd;
  vector[n_agd_rows] log_lik_agd;

  // Calculate effect modification
  delta_beta = beta_index - beta_comparator;

  // Index population (IPD covariates)
  {
    vector[n_ipd] p_index_vec = inv_link_binary_vec(eta_ipd, link);
    vector[n_ipd] p_comparator_vec = inv_link_binary_vec(mu_comparator + X_ipd * beta_comparator, link);
    p_index_index = mean(p_index_vec);
    p_comparator_index = mean(p_comparator_vec);
  }

  // Comparator population (integration points)
  // AgD rows are weighted by their binomial sample sizes.
  {
    real p_index_comp_sum = 0;
    real p_comp_comp_sum = 0;
    real total_n = 0;
    for (k in 1:n_agd_rows) {
      vector[n_int] p_idx_k = inv_link_binary_vec(mu_index + X_int[k] * beta_index, link);
      vector[n_int] p_cmp_k = inv_link_binary_vec(mu_comparator + X_int[k] * beta_comparator, link);
      p_index_comp_sum += mean(p_idx_k) * n_agd[k];
      p_comp_comp_sum += mean(p_cmp_k) * n_agd[k];
      total_n += n_agd[k];
    }
    p_index_comparator = p_index_comp_sum / total_n;
    p_comparator_comparator = p_comp_comp_sum / total_n;
  }

  // LOR is a marginal odds ratio computed from response-scale population
  // probabilities, not generally a conditional linear-predictor contrast.
  // safe_logit clips probabilities to [1e-10, 1-1e-10] to avoid infinite
  // log-odds when probabilities are numerically 0 or 1.
  lor_index = safe_logit(p_index_index) - safe_logit(p_comparator_index);
  lor_comparator = safe_logit(p_index_comparator) - safe_logit(p_comparator_comparator);
  rd_index = p_index_index - p_comparator_index;
  rd_comparator = p_index_comparator - p_comparator_comparator;
  // safe_divide clips denominator to 1e-10; for near-zero comparator rates,
  // RR will be artificially large rather than undefined.
  rr_index = safe_divide(p_index_index, p_comparator_index);
  rr_comparator = safe_divide(p_index_comparator, p_comparator_comparator);

  // Pointwise log-likelihoods keep IPD observations and AgD rows separate.
  // This is the contract used by loo(), waic(), and DIC helpers.
  if (link == 1) {
    for (i in 1:n_ipd)
      log_lik_ipd[i] = bernoulli_logit_lpmf(y_ipd[i] | eta_ipd[i]);
  } else {
    vector[n_ipd] p_ipd = inv_link_binary_vec(eta_ipd, link);
    for (i in 1:n_ipd)
      log_lik_ipd[i] = bernoulli_lpmf(y_ipd[i] | p_ipd[i]);
  }

  for (k in 1:n_agd_rows) {
    vector[n_int] p_int = inv_link_binary_vec(mu_comparator + X_int[k] * beta_comparator, link);
    real p_mean = mean(p_int);
    log_lik_agd[k] = binomial_lpmf(r_agd[k] | n_agd[k], p_mean);
  }
}
