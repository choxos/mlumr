// ML-UMR: Continuous outcomes with Relaxed SPFA
// Normal likelihood with treatment-specific prognostic factors

functions {
#include include/priors_functions.stan
#include include/numerical_functions.stan
}

data {
  // IPD (Index treatment)
  int<lower=0> n_ipd;
  vector[n_ipd] y_ipd;
  int<lower=1> n_cov;
  matrix[n_ipd, n_cov] X_ipd;

  // AgD (Comparator treatment)
  // y_agd and se_agd must be on the original outcome scale.
  int<lower=1> n_agd_rows;
  array[n_agd_rows] real y_agd;
  array[n_agd_rows] real<lower=1e-12> se_agd;

  // Integration points for AgD
  int<lower=1> n_int;
  array[n_agd_rows] matrix[n_int, n_cov] X_int;

#include include/priors_hyperparameters.stan
#include include/priors_sigma_hyperparameters.stan

  int<lower=1,upper=2> link;
}

parameters {
  real mu_index;
  real mu_comparator;
  // Affine (non-centered) reparameterization: see priors_functions.stan.
  vector[n_cov] z_beta_index;
  vector[n_cov] z_beta_comparator;
  // NOTE: beta_comparator is identified only through AgD data.
  // With sparse AgD (few rows), this creates weak identifiability;
  // use informative priors on beta to regularize.
  real<lower=0> sigma;
}

transformed parameters {
  // Relaxed SPFA allows treatment-specific prognostic coefficients.
  vector[n_cov] beta_index      = prior_beta_mean + prior_beta_sd .* z_beta_index;
  vector[n_cov] beta_comparator = prior_beta_mean + prior_beta_sd .* z_beta_comparator;
  vector[n_ipd] theta_ipd;

  // IPD linear predictor
  theta_ipd = mu_index + X_ipd * beta_index;
}

model {
  // Priors (dispatched on dist code)
  target += log_prior_scalar(mu_index, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_scalar(mu_comparator, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_std_vector(z_beta_index, prior_beta_dist, prior_beta_df);
  target += log_prior_std_vector(z_beta_comparator, prior_beta_dist, prior_beta_df);
  // sigma has <lower=0> so normal / student_t become half-*
  target += log_prior_sigma(sigma, prior_sigma_location, prior_sigma_scale,
                            prior_sigma_dist, prior_sigma_df);

  // IPD likelihood. For log link, theta is transformed to the outcome scale.
  if (link == 1)
    y_ipd ~ normal(theta_ipd, sigma);
  else
    y_ipd ~ normal(exp(theta_ipd), sigma);

  // AgD likelihood matches the reported mean and standard error on the
  // original outcome scale after marginalizing over comparator covariates.
  for (k in 1:n_agd_rows) {
    vector[n_int] theta_agd_int = mu_comparator + X_int[k] * beta_comparator;
    if (link == 1) {
      real theta_agd_bar = mean(theta_agd_int);
      y_agd[k] ~ normal(theta_agd_bar, se_agd[k]);
    } else {
      real mu_agd_bar = exp(log_mean_exp_vec(theta_agd_int));
      y_agd[k] ~ normal(mu_agd_bar, se_agd[k]);
    }
  }
}

generated quantities {
  // Marginal effects in both populations
  real delta_index;
  real delta_comparator;

  // Absolute predictions
  real y_index_index;
  real y_comparator_index;
  real y_index_comparator;
  real y_comparator_comparator;
  real link_y_index_index;
  real link_y_comparator_index;
  real link_y_index_comparator;
  real link_y_comparator_comparator;

  // Effect modification parameters (difference in regression coefficients)
  vector[n_cov] delta_beta;

  // Pointwise log-likelihoods keep IPD observations and AgD rows separate.
  // This is the contract used by loo(), waic(), and DIC helpers.
  vector[n_ipd] log_lik_ipd;
  vector[n_agd_rows] log_lik_agd;

  // Marginal predictions in index population - vectorized
  {
    vector[n_ipd] y_index_vec = mu_index + X_ipd * beta_index;
    vector[n_ipd] y_comparator_vec = mu_comparator + X_ipd * beta_comparator;

    if (link == 1) {
      y_index_index = mean(y_index_vec);
      y_comparator_index = mean(y_comparator_vec);
      link_y_index_index = y_index_index;
      link_y_comparator_index = y_comparator_index;
      delta_index = y_index_index - y_comparator_index;
    } else {
      real log_y_index = log_mean_exp_vec(y_index_vec);
      real log_y_comparator = log_mean_exp_vec(y_comparator_vec);
      y_index_index = exp(log_y_index);
      y_comparator_index = exp(log_y_comparator);
      link_y_index_index = log_y_index;
      link_y_comparator_index = log_y_comparator;
      delta_index = exp_difference(log_y_index, log_y_comparator);
    }
  }

  // Marginal predictions in comparator population, equal weights.
  // Equal weighting avoids double-counting with the likelihood (which already
  // upweights lower-SE studies through the normal density). For single-row
  // AgD (the most common case), weighting is irrelevant.
  {
    vector[n_agd_rows] row_index;
    vector[n_agd_rows] row_comparator;
    vector[n_agd_rows] weights;

    for (k in 1:n_agd_rows) {
      vector[n_int] y_idx_k = mu_index + X_int[k] * beta_index;
      vector[n_int] y_cmp_k = mu_comparator + X_int[k] * beta_comparator;
      weights[k] = 1;

      if (link == 1) {
        row_index[k] = mean(y_idx_k);
        row_comparator[k] = mean(y_cmp_k);
      } else {
        row_index[k] = log_mean_exp_vec(y_idx_k);
        row_comparator[k] = log_mean_exp_vec(y_cmp_k);
      }
    }

    if (link == 1) {
      y_index_comparator = dot_product(weights, row_index) / sum(weights);
      y_comparator_comparator = dot_product(weights, row_comparator) /
                                sum(weights);
      link_y_index_comparator = y_index_comparator;
      link_y_comparator_comparator = y_comparator_comparator;
      delta_comparator = y_index_comparator - y_comparator_comparator;
    } else {
      real log_y_index = log_weighted_mean_exp_vec(row_index, weights);
      real log_y_comparator = log_weighted_mean_exp_vec(row_comparator, weights);
      y_index_comparator = exp(log_y_index);
      y_comparator_comparator = exp(log_y_comparator);
      link_y_index_comparator = log_y_index;
      link_y_comparator_comparator = log_y_comparator;
      delta_comparator = exp_difference(log_y_index, log_y_comparator);
    }
  }

  // Calculate effect modification
  delta_beta = beta_index - beta_comparator;

  // Per-observation log-likelihoods
  if (link == 1) {
    for (i in 1:n_ipd)
      log_lik_ipd[i] = normal_lpdf(y_ipd[i] | theta_ipd[i], sigma);
  } else {
    vector[n_ipd] mu_ipd = exp(theta_ipd);
    for (i in 1:n_ipd)
      log_lik_ipd[i] = normal_lpdf(y_ipd[i] | mu_ipd[i], sigma);
  }

  for (k in 1:n_agd_rows) {
    vector[n_int] theta_agd_int = mu_comparator + X_int[k] * beta_comparator;
    if (link == 1) {
      real theta_agd_bar = mean(theta_agd_int);
      log_lik_agd[k] = normal_lpdf(y_agd[k] | theta_agd_bar, se_agd[k]);
    } else {
      real mu_agd_bar = exp(log_mean_exp_vec(theta_agd_int));
      log_lik_agd[k] = normal_lpdf(y_agd[k] | mu_agd_bar, se_agd[k]);
    }
  }
}
