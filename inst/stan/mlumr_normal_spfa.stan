// ML-UMR: Continuous outcomes with SPFA
// Normal likelihood with shared prognostic factors

functions {
#include include/priors_functions.stan
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
  vector[n_cov] z_beta;
  real<lower=0> sigma;
}

transformed parameters {
  // SPFA uses one prognostic coefficient vector for both treatments.
  // NOTE: SPFA imposes a shared beta across treatments.
  // Use the relaxed model as a sensitivity analysis if effect modification is plausible.
  vector[n_cov] beta = prior_beta_mean + prior_beta_sd .* z_beta;
  vector[n_ipd] theta_ipd;

  // IPD linear predictor
  theta_ipd = mu_index + X_ipd * beta;
}

model {
  // Priors (dispatched on dist code)
  target += log_prior_scalar(mu_index, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_scalar(mu_comparator, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_std_vector(z_beta, prior_beta_dist, prior_beta_df);
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
    vector[n_int] theta_agd_int = mu_comparator + X_int[k] * beta;
    if (link == 1) {
      real theta_agd_bar = mean(theta_agd_int);
      y_agd[k] ~ normal(theta_agd_bar, se_agd[k]);
    } else {
      real mu_agd_bar = mean(exp(theta_agd_int));
      y_agd[k] ~ normal(mu_agd_bar, se_agd[k]);
    }
  }
}

generated quantities {
  // Marginal effects in both populations
  // Generated quantities evaluate both treatments in each target population.
  // Population suffixes follow treatment_population naming.
  real delta_index;
  real delta_comparator;

  // Absolute predictions
  real y_index_index;
  real y_comparator_index;
  real y_index_comparator;
  real y_comparator_comparator;

  // Pointwise log-likelihoods keep IPD observations and AgD rows separate.
  // This is the contract used by loo(), waic(), and DIC helpers.
  vector[n_ipd] log_lik_ipd;
  vector[n_agd_rows] log_lik_agd;

  // Marginal predictions in index population - vectorized
  {
    vector[n_ipd] y_index_vec = mu_index + X_ipd * beta;
    vector[n_ipd] y_comparator_vec = mu_comparator + X_ipd * beta;

    if (link == 1) {
      y_index_index = mean(y_index_vec);
      y_comparator_index = mean(y_comparator_vec);
    } else {
      y_index_index = mean(exp(y_index_vec));
      y_comparator_index = mean(exp(y_comparator_vec));
    }
  }

  // Marginal predictions in comparator population - equal weights
  // Equal weighting avoids double-counting with the likelihood (which already
  // upweights lower-SE studies through the normal density). For single-row
  // AgD (the most common case), weighting is irrelevant.
  {
    real y_index_comparator_sum = 0;
    real y_comparator_comparator_sum = 0;

    for (k in 1:n_agd_rows) {
      vector[n_int] y_idx_k = mu_index + X_int[k] * beta;
      vector[n_int] y_cmp_k = mu_comparator + X_int[k] * beta;

      if (link == 1) {
        y_index_comparator_sum += mean(y_idx_k);
        y_comparator_comparator_sum += mean(y_cmp_k);
      } else {
        y_index_comparator_sum += mean(exp(y_idx_k));
        y_comparator_comparator_sum += mean(exp(y_cmp_k));
      }
    }

    y_index_comparator = y_index_comparator_sum / n_agd_rows;
    y_comparator_comparator = y_comparator_comparator_sum / n_agd_rows;
  }

  // Calculate marginal effects (mean differences for continuous outcomes)
  delta_index = y_index_index - y_comparator_index;
  delta_comparator = y_index_comparator - y_comparator_comparator;

  // Per-observation log-likelihoods
  if (link == 1) {
    for (i in 1:n_ipd)
      log_lik_ipd[i] = normal_lpdf(y_ipd[i] | theta_ipd[i], sigma);
  } else {
    vector[n_ipd] mu_ipd = exp(theta_ipd);
    for (i in 1:n_ipd)
      log_lik_ipd[i] = normal_lpdf(y_ipd[i] | mu_ipd[i], sigma);
  }

  // AgD log-likelihood mirrors the model block for row-level diagnostics.
  for (k in 1:n_agd_rows) {
    vector[n_int] theta_agd_int = mu_comparator + X_int[k] * beta;
    if (link == 1) {
      real theta_agd_bar = mean(theta_agd_int);
      log_lik_agd[k] = normal_lpdf(y_agd[k] | theta_agd_bar, se_agd[k]);
    } else {
      real mu_agd_bar = mean(exp(theta_agd_int));
      log_lik_agd[k] = normal_lpdf(y_agd[k] | mu_agd_bar, se_agd[k]);
    }
  }
}
