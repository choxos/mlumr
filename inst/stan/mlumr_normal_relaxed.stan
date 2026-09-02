// ML-UMR: Continuous outcomes with Relaxed SPFA
// Normal likelihood with treatment-specific prognostic factors.
// The combined (intercepts + treatment-specific covariates) design is supplied
// as `Xq_*` (raw centered design when qr = 0, scaled thin-QR factor Q when
// qr = 1); coefficients are recovered as `allbeta = R_inv * beta_tilde`. See R/mlumr.R (.mlumr_qr_design()) for the centering and QR design construction.

functions {
#include include/priors_functions.stan
#include include/numerical_functions.stan
}

data {
  // IPD (Index treatment)
  int<lower=0> n_ipd;
  vector[n_ipd] y_ipd;
  int<lower=1> n_cov;
  matrix[n_ipd, n_cov] X_ipd;            // centered covariates (generated quantities)

  // AgD (Comparator treatment)
  // y_agd and se_agd must be on the original outcome scale.
  int<lower=1> n_agd_rows;
  array[n_agd_rows] real y_agd;
  // set_agd() rejects a non-positive standard error, so the normal density
  // below never sees a zero scale.
  array[n_agd_rows] real<lower=0> se_agd;

  // Integration points for AgD
  int<lower=1> n_int;
  array[n_agd_rows] matrix[n_int, n_cov] X_int;   // centered covariates (gen. quantities)

  // Combined (intercepts + index/comparator covariates) design and optional QR.
  int<lower=0,upper=1> qr;               // 1 = sample on the QR scale
  int<lower=1> nB;                       // design columns = 2 + 2 * n_cov
  matrix[n_ipd, nB] Xq_ipd;             // design for IPD rows (Q if qr = 1)
  array[n_agd_rows] matrix[n_int, nB] Xq_int;     // design for AgD integration rows
  matrix[nB, nB] R_inv;                  // R^{-1} (identity when qr = 0)

#include include/priors_hyperparameters.stan

#include include/priors_sigma_hyperparameters.stan

  int<lower=1,upper=2> link;
}

parameters {
  // Combined coefficients on the (QR) sampling scale:
  // [mu_index, mu_comparator, beta_index, beta_comparator].
  vector[nB] beta_tilde;
  // Note: beta_comparator is identified only through AgD data. With sparse
  // aggregate evidence, regularize it with an informative prior_beta or use
  // model = "spfa", which shares one coefficient vector across treatments.
  real<lower=0> sigma;
}

transformed parameters {
  // Recover original-scale coefficients (affine map, no Jacobian needed).
  vector[nB] allbeta = qr ? R_inv * beta_tilde : beta_tilde;
  real mu_index = allbeta[1];
  real mu_comparator = allbeta[2];
  vector[n_cov] beta_index = segment(allbeta, 3, n_cov);
  vector[n_cov] beta_comparator = segment(allbeta, 3 + n_cov, n_cov);
  // IPD linear predictor via the (possibly QR-rotated) design.
  vector[n_ipd] theta_ipd = Xq_ipd * beta_tilde;
}

model {
  // Priors on the original-scale parameters.
  target += log_prior_scalar(mu_index, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_scalar(mu_comparator, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_vector(beta_index, prior_beta_mean, prior_beta_sd,
                             prior_beta_dist, prior_beta_df);
  target += log_prior_vector(beta_comparator, prior_beta_mean,
                             prior_beta_sd, prior_beta_dist,
                             prior_beta_df);
  // sigma has <lower=0> so normal / student_t become half-*
  target += log_prior_sigma(sigma, prior_sigma_location, prior_sigma_scale,
                            prior_sigma_dist, prior_sigma_df);

  // IPD likelihood. For log link, theta is transformed to the outcome scale.
  // Keep the fused GLM for the (default) non-QR identity-link path.
  if (link == 1) {
    if (qr)
      y_ipd ~ normal(theta_ipd, sigma);
    else
      y_ipd ~ normal_id_glm(X_ipd, mu_index, beta_index, sigma);
  } else {
    y_ipd ~ normal(exp(theta_ipd), sigma);
  }

  // AgD likelihood matches the reported mean and standard error on the
  // original outcome scale after marginalizing over comparator covariates.
  for (k in 1:n_agd_rows) {
    vector[n_int] theta_agd_int = Xq_int[k] * beta_tilde;
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
