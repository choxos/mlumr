// ML-UMR: Count outcomes with Relaxed SPFA
// Poisson likelihood with treatment-specific prognostic factors.
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
  array[n_ipd] int<lower=0> y_ipd;
  // Exposure is a log offset in the IPD likelihood. set_ipd() rejects a
  // non-positive exposure, so log(E_ipd) is always finite here.
  vector<lower=0>[n_ipd] E_ipd;
  int<lower=1> n_cov;
  matrix[n_ipd, n_cov] X_ipd;            // centered covariates (generated quantities)

  // AgD (Comparator treatment)
  int<lower=1> n_agd_rows;
  array[n_agd_rows] int<lower=0> r_agd;
  // AgD exposure scales the marginal rate into an expected total count.
  // set_agd() rejects a non-positive exposure, so log(E_agd) is finite.
  array[n_agd_rows] real<lower=0> E_agd;

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

  int<lower=1,upper=1> link;
}

parameters {
  // Combined coefficients on the (QR) sampling scale:
  // [mu_index, mu_comparator, beta_index, beta_comparator].
  vector[nB] beta_tilde;
  // Note: beta_comparator is identified only through AgD data. With sparse
  // aggregate evidence, regularize it with an informative prior_beta or use
  // model = "spfa", which shares one coefficient vector across treatments.
}

transformed parameters {
  // Recover original-scale coefficients (affine map, no Jacobian needed).
  vector[nB] allbeta = qr ? R_inv * beta_tilde : beta_tilde;
  real mu_index = allbeta[1];
  real mu_comparator = allbeta[2];
  vector[n_cov] beta_index = segment(allbeta, 3, n_cov);
  vector[n_cov] beta_comparator = segment(allbeta, 3 + n_cov, n_cov);
  // IPD log-rate: linear predictor via the (possibly QR-rotated) design + offset.
  vector[n_ipd] log_lambda_ipd = Xq_ipd * beta_tilde + log(E_ipd);
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

  // IPD likelihood. Keep the fused GLM for the (default) non-QR path; the
  // log-exposure offset enters as a per-observation intercept.
  if (qr)
    y_ipd ~ poisson_log(log_lambda_ipd);
  else
    y_ipd ~ poisson_log_glm(X_ipd, mu_index + log(E_ipd), beta_index);

  // AgD likelihood for total counts after marginalizing over covariates.
  for (k in 1:n_agd_rows) {
    // AgD log-rate (integrated over covariate distribution) - log-sum-exp stable.
    vector[n_int] log_lambda_agd_int = Xq_int[k] * beta_tilde;
    real log_lambda_agd_bar = log(E_agd[k]) + log_sum_exp(log_lambda_agd_int) - log(n_int);
    r_agd[k] ~ poisson_log(log_lambda_agd_bar);
  }
}

generated quantities {
  // Marginal effects in both populations
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

  // Effect modification parameters (difference in regression coefficients)
  vector[n_cov] delta_beta;

  // Pointwise log-likelihoods keep IPD observations and AgD rows separate.
  // This is the contract used by loo(), waic(), and DIC helpers.
  vector[n_ipd] log_lik_ipd;
  vector[n_agd_rows] log_lik_agd;

  // Marginal predictions in index population - vectorized
  {
    log_rate_index_index = log_mean_exp_vec(mu_index + X_ipd * beta_index);
    log_rate_comparator_index = log_mean_exp_vec(mu_comparator + X_ipd * beta_comparator);
    rate_index_index = exp(log_rate_index_index);
    rate_comparator_index = exp(log_rate_comparator_index);
  }

  // Marginal predictions in comparator population - vectorized
  {
    vector[n_agd_rows] log_rate_index_row;
    vector[n_agd_rows] log_rate_comparator_row;
    vector[n_agd_rows] exposure;

    for (k in 1:n_agd_rows) {
      log_rate_index_row[k] = log_mean_exp_vec(mu_index + X_int[k] * beta_index);
      log_rate_comparator_row[k] = log_mean_exp_vec(
        mu_comparator + X_int[k] * beta_comparator);
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

  // Calculate effect modification
  delta_beta = beta_index - beta_comparator;

  // Per-observation log-likelihoods
  for (i in 1:n_ipd)
    log_lik_ipd[i] = poisson_log_lpmf(y_ipd[i] | log_lambda_ipd[i]);

  for (k in 1:n_agd_rows) {
    vector[n_int] log_lambda_agd_int = mu_comparator + X_int[k] * beta_comparator;
    real log_lambda_agd_bar = log(E_agd[k]) + log_sum_exp(log_lambda_agd_int) - log(n_int);
    log_lik_agd[k] = poisson_log_lpmf(r_agd[k] | log_lambda_agd_bar);
  }
}
