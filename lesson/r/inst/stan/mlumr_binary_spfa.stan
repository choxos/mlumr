// ML-UMR: Binary outcomes with SPFA
// Bernoulli/binomial likelihood with shared prognostic factors.
// The (intercepts + covariates) design is supplied as `Xq_*`, which is either
// the raw centered design (qr = 0) or its scaled thin-QR factor Q (qr = 1).
// Sampling is on `beta_tilde`; original-scale coefficients are recovered as
// `allbeta = R_inv * beta_tilde` (R_inv = identity when qr = 0). See R/mlumr.R (.mlumr_qr_design()) for the centering and QR design construction.

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
  matrix[n_ipd, n_cov] X_ipd;            // centered covariates (generated quantities)

  // AgD contributes binomial summaries for the comparator treatment.
  int<lower=1> n_agd_rows;
  array[n_agd_rows] int<lower=1> n_agd;
  array[n_agd_rows] int<lower=0> r_agd;

  // Row-specific integration points approximate each AgD covariate distribution.
  int<lower=1> n_int;
  array[n_agd_rows] matrix[n_int, n_cov] X_int;   // centered covariates (gen. quantities)

  // Combined (intercepts + covariates) design and optional QR reparameterization.
  int<lower=0,upper=1> qr;               // 1 = sample on the QR scale
  int<lower=1> nB;                       // design columns = 2 + n_cov
  matrix[n_ipd, nB] Xq_ipd;             // design for IPD rows (Q if qr = 1)
  array[n_agd_rows] matrix[n_int, nB] Xq_int;     // design for AgD integration rows
  matrix[nB, nB] R_inv;                  // R^{-1} (identity when qr = 0)

#include include/priors_hyperparameters.stan

  int<lower=1,upper=3> link;
}

parameters {
  // Combined coefficients on the (QR) sampling scale. The leading two entries
  // are the index/comparator intercepts, the rest the shared prognostic beta.
  vector[nB] beta_tilde;
}

transformed parameters {
  // Recover original-scale coefficients. allbeta = R_inv * beta_tilde is affine
  // in data, so no Jacobian is needed for priors placed on the originals.
  vector[nB] allbeta = qr ? R_inv * beta_tilde : beta_tilde;
  real mu_index = allbeta[1];
  real mu_comparator = allbeta[2];
  vector[n_cov] beta = segment(allbeta, 3, n_cov);
  // IPD linear predictor via the (possibly QR-rotated) design.
  vector[n_ipd] eta_ipd = Xq_ipd * beta_tilde;
}

model {
  // Priors are placed on the original-scale parameters (intercepts + beta).
  target += log_prior_scalar(mu_index, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_scalar(mu_comparator, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_vector(beta, prior_beta_mean, prior_beta_sd,
                             prior_beta_dist, prior_beta_df);

  // IPD likelihood. Keep the fused GLM for the (default) non-QR logit path;
  // otherwise use the precomputed linear predictor.
  if (link == 1) {
    if (qr)
      y_ipd ~ bernoulli_logit(eta_ipd);
    else
      y_ipd ~ bernoulli_logit_glm(X_ipd, mu_index, beta);
  } else {
    for (i in 1:n_ipd)
      target += bernoulli_link_lpmf(y_ipd[i] | eta_ipd[i], link);
  }

  // The binomial AgD likelihood uses average probability, not average linear predictor.
  for (k in 1:n_agd_rows) {
    // Integrate the comparator event probability over the AgD covariates.
    vector[n_int] eta_int = Xq_int[k] * beta_tilde;
    target += integrated_binomial_lpmf(r_agd[k] | n_agd[k], eta_int, link);
  }
}

generated quantities {
  // Estimands are marginal, response-scale population averages.
  // Generated quantities evaluate both treatments in each target population.
  // Population suffixes follow treatment_population naming.
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
  real log_p_index_index;
  real log_q_index_index;
  real log_p_comparator_index;
  real log_q_comparator_index;
  real log_p_index_comparator;
  real log_q_index_comparator;
  real log_p_comparator_comparator;
  real log_q_comparator_comparator;
  vector[n_ipd] log_lik_ipd;
  vector[n_agd_rows] log_lik_agd;

  // Index population (IPD covariates)
  {
    vector[n_ipd] eta_comparator = mu_comparator + X_ipd * beta;
    log_p_index_index = log_mean_event_binary(eta_ipd, link);
    log_q_index_index = log_mean_nonevent_binary(eta_ipd, link);
    log_p_comparator_index = log_mean_event_binary(eta_comparator, link);
    log_q_comparator_index = log_mean_nonevent_binary(eta_comparator, link);
    p_index_index = exp(log_p_index_index);
    p_comparator_index = exp(log_p_comparator_index);
  }

  // Comparator population (integration points)
  // AgD rows are weighted by their binomial sample sizes.
  {
    vector[n_agd_rows] log_p_idx;
    vector[n_agd_rows] log_q_idx;
    vector[n_agd_rows] log_p_cmp;
    vector[n_agd_rows] log_q_cmp;
    vector[n_agd_rows] weights;
    for (k in 1:n_agd_rows) {
      vector[n_int] eta_idx = mu_index + X_int[k] * beta;
      vector[n_int] eta_cmp = mu_comparator + X_int[k] * beta;
      log_p_idx[k] = log_mean_event_binary(eta_idx, link);
      log_q_idx[k] = log_mean_nonevent_binary(eta_idx, link);
      log_p_cmp[k] = log_mean_event_binary(eta_cmp, link);
      log_q_cmp[k] = log_mean_nonevent_binary(eta_cmp, link);
      weights[k] = n_agd[k];
    }
    log_p_index_comparator = log_weighted_mean_exp_vec(log_p_idx, weights);
    log_q_index_comparator = log_weighted_mean_exp_vec(log_q_idx, weights);
    log_p_comparator_comparator = log_weighted_mean_exp_vec(log_p_cmp, weights);
    log_q_comparator_comparator = log_weighted_mean_exp_vec(log_q_cmp, weights);
    p_index_comparator = exp(log_p_index_comparator);
    p_comparator_comparator = exp(log_p_comparator_comparator);
  }

  // LOR is a marginal odds ratio computed from response-scale population
  // probabilities, not generally a conditional linear-predictor contrast.
  lor_index = (log_p_index_index - log_q_index_index) -
              (log_p_comparator_index - log_q_comparator_index);
  lor_comparator = (log_p_index_comparator - log_q_index_comparator) -
                   (log_p_comparator_comparator - log_q_comparator_comparator);
  rd_index = exp_difference(log_p_index_index, log_p_comparator_index);
  rd_comparator = exp_difference(log_p_index_comparator,
                                 log_p_comparator_comparator);
  rr_index = exp(log_p_index_index - log_p_comparator_index);
  rr_comparator = exp(log_p_index_comparator - log_p_comparator_comparator);

  // Pointwise log-likelihoods keep IPD observations and AgD rows separate.
  // This is the contract used by loo(), waic(), and DIC helpers.
  for (i in 1:n_ipd)
    log_lik_ipd[i] = bernoulli_link_lpmf(y_ipd[i] | eta_ipd[i], link);

  // AgD log-likelihood mirrors the model block for row-level diagnostics.
  for (k in 1:n_agd_rows) {
    vector[n_int] eta_int = mu_comparator + X_int[k] * beta;
    log_lik_agd[k] = integrated_binomial_lpmf(r_agd[k] | n_agd[k], eta_int, link);
  }
}
