// ML-UMR: flexible-baseline survival, relaxed model with treatment-specific
// prognostic coefficients (allows effect modification). M-spline baseline
// hazard (piecewise exponential = degree 0); proportional hazards on exp(eta).
//
//   h(t|x) = h0(t) exp(eta),  S(t|x) = exp(-H0(t) exp(eta))
//   scoef = softmax(append_row(0, lscoef)), RW1 smoothing prior on lscoef.
//
// The combined (intercepts + treatment-specific covariates) design is supplied
// as `Xq_*` (raw centered design when qr = 0, scaled thin-QR factor Q when
// qr = 1); coefficients are recovered as `allbeta = R_inv * beta_tilde`. See R/mlumr.R (.mlumr_qr_design()) for the centering and QR design construction.

functions {
#include include/priors_functions.stan
#include include/survival_functions.stan
#include include/survival_mspline_functions.stan
}

data {
  // IPD (index treatment)
  int<lower=1> n_ipd;
  int<lower=1> n_cov;
  matrix[n_ipd, n_cov] X_ipd;            // centered covariates (generated quantities)
  int<lower=2> n_scoef;
  matrix[n_ipd, n_scoef] b_ipd;
  matrix[n_ipd, n_scoef] ib_ipd;
  matrix[n_ipd, n_scoef] ib_ipd_start;
  matrix[n_ipd, n_scoef] ib_ipd_delay;
  array[n_ipd] int<lower=0,upper=3> ipd_status;

  // Comparator pseudo-IPD
  int<lower=1> n_agd;
  int<lower=1> n_agd_rows;
  matrix[n_agd, n_scoef] b_agd;
  matrix[n_agd, n_scoef] ib_agd;
  matrix[n_agd, n_scoef] ib_agd_start;
  matrix[n_agd, n_scoef] ib_agd_delay;
  array[n_agd] int<lower=0,upper=3> agd_status;
  array[n_agd] int<lower=1,upper=n_agd_rows> agd_arm;

  // Integration points (comparator covariate distribution, per arm)
  int<lower=1> n_int;
  array[n_agd_rows] matrix[n_int, n_cov] X_int;   // centered covariates (gen. quantities)

  // Combined (intercepts + index/comparator covariates) design and optional QR.
  int<lower=0,upper=1> qr;               // 1 = sample on the QR scale
  int<lower=1> nB;                       // design columns = 2 + 2 * n_cov
  matrix[n_ipd, nB] Xq_ipd;             // design for IPD rows (Q if qr = 1)
  array[n_agd_rows] matrix[n_int, nB] Xq_int;     // design for AgD integration rows
  matrix[nB, nB] R_inv;                  // R^{-1} (identity when qr = 0)

  int<lower=1,upper=1> link;

  // HTA prediction grid
  int<lower=1> n_pred_times;
  vector<lower=0>[n_pred_times] pred_times;
  matrix[n_pred_times, n_scoef] pred_basis;
  matrix[n_pred_times, n_scoef] pred_ibasis;
  int<lower=2> n_rmst_grid;
  vector<lower=0>[n_rmst_grid] rmst_grid_times;
  matrix[n_rmst_grid, n_scoef] rmst_ibasis;
  // Comparator-stratum bases. With `aux_by = ".study"` each study has its
  // own knots over its own observed support, so the comparator arm is
  // evaluated on its own basis. Under n_strata = 1 these are copies of the
  // index matrices and every expression below reduces to the shared model.
  matrix[n_pred_times, n_scoef] pred_basis_cmp;
  matrix[n_pred_times, n_scoef] pred_ibasis_cmp;
  matrix[n_rmst_grid, n_scoef] rmst_ibasis_cmp;

#include include/priors_hyperparameters.stan

  // Fully separate prior for beta_comparator: its own family, degrees of
  // freedom, location and scale, none of them tied to beta_index. Tighten these
  // to regularize the comparator coefficients, which are identified only
  // through the AgD likelihood.
  vector[n_cov] prior_beta_comparator_mean;
  vector<lower=0>[n_cov] prior_beta_comparator_sd;
  int<lower=0,upper=1> prior_beta_comparator_dist;
  real<lower=0> prior_beta_comparator_df;

  // Baseline-hazard stratification (the analogue of multinma's aux_by).
  //   1 = one baseline shape shared by both studies (proportional hazards
  //       across studies, the historical behavior);
  //   2 = a separate shape per study, so the marginal hazard ratio is free to
  //       vary with time. The simplex constraint on each stratum pins
  //       H0_s(T) = 1 at the upper boundary knot (the I-spline basis there is
  //       all ones), which is what keeps the study intercepts identified when
  //       both shapes are free.
  int<lower=1,upper=2> n_strata;

  matrix[n_scoef - 1, n_strata] lscoef_prior_mean;
  matrix<lower=0>[n_scoef - 1, n_strata] lscoef_weights;
  real prior_sigma_smooth_location;
  real<lower=0> prior_sigma_smooth_scale;
  int<lower=0,upper=2> prior_sigma_smooth_dist;
  real<lower=0> prior_sigma_smooth_df;
}

transformed data {
  int n_int_all = n_agd_rows * n_int;
  matrix[n_int_all, n_cov] X_int_all;
  for (a in 1:n_agd_rows)
    X_int_all[((a - 1) * n_int + 1):(a * n_int)] = X_int[a];
}

parameters {
  // Combined coefficients on the (QR) sampling scale:
  // [mu_index, mu_comparator, beta_index, beta_comparator].
  vector[nB] beta_tilde;
  matrix[n_scoef - 1, n_strata] u_lscoef;      // standard-normal RW1 innovations
  vector<lower=0>[n_strata] sigma_smooth;     // one smoothing SD per stratum
}

transformed parameters {
  // Recover original-scale coefficients (affine map, no Jacobian needed).
  vector[nB] allbeta = qr ? R_inv * beta_tilde : beta_tilde;
  real mu_index = allbeta[1];
  real mu_comparator = allbeta[2];
  vector[n_cov] beta_index = segment(allbeta, 3, n_cov);
  vector[n_cov] beta_comparator = segment(allbeta, 3 + n_cov, n_cov);
  vector[n_ipd] eta_ipd = Xq_ipd * beta_tilde;

  // RW1-smoothed spline coefficients, one simplex per stratum.
  matrix[n_scoef - 1, n_strata] lscoef;
  matrix[n_scoef, n_strata] scoef;
  for (s in 1:n_strata) {
    lscoef[, s] = cumulative_sum(col(u_lscoef, s) .* col(lscoef_weights, s))
                    * sigma_smooth[s] + col(lscoef_prior_mean, s);
    scoef[, s] = softmax(append_row(0, col(lscoef, s)));
  }
  // The index study is always stratum 1. `n_strata` indexes the comparator, so
  // with n_strata = 1 both names point at the same shared simplex and every
  // expression below reduces to the unstratified model exactly.
  vector[n_scoef] scoef_idx = col(scoef, 1);
  vector[n_scoef] scoef_cmp = col(scoef, n_strata);

  vector[n_ipd] log_L_ipd;
  {
    vector[n_ipd] h0  = b_ipd * scoef_idx;
    for (i in 1:n_ipd) {
      real dH_et = fmax(dot_product(ib_ipd[i] - ib_ipd_delay[i], scoef_idx), 0);
      real dH_es = fmax(dot_product(ib_ipd_start[i] - ib_ipd_delay[i], scoef_idx), 0);
      real dH_st = fmax(dot_product(ib_ipd[i] - ib_ipd_start[i], scoef_idx), 0);
      log_L_ipd[i] = mspline_ll_status(dH_et, dH_es, dH_st, h0[i],
                                       eta_ipd[i], ipd_status[i]);
    }
  }
}

model {
  target += log_prior_scalar(mu_index, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_scalar(mu_comparator, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_vector(beta_index, prior_beta_mean, prior_beta_sd,
                             prior_beta_dist, prior_beta_df);
  target += log_prior_vector(beta_comparator, prior_beta_comparator_mean,
                             prior_beta_comparator_sd, prior_beta_comparator_dist,
                             prior_beta_comparator_df);
  to_vector(u_lscoef) ~ std_normal();
  for (s in 1:n_strata)
    target += log_prior_sigma(sigma_smooth[s], prior_sigma_smooth_location,
                              prior_sigma_smooth_scale,
                              prior_sigma_smooth_dist, prior_sigma_smooth_df);

  target += sum(log_L_ipd);

  {
    // eta over the integration grid is the comparator design times beta_tilde,
    // recomputed for every pseudo-individual.
    vector[n_agd] agd_ll;
    for (j in 1:n_agd) {
      int a = agd_arm[j];
      vector[n_int] eta = Xq_int[a] * beta_tilde;
      real dH_et = fmax(dot_product(ib_agd[j] - ib_agd_delay[j], scoef_cmp), 0);
      real dH_es = fmax(dot_product(ib_agd_start[j] - ib_agd_delay[j], scoef_cmp), 0);
      real dH_st = fmax(dot_product(ib_agd[j] - ib_agd_start[j], scoef_cmp), 0);
      real h0  = dot_product(b_agd[j], scoef_cmp);
      vector[n_int] ll;
      for (m in 1:n_int) {
        ll[m] = mspline_ll_status(dH_et, dH_es, dH_st, h0, eta[m],
                                  agd_status[j]);
      }
      agd_ll[j] = log_sum_exp(ll) - log(n_int);
    }
    target += sum(agd_ll);
  }
}

generated quantities {
  real delta_conditional = mu_index - mu_comparator;
  real delta_index;
  real delta_comparator;
  vector[n_cov] delta_beta = beta_index - beta_comparator;

  array[n_pred_times] real surv_index_index;
  array[n_pred_times] real surv_comparator_index;
  array[n_pred_times] real surv_index_comparator;
  array[n_pred_times] real surv_comparator_comparator;
  array[n_pred_times] real haz_index_index;
  array[n_pred_times] real haz_comparator_index;
  array[n_pred_times] real haz_index_comparator;
  array[n_pred_times] real haz_comparator_comparator;
  array[n_pred_times] real cumhaz_index_index;
  array[n_pred_times] real cumhaz_comparator_index;
  array[n_pred_times] real cumhaz_index_comparator;
  array[n_pred_times] real cumhaz_comparator_comparator;
  // Marginal log hazard ratio (index vs comparator) per population, emitted
  // directly from log-space marginal hazards so it stays finite in the tail.
  array[n_pred_times] real loghr_index;
  array[n_pred_times] real loghr_comparator;

  real rmst_index_index;
  real rmst_comparator_index;
  real rmst_index_comparator;
  real rmst_comparator_comparator;
  real rmst_diff_index;
  real rmst_diff_comparator;

  vector[n_ipd] log_lik_ipd;
  vector[n_agd] log_lik_agd;

  {
    vector[n_ipd] eta_idx_ip = mu_index + X_ipd * beta_index;
    vector[n_ipd] eta_cmp_ip = mu_comparator + X_ipd * beta_comparator;
    vector[n_int_all] eta_idx_cp = mu_index + X_int_all * beta_index;
    vector[n_int_all] eta_cmp_cp = mu_comparator + X_int_all * beta_comparator;

    delta_index = log_sum_exp(eta_idx_ip) - log_sum_exp(eta_cmp_ip);
    delta_comparator = log_sum_exp(eta_idx_cp) - log_sum_exp(eta_cmp_cp);

    for (p in 1:n_pred_times) {
      // The baseline travels with the TREATMENT, not the population: each
      // study contributes one arm, so transporting the index treatment to the
      // comparator population carries the index study's baseline with it.
      // Under n_strata = 1 the two are the same vector and this is a no-op.
      real H0i = dot_product(pred_ibasis[p], scoef_idx);
      real h0i = dot_product(pred_basis[p], scoef_idx);
      real H0c = dot_product(pred_ibasis_cmp[p], scoef_cmp);
      real h0c = dot_product(pred_basis_cmp[p], scoef_cmp);
      surv_index_index[p]           = mspline_mean_surv(H0i, eta_idx_ip);
      surv_comparator_index[p]      = mspline_mean_surv(H0c, eta_cmp_ip);
      surv_index_comparator[p]      = mspline_mean_surv(H0i, eta_idx_cp);
      surv_comparator_comparator[p] = mspline_mean_surv(H0c, eta_cmp_cp);
      real lmh_ii = mspline_log_mean_haz(H0i, h0i, eta_idx_ip);
      real lmh_ci = mspline_log_mean_haz(H0c, h0c, eta_cmp_ip);
      real lmh_ic = mspline_log_mean_haz(H0i, h0i, eta_idx_cp);
      real lmh_cc = mspline_log_mean_haz(H0c, h0c, eta_cmp_cp);
      haz_index_index[p]            = exp(lmh_ii);
      haz_comparator_index[p]       = exp(lmh_ci);
      haz_index_comparator[p]       = exp(lmh_ic);
      haz_comparator_comparator[p]  = exp(lmh_cc);
      loghr_index[p]      = lmh_ii - lmh_ci;
      loghr_comparator[p] = lmh_ic - lmh_cc;
      cumhaz_index_index[p]            = -mspline_log_mean_surv(H0i, eta_idx_ip);
      cumhaz_comparator_index[p]       = -mspline_log_mean_surv(H0c, eta_cmp_ip);
      cumhaz_index_comparator[p]       = -mspline_log_mean_surv(H0i, eta_idx_cp);
      cumhaz_comparator_comparator[p]  = -mspline_log_mean_surv(H0c, eta_cmp_cp);
    }

    // delta_* is the MARGINAL log hazard ratio at the START of follow-up
    // (the t -> 0 limit), and it is NOT constant in time. With one shared
    // baseline h0(t) cancels from the CONDITIONAL hazard ratio, which is then
    // exactly exp(eta_i - eta_c) for every t; the MARGINAL ratio still drifts,
    // because it weights the covariate distribution by each arm's own survival
    // (E_x[h S] / E_x[S]) and the two risk sets diverge as follow-up proceeds.
    // The closed form below is therefore the marginal ratio at t -> 0, where
    // the two weightings still coincide. With a stratified
    // baseline it does not cancel: the true contrast picks up an extra
    // log(h0_index(t) / h0_comparator(t)) term and is no longer constant. Rather
    // than report a quantity that silently means something different, take the
    // value straight from the time-varying marginal hazards at the first
    // prediction time, which is what loghr_* already computes with the correct
    // per-study baselines.
    if (n_strata > 1) {
      delta_index      = loghr_index[1];
      delta_comparator = loghr_comparator[1];
    }

    rmst_index_index           = mspline_rmst(rmst_ibasis, rmst_grid_times, scoef_idx, eta_idx_ip);
    rmst_comparator_index      = mspline_rmst(rmst_ibasis_cmp, rmst_grid_times, scoef_cmp, eta_cmp_ip);
    rmst_index_comparator      = mspline_rmst(rmst_ibasis, rmst_grid_times, scoef_idx, eta_idx_cp);
    rmst_comparator_comparator = mspline_rmst(rmst_ibasis_cmp, rmst_grid_times, scoef_cmp, eta_cmp_cp);
    rmst_diff_index      = rmst_index_index - rmst_comparator_index;
    rmst_diff_comparator = rmst_index_comparator - rmst_comparator_comparator;

    for (i in 1:n_ipd) log_lik_ipd[i] = log_L_ipd[i];
    {
      for (j in 1:n_agd) {
        int a = agd_arm[j];
        vector[n_int] eta = Xq_int[a] * beta_tilde;
        real dH_et = fmax(dot_product(ib_agd[j] - ib_agd_delay[j], scoef_cmp), 0);
        real dH_es = fmax(dot_product(ib_agd_start[j] - ib_agd_delay[j], scoef_cmp), 0);
        real dH_st = fmax(dot_product(ib_agd[j] - ib_agd_start[j], scoef_cmp), 0);
        real h0  = dot_product(b_agd[j], scoef_cmp);
        vector[n_int] ll;
        for (m in 1:n_int) {
          ll[m] = mspline_ll_status(dH_et, dH_es, dH_st, h0, eta[m],
                                    agd_status[j]);
        }
        log_lik_agd[j] = log_sum_exp(ll) - log(n_int);
      }
    }
  }
}
