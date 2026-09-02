// ML-UMR: parametric survival (time-to-event) with the Shared Prognostic
// Factor Assumption (SPFA -- one prognostic coefficient vector shared across
// treatments). Unanchored comparison of single-arm trials.
//
//   IPD  (index treatment)     : individual event/censoring times + covariates.
//   AgD  (comparator treatment): reconstructed pseudo-IPD (event/censoring
//                                times from a digitized KM curve) whose
//                                likelihood is integrated over the comparator
//                                covariate distribution (integration points).
//
// 9 parametric distributions via integer `dist` dispatch (see
// include/survival_functions.stan). Baseline-population effects, survival
// curves, hazards and RMST are produced as generated quantities.
//
// The (intercepts + covariates) design is supplied as `Xq_*` (raw centered
// design when qr = 0, scaled thin-QR factor Q when qr = 1); coefficients are
// recovered as `allbeta = R_inv * beta_tilde`. See R/mlumr.R (.mlumr_qr_design()) for the centering and QR design construction.

functions {
#include include/priors_functions.stan
#include include/survival_functions.stan
}

data {
  // IPD (index treatment)
  int<lower=1> n_ipd;
  int<lower=1> n_cov;
  matrix[n_ipd, n_cov] X_ipd;            // centered covariates (generated quantities)
  vector<lower=0>[n_ipd] ipd_time;
  vector<lower=0>[n_ipd] ipd_start_time;   // interval-censoring lower bound (else 0)
  vector<lower=0>[n_ipd] ipd_delay_time;   // delayed-entry time (0 = none)
  array[n_ipd] int<lower=0,upper=3> ipd_status;

  // Comparator pseudo-IPD (reconstructed from digitized KM curve)
  int<lower=1> n_agd;
  int<lower=1> n_agd_rows;                  // number of comparator arms
  vector<lower=0>[n_agd] agd_time;
  vector<lower=0>[n_agd] agd_start_time;
  vector<lower=0>[n_agd] agd_delay_time;
  array[n_agd] int<lower=0,upper=3> agd_status;
  array[n_agd] int<lower=1,upper=n_agd_rows> agd_arm;

  // Integration points (comparator covariate distribution, per arm)
  int<lower=1> n_int;
  array[n_agd_rows] matrix[n_int, n_cov] X_int;   // centered covariates (gen. quantities)

  // Combined (intercepts + covariates) design and optional QR reparameterization.
  int<lower=0,upper=1> qr;               // 1 = sample on the QR scale
  int<lower=1> nB;                       // design columns = 2 + n_cov
  matrix[n_ipd, nB] Xq_ipd;             // design for IPD rows (Q if qr = 1)
  array[n_agd_rows] matrix[n_int, nB] Xq_int;     // design for AgD integration rows
  matrix[nB, nB] R_inv;                  // R^{-1} (identity when qr = 0)

  // Distribution + link
  int<lower=1,upper=9> dist;
  int<lower=1,upper=1> link;                // 1 = log (only option for survival)

  // Baseline stratification (the analogue of multinma's aux_by).
  //   1 = one shape parameter shared by both studies (proportional hazards
  //       across studies, the historical behavior);
  //   2 = a separate shape per study, so the marginal hazard ratio is free to
  //       vary with time. The study intercepts stay identified because the
  //       distribution's scale is carried by exp(eta), not by the shape.
  int<lower=1,upper=2> n_strata;

  // HTA prediction grid
  int<lower=1> n_pred_times;
  vector<lower=0>[n_pred_times] pred_times;
  int<lower=2> n_rmst_grid;
  vector<lower=0>[n_rmst_grid] rmst_grid_times;

#include include/priors_hyperparameters.stan
#include include/survival_aux_hyperparameters.stan
}

transformed data {
  int nonexp = (dist != 1 && dist != 4) ? 1 : 0;   // has a shape parameter?
  int is_gengamma = (dist == 9) ? 1 : 0;           // has a second shape?
  int n_int_all = n_agd_rows * n_int;
  // Flatten the per-arm integration points into one comparator-population set.
  matrix[n_int_all, n_cov] X_int_all;
  for (a in 1:n_agd_rows)
    X_int_all[((a - 1) * n_int + 1):(a * n_int)] = X_int[a];
}

parameters {
  // Combined coefficients on the (QR) sampling scale: [mu_index, mu_comparator, beta].
  vector[nB] beta_tilde;
  matrix<lower=0>[nonexp, n_strata] aux_raw;
  matrix<lower=0>[is_gengamma, n_strata] aux2_raw;
}

transformed parameters {
  // Recover original-scale coefficients (affine map, no Jacobian needed).
  vector[nB] allbeta = qr ? R_inv * beta_tilde : beta_tilde;
  real mu_index = allbeta[1];
  real mu_comparator = allbeta[2];
  vector[n_cov] beta = segment(allbeta, 3, n_cov);
  vector[n_ipd] eta_ipd = Xq_ipd * beta_tilde;
  // Stratum 1 is the index study; `n_strata` indexes the comparator, so with
  // n_strata = 1 both views are the same parameter and the model reduces to the
  // unstratified one exactly.
  real aux_val      = nonexp ? aux_raw[1, 1] : 1.0;
  real aux_val_cmp  = nonexp ? aux_raw[1, n_strata] : 1.0;
  real aux2_val     = is_gengamma ? aux2_raw[1, 1] : 1.0;
  real aux2_val_cmp = is_gengamma ? aux2_raw[1, n_strata] : 1.0;
}

model {
  // Priors
  target += log_prior_scalar(mu_index, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_scalar(mu_comparator, prior_intercept_mean, prior_intercept_sd,
                             prior_intercept_dist, prior_intercept_df);
  target += log_prior_vector(beta, prior_beta_mean, prior_beta_sd,
                             prior_beta_dist, prior_beta_df);
  for (s in 1:n_strata) {
    if (nonexp)
      target += log_prior_sigma(aux_raw[1, s], prior_aux_location, prior_aux_scale,
                                prior_aux_dist, prior_aux_df);
    if (is_gengamma)
      target += log_prior_sigma(aux2_raw[1, s], prior_aux2_location, prior_aux2_scale,
                                prior_aux2_dist, prior_aux2_df);
  }

  // IPD likelihood (index treatment), status-aware.
  for (i in 1:n_ipd)
    target += surv_ll_status(dist, ipd_time[i], ipd_start_time[i],
                             ipd_delay_time[i], ipd_status[i],
                             eta_ipd[i], aux_val, aux2_val);

  // Comparator pseudo-IPD likelihood, integrated over the comparator covariate
  // distribution: for pseudo-individual j in arm a,
  //   log( (1/n_int) * sum_m exp( ll(t_j | eta over X_int[a]) ) ).
  {
    // eta over the integration grid is the (possibly QR-rotated) comparator
    // design times beta_tilde, recomputed for every pseudo-individual.
    vector[n_agd] agd_ll;
    for (j in 1:n_agd) {
      int a = agd_arm[j];
      vector[n_int] eta = Xq_int[a] * beta_tilde;
      vector[n_int] ll;
      for (m in 1:n_int)
        ll[m] = surv_ll_status(dist, agd_time[j], agd_start_time[j],
                               agd_delay_time[j], agd_status[j],
                               eta[m], aux_val_cmp, aux2_val_cmp);
      agd_ll[j] = log_sum_exp(ll) - log(n_int);
    }
    target += sum(agd_ll);
  }
}

generated quantities {
  // Treatment effect: log hazard ratio (PH, dist 1-3) or log time-ratio
  // (AFT, dist 4-9). delta_* are the population-standardized marginal effects.
  // delta_conditional is the intercept contrast mu_index - mu_comparator. It is
  // a conditional log HR / log TR only when the baseline cancels, which needs
  // n_strata == 1 as well as the shared coefficient vector SPFA already gives;
  // under the default n_strata == 2 the two arms carry different shapes and it
  // is an intercept contrast under this model's normalization, not a treatment
  // effect.
  real delta_conditional = mu_index - mu_comparator;
  real delta_index;
  real delta_comparator;

  // Absolute predictions (treatment x population) at pred_times.
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
  // Marginal log hazard ratio (index vs comparator treatment) by population,
  // formed in log space so it stays finite where the natural-scale hazard
  // underflows (predict type "loghr").
  array[n_pred_times] real loghr_index;
  array[n_pred_times] real loghr_comparator;

  // Restricted mean survival time + differences.
  real rmst_index_index;
  real rmst_comparator_index;
  real rmst_index_comparator;
  real rmst_comparator_comparator;
  real rmst_diff_index;
  real rmst_diff_comparator;

  // Pointwise log-likelihood (loo / waic / DIC contract).
  vector[n_ipd] log_lik_ipd;
  vector[n_agd] log_lik_agd;

  {
    // Population linear predictors for the 4 treatment x population cells,
    // built from the recovered original-scale coefficients and centered X.
    vector[n_ipd] eta_idx_ip = mu_index + X_ipd * beta;          // index trt, index pop
    vector[n_ipd] eta_cmp_ip = mu_comparator + X_ipd * beta;     // comp trt, index pop
    vector[n_int_all] eta_idx_cp = mu_index + X_int_all * beta;       // index trt, comp pop
    vector[n_int_all] eta_cmp_cp = mu_comparator + X_int_all * beta;  // comp trt, comp pop

    if (dist <= 3) {  // PH: marginal log HR
      delta_index = log_sum_exp(eta_idx_ip) - log_sum_exp(eta_cmp_ip);
      delta_comparator = log_sum_exp(eta_idx_cp) - log_sum_exp(eta_cmp_cp);
    } else {          // AFT: conditional log time-ratio (covariate terms cancel under SPFA)
      delta_index = mean(eta_idx_ip) - mean(eta_cmp_ip);
      delta_comparator = mean(eta_idx_cp) - mean(eta_cmp_cp);
    }

    for (p in 1:n_pred_times) {
      real t = pred_times[p];
      surv_index_index[p]           = mean_surv(dist, t, eta_idx_ip, aux_val, aux2_val);
      surv_comparator_index[p]      = mean_surv(dist, t, eta_cmp_ip, aux_val_cmp, aux2_val_cmp);
      surv_index_comparator[p]      = mean_surv(dist, t, eta_idx_cp, aux_val, aux2_val);
      surv_comparator_comparator[p] = mean_surv(dist, t, eta_cmp_cp, aux_val_cmp, aux2_val_cmp);
      real lmh_ii = log_mean_haz(dist, t, eta_idx_ip, aux_val, aux2_val);
      real lmh_ci = log_mean_haz(dist, t, eta_cmp_ip, aux_val_cmp, aux2_val_cmp);
      real lmh_ic = log_mean_haz(dist, t, eta_idx_cp, aux_val, aux2_val);
      real lmh_cc = log_mean_haz(dist, t, eta_cmp_cp, aux_val_cmp, aux2_val_cmp);
      haz_index_index[p]            = exp(lmh_ii);
      haz_comparator_index[p]       = exp(lmh_ci);
      haz_index_comparator[p]       = exp(lmh_ic);
      haz_comparator_comparator[p]  = exp(lmh_cc);
      loghr_index[p]                = lmh_ii - lmh_ci;
      loghr_comparator[p]           = lmh_ic - lmh_cc;
      cumhaz_index_index[p]            = -log_mean_surv(dist, t, eta_idx_ip, aux_val, aux2_val);
      cumhaz_comparator_index[p]       = -log_mean_surv(dist, t, eta_cmp_ip, aux_val_cmp, aux2_val_cmp);
      cumhaz_index_comparator[p]       = -log_mean_surv(dist, t, eta_idx_cp, aux_val, aux2_val);
      cumhaz_comparator_comparator[p]  = -log_mean_surv(dist, t, eta_cmp_cp, aux_val_cmp, aux2_val_cmp);
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
    // Only for PROPORTIONAL-HAZARDS distributions that actually have a shape to
    // stratify. For AFT (dist > 3) delta_* is a location contrast and stays a
    // time-ratio functional, so replacing it with a hazard ratio would relabel
    // the estimand; for the exponential (nonexp = 0) there is no shape, so
    // stratification does not change the baseline and the closed form is exact.
    if (n_strata > 1 && nonexp && dist <= 3) {
      delta_index      = loghr_index[1];
      delta_comparator = loghr_comparator[1];
    }

    rmst_index_index           = rmst_param(dist, rmst_grid_times, eta_idx_ip, aux_val, aux2_val);
    rmst_comparator_index      = rmst_param(dist, rmst_grid_times, eta_cmp_ip, aux_val_cmp, aux2_val_cmp);
    rmst_index_comparator      = rmst_param(dist, rmst_grid_times, eta_idx_cp, aux_val, aux2_val);
    rmst_comparator_comparator = rmst_param(dist, rmst_grid_times, eta_cmp_cp, aux_val_cmp, aux2_val_cmp);
    rmst_diff_index      = rmst_index_index - rmst_comparator_index;
    rmst_diff_comparator = rmst_index_comparator - rmst_comparator_comparator;

    for (i in 1:n_ipd)
      log_lik_ipd[i] = surv_ll_status(dist, ipd_time[i], ipd_start_time[i],
                                      ipd_delay_time[i], ipd_status[i],
                                      eta_ipd[i], aux_val, aux2_val);
    {
      for (j in 1:n_agd) {
        int a = agd_arm[j];
        vector[n_int] eta = Xq_int[a] * beta_tilde;
        vector[n_int] ll;
        for (m in 1:n_int)
          ll[m] = surv_ll_status(dist, agd_time[j], agd_start_time[j],
                                 agd_delay_time[j], agd_status[j],
                                 eta[m], aux_val_cmp, aux2_val_cmp);
        log_lik_agd[j] = log_sum_exp(ll) - log(n_int);
      }
    }
  }
}
