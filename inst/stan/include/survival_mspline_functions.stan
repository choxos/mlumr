// M-spline baseline-hazard helpers for the flexible survival models
// (mlumr_survival_mspline_{spfa,relaxed}.stan). The baseline hazard is
//   h0(t) = sum_j scoef_j M_j(t),   H0(t) = sum_j scoef_j I_j(t),
// with scoef a simplex (softmax of an RW1-smoothed log-ratio vector). Piecewise
// exponential is the degree-0 special case (basis built in R via splines2).
//
// Predictions take precomputed baseline values: `h0cum` = H0(t) = I(t) * scoef
// and `h0haz` = h0(t) = M(t) * scoef. Survival is S(t|x) = exp(-H0(t) exp(eta)).

// Combine the baseline cumulative hazard and linear predictor before
// exponentiating. The explicit zero branch preserves H(0) = 0 without forming
// 0 * Inf when eta is large.
real mspline_cumhaz(real h0cum, real eta) {
  if (h0cum == 0) return 0;
  return exp(log(h0cum) + eta);
}

// Status-aware log likelihood conditional on delayed entry. The three baseline
// cumulative-hazard increments are formed from differences of I-basis rows
// before the dot product, avoiding cancellation between large cumulative
// hazards. Each is non-negative; callers clamp only floating-point roundoff.
real mspline_ll_status(real dH_entry_time, real dH_entry_start,
                       real dH_start_time, real h0haz, real eta, int status) {
  real log_ch_entry_time = dH_entry_time == 0
                           ? negative_infinity()
                           : log(dH_entry_time) + eta;
  if (status == 0) return -exp(log_ch_entry_time);
  if (status == 1)
    return log(h0haz) + eta - exp(log_ch_entry_time);
  if (status == 2) return log1m_exp_neg_exp(log_ch_entry_time);
  {
    real log_ch_entry_start = dH_entry_start == 0
                              ? negative_infinity()
                              : log(dH_entry_start) + eta;
    real log_ch_start_time = dH_start_time == 0
                             ? negative_infinity()
                             : log(dH_start_time) + eta;
    return -exp(log_ch_entry_start) + log1m_exp_neg_exp(log_ch_start_time);
  }
}

// Population-standardized survival at one time: mean over linear predictors of
// exp(-H0(t) exp(eta_i)).
real mspline_mean_surv(real h0cum, vector eta) {
  int n = num_elements(eta);
  vector[n] log_s;
  for (i in 1:n) log_s[i] = -mspline_cumhaz(h0cum, eta[i]);
  return exp(log_sum_exp(log_s) - log(n));
}

// Population-standardized log survival at one time, in log space:
//   log S-bar(t) = log_sum_exp_i(-H0(t) exp(eta_i)) - log(n).
// Keeps the marginal cumulative hazard (-log S-bar) finite where mean(S)
// underflows to 0 in the tail.
real mspline_log_mean_surv(real h0cum, vector eta) {
  int n = num_elements(eta);
  vector[n] log_s;
  for (i in 1:n) log_s[i] = -mspline_cumhaz(h0cum, eta[i]);
  return log_sum_exp(log_s) - log(n);
}

// Population-standardized log hazard at one time, evaluated in log space:
//   log h-bar(t) = log( E_x[h(t|x) S(t|x)] / E_x[S(t|x)] )
//               = log_sum_exp_i( log h0haz + eta_i - H0 exp(eta_i) )
//                 - log_sum_exp_i( -H0 exp(eta_i) ).
// (The 1/n averaging factors cancel.) This stays finite where the natural-scale
// ratio mean(hs)/mean(s) underflows to 0/0 deep in the tail (large H0 exp(eta)),
// in the same log-space form as the parametric log_mean_haz, so the M-spline /
// piecewise-exponential marginal log hazard ratio is well defined out to extreme
// times.
real mspline_log_mean_haz(real h0cum, real h0haz, vector eta) {
  int n = num_elements(eta);
  vector[n] log_s_relative;
  real eta_min = min(eta);
  if (h0haz == 0) return negative_infinity();
  for (i in 1:n) {
    real eta_diff = eta[i] - eta_min;
    if (h0cum == 0 || eta_diff == 0) {
      log_s_relative[i] = 0;
    } else {
      // log{H0 [exp(eta_i) - exp(eta_min)]}, evaluated without forming
      // either exponential. Centering on the longest-surviving profile keeps
      // the final log-sum-exp ratio finite deep in the tail.
      real log_ch_diff = log(h0cum) + eta[i] + log1m_exp(-eta_diff);
      log_s_relative[i] = -exp(log_ch_diff);
    }
  }
  return log(h0haz)
         + log_sum_exp(eta + log_s_relative)
         - log_sum_exp(log_s_relative);
}

// Population-standardized hazard h-bar(t) = E_x[h(t|x) S(t|x)] / E_x[S(t|x)],
// computed via the log-space form above for tail stability (identical to
// mean(hs)/mean(s) in ordinary ranges).
real mspline_mean_haz(real h0cum, real h0haz, vector eta) {
  return exp(mspline_log_mean_haz(h0cum, h0haz, eta));
}

// RMST by the trapezoidal rule on the standardized survival curve, using the
// integrated-basis matrix `ib_grid` (rows = grid times) and spline weights.
real mspline_rmst(matrix ib_grid, vector grid_times, vector scoef, vector eta) {
  int g = num_elements(grid_times);
  vector[g] sbar;
  real area = 0;
  for (p in 1:g) sbar[p] = mspline_mean_surv(dot_product(ib_grid[p], scoef), eta);
  for (p in 2:g) area += 0.5 * (grid_times[p] - grid_times[p - 1]) * (sbar[p] + sbar[p - 1]);
  return area;
}
