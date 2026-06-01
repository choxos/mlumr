// Shared survival likelihood / hazard / standardization helpers for the
// ML-UMR parametric survival models (mlumr_survival_{spfa,relaxed}.stan).
//
// Distribution codes (integer `dist`, multinma-style dispatch):
//   1 = Exponential (PH)   4 = Exponential (AFT)   7 = Log-logistic (AFT)
//   2 = Weibull (PH)       5 = Weibull (AFT)        8 = Gamma (AFT)
//   3 = Gompertz (PH)      6 = Log-normal (AFT)     9 = Generalized Gamma (AFT)
//
// `aux` is the (positive) shape/scale parameter where one exists; `aux2` is the
// second shape for the generalized gamma. For exponential families they are
// unused placeholders (value 1).
//
// Censoring status codes (Surv-derived; see R/survival.R .get_surv_data()):
//   0 = right-censored   1 = event   2 = left-censored   3 = interval-censored
// For interval censoring `time` is the upper bound and `start_time` the lower
// bound. `delay_time > 0` flags left truncation (delayed entry).

// log of the regularized upper incomplete gamma Q(k, x), evaluated entirely on
// the log scale by Legendre's continued fraction:
//
//   Q(k, x) = exp(-x + k log x - lgamma(k)) * CF(k, x),
//   CF(k, x) = 1 / (x+1-k - 1(1-k)/(x+3-k - 2(2-k)/(x+5-k - ...))),
//
// where CF is an O(1/x) quantity. It is the exponential prefactor that
// underflows, not the continued fraction, so keeping the prefactor as a log
// leaves the result finite wherever log Q(k, x) is itself representable.
//
// Used only beyond k + max(1, sqrt(k)), where the fraction is safely in its
// tail regime. Closer to a large k, the native incomplete-gamma function is
// more reliable.
real log_gamma_q_cf_factor(real k, real x) {
  real tiny = 1e-300;
  real b = x + 1 - k;
  real c = inv(tiny);
  real d = inv(b);
  real h = d;
  for (i in 1 : 300) {
    real an = -i * (i - k);
    real del;
    b += 2;
    d = an * d + b;
    if (abs(d) < tiny) {
      d = tiny;
    }
    c = b + an / c;
    if (abs(c) < tiny) {
      c = tiny;
    }
    d = inv(d);
    del = d * c;
    h *= del;
    if (abs(del - 1) < 1e-14) {
      break;
    }
  }
  return log(h);
}

real log_gamma_q_cf(real k, real x) {
  return -x + k * log(x) - lgamma(k) + log_gamma_q_cf_factor(k, x);
}

// Log of the regularized lower incomplete gamma P(k, x), using its convergent
// series without first forming x. This keeps left-tail probabilities finite
// when log(x) is representable but x itself underflows to zero.
real log_gamma_p_series(real k, real log_x) {
  real log_term = -log(k);
  real log_total = log_term;
  for (i in 1 : 300) {
    log_term += log_x - log(k + i);
    log_total = log_sum_exp(log_total, log_term);
    if (exp(log_term - log_total) < 1e-14) {
      break;
    }
  }
  return -exp(log_x) + k * log_x - lgamma(k) + log_total;
}

// Generalized Gamma log density (Lawless parameterization).
real gengamma_lpdf(real y, real mu, real sigma, real k) {
  real Q = pow(k, -0.5);
  real z = Q * (log(y) - mu) / sigma;
  real log_w = log(k) + z;
  return -log(sigma) - log(y) - 0.5 * log(k) * (1 - 2 * k)
         + k * z - exp(log_w) - lgamma(k);
}

// Log upper standard-normal tail, log Phi(-z).
//
// Written through the symmetry Phi(-z) = 1 - Phi(z) rather than by calling
// std_normal_lccdf() directly, because Stan's two normal tail functions are not
// equally well conditioned: std_normal_lccdf(z) already carries a relative error
// of 5e-4 at z = 8 and UNDERFLOWS TO -inf at z = 8.5, where the true value is a
// perfectly representable -39.2, while std_normal_lcdf() is exact to 2.5e-16 out
// to z = -300. Using the accurate one on the reflected argument therefore costs
// nothing and removes a band in which the log-normal likelihood was silently
// truncated: a right-censored observation whose (log t - eta) / sigma landed
// above 8.5 contributed -inf and the proposal was rejected, and the marginal
// hazard generated quantities became inf or NaN. Verified against the reference
// normal tail over z in [1, 300]: maximum relative error 2.7e-16.
real log_std_normal_surv(real z) {
  return std_normal_lcdf(-z);
}

// Log inverse Mills ratio log[phi(z) / Phi(-z)], the standard-normal hazard.
// The infinite-z limit is taken explicitly: both terms are -inf there and their
// difference would be NaN, whereas the hazard diverges. An infinite z is
// reachable whenever a tiny scale parameter divides the standardized time.
real log_std_normal_hazard(real z) {
  if (is_inf(z) && z > 0) return positive_infinity();
  return std_normal_lpdf(z) - log_std_normal_surv(z);
}

real log_gamma_cdf_from_log_x(real k, real log_x) {
  real x = exp(log_x);
  if (is_inf(x)) return 0;
  if (x <= k + fmax(1, sqrt(k))) {
    real p = gamma_p(k, x);
    if (p > 0) return log(p);
    return log_gamma_p_series(k, log_x);
  }
  return log1m_exp(log_gamma_q_cf(k, x));
}

real log_gamma_surv_from_log_x(real k, real log_x) {
  real x = exp(log_x);
  if (is_inf(x)) return negative_infinity();
  if (x > k + fmax(1, sqrt(k))) return log_gamma_q_cf(k, x);
  return log(gamma_q(k, x));
}

// Log survival function log S(t | eta) for a single observation.
real log_surv_scalar(int dist, real t, real eta, real aux, real aux2) {
  if (dist == 1) return -exp(log(t) + eta);                         // Exp PH
  else if (dist == 2) return -exp(aux * log(t) + eta);             // Weibull PH
  else if (dist == 3) return -exp(eta - log(aux) + aux * t + log1m_exp(-aux * t)); // Gompertz PH
  else if (dist == 4) return -exp(log(t) - eta);                    // Exp AFT
  else if (dist == 5) return -exp(aux * (log(t) - eta));           // Weibull AFT
  else if (dist == 6) return log_std_normal_surv((log(t) - eta) / aux); // Log-normal
  else if (dist == 7) return -log1p_exp(aux * (log(t) - eta));     // Log-logistic
  else if (dist == 8) return log_gamma_surv_from_log_x(aux, log(t) - eta); // Gamma
  else {                                                            // Gen. Gamma
    real Q = inv(sqrt(aux2));
    // Form the incomplete-gamma argument in LOG space. Building `w` directly
    // overflows to +inf whenever Q * (log t - eta) / aux is large, which happens
    // routinely at random inits (small `aux` divides the exponent) and killed
    // whole chains with
    //   "boost::math::tgamma: Series evaluation exceeded 1000000 iterations".
    real log_w = Q * (log(t) - eta) / aux + log(aux2);
    return log_gamma_surv_from_log_x(aux2, log_w);
  }
}

// Closed-form log hazard log h(t | eta); returns 0 for dists handled via lpdf
// (6, 8, 9) -- callers needing those use log_haz_full().
real log_haz_scalar(int dist, real t, real eta, real aux, real aux2) {
  if (dist == 1) return eta;                                        // Exp PH
  else if (dist == 2) return log(aux) + (aux * log(t) + eta) - log(t); // Weibull PH
  else if (dist == 3) return eta + aux * t;                         // Gompertz PH
  else if (dist == 4) return -eta;                                  // Exp AFT
  else if (dist == 5) return log(aux) + aux * (log(t) - eta) - log(t); // Weibull AFT
  else if (dist == 7) return log(aux) - log(t)
                              - log1p_exp(-aux * (log(t) - eta));   // Log-logistic
  else return 0;                                                    // 6, 8, 9 via lpdf
}

// Log hazard for all distributions (log h = log f - log S for the lpdf-based
// families), written so the tail cancellation between those two terms is taken
// analytically rather than numerically. This is live likelihood code: it is the
// event contribution under delayed entry in surv_ll_status(), and the numerator
// of the population-standardized hazard in log_mean_haz().
real log_haz_full(int dist, real t, real eta, real aux, real aux2) {
  if (dist == 6) {
    real z = (log(t) - eta) / aux;
    return log_std_normal_hazard(z) - log(aux) - log(t);
  } else if (dist == 8) {
    real log_z = log(t) - eta;
    real z = exp(log_z);
    if (is_inf(z)) return -eta;
    if (z > aux + fmax(1, sqrt(aux)))
      return -log(t) - log_gamma_q_cf_factor(aux, z);
    return (aux - 1) * log_z - eta - z - lgamma(aux)
           - log_gamma_surv_from_log_x(aux, log_z);
  } else if (dist == 9) {
    real z = inv(sqrt(aux2)) * (log(t) - eta) / aux;
    real log_w = log(aux2) + z;
    real w = exp(log_w);
    if (is_inf(w))
      return -log(aux) - log(t) + 0.5 * log(aux2) + z;
    if (w > aux2 + fmax(1, sqrt(aux2)))
      return -log(aux) - log(t) - 0.5 * log(aux2)
             - log_gamma_q_cf_factor(aux2, w);
    return gengamma_lpdf(t | eta, aux, aux2)
           - log_gamma_surv_from_log_x(aux2, log_w);
  }
  else
    return log_haz_scalar(dist, t, eta, aux, aux2);
}

// Log density evaluated without forming log(h) + log(S). In deep tails those
// two terms can be +inf and -inf even when the density has the well-defined
// limiting value zero. This helper is shared by the event likelihood and the
// numerator of the marginal hazard.
real log_density_scalar(int dist, real t, real eta, real aux, real aux2) {
  real log_t = log(t);
  if (dist <= 5) {
    real log_ch;
    if (dist == 1) log_ch = log_t + eta;
    else if (dist == 2) log_ch = aux * log_t + eta;
    else if (dist == 3)
      log_ch = eta - log(aux) + aux * t + log1m_exp(-aux * t);
    else if (dist == 4) log_ch = log_t - eta;
    else log_ch = aux * (log_t - eta);
    if (log_ch > 700) return negative_infinity();
    return log_haz_scalar(dist, t, eta, aux, aux2) - exp(log_ch);
  } else if (dist == 6) {
    real z = (log_t - eta) / aux;
    return -0.5 * square(z) - log(aux) - log_t - 0.5 * log(2 * pi());
  } else if (dist == 7) {
    real z = aux * (log_t - eta);
    if (z >= 0)
      return log(aux) - log_t - z - 2 * log1p_exp(-z);
    return log(aux) - log_t + z - 2 * log1p_exp(z);
  } else if (dist == 8) {
    real log_z = log_t - eta;
    if (log_z > 700) return negative_infinity();
    return (aux - 1) * log_z - eta - exp(log_z) - lgamma(aux);
  } else {
    real Q = inv(sqrt(aux2));
    real z = Q * (log_t - eta) / aux;
    real log_w = log(aux2) + z;
    if (log_w > 700) return negative_infinity();
    return -log(aux) - log_t - 0.5 * log(aux2) * (1 - 2 * aux2)
           + aux2 * z - exp(log_w) - lgamma(aux2);
  }
}

real log1m_exp_neg_exp(real log_h) {
  if (log_h < -20) {
    real h = exp(log_h);
    return log_h + log1p(-0.5 * h + square(h) / 6);
  }
  if (log_h > 700) return 0;
  return log1m_exp(-exp(log_h));
}

// Log CDF evaluated directly in the tail where 1 - S rounds to zero.
real log_cdf_scalar(int dist, real t, real eta, real aux, real aux2) {
  if (dist == 1) {
    real log_h = log(t) + eta;
    return log1m_exp_neg_exp(log_h);
  } else if (dist == 2) {
    real log_h = aux * log(t) + eta;
    return log1m_exp_neg_exp(log_h);
  } else if (dist == 3) {
    real log_h = eta - log(aux) + aux * t + log1m_exp(-aux * t);
    return log1m_exp_neg_exp(log_h);
  } else if (dist == 4) {
    real log_h = log(t) - eta;
    return log1m_exp_neg_exp(log_h);
  } else if (dist == 5) {
    real log_h = aux * (log(t) - eta);
    return log1m_exp_neg_exp(log_h);
  } else if (dist == 6) {
    // std_normal_lcdf() is the well-conditioned member of Stan's normal tail
    // pair (see log_std_normal_surv above), and the log CDF is exactly what it
    // computes, so no reflection is needed here.
    return std_normal_lcdf((log(t) - eta) / aux);
  } else if (dist == 7) {
    return -log1p_exp(-aux * (log(t) - eta));
  } else if (dist == 8) {
    return log_gamma_cdf_from_log_x(aux, log(t) - eta);
  } else {
    real log_w = log(aux2) + inv(sqrt(aux2)) * (log(t) - eta) / aux;
    return log_gamma_cdf_from_log_x(aux2, log_w);
  }
}

// Log cumulative hazard, for the five families whose H(t) has a closed form
// that survives on the log scale. Only these are ever asked for: the sole
// caller, log_cumhaz_diff(), is itself reached only under `dist <= 5` in
// log_surv_increment(). The final branch is Weibull AFT (dist 5); a dist above
// 5 would be a programming error rather than a supported input, so it is
// rejected instead of silently returning a Weibull-AFT value.
real log_cumhaz_scalar(int dist, real t, real eta, real aux) {
  if (dist == 1) return log(t) + eta;
  if (dist == 2) return aux * log(t) + eta;
  if (dist == 3)
    return eta - log(aux) + aux * t + log1m_exp(-aux * t);
  if (dist == 4) return log(t) - eta;
  if (dist == 5) return aux * (log(t) - eta);
  reject("log_cumhaz_scalar() has no closed form for dist = ", dist);
}

real log_expm1_from_log_x(real log_x) {
  if (log_x < -10) {
    real x = exp(log_x);
    return log_x + log1p(0.5 * x + square(x) / 6);
  }
  if (log_x > 700) return exp(log_x);
  return log(expm1(exp(log_x)));
}

real log_cumhaz_diff(int dist, real t_upper, real t_lower, real eta,
                     real aux) {
  real dt = t_upper - t_lower;
  if (dt == 0) return negative_infinity();
  if (dist == 1) return eta + log(dt);
  if (dist == 4) return -eta + log(dt);
  if (dist == 3) {
    real log_ax = log(aux) + log(dt);
    return eta - log(aux) + aux * t_lower + log_expm1_from_log_x(log_ax);
  }
  if (t_lower == 0) return log_cumhaz_scalar(dist, t_upper, eta, aux);
  {
    real log_ratio = log1p(dt / t_lower);
    real log_power_diff = aux * log(t_lower)
                          + log_expm1_from_log_x(log(aux) + log(log_ratio));
    if (dist == 2) return eta + log_power_diff;
    return -aux * eta + log_power_diff; // Weibull AFT
  }
}

// log S(upper) - log S(lower), with a cumulative-hazard difference for the
// five exponential/Weibull/Gompertz families.
real log_surv_increment(int dist, real t_upper, real t_lower, real eta,
                        real aux, real aux2) {
  if (dist <= 5)
    return -exp(log_cumhaz_diff(dist, t_upper, t_lower, eta, aux));
  if (t_lower == 0)
    return log_surv_scalar(dist, t_upper, eta, aux, aux2);
  {
    real log_ratio = log1p((t_upper - t_lower) / t_lower);
    if (dist == 6) {
      real z_lower = (log(t_lower) - eta) / aux;
      real dz = log_ratio / aux;
      real z_upper = z_lower + dz;
      if (z_lower > 5) {
        return -0.5 * dz * (z_upper + z_lower)
               - (log_std_normal_hazard(z_upper)
                  - log_std_normal_hazard(z_lower));
      }
    } else if (dist == 7) {
      real z_lower = aux * (log(t_lower) - eta);
      if (z_lower > 0) {
        real dz = aux * log_ratio;
        real z_upper = z_lower + dz;
        return -dz - (log1p_exp(-z_upper) - log1p_exp(-z_lower));
      }
    } else {
      real shape = dist == 8 ? aux : aux2;
      real log_x_lower;
      real dlog_x;
      if (dist == 8) {
        log_x_lower = log(t_lower) - eta;
        dlog_x = log_ratio;
      } else {
        real Q = inv(sqrt(aux2));
        log_x_lower = log(aux2) + Q * (log(t_lower) - eta) / aux;
        dlog_x = Q * log_ratio / aux;
      }
      if (log_x_lower > log(shape + fmax(1, sqrt(shape)))) {
        real log_dx = log_x_lower + log(expm1(dlog_x));
        real dx = exp(log_dx);
        if (is_inf(exp(log_x_lower)))
          return -dx + (shape - 1) * dlog_x;
        {
          real x_lower = exp(log_x_lower);
          real x_upper = exp(log_x_lower + dlog_x);
          return -dx + shape * dlog_x
                 + log_gamma_q_cf_factor(shape, x_upper)
                 - log_gamma_q_cf_factor(shape, x_lower);
        }
      }
    }
  }
  return log_surv_scalar(dist, t_upper, eta, aux, aux2)
         - log_surv_scalar(dist, t_lower, eta, aux, aux2);
}

real log_interval_prob_scalar(int dist, real t_upper, real t_lower, real eta,
                              real aux, real aux2) {
  real log_cdf_upper = log_cdf_scalar(dist, t_upper, eta, aux, aux2);
  if (log_cdf_upper < -0.6931471805599453)
    return log_diff_exp(log_cdf_upper,
                        log_cdf_scalar(dist, t_lower, eta, aux, aux2));
  return log_surv_scalar(dist, t_lower, eta, aux, aux2)
         + log1m_exp(log_surv_increment(dist, t_upper, t_lower, eta, aux, aux2));
}

// Status-aware single-observation log-likelihood with optional delayed entry.
real surv_ll_status(int dist, real time, real start_time, real delay_time,
                    int status, real eta, real aux, real aux2) {
  real l;
  if (status == 0) {            // right-censored: log S(t)
    if (delay_time > 0)
      l = log_surv_increment(dist, time, delay_time, eta, aux, aux2);
    else
      l = log_surv_scalar(dist, time, eta, aux, aux2);
  } else if (status == 1) {     // event: log h(t) + log S(t)  (or log f(t))
    if (delay_time > 0)
      l = log_haz_full(dist, time, eta, aux, aux2)
          + log_surv_increment(dist, time, delay_time, eta, aux, aux2);
    else
      l = log_density_scalar(dist, time, eta, aux, aux2);
  } else if (status == 2) {     // left-censored: event in (entry, t]
    // Under delayed entry this is the conditional probability of an event in
    // (delay, t]. With delay = 0 this reduces to log F(t).
    if (delay_time > 0)
      l = log1m_exp(log_surv_increment(dist, time, delay_time, eta, aux, aux2));
    else
      l = log_cdf_scalar(dist, time, eta, aux, aux2);
  } else {                       // interval-censored: log(S(lower) - S(upper))
    l = log_interval_prob_scalar(dist, time, start_time, eta, aux, aux2);
    if (delay_time > 0)
      l -= log_surv_scalar(dist, delay_time, eta, aux, aux2);
  }
  return l;
}


// Population-standardized (marginal) survival at time t: mean over a set of
// linear predictors of S(t | eta_i). This is S-bar(t) = E_x[S(t|x)].
real mean_surv(int dist, real t, vector eta, real aux, real aux2) {
  int n = num_elements(eta);
  vector[n] s;
  for (i in 1:n) s[i] = exp(log_surv_scalar(dist, t, eta[i], aux, aux2));
  return mean(s);
}

// Population-standardized (marginal) log survival at time t, in log space:
//   log S-bar(t) = log_sum_exp_i(log S(t|eta_i)) - log(n).
// Stays finite where the natural-scale mean(S) underflows to 0 deep in the tail,
// so the marginal cumulative hazard (cumhaz = -log S-bar) does not blow up to
// +inf at extreme prediction times (the same log-space form as log_mean_haz).
real log_mean_surv(int dist, real t, vector eta, real aux, real aux2) {
  int n = num_elements(eta);
  vector[n] log_s;
  for (i in 1:n) log_s[i] = log_surv_scalar(dist, t, eta[i], aux, aux2);
  return log_sum_exp(log_s) - log(n);
}

// Population-standardized (marginal) log hazard at time t:
//   log h-bar(t) = log E_x[f(t|x)] - log E_x[S(t|x)]   (= log of -d/dt log S-bar).
// Stays on the log scale (log_sum_exp of numerator and denominator), so the
// marginal log hazard ratio (predict type "loghr") is finite even where the
// natural-scale hazard underflows to 0 deep in the tail.
real log_mean_haz(int dist, real t, vector eta, real aux, real aux2) {
  int n = num_elements(eta);
  vector[n] log_s;
  vector[n] log_num;
  real max_log_s;
  for (i in 1:n) {
    log_s[i] = log_surv_scalar(dist, t, eta[i], aux, aux2);
  }
  max_log_s = max(log_s);
  if (is_inf(max_log_s)) {
    int best = 1;
    for (i in 2:n) {
      if ((dist <= 3 && eta[i] < eta[best])
          || (dist >= 4 && eta[i] > eta[best])) best = i;
    }
    return log_haz_full(dist, t, eta[best], aux, aux2);
  }
  for (i in 1:n) {
    log_s[i] -= max_log_s;
    log_num[i] = log_haz_full(dist, t, eta[i], aux, aux2) + log_s[i];
    if (is_nan(log_num[i]))
      log_num[i] = log_density_scalar(dist, t, eta[i], aux, aux2)
                   - max_log_s;
  }
  return log_sum_exp(log_num) - log_sum_exp(log_s);
}

// Population-standardized (marginal) hazard at time t:
//   h-bar(t) = E_x[h(t|x) S(t|x)] / E_x[S(t|x)].
// Note: intentionally retained as API surface; not called by any current model
// (predictions use log_mean_haz in log space). Kept for downstream use.
real mean_haz(int dist, real t, vector eta, real aux, real aux2) {
  return exp(log_mean_haz(dist, t, eta, aux, aux2));
}

// Restricted mean survival time over [grid[1], grid[g]] by the trapezoidal
// rule on the standardized survival curve. `grid` should start at 0.
real rmst_param(int dist, vector grid, vector eta, real aux, real aux2) {
  int g = num_elements(grid);
  vector[g] sbar;
  real area = 0;
  for (p in 1:g) sbar[p] = mean_surv(dist, grid[p], eta, aux, aux2);
  for (p in 2:g) area += 0.5 * (grid[p] - grid[p - 1]) * (sbar[p] + sbar[p - 1]);
  return area;
}
