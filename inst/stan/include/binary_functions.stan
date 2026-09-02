// Binary inverse-link dispatcher.
// link_code: 1 = logit, 2 = probit, 3 = complementary log-log.
real inv_link_binary(real eta, int link_code) {
  if (link_code == 1) return inv_logit(eta);
  else if (link_code == 2) return Phi(eta);
  else return inv_cloglog(eta);
}

vector inv_link_binary_vec(vector eta, int link_code) {
  int N = num_elements(eta);
  vector[N] result;
  for (i in 1:N) result[i] = inv_link_binary(eta[i], link_code);
  return result;
}

// Exact log event and non-event probabilities. Likelihoods use these helpers
// so extreme predictors are not changed by natural-scale probability bounds.
real log_event_prob_binary(real eta, int link_code) {
  if (link_code == 1) return -log1p_exp(-eta);
  else if (link_code == 2) return std_normal_lcdf(eta);
  else if (eta < -37) return eta;
  else return log1m_exp(-exp(eta));
}

real log_nonevent_prob_binary(real eta, int link_code) {
  if (link_code == 1) return -log1p_exp(eta);
  // Probit: log(1 - Phi(eta)) is written as log Phi(-eta) rather than as
  // std_normal_lccdf(eta). Stan's two normal tail functions are not equally
  // well conditioned: std_normal_lccdf() carries a relative error of 5e-4 at
  // eta = 8 and UNDERFLOWS TO -inf from eta = 8.5, where the true log
  // non-event probability is a representable -39.2, while std_normal_lcdf() is
  // exact to 2.5e-16 out to -300. Before this, a probit fit with a strongly
  // predicted event contributed -inf for an observed non-event (rejecting the
  // proposal), the aggregate binomial term went to -inf through
  // log_mean_nonevent_binary(), and the marginal log odds ratio in generated
  // quantities became +inf.
  else if (link_code == 2) return std_normal_lcdf(-eta);
  else return -exp(eta);
}

vector log_event_prob_binary_vec(vector eta, int link_code) {
  int N = num_elements(eta);
  vector[N] result;
  for (i in 1:N) result[i] = log_event_prob_binary(eta[i], link_code);
  return result;
}

vector log_nonevent_prob_binary_vec(vector eta, int link_code) {
  int N = num_elements(eta);
  vector[N] result;
  for (i in 1:N) result[i] = log_nonevent_prob_binary(eta[i], link_code);
  return result;
}

real log_mean_event_binary(vector eta, int link_code) {
  return log_sum_exp(log_event_prob_binary_vec(eta, link_code)) -
         log(num_elements(eta));
}

real log_mean_nonevent_binary(vector eta, int link_code) {
  return log_sum_exp(log_nonevent_prob_binary_vec(eta, link_code)) -
         log(num_elements(eta));
}

real bernoulli_link_lpmf(int y, real eta, int link_code) {
  if (y == 1) return log_event_prob_binary(eta, link_code);
  return log_nonevent_prob_binary(eta, link_code);
}

real integrated_binomial_lpmf(int r, int n, vector eta, int link_code) {
  real lp = lgamma(n + 1) - lgamma(r + 1) - lgamma(n - r + 1);
  if (r > 0) lp += r * log_mean_event_binary(eta, link_code);
  if (r < n) lp += (n - r) * log_mean_nonevent_binary(eta, link_code);
  return lp;
}
