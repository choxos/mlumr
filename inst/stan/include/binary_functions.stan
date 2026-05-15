// Clamp marginal probabilities before applying logit in generated quantities.
// This avoids infinite log-odds when posterior draws are numerically 0 or 1.
real safe_logit(real p) {
  return logit(fmin(fmax(p, 1e-10), 1 - 1e-10));
}

// Binary inverse-link dispatcher.
// link_code: 1 = logit, 2 = probit, 3 = complementary log-log.
real inv_link_binary(real eta, int link_code) {
  if (link_code == 1) return inv_logit(eta);
  else if (link_code == 2) return Phi(eta);
  else return 1 - exp(-exp(eta));  // cloglog
}

// Vectorized inverse-link helper for IPD and integration-point predictors.
vector inv_link_binary_vec(vector eta, int link_code) {
  int N = num_elements(eta);
  if (link_code == 1) return inv_logit(eta);
  else if (link_code == 2) return Phi(eta);
  else {
    vector[N] result;
    for (i in 1:N) result[i] = 1 - exp(-exp(eta[i]));
    return result;
  }
}
