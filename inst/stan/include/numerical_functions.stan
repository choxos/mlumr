real log_mean_exp_vec(vector x) {
  return log_sum_exp(x) - log(num_elements(x));
}

real log_weighted_mean_exp_vec(vector log_x, vector weights) {
  return log_sum_exp(log(weights) + log_x) - log_sum_exp(log(weights));
}

// Difference of two positive quantities represented by their logarithms.
// This preserves cancellation before returning to the natural scale.
real exp_difference(real log_x, real log_y) {
  // Two positive infinities mean both quantities are unbounded, so their
  // difference is indeterminate rather than zero.
  if (is_inf(log_x) && is_inf(log_y) && log_x > 0 && log_y > 0)
    return not_a_number();
  if (log_x == log_y) return 0;
  else if (log_x > log_y)
    return exp(log_x + log1m_exp(log_y - log_x));
  else
    return -exp(log_y + log1m_exp(log_x - log_y));
}
