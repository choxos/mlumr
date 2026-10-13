// Prior hyperparameters for the residual SD (normal family only).
//
//   prior_sigma_dist:  0 = normal (half-normal via <lower=0>)
//                      1 = student_t (half-t)
//                      2 = exponential (rate = 1 / scale)
//   prior_sigma_df:    used only when dist == 1; otherwise any positive
//                      placeholder.

real prior_sigma_location;
real<lower=0> prior_sigma_scale;
int<lower=0,upper=2> prior_sigma_dist;
real<lower=0> prior_sigma_df;
