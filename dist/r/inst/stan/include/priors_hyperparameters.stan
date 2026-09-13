// Prior hyperparameters for ML-UMR intercepts and regression coefficients.
//
// This file is included from each model's data block *after* `n_cov` is
// declared. The four `_dist` / `_df` fields let the model branch on prior
// family at runtime without re-compiling.
//
//   prior_*_dist:  0 = normal, 1 = student_t
//   prior_*_df:    used only when dist == 1; otherwise any positive
//                  placeholder (Stan rejects non-positive df in `data`).
//
// `prior_beta_mean` and `prior_beta_sd` are vectors so that per-coefficient
// priors and `autoscale` (divide scale by sd(x_j)) are supported without
// another Stan model variant. For a scalar user-facing prior the R side
// broadcasts to length `n_cov`.

real prior_intercept_mean;
real<lower=0> prior_intercept_sd;
int<lower=0,upper=1> prior_intercept_dist;
real<lower=0> prior_intercept_df;

vector[n_cov] prior_beta_mean;
vector<lower=0>[n_cov] prior_beta_sd;
int<lower=0,upper=1> prior_beta_dist;
real<lower=0> prior_beta_df;
