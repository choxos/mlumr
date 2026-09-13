// Prior hyperparameters for the auxiliary shape/scale parameters of the
// parametric survival models. Included in the data block of
// mlumr_survival_{spfa,relaxed}.stan after `n_cov` is declared.
//
//   prior_aux_dist:  0 = half-normal, 1 = half-t, 2 = exponential
//                    (positivity comes from the <lower=0> parameter constraint;
//                     see log_prior_sigma() in priors_functions.stan)
//   prior_aux_df:    used only when dist == 1; otherwise a positive placeholder.
//
// `aux`  is the shape (Weibull/gamma), rate-of-increase (Gompertz), shape
//        (log-logistic), sdlog (log-normal), or first shape (gen. gamma).
// `aux2` is the second shape of the generalized gamma only.

real prior_aux_location;
real<lower=0> prior_aux_scale;
int<lower=0,upper=2> prior_aux_dist;
real<lower=0> prior_aux_df;

real prior_aux2_location;
real<lower=0> prior_aux2_scale;
int<lower=0,upper=2> prior_aux2_dist;
real<lower=0> prior_aux2_df;
