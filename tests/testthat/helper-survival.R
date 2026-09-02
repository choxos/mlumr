# Shared helpers for survival (time-to-event) tests.

# Simulate an unanchored survival data set: index IPD from a Weibull PH model
# and a reconstructed comparator pseudo-IPD, returning a combined mlumr_data
# object with integration points.
#
# `loghr` is the CONDITIONAL log hazard ratio: both arms are drawn with the same
# Weibull shape and the same coefficients, so at any fixed covariate profile the
# hazard ratio is the constant `exp(mu_index - mu_comparator)`. It is not the
# marginal log hazard ratio throughout follow-up. Even with a constant
# individual-level hazard ratio, differential survival changes the covariate
# composition of the two risk sets, so
#
#   hbar_k(t) = E_X[h_k(t | X) S_k(t | X)] / E_X[S_k(t | X)]
#
# and the marginal ratio drifts with time. The two coincide only in the t -> 0
# limit, where the risk sets are still the randomized populations. That limit is
# what `delta_*` reports for a shared-shape PH fit, which is why the recovery
# test compares `delta_comparator` against `loghr`; tests that look at the
# marginal HR over follow-up (test-survival-predict.R) expect it to vary.
sim_survival_data <- function(seed = 2026, n_ipd = 150, n_agd = 180,
                              loghr = -0.4, shape = 1.2, n_int = 32) {
  set.seed(seed)
  beta <- c(0.3, -0.2)

  sim_arm <- function(n, mu, age_mean, male_prob) {
    age <- stats::rnorm(n, age_mean, 1)
    male <- stats::rbinom(n, 1, male_prob)
    eta <- mu + beta[1] * age + beta[2] * male
    u <- stats::runif(n)
    t_event <- (-log(u) / exp(eta))^(1 / shape)
    cens <- stats::rexp(n, 0.08)
    list(time = pmin(t_event, cens),
         status = as.integer(t_event <= cens),
         age = age, male = male)
  }

  idx <- sim_arm(n_ipd, mu = 0, age_mean = 0, male_prob = 0.5)
  ipd <- data.frame(trt = "A", time = idx$time, status = idx$status,
                    age = idx$age, male = idx$male)
  ipd_obj <- set_ipd(ipd, "trt", covariates = c("age", "male"),
                     family = "survival", time = "time", status = "status")

  cmp <- sim_arm(n_agd, mu = -loghr, age_mean = 0.2, male_prob = 0.45)
  agd <- data.frame(trt = "B", time = cmp$time, status = cmp$status,
                    age_mean = mean(cmp$age), age_sd = stats::sd(cmp$age),
                    male_prop = mean(cmp$male))
  agd_obj <- set_agd_surv(agd, "trt", time = "time", status = "status",
                          cov_means = c("age_mean", "male_prop"),
                          cov_sds = c("age_sd", NA),
                          cov_types = c("continuous", "binary"))

  dat <- combine_data(ipd_obj, agd_obj)
  suppressWarnings(add_integration(
    dat, n_int = n_int,
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    male = distr(qbern, prob = male_mean),
    verbose = FALSE
  ))
}

# Fit a short survival chain for tests.
fit_survival_test <- function(dat, model = "spfa", distribution = "weibull",
                              chains = 2, iter = 600, warmup = 300, seed = 2026,
                              ...) {
  mlumr(dat, model = model, distribution = distribution,
        chains = chains, iter = iter, warmup = warmup,
        refresh = 0, seed = seed, ...)
}
