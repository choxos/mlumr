# A comparator pseudo-individual under delayed entry contributes a likelihood
# conditional on having survived to entry. With individual data the covariates
# are known and the conditioning is unambiguous. With aggregate data they are
# integrated out, and the order matters:
#
#   E_X[ f(t | X) / S(entry | X) ]        conditions first, averages second;
#   E_X[ f(t | X) ] / E_X[ S(entry | X) ] averages first, conditions second.
#
# The model does the first, which is the right form when the supplied covariate
# moments describe the population observed at entry; `?set_agd_surv` says so.
# The IPD delayed-entry tests cannot establish that the aggregate path does
# this, because there the covariates are known and the two orders coincide.
# This compares the Stan likelihood, draw by draw, against R's own integration
# under a two-component covariate mixture where the exact expectation is a
# two-term sum, and checks that the other order would have given a different
# number, so the comparison can tell them apart.

test_that("the aggregate delayed-entry likelihood conditions on entry before averaging", {
  skip_on_cran()
  set.seed(2026)
  beta <- 0.7

  # Index IPD: exponential hazards, one binary covariate.
  n_ipd <- 60L
  x <- stats::rbinom(n_ipd, 1, 0.5)
  t_ev <- stats::rexp(n_ipd, exp(-1 + beta * x))
  t_c <- stats::rexp(n_ipd, 0.3)
  ipd <- data.frame(trt = "A", time = pmin(t_ev, t_c),
                    status = as.integer(t_ev <= t_c), x = x)
  ipd_obj <- set_ipd(ipd, "trt", covariates = "x", family = "survival",
                     time = "time", status = "status")

  # Comparator pseudo-individuals, every one entering late.
  n_agd <- 40L
  xc <- stats::rbinom(n_agd, 1, 0.5)
  entry <- stats::runif(n_agd, 0.1, 1.5)
  t_ev <- entry + stats::rexp(n_agd, exp(-0.6 + beta * xc))
  t_c <- entry + stats::rexp(n_agd, 0.3)
  agd <- data.frame(trt = "B", time = pmin(t_ev, t_c),
                    status = as.integer(t_ev <= t_c), entry = entry,
                    x_mean = 0.5)
  agd_obj <- set_agd_surv(agd, "trt", time = "time", status = "status",
                          entry_time = "entry", cov_means = "x_mean",
                          cov_sds = NA, cov_types = "binary")

  # With a binary covariate the integration grid takes two values, so the
  # grid average IS a two-component mixture expectation with the realized
  # proportion as its weight. That proportion is the quasi-random grid's
  # rendering of the declared one half (64 points put 31 at one value here),
  # and integration fidelity is a separate question covered elsewhere; this
  # test is about the order of conditioning and averaging, so the exact
  # expectation below uses the realized weight.
  dat <- combine_data(ipd_obj, agd_obj)
  dat <- add_integration(dat, n_int = 64, x = distr(qbern, prob = x_mean),
                         verbose = FALSE)
  fit <- mlumr(dat, model = "spfa", distribution = "exponential",
               chains = 1, iter = 200, warmup = 100, refresh = 0, seed = 2026)

  sd <- fit$stan_data
  expect_identical(sd$dist, 1L)
  expect_true(all(sd$agd_delay_time > 0))
  grid <- as.numeric(sd$X_int[1, , 1])           # centered covariate values
  center <- as.numeric(sd$cov_center)
  expect_setequal(round(grid + center, 10), c(0, 1))
  p1 <- mean(grid + center)
  expect_lt(abs(p1 - 0.5), 0.05)
  w <- c(1 - p1, p1)

  t_j <- sd$agd_time
  d_j <- sd$agd_delay_time
  s_j <- sd$agd_status
  # Exponential hazards: h = exp(eta), S(t) = exp(-t h), f(t) = h S(t).
  # Conditional on survival to entry: f(t)/S(d) for an event, S(t)/S(d) for a
  # right-censored time.
  cond_ll <- function(eta, t, d, status) {
    h <- exp(eta)
    if (status == 1L) log(h) - h * (t - d) else -h * (t - d)
  }
  draws <- fit$draws
  ll_stan <- as.matrix(draws[, sprintf("log_lik_agd[%d]", seq_len(n_agd))])
  ll_grid <- ll_exact <- ll_other <- matrix(NA_real_, nrow(draws), n_agd)
  xs <- c(0, 1) - center
  for (s in seq_len(nrow(draws))) {
    mu <- draws$mu_comparator[s]
    b <- draws[["beta[1]"]][s]
    for (j in seq_len(n_agd)) {
      # What the model computes: average over the grid of the conditional
      # likelihood.
      ll_grid[s, j] <- log(mean(exp(cond_ll(mu + b * grid, t_j[j], d_j[j], s_j[j]))))
      # The two-component expectation under the same definition.
      ll_exact[s, j] <- log(sum(w * exp(cond_ll(mu + b * xs, t_j[j], d_j[j], s_j[j]))))
      # The other order: average the unconditional likelihood and the
      # survival to entry separately, then take their ratio.
      h <- exp(mu + b * xs)
      num <- if (s_j[j] == 1L) h * exp(-h * t_j[j]) else exp(-h * t_j[j])
      ll_other[s, j] <- log(sum(w * num)) - log(sum(w * exp(-h * d_j[j])))
    }
  }
  # Absolute, not relative: the draws come back through the backend's output
  # files, and CmdStan writes them with six significant figures, so a value
  # recomputed from the rounded parameters differs at that level. The other
  # order differs by orders of magnitude more, so the agreement is with the
  # documented order and not with both.
  expect_lt(max(abs(ll_stan - ll_grid)), 1e-4)
  expect_lt(max(abs(ll_stan - ll_exact)), 1e-4)
  expect_gt(max(abs(ll_stan - ll_other)), 1e-2)
  expect_gt(stats::median(abs(ll_stan - ll_other)), 1e-2)
})
