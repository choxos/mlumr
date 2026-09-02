# Sampling coverage for every survival distribution and Stan model.
#
# Mirror formulas in R do not exercise the Stan parameter plumbing, the
# generated quantities, or the priors. Before this file, `weibull` carried
# almost all survival sampling coverage: `mlumr_survival_mspline_relaxed.stan`
# was never sampled at all, and gompertz, the two AFT exponential/Weibull forms,
# log-logistic, and gamma had no sampling test.
#
# These are deliberately tiny, unconverged fits. They assert plumbing and
# finiteness, not inference: the point is that each model compiles, accepts its
# data, produces the generated quantities under the names the R layer expects,
# and returns finite effects on the documented scale.

fit_tiny <- function(dat, ...) {
  suppressWarnings(suppressMessages(
    fit_survival_test(dat, chains = 1, iter = 120, warmup = 60, ...)
  ))
}

test_that("every parametric survival distribution samples and reports finite effects", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)

  # PH distributions report a hazard ratio; AFT distributions a time ratio.
  # Both labels are only earned when the baseline shape is shared, so these run
  # with aux_by = "none".
  ph  <- c("exponential", "weibull", "gompertz")
  aft <- c("exponential-aft", "weibull-aft", "lognormal", "loglogistic", "gamma",
           "gengamma")

  for (d in c(ph, aft)) {
    fit <- fit_tiny(dat, distribution = d, aux_by = "none", init = 0)
    expect_s3_class(fit, "mlumr_fit")

    me <- suppressMessages(marginal_effects(fit, effect = "hr"))
    expect_true(all(is.finite(me$mean)), info = d)
    expect_true(all(me$mean > 0), info = d)          # natural scale, null 1
    expect_true(all(me$effect == if (d %in% ph) "HR" else "TR"), info = d)

    rd <- marginal_effects(fit, effect = "rmstd")
    expect_true(all(is.finite(rd$mean)), info = d)
    expect_true(all(rd$effect == "RMSTD"), info = d)

    s <- predict(fit, type = "survival")
    expect_true(all(is.finite(s$mean)), info = d)
    expect_true(all(s$mean >= 0 & s$mean <= 1), info = d)
  }
})

test_that("the relaxed M-spline model samples end to end", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("splines2")

  # mlumr_survival_mspline_relaxed.stan: previously covered only by static
  # source-string matching, never actually run.
  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)
  fit <- fit_tiny(dat, model = "relaxed", distribution = "mspline", n_knots = 3)

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$model, "relaxed")
  # Treatment-specific coefficients exist, and the per-stratum spline simplex
  # is present under one of the accepted draw-name layouts.
  expect_true(any(grepl("^beta_comparator\\[", names(fit$draws))))
  expect_true(any(grepl("^scoef", names(fit$draws))))

  rd <- suppressMessages(marginal_effects(fit, effect = "rmstd"))
  expect_true(all(is.finite(rd$mean)))
  expect_equal(sort(unique(rd$population)), c("Comparator", "Index"))

  s <- predict(fit, type = "survival")
  expect_true(all(is.finite(s$mean)))
  expect_true(all(s$mean >= 0 & s$mean <= 1))
})

test_that("the relaxed piecewise-exponential model samples end to end", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("splines2")

  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)
  fit <- fit_tiny(dat, model = "relaxed", distribution = "pexp", n_knots = 3)

  expect_s3_class(fit, "mlumr_fit")
  rd <- suppressMessages(marginal_effects(fit, effect = "rmstd"))
  expect_true(all(is.finite(rd$mean)))
})

test_that("left censoring and delayed entry sample through the likelihood", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("survival")

  # These branches were validated at parsing level but never sampled, so a
  # mistake in the Stan status dispatch would not have surfaced. Built standalone
  # rather than reusing sim_survival_data(), which returns an already-combined
  # mlumr_data whose constructor objects are not recoverable.
  set.seed(2026)
  n <- 50
  mk <- function(surv_obj, age, male) {
    d <- data.frame(trt = "A", age = age, male = male)
    ipd_obj <- set_ipd(d, "trt", covariates = c("age", "male"),
                       family = "survival", Surv = surv_obj)
    cn <- 60
    ct <- stats::rexp(cn, 0.5)
    agd <- data.frame(trt = "B", time = ct,
                      status = stats::rbinom(cn, 1, 0.7),
                      age_mean = 0.2, age_sd = 1, male_prop = 0.45)
    agd_obj <- set_agd_surv(agd, "trt", time = "time", status = "status",
                            cov_means = c("age_mean", "male_prop"),
                            cov_sds = c("age_sd", NA),
                            cov_types = c("continuous", "binary"))
    suppressWarnings(add_integration(
      combine_data(ipd_obj, agd_obj), n_int = 8,
      age = distr(qnorm, mean = age_mean, sd = age_sd),
      male = distr(qbern, prob = male_mean), verbose = FALSE))
  }

  age <- stats::rnorm(n)
  male <- stats::rbinom(n, 1, 0.5)
  tt <- stats::rexp(n, 0.5)

  # Left censoring: Surv(type = "left") marks status 0 as left-censored rather
  # than right-censored, which is the branch mlumr maps to internal status 2.
  st <- stats::rbinom(n, 1, 0.7)
  d_left <- mk(survival::Surv(tt, st, type = "left"), age, male)
  fit_left <- fit_tiny(d_left, distribution = "weibull")
  expect_s3_class(fit_left, "mlumr_fit")
  expect_true(all(is.finite(predict(fit_left, type = "survival")$mean)))

  # Delayed entry (left truncation): entry strictly before the event time.
  entry <- pmax(pmin(tt * 0.1, tt - 1e-6), 0)
  d_delay <- mk(survival::Surv(entry, tt, stats::rbinom(n, 1, 0.7)), age, male)
  fit_delay <- fit_tiny(d_delay, distribution = "weibull")
  expect_s3_class(fit_delay, "mlumr_fit")
  expect_true(all(is.finite(predict(fit_delay, type = "survival")$mean)))
})
