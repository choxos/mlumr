# Regression tests for prior_sensitivity()'s argument replay.
#
# These previously asserted on helper closures defined inside the tests
# themselves (a local `na_to_null`, a local `missing()` probe, a local
# `forward()`), which meant they described the intended behavior without
# touching the package: prior_sensitivity() could regress in exactly the ways
# the comments warn about and every assertion would still pass. They now run
# the real function, which requires Stan and so is gated accordingly.

test_that("prior_sensitivity replays a parametric survival baseline", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # surv_controls records mspline_degree = NA for a parametric baseline.
  # mlumr() accepts NULL for "not applicable" but rejects NA, and `%||%` does
  # not catch NA, so the refit aborted with "`mspline_degree` must be a
  # non-negative integer" on every parametric survival fit.
  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)
  fit <- suppressWarnings(suppressMessages(
    fit_survival_test(dat, distribution = "weibull", chains = 1,
                      iter = 120, warmup = 60)
  ))
  expect_true(is.na(fit$surv_controls$mspline_degree))

  out <- suppressWarnings(suppressMessages(
    prior_sensitivity(fit, prior_beta_scales = c(1, 2.5), verbose = FALSE,
                      chains = 1, iter = 120, warmup = 60)
  ))
  expect_s3_class(out, "data.frame")
  expect_setequal(out$scale, c(1, 2.5))
  expect_true(all(is.finite(out$mean)))
})

test_that("prior_sensitivity does not forward aux_by to non-survival refits", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # The refit goes through do.call(mlumr, base_args). mlumr() guards `aux_by`
  # with !missing(), which is TRUE even when the value passed is NULL, so a
  # survival-only argument cannot be neutralized by setting it to NULL: it has
  # to be left out of the argument list entirely. Forwarding it for a binomial
  # fit aborts with "`aux_by` is only used for family = 'survival'".
  set.seed(2026)
  n <- 60
  ipd <- data.frame(trt = "A", y = rbinom(n, 1, 0.4), age = rnorm(n))
  agd <- data.frame(trt = "B", n = 100, r = 35, age_mean = 0.2, age_sd = 1)
  dat <- suppressWarnings(suppressMessages(add_integration(
    combine_data(
      set_ipd(ipd, "trt", outcome = "y", covariates = "age"),
      set_agd(agd, "trt", outcome_n = "n", outcome_r = "r",
              cov_means = "age_mean", cov_sds = "age_sd",
              cov_types = "continuous")
    ),
    n_int = 8, age = distr(qnorm, mean = age_mean, sd = age_sd), verbose = FALSE
  )))
  fit <- suppressWarnings(suppressMessages(
    mlumr(dat, model = "spfa", chains = 1, iter = 120, warmup = 60,
          seed = 2026, refresh = 0, verbose = FALSE)
  ))

  out <- suppressWarnings(suppressMessages(
    prior_sensitivity(fit, prior_beta_scales = c(1, 2.5), verbose = FALSE,
                      chains = 1, iter = 120, warmup = 60)
  ))
  expect_setequal(out$scale, c(1, 2.5))
  # SPFA has no comparator coefficient prior, so the paired column is dropped.
  expect_null(out$scale_comparator)
})
