# These tests require Stan compilation and are slow.
# Skip on CRAN and when rstan is not available.

test_that("mlumr fits SPFA model", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(42)
  n <- 100
  x1 <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.3 + 0.8 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(
    trt = "B", n_total = 200, n_events = 70, x1_mean = 0.4
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  fit <- mlumr(dat, model = "spfa", chains = 2, iter = 500,
               warmup = 250, refresh = 0, seed = 123)

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$model, "spfa")
  expect_equal(fit$family, "binomial")
  expect_true("lor_index" %in% names(fit$draws))
  expect_true("lor_comparator" %in% names(fit$draws))

  # Convergence checks
  expect_true(all(fit$summary$Rhat < 1.05, na.rm = TRUE),
              info = sprintf("max Rhat = %.3f", max(fit$summary$Rhat, na.rm = TRUE)))
  expect_true(min(fit$summary$n_eff, na.rm = TRUE) > 50,
              info = sprintf("min ESS = %.1f", min(fit$summary$n_eff, na.rm = TRUE)))
  expect_equal(fit$diagnostics$n_divergent, 0)

  # predict works
  preds <- predict(fit, population = "both")
  expect_s3_class(preds, "data.frame")
  expect_equal(nrow(preds), 4)
  expect_true(all(c("mean", "sd", "treatment", "population") %in% names(preds)))

  # marginal_effects works
  me <- marginal_effects(fit, effect = "lor")
  expect_s3_class(me, "data.frame")
  expect_true(all(me$effect == "LOR"))

  # DIC works
  dic <- calculate_dic(fit)
  expect_s3_class(dic, "mlumr_dic")
  expect_true(is.finite(dic$DIC))
})


test_that("mlumr fits Relaxed model", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(42)
  n <- 100
  x1 <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.3 + 0.8 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(
    trt = "B", n_total = 200, n_events = 70, x1_mean = 0.4
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  # Relaxed model with a single AgD row and one covariate is weakly
  # identified for beta_comparator (see the warning in mlumr()). The
  # settings below — a mildly informative prior on beta plus stronger
  # adaptation — are what a user would realistically use in this regime
  # and they produce stable convergence in short runs.
  fit <- suppressWarnings(mlumr(
    dat, model = "relaxed",
    prior_beta = prior_normal(0, 2),
    chains = 2, iter = 1000, warmup = 500,
    adapt_delta = 0.99, refresh = 0, seed = 123
  ))

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$model, "relaxed")
  expect_true("delta_beta[1]" %in% fit$summary$variable)

  # Convergence checks
  expect_true(all(fit$summary$Rhat < 1.05, na.rm = TRUE),
              info = sprintf("max Rhat = %.3f", max(fit$summary$Rhat, na.rm = TRUE)))
  expect_true(min(fit$summary$n_eff, na.rm = TRUE) > 50,
              info = sprintf("min ESS = %.1f", min(fit$summary$n_eff, na.rm = TRUE)))
  expect_equal(fit$diagnostics$n_divergent, 0)

  # Print and summary work
  expect_output(print(fit), "Relaxed SPFA")
  expect_output(summary(fit), "Relaxed SPFA")
})


test_that("mlumr rejects data without integration points", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5), x1 = rbinom(50, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_error(mlumr(dat), "Integration points not found")
})


# ---- Normal family Stan tests ----

test_that("mlumr fits Normal SPFA model", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(42)
  n <- 100
  x1 <- rbinom(n, 1, 0.5)
  score <- 2.0 + 0.5 * x1 + rnorm(n, 0, 1)

  ipd_df <- data.frame(trt = "A", score = score, x1 = x1)
  agd_df <- data.frame(trt = "B", y_mean = 1.5, se = 0.2, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "score", "x1", family = "normal")
  agd <- set_agd(agd_df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  fit <- mlumr(dat, model = "spfa", chains = 2, iter = 500,
               warmup = 250, refresh = 0, seed = 123)

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$family, "normal")
  expect_equal(fit$model_name, "mlumr_normal_spfa")
  expect_true("delta_index" %in% names(fit$draws))
  expect_true("delta_comparator" %in% names(fit$draws))
  expect_true("sigma" %in% names(fit$draws))
  expect_true("y_index_index" %in% names(fit$draws))

  # Convergence checks
  expect_true(all(fit$summary$Rhat < 1.05, na.rm = TRUE),
              info = sprintf("max Rhat = %.3f", max(fit$summary$Rhat, na.rm = TRUE)))
  expect_true(min(fit$summary$n_eff, na.rm = TRUE) > 50,
              info = sprintf("min ESS = %.1f", min(fit$summary$n_eff, na.rm = TRUE)))
  expect_equal(fit$diagnostics$n_divergent, 0)

  # predict works
  preds <- predict(fit, population = "both")
  expect_equal(nrow(preds), 4)

  # marginal effects (normal family: canonical label is MD, not DELTA)
  me <- marginal_effects(fit)
  expect_true("MD" %in% me$effect)

  # Print works
  expect_output(print(fit), "Normal")
  expect_output(summary(fit), "Mean Differences")
})


test_that("mlumr fits Normal Relaxed model", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(42)
  n <- 100
  x1 <- rbinom(n, 1, 0.5)
  score <- 2.0 + 0.5 * x1 + rnorm(n, 0, 1)

  ipd_df <- data.frame(trt = "A", score = score, x1 = x1)
  agd_df <- data.frame(trt = "B", y_mean = 1.5, se = 0.2, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "score", "x1", family = "normal")
  agd <- set_agd(agd_df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  fit <- mlumr(dat, model = "relaxed", chains = 2, iter = 500,
               warmup = 250, refresh = 0, seed = 123)

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$model_name, "mlumr_normal_relaxed")
  expect_true("delta_beta[1]" %in% fit$summary$variable)
  expect_true("sigma" %in% names(fit$draws))

  # Convergence checks
  expect_true(all(fit$summary$Rhat < 1.05, na.rm = TRUE),
              info = sprintf("max Rhat = %.3f", max(fit$summary$Rhat, na.rm = TRUE)))
  expect_true(min(fit$summary$n_eff, na.rm = TRUE) > 50,
              info = sprintf("min ESS = %.1f", min(fit$summary$n_eff, na.rm = TRUE)))
  expect_equal(fit$diagnostics$n_divergent, 0)
})


# ---- Poisson family Stan tests ----

test_that("mlumr fits Poisson SPFA model", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(42)
  n <- 100
  x1 <- rbinom(n, 1, 0.5)
  exposure <- runif(n, 0.5, 2.0)
  events <- rpois(n, exp(0.5 + 0.3 * x1) * exposure)

  ipd_df <- data.frame(trt = "A", events = events, pyears = exposure, x1 = x1)
  agd_df <- data.frame(trt = "B", n_events = 80, pyears = 400, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  fit <- mlumr(dat, model = "spfa", chains = 2, iter = 500,
               warmup = 250, refresh = 0, seed = 123)

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$family, "poisson")
  expect_equal(fit$model_name, "mlumr_poisson_spfa")
  expect_true("delta_index" %in% names(fit$draws))
  expect_true("rate_index_index" %in% names(fit$draws))

  # Convergence checks
  expect_true(all(fit$summary$Rhat < 1.05, na.rm = TRUE),
              info = sprintf("max Rhat = %.3f", max(fit$summary$Rhat, na.rm = TRUE)))
  expect_true(min(fit$summary$n_eff, na.rm = TRUE) > 50,
              info = sprintf("min ESS = %.1f", min(fit$summary$n_eff, na.rm = TRUE)))
  expect_equal(fit$diagnostics$n_divergent, 0)

  # predict works
  preds <- predict(fit, population = "both")
  expect_equal(nrow(preds), 4)

  # marginal effects (poisson family: canonical label is RR, not DELTA)
  me <- marginal_effects(fit)
  expect_true("RR" %in% me$effect)

  # Print works
  expect_output(print(fit), "Poisson")
  expect_output(summary(fit), "Rate Ratios")
})


test_that("mlumr fits Poisson Relaxed model", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(42)
  n <- 100
  x1 <- rbinom(n, 1, 0.5)
  exposure <- runif(n, 0.5, 2.0)
  events <- rpois(n, exp(0.5 + 0.3 * x1) * exposure)

  ipd_df <- data.frame(trt = "A", events = events, pyears = exposure, x1 = x1)
  agd_df <- data.frame(trt = "B", n_events = 80, pyears = 400, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  # Tighter priors for relaxed model: beta_comparator is weakly identified

  # with sparse AgD, so prior_beta = normal(0, 10) causes funnel geometry.
  fit <- mlumr(dat, model = "relaxed", chains = 2, iter = 1000,
               warmup = 500, refresh = 0, seed = 123,
               prior_beta = prior_normal(0, 2))

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$model_name, "mlumr_poisson_relaxed")
  expect_true("delta_beta[1]" %in% fit$summary$variable)
  expect_true("rate_index_index" %in% names(fit$draws))

  # Convergence checks
  expect_true(all(fit$summary$Rhat < 1.05, na.rm = TRUE),
              info = sprintf("max Rhat = %.3f", max(fit$summary$Rhat, na.rm = TRUE)))
  expect_true(min(fit$summary$n_eff, na.rm = TRUE) > 50,
              info = sprintf("min ESS = %.1f", min(fit$summary$n_eff, na.rm = TRUE)))
  expect_equal(fit$diagnostics$n_divergent, 0)
})
