test_that("stc computes LOR with delta-method SE", {
  set.seed(123)
  n <- 200

  # Generate IPD with known effect
  x1 <- rbinom(n, 1, 0.4)
  x2 <- rbinom(n, 1, 0.6)
  logit_p <- -0.5 + 1.0 * x1 - 0.5 * x2
  outcome <- rbinom(n, 1, plogis(logit_p))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1, x2 = x2)
  agd_df <- data.frame(
    trt = "B", n_total = 300, n_events = 120,
    x1_mean = 0.3, x2_mean = 0.7
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = c("x1_mean", "x2_mean"))
  dat <- combine_data(ipd, agd)

  result <- stc(dat)

  expect_s3_class(result, "mlumr_stc")
  expect_true(is.finite(result$estimate))
  expect_gt(result$se, 0)
  expect_lt(result$ci_lower, result$estimate)
  expect_gt(result$ci_upper, result$estimate)

  # GLM fit should be stored
  expect_s3_class(result$glm_fit, "glm")

  # Print method works
  expect_output(print(result), "Simulated Treatment Comparison")
})


test_that("stc works with integration points", {
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

  # Add integration points
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  result <- stc(dat)
  expect_s3_class(result, "mlumr_stc")
  expect_true(is.finite(result$estimate))
})


test_that("stc respects custom conf_level", {
  set.seed(123)
  n <- 200
  x1 <- rbinom(n, 1, 0.4)
  outcome <- rbinom(n, 1, plogis(-0.5 + 1.0 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(trt = "B", n_total = 300, n_events = 120, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  r95 <- stc(dat, conf_level = 0.95)
  r90 <- stc(dat, conf_level = 0.90)

  expect_lt(r90$ci_upper - r90$ci_lower, r95$ci_upper - r95$ci_lower)
})


test_that("stc validates data and conf_level", {
  ipd_df <- data.frame(trt = "A", outcome = c(1, 0, 1, 0, 1, 0),
                       x1 = c(0, 1, 0, 1, 0, 1))
  agd_df <- data.frame(trt = "B", n_total = 20, n_events = 8, x1_mean = 0.5)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_error(stc(data.frame()), "combine_data")
  expect_error(stc(dat, conf_level = 1), "`conf_level`")
  expect_error(stc(dat, conf_level = c(0.90, 0.95)), "`conf_level`")
  expect_error(stc(dat, conf_level = NA_real_), "`conf_level`")
})


test_that("stc supports non-syntactic covariate names", {
  set.seed(12)
  n <- 120
  x <- rbinom(n, 1, 0.4)
  outcome <- rbinom(n, 1, plogis(-0.2 + 0.7 * x))
  ipd_df <- data.frame(trt = "A", outcome = outcome, check.names = FALSE)
  ipd_df[["x one"]] <- x
  agd_df <- data.frame(trt = "B", n_total = 180, n_events = 70,
                       check.names = FALSE)
  agd_df[["x one_mean"]] <- 0.3

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x one")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x one_mean")
  dat <- combine_data(ipd, agd)

  result <- stc(dat)
  expect_s3_class(result, "mlumr_stc")
  expect_true(is.finite(result$estimate))
  expect_true(is.finite(result$se))
})


test_that("stc handles multi-row AgD with weighted integration", {
  set.seed(42)
  n <- 150
  x1 <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.3 + 0.8 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(
    trt = c("B", "B"),
    n_total = c(100, 200),
    n_events = c(35, 80),
    x1_mean = c(0.3, 0.5)
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  # Without integration points
  result_no_int <- stc(dat)
  expect_s3_class(result_no_int, "mlumr_stc")
  expect_true(is.finite(result_no_int$estimate))

  # With integration points
  dat <- add_integration(dat, n_int = 32, x1 = distr(qbern, prob = x1_mean))
  result_int <- stc(dat)
  expect_s3_class(result_int, "mlumr_stc")
  expect_true(is.finite(result_int$estimate))
})


# ---- Normal family ----

test_that("stc works with normal family", {
  set.seed(123)
  n <- 200
  x1 <- rnorm(n, 0, 1)
  score <- 2.0 + 0.5 * x1 + rnorm(n, 0, 1)

  ipd_df <- data.frame(trt = "A", score = score, x1 = x1)
  agd_df <- data.frame(trt = "B", y_mean = 1.5, se = 0.2, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "score", "x1", family = "normal")
  agd <- set_agd(agd_df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- stc(dat)
  expect_s3_class(result, "mlumr_stc")
  expect_equal(result$family, "normal")
  expect_true(is.finite(result$estimate))
  expect_gt(result$se, 0)

  # Print works
  expect_output(print(result), "Mean Difference")
})


# ---- Poisson family ----

test_that("stc works with poisson family", {
  set.seed(123)
  n <- 200
  x1 <- rbinom(n, 1, 0.4)
  exposure <- runif(n, 0.5, 2.0)
  events <- rpois(n, exp(0.5 + 0.3 * x1) * exposure)

  ipd_df <- data.frame(trt = "A", events = events, pyears = exposure, x1 = x1)
  agd_df <- data.frame(trt = "B", n_events = 80, pyears = 400, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- stc(dat)
  expect_s3_class(result, "mlumr_stc")
  expect_equal(result$family, "poisson")
  expect_true(is.finite(result$estimate))
  expect_gt(result$se, 0)

  # Print works
  expect_output(print(result), "Log Rate Ratio")
})


test_that("stc handles zero-event Poisson comparator data", {
  set.seed(321)
  n <- 200
  x1 <- rbinom(n, 1, 0.4)
  exposure <- runif(n, 0.5, 2.0)
  events <- rpois(n, exp(0.3 + 0.2 * x1) * exposure)

  ipd_df <- data.frame(trt = "A", events = events, pyears = exposure, x1 = x1)
  agd_df <- data.frame(trt = "B", n_events = 0, pyears = 400, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- stc(dat)
  expect_true(is.finite(result$estimate))
  expect_true(is.finite(result$se))
  expect_equal(result$rate_comparator, 0)
  expect_equal(result$events_comparator_adjusted, 0.5)
})
