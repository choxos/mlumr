test_that("naive computes correct LOR", {
  # Create simple data
  set.seed(2026)
  n <- 100
  ipd_df <- data.frame(
    trt = "A",
    outcome = c(rep(1, 60), rep(0, 40)),
    x1 = rnorm(n)
  )
  agd_df <- data.frame(
    trt = "B", n_total = 200, n_events = 80, x1_mean = 0.5
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- naive(dat)

  expect_s3_class(result, "mlumr_naive")

  # Manual check: p_A = 0.6, p_B = 0.4
  p_A <- 0.6
  p_B <- 0.4
  expected_lor <- qlogis(p_A) - qlogis(p_B)
  expect_equal(result$estimate, expected_lor, tolerance = 1e-4)

  # SE should be positive
  expect_gt(result$se, 0)

  # CI should contain the estimate
  expect_lt(result$ci_lower, result$estimate)
  expect_gt(result$ci_upper, result$estimate)

  # Print method works
  expect_output(print(result), "Naive")
})


test_that("naive handles extreme rates", {
  # All events
  ipd_df <- data.frame(trt = "A", outcome = rep(1, 50), x1 = rnorm(50))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 1, x1_mean = 0.5)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- naive(dat)
  expect_true(is.finite(result$estimate))
  expect_true(is.finite(result$se))
  expect_gte(result$p_index_lower, 0)
  expect_lte(result$p_index_upper, 1)
  expect_gte(result$p_comparator_lower, 0)
  expect_lte(result$p_comparator_upper, 1)
})


test_that("naive respects custom conf_level", {
  ipd_df <- data.frame(trt = "A", outcome = c(rep(1, 60), rep(0, 40)),
                       x1 = rnorm(100))
  agd_df <- data.frame(trt = "B", n_total = 200, n_events = 80, x1_mean = 0.5)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  r95 <- naive(dat, conf_level = 0.95)
  r90 <- naive(dat, conf_level = 0.90)

  # 90% CI should be narrower than 95% CI
  expect_lt(r90$ci_upper - r90$ci_lower, r95$ci_upper - r95$ci_lower)
  expect_equal(r90$conf_level, 0.90)
})


test_that("naive validates data and conf_level", {
  ipd_df <- data.frame(trt = "A", outcome = c(1, 0, 1, 0), x1 = c(0, 1, 0, 1))
  agd_df <- data.frame(trt = "B", n_total = 10, n_events = 4, x1_mean = 0.5)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_error(naive(data.frame()), "combine_data")
  expect_error(naive(dat, conf_level = 1), "`conf_level`")
  expect_error(naive(dat, conf_level = c(0.90, 0.95)), "`conf_level`")
  expect_error(naive(dat, conf_level = NA_real_), "`conf_level`")
})


test_that("naive works with equal rates", {
  ipd_df <- data.frame(trt = "A", outcome = c(rep(1, 50), rep(0, 50)),
                       x1 = rnorm(100))
  agd_df <- data.frame(trt = "B", n_total = 200, n_events = 100, x1_mean = 0.5)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- naive(dat)
  expect_equal(result$estimate, 0, tolerance = 1e-10)
})


# ---- Normal family ----

test_that("naive works with normal family", {
  set.seed(2026)
  ipd_df <- data.frame(trt = "A", score = rnorm(100, mean = 5, sd = 2),
                       x1 = rnorm(100))
  agd_df <- data.frame(trt = "B", y_mean = 3.0, se = 0.2, x1_mean = 0.5)

  ipd <- set_ipd(ipd_df, "trt", "score", "x1", family = "normal")
  agd <- set_agd(agd_df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- naive(dat)
  expect_s3_class(result, "mlumr_naive")
  expect_equal(result$family, "normal")
  expect_true(is.finite(result$estimate))
  expect_gt(result$se, 0)

  # Mean difference should be approximately 5 - 3 = 2
  expect_equal(result$estimate, mean(ipd_df$score) - 3.0, tolerance = 1e-10)

  # Print works
  expect_output(print(result), "Mean Difference")
})

test_that("normal naive benchmark combines aggregate strata using population weights", {
  ipd_df <- data.frame(trt = "A", y = 0:5, x = -2:3)
  agd_df <- data.frame(
    trt = c("B", "B"), n = c(10, 90), y = c(0, 10),
    se = c(0.05, 5), x_mean = c(-1, 1)
  )
  dat <- combine_data(
    set_ipd(ipd_df, "trt", "y", "x", family = "normal"),
    set_agd(
      agd_df, "trt", family = "normal", outcome_n = "n",
      outcome_mean = "y", outcome_se = "se", cov_means = "x_mean",
      cov_types = "continuous"
    )
  )

  result <- naive(dat)
  population_weights <- agd_df$n / sum(agd_df$n)

  expect_equal(result$mean_comparator, weighted.mean(agd_df$y, agd_df$n),
               tolerance = 1e-12)
  expect_equal(result$mean_comparator_se,
               sqrt(sum(population_weights^2 * agd_df$se^2)),
               tolerance = 1e-12)
  expect_gt(abs(result$mean_comparator -
                  weighted.mean(agd_df$y, 1 / agd_df$se^2)), 1)

  expect_error(
    set_agd(
      agd_df, "trt", family = "normal", outcome_mean = "y",
      outcome_se = "se", cov_means = "x_mean", cov_types = "continuous"
    ),
    "outcome_n.*multiple normal aggregate rows"
  )
})


# ---- Poisson family ----

test_that("naive works with poisson family", {
  set.seed(2026)
  ipd_df <- data.frame(trt = "A", events = rpois(50, 3),
                       pyears = runif(50, 0.5, 2), x1 = rnorm(50))
  agd_df <- data.frame(trt = "B", n_events = 100, pyears = 500, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- naive(dat)
  expect_s3_class(result, "mlumr_naive")
  expect_equal(result$family, "poisson")
  expect_true(is.finite(result$estimate))
  expect_gt(result$se, 0)

  # Manual check
  rate_A <- sum(ipd_df$events) / sum(ipd_df$pyears)
  rate_B <- 100 / 500
  expected_lrr <- log(rate_A) - log(rate_B)
  expect_equal(result$estimate, expected_lrr, tolerance = 1e-10)

  # Print works
  expect_output(print(result), "Log Rate Ratio")
})

test_that("naive() rejects left/interval-censored survival data", {
  skip_if_not_installed("survival")
  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)
  res <- naive(dat)
  expect_s3_class(res, "mlumr_naive")
  expect_true(is.finite(res$estimate))
  dat_left <- dat
  dat_left$ipd$data$.status[1] <- 2L
  expect_error(naive(dat_left), "left- or interval-censored")
  dat_interval <- dat
  dat_interval$agd$pseudo_ipd$.status[1] <- 3L
  expect_error(naive(dat_interval), "left- or interval-censored")
})
