test_that("add_integration generates valid integration points", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5), x1 = rbinom(50, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  dat <- add_integration(dat, n_int = 32, x1 = distr(qbern, prob = x1_mean))

  expect_true(dat$has_integration)
  expect_equal(dat$n_int, 32L)
  expect_equal(dim(dat$integration_points), c(1, 32, 1))
  # Bernoulli integration points should be 0 or 1
  expect_true(all(dat$integration_points %in% c(0, 1)))
})

test_that("add_integration can suppress progress messages", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5), x1 = rbinom(50, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_message(
    out <- add_integration(
      dat,
      n_int = 32,
      verbose = FALSE,
      x1 = distr(qbern, prob = x1_mean)
    ),
    NA
  )
  expect_true(out$has_integration)
})

test_that("add_integration works with two covariates and correlation", {
  set.seed(42)
  n <- 100
  ipd_df <- data.frame(
    trt = "A", outcome = rbinom(n, 1, 0.5),
    x1 = rbinom(n, 1, 0.4), x2 = rnorm(n, 5, 1)
  )
  agd_df <- data.frame(
    trt = "B", n_total = 200, n_events = 80,
    x1_mean = 0.3, x2_mean = 5.2, x2_sd = 0.9
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events",
                 cov_means = c("x1_mean", "x2_mean"),
                 cov_sds = c(NA, "x2_sd"))
  dat <- combine_data(ipd, agd)

  dat <- add_integration(dat, n_int = 64,
                         x1 = distr(qbern, prob = x1_mean),
                         x2 = distr(qnorm, mean = x2_mean, sd = x2_sd))

  expect_equal(dim(dat$integration_points), c(1, 64, 2))
  expect_true(all(dat$integration_points[1, , 1] %in% c(0, 1)))
  # Continuous covariate should have reasonable range
  x2_pts <- dat$integration_points[1, , 2]
  expect_true(all(is.finite(x2_pts)))
  expect_true(mean(x2_pts) > 4 && mean(x2_pts) < 6)
})

test_that("add_integration keeps integration array in data covariate order", {
  set.seed(42)
  n <- 100
  ipd_df <- data.frame(
    trt = "A", outcome = rbinom(n, 1, 0.5),
    x1 = rbinom(n, 1, 0.4), x2 = rnorm(n, 5, 1)
  )
  agd_df <- data.frame(
    trt = "B", n_total = 200, n_events = 80,
    x1_mean = 0.3, x2_mean = 5.2, x2_sd = 0.9
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events",
                 cov_means = c("x1_mean", "x2_mean"),
                 cov_sds = c(NA, "x2_sd"))
  dat <- combine_data(ipd, agd)

  dat <- add_integration(
    dat,
    n_int = 64,
    x2 = distr(qnorm, mean = x2_mean, sd = x2_sd),
    x1 = distr(qbern, prob = x1_mean)
  )

  expect_equal(dimnames(dat$integration_points)[[3]], c("x1", "x2"))
  expect_true(all(dat$integration_points[1, , "x1"] %in% c(0, 1)))
  expect_true(mean(dat$integration_points[1, , "x2"]) > 4)
})

test_that("add_integration reorders named correlation matrices to covariate order", {
  set.seed(42)
  n <- 100
  ipd_df <- data.frame(
    trt = "A", outcome = rbinom(n, 1, 0.5),
    x1 = rbinom(n, 1, 0.4), x2 = rnorm(n, 5, 1)
  )
  agd_df <- data.frame(
    trt = "B", n_total = 200, n_events = 80,
    x1_mean = 0.3, x2_mean = 5.2, x2_sd = 0.9
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events",
                 cov_means = c("x1_mean", "x2_mean"),
                 cov_sds = c(NA, "x2_sd"))
  dat <- combine_data(ipd, agd)

  cor <- matrix(c(1, 0.2, 0.2, 1), nrow = 2,
                dimnames = list(c("x2", "x1"), c("x2", "x1")))
  dat <- add_integration(
    dat,
    n_int = 64,
    cor = cor,
    x1 = distr(qbern, prob = x1_mean),
    x2 = distr(qnorm, mean = x2_mean, sd = x2_sd)
  )

  expect_equal(rownames(dat$int_cor), c("x1", "x2"))
  expect_equal(colnames(dat$int_cor), c("x1", "x2"))
})

test_that("add_integration rejects missing distribution specs", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5),
                       x1 = rbinom(50, 1, 0.4), x2 = rnorm(50))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40,
                       x1_mean = 0.3, x2_mean = 0.1)

  ipd <- set_ipd(ipd_df, "trt", "outcome", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events",
                 cov_means = c("x1_mean", "x2_mean"))
  dat <- combine_data(ipd, agd)

  # Only specifying one of two covariates
  expect_error(
    add_integration(dat, n_int = 32, x1 = distr(qbern, prob = x1_mean)),
    "Missing distribution"
  )
})

test_that("add_integration rejects invalid scalar arguments cleanly", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5),
                       x1 = rbinom(50, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40,
                       x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_error(add_integration(dat, n_int = Inf,
                               x1 = distr(qbern, prob = x1_mean)),
               "`n_int` should be a positive integer")
  expect_error(add_integration(dat, n_int = NA_real_,
                               x1 = distr(qbern, prob = x1_mean)),
               "`n_int` should be a positive integer")
  expect_error(add_integration(dat, n_int = 32,
                               cor_adjust = c("none", "pearson"),
                               x1 = distr(qbern, prob = x1_mean)),
               "`cor_adjust` must be one of")
  expect_error(add_integration(dat, n_int = 32,
                               cor_adjust = NA_character_,
                               x1 = distr(qbern, prob = x1_mean)),
               "`cor_adjust` must be one of")
})

test_that("add_integration rejects duplicate distribution names", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5),
                       x1 = rbinom(50, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40,
                       x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_error(
    add_integration(dat, n_int = 32,
                    x1 = distr(qbern, prob = x1_mean),
                    x1 = distr(qbern, prob = x1_mean)),
    "Duplicate distribution specifications for: x1"
  )
})

test_that("add_integration validates correlation names and computed correlations", {
  set.seed(42)
  n <- 100
  ipd_df <- data.frame(
    trt = "A", outcome = rbinom(n, 1, 0.5),
    x1 = rbinom(n, 1, 0.4), x2 = rnorm(n)
  )
  agd_df <- data.frame(
    trt = "B", n_total = 100, n_events = 40,
    x1_mean = 0.3, x2_mean = 0, x2_sd = 1
  )
  ipd <- set_ipd(ipd_df, "trt", "outcome", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events",
                 cov_means = c("x1_mean", "x2_mean"),
                 cov_sds = c(NA, "x2_sd"))
  dat <- combine_data(ipd, agd)

  bad_cor <- matrix(c(1, 0.2, 0.2, 1), nrow = 2,
                    dimnames = list(c("x1", "wrong"), c("x1", "wrong")))
  expect_error(
    add_integration(dat, n_int = 64, cor = bad_cor,
                    x1 = distr(qbern, prob = x1_mean),
                    x2 = distr(qnorm, mean = x2_mean, sd = x2_sd)),
    "`cor` dimnames must match covariate names"
  )

  constant_ipd <- ipd_df
  constant_ipd$x2 <- 1
  expect_warning(
    ipd_const <- set_ipd(constant_ipd, "trt", "outcome", c("x1", "x2")),
    "zero empirical variation"
  )
  dat_const <- combine_data(ipd_const, agd)
  expect_error(
    add_integration(dat_const, n_int = 64,
                    x1 = distr(qbern, prob = x1_mean),
                    x2 = distr(qnorm, mean = x2_mean, sd = x2_sd)),
    "Computed IPD correlation matrix contains non-finite values"
  )
})

test_that("add_integration works with multi-row AgD", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5), x1 = rbinom(50, 1, 0.4))
  agd_df <- data.frame(
    trt = c("B", "B"),
    n_total = c(100, 150),
    n_events = c(40, 55),
    x1_mean = c(0.3, 0.5)
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  dat <- add_integration(dat, n_int = 32, x1 = distr(qbern, prob = x1_mean))

  expect_equal(dim(dat$integration_points), c(2, 32, 1))
})

test_that("unnest_integration produces correct long format", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5), x1 = rbinom(50, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32, x1 = distr(qbern, prob = x1_mean))

  long <- unnest_integration(dat)
  expect_equal(nrow(long), 32)
  expect_true("x1" %in% names(long))
  expect_true(".int_id" %in% names(long))
  expect_true(".agd_row" %in% names(long))
})


test_that("check_integration errors without integration points", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(50, 1, 0.5), x1 = rbinom(50, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40, x1_mean = 0.3)
  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_error(
    check_integration(dat, x1 = distr(qbern, prob = x1_mean)),
    "add_integration"
  )
})


test_that("check_integration returns marginals data frame", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(100, 1, 0.5),
                       x1 = rbinom(100, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40, x1_mean = 0.3)
  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 64, x1 = distr(qbern, prob = x1_mean))

  res <- suppressMessages(suppressWarnings(
    capture.output(out <- check_integration(dat, x1 = distr(qbern, prob = x1_mean),
                                            check_joint = FALSE))
  ))
  expect_type(out, "list")
  expect_s3_class(out$marginals, "data.frame")
  expect_true(all(c("covariate", "agd_row", "mean_current",
                    "rel_diff_mean") %in% names(out$marginals)))
  # No $correlations when check_joint = FALSE
  expect_null(out$correlations)
})

test_that("check_integration can suppress printed diagnostics", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(100, 1, 0.5),
                       x1 = rbinom(100, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40, x1_mean = 0.3)
  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 64, verbose = FALSE,
                         x1 = distr(qbern, prob = x1_mean))

  expect_output(
    out <- check_integration(
      dat,
      x1 = distr(qbern, prob = x1_mean),
      check_joint = FALSE,
      verbose = FALSE
    ),
    NA
  )
  expect_s3_class(out$marginals, "data.frame")
})

test_that("check_integration validates check_joint", {
  set.seed(42)
  ipd_df <- data.frame(trt = "A", outcome = rbinom(100, 1, 0.5),
                       x1 = rbinom(100, 1, 0.4))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40, x1_mean = 0.3)
  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 64, verbose = FALSE,
                         x1 = distr(qbern, prob = x1_mean))

  expect_error(
    check_integration(dat, x1 = distr(qbern, prob = x1_mean),
                      check_joint = NA),
    "`check_joint` must be TRUE or FALSE"
  )
  expect_error(
    check_integration(dat, x1 = distr(qbern, prob = x1_mean),
                      check_joint = c(TRUE, FALSE)),
    "`check_joint` must be TRUE or FALSE"
  )
})


test_that("check_integration with check_joint returns pairwise correlations", {
  set.seed(42)
  n <- 100
  ipd_df <- data.frame(
    trt = "A", outcome = rbinom(n, 1, 0.5),
    x1 = rbinom(n, 1, 0.4), x2 = rnorm(n, 5, 1)
  )
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40,
                       x1_mean = 0.3, x2_mean = 5.2, x2_sd = 0.9)
  ipd <- set_ipd(ipd_df, "trt", "outcome", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events",
                 cov_means = c("x1_mean", "x2_mean"),
                 cov_sds = c(NA, "x2_sd"))
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 64,
                         x1 = distr(qbern, prob = x1_mean),
                         x2 = distr(qnorm, mean = x2_mean, sd = x2_sd))

  res <- suppressMessages(suppressWarnings(
    capture.output(out <- check_integration(
      dat,
      x1 = distr(qbern, prob = x1_mean),
      x2 = distr(qnorm, mean = x2_mean, sd = x2_sd),
      check_joint = TRUE
    ))
  ))
  expect_s3_class(out$correlations, "data.frame")
  expect_true(all(c("agd_row", "pair", "cor_current", "cor_doubled",
                    "abs_diff") %in% names(out$correlations)))
})
