# The arms of a naive or STC comparison are observed directly, and their
# intervals are exact. A Wald interval around a boundary-corrected standard
# error, bounded to the parameter range, covered a true probability of
# 0.014 only 75.5% of the time at n = 100: the zero-count outcome alone has
# probability 0.24 there and its interval ended at 0.0138. Coverage is
# checked by enumerating every count, not by simulation.

.arm_data <- function(r_index, r_comparator, n = 100L) {
  ipd <- set_ipd(
    data.frame(trt = "A",
               y = c(rep(1L, r_index), rep(0L, n - r_index)),
               x = rep(c(0, 1), length.out = n)),
    "trt", outcome = "y", covariates = "x"
  )
  agd <- set_agd(data.frame(trt = "B", n = n, r = r_comparator, x_mean = 0.5),
                 "trt", outcome_n = "n", outcome_r = "r",
                 cov_means = "x_mean", cov_sds = NA_character_,
                 cov_types = "binary")
  suppressWarnings(combine_data(ipd, agd))
}

test_that("the exact binomial interval is Clopper-Pearson", {
  n <- 100L
  for (level in c(0.95, 0.9)) {
    for (r in c(0L, 1L, 2L, 13L, 50L, 99L, 100L)) {
      got <- mlumr:::.clopper_pearson_interval(r, n, level)
      ref <- stats::binom.test(r, n, conf.level = level)$conf.int
      expect_equal(c(got$lower, got$upper), as.numeric(ref), tolerance = 1e-12)
    }
  }
  # 0 of 100: the exact upper bound, not the Wald 0.0138.
  expect_equal(mlumr:::.clopper_pearson_interval(0, 100, 0.95)$upper,
               0.03621669, tolerance = 1e-7)
  # A non-integer count is taken as given, with the same quantiles.
  got <- mlumr:::.clopper_pearson_interval(2.5, 10, 0.95)
  expect_equal(c(got$lower, got$upper),
               c(qbeta(0.025, 2.5, 8.5), qbeta(0.975, 3.5, 7.5)),
               tolerance = 1e-12)
  # Vectors mixing boundary and interior counts: no quantile at shape zero
  # is evaluated, so no warning and no NaN.
  expect_silent(got <- mlumr:::.clopper_pearson_interval(c(0, 50, 100), 100,
                                                         0.95))
  expect_equal(got$lower, c(0, qbeta(0.025, 50, 51), qbeta(0.025, 100, 1)),
               tolerance = 1e-12)
  expect_equal(got$upper, c(qbeta(0.975, 1, 100), qbeta(0.975, 51, 50), 1),
               tolerance = 1e-12)
  expect_silent(got <- mlumr:::.garwood_interval(c(0, 3), 100, 0.95))
  expect_equal(got$lower, c(0, qgamma(0.025, 3) / 100), tolerance = 1e-12)
})

test_that("the arm intervals follow the requested confidence level", {
  d <- .arm_data(30L, 0L)
  for (level in c(0.9, 0.99)) {
    nv <- suppressWarnings(naive(d, conf_level = level))
    expect_equal(c(nv$p_index_lower, nv$p_index_upper),
                 as.numeric(stats::binom.test(30, 100,
                                              conf.level = level)$conf.int),
                 tolerance = 1e-12)
    expect_equal(c(nv$p_comparator_lower, nv$p_comparator_upper),
                 as.numeric(stats::binom.test(0, 100,
                                              conf.level = level)$conf.int),
                 tolerance = 1e-12)
    di <- suppressWarnings(add_integration(d, n_int = 16,
                                           x = distr(qbern, prob = x_mean),
                                           verbose = FALSE))
    st <- suppressWarnings(stc(di, conf_level = level))
    expect_equal(c(st$p_comparator_lower, st$p_comparator_upper),
                 c(nv$p_comparator_lower, nv$p_comparator_upper),
                 tolerance = 1e-12)
  }
  ipd <- set_ipd(data.frame(trt = "A", y = c(3L, rep(0L, 9)), E = rep(10, 10),
                            age = 1:10),
                 "trt", outcome = "y", covariates = "age",
                 family = "poisson", exposure = "E")
  agd <- set_agd(data.frame(trt = "B", r = 0L, E = 100, age_mean = 5,
                            age_sd = 3),
                 "trt", family = "poisson", outcome_r = "r", outcome_E = "E",
                 cov_means = "age_mean", cov_sds = "age_sd",
                 cov_types = "continuous")
  nv <- suppressWarnings(naive(combine_data(ipd, agd), conf_level = 0.9))
  expect_equal(c(nv$rate_index_lower, nv$rate_index_upper),
               as.numeric(stats::poisson.test(3, 100,
                                              conf.level = 0.9)$conf.int),
               tolerance = 1e-12)
  expect_equal(c(nv$rate_comparator_lower, nv$rate_comparator_upper),
               as.numeric(stats::poisson.test(0, 100,
                                              conf.level = 0.9)$conf.int),
               tolerance = 1e-12)
})

test_that("the exact Poisson interval is Garwood's", {
  for (level in c(0.95, 0.9)) {
    for (x in c(0L, 1L, 3L, 20L, 150L)) {
      got <- mlumr:::.garwood_interval(x, 100, level)
      ref <- stats::poisson.test(x, 100, conf.level = level)$conf.int
      expect_equal(c(got$lower, got$upper), as.numeric(ref), tolerance = 1e-12)
    }
  }
})

test_that("both naive binomial arm intervals cover at least the nominal level", {
  n <- 100L
  ci <- vapply(0:n, function(r) {
    s <- suppressWarnings(naive(.arm_data(r, r, n)))
    c(s$p_index_lower, s$p_index_upper, s$p_comparator_lower,
      s$p_comparator_upper)
  }, numeric(4))
  # The two arms are computed the same way.
  expect_equal(ci[1:2, ], ci[3:4, ], tolerance = 1e-12)
  for (p in c(0.001, 0.01, 0.014, 0.02, 0.05, 0.5, 0.95, 0.98, 0.986)) {
    covered <- ci[3, ] <= p & ci[4, ] >= p
    coverage <- sum(stats::dbinom(0:n, n, p)[covered])
    # The Wald interval gave 0.7553 at 0.014 and 0.986, 0.8664 at 0.02.
    expect_gte(coverage, 0.95)
  }
  # The zero-count interval contains the proportion it is printed beside.
  expect_identical(ci[3, 1], 0)
  expect_identical(ci[4, n + 1], 1)
})

test_that("the STC comparator arm gets the same exact interval", {
  d <- .arm_data(30L, 0L)
  d <- suppressWarnings(add_integration(d, n_int = 16,
                                        x = distr(qbern, prob = x_mean),
                                        verbose = FALSE))
  s <- suppressWarnings(stc(d))
  nv <- suppressWarnings(naive(d))
  expect_equal(c(s$p_comparator_lower, s$p_comparator_upper),
               c(nv$p_comparator_lower, nv$p_comparator_upper),
               tolerance = 1e-12)
  expect_equal(s$p_comparator_upper, 0.03621669, tolerance = 1e-7)
})

test_that("the naive Poisson arm intervals cover at least the nominal level", {
  exposure <- 100
  counts <- 0:400
  ci <- vapply(counts, function(x) {
    ipd <- set_ipd(data.frame(trt = "A", y = c(x, rep(0L, 9)), E = rep(10, 10),
                              age = 1:10),
                   "trt", outcome = "y", covariates = "age",
                   family = "poisson", exposure = "E")
    agd <- set_agd(data.frame(trt = "B", r = x, E = exposure, age_mean = 5,
                              age_sd = 3),
                   "trt", family = "poisson", outcome_r = "r",
                   outcome_E = "E", cov_means = "age_mean", cov_sds = "age_sd",
                   cov_types = "continuous")
    s <- suppressWarnings(naive(combine_data(ipd, agd)))
    c(s$rate_index_lower, s$rate_index_upper, s$rate_comparator_lower,
      s$rate_comparator_upper)
  }, numeric(4))
  expect_equal(ci[1:2, ], ci[3:4, ], tolerance = 1e-12)
  for (rate in c(0.005, 0.01, 0.02, 0.05, 0.5, 2)) {
    w <- stats::dpois(counts, rate * exposure)
    expect_lt(1 - sum(w), 1e-12)
    covered <- ci[3, ] <= rate & ci[4, ] >= rate
    # The Wald interval gave 0.8636 at a rate of 0.02.
    expect_gte(sum(w[covered]), 0.95)
  }
  expect_identical(ci[3, 1], 0)
})

test_that("the naive contrast intervals are calibrated as documented", {
  # The contrasts stay Wald on the link scale around the boundary-corrected
  # quantities. Enumerating every pair of counts at n = 100 per arm gives
  # coverage between 0.94 and 0.999 over these true probabilities; the
  # values are recorded so a change in the method is a change in this test.
  n <- 100L
  d0 <- .arm_data(1L, 1L, n)
  grid <- array(NA_real_, c(n + 1L, n + 1L, 6L))
  for (r1 in 0:n) {
    for (r2 in 0:n) {
      d <- d0
      d$ipd$data$.outcome <- c(rep(1L, r1), rep(0L, n - r1))
      d$agd$data$.r <- r2
      s <- suppressWarnings(naive(d))
      grid[r1 + 1L, r2 + 1L, ] <- c(s$ci_lower, s$ci_upper, s$log_rr_lower,
                                    s$log_rr_upper, s$rd_lower, s$rd_upper)
    }
  }
  coverage <- function(p1, p2) {
    w <- outer(stats::dbinom(0:n, n, p1), stats::dbinom(0:n, n, p2))
    truth <- c(qlogis(p1) - qlogis(p2), log(p1 / p2), p1 - p2)
    vapply(1:3, function(k) {
      sum(w[grid[, , 2 * k - 1] <= truth[k] & grid[, , 2 * k] >= truth[k]])
    }, numeric(1))
  }
  # Each row: p1, p2, then the observed coverage of the log odds ratio,
  # the log risk ratio and the risk difference. A method change that
  # lowers any of them by more than 0.001 fails here.
  observed <- rbind(
    c(0.014, 0.014, 0.9999, 0.9999, 0.9930),
    c(0.020, 0.020, 0.9993, 0.9994, 0.9838),
    c(0.014, 0.050, 0.9766, 0.9766, 0.9388),
    c(0.050, 0.050, 0.9761, 0.9818, 0.9480),
    c(0.100, 0.300, 0.9553, 0.9573, 0.9474),
    c(0.500, 0.500, 0.9440, 0.9559, 0.9440),
    c(0.020, 0.500, 0.9659, 0.9595, 0.9417),
    c(0.980, 0.980, 0.9993, 0.9936, 0.9838),
    c(0.986, 0.986, 0.9999, 0.9982, 0.9930),
    c(0.986, 0.500, 0.9639, 0.9494, 0.9494)
  )
  for (i in seq_len(nrow(observed))) {
    cov <- coverage(observed[i, 1], observed[i, 2])
    expect_true(all(cov >= observed[i, 3:5] - 0.001),
                label = paste(observed[i, 1], observed[i, 2]))
  }
})
