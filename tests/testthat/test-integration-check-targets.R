# check_integration() compares a deterministic grid with the distribution it
# was asked to represent. Two things it got wrong: a binary margin's target
# SD was read from a supplied sample-SD column that no Bernoulli grid can
# reproduce, and a correlation summary was `close` when two of three pairs
# had never been measured.

test_that("a binary margin is judged against its distribution's SD", {
  # Two zeros and two ones have sample SD sqrt(1/3) = 0.577, a valid summary
  # of the source data. The Bernoulli(0.5) distribution has SD 0.5, and the
  # largest sample SD a binary grid of m points can have is
  # sqrt(m / (m - 1)) / 2, so the grid was 13% off at every resolution and
  # the verdict never left `review`.
  ip <- set_ipd(data.frame(trt = "A", y = c(0L, 1L, 1L, 0L),
                           x = c(0, 0, 1, 1)),
                "trt", outcome = "y", covariates = "x")
  with_sd <- set_agd(data.frame(trt = "B", n = 4L, r = 2L, x_mean = 0.5,
                                x_sd = stats::sd(c(0, 0, 1, 1))),
                     "trt", outcome_n = "n", outcome_r = "r",
                     cov_means = "x_mean", cov_sds = "x_sd",
                     cov_types = "binary")
  without_sd <- set_agd(data.frame(trt = "B", n = 4L, r = 2L, x_mean = 0.5),
                        "trt", outcome_n = "n", outcome_r = "r",
                        cov_means = "x_mean", cov_sds = NA_character_,
                        cov_types = "binary")
  for (m in c(64L, 1024L)) {
    checks <- lapply(list(with_sd, without_sd), function(ag) {
      d <- suppressWarnings(add_integration(
        combine_data(ip, ag), n_int = m, x = distr(qbern, prob = x_mean),
        verbose = FALSE
      ))
      suppressWarnings(check_integration(d, x = distr(qbern, prob = x_mean),
                                         check_joint = FALSE, verbose = FALSE))
    })
    for (ck in checks) {
      # Before: sd_target 0.577, rel_diff_sd_target 0.13, verdict `review`
      # at both resolutions. A 64-point Sobol grid puts 31 points at one
      # and its MEAN 1.6% off, which is a resolution finding of its own
      # and the only thing left for the verdict to review there.
      expect_equal(ck$marginals$sd_target, 0.5)
      expect_equal(ck$marginals$sd_current, 0.5, tolerance = 1e-3)
      expect_lt(ck$marginals$rel_diff_sd_target, 1e-3)
      if (m == 1024L) {
        expect_identical(ck$verdict$target_moments, "close")
      } else {
        expect_gt(ck$marginals$rel_diff_mean_target, 0.01)
      }
    }
    expect_identical(checks[[1]]$marginals, checks[[2]]$marginals)
  }
})

test_that("grid SDs are population SDs", {
  X <- array(c(rep(c(-1, 1), 32)), dim = c(1, 64, 1),
             dimnames = list(NULL, NULL, "x"))
  s <- mlumr:::.int_stats(X, "x", 1L)
  expect_equal(s$sd, 1)
  expect_false(isTRUE(all.equal(s$sd, stats::sd(rep(c(-1, 1), 32)))))
})

test_that("a correlation summary counts the pairs it could not measure", {
  # Three variables: orthogonal balanced x and y, and z constant on the
  # grid. One of three pairs is measured, with a difference of zero from a
  # zero target, and the summary used to say `close`.
  m <- cbind(x = rep(c(-1, -1, 1, 1), 16), y = rep(c(-1, 1, -1, 1), 16),
             z = rep(0, 64))
  original <- array(m, dim = c(1, 64, 3),
                    dimnames = list(NULL, NULL, colnames(m)))
  doubled <- array(rbind(m, m), dim = c(1, 128, 3),
                   dimnames = list(NULL, NULL, colnames(m)))
  tab <- mlumr:::.int_cor_stats(original, doubled, colnames(m), 1L,
                                cor_target = diag(3),
                                cor_method = "pearson")$diff
  expect_identical(sum(is.finite(tab$abs_diff_target)), 1L)
  expect_identical(mlumr:::.max_finite(tab$abs_diff_target), 0)
  stats <- mlumr:::.int_stats(original, colnames(m), 1L)
  # z declared with variance: the grid failed to vary it.
  pairs <- mlumr:::.int_cor_pair_status(tab, stats, c(0, 0, 0.01),
                                        c(1, 1, 0.0995))
  expect_identical(pairs$expected, 3L)
  expect_identical(pairs$measured, 1L)
  expect_identical(sort(pairs$omitted$pair), c("x~z", "y~z"))
  expect_identical(unique(pairs$omitted$reason), "constant_on_grid")
  # z declared with no variance: nothing to realize.
  pairs <- mlumr:::.int_cor_pair_status(tab, stats, c(0, 0, 0), c(1, 1, 0))
  expect_identical(unique(pairs$omitted$reason), "declared_degenerate")
})

test_that("check_integration() reports a partial correlation verdict", {
  # A binary covariate with prevalence 1e-6 never varies on a grid of 64
  # or 128 points, so its two pairs cannot be measured.
  ip <- set_ipd(data.frame(trt = "A", y = rbinom(40, 1, 0.5),
                           a = rnorm(40), b = rnorm(40),
                           rare = rbinom(40, 1, 0.5)),
                "trt", outcome = "y", covariates = c("a", "b", "rare"))
  ag <- set_agd(data.frame(trt = "B", n = 100L, r = 40L, a_mean = 0,
                           a_sd = 1, b_mean = 0, b_sd = 1, rare_mean = 1e-6),
                "trt", outcome_n = "n", outcome_r = "r",
                cov_means = c("a_mean", "b_mean", "rare_mean"),
                cov_sds = c("a_sd", "b_sd", NA),
                cov_types = c("continuous", "continuous", "binary"))
  d <- suppressWarnings(add_integration(
    combine_data(ip, ag), n_int = 64, cor = diag(3), cor_adjust = "pearson",
    a = distr(qnorm, mean = a_mean, sd = a_sd),
    b = distr(qnorm, mean = b_mean, sd = b_sd),
    rare = distr(qbern, prob = rare_mean), verbose = FALSE
  ))
  ck <- suppressWarnings(check_integration(
    d, cor = diag(3), cor_adjust = "pearson",
    a = distr(qnorm, mean = a_mean, sd = a_sd),
    b = distr(qnorm, mean = b_mean, sd = b_sd),
    rare = distr(qbern, prob = rare_mean), verbose = FALSE
  ))
  expect_identical(ck$verdict$target_correlation, "partial")
  expect_identical(ck$verdict$resolution_correlation, "partial")
  expect_identical(ck$correlation_pairs$expected, 3L)
  expect_identical(ck$correlation_pairs$measured, 1L)
  expect_identical(sort(ck$correlation_pairs$omitted$pair),
                   c("a~rare", "b~rare"))
  expect_identical(unique(ck$correlation_pairs$omitted$reason),
                   "constant_on_grid")
  out <- capture.output(suppressWarnings(check_integration(
    d, cor = diag(3), cor_adjust = "pearson",
    a = distr(qnorm, mean = a_mean, sd = a_sd),
    b = distr(qnorm, mean = b_mean, sd = b_sd),
    rare = distr(qbern, prob = rare_mean), verbose = TRUE
  )))
  expect_true(any(grepl("Pairs compared: 1 of 3", out)))
  expect_true(any(grepl("a~rare", out)))
})
