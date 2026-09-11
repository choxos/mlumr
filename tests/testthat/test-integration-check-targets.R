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
  # z declared with variance: the grid failed to vary it, on both grids.
  pairs <- mlumr:::.int_cor_pair_status(tab, stats, c(1, 1, 0.0995))
  expect_identical(pairs$expected, 3L)
  expect_identical(pairs$measured, 1L)
  expect_identical(pairs$measured_resolution, 1L)
  expect_identical(sort(pairs$omitted$pair), c("x~z", "y~z"))
  expect_identical(unique(pairs$omitted$reason), "constant_on_grid")
  expect_identical(nrow(pairs$not_applicable), 0L)
  # z declared with no variance: nothing to realize, so its pairs are not
  # expected and the one measured pair is the whole question.
  pairs <- mlumr:::.int_cor_pair_status(tab, stats, c(1, 1, 0))
  expect_identical(pairs$expected, 1L)
  expect_identical(pairs$measured, 1L)
  expect_identical(nrow(pairs$omitted), 0L)
  expect_identical(sort(pairs$not_applicable$pair), c("x~z", "y~z"))
  # A margin constant on the current grid but not on the doubled one: the
  # target comparison can read it, the resolution comparison cannot.
  m2 <- m
  m2[64, "z"] <- 1
  doubled2 <- array(rbind(m, m2), dim = c(1, 128, 3),
                    dimnames = list(NULL, NULL, colnames(m)))
  tab2 <- mlumr:::.int_cor_stats(original, doubled2, colnames(m), 1L,
                                 cor_target = diag(3),
                                 cor_method = "pearson")$diff
  pairs <- mlumr:::.int_cor_pair_status(tab2, stats, c(1, 1, 0.0995))
  expect_identical(pairs$measured, 3L)
  expect_identical(pairs$measured_resolution, 1L)
  expect_identical(unique(pairs$omitted$reason), "constant_on_current_grid")
})

test_that("check_integration() reports a partial correlation verdict", {
  # A binary covariate with prevalence 1e-6 never varies on a grid of 64
  # or 128 points, so its two pairs cannot be measured.
  make <- function(cor_ab = 0, n_int = 64, a_mean = 0.5, b_mean = 0.5) {
    set.seed(2026)
    ip <- set_ipd(data.frame(trt = "A", y = rbinom(40, 1, 0.5),
                             a = rbinom(40, 1, 0.5), b = rbinom(40, 1, 0.5),
                             rare = rbinom(40, 1, 0.5)),
                  "trt", outcome = "y", covariates = c("a", "b", "rare"))
    ag <- set_agd(data.frame(trt = "B", n = 100L, r = 40L, a_mean = a_mean,
                             b_mean = b_mean, rare_mean = 1e-6),
                  "trt", outcome_n = "n", outcome_r = "r",
                  cov_means = c("a_mean", "b_mean", "rare_mean"),
                  cov_sds = c(NA, NA, NA),
                  cov_types = c("binary", "binary", "binary"))
    cor <- diag(3)
    cor[1, 2] <- cor[2, 1] <- cor_ab
    dimnames(cor) <- list(c("a", "b", "rare"), c("a", "b", "rare"))
    d <- suppressWarnings(add_integration(
      combine_data(ip, ag), n_int = n_int, cor = cor, cor_adjust = "pearson",
      a = distr(qbern, prob = a_mean), b = distr(qbern, prob = b_mean),
      rare = distr(qbern, prob = rare_mean), verbose = FALSE
    ))
    list(data = d, cor = cor)
  }
  run <- function(made, verbose = FALSE) {
    suppressWarnings(check_integration(
      made$data, cor = made$cor, cor_adjust = "pearson",
      a = distr(qbern, prob = a_mean), b = distr(qbern, prob = b_mean),
      rare = distr(qbern, prob = rare_mean), verbose = verbose
    ))
  }
  ck <- run(make())
  expect_identical(ck$verdict$target_correlation, "partial")
  expect_identical(ck$verdict$resolution_correlation, "partial")
  expect_identical(ck$correlation_pairs$expected, 3L)
  expect_identical(ck$correlation_pairs$measured, 1L)
  expect_identical(sort(ck$correlation_pairs$omitted$pair),
                   c("a~rare", "b~rare"))
  expect_identical(unique(ck$correlation_pairs$omitted$reason),
                   "constant_on_grid")
  out <- capture.output(run(make(), verbose = TRUE))
  expect_true(any(grepl("Pairs measured: 1 of 3", out)))
  expect_true(any(grepl("a~rare", out)))
  # A measured pair that misses the heuristic is `review`, whatever else is
  # missing: a requested a~b correlation of 0.9 between margins of 0.1 and
  # 0.9, whose Frechet bound is 0.11.
  ck <- run(make(cor_ab = 0.9, a_mean = 0.1, b_mean = 0.9))
  expect_gt(max(ck$correlations$abs_diff_target, na.rm = TRUE), 0.05)
  expect_identical(ck$verdict$target_correlation, "review")
})

test_that("a margin declared without variance leaves the other pairs an ordinary verdict", {
  # Two AgD rows; the second is an all-male subgroup, so its age~sex pair
  # has no correlation to realize and is outside the count. The first
  # row's pair decides the verdict, which used to stay `partial` forever.
  set.seed(2026)
  ip <- set_ipd(data.frame(trt = "A", y = rbinom(40, 1, 0.5),
                           age = rnorm(40), sex = rbinom(40, 1, 0.5)),
                "trt", outcome = "y", covariates = c("age", "sex"))
  ag <- set_agd(data.frame(trt = "B", n = c(60L, 40L), r = c(20L, 15L),
                           age_mean = c(0, 0.2), age_sd = c(1, 1),
                           sex_mean = c(0.5, 0)),
                "trt", outcome_n = "n", outcome_r = "r",
                cov_means = c("age_mean", "sex_mean"),
                cov_sds = c("age_sd", NA), cov_types = c("continuous", "binary"))
  cor <- diag(2)
  dimnames(cor) <- list(c("age", "sex"), c("age", "sex"))
  d <- suppressWarnings(add_integration(
    combine_data(ip, ag), n_int = 256, cor = cor, cor_adjust = "pearson",
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    sex = distr(qbern, prob = sex_mean), verbose = FALSE
  ))
  ck <- suppressWarnings(check_integration(
    d, cor = cor, cor_adjust = "pearson",
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    sex = distr(qbern, prob = sex_mean), verbose = FALSE
  ))
  expect_identical(ck$correlation_pairs$expected, 1L)
  expect_identical(ck$correlation_pairs$measured, 1L)
  expect_identical(nrow(ck$correlation_pairs$omitted), 0L)
  expect_identical(ck$correlation_pairs$not_applicable$agd_row, 2L)
  expect_identical(ck$verdict$target_correlation, "close")
  expect_identical(ck$verdict$resolution_correlation, "stable")
  # A margin declared without variance but supplied a varying distribution
  # realizes a correlation that must not decide anything: one row, sex
  # declared all-male but drawn at prob = 0.5, has a finite age~sex
  # correlation on the grid and no pair with a correlation to realize. The
  # verdict is unavailable, where the realized value used to make it close.
  ag1 <- set_agd(data.frame(trt = "B", n = 60L, r = 20L, age_mean = 0,
                            age_sd = 1, sex_mean = 0),
                 "trt", outcome_n = "n", outcome_r = "r",
                 cov_means = c("age_mean", "sex_mean"),
                 cov_sds = c("age_sd", NA), cov_types = c("continuous", "binary"))
  d1 <- suppressWarnings(add_integration(
    combine_data(ip, ag1), n_int = 256, cor = cor, cor_adjust = "pearson",
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    sex = distr(qbern, prob = 0.5), verbose = FALSE
  ))
  ck1 <- suppressWarnings(check_integration(
    d1, cor = cor, cor_adjust = "pearson",
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    sex = distr(qbern, prob = 0.5), verbose = FALSE
  ))
  expect_true(is.finite(ck1$correlations$abs_diff_target))
  expect_identical(ck1$correlation_pairs$expected, 0L)
  expect_identical(ck1$verdict$target_correlation, "unavailable")
  expect_identical(ck1$verdict$resolution_correlation, "unavailable")
})
