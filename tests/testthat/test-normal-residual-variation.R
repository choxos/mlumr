# A normal fit whose covariates reproduce the outcome exactly has an improper
# posterior for the residual SD: marginalizing the coefficients leaves a density
# behaving as sigma^(rank - n) near zero, which does not integrate. Nothing in
# the sampler reports that. It drifts toward zero and returns where it stopped.

.normal_stub <- function(y, x) {
  list(ipd = list(data = data.frame(.outcome = y, x = x)),
       covariates = "x")
}

test_that("an exactly fitting design is refused", {
  d <- .normal_stub(c(0, 0, 1, 1), c(-0.5, -0.5, 0.5, 0.5))
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("the verdict does not depend on the units of the outcome", {
  y <- c(0, 0, 1, 1)
  x <- c(-0.5, -0.5, 0.5, 0.5)
  for (scale in c(1e-6, 1, 1e6)) {
    scaled <- .normal_stub(y * scale, x)
    expect_error(mlumr:::.check_normal_residual_variation(scaled), "improper")
  }
  # An ordinary fit stays acceptable at every scale too.
  set.seed(2026)
  yy <- rnorm(50)
  xx <- rnorm(50)
  for (scale in c(1e-6, 1, 1e6)) {
    scaled <- .normal_stub(yy * scale, xx)
    expect_silent(mlumr:::.check_normal_residual_variation(scaled))
  }
})

test_that("a constant outcome and a saturated design are named for what they are", {
  flat <- .normal_stub(rep(2, 5), c(1, 2, 3, 4, 5))
  expect_error(mlumr:::.check_normal_residual_variation(flat), "constant")
  # Two rows, intercept plus one covariate: rank equals n.
  # Two rows, intercept plus one covariate: rank equals n. That leaves the
  # residual SD to the prior, but the marginal density is sigma^0 times a prior
  # that integrates, so the posterior is proper and this warns, not refuses.
  saturated <- .normal_stub(c(1, 3), c(0, 1))
  expect_warning(mlumr:::.check_normal_residual_variation(saturated),
                 "no residual degrees of freedom")
  expect_warning(mlumr:::.check_normal_residual_variation(saturated),
                 "still")
})

test_that("a nearly exact fit warns instead of refusing", {
  set.seed(2026)
  x <- rnorm(60)
  y <- 2 + 3 * x + rnorm(60, sd = 1e-5)
  ratio <- sum(residuals(lm(y ~ x))^2) / sum((y - mean(y))^2)
  # The fixture has to land between the two thresholds for the test to mean
  # anything.
  expect_gt(ratio, 60 * .Machine$double.eps^2)
  expect_lt(ratio, 1e-6)
  expect_warning(mlumr:::.check_normal_residual_variation(.normal_stub(y, x)),
                 "concentrated hard against zero")
})

test_that("ordinary data passes silently", {
  set.seed(2026)
  x <- rnorm(100)
  y <- 1 + 0.5 * x + rnorm(100)
  expect_silent(mlumr:::.check_normal_residual_variation(.normal_stub(y, x)))
})

test_that("mlumr() refuses before it reaches the engine", {
  # No Stan model is compiled and no engine is chosen: the refusal happens in
  # validation, which is what makes it cheap and unambiguous.
  ipd <- set_ipd(data.frame(trt = "A", y = c(0, 0, 1, 1),
                            x = c(-0.5, -0.5, 0.5, 0.5)),
                 "trt", "y", "x", family = "normal")
  agd <- set_agd(data.frame(trt = "B", n_total = 100, y_mean = 0.4,
                            y_se = 0.1, x_mean = 0.1, x_sd = 0.5),
                 "trt", family = "normal", outcome_n = "n_total",
                 outcome_mean = "y_mean", outcome_se = "y_se",
                 cov_means = "x_mean", cov_sds = "x_sd")
  dat <- suppressWarnings(
    add_integration(combine_data(ipd, agd), n_int = 32,
                    x = distr(qnorm, mean = x_mean, sd = x_sd))
  )
  expect_error(suppressWarnings(mlumr(dat, family = "normal")), "improper")
})

test_that("a log link is tested where an exact log-link fit would show", {
  # y = exp(eta) exactly. The residual sum of squares in y is not zero and the
  # identity-scale test sees nothing, but the posterior is improper all the
  # same, so the test goes on log(y).
  # Three or more distinct covariate values, so the exponential curve is not
  # also a straight line through the points and the two scales disagree.
  x <- c(-1, 0, 1, 2)
  y <- exp(0.5 + x)
  d <- .normal_stub(y, x)
  expect_silent(mlumr:::.check_normal_residual_variation(d, "identity"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"), "improper")

  # A non-positive outcome cannot come from a log link at all.
  expect_silent(
    mlumr:::.check_normal_residual_variation(.normal_stub(c(-1, 0, 1, 2), x),
                                             "log"))
})

test_that("the log link is measured on the scale its likelihood uses", {
  # normal(exp(theta), sigma) puts the residual on the RESPONSE scale. Judging
  # it by the OLS residual of log(y) measures relative error instead: here the
  # log-scale residual is ordinary, while the response-scale fit reproduces
  # every large observation exactly and leaves almost nothing behind.
  x <- seq(-20, 20, length.out = 25)
  y <- exp(x)
  y[which.min(y)] <- exp(-10)

  log_ratio <- sum(residuals(lm(log(y) ~ x))^2) / sum((log(y) - mean(log(y)))^2)
  expect_gt(log_ratio, 1e-6)

  d <- list(ipd = list(data = data.frame(.outcome = y, x = x)),
            covariates = "x")
  # The moved point is genuinely off the curve, so this residual is real, not
  # rounding: it is 8.95e-27 where the same design fitted to points exactly on
  # the curve leaves 3.06e-29. The posterior is proper and this warns.
  expect_warning(mlumr:::.check_normal_residual_variation(d, "log"),
                 "concentrated hard against zero")

  # Its companion, and the pair is what pins the log branch: identical design,
  # every point on the curve, so the fit is exact and the posterior is not.
  on_curve <- list(ipd = list(data = data.frame(.outcome = exp(x), x = x)),
                   covariates = "x")
  expect_error(mlumr:::.check_normal_residual_variation(on_curve, "log"),
               "improper")
})

test_that("extreme units neither underflow nor overflow the test", {
  # Both sums are squares. Unscaled, 1e-170 underflows them to zero (a varying
  # outcome read as constant) and 1e160 overflows them to Inf (ratio NaN, and
  # the comparison that follows errors rather than deciding).
  set.seed(2026)
  x <- rnorm(40)
  y <- 1 + 0.5 * x + rnorm(40)
  for (scale in c(1e-170, 1e160)) {
    d <- .normal_stub(y * scale, x)
    expect_silent(mlumr:::.check_normal_residual_variation(d))
  }
  # And an exact fit is still caught at those units.
  xe <- c(-0.5, -0.5, 0.5, 0.5)
  ye <- c(0, 0, 1, 1)
  for (scale in c(1e-170, 1e160)) {
    d <- .normal_stub(ye * scale, xe)
    expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
  }
})

test_that("a near-collinear column is not dropped before the residual is taken", {
  # x2 differs from x1 by 1e-8 * z, and y is exactly that difference. The full
  # design reproduces y with coefficients (-1, 1); lm.fit()'s default pivot
  # tolerance discards x2 and measures a large residual against what is left.
  #
  # This case and "a small but real residual" below are the pair that fixes the
  # refusal floor. Both are ordinary data, their residual ratios are only about
  # 400 apart (3e-16 here, 1e-13 there), and they must be decided in opposite
  # directions. No fixed threshold does that. What separates them is the size
  # of the fitted coefficients: about 4e7 here, since the design reaches y only
  # by cancelling two huge terms, against 3 there. The rounding a residual can
  # carry scales with them.
  set.seed(2026)
  n <- 60
  x1 <- rnorm(n)
  z <- rnorm(n)
  x2 <- x1 + 1e-8 * z
  y <- x2 - x1
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("a small but real residual is proper, and warns rather than errors", {
  # Only an exactly zero residual is improper: for any positive one the
  # exp(-RSS / (2 sigma^2)) factor drives the density to zero as sigma does and
  # the integral converges. A ratio around 1e-13 is readily representable, so
  # refusing it would reject a valid, highly predictive dataset.
  set.seed(2026)
  n <- 50
  x <- rnorm(n)
  y <- 2 + 3 * x + rnorm(n, sd = 1e-6)
  ratio <- sum(residuals(lm(y ~ x))^2) / sum((y - mean(y))^2)
  expect_gt(ratio, n * .Machine$double.eps^2)
  expect_lt(ratio, 1e-6)

  d <- .normal_stub(y, x)
  expect_warning(mlumr:::.check_normal_residual_variation(d),
                 "concentrated hard against zero")
  expect_error(mlumr:::.check_normal_residual_variation(d), NA)
})

test_that("the verdict does not depend on the units of a predictor", {
  # Multiplying a predictor by 1e12 leaves the fitted mean surface and the
  # propriety question untouched. Taking kappa() on the raw design would let it
  # drive the floor to its cap and hard-reject a genuine residual.
  set.seed(2026)
  n <- 50
  x <- rnorm(n)
  y <- 2 + 3e-12 * (x * 1e12) + rnorm(n, sd = 1e-6)
  for (unit in c(1, 1e12)) {
    d <- .normal_stub(y, x * unit)
    expect_warning(mlumr:::.check_normal_residual_variation(d),
                   "concentrated hard against zero")
    expect_error(mlumr:::.check_normal_residual_variation(d), NA)
  }
})

test_that("a near-collinear column survives the log-link check too", {
  # glm.fit() takes its pivot tolerance as min(1e-7, epsilon / 1000), so the
  # default drops x2 and reports an ordinary residual for a design that fits
  # exactly through it.
  set.seed(2026)
  n <- 60
  x1 <- rnorm(n)
  z <- rnorm(n)
  x2 <- x1 + 1e-12 * z
  y <- exp(x2 - x1)
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"), "improper")
})

test_that("an outcome with a large offset keeps its exact fit", {
  # y is stored exactly and fitted exactly by the intercept and x. Dividing by
  # its own maximum would push the whole 147-wide spread into the last few
  # digits of a value near 1 and read it back as noise.
  x <- 0:49
  y <- 1e15 + 3 * x
  d <- .normal_stub(y, x)
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("a redundant column does not make the design look well conditioned", {
  # kappa() is singular here, and treating that as perfectly conditioned drops
  # the floor to the well-conditioned scale, where this exact fit's numerical
  # residual is large enough to pass.
  set.seed(2026)
  n <- 60
  x1 <- rnorm(n)
  z <- rnorm(n)
  x2 <- x1 + 1e-8 * z
  d <- list(
    ipd = list(data = data.frame(.outcome = x2 - x1, x1 = x1, x2 = x2,
                                 x3 = rep(1, n))),
    covariates = c("x1", "x2", "x3")
  )
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("an offset predictor does not inflate the numerical zero", {
  # Shifting a predictor does not change the fitted column space, but it does
  # force the intercept to cancel the offset. Counting that cancellation as
  # rounding pushes the floor above a genuine residual and refuses a proper
  # posterior.
  set.seed(2026)
  n <- 50
  x <- rnorm(n)
  y <- 2 + 3 * x + rnorm(n, sd = 1e-6)
  for (shift in c(0, 1e12)) {
    d <- .normal_stub(y, x + shift)
    expect_warning(mlumr:::.check_normal_residual_variation(d),
                   "concentrated hard against zero")
    expect_error(mlumr:::.check_normal_residual_variation(d), NA)
  }
})

test_that("an outcome spanning both extremes does not overflow the shift", {
  # y - min(y) is Inf here, and a non-finite response aborts the fit in a
  # low-level error rather than a diagnosis.
  y <- c(-1e308, -1e308, 1e308, 1e308)
  x <- c(-0.5, -0.5, 0.5, 0.5)
  d <- .normal_stub(y, x)
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("a wide log-link outcome is normalized without manufacturing zeros", {
  # Dividing by the maximum underflowed the small end to exactly zero, and zero
  # is not a value a log link can start from. Centering log(y) keeps every
  # value positive, which is what the fit needs to run at all.
  x <- c(-1, -1, 1, 1)
  y <- exp(150 * x)
  expect_equal(min(y) / max(y), 0)

  log_y <- log(y)
  centered <- exp(log_y - mean(log_y))
  expect_true(all(centered > 0))

  d <- .normal_stub(y, x)
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"), "improper")
})

test_that("a check that cannot run says so instead of passing quietly", {
  # Past about 700 log units of span the fit overflows internally whatever the
  # normalization, so there is no verdict to give. Returning quietly would let
  # an exactly fitting design reach the sampler unremarked, which is the case
  # this whole check exists to catch, so it reports that it could not decide.
  x <- c(-1, -1, 1, 1)
  d <- .normal_stub(exp(400 * x), x)
  expect_warning(mlumr:::.check_normal_residual_variation(d, "log"),
                 "could not be checked")
})
