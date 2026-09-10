# A Poisson STC whose likelihood has no finite maximum. The log-likelihood is
# sum(y eta - E exp(eta)); along a direction of the coefficients that leaves
# every positive-count row's rate fixed and lowers a zero-count row's, it
# rises without bound, and iterative reweighting stops anyway when the
# deviance stops changing. What comes back is where it stopped.

make_poisson <- function(y, x, binary = FALSE, target_mean = NULL) {
  source <- data.frame(trt = "A", y = y, x = x, E = 1)
  ip <- set_ipd(source, treatment = "trt", outcome = "y", covariates = "x",
                family = "poisson", exposure = "E")
  ad <- data.frame(trt = "B", r = 20, E = 100,
                   x_mean = if (is.null(target_mean)) mean(x) else target_mean,
                   x_sd = stats::sd(x))
  ag <- set_agd(ad, treatment = "trt", family = "poisson", outcome_r = "r",
                outcome_E = "E", cov_means = "x_mean",
                cov_sds = if (binary) NA_character_ else "x_sd",
                cov_types = if (binary) "binary" else "continuous")
  d <- combine_data(ip, ag)
  suppressWarnings(if (binary) {
    add_integration(d, n_int = 64, x = distr(qbern, prob = x_mean),
                    verbose = FALSE)
  } else {
    add_integration(d, n_int = 64, x = distr(stats::qnorm, mean = x_mean,
                                             sd = x_sd), verbose = FALSE)
  })
}

test_that("an index arm with no events is refused, not reported", {
  d <- make_poisson(rep(0L, 80), rep(c(-1, 1), each = 40))
  # The GLM itself stops with finite numbers and reports convergence, which
  # is the whole problem.
  fit <- suppressWarnings(glm(.outcome ~ x + offset(log(.exposure)),
                              family = poisson(), data = d$ipd$data))
  expect_true(fit$converged)
  expect_true(all(is.finite(coef(fit))))
  expect_error(suppressWarnings(stc(d)), "no events")
})

test_that("a subgroup with no events beside events elsewhere is refused", {
  # Zero counts at x = 0 and positive counts at x = 1: the direction
  # (b0 - t, b1 + t) leaves the x = 1 rates fixed and lowers the x = 0 ones,
  # so the likelihood rises without bound. The target population sits at
  # the affected profile, and the transcribed iteration stops at a slope
  # near 21 with a standard error in the thousands.
  d <- make_poisson(c(rep(0L, 40), rep(1:4, 10)), rep(c(0, 1), each = 40),
                    binary = TRUE, target_mean = 0)
  expect_error(suppressWarnings(stc(d)), "no finite maximum")
  expect_error(suppressWarnings(stc(d)), "subgroup with no events")
})

test_that("ordinary Poisson data pass and the result says the maximum is finite", {
  set.seed(2026)
  x <- rnorm(60)
  y <- rpois(60, exp(0.2 + 0.5 * x))
  skip_if(sum(y) == 0L)
  d <- make_poisson(y, x)
  s <- suppressWarnings(stc(d))
  expect_s3_class(s, "mlumr_stc")
  expect_identical(s$separation$status, "not_applicable")
  expect_match(s$separation$reason, "finite")
  expect_true(is.finite(s$se))
  # Zero counts beside positive counts at the SAME profiles pin nothing
  # down: the positive rows span the design, so no direction moves the
  # zero rows alone.
  y2 <- y
  y2[seq(1, 60, by = 3)] <- 0L
  skip_if(sum(y2) == 0L)
  expect_s3_class(suppressWarnings(stc(make_poisson(y2, x))), "mlumr_stc")
})

test_that("the weak boundary question drops pinned rows and needs no strictness", {
  # Positive rows all at x = 0, zero rows at x = 0 and x = 1. The zero row
  # at x = 0 is pinned. For the normal log link that blocks the ray, since
  # every zero row must be lowered; for the Poisson likelihood the row
  # simply drops out and the x = 1 row still recedes.
  X_pos <- cbind(1, c(0, 0))
  X_zero <- cbind(1, c(0, 1))
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero), "unreachable")
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero, strict = FALSE),
                   "reachable")
  # Zero rows on opposite sides recede in opposite directions under either
  # reading.
  X_zero <- cbind(1, c(-1, 1))
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero, strict = FALSE),
                   "unreachable")
  # Positive rows spanning the design leave no free direction.
  X_pos <- cbind(1, c(0, 1))
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero, strict = FALSE),
                   "unreachable")
})
