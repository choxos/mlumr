# A normal fit whose covariates reproduce the outcome exactly has an improper
# posterior for the residual SD, and the sampler does not report it.

.normal_stub <- function(y, x) {
  list(ipd = list(data = data.frame(.outcome = y, x = x)),
       covariates = "x")
}

test_that("an exactly fitting design is refused at any outcome scale", {
  for (scale in c(1e-6, 1, 1e6)) {
    d <- .normal_stub(c(0, 0, 1, 1) * scale, c(-0.5, -0.5, 0.5, 0.5))
    expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
  }
  # An exact fit with a large offset is still an exact fit.
  offset <- .normal_stub(1e15 + 3 * (0:49), 0:49)
  expect_error(mlumr:::.check_normal_residual_variation(offset), "improper")
})

test_that("a constant outcome is refused and a saturated design warns", {
  flat <- .normal_stub(rep(2, 5), c(1, 2, 3, 4, 5))
  expect_error(mlumr:::.check_normal_residual_variation(flat), "constant")
  saturated <- .normal_stub(c(1, 3), c(0, 1))
  expect_warning(mlumr:::.check_normal_residual_variation(saturated),
                 "as many free columns as rows")
})

test_that("ordinary data passes silently at any scale", {
  set.seed(2026)
  x <- rnorm(100)
  y <- 1 + 0.5 * x + rnorm(100)
  for (scale in c(1e-6, 1, 1e6)) {
    d <- .normal_stub(y * scale, x)
    expect_silent(mlumr:::.check_normal_residual_variation(d))
  }
})

test_that("a log link is judged on log(y) when every outcome is positive", {
  x <- c(-1, 0, 1, 2)
  d <- .normal_stub(exp(0.5 + x), x)
  expect_silent(mlumr:::.check_normal_residual_variation(d, "identity"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"), "improper")
  # Non-positive outcomes are left to the sampler.
  nonpositive <- .normal_stub(c(-1, 0, 1, 2), x)
  expect_silent(mlumr:::.check_normal_residual_variation(nonpositive, "log"))
})

test_that("mlumr() refuses before it reaches the engine", {
  local_mocked_bindings(
    .mlumr_fit_backend = function(...) stop("engine reached"),
    .package = "mlumr"
  )
  make_data <- function(y) {
    ipd <- set_ipd(data.frame(trt = "A", y = y, x = c(-0.5, -0.5, 0.5, 0.5)),
                   "trt", "y", "x", family = "normal")
    agd <- set_agd(data.frame(trt = "B", n_total = 100, y_mean = 0.4,
                              y_se = 0.1, x_mean = 0.1, x_sd = 0.5),
                   "trt", family = "normal", outcome_n = "n_total",
                   outcome_mean = "y_mean", outcome_se = "y_se",
                   cov_means = "x_mean", cov_sds = "x_sd")
    suppressWarnings(
      add_integration(combine_data(ipd, agd), n_int = 32,
                      x = distr(qnorm, mean = x_mean, sd = x_sd))
    )
  }
  expect_error(suppressWarnings(mlumr(make_data(c(0, 0, 1, 1)))), "improper")
  set.seed(2026)
  expect_error(suppressWarnings(mlumr(make_data(rnorm(4)))), "engine reached")
})
