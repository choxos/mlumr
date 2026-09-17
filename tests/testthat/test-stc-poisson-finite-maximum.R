# A Poisson STC with no events has no finite maximum likelihood estimate; the
# GLM stops anyway and reports convergence.

make_poisson <- function(y, x) {
  source <- data.frame(trt = "A", y = y, x = x, E = 1)
  ip <- set_ipd(source, treatment = "trt", outcome = "y", covariates = "x",
                family = "poisson", exposure = "E")
  ad <- data.frame(trt = "B", r = 20, E = 100, x_mean = mean(x),
                   x_sd = stats::sd(x))
  ag <- set_agd(ad, treatment = "trt", family = "poisson", outcome_r = "r",
                outcome_E = "E", cov_means = "x_mean", cov_sds = "x_sd",
                cov_types = "continuous")
  d <- combine_data(ip, ag)
  suppressWarnings(add_integration(
    d, n_int = 64, x = distr(stats::qnorm, mean = x_mean, sd = x_sd),
    verbose = FALSE
  ))
}

test_that("an index arm with no events is refused", {
  d <- make_poisson(rep(0L, 80), rep(c(-1, 1), each = 40))
  fit <- suppressWarnings(glm(.outcome ~ x + offset(log(.exposure)),
                              family = poisson(), data = d$ipd$data))
  expect_true(fit$converged)
  expect_error(suppressWarnings(stc(d)), "has no events")
})

test_that("ordinary Poisson data pass", {
  set.seed(2026)
  x <- rep(c(-1, 0, 1), 20)
  y <- rpois(60, exp(0.2 + 0.5 * x))
  skip_if(sum(y) == 0)
  s <- suppressWarnings(stc(make_poisson(y, x)))
  expect_s3_class(s, "mlumr_stc")
  expect_identical(s$separation$status, "not_applicable")
  expect_lt(s$se, 2)
})
