expose_family_numerical_functions <- function() {
  stan_dir <- system.file("stan", package = "mlumr")
  skip_if(stan_dir == "", "installed Stan includes not found")
  code <- paste(
    "functions {",
    "#include include/numerical_functions.stan",
    "#include include/binary_functions.stan",
    "}",
    sep = "\n"
  )
  env <- new.env()
  suppressWarnings(
    rstan::expose_stan_functions(
      rstan::stanc(model_code = code, isystem = stan_dir,
                   allow_undefined = TRUE),
      env = env
    )
  )
  env
}

test_that("binary log probabilities remain exact for extreme predictors", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_family_numerical_functions()

  expect_equal(env$log_event_prob_binary(-20, 2L),
               pnorm(-20, log.p = TRUE), tolerance = 1e-12)
  expect_equal(env$log_event_prob_binary(-800, 3L), -800,
               tolerance = 1e-12)
  expect_equal(env$log_nonevent_prob_binary(10, 3L), -exp(10),
               tolerance = 1e-12)

  eta <- c(-1000, -750)
  expected <- max(eta) + log(sum(exp(eta - max(eta)))) - log(length(eta))
  expect_equal(env$log_mean_event_binary(eta, 1L), expected,
               tolerance = 1e-12)
  expect_equal(env$integrated_binomial_lpmf(1L, 1L, eta, 1L), expected,
               tolerance = 1e-12)
  expect_equal(env$integrated_binomial_lpmf(0L, 1L, rep(10, 2), 3L),
               -exp(10), tolerance = 1e-12)
})

test_that("log-scale mean and contrast helpers preserve finite results", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_family_numerical_functions()

  x <- c(800, 799)
  expected_mean <- 800 + log((1 + exp(-1)) / 2)
  expect_equal(env$log_mean_exp_vec(x), expected_mean, tolerance = 1e-12)

  weights <- c(1, 3)
  expected_weighted <- 800 + log((1 + 3 * exp(-1)) / 4)
  expect_equal(env$log_weighted_mean_exp_vec(x, weights), expected_weighted,
               tolerance = 1e-12)

  expected_difference <- exp(710 + log1p(-exp(709.999 - 710)))
  expect_true(is.finite(expected_difference))
  expect_equal(env$exp_difference(710, 709.999), expected_difference,
               tolerance = 1e-11)
  expect_equal(env$exp_difference(710, 710), 0)
})

test_that("family Stan models aggregate on the log scale", {
  stan_dir <- system.file("stan", package = "mlumr")
  skip_if(stan_dir == "", "installed Stan sources not found")
  models <- file.path(
    stan_dir,
    paste0("mlumr_", rep(c("binary", "normal", "poisson"), each = 2),
           "_", rep(c("spfa", "relaxed"), 3), ".stan")
  )
  code <- lapply(models, readLines, warn = FALSE)

  expect_true(all(vapply(code, function(x) any(grepl(
    "log_mean_", x, fixed = TRUE)), logical(1))))
  expect_false(any(vapply(code, function(x) any(grepl(
    "safe_divide", x, fixed = TRUE)), logical(1))))
  expect_false(any(vapply(code, function(x) any(grepl(
    "mean(exp(", x, fixed = TRUE)), logical(1))))
})

test_that("R log-scale helpers preserve extreme marginal contrasts", {
  expect_equal(
    mlumr:::.weighted_log_mean_exp(c(800, 799), c(1e308, 1e308)),
    800 + log((1 + exp(-1)) / 2),
    tolerance = 1e-12
  )
  expect_equal(mlumr:::.normalize_weights(c(1e308, 1e308)), c(0.5, 0.5))

  low <- mlumr:::.binary_log_probs(c(-750, -751), "logit")
  expect_equal(exp(low$event[1] - low$event[2]), exp(1), tolerance = 1e-12)
})
