test_that("mlumr sampler controls are validated before backend dispatch", {
  expect_silent(.validate_mlumr_sampling_args(
    chains = 2,
    iter = 100,
    warmup = 50,
    seed = 123,
    adapt_delta = 0.95,
    max_treedepth = 15,
    refresh = 0
  ))

  expect_error(
    .validate_mlumr_sampling_args(0, 100, 50, NULL, 0.95, 15, 0),
    "`chains` must be a single integer >= 1"
  )
  expect_error(
    .validate_mlumr_sampling_args(2, 0, 0, NULL, 0.95, 15, 0),
    "`iter` must be a single integer >= 1"
  )
  expect_error(
    .validate_mlumr_sampling_args(2, 100, -1, NULL, 0.95, 15, 0),
    "`warmup` must be a single integer >= 0"
  )
  expect_error(
    .validate_mlumr_sampling_args(2, 100, 100, NULL, 0.95, 15, 0),
    "`warmup` must be smaller than `iter`"
  )
  expect_error(
    .validate_mlumr_sampling_args(2, 100, 50, -1, 0.95, 15, 0),
    "`seed` must be a single integer >= 0"
  )
  expect_error(
    .validate_mlumr_sampling_args(2, 100, 50, NULL, 1, 15, 0),
    "`adapt_delta` must be a single finite number between 0 and 1"
  )
  expect_error(
    .validate_mlumr_sampling_args(2, 100, 50, NULL, 0.95, 0, 0),
    "`max_treedepth` must be a single integer >= 1"
  )
  expect_error(
    .validate_mlumr_sampling_args(2, 100, 50, NULL, 0.95, 15, -1),
    "`refresh` must be a single integer >= 0"
  )
})


test_that("mlumr engine is scalar and supported", {
  old_engine <- getOption("mlumr.stan_engine")
  on.exit(options(mlumr.stan_engine = old_engine), add = TRUE)
  options(mlumr.stan_engine = "rstan")

  expect_equal(.resolve_mlumr_engine(NULL), "rstan")
  expect_equal(.resolve_mlumr_engine("cmdstanr"), "cmdstanr")
  expect_error(.resolve_mlumr_engine(c("rstan", "cmdstanr")),
               "`engine` must be 'rstan' or 'cmdstanr'")
  expect_error(.resolve_mlumr_engine(NA_character_),
               "`engine` must be 'rstan' or 'cmdstanr'")
  expect_error(.resolve_mlumr_engine("bad"),
               "`engine` must be 'rstan' or 'cmdstanr'")
  expect_error(.resolve_mlumr_engine("c"),
               "`engine` must be 'rstan' or 'cmdstanr'")
})


test_that("check_diagnostics ignores unavailable Rhat and ESS cleanly", {
  fit <- list(
    diagnostics = list(n_divergent = NA_real_, n_max_treedepth = NA_real_),
    summary = data.frame(variable = "x", Rhat = NA_real_, n_eff = NA_real_),
    sampling_args = list(adapt_delta = 0.95, max_treedepth = 15)
  )
  class(fit) <- c("mlumr_fit", "list")

  expect_warning(check_diagnostics(fit), NA)
})


test_that("check_diagnostics still warns on finite diagnostic problems", {
  fit <- list(
    diagnostics = list(n_divergent = 2, n_max_treedepth = 3),
    summary = data.frame(variable = "x", Rhat = 1.06, n_eff = 100),
    sampling_args = list(adapt_delta = 0.8, max_treedepth = 10)
  )
  class(fit) <- c("mlumr_fit", "list")

  expect_warning(check_diagnostics(fit), "divergent transitions")
  fit$diagnostics$n_divergent <- 0
  expect_warning(check_diagnostics(fit), "max treedepth")
  fit$diagnostics$n_max_treedepth <- 0
  expect_warning(check_diagnostics(fit), "Rhat values > 1.05")
  fit$summary$Rhat <- 1
  expect_warning(check_diagnostics(fit), "ESS values < 400")
})
