test_that("default engine is rstan", {
  withr::local_options(list(mlumr.stan_engine = NULL))
  expect_equal(mlumr_engine(), "rstan")
})

test_that("setting engine to rstan works", {
  withr::local_options(list(mlumr.stan_engine = NULL))
  mlumr_engine("rstan")
  expect_equal(mlumr_engine(), "rstan")
})

test_that("setting engine to invalid value errors", {
  expect_error(mlumr_engine("jags"), "`engine` must be 'rstan' or 'cmdstanr'")
  expect_error(mlumr_engine("c"), "`engine` must be 'rstan' or 'cmdstanr'")
  expect_error(mlumr_engine(c("rstan", "cmdstanr")),
               "`engine` must be 'rstan' or 'cmdstanr'")
})

test_that("get_engine returns option value", {
  withr::local_options(list(mlumr.stan_engine = "cmdstanr"))
  expect_equal(mlumr:::get_engine(), "cmdstanr")
})

test_that("get_engine rejects invalid option values", {
  withr::local_options(list(mlumr.stan_engine = "bad"))
  expect_error(mlumr:::get_engine(), "`engine` must be 'rstan' or 'cmdstanr'")
})

test_that("unchanged-engine message tolerates invalid option values", {
  withr::local_options(list(mlumr.stan_engine = "bad"))
  expect_message(value <- mlumr:::.message_engine_unchanged(),
                 "Engine unchanged \\(invalid current option\\)")
  expect_true(is.na(value))
})

test_that("get_engine defaults to rstan when option is NULL", {
  withr::local_options(list(mlumr.stan_engine = NULL))
  expect_equal(mlumr:::get_engine(), "rstan")
})


test_that("cmdstanr backend fits a model end-to-end", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if_not(
    tryCatch({
      cmdstanr::cmdstan_path()
      TRUE
    }, error = function(e) FALSE),
    "CmdStan not available"
  )

  set.seed(2026)
  n <- 50
  x1 <- rbinom(n, 1, 0.5)
  y <- rbinom(n, 1, plogis(-0.5 + 0.8 * x1))
  ipd_df <- data.frame(trt = "A", outcome = y, x1 = x1)
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 35, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  fit <- mlumr(dat, model = "spfa", engine = "cmdstanr",
               chains = 2, iter = 500, warmup = 250, refresh = 0, seed = 2026)

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$family, "binomial")
  expect_equal(fit$engine, "cmdstanr")
  expect_true("mu_index" %in% names(fit$draws))
  expect_true("variable" %in% names(fit$summary))
  expect_true("mean" %in% names(fit$summary))
  expect_true("Rhat" %in% names(fit$summary))
  expect_true(is.numeric(fit$diagnostics$n_divergent))
  expect_true(is.numeric(fit$diagnostics$n_max_treedepth))
  expect_false(file.exists(file.path("inst", "stan", "mlumr_binary_spfa")))
})

test_that("the pinned cmdstanr is told to upgrade only when its toolchain check fails", {
  # The repository DESCRIPTION pins serves 0.8.0, which does not recognize the
  # Rtools of current R versions; 0.9.0 from the maintained repository does.
  # The version alone does not decide it: 0.8.x on R 4.4 with Rtools44 builds
  # fine, so the decision is cmdstanr's own toolchain check.
  fails <- function() {
    stop("Rtools45 was not found but is required to run CmdStan with R version 4.5.1.")
  }
  passes <- function() invisible(TRUE)
  other <- function() stop("something else entirely")
  can <- function() TRUE
  cannot <- function() FALSE
  expect_true(.cmdstanr_too_old_for_windows("windows", "0.8.0", fails, can))
  expect_true(.cmdstanr_too_old_for_windows("windows", package_version("0.8.1"), fails, can))
  # Same old version, a toolchain that works: offer the installation.
  expect_false(.cmdstanr_too_old_for_windows("windows", "0.8.0", passes, can))
  # A different failure is not the known one and is not blamed on the version.
  expect_false(.cmdstanr_too_old_for_windows("windows", "0.8.0", other, can))
  # The same sentence from a machine where R itself cannot compile means
  # Rtools is absent, which no cmdstanr upgrade restores; cmdstanr's own
  # error, which names the missing Rtools, is the right one to reach the user.
  expect_false(.cmdstanr_too_old_for_windows("windows", "0.8.0", fails, cannot))
  # A current cmdstanr, another platform, or no cmdstanr: never, and without
  # touching the toolchain or the compiler at all.
  never <- function() stop("must not be consulted")
  expect_false(.cmdstanr_too_old_for_windows("windows", "0.9.0", never, never))
  expect_false(.cmdstanr_too_old_for_windows("unix", "0.8.0", never, never))
  expect_false(.cmdstanr_too_old_for_windows("windows", NULL, never, never))
})

test_that("the compiler probe reports what R CMD SHLIB can do here", {
  skip_on_cran()
  # This machine builds the package's own Stan code, so it can compile C.
  expect_true(.r_can_compile())
  # The probe cleans up after itself and leaves the working directory alone.
  before <- getwd()
  .r_can_compile()
  expect_identical(getwd(), before)
  expect_length(list.files(tempdir(), pattern = "^mlumr-probe-"), 0L)
})

test_that("a fit that selects cmdstanr by argument or option meets the same guard", {
  # mlumr_engine() is not the only way to select the backend: the option in a
  # profile and the per-fit argument both bypass it, and the first fit used to
  # reach compilation and fail there without the upgrade advice.
  testthat::local_mocked_bindings(
    .cmdstanr_too_old_for_windows = function(...) TRUE,
    .cmdstanr_upgrade_advice = function() "upgrade cmdstanr first"
  )
  expect_error(.resolve_mlumr_engine("cmdstanr"), "upgrade cmdstanr first")
  withr::local_options(mlumr.stan_engine = "cmdstanr")
  expect_error(.resolve_mlumr_engine(NULL), "upgrade cmdstanr first")
  # rstan is never held up by a cmdstanr problem.
  expect_identical(.resolve_mlumr_engine("rstan"), "rstan")
  testthat::local_mocked_bindings(.cmdstanr_too_old_for_windows = function(...) FALSE)
  expect_identical(.resolve_mlumr_engine("cmdstanr"), "cmdstanr")
})
