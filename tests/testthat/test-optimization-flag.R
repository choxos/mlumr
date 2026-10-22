# `optimization` reaches the survival models as the data flag `reuse_arm_eta`,
# travels with the fit, and is replayed by prior_sensitivity(). The numerical
# equivalence of the two settings is tested natively by
# tests/parity/armfast-parity-gate.R, which runs every case under both.

.captured_stan_data <- function(dat, ...) {
  local_mocked_bindings(
    .mlumr_fit_backend = function(engine, model_name, stan_data, ...) {
      stop(structure(class = c("captured_stan_data", "condition"),
                     list(message = "captured", call = NULL,
                          stan_data = stan_data)))
    },
    .package = "mlumr"
  )
  res <- tryCatch(
    suppressWarnings(mlumr(dat, ..., chains = 1, iter = 2, warmup = 1,
                           refresh = 0, verbose = FALSE, seed = 2026)),
    captured_stan_data = function(cond) cond
  )
  expect_s3_class(res, "captured_stan_data")
  res$stan_data
}

test_that("survival fits pass the flag the argument sets", {
  dat <- sim_survival_data(n_ipd = 40, n_agd = 50, n_int = 8)
  for (distribution in c("weibull", "mspline")) {
    on <- .captured_stan_data(dat, distribution = distribution)
    off <- .captured_stan_data(dat, distribution = distribution,
                               optimization = FALSE)
    expect_identical(on$reuse_arm_eta, 1L, info = distribution)
    expect_identical(off$reuse_arm_eta, 0L, info = distribution)
    # Nothing else in the Stan data depends on the setting.
    on$reuse_arm_eta <- off$reuse_arm_eta <- NULL
    expect_identical(on, off, info = distribution)
  }
})

test_that("other families get no flag", {
  ipd <- set_ipd(data.frame(trt = "A", y = c(0, 1, 1, 0, 1, 0),
                            x = c(-1, 0.2, 0.5, -0.3, 1.1, 0.4)),
                 "trt", "y", "x", family = "binomial")
  agd <- set_agd(data.frame(trt = "B", n = 100, r = 40, x_mean = 0.1,
                            x_sd = 0.8),
                 "trt", outcome_n = "n", outcome_r = "r",
                 cov_means = "x_mean", cov_sds = "x_sd", family = "binomial")
  dat <- suppressWarnings(add_integration(
    combine_data(ipd, agd), n_int = 8,
    x = distr(qnorm, mean = x_mean, sd = x_sd), verbose = FALSE))
  sd <- .captured_stan_data(dat, optimization = FALSE)
  expect_false("reuse_arm_eta" %in% names(sd))
})

test_that("optimization must be one logical value", {
  dat <- sim_survival_data(n_ipd = 40, n_agd = 50, n_int = 8)
  for (bad in list(NA, "yes", c(TRUE, FALSE), 1)) {
    expect_error(mlumr(dat, optimization = bad, seed = 2026, verbose = FALSE),
                 "`optimization` must be TRUE or FALSE.", fixed = TRUE)
  }
})

test_that("prior_sensitivity() replays the setting, and older fits the old computation", {
  mk <- function(controls) {
    structure(
      list(family = "survival", model = "spfa", link = "log",
           engine = "rstan", data = list(covariates = "age"),
           distribution = "weibull",
           sampling_args = list(chains = 2, iter = 600, warmup = 300),
           model_controls = controls,
           surv_controls = list(aux_by = ".study", n_knots = 7L,
                                mspline_degree = NA_integer_),
           priors = list(intercept = prior_normal(0, 10),
                         beta = prior_normal(0, 2.5))),
      class = "mlumr_fit"
    )
  }
  replay <- function(controls) {
    mlumr:::.prior_sensitivity_args(mk(controls), prior_normal(0, 1),
                                    FALSE)$optimization
  }
  expect_false(replay(list(center = TRUE, qr = FALSE, optimization = FALSE)))
  expect_true(replay(list(center = TRUE, qr = FALSE, optimization = TRUE)))
  # A fit made before the argument existed used the computation without reuse.
  expect_false(replay(list(center = TRUE, qr = FALSE)))
})

# The package's own compiled models (the rstan path every CRAN user takes): one
# log density and one gradient under both settings, at the same points.
test_that("both settings give one log density through the package's models", {
  skip_on_cran()
  dat <- sim_survival_data(n_ipd = 60, n_agd = 80, n_int = 16)
  for (distribution in c("weibull", "mspline")) {
    for (model in c("spfa", "relaxed")) {
      info <- paste(distribution, model)
      fit_with <- function(optimization) {
        suppressWarnings(fit_survival_test(
          dat, model = model, distribution = distribution, chains = 1,
          iter = 40, warmup = 20, engine = "rstan", verbose = FALSE,
          optimization = optimization))
      }
      on <- fit_with(TRUE)
      off <- fit_with(FALSE)
      expect_true(on$model_controls$optimization, info = info)
      expect_false(off$model_controls$optimization, info = info)
      np <- rstan::get_num_upars(on$stanfit)
      u <- withr::with_seed(2026, matrix(stats::rnorm(3 * np, sd = 0.3), 3))
      for (k in seq_len(nrow(u))) {
        expect_equal(rstan::log_prob(off$stanfit, u[k, ]),
                     rstan::log_prob(on$stanfit, u[k, ]),
                     tolerance = 1e-10, info = info)
        expect_equal(rstan::grad_log_prob(off$stanfit, u[k, ]),
                     rstan::grad_log_prob(on$stanfit, u[k, ]),
                     tolerance = 1e-8, info = info)
      }
    }
  }
})

test_that("CmdStan reads the flag under both settings", {
  skip_on_cran()
  skip_if_not(cmdstan_is_usable(), "CmdStan is not usable")
  dat <- sim_survival_data(n_ipd = 60, n_agd = 80, n_int = 16)
  fits <- lapply(c(TRUE, FALSE), function(optimization) {
    suppressWarnings(fit_survival_test(
      dat, chains = 1, iter = 200, warmup = 100, engine = "cmdstanr",
      verbose = FALSE, optimization = optimization))
  })
  means <- vapply(fits, function(f) {
    f$summary$mean[f$summary$variable == "rmst_diff_comparator"]
  }, numeric(1))
  sds <- vapply(fits, function(f) {
    f$summary$sd[f$summary$variable == "rmst_diff_comparator"]
  }, numeric(1))
  expect_true(all(is.finite(means)))
  # One seed and one model: the draws may coincide, and at most drift apart.
  expect_lt(abs(diff(means)), 0.5 * max(sds))
})
