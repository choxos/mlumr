# Generalized-gamma upper-tail regression tests, at the Stan level.
#
# The generalized-gamma survival function is the regularized upper incomplete
# gamma Q(k, w). Evaluating it as log(gamma_q(k, w)) forms the probability
# first and takes its logarithm second, so the result collapses to -Inf as soon
# as that probability underflows, even though its logarithm is finite and
# ordinary. With k = 1, sigma = 1, mu = 0 the distribution is a unit-rate
# exponential, log S(t) = -t and h(t) = 1, and the natural-scale evaluation
# underflows from about t = 746 upward. A -Inf log survival is not a harmless
# rounding of a tiny number: log_mean_haz() subtracts it from a separately
# computed finite log density, turning the log hazard into +Inf, and it makes a
# right-censored likelihood contribution -Inf rather than -746.
#
# These tests pin the exponential special case, where the exact answer is known
# in closed form, and check the log-scale continued fraction against R's own
# log-scale pgamma() over the range of shapes and arguments a fit can reach.
# The R mirror (.r_log_surv) is not a substitute: it goes through
# pgamma(log.p = TRUE), which is a different numerical path from Stan's.

expose_survival_functions <- function() {
  stan_dir <- system.file("stan", package = "mlumr")
  skip_if(stan_dir == "" ||
            !file.exists(file.path(stan_dir, "include", "survival_functions.stan")),
          "installed Stan include not found")
  code <- "functions {\n#include include/survival_functions.stan\n}\n"
  env <- new.env()
  suppressWarnings(
    rstan::expose_stan_functions(
      rstan::stanc(model_code = code, isystem = stan_dir, allow_undefined = TRUE),
      env = env
    )
  )
  env
}

test_that("generalized-gamma log survival is finite far into the upper tail", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_functions()

  # aux2 = k = 1, aux = sigma = 1, eta = mu = 0  =>  unit-rate exponential.
  # Includes t = 1000, well inside the range where the natural-scale upper tail
  # underflows to zero, and t = 746, one step past where it first does.
  for (t in c(1, 10, 100, 700, 745, 746, 1000, 1e4, 1e6)) {
    expect_equal(env$log_surv_scalar(9L, t, 0, 1, 1), -t,
                 tolerance = 1e-8, info = sprintf("t = %g", t))
    # log h = log f - log S must be 0 for a unit-rate exponential. Before the
    # fix this was +Inf wherever log S had collapsed.
    expect_equal(env$log_haz_full(9L, t, 0, 1, 1), 0,
                 tolerance = 1e-8, info = sprintf("t = %g", t))
  }
})

test_that("generalized-gamma likelihood contributions stay finite in the tail", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_functions()

  # Right-censored at t = 1000: log S(1000) = -1000, not -Inf.
  expect_equal(env$surv_ll_status(9L, 1000, 0, 0, 0L, 0, 1, 1), -1000,
               tolerance = 1e-8)
  # Event at t = 1000: log f = log h + log S = -1000.
  expect_equal(env$surv_ll_status(9L, 1000, 0, 0, 1L, 0, 1, 1), -1000,
               tolerance = 1e-8)
  # Interval-censored on (500, 1000]: log(S(500) - S(1000)), dominated by S(500).
  expect_equal(env$surv_ll_status(9L, 1000, 500, 0, 3L, 0, 1, 1),
               -500 + log1p(-exp(-500)), tolerance = 1e-8)

  # Marginal (population-standardized) hazard and survival over several linear
  # predictors: both finite, and the marginal hazard tends to the smallest
  # arm-specific hazard exp(-max(eta)) deep in the tail.
  etas <- c(-0.2, 0, 0.3)
  for (t in c(100, 1000, 1e5)) {
    expect_true(is.finite(env$log_mean_surv(9L, t, etas, 1, 1)),
                info = sprintf("t = %g", t))
    expect_true(is.finite(env$log_mean_haz(9L, t, etas, 1, 1)),
                info = sprintf("t = %g", t))
  }
  expect_equal(env$log_mean_haz(9L, 1e5, etas, 1, 1), -0.3, tolerance = 1e-6)
})

test_that("the log-scale upper incomplete gamma matches pgamma(log.p = TRUE)", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_functions()

  grid <- expand.grid(
    k = c(0.05, 0.2, 0.5, 1, 2, 5, 10, 50, 200),
    x = c(1.5, 3, 10, 30, 100, 745, 1000, 1e4, 1e6, 1e12, 1e100)
  )
  # The continued fraction is used, and is convergent, on x > k + 1.
  grid <- grid[grid$x > grid$k + 1, ]
  got <- mapply(function(k, x) env$log_gamma_q_cf(k, x), grid$k, grid$x)
  want <- mapply(
    function(k, x) stats::pgamma(x, shape = k, lower.tail = FALSE, log.p = TRUE),
    grid$k, grid$x
  )
  expect_true(all(is.finite(got)))
  expect_equal(max(abs(got - want) / pmax(abs(want), 1)), 0, tolerance = 1e-12)
})

test_that("generalized-gamma survival is continuous across the evaluation switch", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_functions()

  # Boost's gamma_q() is used at w <= k + 1 and the continued fraction above it.
  # The two must agree at the boundary or the likelihood has a seam in it.
  # With aux = 1 and eta = 0, w = k * t, so t = (k + 1) / k puts w on the switch.
  for (k in c(0.3, 1, 2.5, 7)) {
    t_switch <- (k + 1) / k
    below <- env$log_surv_scalar(9L, t_switch * (1 - 1e-9), 0, 1, k)
    above <- env$log_surv_scalar(9L, t_switch * (1 + 1e-9), 0, 1, k)
    expect_equal(below, above, tolerance = 1e-7, info = sprintf("k = %g", k))
  }
})

test_that("analytic log survival formulas stay finite under cancelling extremes", {
  exp_tail <- mlumr:::.r_log_surv(1L, exp(700), -1000, 1, 1)
  expect_lt(exp_tail, 0)
  expect_equal(exp_tail, -exp(-300), tolerance = 1e-12)
  expect_equal(mlumr:::.r_log_surv(2L, exp(400), -800, 2, 1), -1,
               tolerance = 1e-12)
  expect_equal(mlumr:::.r_log_surv(5L, exp(400), 400, 2, 1), -1,
               tolerance = 1e-12)
  expect_equal(mlumr:::.r_log_surv(3L, 800, -800, 1, 1), -1,
               tolerance = 1e-12)
  expect_equal(mlumr:::.r_log_surv(7L, exp(400), 0, 2, 1), -800,
               tolerance = 1e-12)
  expect_equal(mlumr:::.r_log_surv(8L, exp(400), 800, 2, 1), 0,
               tolerance = 1e-12)
})

test_that("Stan analytic log survival formulas match their finite limits", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_functions()

  exp_tail <- env$log_surv_scalar(1L, exp(700), -1000, 1, 1)
  expect_lt(exp_tail, 0)
  expect_equal(exp_tail, -exp(-300), tolerance = 1e-12)
  expect_equal(env$log_surv_scalar(2L, exp(400), -800, 2, 1), -1,
               tolerance = 1e-12)
  expect_equal(env$log_surv_scalar(5L, exp(400), 400, 2, 1), -1,
               tolerance = 1e-12)
  expect_equal(env$log_surv_scalar(3L, 800, -800, 1, 1), -1,
               tolerance = 1e-12)
  expect_equal(env$log_surv_scalar(7L, exp(400), 0, 2, 1), -800,
               tolerance = 1e-12)
  expect_equal(env$log_surv_scalar(8L, exp(400), 800, 2, 1), 0,
               tolerance = 1e-12)
  expect_equal(env$log_haz_scalar(7L, exp(400), 0, 2, 1),
               log(2) - 400, tolerance = 1e-12)

  huge_shape <- 1e306
  expect_equal(env$log_haz_scalar(2L, exp(1), -huge_shape, huge_shape, 1),
               log(huge_shape) - 1, tolerance = 1e-12)
  expect_equal(env$log_haz_scalar(5L, exp(400), 400, huge_shape, 1),
               log(huge_shape) - 400, tolerance = 1e-12)
  expect_equal(env$log_haz_scalar(7L, exp(1), 0, huge_shape, 1),
               log(huge_shape) - 1, tolerance = 1e-12)

  expect_equal(env$surv_ll_status(8L, exp(700), 0, 0, 1L, 800, 2, 1),
               -900, tolerance = 1e-12)
})

test_that("parametric log hazards remain finite after tail cancellation", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_functions()

  expect_equal(env$log_haz_full(8L, exp(400), 0, 2, 1), 0,
               tolerance = 1e-12)

  z <- 100
  inv_z2 <- 1 / z^2
  log_mills <- log(z) +
    log(1 + inv_z2 * (1 + inv_z2 * (-2 + inv_z2 * (10 - 74 * inv_z2))))
  expect_equal(env$log_haz_full(6L, exp(100), 0, 1, 1),
               log_mills - 100, tolerance = 1e-12)

  expect_equal(env$log_mean_haz(1L, exp(290), c(400, 400), 1, 1),
               400, tolerance = 1e-12)

  # One profile has an ordinary density while the other has log(S) = -Inf and
  # log(h) = Inf. Combining log density directly must not form Inf + -Inf.
  tiny_scale <- 1e-308
  expected <- stats::dnorm(0, log = TRUE) - log(tiny_scale) - log(0.5)
  expect_equal(env$log_mean_haz(6L, 1, c(0, -2), tiny_scale, 1),
               expected, tolerance = 1e-10)
  expect_equal(mlumr:::.r_log_density(6L, 1, c(0, -2), tiny_scale, 1)[1],
               stats::dlnorm(1, 0, tiny_scale, log = TRUE),
               tolerance = 1e-12)
  expect_identical(mlumr:::.r_log_density(
    6L, 1, c(0, -2), tiny_scale, 1
  )[2], -Inf)
})

test_that("generalized-gamma density combines its exponential term", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_functions()

  k <- 1e-6
  y <- exp(0.72)
  z <- 1 / sqrt(k) * log(y)
  want <- -log(y) - 0.5 * log(k) * (1 - 2 * k) + k * z -
    exp(log(k) + z) - lgamma(k)
  got <- env$gengamma_lpdf(y, 0, 1, k)
  expect_true(is.finite(got))
  expect_equal(got / want, 1, tolerance = 1e-14)
})

test_that("tail censoring probabilities use direct stable formulas", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_functions()

  expect_equal(env$surv_ll_status(1L, 1, 0, 0, 2L, -800, 1, 1),
               -800, tolerance = 1e-12)
  expect_equal(env$surv_ll_status(1L, 2, 1, 0, 3L, -800, 1, 1),
               -800, tolerance = 1e-12)
  expect_equal(env$surv_ll_status(8L, 1, 0, 0, 2L, 800, 2, 1),
               -1600 - lgamma(3), tolerance = 1e-12)
  expect_equal(env$surv_ll_status(9L, 1, 0, 0, 2L, 800, 1, 1),
               -800, tolerance = 1e-12)

  entry <- 1
  exit <- entry + .Machine$double.eps
  want <- -exp(36) * (exit - entry)
  expect_equal(env$surv_ll_status(1L, exit, 0, entry, 0L, 36, 1, 1),
               want, tolerance = 1e-12)
})

test_that("target cumulative hazard stays on the log-survival scale", {
  skip_if_not(exists(".predict_target_survival", asNamespace("mlumr")),
             "survival prediction helpers arrive with the prediction change")
  fit <- structure(
    list(
      data = list(
        index_treatment = "A", comparator_treatment = "B", covariates = "x",
        ipd = list(data = data.frame(x = 0))
      ),
      model = "spfa",
      draws = data.frame(
        mu_index = c(0, 0), mu_comparator = c(0, 0),
        `beta[1]` = c(0, 0), aux_val = c(1, 1), check.names = FALSE
      ),
      surv_info = list(kind = "parametric", dist_code = 1L),
      pred_times = c(1, 1000),
      stan_data = list(
        cov_center = 0, pred_ibasis = matrix(0, 2, 1),
        pred_ibasis_cmp = matrix(0, 2, 1)
      )
    ),
    class = "mlumr_fit"
  )

  out <- mlumr:::.predict_target_survival(
    fit, data.frame(x = c(0, 0)), "cumhaz", FALSE, c(0.025, 0.975)
  )
  expect_equal(out$t_1000, rep(1000, 4), tolerance = 1e-12)
})

test_that("flexible target log survival combines the cumulative hazard scale", {
  fit <- structure(
    list(
      draws = data.frame(
        `scoef_idx[1]` = c(exp(-720), 0), check.names = FALSE
      ),
      surv_info = list(kind = "mspline"),
      stan_data = list(n_scoef = 1L, n_strata = 1L)
    ),
    class = "mlumr_fit"
  )

  out <- mlumr:::.surv_s_at_times(
    fit, c(720, 800), 1, matrix(1, nrow = 1), log_scale = TRUE
  )
  expect_equal(drop(out), c(-1, 0), tolerance = 1e-10)
})
