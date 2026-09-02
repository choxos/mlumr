make_survival_data <- function(seed = 2026, n_ipd = 50, n_agd = 60) {
  set.seed(seed)
  ipd <- data.frame(trt = "A", time = rexp(n_ipd, 0.1),
                    status = rbinom(n_ipd, 1, 0.7),
                    age = rnorm(n_ipd, 60, 10))
  ipd_obj <- set_ipd(ipd, treatment = "trt", covariates = "age",
                     family = "survival", time = "time", status = "status")
  agd <- data.frame(trt = "B", time = rexp(n_agd, 0.12),
                    status = rbinom(n_agd, 1, 0.6),
                    age_mean = 62, age_sd = 9)
  agd_obj <- set_agd_surv(agd, treatment = "trt", time = "time",
                          status = "status", cov_means = "age_mean",
                          cov_sds = "age_sd", cov_types = "continuous")
  combine_data(ipd_obj, agd_obj)
}

test_that("make_knots returns sensible boundaries and internal knots", {
  dat <- make_survival_data()
  kn <- make_knots(dat, n_knots = 5)
  expect_length(kn$boundary, 2)
  expect_true(kn$boundary[1] >= 0)
  expect_true(kn$boundary[2] > kn$boundary[1])
  expect_true(kn$n_knots <= 5)
  expect_true(all(kn$internal > kn$boundary[1] & kn$internal < kn$boundary[2]))
})

test_that("make_knots requires a survival mlumr_data object", {
  expect_error(make_knots(list()), "combine_data")
})

test_that("M-spline basis has the expected number of coefficients", {
  skip_if_not_installed("splines2")
  dat <- make_survival_data()
  kn <- make_knots(dat, n_knots = 5)
  spec3 <- mlumr:::.build_mspline_basis(kn, degree = 3)
  spec0 <- mlumr:::.build_mspline_basis(kn, degree = 0)
  expect_equal(spec3$n_scoef, length(kn$internal) + 3 + 1)
  expect_equal(spec0$n_scoef, length(kn$internal) + 1)
})

test_that("basis support is invariant to the time unit", {
  skip_if_not_installed("splines2")
  # M-spline hazards scale as inverse time. On this scale every live column is
  # below 1e-12, but changing units cannot turn supported coefficients into an
  # identification failure.
  kn <- list(internal = c(2e14, 5e14, 8e14), boundary = c(0, 1e15))
  for (degree in c(0L, 3L)) {
    spec <- mlumr:::.build_mspline_basis(kn, degree)
    basis <- mlumr:::.eval_basis(spec, seq(0, 1e15, length.out = 101))
    expect_lt(max(basis), 1e-12)
    expect_true(mlumr:::.assert_basis_support(spec, 1e15, "index"))
  }
})

expose_mspline_functions <- function() {
  stan_dir <- system.file("stan", package = "mlumr")
  skip_if(stan_dir == "" ||
            !file.exists(file.path(stan_dir, "include",
                                  "survival_mspline_functions.stan")),
          "installed Stan include not found")
  code <- paste(
    "functions {",
    "#include include/survival_functions.stan",
    "#include include/survival_mspline_functions.stan",
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

test_that("M-spline cumulative hazards combine on the log scale", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_mspline_functions()

  tiny <- exp(-720)
  expect_equal(env$mspline_cumhaz(tiny, 720), 1, tolerance = 1e-10)
  expect_equal(env$mspline_cumhaz(0, 800), 0)
  expect_equal(env$mspline_ll_status(tiny, 0, 0, tiny, 720, 1L),
               -1, tolerance = 1e-10)
  expect_equal(env$mspline_ll_status(tiny, 0, 0, 1, 720, 2L),
               log1p(-exp(-1)), tolerance = 1e-10)

  # A zero event hazard is impossible, not an event with an arbitrary floor.
  expect_identical(env$mspline_ll_status(1, 0, 0, 0, 0, 1L), -Inf)
})

test_that("M-spline marginal log hazard retains the surviving profile scale", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_mspline_functions()

  # At this cumulative hazard the eta = 700 profile dominates survival. The
  # result is log(h0) + eta, which is lost when two order-1e304 log totals are
  # subtracted after eta has rounded away.
  got <- env$mspline_log_mean_haz(1, 2, c(700, 701))
  expect_true(is.finite(got))
  expect_equal(got, log(2) + 700, tolerance = 1e-10)
  expect_identical(env$mspline_log_mean_haz(1, 0, c(0, 1)), -Inf)
})

test_that("integrated basis is zero at the lower boundary and monotone", {
  skip_if_not_installed("splines2")
  dat <- make_survival_data()
  kn <- make_knots(dat, n_knots = 5)
  spec <- mlumr:::.build_mspline_basis(kn, degree = 3)
  times <- seq(0, kn$boundary[2], length.out = 20)
  ib <- mlumr:::.eval_basis(spec, times, integral = TRUE)
  expect_equal(unname(ib[1, ]), rep(0, spec$n_scoef))
  # each column is non-decreasing in time
  for (j in seq_len(ncol(ib))) {
    expect_true(all(diff(ib[, j]) >= -1e-8))
  }
  # each integrated column reaches 1 at the upper boundary, so the
  # simplex-weighted baseline cumulative hazard H0(T) = sum(scoef * I(T)) = 1
  expect_equal(unname(ib[nrow(ib), ]), rep(1, spec$n_scoef), tolerance = 1e-6)
})

test_that("M-spline basis evaluation handles empty and extrapolated times", {
  skip_if_not_installed("splines2")
  dat <- make_survival_data()
  kn <- make_knots(dat, n_knots = 4)
  spec <- mlumr:::.build_mspline_basis(kn, degree = 3)
  empty <- mlumr:::.eval_basis(spec, numeric(0), integral = TRUE)
  expect_equal(dim(empty), c(0L, spec$n_scoef))

  # Hazard is constant beyond the upper boundary, so cumulative hazard keeps
  # increasing linearly instead of clamping at the boundary value.
  at_upper <- mlumr:::.eval_basis(spec, kn$boundary[2], integral = TRUE)
  beyond <- mlumr:::.eval_basis(spec, kn$boundary[2] * 2, integral = TRUE)
  expect_true(all(is.finite(beyond)))
  scoef <- rep(1 / spec$n_scoef, spec$n_scoef)
  expect_gt(as.numeric(beyond %*% scoef), as.numeric(at_upper %*% scoef))

  haz_upper <- mlumr:::.eval_basis(spec, kn$boundary[2], integral = FALSE)
  haz_beyond <- mlumr:::.eval_basis(spec, kn$boundary[2] * 2, integral = FALSE)
  expect_equal(haz_beyond, haz_upper, tolerance = 1e-10)
})

test_that("constant-hazard anchor centers on a flat baseline hazard", {
  skip_if_not_installed("splines2")
  # Deliberately uneven knots, where equal simplex weights would NOT be flat.
  kn <- list(internal = c(0.5, 1.0, 1.8, 3.2, 5.5), boundary = c(0, 10))
  for (deg in c(3, 0)) {
    spec <- mlumr:::.build_mspline_basis(kn, degree = deg)
    anchor <- mlumr:::.mspline_constant_hazard(spec)
    weights <- mlumr:::.rw1_prior_weights(spec)
    expect_length(anchor, spec$n_scoef - 1)
    expect_length(weights, spec$n_scoef - 1)
    expect_true(all(weights > 0))

    # softmax(append_row(0, anchor)) is the constant-hazard simplex
    scoef <- mlumr:::.softmax(c(0, anchor))
    expect_equal(sum(scoef), 1)

    # baseline hazard h0(t) = M(t) %*% scoef is (numerically) constant in t
    tt <- seq(0.2, 9.8, length.out = 40)
    h0 <- as.vector(mlumr:::.eval_basis(spec, tt, integral = FALSE) %*% scoef)
    expect_lt(stats::sd(h0) / mean(h0), 1e-6)   # flat under uneven knots
  }
})

test_that("RW1 weights are knot-spacing-aware (not all equal under uneven knots)", {
  skip_if_not_installed("splines2")
  kn <- list(internal = c(0.5, 1.0, 1.8, 3.2, 5.5), boundary = c(0, 10))
  spec <- mlumr:::.build_mspline_basis(kn, degree = 3)
  weights <- mlumr:::.rw1_prior_weights(spec)
  expect_gt(length(unique(round(weights, 8))), 1L)
})

test_that("M-spline build warns when prediction times extrapolate past boundary", {
  skip_if_not_installed("splines2")
  dat <- make_survival_data()
  surv_info <- mlumr:::.survival_distribution_info("mspline")
  max_time <- max(c(dat$ipd$data$.time, dat$agd$pseudo_ipd$.time))

  # pred_times within the observed range: no extrapolation warning
  expect_no_warning(
    mlumr:::.build_stan_data_survival(
      stan_data = list(), data = dat, surv_info = surv_info,
      pred_times = seq(max_time / 10, max_time, length.out = 5), n_knots = 4,
      rmst_horizon = max_time, prior_aux = default_prior_aux(),
      prior_smooth = default_prior_smooth()
    )
  )

  # pred_times beyond the largest observed time: extrapolated, so warn
  expect_warning(
    mlumr:::.build_stan_data_survival(
      stan_data = list(), data = dat, surv_info = surv_info,
      pred_times = c(max_time, max_time * 2), n_knots = 4,
      rmst_horizon = max_time, prior_aux = default_prior_aux(),
      prior_smooth = default_prior_smooth()
    ),
    "beyond the .*largest observed time|extrapolated"
  )
})
