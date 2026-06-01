test_that("mlumr sampler controls are validated before backend dispatch", {
  expect_silent(.validate_mlumr_sampling_args(
    chains = 2,
    iter = 100,
    warmup = 50,
    seed = 2026,
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


test_that("mlumr validates center and qr flags", {
  expect_error(mlumr(NULL, center = "yes"), "`center` must be TRUE or FALSE")
  expect_error(mlumr(NULL, center = NA), "`center` must be TRUE or FALSE")
  expect_error(mlumr(NULL, center = c(TRUE, FALSE)), "`center` must be TRUE or FALSE")
  expect_error(mlumr(NULL, qr = 1), "`qr` must be TRUE or FALSE")
  expect_error(mlumr(NULL, qr = NA), "`qr` must be TRUE or FALSE")
})


test_that(".mlumr_qr_design builds the combined design with the right shape", {
  # SPFA: nB = 2 + n_cov; QR off -> R_inv is identity, Xq is the raw design.
  sd0 <- list(n_cov = 2L,
              X_ipd = matrix(rnorm(20), 10, 2),
              X_int = array(rnorm(2 * 5 * 2), dim = c(2, 5, 2)))
  out <- mlumr:::.mlumr_qr_design(sd0, model = "spfa", qr = FALSE)
  expect_equal(out$nB, 4L)
  expect_equal(dim(out$Xq_ipd), c(10L, 4L))
  expect_equal(dim(out$Xq_int), c(2L, 5L, 4L))
  expect_equal(out$R_inv, diag(4L))
  expect_equal(out$qr, 0L)
  # Intercept dummy columns: IPD rows are [1, 0, X], integration rows [0, 1, X].
  expect_equal(unname(out$Xq_ipd[, 1:2]), cbind(rep(1, 10), rep(0, 10)))

  # Relaxed: nB = 2 + 2 * n_cov.
  outr <- mlumr:::.mlumr_qr_design(sd0, model = "relaxed", qr = FALSE)
  expect_equal(outr$nB, 6L)
  expect_equal(dim(outr$Xq_ipd), c(10L, 6L))

  # QR on: R_inv is no longer the identity, dimensions preserved.
  outq <- mlumr:::.mlumr_qr_design(sd0, model = "spfa", qr = TRUE)
  expect_equal(outq$qr, 1L)
  expect_equal(dim(outq$R_inv), c(4L, 4L))
  expect_false(isTRUE(all.equal(outq$R_inv, diag(4L))))
})


test_that(".mlumr_center_covariates centers about the pooled mean", {
  sd0 <- list(n_cov = 1L,
              X_ipd = matrix(c(1, 2, 3, 4), 4, 1),
              X_int = array(c(5, 6), dim = c(1, 2, 1)))
  out <- mlumr:::.mlumr_center_covariates(sd0, center = TRUE)
  # Pooled mean weighting each IPD patient by 1 and the single AgD row (mean of
  # its integration points = 5.5) by 1: (4 * 2.5 + 5.5) / (4 + 1) = 3.1.
  expect_equal(out$cov_center, 3.1)
  expect_equal(as.numeric(out$X_ipd), c(1, 2, 3, 4) - 3.1)
  # center = FALSE -> zeros, design untouched
  out0 <- mlumr:::.mlumr_center_covariates(sd0, center = FALSE)
  expect_equal(out0$cov_center, 0)
  expect_equal(as.numeric(out0$X_ipd), c(1, 2, 3, 4))
})

test_that(".mlumr_center_covariates center is independent of n_int (H5)", {
  # Same AgD covariate distribution (mean 5.5), 2 vs 4 integration points.
  sd_small <- list(n_cov = 1L, X_ipd = matrix(c(1, 2, 3, 4), 4, 1),
                   X_int = array(c(5, 6), dim = c(1, 2, 1)))
  sd_large <- list(n_cov = 1L, X_ipd = matrix(c(1, 2, 3, 4), 4, 1),
                   X_int = array(c(5, 6, 5, 6), dim = c(1, 4, 1)))
  c_small <- mlumr:::.mlumr_center_covariates(sd_small, center = TRUE)$cov_center
  c_large <- mlumr:::.mlumr_center_covariates(sd_large, center = TRUE)$cov_center
  expect_equal(c_small, c_large)
})

test_that("multiple normal aggregate strata require outcome_n", {
  expect_error(
    set_agd(
      data.frame(trt = c("B", "B"), y = c(0, 1), se = c(0.2, 0.3),
                 x_mean = c(-0.5, 0.5), x_sd = c(1, 1)),
      "trt", family = "normal", outcome_mean = "y", outcome_se = "se",
      cov_means = "x_mean", cov_sds = "x_sd"
    ),
    "outcome_n.*multiple normal aggregate rows"
  )
})

test_that("flexible survival builders accept shared and per-study knots", {
  skip_if_not_installed("splines2")
  dat <- sim_survival_data(n_ipd = 50, n_agd = 60, n_int = 8)
  info <- mlumr:::.survival_distribution_info("mspline")
  ipd <- dat$ipd$data
  cmp <- dat$agd$pseudo_ipd
  common_time <- min(max(ipd$.time), max(cmp$.time))

  shared_knots <- make_knots(dat, n_knots = 3, type = "equal")
  shared_knots$internal <- shared_knots$boundary[2] * c(0.2, 0.5, 0.8)
  shared <- mlumr:::.build_stan_data_survival(
    list(), dat, info, pred_times = common_time, n_knots = 7,
    knots = shared_knots, rmst_horizon = common_time,
    prior_aux = default_prior_aux(), prior_smooth = default_prior_smooth(),
    n_strata = 1L
  )
  expect_equal(shared$n_scoef, 7L)

  per_study <- list(
    index = list(internal = max(ipd$.time) * c(0.25, 0.7),
                 boundary = c(0, max(ipd$.time))),
    comparator = list(internal = max(cmp$.time) * c(0.25, 0.7),
                      boundary = c(0, max(cmp$.time)))
  )
  stratified <- mlumr:::.build_stan_data_survival(
    list(), dat, info, pred_times = common_time, n_knots = 7,
    knots = per_study, rmst_horizon = common_time,
    prior_aux = default_prior_aux(), prior_smooth = default_prior_smooth(),
    n_strata = 2L
  )
  expect_equal(stratified$n_scoef, 6L)
  expect_error(
    mlumr:::.build_stan_data_survival(
      list(), dat, info, pred_times = common_time, n_knots = 7,
      knots = shared_knots, rmst_horizon = common_time,
      prior_aux = default_prior_aux(), prior_smooth = default_prior_smooth(),
      n_strata = 2L
    ),
    "named list.*index.*comparator"
  )
})

test_that("custom knots must cover the observed baseline support", {
  expect_error(
    mlumr:::.validate_user_knots(
      list(internal = c(1, 2), boundary = c(0, 3)),
      max_time = 4, label = "shared"
    ),
    "covering all observed times"
  )
  expect_true("knots" %in% names(formals(mlumr)))
})
