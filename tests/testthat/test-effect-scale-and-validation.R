# Regression tests for the effect-scale API, the native logit-normal
# parameterization, the second generalized-gamma auxiliary prior, and the
# prediction-time grid snapping. All pure R: none of these fit a model.

# ---- the survival scalar effect selector is literal ------------------------

test_that("each survival fit has exactly one legitimate scalar effect name", {
  expect_equal(mlumr:::.surv_scalar_effect_name("HR"), "hr")
  expect_equal(mlumr:::.surv_scalar_effect_name("TR"), "tr")
  expect_equal(mlumr:::.surv_scalar_effect_name("EXP_DELTA_ETA"), "exp_delta_eta")
  # A label the mapping does not know must not fall through to a default: that
  # would silently name one estimand with another's selector, which is the whole
  # failure mode this mapping exists to prevent.
  expect_error(mlumr:::.surv_scalar_effect_name("LOG_HR"), "Unrecognized")
})

test_that("asking for the wrong survival scale names the one the fit has", {
  err <- mlumr:::.surv_effect_scale_error
  # PH fit asked for a time ratio.
  msg <- err("tr", "HR", "hr", stratified = FALSE,
             valid_effects = c("all", "hr", "rmstd", "rmstr"))
  expect_match(msg, "proportional-hazards")
  expect_match(msg, 'effect = "hr"', fixed = TRUE)

  # Shared-shape AFT fit asked for a hazard ratio. This is the case that used to
  # succeed and hand back a time ratio labeled TR.
  msg <- err("hr", "TR", "tr", stratified = FALSE,
             valid_effects = c("all", "tr", "rmstd", "rmstr"))
  expect_match(msg, "time ratio")
  expect_match(msg, 'effect = "tr"', fixed = TRUE)

  # Stratified and relaxed AFT keep their distinct explanations.
  expect_match(err("hr", "EXP_DELTA_ETA", "exp_delta_eta", TRUE, character(0)),
               "own AFT shape")
  expect_match(err("tr", "EXP_DELTA_ETA", "exp_delta_eta", FALSE, character(0)),
               "relaxed fit")

  # And the mirror image: the location contrast is not the right name for a fit
  # that has a real hazard ratio or time ratio.
  msg <- err("exp_delta_eta", "TR", "tr", stratified = FALSE,
             valid_effects = c("all", "tr", "rmstd", "rmstr"))
  expect_match(msg, "shapes differ by study")
  expect_match(msg, "relaxed AFT")

  # A name that is not a scalar effect at all falls back to the plain list.
  expect_match(err("nonsense", "HR", "hr", FALSE, c("all", "hr", "rmstd")),
               "must be one of")
})

# ---- native logit-normal parameters are validated --------------------------

test_that("the native logit-normal parameterization rejects unusable sigma", {
  # One invalid `sigma` used to give three different answers: dlogitnorm()
  # returned Inf at sigma = 0, plogitnorm() returned 1, qlogitnorm() returned
  # 0.5, and all three were finite and unflagged.
  for (f in list(dlogitnorm, plogitnorm, qlogitnorm)) {
    expect_error(f(0.5, mu = 0, sigma = 0), "strictly positive")
    expect_error(f(0.5, mu = 0, sigma = -1), "strictly positive")
    expect_error(f(0.5, mu = 0, sigma = Inf), "finite")
    expect_error(f(0.5, mu = Inf, sigma = 1), "finite")
    expect_error(f(0.5, mu = 0, sigma = NA), "must not be missing")
    expect_error(f(0.5, mu = 0, sigma = "1"), "numeric")
  }
  # Partial recycling paired each value with the wrong parameter.
  expect_error(dlogitnorm(0.5, mu = c(0, 1), sigma = c(1, 1, 1)), "same length")
  expect_error(plogitnorm(0.5, mu = c(0, 1), sigma = c(1, 1, 1)), "same length")

  # The valid paths are untouched, including scalar-against-vector recycling.
  expect_equal(qlogitnorm(0.5, mu = 0, sigma = 1), 0.5)
  expect_equal(plogitnorm(0.5, mu = 0, sigma = 1), 0.5)
  expect_equal(dlogitnorm(0.5, mu = 0, sigma = c(1, 2)),
               stats::dnorm(0, 0, c(1, 2)) / 0.25)
  expect_equal(dlogitnorm(numeric(0), mu = 0, sigma = 1), numeric(0))
})

# ---- the second generalized-gamma auxiliary prior ---------------------------

test_that("prior_aux2 reaches Stan and defaults to prior_aux", {
  skip_if_not_installed("splines2")
  dat <- sim_survival_data(n_ipd = 40, n_agd = 50, n_int = 8)
  info <- mlumr:::.survival_distribution_info("gengamma")
  horizon <- min(max(dat$ipd$data$.time), max(dat$agd$pseudo_ipd$.time))

  build_surv <- mlumr:::.build_stan_data_survival
  build <- function(...) {
    build_surv(list(), dat, info, pred_times = horizon, n_knots = 7,
               knots = NULL, rmst_horizon = horizon,
               prior_smooth = default_prior_smooth(), n_strata = 1L, ...)
  }

  # Unspecified: the two auxiliaries share one specification, which is what the
  # package did before `prior_aux2` existed.
  sd_only <- build(prior_aux = prior_normal(0, 2))
  expect_equal(sd_only$prior_aux2_location, sd_only$prior_aux_location)
  expect_equal(sd_only$prior_aux2_scale, sd_only$prior_aux_scale)

  # Specified: Stan has always read the aux2 slots separately; they were simply
  # being fed the same numbers.
  both <- build(prior_aux = prior_normal(0, 2),
                prior_aux2 = prior_normal(0, 0.5))
  expect_equal(both$prior_aux_scale, 2)
  expect_equal(both$prior_aux2_scale, 0.5)
  expect_false(isTRUE(all.equal(both$prior_aux_scale, both$prior_aux2_scale)))

  # A different family, not only a different scale, must survive the trip.
  fam <- build(prior_aux = prior_normal(0, 2),
               prior_aux2 = prior_exponential(1))
  expect_false(identical(fam$prior_aux_dist, fam$prior_aux2_dist))
})

# ---- snapped prediction times are announced --------------------------------

test_that("prediction times moved onto the fitted grid are reported", {
  grid <- c(1, 2, 3, 4, 5)
  sel <- mlumr:::.surv_time_selection

  # An exact hit is not an approximation and must stay quiet.
  expect_silent(idx <- sel(c(2, 4), grid))
  expect_equal(idx, c(2L, 4L))

  # A moved time says what was asked for and what was used.
  expect_message(sel(c(2.4, 4.9), grid), "2.4 -> 2")
  expect_message(sel(c(2.4, 4.9), grid), "4.9 -> 5")

  # Two requested times collapsing to one grid point silently produced a result
  # with fewer rows than the request had times.
  expect_message(sel(c(2.1, 2.2), grid), "share a grid point")
  expect_equal(suppressMessages(sel(c(2.1, 2.2), grid)), 2L)

  # The whole-grid default is not a request and is not reported on.
  expect_silent(sel(NULL, grid))
})
