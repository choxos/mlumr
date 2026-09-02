# Identification of a stratified flexible baseline.
#
# With ONE pooled basis whose upper boundary is the longest study's last time, a
# shorter study can have basis functions with no support over its observed
# period. Scaling that study's observed coefficients by c, moving the surplus
# simplex mass into an unsupported column, and replacing mu by mu - log(c)
# leaves h0(t)exp(eta) and H0(t)exp(eta) unchanged at every observed time: the
# likelihood is exactly flat along that direction, so the study intercept is set
# by the prior rather than by data.
#
# Per-study knots (multinma's default `type = "quantile"`) remove it: each
# study's basis spans only its own observed support, so no column is unsupported
# and H0_s is pinned at a time the study actually observed.

skip_if_not_installed("splines2")

# Markedly unequal follow-up: index runs to ~100 with a heavy late tail, the
# comparator stops at ~20, so pooled quantile knots land above the comparator's
# last observation.
unequal_data <- function() {
  set.seed(2026)
  list(index = c(stats::runif(50, 0, 20), stats::runif(150, 40, 100)),
       comparator = stats::runif(200, 0, 20))
}

zero_support_cols <- function(spec, observed_max) {
  grid <- seq(0, observed_max, length.out = 4000)
  b <- mlumr:::.eval_basis(spec, grid, integral = FALSE)
  which(apply(b, 2, max) < 1e-12)
}

test_that("a pooled basis leaves the shorter study's late columns unsupported", {
  d <- unequal_data()
  all_t <- c(d$index, d$comparator)
  pooled <- mlumr:::.knots_from_times(all_t, all_t, n_knots = 7)

  # Some internal knots sit beyond the comparator's follow-up: that is the
  # precondition for an unsupported column.
  expect_true(any(pooled$internal > max(d$comparator)))

  for (degree in c(0L, 3L)) {
    spec <- mlumr:::.build_mspline_basis(pooled, degree = degree)
    dead <- zero_support_cols(spec, max(d$comparator))
    expect_gt(length(dead), 0)
  }
})

test_that("the unsupported columns give an exact likelihood ridge", {
  d <- unequal_data()
  all_t <- c(d$index, d$comparator)
  pooled <- mlumr:::.knots_from_times(all_t, all_t, n_knots = 7)
  spec <- mlumr:::.build_mspline_basis(pooled, degree = 0L)

  comp_max <- max(d$comparator)
  dead <- zero_support_cols(spec, comp_max)
  live <- setdiff(seq_len(spec$n_scoef), dead)
  expect_gt(length(dead), 0)

  grid <- seq(0, comp_max, length.out = 2000)
  b <- mlumr:::.eval_basis(spec, grid, integral = FALSE)
  ib <- mlumr:::.eval_basis(spec, grid, integral = TRUE)

  scoef <- rep(1 / spec$n_scoef, spec$n_scoef)
  cc <- 1.15                                  # the rescaling factor
  scoef2 <- scoef
  scoef2[live] <- cc * scoef[live]
  scoef2[dead] <- (1 - cc * sum(scoef[live])) / length(dead)

  expect_true(all(scoef2 >= 0))               # still a valid simplex
  expect_equal(sum(scoef2), 1, tolerance = 1e-12)

  # mu -> mu - log(c) divides exp(eta) by c, so the products must be unchanged.
  expect_equal(as.vector(b %*% scoef), as.vector(b %*% scoef2) / cc,
               tolerance = 1e-12)
  expect_equal(as.vector(ib %*% scoef), as.vector(ib %*% scoef2) / cc,
               tolerance = 1e-12)
})

test_that("per-study knots leave every column supported", {
  d <- unequal_data()
  k_idx <- mlumr:::.knots_from_times(d$index, d$index, n_knots = 7)
  k_cmp <- mlumr:::.knots_from_times(d$comparator, d$comparator, n_knots = 7)

  # Each study's boundary is its own last observed time.
  expect_equal(k_idx$boundary[2], max(d$index))
  expect_equal(k_cmp$boundary[2], max(d$comparator))
  # No internal knot sits beyond the study it describes.
  expect_false(any(k_cmp$internal > max(d$comparator)))
  expect_false(any(k_idx$internal > max(d$index)))

  for (degree in c(0L, 3L)) {
    spec_idx <- mlumr:::.build_mspline_basis(k_idx, degree = degree)
    spec_cmp <- mlumr:::.build_mspline_basis(k_cmp, degree = degree)
    expect_equal(length(zero_support_cols(spec_idx, max(d$index))), 0)
    expect_equal(length(zero_support_cols(spec_cmp, max(d$comparator))), 0)
    # The simplex dimension is shared across strata, so the bases must match.
    expect_equal(spec_idx$n_scoef, spec_cmp$n_scoef)
  }
})

test_that("stratified survival fits build one basis per study", {
  skip_on_cran()
  dat <- sim_survival_data(n_int = 16)
  build <- function(n_strata) {
    mlumr:::.build_stan_data_survival(
      stan_data = list(), data = dat,
      surv_info = mlumr:::.survival_distribution_info("mspline"),
      pred_times = NULL, n_knots = 3, rmst_horizon = NULL, n_rmst_grid = 20L,
      prior_aux = default_prior_aux(), prior_smooth = default_prior_smooth(),
      n_strata = n_strata)
  }

  sd2 <- build(2L)
  # Per-stratum objects exist and the comparator basis genuinely differs.
  expect_false(is.null(sd2$pred_ibasis_cmp))
  expect_equal(dim(sd2$pred_ibasis_cmp), dim(sd2$pred_ibasis))
  expect_equal(ncol(sd2$lscoef_prior_mean), 2L)
  expect_equal(ncol(sd2$lscoef_weights), 2L)
  # Matching shapes are not evidence of stratification: sd1 below shows the
  # shared path produces identical copies at the same dimensions. Assert the
  # two bases actually differ, which is the thing this test is named for.
  expect_false(isTRUE(all.equal(sd2$pred_ibasis_cmp, sd2$pred_ibasis)))

  # aux_by = "none" keeps one shared basis, so the copies coincide exactly and
  # the shared-baseline model is unchanged.
  sd1 <- build(1L)
  expect_equal(sd1$pred_ibasis_cmp, sd1$pred_ibasis)
  expect_equal(sd1$rmst_ibasis_cmp, sd1$rmst_ibasis)
  expect_equal(ncol(sd1$lscoef_prior_mean), 1L)
})

test_that("per-study knot reduction can reach zero internal knots for a cubic", {
  # How far the knot count may be reduced depends on the DEGREE. A cubic basis
  # with no internal knots still has degree + 1 = 4 coefficients, so zero is a
  # valid last resort (and always matches, since both studies then get the same
  # dimension by construction). A degree-0 piecewise-exponential basis with no
  # internal knots collapses to one constant hazard, so it must keep at least
  # one. A single floor of 1 for every degree hid the valid cubic fallback
  # behind an error.
  set.seed(2026)
  ipd <- data.frame(.time = sort(stats::runif(200, 1, 40)), .status = 1L)
  # Median event time equals the largest observed time, so every internal
  # quantile knot lands on the upper boundary and is dropped. The index study
  # keeps its knots, so the two bases disagree at every n_knots >= 1.
  pseudo <- data.frame(.time = c(rep(2, 20), rep(10, 60)), .status = 1L)

  specs <- suppressMessages(
    mlumr:::.matched_per_study_bases(ipd, pseudo, n_knots = 3L, degree = 3L))
  expect_equal(specs$n_knots, 0L)
  expect_equal(specs$index$n_scoef, 4L)         # degree + 1, not 1
  expect_equal(specs$comparator$n_scoef, 4L)

  # The reduction is reported rather than silent.
  expect_message(
    mlumr:::.matched_per_study_bases(ipd, pseudo, n_knots = 3L, degree = 3L),
    "Reduced `n_knots` from 3 to 0")

  # Degree 0 has no valid zero-knot fallback and must still stop.
  expect_error(
    mlumr:::.matched_per_study_bases(ipd, pseudo, n_knots = 3L, degree = 0L),
    "equal dimension")
})

test_that("a shared baseline asserts basis support, as the stratified one does", {
  # The stratified branch calls .assert_basis_support() on both specs; the
  # shared branch did not, so a single-stratum fit could recreate the exact
  # ridge that assertion exists to prevent. .validate_user_knots() only
  # requires the upper boundary to reach the last observed time, not to stop
  # near it, so internal knots past the observed maximum leave columns
  # supported nowhere the data reach. Simplex mass can move onto those and
  # trade one for one against the intercept, leaving the likelihood exactly
  # flat along that direction and the intercept set by its prior.
  skip_on_cran()
  d <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)
  tmax <- max(c(d$ipd$data$.time, d$agd$pseudo_ipd$.time))
  expect_lt(tmax, 10)

  spec <- mlumr:::.build_mspline_basis(
    mlumr:::.validate_user_knots(
      list(internal = c(10, 20, 30), boundary = c(0, 50)), tmax, "shared"),
    0L)
  expect_error(mlumr:::.assert_basis_support(spec, tmax, "shared"),
               "no support over its observed follow-up")

  # Knots placed over the observed range are accepted.
  ok <- mlumr:::.build_mspline_basis(
    mlumr:::.validate_user_knots(
      list(internal = c(1, 2, 3), boundary = c(0, tmax)), tmax, "shared"),
    0L)
  expect_true(mlumr:::.assert_basis_support(ok, tmax, "shared"))
})
