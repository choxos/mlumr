# A shape parameter that could not be found is not a shape parameter of 1.
# Substituting one silently turns the fitted distribution into a different one:
# at dist 2 a Weibull becomes an exponential, and every survival, hazard and
# RMST prediction downstream is computed from the wrong model.

.aux_fit <- function(draws, dist = 2L, n_strata = 2L,
                     distribution = "weibull") {
  list(draws = draws,
       stan_data = list(n_strata = n_strata),
       surv_info = list(dist_code = dist, distribution = distribution))
}

test_that("the named per-treatment views are used when present", {
  fit <- .aux_fit(list(aux_val = c(1.1, 1.2), aux_val_cmp = c(2.1, 2.2)))
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "index", 2),
               c(1.1, 1.2))
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "comparator", 2),
               c(2.1, 2.2))
})

test_that("the raw parameter matrix is read when the views were excluded", {
  # `aux_val` is a TRANSFORMED parameter, so pars/include = FALSE drops it and
  # leaves the matrix it is read off. Stratum 1 is the index, stratum n_strata
  # the comparator.
  draws <- list(`aux_raw[1,1]` = c(1.1, 1.2), `aux_raw[1,2]` = c(2.1, 2.2))
  fit <- .aux_fit(draws)
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "index", 2),
               c(1.1, 1.2))
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "comparator", 2),
               c(2.1, 2.2))
})

test_that("a shared baseline reads the same stratum for both treatments", {
  fit <- .aux_fit(list(`aux_raw[1,1]` = c(1.5, 1.6)), n_strata = 1L)
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "comparator", 2),
               c(1.5, 1.6))
})

test_that("an unfindable shape is refused rather than defaulted to 1", {
  fit <- .aux_fit(list(eta = c(0, 0)))
  expect_error(mlumr:::.surv_aux_draws(fit, "aux_val", "index", 2),
               "Could not find aux_val draws")
  expect_error(mlumr:::.surv_aux_draws(fit, "aux_val", "index", 2), "weibull")
  # gengamma's second shape is a real parameter too.
  gg <- .aux_fit(list(eta = c(0, 0)), dist = 9L, distribution = "gengamma")
  expect_error(mlumr:::.surv_aux_draws(gg, "aux2_val", "index", 2),
               "Could not find aux2_val draws")
})

test_that("1 is still returned where Stan itself fixes the shape at 1", {
  # dist 1 and 4 have no shape: Stan declares aux_raw with zero rows and sets
  # the transformed value to 1.0, so there is nothing to find and 1 is right.
  for (d in c(1L, 4L)) {
    fit <- .aux_fit(list(eta = c(0, 0)), dist = d)
    expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "index", 2), c(1, 1))
  }
  # Only gengamma has a second shape, so every other distribution fixes it.
  fit <- .aux_fit(list(eta = c(0, 0)), dist = 8L, distribution = "gamma")
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux2_val", "index", 3), c(1, 1, 1))
})

test_that("the comparator reads its own stratum before any index fallback", {
  # `aux_val_cmp` excluded, `aux_val` kept. Taking the index view here would
  # give the comparator the index study's shape under `aux_by = ".study"`,
  # which is the substitution this function exists to stop, one axis over.
  draws <- list(aux_val = c(1.1, 1.2), `aux_raw[1,2]` = c(2.1, 2.2))
  fit <- .aux_fit(draws)
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "comparator", 2),
               c(2.1, 2.2))
})

test_that("a stratified fit with only the index view is refused", {
  fit <- .aux_fit(list(aux_val = c(1.1, 1.2)))
  expect_error(mlumr:::.surv_aux_draws(fit, "aux_val", "comparator", 2),
               "Could not find aux_val draws")
})

test_that("a shared baseline still lets the comparator read the index view", {
  # One stratum: Stan sets aux_val_cmp from the same aux_raw[1,1], so the two
  # are the same number and a fit that saved only one of them is readable.
  fit <- .aux_fit(list(aux_val = c(1.5, 1.6)), n_strata = 1L)
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "comparator", 2),
               c(1.5, 1.6))
})

test_that("an unrecognized distribution code takes the error path", {
  # The second-shape branch used to be written as a negation, so anything that
  # was not the integer 9 counted as "no second shape". A gengamma whose code
  # arrives as the double 9 is still a gengamma, and an absent or NA code is not
  # a licence to substitute 1.
  gg <- .aux_fit(list(eta = c(0, 0)), dist = 9, distribution = "gengamma")
  expect_error(mlumr:::.surv_aux_draws(gg, "aux2_val", "index", 2),
               "Could not find aux2_val draws")

  for (bad in list(NA_integer_, NULL)) {
    fit <- .aux_fit(list(eta = c(0, 0)), dist = bad, distribution = "unknown")
    expect_error(mlumr:::.surv_aux_draws(fit, "aux2_val", "index", 2),
                 "Could not find aux2_val draws")
    expect_error(mlumr:::.surv_aux_draws(fit, "aux_val", "index", 2),
                 "Could not find aux_val draws")
  }

  # A recognized code given as a double is still recognized.
  gam <- .aux_fit(list(eta = c(0, 0)), dist = 8, distribution = "gamma")
  expect_equal(mlumr:::.surv_aux_draws(gam, "aux2_val", "index", 2), c(1, 1))
})

test_that("a shared baseline is symmetric between the two views", {
  # One stratum, and only the comparator view survives. Stan reads both views
  # off the same aux_raw[1,1] there, so the index can use it; refusing would
  # fail predict() for a shape that is present under another name.
  fit <- .aux_fit(list(aux_val_cmp = c(1.5, 1.6)), n_strata = 1L)
  expect_equal(mlumr:::.surv_aux_draws(fit, "aux_val", "index", 2), c(1.5, 1.6))

  # With two strata the two views are different studies' shapes, so the index
  # must not borrow the comparator's.
  strat <- .aux_fit(list(aux_val_cmp = c(1.5, 1.6)), n_strata = 2L)
  expect_error(mlumr:::.surv_aux_draws(strat, "aux_val", "index", 2),
               "Could not find aux_val draws")
})
