test_that("prior constructors validate scalar arguments", {
  expect_error(prior_normal(mean = c(0, 1)),
               "`mean` must be a single finite number")
  expect_error(prior_normal(sd = c(1, 2)),
               "`sd` must be a single positive finite number")
  expect_error(prior_normal(sd = 0),
               "`sd` must be a single positive finite number")
  expect_error(prior_normal(autoscale = "TRUE"),
               "`autoscale` must be TRUE or FALSE")
  expect_error(prior_normal(autoscale = NA),
               "`autoscale` must be TRUE or FALSE")

  expect_error(prior_student_t(df = c(3, 4)),
               "`df` must be a single positive finite number")
  expect_error(prior_student_t(mean = NA_real_),
               "`mean` must be a single finite number")
  expect_error(prior_student_t(sd = Inf),
               "`sd` must be a single positive finite number")

  expect_error(prior_exponential(rate = c(1, 2)),
               "`rate` must be a single positive finite number")
  expect_error(prior_exponential(rate = 0),
               "`rate` must be a single positive finite number")
})


test_that("validate_prior rejects malformed hand-built prior lists cleanly", {
  expect_error(
    validate_prior(list(distribution = "normal", mean = c(0, 1), sd = 1),
                   "beta"),
    "Prior mean must be a single finite number for beta"
  )
  expect_error(
    validate_prior(list(distribution = "normal", mean = 0, sd = c(1, 2)),
                   "beta"),
    "Prior sd must be a single positive finite number for beta"
  )
  expect_error(
    validate_prior(list(distribution = "student_t", mean = 0, sd = 1,
                        df = c(3, 4)), "beta"),
    "Prior df must be a single positive finite number for beta"
  )
  expect_error(
    validate_prior(list(distribution = "exponential", rate = c(1, 2)),
                   "sigma"),
    "Prior rate must be a single positive finite number for sigma"
  )
})


test_that("Stan prior translators reject vector scalar priors", {
  vector_prior <- list(
    distribution = "normal",
    mean = c(0, 1),
    sd = c(2, 3),
    df = NA_real_,
    autoscale = FALSE
  )

  expect_error(stan_prior_fields(vector_prior),
               "Prior mean must be a single finite number")
  expect_error(stan_prior_fields_beta(vector_prior, n_cov = 2),
               "Prior mean must be a single finite number for beta")
})


test_that("per-coefficient priors still support heterogeneous means and scales", {
  fields <- stan_prior_fields_beta(
    list(prior_normal(0, 2), prior_normal(1, 3)),
    n_cov = 2
  )

  expect_equal(fields$mean, c(0, 1))
  expect_equal(fields$sd, c(2, 3))
  expect_equal(fields$dist, 0L)
  expect_equal(fields$df, 3)
})
