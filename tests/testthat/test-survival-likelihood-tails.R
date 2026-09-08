# The delayed-entry survival likelihood, checked against the CDF in the tail
# where survival is not representable.
#
# Compiling the model and sampling from it does not reach this: the fixtures
# below are ordinary inputs that the validator accepts, and the failure is a
# single branch returning a value no fit can use. Only calling the compiled
# helper finds it, so that is what these do.

expose_survival_likelihood <- function() {
  stan_dir <- system.file("stan", package = "mlumr")
  skip_if(stan_dir == "", "installed Stan includes not found")
  code <- paste(
    "functions {",
    "#include include/priors_functions.stan",
    "#include include/survival_functions.stan",
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

# log(e^a - e^b) for a > b, kept in logs so the tail references stay exact.
.log_diff <- function(a, b) a + log1p(-exp(b - a))

test_that("a delayed-entry interval keeps its probability in the lower tail", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  shape <- 10
  log_cdf <- function(t) pgamma(t, shape, log.p = TRUE)
  log_surv <- function(t) pgamma(t, shape, lower.tail = FALSE, log.p = TRUE)

  # This is the whole problem in one line: at shape 10 the upper-tail gamma
  # rounds to exactly 1 at all three of these times, so every difference of
  # survival probabilities is 0 and every log of one is unusable. The CDF is
  # representable throughout, and F(0.1) is about 2.5e-17.
  expect_true(all(pgamma(c(0.025, 0.05, 0.1), shape, lower.tail = FALSE) == 1))

  # Interval-censored (status 3), entry 0.025, event in (0.05, 0.1].
  expect_equal(
    env$surv_ll_status(8L, 0.1, 0.05, 0.025, 3L, 0, shape, 0),
    .log_diff(log_cdf(0.1), log_cdf(0.05)) - log_surv(0.025),
    tolerance = 1e-12
  )

  # Left-censored (status 2) under delayed entry reaches the same branch.
  expect_equal(
    env$surv_ll_status(8L, 0.1, 0, 0.025, 2L, 0, shape, 0),
    .log_diff(log_cdf(0.1), log_cdf(0.025)) - log_surv(0.025),
    tolerance = 1e-12
  )
})

test_that("a delayed-entry interval survives an underflowing hazard", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # Exponential PH with eta = -1000: the rate is exp(-1000), which underflows
  # to zero, so a cumulative-hazard increment exponentiates to 0. The interval
  # probability is the rate itself to any precision that matters here, so the
  # log of it is the linear predictor back again.
  expect_equal(
    env$surv_ll_status(1L, 2, 1, 0.5, 3L, -1000, 0, 0),
    -1000,
    tolerance = 1e-9
  )
})

test_that("the right tail keeps the accuracy the conditional form gave it", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # The reason the conditional-increment form was adopted. Forming the
  # unconditional interval probability and subtracting log S(entry) cancels
  # away the significant digits out here, so these two must keep coming from
  # the increments and must not regress when the lower tail is repaired.
  shape <- 10
  log_surv <- function(t) pgamma(t, shape, lower.tail = FALSE, log.p = TRUE)
  expect_equal(
    env$surv_ll_status(8L, 50, 40, 30, 3L, 0, shape, 0),
    .log_diff(log_surv(40), log_surv(50)) - log_surv(30),
    tolerance = 1e-12
  )

  # Exponential, rate 1, entry 50, event in (60, 70].
  expect_equal(
    env$surv_ll_status(1L, 70, 60, 50, 3L, 0, 0, 0),
    .log_diff(-60, -70) - (-50),
    tolerance = 1e-12
  )
})
