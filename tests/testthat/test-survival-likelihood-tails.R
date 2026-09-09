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

test_that("an interval too narrow to separate two CDFs still has a probability", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # The mirror of the failure above. Exponential rate 1, entry 0.05, event
  # between 0.1 and the very next representable double: both bounds have the
  # same log CDF, -2.3521684610440907, so a difference of CDFs is zero. The
  # increments compute a ratio and still resolve it.
  lower <- 0.1
  upper <- lower + .Machine$double.eps / 8
  expect_true(upper > lower)
  expect_equal(pexp(upper, log.p = TRUE), pexp(lower, log.p = TRUE))

  expect_equal(
    env$surv_ll_status(1L, upper, lower, 0.05, 3L, 0, 0, 0),
    -(lower - 0.05) + log(-expm1(-(upper - lower))),
    tolerance = 1e-9
  )
})

test_that("a tiny but real CDF gap is not thrown away with the equal ones", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # Falling back to increments whenever the CDFs are CLOSE, rather than only
  # when they are equal, rejects intervals the CDF can still resolve and hands
  # them to increments that cannot. At shape 10 with entry 0.025, the interval
  # (0.05, 0.050000000000003555] has log CDFs 7.2e-13 apart, so a margin of
  # 1e-12 sent it to the increments, where every survival probability rounds to
  # 1 and the answer is -Inf.
  shape <- 10
  lower <- 0.05
  upper <- 0.050000000000003555
  log_cdf <- function(t) pgamma(t, shape, log.p = TRUE)
  expect_gt(log_cdf(upper), log_cdf(lower))
  expect_lt(log_cdf(upper) - log_cdf(lower), 1e-12)
  expect_true(all(pgamma(c(0.025, lower, upper), shape,
                         lower.tail = FALSE) == 1))

  got <- env$surv_ll_status(8L, upper, lower, 0.025, 3L, 0, shape, 0)
  # Finiteness is the regression. The value itself cannot be pinned tightly:
  # the two log CDFs agree to thirteen digits, so the gap is about a hundred
  # units in the last place and one ULP of disagreement between two gamma-CDF
  # implementations moves its log by about 0.01. R and Stan differ by 0.02
  # here, which is that, not an error in either.
  expect_true(is.finite(got))
  # Bound the log-scale gap directly. `expect_equal(tolerance = 0.05)` reads as
  # an absolute 0.05 but testthat's tolerance is RELATIVE, and against a value
  # near -73 it admits 3.6 log units, a factor of 36 in probability.
  ref <- .log_diff(log_cdf(upper), log_cdf(lower)) -
    pgamma(0.025, shape, lower.tail = FALSE, log.p = TRUE)
  expect_lt(abs(got - ref), 0.05)
})

test_that("an interval too narrow for both differences keeps its probability", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # One ULP wide, which is narrower than the case above. Here the CDF route
  # declines for a second reason: the two linear CDFs still differ, but taking
  # their logs coalesces them, so there is no gap left to difference. The
  # survival increments cannot resolve it either, since every survival
  # probability in the region rounds to exactly 1. Neither difference has any
  # digits, and the probability is an ordinary small number.
  nextafter <- function(t) t + 2^(floor(log2(abs(t))) - 52)
  lower <- 0.1
  upper <- nextafter(lower)
  entry <- 0.05
  expect_gt(upper, lower)

  cases <- list(
    list(dist = 8L, aux = 10, log_dens = function(t) dgamma(t, 10, log = TRUE),
         log_cdf = function(t) pgamma(t, 10, log.p = TRUE),
         log_surv = function(t) {
           pgamma(t, 10, lower.tail = FALSE, log.p = TRUE)
         }),
    list(dist = 1L, aux = 1, log_dens = function(t) dexp(t, log = TRUE),
         log_cdf = function(t) pexp(t, log.p = TRUE),
         log_surv = function(t) pexp(t, lower.tail = FALSE, log.p = TRUE))
  )

  for (cs in cases) {
    label <- paste("dist", cs$dist)
    # Neither difference has a digit left, stated as the exact doubles rather
    # than through a tolerance: the two log CDFs are the same double, and
    # exponentiating the survival increment gives exactly 1, which is what
    # makes log1m_exp() of it -Inf.
    expect_identical(cs$log_cdf(upper), cs$log_cdf(lower))
    expect_identical(exp(cs$log_surv(upper) - cs$log_surv(lower)), 1)

    # Across one ULP the density is constant to about thirty digits, so
    # f(t) * width is the reference, not an approximation to one.
    mass <- cs$log_dens(0.5 * (lower + upper)) + log(upper - lower)

    got <- env$surv_ll_status(cs$dist, upper, lower, entry, 3L, 0, cs$aux, 0)
    expect_true(is.finite(got), label = label)
    expect_lt(abs(got - (mass - cs$log_surv(entry))), 1e-9)

    plain <- env$log_interval_prob_scalar(cs$dist, upper, lower, 0, cs$aux, 0)
    expect_true(is.finite(plain), label = label)
    expect_lt(abs(plain - mass), 1e-9)
  }
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
