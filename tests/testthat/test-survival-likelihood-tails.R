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

  # At shape 10 with entry 0.025, the interval (0.05, 0.050000000000003555]
  # has log CDFs 7.2e-13 apart. Every survival probability there rounds to 1,
  # so the increments give -Inf, and a CDF difference this close to its own
  # rounding is mostly rounding: the resolution test declines it, and the
  # quadrature of the density has the value.
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
  # The density is constant to thirty digits across 3.6e-15, so f(t) * width
  # is the tight reference, and the quadrature meets it.
  dens_ref <- dgamma(0.5 * (lower + upper), shape, log = TRUE) +
    log(upper - lower) - pgamma(0.025, shape, lower.tail = FALSE, log.p = TRUE)
  expect_lt(abs(got - dens_ref), 1e-9)
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

test_that("the log-logistic interval is exact whatever its width or shape", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # Shape 1e-20 spreads the distribution so thin that (1, 100] holds 1e-20 of
  # its mass, below one ULP of F(1) = 1/2, so neither difference resolves it,
  # and across it the density falls a hundredfold, so a midpoint value times
  # the width does not either: that gave -46.76 for -45.91. The closed form
  # has no such regime. First order in the shape, F(t) = 1/2 + a log(t) / 4,
  # so the mass is a log(100) / 4 to twenty digits.
  a <- 1e-20
  ref <- log(a * log(100) / 4)
  expect_lt(abs(env$surv_ll_status(7L, 100, 1, 0, 3L, 0, a, 0) - ref), 1e-10)
  # With entry at 1 the mass is divided by S(1) = 1/2.
  expect_lt(abs(env$surv_ll_status(7L, 100, 1, 1, 3L, 0, a, 0) -
                  (ref + log(2))), 1e-10)

  # An ordinary interval agrees with the CDF difference where that is exact.
  z <- function(t) 1.5 * (log(t) - 0.2)
  ref <- log(plogis(z(2)) - plogis(z(0.5)))
  expect_lt(abs(env$surv_ll_status(7L, 2, 0.5, 0, 3L, 0.2, 1.5, 0) - ref),
            1e-12)
  expect_lt(abs(env$surv_ll_status(7L, 2, 0.5, 0.3, 3L, 0.2, 1.5, 0) -
                  (ref - plogis(z(0.3), lower.tail = FALSE, log.p = TRUE))),
            1e-12)
})

test_that("a few-ULP interval is resolved to rounding on every route", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("flexsurv")
  env <- expose_survival_likelihood()

  # A CDF difference that is finite is not thereby accurate. Two CDFs each
  # carry half an ULP of rounding, so a difference a few ULPs wide is mostly
  # rounding: four ULPs from 0.1 with entry at 0.05 came back 16% low for the
  # exponential and 29% high for the gamma. The route is now chosen by
  # whether its rounding is well below the mass, and the five closed-form
  # families never difference at all.
  nextafter <- function(t) t + 2^(floor(log2(abs(t))) - 52)
  ulps <- function(t, k) {
    for (i in seq_len(k)) t <- nextafter(t)
    t
  }
  cases <- list(
    list(dist = 1L, aux = 0, aux2 = 0, dens = function(t) dexp(t, 1),
         log_surv = function(t) -t),
    list(dist = 2L, aux = 2, aux2 = 0,
         dens = function(t) dweibull(t, shape = 2, scale = 1),
         log_surv = function(t) -t^2),
    list(dist = 6L, aux = 1, aux2 = 0, dens = function(t) dlnorm(t, 0, 1),
         log_surv = function(t) {
           plnorm(t, 0, 1, lower.tail = FALSE, log.p = TRUE)
         }),
    list(dist = 7L, aux = 1.5, aux2 = 0,
         dens = function(t) {
           z <- 1.5 * log(t)
           1.5 / t * exp(z) / (1 + exp(z))^2
         },
         log_surv = function(t) -log1p(t^1.5)),
    list(dist = 8L, aux = 10, aux2 = 0,
         dens = function(t) dgamma(t, shape = 10, rate = 1),
         log_surv = function(t) {
           pgamma(t, 10, lower.tail = FALSE, log.p = TRUE)
         }),
    # Generalized gamma: aux is sigma, aux2 is 1 / Q^2, so Q = 0.5 is 4.
    list(dist = 9L, aux = 0.8, aux2 = 4,
         dens = function(t) flexsurv::dgengamma(t, 0, 0.8, 0.5),
         log_surv = function(t) {
           log(flexsurv::pgengamma(t, 0, 0.8, 0.5, lower.tail = FALSE))
         })
  )
  lower <- 0.1
  entry <- 0.05
  for (cs in cases) {
    for (k in c(1, 2, 4, 8, 16)) {
      upper <- ulps(lower, k)
      label <- sprintf("dist %d, %d ULPs", cs$dist, k)
      # Across a few ULPs the density is constant to thirty digits, so
      # f(t) * width is the reference, not an approximation to one.
      ref <- log(cs$dens(0.5 * (lower + upper))) + log(upper - lower)
      got <- env$surv_ll_status(cs$dist, upper, lower, 0, 3L, 0, cs$aux,
                                cs$aux2)
      expect_lt(abs(got - ref), 1e-10, label = label)
      expect_lt(abs(expm1(got - ref)), 1e-10, label = label)
      got_entry <- env$surv_ll_status(cs$dist, upper, lower, entry, 3L, 0,
                                      cs$aux, cs$aux2)
      ref_entry <- ref - cs$log_surv(entry)
      expect_lt(abs(got_entry - ref_entry), 1e-10, label = label)
      expect_lt(abs(expm1(got_entry - ref_entry)), 1e-10, label = label)
    }
  }
})

test_that("a wide interval with a small mass is integrated, not approximated", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # Small mass on a wide interval, where the density is anything but constant
  # and the differences have no digits. The references are independent of the
  # code under test: for a gamma with shape k near zero, F(u) - F(l) is
  # k * int_l^u exp(-t) / t dt to first order in k, and for a log-normal with
  # sigma huge it is log(u / l) / (sigma * sqrt(2 * pi)).
  k <- 1e-12
  ref <- log(k * integrate(function(t) exp(-t) / t, 1, 100,
                           rel.tol = 1e-13)$value)
  got <- env$surv_ll_status(8L, 100, 1, 0, 3L, 0, k, 0)
  expect_lt(abs(got - ref), 1e-9)
  got_entry <- env$surv_ll_status(8L, 100, 1, 0.5, 3L, 0, k, 0)
  expect_lt(abs(got_entry - (ref - pgamma(0.5, k, lower.tail = FALSE,
                                          log.p = TRUE))), 1e-9)

  sigma <- 1e9
  ref <- log(log(100) / (sigma * sqrt(2 * pi)))
  got <- env$surv_ll_status(6L, 100, 1, 0, 3L, 0, sigma, 0)
  expect_lt(abs(got - ref), 1e-9)
  got_entry <- env$surv_ll_status(6L, 100, 1, 0.5, 3L, 0, sigma, 0)
  expect_lt(abs(got_entry - (ref - plnorm(0.5, 0, sigma, lower.tail = FALSE,
                                          log.p = TRUE))), 1e-9)
})

test_that("a deep right tail keeps the increment route", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # A log-normal with sigma 1e-4 has log S near -5e9 by t = 1e4. The
  # resolution test asks the interval's mass to exceed 1e-5 of |log S(l)| as
  # a fraction of S(l), and uncapped that is more than one, which no interval
  # can meet: every interval out here went to quadrature, whose grid cannot
  # resolve the layer next to the lower bound where the tail's mass sits, and
  # the conditional log-likelihood came back 2.8 too high. The increment is
  # computed in the tail branch without differencing and is the right route,
  # so the threshold is capped at one half.
  eta <- -1
  aux <- 1e-4
  log_surv <- function(t) {
    pnorm((log(t) - eta) / aux, lower.tail = FALSE, log.p = TRUE)
  }
  lower <- 10000
  upper <- 10001
  entry <- 9999.9999
  expect_lt(log_surv(lower), -1e9)
  ref <- log_surv(lower) + log1p(-exp(log_surv(upper) - log_surv(lower)))
  expect_lt(abs(env$surv_ll_status(6L, upper, lower, entry, 3L, eta, aux, 0) -
                  (ref - log_surv(entry))), 1e-6)
  expect_lt(abs(env$surv_ll_status(6L, upper, lower, 0, 3L, eta, aux, 0) - ref),
            1e-6)
})
