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

test_that("a lower bound at the bottom of the double range does not overflow the ratio", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # log(u / l) was taken as log1p((u - l) / l), exact for close bounds and
  # +Inf once l is a subnormal: the Weibull cumulative-hazard difference then
  # read as infinite and the interval's log probability as 0, and the
  # log-logistic form produced NaN. The two logs are subtracted directly once
  # the bounds are far apart, where no cancellation remains.
  lower <- 1e-320
  upper <- 0.5
  expect_true(lower > 0)
  expect_identical(lower / lower * (upper - lower) / lower, Inf)

  weibull <- log(-expm1(-upper^1.5))
  for (dist in c(2L, 5L)) {
    expect_lt(abs(env$surv_ll_status(dist, upper, lower, 0, 3L, 0, 1.5, 0) -
                    weibull), 1e-12)
    expect_lt(abs(env$surv_ll_status(dist, upper, lower, 1e-321, 3L, 0, 1.5,
                                     0) - weibull), 1e-12)
  }
  loglogistic <- log(plogis(log(upper)) - plogis(log(lower)))
  expect_lt(abs(env$surv_ll_status(7L, upper, lower, 0, 3L, 0, 1, 0) -
                  loglogistic), 1e-12)
  expect_lt(abs(env$surv_ll_status(7L, upper, lower, 1e-321, 3L, 0, 1, 0) -
                  loglogistic), 1e-12)
  expect_lt(abs(env$surv_ll_status(6L, upper, lower, 0, 3L, 0, 1, 0) -
                  plnorm(upper, log.p = TRUE)), 1e-12)
  expect_lt(abs(env$surv_ll_status(8L, upper, lower, 0, 3L, 0, 2, 0) -
                  pgamma(upper, 2, log.p = TRUE)), 1e-12)
})

test_that("an increment formed in a tail branch resolves its interval whatever its size", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()
  nextafter <- function(t) t + 2^(floor(log2(abs(t))) - 52)

  # Log-normal, sigma 5e-8, entry and lower bound at 1, upper one ULP above:
  # z is 5e7 and log S near -1.25e15. The tail branch forms the increment
  # from dz (z_u + z_l) / 2 without differencing, -0.222 here, an interval
  # holding a fifth of what remains. The mass test sent it to quadrature,
  # whose grid cannot resolve the layer next to the lower bound, and the
  # result cancelled against log S(entry) to -1.25 for a value of -1.614.
  eta <- -2.5
  aux <- 5e-8
  z_l <- (log(1) - eta) / aux
  dz <- (log(nextafter(1)) - log(1)) / aux
  ref <- log1p(-exp(-0.5 * dz * (2 * z_l + dz)))
  expect_lt(abs(env$surv_ll_status(6L, nextafter(1), 1, 1, 3L, eta, aux, 0) -
                  ref), 1e-6)

  # An increment of -Inf from a tail branch is an interval holding everything
  # past S(l), for which log1m_exp() is exactly zero: the log probability is
  # log S(l). It was rejected as unresolved and sent to quadrature.
  eta <- -1e-169
  aux <- 1e-170
  ref <- pnorm((log(1) - eta) / aux, lower.tail = FALSE, log.p = TRUE)
  expect_lt(abs(env$surv_ll_status(6L, nextafter(1), 1, 0, 3L, eta, aux, 0) -
                  ref), 1e-9)
})

test_that("the log-logistic conditional form does not cancel in a far tail", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # Entry and lower bound at 1, upper at e, eta -1e16: the unconditional log
  # probability is near -1e16 and so is -log S(entry), and their sum
  # returned 0 for a conditional value of log(1 - e^-1). The conditional
  # form takes z_e - z_u as a log of a time ratio and never forms either.
  ref <- log(-expm1(-1))
  expect_lt(abs(env$surv_ll_status(7L, exp(1), 1, 1, 3L, -1e16, 1, 0) - ref),
            1e-12)
  # And the ordinary conditional case is unchanged by the rewrite.
  z <- function(t) 1.5 * (log(t) - 0.2)
  ref <- log(plogis(z(0.01)) - plogis(z(0.005))) -
    plogis(z(0.001), lower.tail = FALSE, log.p = TRUE)
  expect_lt(abs(env$surv_ll_status(7L, 0.01, 0.005, 0.001, 3L, 0.2, 1.5, 0) -
                  ref), 1e-12)

  # A shape of 1e17 across the center: log(expm1(d)) and the z_u term are
  # each near 7e16 and cancel to an answer of log(1/2). Written so that the
  # large parts cancel algebraically, the form is bounded by the answer.
  expect_lt(abs(env$surv_ll_status(7L, 2, 1, 0, 3L, 0, 1e17, 0) - log(0.5)),
            1e-12)
  expect_lt(abs(env$surv_ll_status(7L, 2, 1, 0.5, 3L, 0, 1e17, 0) - log(0.5)),
            1e-12)
  # An interval crossing the center at that shape: z_u formed as z_l + d is
  # the sum of two values near 7e16 that cancel to -11.1, and rounding left
  # -8; formed from the upper bound directly it is exact.
  ref <- log(plogis(1e17 * log(0.9999999999999999)) - plogis(1e17 * log(0.5)))
  expect_lt(abs(env$surv_ll_status(7L, 0.9999999999999999, 0.5, 0, 3L, 0,
                                   1e17, 0) - ref), 1e-12)
  lower <- 146514.5092358064
  upper <- 151776.28223159446
  eta <- 11.93016288820274
  ref <- log(plogis(1e17 * (log(upper) - eta)) -
               plogis(1e17 * (log(lower) - eta)))
  expect_lt(abs(env$surv_ll_status(7L, upper, lower, 0, 3L, eta, 1e17, 0) -
                  ref), 1e-12)
  # Entry and lower bound far below the center with the same shape: the
  # conditioning terms are each near 1e17 and the answer is log(1) = 0; the
  # sum of them returned 32. The difference of two g values is taken by the
  # signs of the two arguments, never as two enormous numbers.
  expect_lt(abs(env$surv_ll_status(7L, exp(2), exp(1), exp(-0.5), 3L, 1.5,
                                   1e17, 0)), 1e-12)
  # And a conditional interval straddling the center at an ordinary shape.
  ref <- log(plogis(z(3)) - plogis(z(1.5))) -
    plogis(z(0.5), lower.tail = FALSE, log.p = TRUE)
  expect_lt(abs(env$surv_ll_status(7L, 3, 1.5, 0.5, 3L, 0.2, 1.5, 0) - ref),
            1e-12)
})

test_that("a differenced increment of -Inf beside a finite log S(l) resolves too", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()
  nextafter <- function(t) t + 2^(floor(log2(abs(t))) - 52)

  # A log-normal centered at the lower bound with sigma 1e-171: S(1) is 1/2
  # and S of the next double underflows to zero, so the differenced
  # increment is -Inf and the interval holds everything that remains. That
  # is a resolved interval whichever way the increment was formed, and it
  # was sent to a quadrature whose grid cannot see the layer.
  expect_lt(abs(env$surv_ll_status(6L, nextafter(1), 1, 0, 3L, 0, 1e-171, 0) -
                  log(0.5)), 1e-12)
  expect_lt(abs(env$surv_ll_status(6L, nextafter(1), 1, 0.5, 3L, 0, 1e-171,
                                   0) - log(0.5)), 1e-12)
})

test_that("a Weibull cumulative-hazard difference is written from the upper bound", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()
  nextafter <- function(t) t + 2^(floor(log2(abs(t))) - 52)

  # Shape 3e14, lower bound 0.5, upper a hair under 1: written from the
  # lower bound the difference adds -2e14 to +2e14 and kept rounding of
  # order 0.05 in a value near -2.35, returning -2.87. From the upper bound
  # the first term is the value itself and the second is zero.
  a <- 3e14
  lower <- 0.5
  upper <- exp(log(0.1) / a)
  ref <- log(pweibull(upper, a, 1) - pweibull(lower, a, 1))
  for (dist in c(2L, 5L)) {
    expect_lt(abs(env$surv_ll_status(dist, upper, lower, 0, 3L, 0, a, 0) -
                    ref), 1e-9)
    expect_lt(abs(env$surv_ll_status(dist, upper, lower, 0.4, 3L, 0, a, 0) -
                    (ref - pweibull(0.4, a, 1, lower.tail = FALSE,
                                    log.p = TRUE))), 1e-9)
  }
  # Weibull AFT with the upper bound near exp(eta) at that shape: the shape
  # multiplies log(t_upper) - eta as one small difference. As two products
  # of 1e16 each the difference of -1.39 was lost to rounding and the log
  # probability moved by a unit.
  # The reference forms the same small difference, since exp(eta) rounded
  # and logged again moves it by a few units in the last place, which the
  # shape turns into hundredths.
  upper <- 1.1051709180756477
  eta <- 0.10000000000000009
  cumhaz <- function(t) exp(a * (log(t) - eta))
  ref <- log(-expm1(-(cumhaz(upper) - cumhaz(upper / 2)))) - cumhaz(upper / 2)
  expect_lt(abs(env$surv_ll_status(5L, upper, upper / 2, 0, 3L, eta, a, 0) -
                  ref), 1e-9)
  # Narrow intervals agree with the density to rounding, as before.
  ref <- dweibull(0.1, 2, 1, log = TRUE) + log(nextafter(0.1) - 0.1) + 0.05^2
  expect_lt(abs(env$surv_ll_status(2L, nextafter(0.1), 0.1, 0.05, 3L, 0, 2,
                                   0) - ref), 1e-10)
  # The Gompertz takes the same form, and keeps its accuracy at a tiny shape,
  # where expm1() is what the reference needs too.
  gomp_h <- function(t, eta, b) exp(eta) / b * expm1(b * t)
  for (b in c(0.7, 1e-12)) {
    ref <- -(gomp_h(1, 0.1, b) - gomp_h(0.5, 0.1, b)) +
      log(-expm1(-(gomp_h(2, 0.1, b) - gomp_h(1, 0.1, b))))
    expect_lt(abs(env$surv_ll_status(3L, 2, 1, 0.5, 3L, 0.1, b, 0) - ref),
              1e-10)
  }
})

test_that("a shape near the bottom of the double range keeps a tiny increment", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()
  nextafter <- function(t) t + 2^(floor(log2(abs(t))) - 52)

  # The product of a shape of 1e-310 and an interval of 1e-14 underflows to
  # zero, and log(1 - exp(-0)) is -Inf, where the log probability is an
  # ordinary -33. The product's log is taken as the sum of the logs there.
  upper <- 1 + 1e-14
  dt <- upper - 1
  expect_identical(1e-310 * dt, 0)
  ref <- log(dt) - 1
  expect_lt(abs(env$surv_ll_status(3L, upper, 1, 0, 3L, 0, 1e-310, 0) - ref),
            1e-9)
  # The log-logistic and Weibull forms take the same product of the shape
  # and log(u / l), one ULP wide here, whose true log probability near -774
  # is representable.
  width <- nextafter(1) - 1
  expect_identical(1e-320 * width, 0)
  expect_lt(abs(env$surv_ll_status(7L, nextafter(1), 1, 0, 3L, 0, 1e-320, 0) -
                  (log(1e-320) + log(width) - log(4))), 1e-9)
  expect_lt(abs(env$surv_ll_status(2L, nextafter(1), 1, 0, 3L, 0, 1e-320, 0) -
                  (log(1e-320) + log(width) - 1)), 1e-9)
})

test_that("a differenced CDF is trusted by its cancellation error, not its finiteness", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()
  previous <- function(t) t - 2^(floor(log2(abs(t))) - 52)

  # The cancellation error of log F(u) + log(1 - exp(log F(l) - log F(u)))
  # is eps |log F(u)| (1 - m) / m for a mass fraction m. A wide interval in
  # an extreme lower tail has m near one, so the difference is the answer to
  # the rounding of the answer itself: a log-normal with sigma 1e-4 and
  # eta = log(2) + 1 puts log F(2) near -5e7 with F(1) negligible beside it,
  # and the density's mass sits in a layer next to the upper bound that no
  # grid resolves, which came back 9.3 log units high when the difference
  # was refused.
  eta <- log(2) + 1
  aux <- 1e-4
  log_cdf <- function(t) pnorm((log(t) - eta) / aux, log.p = TRUE)
  ref <- log_cdf(2) + log1p(-exp(log_cdf(1) - log_cdf(2)))
  got <- env$surv_ll_status(6L, 2, 1, 0, 3L, eta, aux, 0)
  expect_lt(abs(got - ref) / abs(ref), 1e-12)
  entry <- pnorm((log(0.5) - eta) / aux, lower.tail = FALSE, log.p = TRUE)
  got <- env$surv_ll_status(6L, 2, 1, 0.5, 3L, eta, aux, 0)
  expect_lt(abs(got - (ref - entry)) / abs(ref), 1e-12)

  # Log-normal with sigma 1e-8 at t = exp(-1): z is -1e8 and the log CDFs
  # near -5e15 carry rounding of order one, so over one ULP they round to
  # the same double and no difference exists to take. The density resolves
  # the interval to the rounding of the value itself: one unit at that
  # magnitude.
  aux <- 1e-8
  upper <- exp(-1)
  lower <- previous(upper)
  mid <- 0.5 * (lower + upper)
  z <- log(mid) / aux
  expect_lt(z, -1e7)
  ref <- -0.5 * z^2 - log(aux) - log(mid) - 0.5 * log(2 * pi) +
    log(upper - lower)
  got <- env$surv_ll_status(6L, upper, lower, 0, 3L, 0, aux, 0)
  expect_true(is.finite(got))
  expect_lt(abs(got - ref) / abs(ref), 1e-15)
})

test_that("quadrature nodes keep offsets below an ULP of the log time", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("flexsurv")
  env <- expose_survival_likelihood()
  nextafter <- function(t) t + 2^(floor(log2(abs(t))) - 52)

  # Bounds one ULP apart at 0.1 with eta = log(0.1) and a scale of 1e-17:
  # every node's log time is eta to the last bit, so nodes formed as
  # exp(s) and logged again all stood at z = 0 and the density read as
  # constant across an interval whose standardized width is 14. The offset
  # of each node travels separately into the density and is added to the
  # small centered difference, which keeps it. The interval holds exactly
  # half the distribution.
  lower <- 0.1
  upper <- nextafter(lower)
  eta <- log(0.1)
  aux <- 1e-17
  z <- function(t) log1p((t - 0.1) / 0.1) / aux
  expect_gt(z(upper) - z(lower), 10)
  ref <- log(pnorm(z(upper)) - pnorm(z(lower)))
  expect_lt(abs(env$surv_ll_status(6L, upper, lower, 0, 3L, eta, aux, 0) -
                  ref), 1e-9)
  # The event density is the same function at offset zero, for every
  # family. The exponential AFT is pinned separately: its log cumulative
  # hazard is log(t) - eta itself, and a sign slip there once turned the
  # event likelihood into -eta - exp(eta) / t.
  expect_lt(abs(env$surv_ll_status(6L, 2, 0, 0, 1L, 0.3, 0.8, 0) -
                  dlnorm(2, 0.3, 0.8, log = TRUE)), 1e-12)
  expect_lt(abs(env$surv_ll_status(9L, 2, 0, 0, 1L, 0.3, 0.8, 4) -
                  flexsurv::dgengamma(2, 0.3, 0.8, 0.5, log = TRUE)), 1e-12)
  expect_lt(abs(env$surv_ll_status(4L, 1, 0, 0, 1L, log(2), 0, 0) -
                  (-log(2) - 0.5)), 1e-12)
  expect_lt(abs(env$surv_ll_status(4L, 3, 0, 0, 1L, 0.5, 0, 0) -
                  dexp(3, exp(-0.5), log = TRUE)), 1e-12)
  expect_lt(abs(env$surv_ll_status(1L, 3, 0, 0, 1L, 0.5, 0, 0) -
                  dexp(3, exp(0.5), log = TRUE)), 1e-12)
  expect_lt(abs(env$surv_ll_status(5L, 3, 0, 0, 1L, 0.5, 1.5, 0) -
                  dweibull(3, 1.5, exp(0.5), log = TRUE)), 1e-12)
  expect_lt(abs(env$surv_ll_status(2L, 3, 0, 0, 1L, 0.5, 1.5, 0) -
                  (log(1.5) + 0.5 + 0.5 * log(3) - 3^1.5 * exp(0.5))), 1e-12)
  expect_lt(abs(env$surv_ll_status(8L, 3, 0, 0, 1L, 0.5, 2, 0) -
                  dgamma(3, 2, exp(-0.5), log = TRUE)), 1e-12)
  expect_lt(abs(env$surv_ll_status(3L, 2, 0, 0, 1L, 0.1, 0.7, 0) -
                  (0.1 + 0.7 * 2 - exp(0.1) / 0.7 * expm1(0.7 * 2))), 1e-12)
  expect_lt(abs(env$surv_ll_status(7L, 3, 0, 0, 1L, 0.5, 1.5, 0) -
                  (log(1.5) - log(3) + 1.5 * (log(3) - 0.5) -
                     2 * log1p(exp(1.5 * (log(3) - 0.5))))), 1e-12)

  # The log-logistic closed form keeps the same offset: bounds one ULP apart
  # at 0.1 with eta = log(0.1) and a shape of 1e17 have the same rounded log,
  # and a direct upper score read the interval as empty, -1.386 for
  # log(1/2). The upper centered log time is formed from the lower one plus
  # log(u / l) there, and directly where the upper bound is the one nearer
  # the center, which is the crossing case pinned above.
  ref <- log(plogis(1e17 * log1p((upper - 0.1) / 0.1)) - 0.5)
  expect_lt(abs(env$surv_ll_status(7L, upper, lower, 0, 3L, eta, 1e17, 0) -
                  ref), 1e-9)
  # The same for the entry: entry at 0.1 and lower bound one ULP above it
  # share a rounded log, and a direct lower score put the two at the same
  # point while the conditioning still counted their separation.
  score <- function(t) 1e17 * log1p((t - 0.1) / 0.1)
  ref <- log(plogis(score(0.2)) - plogis(score(upper))) -
    log(1 - plogis(score(lower)))
  expect_lt(abs(env$surv_ll_status(7L, 0.2, upper, lower, 3L, eta, 1e17, 0) -
                  ref), 1e-9)
})

test_that("an overflowing incomplete-gamma increment is an interval holding the rest", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("flexsurv")
  env <- expose_survival_likelihood()

  # A generalized gamma with sigma 0.0009 over (1, 2] has an upper
  # incomplete-gamma argument of exp(780). The survival ratio is zero to
  # double precision and the increment -Inf, which resolves the interval as
  # holding everything past S(1); the continued-fraction factor at an
  # infinite argument gave NaN instead, and the interval fell to a
  # quadrature whose grid could not see the layer next to the lower bound,
  # 8 log units high.
  ref <- flexsurv::pgengamma(1, mu = -0.009, sigma = 0.0009, Q = 1,
                             lower.tail = FALSE, log.p = TRUE)
  expect_lt(abs(env$surv_ll_status(9L, 2, 1, 0, 3L, -0.009, 0.0009, 1) - ref),
            1e-6)
  entry <- flexsurv::pgengamma(0.5, mu = -0.009, sigma = 0.0009, Q = 1,
                               lower.tail = FALSE, log.p = TRUE)
  expect_lt(abs(env$surv_ll_status(9L, 2, 1, 0.5, 3L, -0.009, 0.0009, 1) -
                  (ref - entry)), 1e-6)
})

test_that("the quadrature resolves a layer next to either endpoint", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # Called directly, since the routes above keep these intervals off the
  # quadrature. A log-normal with sigma 0.01 over (1, 2] puts the mass in a
  # layer of width 1e-4 in log time next to whichever bound is nearer the
  # center: a single Simpson pass does not converge, and returning its last
  # estimate was the error. The interval is cut geometrically toward the
  # heavier endpoint and the pieces summed innermost first.
  log_surv <- function(t, eta) {
    plnorm(t, eta, 0.01, lower.tail = FALSE, log.p = TRUE)
  }
  log_cdf <- function(t, eta) plnorm(t, eta, 0.01, log.p = TRUE)
  ref <- log_surv(1, -0.5) + log1p(-exp(log_surv(2, -0.5) - log_surv(1, -0.5)))
  got <- env$log_interval_prob_quad(6L, 2, 1, -0.5, 0.01, 0)
  expect_lt(abs(got - ref) / abs(ref), 1e-12)
  eta <- log(2) + 0.5
  ref <- log_cdf(2, eta) + log1p(-exp(log_cdf(1, eta) - log_cdf(2, eta)))
  got <- env$log_interval_prob_quad(6L, 2, 1, eta, 0.01, 0)
  expect_lt(abs(got - ref) / abs(ref), 1e-12)
  # And a gamma with shape 30 over (0.1, 0.2], whose mass sits at the upper
  # bound of a lower-tail interval.
  ref <- pgamma(0.2, 30, log.p = TRUE) +
    log1p(-exp(pgamma(0.1, 30, log.p = TRUE) - pgamma(0.2, 30, log.p = TRUE)))
  got <- env$log_interval_prob_quad(8L, 0.2, 0.1, 0, 30, 0)
  expect_lt(abs(got - ref) / abs(ref), 1e-11)
})

test_that("two log-logistic scores that straddle the center are taken directly", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # Entry and lower bound at equal rounded distances on opposite sides of
  # the center: formed as the entry score plus log(l / e) the lower score is
  # two opposite terms that cancel to rounding, which a shape of 1e16 turns
  # into units. Straddling points take the direct form. The reference is
  # formed in the log domain, since both tails underflow.
  entry <- 0.012532072394604387
  lower <- 0.10318293758574673
  eta <- -3.3253579511021445
  expect_equal(abs(log(lower) - eta), abs(log(entry) - eta))
  a <- 1e16
  z <- function(t) a * (log(t) - eta)
  # log(F(u) - F(l)) = -z_l + log1p(-exp(-(z_u - z_l))) for large z, and
  # S(e) is 1 to rounding.
  ref <- -z(lower) + log1p(-exp(-(z(0.2) - z(lower)))) -
    plogis(z(entry), lower.tail = FALSE, log.p = TRUE)
  got <- env$surv_ll_status(7L, 0.2, lower, entry, 3L, eta, a, 0)
  expect_lt(abs(got - ref) / abs(ref), 1e-12)
})

test_that("an overflowing cumulative-hazard endpoint does not lose a finite increment", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  env <- expose_survival_likelihood()

  # A Gamma of shape 1 is an exponential: at eta = -710 its rate is
  # exp(710), so both cumulative hazards, at entry 1 and one ULP above it,
  # are beyond the double range. Their difference is exp(710) * 2^-52, a
  # finite log likelihood near -4.96e292. The tail branch returned -Inf as
  # soon as an ENDPOINT overflowed, and this right-censored observation
  # under delayed entry became impossible; the decision is now on the
  # increment. This is the neighboring status to the interval test above:
  # not an interval whose conditional probability rounds to one.
  entry <- 1
  upper <- 1 + .Machine$double.eps
  for (eta in c(-710, -720)) {
    ref <- -exp(-eta + log(upper - entry))
    expect_true(is.finite(ref))
    gamma <- env$surv_ll_status(8L, upper, 0, entry, 0L, eta, 1, 1)
    expaft <- env$surv_ll_status(4L, upper, 0, entry, 0L, eta, 1, 1)
    expect_true(is.finite(gamma))
    expect_lt(abs(gamma / ref - 1), 1e-12)
    expect_lt(abs(expaft / ref - 1), 1e-12)
    # An exact event under the same entry: log h(t) is -eta and the rest
    # is the same increment.
    event_ref <- -eta + ref
    expect_lt(abs(env$surv_ll_status(8L, upper, 0, entry, 1L, eta, 1, 1) /
                    event_ref - 1), 1e-12)
    expect_lt(abs(env$surv_ll_status(4L, upper, 0, entry, 1L, eta, 1, 1) /
                    event_ref - 1), 1e-12)
    # The derivative in eta is representable too: d/d eta of
    # -(u - e) exp(-eta) is (u - e) exp(-eta), the increment itself.
    h <- 1e-6
    fd <- (env$surv_ll_status(8L, upper, 0, entry, 0L, eta + h, 1, 1) -
             env$surv_ll_status(8L, upper, 0, entry, 0L, eta - h, 1, 1)) /
      (2 * h)
    expect_lt(abs(fd / (-ref) - 1), 1e-8)
  }
  # And an increment that itself exceeds the range is still the certain
  # event it was: the survival ratio is zero to double precision.
  expect_identical(env$surv_ll_status(8L, 2, 0, 1, 0L, -720, 1, 1), -Inf)
})
