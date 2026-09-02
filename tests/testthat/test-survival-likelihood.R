# Deterministic reference tests for the survival log-likelihood building blocks
# (no Stan). `.r_log_surv()` mirrors the Stan `log_surv_scalar()` used in the
# survival models, so checking it against stats/flexsurv validates all nine
# parametric distributions formula-for-formula.

test_that(".r_log_surv matches stats reference distributions", {
  rls <- mlumr:::.r_log_surv
  t <- c(0.4, 1.0, 1.7, 3.5)
  eta <- 0.3
  aux <- 1.4

  # 1 = Exponential PH: rate = exp(eta)
  expect_equal(rls(1L, t, eta, aux, 1),
               stats::pexp(t, rate = exp(eta), lower.tail = FALSE, log.p = TRUE))
  # 4 = Exponential AFT: rate = exp(-eta)
  expect_equal(rls(4L, t, eta, aux, 1),
               stats::pexp(t, rate = exp(-eta), lower.tail = FALSE, log.p = TRUE))
  # 5 = Weibull AFT: shape = aux, scale = exp(eta)
  expect_equal(rls(5L, t, eta, aux, 1),
               stats::pweibull(t, shape = aux, scale = exp(eta),
                               lower.tail = FALSE, log.p = TRUE))
  # 6 = Log-normal: meanlog = eta, sdlog = aux
  expect_equal(rls(6L, t, eta, aux, 1),
               stats::plnorm(t, meanlog = eta, sdlog = aux,
                             lower.tail = FALSE, log.p = TRUE))
  # 8 = Gamma: shape = aux, rate = exp(-eta)
  expect_equal(rls(8L, t, eta, aux, 1),
               stats::pgamma(t, shape = aux, rate = exp(-eta),
                             lower.tail = FALSE, log.p = TRUE))
})

test_that(".r_log_surv matches flexsurv reference distributions", {
  skip_if_not_installed("flexsurv")
  rls <- mlumr:::.r_log_surv
  t <- c(0.4, 1.0, 1.7, 3.5)
  eta <- 0.3
  aux <- 1.4
  aux2 <- 0.8

  # 2 = Weibull PH: shape = aux, scale = exp(eta)
  expect_equal(rls(2L, t, eta, aux, 1),
               flexsurv::pweibullPH(t, shape = aux, scale = exp(eta),
                                    lower.tail = FALSE, log.p = TRUE))
  # 3 = Gompertz PH: shape = aux, rate = exp(eta)
  expect_equal(rls(3L, t, eta, aux, 1),
               flexsurv::pgompertz(t, shape = aux, rate = exp(eta),
                                   lower.tail = FALSE, log.p = TRUE))
  # 7 = Log-logistic: shape = aux, scale = exp(eta)
  expect_equal(rls(7L, t, eta, aux, 1),
               flexsurv::pllogis(t, shape = aux, scale = exp(eta),
                                 lower.tail = FALSE, log.p = TRUE))
  # 9 = Generalized gamma (Lawless k = aux2 -> Prentice Q = 1/sqrt(k))
  expect_equal(rls(9L, t, eta, aux, aux2),
               flexsurv::pgengamma(t, mu = eta, sigma = aux, Q = 1 / sqrt(aux2),
                                   lower.tail = FALSE, log.p = TRUE))
})

test_that(".r_log_surv is a valid log-survival (monotone, S(0+)=1, finite tails)", {
  rls <- mlumr:::.r_log_surv
  for (dist in 1:9) {
    ls_small <- rls(dist, 1e-6, 0.2, 1.3, 0.9)
    ls_big <- rls(dist, 50, 0.2, 1.3, 0.9)
    expect_true(is.finite(ls_small) && ls_small <= 1e-6)   # S near 1 -> logS ~ 0
    expect_true(is.finite(ls_big) && ls_big < ls_small)    # decreasing, no NaN
  }
})

test_that("the log(exp(z) - 1) helper guards on z, not on log(z)", {
  # The Stan helper takes log(z) and returns log(exp(z) - 1). expm1(z) leaves
  # double precision near z = 709, so the guard belongs on z = exp(log_x). It
  # was written against log_x, so it fired only above exp(700) and every log_x
  # between log(709) and 700 returned log(Inf): for a Gompertz fit with delayed
  # entry or interval censoring the log-likelihood became -Inf and the draw was
  # rejected, at parameter values whose true log difference is an ordinary
  # finite number. The same helper serves the Weibull cumulative-hazard
  # difference, so a large shape with a wide interval hit it too.
  #
  # Mirrored in R because the Stan function is not callable from here; the
  # threshold and the branch are what this pins.
  helper <- function(log_x) {
    if (log_x < -10) {
      x <- exp(log_x)
      return(log_x + log1p(0.5 * x + x^2 / 6))
    }
    if (log_x > log(700)) return(exp(log_x))
    log(expm1(exp(log_x)))
  }
  # Below the threshold the exact form is used and is correct.
  for (lx in c(-12, -5, 0, 3, 6, 6.5)) {
    expect_equal(helper(lx), log(expm1(exp(lx))), tolerance = 1e-10,
                 info = sprintf("log_x = %g", lx))
  }
  # Above it the exact form overflows and the approximation is the answer.
  for (lx in c(6.6, 7, 10, 20)) {
    expect_true(is.infinite(log(expm1(exp(lx)))), info = sprintf("log_x = %g", lx))
    expect_equal(helper(lx), exp(lx), tolerance = 1e-12,
                 info = sprintf("log_x = %g", lx))
  }
  # And the Stan source carries the corrected threshold.
  src <- readLines(system.file("stan", "include", "survival_functions.stan",
                               package = "mlumr"))
  expect_true(any(grepl("log_x > log(700)", src, fixed = TRUE)))
  expect_false(any(grepl("log_x > 700)", src, fixed = TRUE)))
})
