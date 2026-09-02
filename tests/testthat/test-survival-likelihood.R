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
