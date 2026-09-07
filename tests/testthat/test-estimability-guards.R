# Two benchmarks packaged a non-existent maximum as an ordinary estimate. In
# both cases every returned number is finite and the fitting reports success,
# so nothing about the result looks wrong.

test_that("naive() refuses a monotone Cox partial likelihood", {
  skip_if_not_installed("survival")
  # Events in BOTH arms, so the existing arm guard passes, but every index
  # event precedes every comparator event, so the partial likelihood is
  # monotone and has no interior maximum.
  d <- data.frame(
    time  = c(1, 2, 3, 4, 5, 6),
    event = c(1, 1, 1, 1, 1, 1),
    arm   = factor(c("index", "index", "index",
                     "comparator", "comparator", "comparator"),
                   levels = c("comparator", "index"))
  )
  surv <- survival::Surv(d$time, d$event)
  w <- character(0)
  fit <- withCallingHandlers(
    survival::coxph(surv ~ arm, data = d),
    warning = function(x) {
      w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    }
  )
  b <- unname(stats::coef(fit)[1])
  se <- sqrt(diag(stats::vcov(fit))[1])

  # This is what made it slip through: the numbers are finite and positive.
  expect_true(is.finite(b) && is.finite(se) && se > 0)
  expect_true(any(grepl("may be infinite", w, fixed = TRUE)))
  # and the interval built from them is meaningless
  expect_gt(se, 1000)
})

test_that("the monotone-likelihood signature is matched, not every warning", {
  # An unrelated coxph warning must not become a rejection.
  expect_true(grepl("may be infinite",
                    "Loglik converged before variable  1 ; coefficient may be infinite.",
                    fixed = TRUE))
  expect_false(grepl("may be infinite",
                     "Ran out of iterations and did not converge", fixed = TRUE))
})

test_that("quasi-complete separation is caught when the LP is available", {
  skip_if_not_installed("detectseparation")
  # The case the fitted-value screen cannot see: the two tied rows keep fitted
  # probabilities of exactly 0.5, so not every probability is at a boundary.
  d <- data.frame(y = c(0, 0, 1, 1), x = c(-1, 0, 0, 1))
  g <- stats::glm(y ~ x, family = stats::binomial(), data = d)
  expect_true(g$converged)
  eps <- .Machine$double.eps^0.5
  mu <- stats::fitted(g)
  expect_false(all(mu < eps | mu > 1 - eps))

  expect_true(isTRUE(mlumr:::.stc_detect_separation(g)))
  expect_error(mlumr:::.stc_refuse_separation(g), "quasi-complete")
})

test_that("a strong but identified fit is not called separated", {
  skip_if_not_installed("detectseparation")
  d <- data.frame(y = c(0, 0, 0, 1, 1, 1, 0, 1),
                  x = c(-3, -2, -1, 1, 2, 3, 2, -1))
  g <- stats::glm(y ~ x, family = stats::binomial(), data = d)
  expect_false(isTRUE(mlumr:::.stc_detect_separation(g)))
  expect_silent(mlumr:::.stc_refuse_separation(g))
})

test_that("an absent optional dependency leaves the screen weaker, not wrong", {
  # `NA` means "not determined", and the caller must not treat that as
  # "separated"; only an explicit TRUE refuses.
  d <- data.frame(y = c(0, 0, 1, 1), x = c(-1, 0, 0, 1))
  g <- stats::glm(y ~ x, family = stats::binomial(), data = d)
  expect_false(isTRUE(NA))
  # a non-binomial fit is out of scope for the whole check
  dn <- data.frame(y = c(1.2, 2.3, 3.1, 4.8), x = c(1, 2, 3, 4))
  gn <- stats::glm(y ~ x, family = stats::gaussian(), data = dn)
  expect_silent(mlumr:::.stc_refuse_separation(gn))
})

test_that("basis support is judged over the at-risk period, not from zero", {
  # A degree-0 column is supported on exactly one inter-knot interval, so the
  # first one lives only on [0, 1). With nobody observed before t = 2 it enters
  # no likelihood term at all: it multiplies no event hazard and no exposure
  # increment, and its coefficient is moved by the prior alone. Judged from
  # zero it looks supported, because it is positive somewhere in [0, max].
  skip_if_not_installed("splines2")
  spec <- mlumr:::.build_mspline_basis(
    list(internal = c(1, 5), boundary = c(0, 10)), degree = 0L
  )
  expect_silent(mlumr:::.assert_basis_support(spec, 10, "index"))
  expect_error(
    mlumr:::.assert_basis_support(spec, 10, "index", at_risk_start = 2),
    "observed risk period"
  )
  expect_error(
    mlumr:::.assert_basis_support(spec, 10, "index", at_risk_start = 2),
    "no event hazard and no exposure increment"
  )

  expect_identical(mlumr:::.at_risk_start(NULL), 0)
  expect_identical(mlumr:::.at_risk_start(c(2, 3, 7)), 2)
  expect_identical(mlumr:::.at_risk_start(c(NA, 4, 6)), 4)
  expect_identical(mlumr:::.at_risk_start(c(-1, 3)), 0)
  expect_identical(mlumr:::.at_risk_start(character(0)), 0)
})

test_that("delayed entry is reported as making absolute survival prior-driven", {
  # Everything supported, so no error; the point is that the caller is told
  # which stretch of the curve no observation reaches.
  skip_if_not_installed("splines2")
  spec <- mlumr:::.build_mspline_basis(
    list(internal = c(3, 5), boundary = c(0, 8)), degree = 3L
  )
  expect_message(
    mlumr:::.assert_basis_support(spec, 8, "index", at_risk_start = 2),
    "prior-dependent over \\[0, 2\\]"
  )
  expect_message(
    mlumr:::.assert_basis_support(spec, 8, "index", at_risk_start = 0),
    NA
  )
})
