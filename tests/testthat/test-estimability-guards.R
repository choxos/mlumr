# Two benchmarks packaged a non-existent maximum as an ordinary estimate. In
# both cases every returned number is finite and the fitting reports success,
# so nothing about the result looks wrong.
#
# These call the public functions. An earlier version of this file checked
# that survival::coxph() warns and that grepl() matches a string, which would
# have kept passing with naive()'s guard deleted.

# A survival data set whose times and statuses are set outright, so the risk
# sets are exactly the ones under test.
.arm_timed_data <- function(index_time, index_status,
                            comparator_time, comparator_status) {
  dat <- sim_survival_data(seed = 2026, n_ipd = length(index_time),
                           n_agd = length(comparator_time), n_int = 8)
  dat$ipd$data$.time <- index_time
  dat$ipd$data$.status <- as.integer(index_status)
  dat$agd$pseudo_ipd$.time <- comparator_time
  dat$agd$pseudo_ipd$.status <- as.integer(comparator_status)
  dat
}

test_that("naive() refuses a monotone Cox partial likelihood", {
  skip_if_not_installed("survival")
  # Events in BOTH arms, so the arm guard passes, every index event before
  # every comparator event, and nothing censored, so no index subject is at
  # risk when the comparator fails. The partial likelihood is monotone.
  dat <- .arm_timed_data(1:10, rep(1, 10), 11:20, rep(1, 10))
  expect_error(naive(dat), "no interior maximum")
})

test_that("ordered event times alone do not make naive() refuse", {
  skip_if_not_installed("survival")
  # The same ordering, with censoring. Every index event still precedes every
  # comparator event, but the censored index subject is at risk when the
  # comparator fails, and that one risk-set comparison gives the partial
  # likelihood r/(2r + 2) * 1/(r + 2), whose maximum is at r = sqrt(2). A
  # refusal here would be a false positive on an estimable fit.
  dat <- .arm_timed_data(c(1, 4), c(1, 0), c(2, 3), c(1, 0))
  res <- naive(dat)
  expect_s3_class(res, "mlumr_naive")
  expect_equal(res$estimate, log(sqrt(2)), tolerance = 1e-8)
})

test_that("naive() refuses a Cox fit that stopped without converging", {
  skip_if_not_installed("survival")
  # coxph() documents several termination conditions and says its own
  # detection of an infinite coefficient is not always successful, so the
  # absence of the monotone warning is not a certificate that a finite maximum
  # exists. Reissuing the nonconvergence warning and then returning a Wald
  # interval presented the state the iteration stopped in as an estimate.
  real_coxph <- survival::coxph
  local_mocked_bindings(
    coxph = function(...) {
      warning("Ran out of iterations and did not converge")
      real_coxph(...)
    },
    .package = "survival"
  )
  dat <- .arm_timed_data(c(1, 4), c(1, 0), c(2, 3), c(1, 0))
  expect_error(naive(dat), "did not converge")
})

test_that("an unrelated coxph warning is passed on, not turned into a refusal", {
  skip_if_not_installed("survival")
  real_coxph <- survival::coxph
  local_mocked_bindings(
    coxph = function(...) {
      warning("Loglik converged before variable 1")
      real_coxph(...)
    },
    .package = "survival"
  )
  dat <- .arm_timed_data(c(1, 4), c(1, 0), c(2, 3), c(1, 0))
  expect_warning(res <- naive(dat), "Loglik converged before variable 1")
  expect_s3_class(res, "mlumr_naive")
})

test_that("quasi-complete separation is reported as separated, and refused", {
  skip_if_not_installed("detectseparation")
  # The case the fitted-value screen cannot see: the two tied rows keep fitted
  # probabilities of exactly 0.5, so not every probability is at a boundary.
  d <- data.frame(y = c(0, 0, 1, 1), x = c(-1, 0, 0, 1))
  g <- stats::glm(y ~ x, family = stats::binomial(), data = d)
  expect_true(g$converged)
  eps <- .Machine$double.eps^0.5
  mu <- stats::fitted(g)
  expect_false(all(mu < eps | mu > 1 - eps))

  status <- mlumr:::.stc_separation_status(g)
  expect_identical(status$status, "separated")
  expect_error(mlumr:::.stc_refuse_separation(g), "quasi-complete")
})

test_that("a strong but identified fit is reported as not separated", {
  skip_if_not_installed("detectseparation")
  d <- data.frame(y = c(0, 0, 0, 1, 1, 1, 0, 1),
                  x = c(-3, -2, -1, 1, 2, 3, 2, -1))
  g <- stats::glm(y ~ x, family = stats::binomial(), data = d)
  # Explicitly NOT separated. `!isTRUE(...)` also passed for an unknown, which
  # is what let a fit that was never checked look like one that was cleared.
  expect_identical(mlumr:::.stc_separation_status(g)$status, "not_separated")
  expect_silent(mlumr:::.stc_refuse_separation(g))
})

test_that("an unknown separation status warns rather than passing silently", {
  d <- data.frame(y = c(0, 0, 1, 1), x = c(-1, 0, 0, 1))
  g <- stats::glm(y ~ x, family = stats::binomial(), data = d)
  # A fit the check cannot be run on. The estimate is still returned, which is
  # the point: what must not happen is returning it as though it had been
  # checked.
  g$call <- NULL
  status <- mlumr:::.stc_separation_status(g)
  expect_identical(status$status, "unknown")
  expect_match(status$reason, "no call")
  expect_warning(mlumr:::.stc_refuse_separation(g), "did not run")
  expect_warning(mlumr:::.stc_refuse_separation(g), "unverified")
})

test_that("a non-binomial fit is out of scope for the separation check", {
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
    mlumr:::.assert_basis_support(spec, 10, "index", entry = c(2, 2),
                                  exit = c(9, 10)),
    "no event hazard and no exposure increment"
  )

  expect_error(
    mlumr:::.assert_basis_support(spec, 10, "index", entry = c(2, 2),
                                  exit = c(9, 10)),
    "observed risk set"
  )
})

test_that("a gap with an empty risk set is not treated as observed", {
  skip_if_not_installed("splines2")
  # Subjects seen on [1, 2] and [8, 9]: nobody is under observation in (2, 8),
  # so a degree-0 column living only there enters no likelihood term. Reducing
  # the history to one span from the first entry to the last exit would call it
  # supported.
  spec <- mlumr:::.build_mspline_basis(
    list(internal = c(2, 8), boundary = c(0, 9)), degree = 0L
  )
  expect_error(
    mlumr:::.assert_basis_support(spec, 9, "index",
                                  entry = c(1, 8), exit = c(2, 9)),
    "observed risk set"
  )
  # the same basis IS supported when the risk set is continuous
  expect_silent(
    mlumr:::.assert_basis_support(spec, 9, "index",
                                  entry = c(0, 0), exit = c(9, 9))
  )
})

test_that("risk intervals are merged, and degrade safely", {
  whole <- mlumr:::.risk_intervals(NULL, NULL, 10)
  expect_length(whole, 1L)
  expect_identical(unname(whole[[1L]]), c(0, 10))

  # overlapping intervals collapse; [0,2] and [1,3] do, and [5,9] is a gap away
  merged <- mlumr:::.risk_intervals(c(0, 1, 5), c(2, 3, 9), 9)
  expect_length(merged, 2L)
  expect_identical(unname(merged[[1L]]), c(0, 3))
  expect_identical(unname(merged[[2L]]), c(5, 9))

  # touching intervals do collapse
  touching <- mlumr:::.risk_intervals(c(0, 3), c(3, 9), 9)
  expect_length(touching, 1L)
  expect_identical(unname(touching[[1L]]), c(0, 9))

  # disjoint ones do not
  split <- mlumr:::.risk_intervals(c(1, 8), c(2, 9), 9)
  expect_length(split, 2L)
  expect_identical(unname(split[[1L]]), c(1, 2))
  expect_identical(unname(split[[2L]]), c(8, 9))

  # entry times with no matching exits still say where observation starts
  no_exit <- mlumr:::.risk_intervals(c(2, 3), NULL, 10)
  expect_length(no_exit, 1L)
  expect_identical(unname(no_exit[[1L]]), c(2, 10))

  # nothing usable falls back to the whole span rather than inventing one
  expect_identical(unname(mlumr:::.risk_intervals(c(NA, NA), c(NA, NA), 7)[[1L]]),
                   c(0, 7))
  expect_identical(unname(mlumr:::.risk_intervals(c(-1, 0), c(3, 4), 4)[[1L]]),
                   c(0, 4))
})

test_that("delayed entry is reported as making absolute survival prior-driven", {
  # Everything supported, so no error; the point is that the caller is told
  # which stretch of the curve no observation reaches.
  skip_if_not_installed("splines2")
  spec <- mlumr:::.build_mspline_basis(
    list(internal = c(3, 5), boundary = c(0, 8)), degree = 3L
  )
  expect_message(
    mlumr:::.assert_basis_support(spec, 8, "index",
                                  entry = c(2, 2), exit = c(7, 8)),
    "prior-dependent over \\[0, 2\\]"
  )
  expect_message(
    mlumr:::.assert_basis_support(spec, 8, "index",
                                  entry = c(0, 0), exit = c(7, 8)),
    NA
  )
})

test_that("risk intervals never come back inverted", {
  # `.validate_survival_times()` refuses a negative delayed-entry time, so the
  # public path cannot deliver one. The clamp inside `.risk_intervals()` says
  # the helper does not rely on that, and the keep filter has to agree with it:
  # judging the RAW entry admitted an interval lying wholly before zero, which
  # the clamp then inverted.
  whole <- mlumr:::.risk_intervals(-2, -1, 10)
  expect_length(whole, 1L)
  expect_equal(unname(whole[[1L]]), c(0, 10))

  # an interval that merely STARTS before zero still contributes, from zero
  spanning <- mlumr:::.risk_intervals(-2, 5, 10)
  expect_equal(unname(spanning[[1L]]), c(0, 5))

  # and one bad row does not take a good one with it
  mixed <- mlumr:::.risk_intervals(c(-4, 3), c(-3, 5), 10)
  expect_length(mixed, 1L)
  expect_equal(unname(mixed[[1L]]), c(3, 5))

  # the contract, over every shape above
  for (iv in c(whole, spanning, mixed)) {
    expect_gte(iv[["hi"]], iv[["lo"]])
    expect_gte(iv[["lo"]], 0)
  }
})

test_that("a column alive only at an event time is supported", {
  skip_if_not_installed("splines2")
  # The mirror of the gap test above. The cumulative hazard integrates over the
  # risk intervals, so a degree-0 column on [2, 8) that is positive only at
  # t = 2 adds no exposure. The EVENT term does not integrate: it evaluates the
  # hazard at each event time. So the same column is dead when nobody fails at
  # 2 and live when somebody does, and the two cases differ only by `event`.
  spec <- mlumr:::.build_mspline_basis(
    list(internal = c(2, 8), boundary = c(0, 9)), degree = 0L
  )
  # nobody fails at the endpoint: still dead
  expect_error(
    mlumr:::.assert_basis_support(spec, 9, "index", entry = c(1, 8),
                                  exit = c(2, 9), event = 9),
    "observed risk set"
  )
  # the first subject fails exactly at 2, which the event term evaluates.
  # Delayed entry emits its own note here, so this asserts the return value
  # rather than silence.
  expect_true(suppressMessages(
    mlumr:::.assert_basis_support(spec, 9, "index", entry = c(1, 8),
                                  exit = c(2, 9), event = c(2, 9))
  ))
  # An event time is not a licence to pass the rest of the basis: a column out
  # past the last exit is still dead, event times or not.
  far <- mlumr:::.build_mspline_basis(
    list(internal = c(2, 8, 20), boundary = c(0, 40)), degree = 0L
  )
  expect_error(
    mlumr:::.assert_basis_support(far, 9, "index", entry = c(1, 8),
                                  exit = c(2, 9), event = c(2, 9)),
    "no support"
  )
})
