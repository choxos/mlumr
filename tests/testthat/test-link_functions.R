# Tests for link function utilities

test_that("check_link returns correct defaults", {
  expect_equal(check_link("binomial")$link, "logit")
  expect_equal(check_link("binomial")$code, 1L)
  expect_equal(check_link("normal")$link, "identity")
  expect_equal(check_link("normal")$code, 1L)
  expect_equal(check_link("poisson")$link, "log")
  expect_equal(check_link("poisson")$code, 1L)
})

test_that("check_link returns canonical family name", {
  expect_equal(check_link("binomial")$family, "binomial")
  expect_equal(check_link("binary")$family, "binomial")
  expect_equal(check_link("count")$family, "binomial")
  expect_equal(check_link("normal")$family, "normal")
  expect_equal(check_link("continuous")$family, "normal")
  expect_equal(check_link("poisson")$family, "poisson")
  expect_equal(check_link("rate")$family, "poisson")
})

test_that("check_link resolves data-type aliases", {
  # binary -> binomial

  expect_equal(check_link("binary")$link, "logit")
  expect_equal(check_link("binary", "probit")$code, 2L)
  expect_equal(check_link("binary", "cloglog")$code, 3L)

  # count -> binomial (from multinma convention: r/n aggregate counts)
  expect_equal(check_link("count")$link, "logit")
  expect_equal(check_link("count", "probit")$link, "probit")

  # rate -> poisson
  expect_equal(check_link("rate")$link, "log")
  expect_equal(check_link("rate")$code, 1L)

  # continuous -> normal
  expect_equal(check_link("continuous")$link, "identity")
  expect_equal(check_link("continuous", "log")$code, 2L)
})

test_that("check_link returns correct codes for all valid links", {
  expect_equal(check_link("binomial", "logit")$code, 1L)
  expect_equal(check_link("binomial", "probit")$code, 2L)
  expect_equal(check_link("binomial", "cloglog")$code, 3L)

  expect_equal(check_link("normal", "identity")$code, 1L)
  expect_equal(check_link("normal", "log")$code, 2L)

  expect_equal(check_link("poisson", "log")$code, 1L)
})

test_that("check_link is case-insensitive", {
  expect_equal(check_link("binomial", "Logit")$link, "logit")
  expect_equal(check_link("binomial", "PROBIT")$link, "probit")
  expect_equal(check_link("normal", "LOG")$link, "log")
  expect_equal(check_link("Binary", "cloglog")$link, "cloglog")
  expect_equal(check_link("CONTINUOUS")$link, "identity")
})

test_that("check_link errors on invalid links", {
  expect_error(check_link("binomial", "identity"), "not valid for family")
  expect_error(check_link("binomial", "log"), "not valid for family")
  expect_error(check_link("normal", "logit"), "not valid for family")
  expect_error(check_link("poisson", "logit"), "not valid for family")
  expect_error(check_link("normal", "probit"), "not valid for family")
  expect_error(check_link("poisson", "identity"), "not valid for family")
})

test_that("check_link errors on unknown family", {
  expect_error(check_link("gamma"), "Unknown family")
  expect_error(check_link("ordered"), "Unknown family")
})

test_that("check_link rejects malformed scalar inputs cleanly", {
  expect_error(check_link(c("binomial", "normal")),
               "`family` must be a single non-missing string")
  expect_error(check_link(NA_character_),
               "`family` must be a single non-missing string")
  expect_error(check_link(""),
               "`family` must be a single non-missing string")
  expect_error(check_link(1),
               "`family` must be a single non-missing string")

  expect_error(check_link("binomial", c("logit", "probit")),
               "`link` must be a single non-missing string")
  expect_error(check_link("binomial", NA_character_),
               "`link` must be a single non-missing string")
  expect_error(check_link("binomial", ""),
               "`link` must be a single non-missing string")
})

test_that("inverse_link matches expected functions", {
  x <- seq(-3, 3, by = 0.5)
  expect_equal(inverse_link(x, "identity"), x)
  expect_equal(inverse_link(x, "log"), exp(x))
  expect_equal(inverse_link(x, "logit"), plogis(x))
  expect_equal(inverse_link(x, "probit"), pnorm(x))
  expect_equal(inverse_link(x, "cloglog"), 1 - exp(-exp(x)))
})

test_that("cloglog inverse link is stable for extreme predictors", {
  eta <- c(-1000, -50, 0, 50, 1000)
  out <- inverse_link(eta, "cloglog")

  expect_equal(out[[1L]], 0)
  expect_equal(out[[2L]], -expm1(-exp(-50)))
  expect_equal(out[[3L]], 1 - exp(-1))
  expect_equal(out[[4L]], 1)
  expect_equal(out[[5L]], 1)
  expect_true(all(is.finite(out)))
})

test_that("link_fun matches expected functions", {
  p <- seq(0.1, 0.9, by = 0.1)
  expect_equal(link_fun(p, "identity"), p)
  expect_equal(link_fun(p, "logit"), qlogis(p))
  expect_equal(link_fun(p, "probit"), qnorm(p))
  expect_equal(link_fun(exp(1:3), "log"), 1:3)
})

test_that("inverse_link and link_fun are inverses", {
  p <- seq(0.05, 0.95, by = 0.05)
  for (lnk in c("logit", "probit", "cloglog")) {
    expect_equal(inverse_link(link_fun(p, lnk), lnk), p, tolerance = 1e-10,
                 label = paste("roundtrip for", lnk))
  }

  x <- seq(-2, 2, by = 0.5)
  expect_equal(link_fun(inverse_link(x, "logit"), "logit"), x, tolerance = 1e-10)
  expect_equal(link_fun(inverse_link(x, "probit"), "probit"), x, tolerance = 1e-10)
  expect_equal(link_fun(inverse_link(x, "cloglog"), "cloglog"), x, tolerance = 1e-10)

  y <- exp(seq(0.1, 3, by = 0.5))
  expect_equal(inverse_link(link_fun(y, "log"), "log"), y, tolerance = 1e-10)
})

test_that("link_fun handles boundary values safely", {
  # Should not produce Inf or NaN
  expect_true(is.finite(link_fun(0, "logit")))
  expect_true(is.finite(link_fun(1, "logit")))
  expect_true(is.finite(link_fun(0, "probit")))
  expect_true(is.finite(link_fun(1, "probit")))
  expect_true(is.finite(link_fun(0, "cloglog")))
  expect_true(is.finite(link_fun(1, "cloglog")))
  expect_true(is.finite(link_fun(0, "log")))
})

test_that("link helpers reject non-numeric response inputs cleanly", {
  expect_error(inverse_link("x", "logit"), "`x` must be numeric")
  expect_error(link_fun("x", "logit"), "`x` must be numeric")
  expect_error(link_derivative_response("x", "logit"), "`p` must be numeric")
  expect_error(binomial_link_variance("x", 10, "logit"), "`p` must be numeric")
})

test_that("bound_probability validates counts and min_count", {
  expect_equal(
    bound_probability(c(0, 0.2, 1), n = 10),
    c(0.5 / 11, 0.2, 10.5 / 11)
  )
  expect_error(bound_probability(0.2, n = 0),
               "`n` must contain positive finite values")
  expect_error(bound_probability(0.2, n = Inf),
               "`n` must contain positive finite values")
  expect_error(bound_probability(0.2, n = 10, min_count = 0),
               "`min_count` must contain positive finite values")
  expect_error(bound_probability(0.2, n = 10, min_count = 6),
               "`min_count` must be no larger than n / 2")
})

test_that("log-scale differences evaluate only the applicable branch", {
  log_x <- c(log(0.8), log(0.2), -Inf, 0)
  log_y <- c(log(0.2), log(0.8), 0, -Inf)

  expect_warning(
    out <- mlumr:::.exp_difference_logs(log_x, log_y),
    NA
  )
  expect_equal(out, exp(log_x) - exp(log_y), tolerance = 1e-15)
})

test_that("binomial link derivatives match existing formulas", {
  p <- 0.37
  n <- 123

  expect_equal(link_derivative_response(p, "logit"), 1 / (p * (1 - p)))
  expect_equal(link_derivative_response(p, "probit"), 1 / dnorm(qnorm(p)))
  expect_equal(
    link_derivative_response(p, "cloglog"),
    1 / ((1 - p) * (-log1p(-p)))
  )

  expect_equal(
    binomial_link_variance(p, n, "logit"),
    1 / (n * p * (1 - p))
  )
  expect_equal(
    binomial_link_variance(p, n, "probit"),
    p * (1 - p) / (n * dnorm(qnorm(p))^2)
  )
})

test_that("inverse link derivatives and binomial variances stay finite at boundaries", {
  eta <- c(-1000, 0, 1000)

  expect_equal(inverse_link_derivative(eta, link = "cloglog")[[1L]], 0)
  expect_equal(inverse_link_derivative(eta, link = "cloglog")[[3L]], 0)
  expect_true(all(is.finite(inverse_link_derivative(eta, link = "cloglog"))))

  vars <- binomial_link_variance(c(0, 0.5, 1), n = 100, link = "logit")
  expect_true(all(is.finite(vars)))
  expect_true(all(vars > 0))
  expect_error(binomial_link_variance(0.5, n = 0, link = "logit"),
               "`n` must contain positive finite values")
})

test_that("mlumr_message respects verbose flag", {
  expect_message(mlumr_message("visible", verbose = TRUE), "visible")
  expect_message(mlumr_message("hidden", verbose = FALSE), NA)
  expect_error(mlumr_message("bad", verbose = NA),
               "`verbose` must be TRUE or FALSE")
})

test_that("the binary log-probability helpers propagate NA on every link", {
  # `.binary_log_probs()` gated its cloglog series expansion on `if (any(small))`
  # with `small <- eta < -18`, and `.binary_link_from_logs()` indexed an
  # assignment by `log_event <= log(0.5)`. A missing `eta` made the first an
  # error ("missing value where TRUE/FALSE needed") and the second an illegal
  # subscript, while the logit branch of both returned NA as it should. Missing
  # input must propagate, identically, on all three links.
  eta <- c(NA_real_, -30, 0, 2)

  for (lk in c("logit", "probit", "cloglog")) {
    lp <- expect_silent(mlumr:::.binary_log_probs(eta, lk))
    expect_true(is.na(lp$event[1]), info = lk)
    expect_true(is.na(lp$nonevent[1]), info = lk)
    expect_true(all(is.finite(lp$event[-1])), info = lk)
    expect_true(all(is.finite(lp$nonevent[-1])), info = lk)

    back <- expect_silent(
      mlumr:::.binary_link_from_logs(lp$event, lp$nonevent, lk))
    expect_true(is.na(back[1]), info = lk)
    # ...and the non-missing entries still round-trip to the eta they came from.
    expect_equal(back[-1], eta[-1], tolerance = 1e-6, info = lk)
  }
})

test_that(".exp_difference_logs recycles and handles equal logarithms", {
  ed <- mlumr:::.exp_difference_logs
  # Unequal lengths used to leave an NA tail: `out` was sized from `log_x` while
  # the comparisons recycled, so the answer was silently truncated.
  expect_equal(ed(c(0, log(2)), 0), c(0, 1))
  expect_equal(ed(0, c(0, log(2))), c(0, -1))
  # Equal logarithms are exactly zero, including both infinite cases.
  expect_equal(ed(-Inf, -Inf), 0)
  expect_equal(ed(Inf, Inf), 0)
  # Cancellation is done before returning to the natural scale.
  expect_equal(ed(log(1), log1p(-1e-15)), 1e-15, tolerance = 1e-6)
  expect_true(is.nan(ed(NA_real_, 0)))
})

test_that("bound_probability corrects only the boundaries", {
  # It used to clamp every input into [min_count/n, 1 - min_count/n]. It now
  # leaves interior probabilities alone and replaces an observed 0 or 1 with the
  # pseudo-count estimate (r + c) / (n + 2c), which for c = 0.5 and r = 0 is
  # 0.5 / (n + 1), not 0.5 / n.
  expect_equal(bound_probability(c(0, 0.2, 1), n = 10),
               c(0.5 / 11, 0.2, 10.5 / 11))
  # An interior value below the old clip boundary is no longer moved.
  expect_equal(bound_probability(0.01, n = 10), 0.01)
  expect_true(all(bound_probability(c(0, 1), n = 10) > 0))
  expect_true(all(bound_probability(c(0, 1), n = 10) < 1))
})
