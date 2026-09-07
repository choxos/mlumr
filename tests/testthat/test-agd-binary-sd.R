# A binary covariate's SAMPLE standard deviation uses the n-1 denominator, so
# it equals sqrt(n/(n-1) * p(1-p)) exactly and always exceeds the population
# value sqrt(p(1-p)). Validating against the population figure therefore
# rejected ordinary data: five zeros and five ones report mean 0.5 and sd
# 0.5270, and that was called impossible.

binary_agd <- function(p, s, n) {
  data.frame(study = "S", trt = "B", x_prop = p, x_sd = s, n = n, r = 5)
}

accepts <- function(p, s, n) {
  d <- binary_agd(p, s, n)
  isTRUE(tryCatch({
    mlumr:::.validate_agd_binary_covariates(d, "x_prop", "x_sd", "binary", "n")
    TRUE
  }, error = function(e) FALSE))
}

test_that("an ordinary binary sample SD is not called impossible", {
  x <- rep(c(0, 1), each = 5)
  expect_identical(mean(x), 0.5)
  # the exact sample SD, and the same figure as a table would print it
  expect_true(accepts(mean(x), stats::sd(x), 10))
  expect_true(accepts(0.5, 0.53, 10))

  y <- rep(c(0, 1), c(16, 4))
  expect_true(accepts(mean(y), stats::sd(y), 20))

  # n = 2 is the loosest any sample can be, and is what an unknown n assumes
  expect_true(accepts(0.5, stats::sd(c(0, 1)), NA))
})

test_that("genuinely inconsistent binary summaries are still refused", {
  expect_false(accepts(0.5, 0.9, 10))
  expect_false(accepts(0.5, 0.72, 10))   # above even the n = 2 ceiling
  expect_false(accepts(0.05, 0.4, 100))
  expect_error(
    mlumr:::.validate_agd_binary_covariates(binary_agd(0.5, 0.9, 10),
                                            "x_prop", "x_sd", "binary", "n"),
    "sqrt\\(n/\\(n-1\\)"
  )
})

test_that("the reported-precision allowance tracks the precision given", {
  # half a unit in the last place a number was actually written to
  expect_equal(mlumr:::.reported_precision(0.53), 0.005)
  expect_equal(mlumr:::.reported_precision(0.5), 0.05)
  expect_equal(mlumr:::.reported_precision(2), 0.5)
  # an unrounded value earns essentially nothing, so this is not blanket slack
  expect_lt(mlumr:::.reported_precision(0.5270462766947299), 1e-8)
})

test_that("the reported proportion's own rounding widens the ceiling", {
  # 493 ones out of 500: p = 0.986, sample SD 0.1176. A table printing both to
  # two decimals supplies 0.99 and 0.12, and a ceiling computed from 0.99 alone
  # is about 0.0996, which refuses a perfectly ordinary summary.
  x <- rep(c(0, 1), c(7, 493))
  expect_equal(round(mean(x), 3), 0.986)
  expect_equal(round(stats::sd(x), 4), 0.1176)
  expect_true(accepts(0.99, 0.12, 500))

  # one success in 25, reported to one decimal: p = 0.0 and SD = 0.2, for which
  # the displayed proportion alone gives a ceiling of exactly zero
  y <- rep(c(0, 1), c(24, 1))
  expect_equal(round(stats::sd(y), 2), 0.2)
  expect_true(accepts(0.0, 0.2, 25))

  # and the ceiling still binds where it should
  expect_false(accepts(0.99, 0.35, 500))
  expect_false(accepts(0.0, 0.6, 25))
  expect_false(accepts(0.5, 0.9, 10))
  expect_false(accepts(0.5, 0.72, 10))
  expect_false(accepts(0.05, 0.4, 100))
})
