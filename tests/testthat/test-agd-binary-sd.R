# A binary covariate's SAMPLE standard deviation uses the n-1 denominator, so
# it equals sqrt(n/(n-1) * p(1-p)) exactly and always exceeds the population
# value sqrt(p(1-p)). Validating against the population figure therefore
# rejected ordinary data: five zeros and five ones report mean 0.5 and sd
# 0.5270, and that was called impossible.
#
# The ceiling uses n = 2, the loosest factor any sample can have. `n` below is
# the row's OUTCOME count, which is not the covariate's denominator and must
# not change the verdict.

binary_agd <- function(p, s, n) {
  data.frame(study = "S", trt = "B", x_prop = p, x_sd = s, n = n, r = 5)
}

accepts <- function(p, s, n) {
  d <- binary_agd(p, s, n)
  isTRUE(tryCatch({
    mlumr:::.validate_agd_binary_covariates(d, "x_prop", "x_sd", "binary")
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
                                            "x_prop", "x_sd", "binary"),
    "sqrt\\(2 \\* p"
  )
})

test_that("the reported-precision allowance tracks the precision given", {
  # half a unit in the last place a number was actually written to
  expect_equal(mlumr:::.reported_precision(0.53), 0.005)
  expect_equal(mlumr:::.reported_precision(0.5), 0.05)
  expect_equal(mlumr:::.reported_precision(2), 0.5)
  # an unrounded value earns essentially nothing, so this is not blanket slack
  expect_lt(mlumr:::.reported_precision(0.5270462766947299), 1e-8)

  # and precision finer than eight decimals is still precision. Stopping there
  # called a nine-decimal figure exact, so it was compared against a ceiling it
  # sat 2e-10 above and could only miss.
  x <- rep(c(0, 1), each = 5)
  expect_equal(mlumr:::.reported_precision(round(stats::sd(x), 9)), 5e-10)
  expect_equal(mlumr:::.reported_precision(0.123456789012), 5e-13)
  expect_true(accepts(0.5, round(stats::sd(x), 9), 10))
  expect_true(accepts(0.5, round(stats::sd(x), 12), 10))
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

  # A proportion printed with no decimals at all carries a half-unit
  # allowance, so 0 is consistent with p up to 0.5 and the ceiling there is
  # the full 0.7071. That is loose, and deliberately so: the alternative is
  # deciding on the caller's behalf how many decimals a table "meant" to
  # print. The guard still refuses what no binary sample can produce.
  expect_true(accepts(0.0, 0.6, 25))
  expect_false(accepts(0.0, 0.75, 25))

  # and the ceiling still binds where it should
  expect_false(accepts(0.99, 0.35, 500))
  expect_false(accepts(0.5, 0.9, 10))
  expect_false(accepts(0.5, 0.72, 10))
  expect_false(accepts(0.05, 0.4, 100))
})

test_that("the outcome's sample size is not the covariate's denominator", {
  # A published table summarizes each covariate over whatever was observed for
  # it, which is not in general the outcome's analysis set. A covariate with
  # two observed values reports p = 0.5 and a sample SD of 0.7071; judging it
  # against an outcome count of 10 gives a ceiling of 0.5270 and refuses it.
  # Fewer rows make the true bound LOOSER, so the outcome count can only ever
  # be too tight, which is the false-rejection direction.
  x <- c(0, 1)
  expect_equal(round(stats::sd(x), 4), 0.7071)
  expect_true(accepts(0.5, round(stats::sd(x), 4), 10))
  expect_true(accepts(0.5, round(stats::sd(x), 4), 500))

  # and the same row must get the same verdict from both entry points, which
  # it did not while only one of them had a count to tighten with
  expect_false(accepts(0.5, 0.85, 10))
  expect_false(accepts(0.5, 0.85, NA))
})
