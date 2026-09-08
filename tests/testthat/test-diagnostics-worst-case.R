# A worst-case statistic that filters out the worst case is not one. An Rhat of
# Inf is a parameter whose chains did not mix at all; dropping it left a column
# holding 1.001 and Inf reporting a maximum of 1.001, with no warning. A
# divergence count the backend never supplied is likewise not a count of zero,
# and 0 is the reassuring one of the two.

test_that("an infinite Rhat survives into the reported maximum", {
  d <- mlumr:::.usable_diagnostic_values(c(1.001, Inf))
  expect_equal(max(d$values), Inf)
  expect_identical(d$n_missing, 0L)
  # the pre-existing helper is what discarded it, and still does; the point is
  # that the diagnostic paths no longer use it for a worst case
  expect_equal(max(mlumr:::.finite_numeric_values(c(1.001, Inf))), 1.001)
})

test_that("a missing Rhat is counted, not silently excluded", {
  d <- mlumr:::.usable_diagnostic_values(c(1.001, NA, NaN, 1.02))
  expect_equal(d$values, c(1.001, 1.02))
  expect_identical(d$n_missing, 2L)
  expect_identical(d$n_total, 4L)
  expect_match(mlumr:::.missing_suffix(d), "2 of 4 parameters; 2 unavailable")

  none <- mlumr:::.usable_diagnostic_values(c(1.0, 1.01))
  expect_identical(mlumr:::.missing_suffix(none), "")

  all_missing <- mlumr:::.usable_diagnostic_values(c(NA_real_, NaN))
  expect_length(all_missing$values, 0L)
  expect_identical(all_missing$n_missing, 2L)

  # This used to assert a total of 0, which is what let a malformed or absent
  # column disappear from the accounting entirely. A column that is not numeric
  # holds no usable diagnostics, and that is one missing diagnostic per
  # parameter, not zero parameters.
  not_numeric <- mlumr:::.usable_diagnostic_values("not numeric")
  expect_identical(not_numeric$n_total, 1L)
  expect_identical(not_numeric$n_missing, 1L)
  expect_length(not_numeric$values, 0L)
})

test_that("an unknown transition count stays unknown", {
  expect_identical(mlumr:::.transition_count(7), 7L)
  expect_identical(mlumr:::.transition_count(0), 0L)
  expect_identical(mlumr:::.transition_count(NULL), NA_integer_)
  expect_identical(mlumr:::.transition_count(NA), NA_integer_)
  expect_identical(mlumr:::.transition_count(Inf), NA_integer_)
  expect_identical(mlumr:::.transition_count(-1), NA_integer_)
  # the old reader mapped every one of those to zero, which reads as "clean"
  expect_identical(mlumr:::.diagnostic_count(NULL), 0)
  expect_identical(mlumr:::.diagnostic_count(NA), 0)

  expect_identical(mlumr:::.diagnostic_display(NA_integer_), "unknown")
  expect_identical(mlumr:::.diagnostic_display(0L), "0")
})

test_that("Inf is formatted as Inf rather than as a number", {
  expect_identical(mlumr:::.format_diagnostic(Inf), "Inf")
  expect_identical(mlumr:::.format_diagnostic(NaN), "NaN")
  expect_match(mlumr:::.format_diagnostic(1.0012345), "^1\\.001")
})


test_that("a column that is absent or not numeric is missing, not empty", {
  # `c(NA, NA)` is LOGICAL in R, which is what a backend writes into a column
  # it never filled. Reading that as zero diagnostics made the reporter silent
  # about the one case it exists for.
  d <- .usable_diagnostic_values(c(NA, NA), 2L)
  expect_equal(d$n_missing, 2L)
  expect_equal(d$n_total, 2L)
  expect_length(d$values, 0L)

  # An absent column, with the count the summary says it should have had.
  d <- .usable_diagnostic_values(NULL, 5L)
  expect_equal(d$n_missing, 5L)
  expect_equal(d$n_total, 5L)

  # A malformed column is reported, not ignored.
  d <- .usable_diagnostic_values(c("a", "b", "c"), 3L)
  expect_equal(d$n_missing, 3L)
  expect_equal(d$n_total, 3L)

  # A numeric column is unchanged.
  d <- .usable_diagnostic_values(c(NA_real_, 1, 2))
  expect_equal(d$values, c(1, 2))
  expect_equal(d$n_missing, 1L)
  expect_equal(d$n_total, 3L)
})

test_that("an absent column is still reported as unavailable", {
  d <- .usable_diagnostic_values(NULL, 3L)
  expect_message(
    .report_missing_diagnostics(d, "Rhat", "convergence"),
    "unavailable for 3 of 3"
  )
})

test_that("the missing-diagnostic message names outcomes, not a cause", {
  d <- .usable_diagnostic_values(c(1.0, NA, NA, 1.01), 4L)
  msg <- testthat::capture_messages(
    .report_missing_diagnostics(d, "Rhat", "convergence",
                                c("mu_a", "mu_b", "beta[1]", "sigma"))
  )
  # Which parameters were not checked is something it knows.
  expect_match(msg, "mu_b", fixed = TRUE, all = FALSE)
  expect_match(msg, "beta[1]", fixed = TRUE, all = FALSE)
  # Why the value is missing is not. A constant generated quantity is one
  # reason among several and was being reported as "the usual" one.
  expect_false(any(grepl("usual reason", msg, fixed = TRUE)))
  expect_match(msg, "chains are stuck", fixed = TRUE, all = FALSE)
})

test_that("a fractional transition count is unknown, not zero", {
  # as.integer() truncates, and 0 is precisely the value that says the sampler
  # behaved, so the coercion turned an invalid count into a clean bill.
  expect_true(is.na(.transition_count(0.5)))
  expect_true(is.na(.transition_count(2^31)))
  expect_equal(.transition_count(3), 3L)
  expect_equal(.transition_count(0), 0L)
  expect_true(is.na(.transition_count(-1)))
})
