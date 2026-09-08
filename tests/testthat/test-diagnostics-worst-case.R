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

  expect_identical(mlumr:::.usable_diagnostic_values("not numeric")$n_total, 0L)
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
