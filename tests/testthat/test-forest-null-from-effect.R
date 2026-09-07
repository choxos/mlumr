# The null belongs to the measure, not to the axis. Choosing it from `log_x`
# put a reference line at 0 on a risk ratio or hazard ratio drawn on a linear
# axis, which is not a value those measures can take.

skip_if_not_installed("ggplot2")

ref_of <- function(p) {
  vl <- Filter(function(l) inherits(l$geom, "GeomVline"), p$layers)
  expect_gt(length(vl), 0)
  unique(unlist(lapply(vl, function(l) {
    v <- l$data$xintercept
    if (is.null(v)) v <- l$aes_params$xintercept
    v
  })))
}

frame <- function(effect = NULL) {
  d <- data.frame(label = c("A", "B"), est = c(1.2, 0.9),
                  lo = c(0.8, 0.6), hi = c(1.8, 1.3))
  if (!is.null(effect)) d$effect <- effect
  d
}

test_that("a ratio measure gets null 1 on either axis", {
  for (e in c("HR", "TR", "RR", "RMSTR")) {
    expect_equal(ref_of(mlumr_forest(frame(e))), 1,
                 info = paste(e, "on a linear axis"))
    expect_equal(ref_of(mlumr_forest(frame(e), log_x = TRUE)), 1,
                 info = paste(e, "on a log axis"))
  }
})

test_that("a difference measure gets null 0", {
  for (e in c("RMSTD", "MD", "LOR", "LOG_HR")) {
    expect_equal(ref_of(mlumr_forest(frame(e))), 0, info = e)
  }
})

test_that("without an effect column the axis is still the only hint", {
  expect_equal(ref_of(mlumr_forest(frame())), 0)
  expect_equal(ref_of(mlumr_forest(frame(), log_x = TRUE)), 1)
})

test_that("an explicit ref_line still wins, and a log axis still rejects 0", {
  expect_equal(ref_of(mlumr_forest(frame("HR"), ref_line = 2)), 2)
  expect_equal(ref_of(mlumr_forest(frame(), ref_line = 0.5)), 0.5)
  expect_error(mlumr_forest(frame("LOR"), log_x = TRUE),
               "must be positive when `log_x = TRUE`")
})

test_that("an unrecognized label falls back to the axis, not to zero", {
  # `mlumr_forest()` accepts any data frame, so `effect` can be a label the
  # package never produces. Treating it as a difference gave "OR" a null of 0
  # and, on a log axis, refused the plot outright, where reading the axis had
  # been right.
  expect_equal(ref_of(mlumr_forest(frame("OR"), log_x = TRUE)), 1)
  expect_equal(ref_of(mlumr_forest(frame("OR"))), 0)
  expect_equal(ref_of(mlumr_forest(frame("something else"), log_x = TRUE)), 1)

  # a recognized difference on a log axis is still refused, because that is a
  # real contradiction rather than an unknown
  expect_error(mlumr_forest(frame("LOR"), log_x = TRUE),
               "must be positive when `log_x = TRUE`")

  expect_true(mlumr:::.known_measure("hr"))
  expect_true(mlumr:::.known_measure("RMSTD"))
  expect_false(mlumr:::.known_measure("OR"))
  expect_false(mlumr:::.known_measure(NA))
})
