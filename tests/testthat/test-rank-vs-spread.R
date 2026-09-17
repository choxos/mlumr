# `.profile_rank()` counts directions whose spread reaches a practical
# threshold, a statement about how far a design moves. Whether the likelihood
# separates the parameters is a statement about whether those directions exist
# at all, which only the numerical rank answers.

test_that("tiny spread is not the same as rank deficiency", {
  tiny <- matrix(c(-0.01, 0.01), ncol = 1)
  expect_equal(.profile_rank(tiny, 1), 1L)          # the spread screen fires
  expect_equal(.profile_numeric_rank(tiny, 1), 2L)  # the direction exists
  # The direction is estimable, and precisely, when the reported standard
  # errors are small. Precision is not a property of the profiles.
  X <- cbind(1, as.numeric(tiny))
  slope_sd <- sqrt((solve(t(X) %*% X) * 1e-6^2)[2, 2])
  expect_lt(slope_sd, 1e-3)
})

test_that("genuine collinearity is rank deficient on both measures", {
  collinear <- matrix(rep(c(1, 2), each = 3), ncol = 2)
  expect_equal(.profile_rank(collinear, c(1, 1)), 1L)
  expect_equal(.profile_numeric_rank(collinear, c(1, 1)), 1L)
})

test_that("a well spread design is full rank on both measures", {
  good <- matrix(c(0, 1, 2), ncol = 1)
  expect_equal(.profile_rank(good, 1), 2L)
  expect_equal(.profile_numeric_rank(good, 1), 2L)
})

test_that("numeric rank fails closed on unusable profiles", {
  expect_equal(.profile_numeric_rank(matrix(c(NA_real_, 1), ncol = 1), 1), 0L)
})

test_that("the two ranks disagree exactly where the claim must change", {
  # Where the two agree there is no ambiguity; where they disagree the design
  # is identified but weakly informed, and the warning wording changes.
  cases <- list(
    tiny      = list(m = matrix(c(-0.01, 0.01), ncol = 1), sd = 1),
    collinear = list(m = matrix(rep(c(1, 2), each = 3), ncol = 2), sd = c(1, 1)),
    good      = list(m = matrix(c(0, 1, 2), ncol = 1), sd = 1)
  )
  disagree <- vapply(cases, function(x) {
    .profile_rank(x$m, x$sd) != .profile_numeric_rank(x$m, x$sd)
  }, logical(1))
  expect_equal(unname(disagree), c(TRUE, FALSE, FALSE))
})

# `.realized_matches_declared()` compares where each realized row sits with
# where its declared row sits, so a grid shifted bodily, or attached to the
# wrong rows, is not a match.

test_that("a shifted realized grid does not match the declared means", {
  one <- matrix(0, nrow = 1, ncol = 1)
  expect_false(.realized_matches_declared(one, matrix(1, 1, 1), 1))
  expect_true(.realized_matches_declared(one, one, 1))
  # A shift well inside the threshold is still a match.
  expect_true(.realized_matches_declared(one, matrix(0.01, 1, 1), 1))
})

test_that("location is checked independently of spread", {
  d <- matrix(c(0, 1, 2), ncol = 1)
  expect_true(.realized_matches_declared(d, d, 1))
  # Identical spread, moved one reference SD.
  expect_false(.realized_matches_declared(d, d + 1, 1))
})

test_that("integration noise is not mistaken for a misread distr()", {
  # A finite grid misses its declared mean by about 0.05 reference SD (32 QMC
  # points against a normal margin); a `distr()` that ignores its row misses
  # by a whole SD or more. The threshold sits between the two.
  declared <- cbind(x = c(0.5, 0.5, 0.5, 0.5), z = c(-0.2, -0.2, -0.2, -0.2))
  ref_sd <- c(1, 1)

  noisy <- declared
  noisy[, "x"] <- noisy[, "x"] - 0.0515
  noisy[, "z"] <- noisy[, "z"] - 0.0247
  expect_true(mlumr:::.realized_matches_declared(declared, noisy, ref_sd))

  # A hard-coded standard normal against declared means of 0.5 and -0.2.
  ignored <- declared
  ignored[, "x"] <- 0
  ignored[, "z"] <- 0
  expect_false(mlumr:::.realized_matches_declared(declared, ignored, ref_sd))

  gap <- function(d) {
    m <- declared
    m[, "x"] <- m[, "x"] + d
    mlumr:::.realized_matches_declared(declared, m, ref_sd)
  }
  expect_true(gap(0.24))
  expect_false(gap(0.26))
})

test_that("the location check compares each row with its own declared profile", {
  # Column means and the centered spectrum are blind to row order, so a grid
  # whose rows were attached to the wrong aggregate rows must fail here.
  declared <- rbind(c(0, 1), c(1, 0))
  ref_sd <- c(1, 1)
  expect_true(mlumr:::.realized_matches_declared(declared, declared, ref_sd))
  expect_false(mlumr:::.realized_matches_declared(declared, declared[2:1, ],
                                                  ref_sd))
  expect_false(mlumr:::.realized_matches_declared(declared, declared + 0.5,
                                                  ref_sd))
})
