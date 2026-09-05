# Two different claims were being made with one number. `.profile_rank()`
# counts directions whose spread reaches a practical threshold; that is a
# statement about how far a design MOVES. Whether the likelihood separates the
# parameters is a statement about whether those directions exist at all, and
# only the numerical rank answers it.
#
# The counterexample: profiles at -0.01 and +0.01 with an IPD SD of 1 have
# spread 0.01, below the 0.05 screen, but numerical rank 2. With aggregate
# standard errors of 1e-6 the slope is pinned to about 7e-5. Telling that user
# their parameters are "not separated by the likelihood" is false.

test_that("tiny spread is not the same as rank deficiency", {
  tiny <- matrix(c(-0.01, 0.01), ncol = 1)
  expect_equal(.profile_rank(tiny, 1), 1L)          # the spread screen fires
  expect_equal(.profile_numeric_rank(tiny, 1), 2L)  # the direction exists

  # And the direction really is estimable, and precisely, when the reported
  # standard errors are small. Precision is not a property of the profiles.
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
  # The decision the warning branches on. Where these two agree there is no
  # ambiguity; where they disagree the design is identified but weakly
  # informed, and that is the only case whose wording had to change.
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

# `.target_in_span()` decides whether the index-population estimand is a linear
# combination of the aggregate profiles, and its verdict is printed as an
# identification statement. It compared the whole residual vector against one
# tolerance taken from the largest coordinate of the target, so a single large
# covariate could excuse a complete failure on another coordinate.

test_that("a large covariate cannot excuse a failure on another coordinate", {
  # One profile at 1e10, target at 2e10. The span of a single row is its
  # multiples, and (1, 2e10) is not a multiple of (1, 1e10): the least-squares
  # residual is 1, which is the entire intercept. The old tolerance was
  # 1e-8 * 2e10 = 200, so this was reported as spanned.
  expect_false(.target_in_span(matrix(1e10, nrow = 1, ncol = 1), 2e10, 1))
  # The same profile with a target it really does span still passes.
  expect_true(.target_in_span(matrix(1e10, nrow = 1, ncol = 1), 1e10, 1))
})

test_that("ordinary in-span and out-of-span cases are unchanged", {
  expect_true(.target_in_span(matrix(c(0, 1, 2), ncol = 1), 1, 1))
  expect_true(.target_in_span(matrix(c(0, 2), ncol = 1), 1, 1))
  # Away from the origin: two profiles still span the point between them.
  expect_true(.target_in_span(matrix(c(10, 12), ncol = 1), 11, 1))
  # A single profile spans only itself.
  expect_false(.target_in_span(matrix(0, nrow = 1, ncol = 1), 5, 1))
})
