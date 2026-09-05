# The trapezoid rule is accurate only where the curve is resolved. When events
# happen far earlier than the restriction time, nearly all of the decay falls
# inside the FIRST interval, and a straight line from S(0) = 1 to S(t_1) is a
# poor stand-in for a steep exponential. Both arms are overstated in almost the
# same way, so a difference or ratio can lose the whole effect rather than
# merely blur it, and nothing about the number looks wrong.

surv_rows <- function(rate, tau, n) {
  matrix(exp(-rate * seq(0, tau, length.out = n)), nrow = 1)
}

test_that("a grid too coarse for the hazard is reported", {
  times <- seq(0, 10, length.out = 100)
  expect_warning(
    .rmst_from_surv_matrix(surv_rows(100, 10, 100), times),
    "before the second point of the RMST grid"
  )
})

test_that("an adequately resolved curve is not reported", {
  times <- seq(0, 10, length.out = 100)
  expect_silent(.rmst_from_surv_matrix(surv_rows(1, 10, 100), times))
  # A slow curve over a short horizon is also fine.
  t2 <- seq(0, 1, length.out = 50)
  expect_silent(.rmst_from_surv_matrix(surv_rows(0.5, 1, 50), t2))
})

test_that("the numbers behind the warning are what it claims", {
  # The reason this matters: the ratio, not just the level, is destroyed.
  rmst <- function(rate, n) {
    t <- seq(0, 10, length.out = n)
    s <- exp(-rate * t)
    sum(diff(t) * (utils::head(s, -1) + utils::tail(s, -1)) / 2)
  }
  coarse <- rmst(100, 100) / rmst(200, 100)
  fine <- rmst(100, 100000) / rmst(200, 100000)
  expect_lt(abs(coarse - 1), 0.01)   # the effect has vanished
  expect_lt(abs(fine - 2), 0.01)     # and returns with resolution
})

test_that("degenerate inputs do not warn", {
  # A flat curve has no decay to apportion, and a two-point grid has no
  # interior to compare against.
  flat <- matrix(1, nrow = 1, ncol = 5)
  expect_silent(.rmst_from_surv_matrix(flat, seq(0, 1, length.out = 5)))
  expect_silent(.rmst_from_surv_matrix(matrix(c(1, 0.5), nrow = 1), c(0, 1)))
})
