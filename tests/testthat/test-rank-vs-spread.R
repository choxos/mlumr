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

# `.realized_matches_declared()` centered both matrices before comparing them,
# which is blind to WHERE each design sits. A grid shifted bodily away from the
# declared means therefore matched, and with a single row centering sends both
# to zero whatever the means were. The declared profiles were then used for the
# printed identification statement while the likelihood integrated elsewhere.

test_that("a shifted realized grid does not match the declared means", {
  one <- matrix(0, nrow = 1, ncol = 1)
  expect_false(.realized_matches_declared(one, matrix(1, 1, 1), 1))
  expect_true(.realized_matches_declared(one, one, 1))
  # A shift well inside the screening threshold is still a match.
  expect_true(.realized_matches_declared(one, matrix(0.01, 1, 1), 1))
})

test_that("location is checked independently of spread", {
  d <- matrix(c(0, 1, 2), ncol = 1)
  # Identical spread AND location.
  expect_true(.realized_matches_declared(d, d, 1))
  # Identical spread, moved one reference SD: caught only by the location test.
  expect_false(.realized_matches_declared(d, d + 1, 1))
})

test_that("integration noise is not mistaken for a misread distr()", {
  # The location test has to sit between two distances that are only a factor
  # of ten apart, so the threshold is the whole content of it.
  #
  # BELOW: a finite integration grid does not land on its own declared mean. 32
  # QMC points against a normal margin miss by about 0.05 reference SD, which
  # was the spread floor this check first borrowed. Reusing that floor called a
  # perfectly specified design a mismatch, and the cost was not a spurious
  # warning: on a mismatch the caller reports the REALIZED geometry instead of
  # the declared one, and four identical declared profiles then came back as a
  # grid of numerically distinct ones. The rank-deficiency warning that design
  # exists to trigger disappeared.
  #
  # ABOVE: a `distr()` that ignores the columns it should read misses by the
  # full distance between the declared mean and whatever constant was written
  # in its place, which is a whole SD or more.
  declared <- cbind(x = c(0.5, 0.5, 0.5, 0.5), z = c(-0.2, -0.2, -0.2, -0.2))
  ref_sd <- c(1, 1)

  # Integration error, in the direction and of the size actually observed.
  noisy <- declared
  noisy[, "x"] <- noisy[, "x"] - 0.0515
  noisy[, "z"] <- noisy[, "z"] - 0.0247
  expect_true(mlumr:::.realized_matches_declared(declared, noisy, ref_sd))

  # A hard-coded standard normal against declared means of 0.5 and -0.2.
  ignored <- declared
  ignored[, "x"] <- 0
  ignored[, "z"] <- 0
  expect_false(mlumr:::.realized_matches_declared(declared, ignored, ref_sd))

  # The threshold is a parameter, so the boundary is testable rather than
  # implied: just inside passes, just outside does not.
  gap <- function(d) {
    m <- declared
    m[, "x"] <- m[, "x"] + d
    mlumr:::.realized_matches_declared(declared, m, ref_sd)
  }
  expect_true(gap(0.24))
  expect_false(gap(0.26))

  # A threshold that is not a number is still rejected by name, on this path
  # too. A bare comparison against NA aborted from inside an `if` with "missing
  # value where TRUE/FALSE needed", which names nothing.
  expect_error(
    mlumr:::.realized_matches_declared(declared, declared, ref_sd,
                                       max_location_gap = NA),
    "Spread threshold values must not be")
})
