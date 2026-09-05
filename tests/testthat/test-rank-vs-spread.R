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

test_that("a common offset far from the origin does not hide the span", {
  # Two profiles two SD apart span every one-covariate target, wherever they
  # sit. Solved from the origin, the intercept column and a covariate column
  # at 1e6 made the QR call the pair rank one, and the target between them
  # was reported outside the span. Measured from the target, the offset is
  # gone and the same pair spans it at any magnitude.
  for (offset in c(1e3, 1e6, 1e8, 1e10)) {
    A <- matrix(c(offset, offset + 2), ncol = 1)
    expect_true(.target_in_span(A, offset + 1, 1))
    expect_true(.target_in_span(A, offset + 1000, 1))
  }
  # Centering does not manufacture a span that is not there.
  expect_false(.target_in_span(matrix(1e10, nrow = 1, ncol = 1), 1e10 + 1, 1))
  two <- matrix(c(1e6, 1e6, 1e6 + 2, 1e6 + 2), ncol = 2)
  expect_false(.target_in_span(two, c(1e6 + 1, 1e6 + 1.5), c(1, 1)))
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
  expect_error(mlumr:::.realized_matches_declared(declared, declared, ref_sd,
                                                  max_location_gap = NA),
               "Spread threshold values must not be")
})

test_that("the location check compares each row with its own declared profile", {
  # Everything after the location check is blind to row ORDER: permuting the
  # rows leaves the column means alone and leaves the centered spectrum alone.
  # A grid whose rows were attached to the wrong aggregate rows therefore
  # reproduced the declared design exactly, and the function said so, while
  # every distribution integrated against another row's outcome.
  declared <- rbind(c(0, 1), c(1, 0))
  ref_sd <- c(1, 1)
  expect_true(mlumr:::.realized_matches_declared(declared, declared, ref_sd))
  expect_false(mlumr:::.realized_matches_declared(declared, declared[2:1, ],
                                                  ref_sd))
  # Per-row is stricter than the means in every case, so a bodily shift is
  # still caught.
  expect_false(mlumr:::.realized_matches_declared(declared, declared + 0.5,
                                                  ref_sd))
})

test_that("the location threshold must be one non-negative number", {
  declared <- rbind(c(0, 1), c(1, 0))
  ref_sd <- c(1, 1)
  # Zero-length made the comparison zero-length, and `any()` of nothing is
  # FALSE, so a grid five SD away was accepted without a word.
  check <- function(realized, gap) {
    mlumr:::.realized_matches_declared(declared, realized, ref_sd,
                                       max_location_gap = gap)
  }
  expect_error(check(declared + 5, numeric(0)), "single non-negative number")
  expect_error(check(declared + 5, NULL), "single non-negative number")
  # Negative rejected identical designs.
  expect_error(check(declared, -1), "single non-negative number")
  expect_true(mlumr:::.realized_matches_declared(declared, declared, ref_sd,
                                                 max_location_gap = 0))
})

# `check_identification()` reports two things about the index-population
# estimand, on the rows the likelihood integrates over. `target_in_span` is the
# exact statement: is the target in the affine span of the fitted rows. It is
# judged on the realized rows because a grid a fifth of an SD from its declared
# mean still passes `.realized_matches_declared()`, and the declared row would
# then certify a target the fitted row's span does not contain. It says nothing
# about precision, and no tolerance is folded into it: a target 0.09 SD from a
# single row is off that row's span however small 0.09 is. `target_span_gap`
# carries the practical complement, the distance from the target to the span
# of the directions the rows spread along by at least the screen's floor.

test_that("the span gap ignores directions below the spread floor", {
  g <- .target_span_gap
  # One row: the whole design, and the gap is the distance to it.
  expect_equal(g(matrix(0.24, 1, 1), 0, 1), 0.24)
  expect_equal(g(matrix(0.05, 1, 1), 0, 1), 0.05)
  # Four rows declared identical to the target, realized with grid noise: the
  # noise is below the floor, so the gap is the distance to their center.
  set.seed(2026)
  noisy <- matrix(stats::rnorm(4, 0, 0.03), 4, 1)
  expect_lt(g(noisy, 0, 1), 0.1)
  expect_gt(g(noisy + 0.5, 0, 1), 0.4)
  # Exact arithmetic on those same rows says every target is in their span,
  # which is true of the noise and useless about precision: that is why the
  # gap is reported beside the verdict rather than replacing it.
  expect_true(.target_in_span(noisy + 0.5, 0, 1))
  # Rows that move in one covariate only: a target displaced along that
  # direction has no gap, one displaced along the other has the full one.
  two <- matrix(c(-1, 1, 0, 0), 2, 2)
  expect_equal(g(two, c(0.3, 0), c(1, 1)), 0)
  expect_equal(g(two, c(0, 0.5), c(1, 1)), 0.5)
  # A span, not a hull: a well-spread design reaches a target far outside it.
  expect_equal(g(matrix(c(-1, 1), 2, 1), 5, 1), 0)
  expect_true(is.na(g(matrix(NA_real_, 1, 1), 0, 1)))
})

test_that("check_identification() judges the span on the fitted rows", {
  set.seed(2026)
  ipd <- data.frame(x1 = stats::rnorm(200))
  m <- mean(ipd$x1)
  s <- stats::sd(ipd$x1)
  mk <- function(realized) {
    out <- list(family = "normal", covariates = "x1", n_covariates = 1L,
                ipd = list(data = ipd),
                agd = list(data = data.frame(x1_mean = m)),
                integration_points = array(realized, dim = c(1L, 8L, 1L)))
    class(out) <- "mlumr_data"
    out
  }
  # Declared at the IPD mean, so the DECLARED row certifies the target; the
  # realized grid sits 0.24 SD away, inside the location tolerance, and it is
  # the realized row the likelihood integrates over. Not in its span, and the
  # gap says by how much.
  shifted <- suppressWarnings(check_identification(mk(m + 0.24 * s),
                                                   verbose = FALSE))
  expect_true(shifted$flagged)
  expect_false(shifted$target_in_span)
  expect_equal(shifted$target_span_gap, 0.24, tolerance = 1e-6)
  printed <- capture.output(suppressWarnings(check_identification(mk(m + 0.24 * s))))
  expect_true(any(grepl("not in the row space", printed)))
  expect_true(any(grepl("0.24 IPD SD", printed)))
  expect_false(any(grepl("nonetheless identified", printed)))
  # A grid within integration error of the target is still, exactly, off the
  # span, and the report says so and says the gap is what a grid produces.
  close <- suppressWarnings(check_identification(mk(m + 0.04 * s),
                                                 verbose = FALSE))
  expect_false(close$target_in_span)
  expect_equal(close$target_span_gap, 0.04, tolerance = 1e-6)
  expect_true(close$target_in_declared_span)
  printed <- capture.output(suppressWarnings(check_identification(mk(m + 0.04 * s))))
  expect_true(any(grepl("integration grid missing its declared means", printed)))
  # The same small gap when the DECLARED row is itself off the target is the
  # populations' difference, and no integration resolution changes it.
  mk_off <- function() {
    out <- mk(m + 0.04 * s)
    out$agd$data$x1_mean <- m + 0.04 * s
    out
  }
  off <- suppressWarnings(check_identification(mk_off(), verbose = FALSE))
  expect_false(off$target_in_span)
  expect_false(off$target_in_declared_span)
  expect_equal(off$target_span_gap, 0.04, tolerance = 1e-6)
  printed <- capture.output(suppressWarnings(check_identification(mk_off())))
  expect_true(any(grepl("not integration error", printed)))
  expect_false(any(grepl("larger `n_int`", printed)))
  # A grid exactly at the target is in the span, and the report says so
  # without calling the coefficients prior-driven.
  exact <- suppressWarnings(check_identification(mk(m), verbose = FALSE))
  expect_true(exact$target_in_span)
  expect_equal(exact$target_span_gap, 0)
  printed <- capture.output(suppressWarnings(check_identification(mk(m))))
  expect_true(any(grepl("nonetheless identified", printed)))
  expect_false(any(grepl("remain prior-driven", printed)))
})
