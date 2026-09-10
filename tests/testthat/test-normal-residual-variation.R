# A normal fit whose covariates reproduce the outcome exactly has an improper
# posterior for the residual SD: marginalizing the coefficients leaves a density
# behaving as sigma^(rank - n) near zero, which does not integrate. Nothing in
# the sampler reports that. It drifts toward zero and returns where it stopped.

.normal_stub <- function(y, x) {
  list(ipd = list(data = data.frame(.outcome = y, x = x)),
       covariates = "x")
}

test_that("an exactly fitting design is refused", {
  d <- .normal_stub(c(0, 0, 1, 1), c(-0.5, -0.5, 0.5, 0.5))
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("the verdict does not depend on the units of the outcome", {
  y <- c(0, 0, 1, 1)
  x <- c(-0.5, -0.5, 0.5, 0.5)
  for (scale in c(1e-6, 1, 1e6)) {
    scaled <- .normal_stub(y * scale, x)
    expect_error(mlumr:::.check_normal_residual_variation(scaled), "improper")
  }
  # An ordinary fit stays acceptable at every scale too.
  set.seed(2026)
  yy <- rnorm(50)
  xx <- rnorm(50)
  for (scale in c(1e-6, 1, 1e6)) {
    scaled <- .normal_stub(yy * scale, xx)
    expect_silent(mlumr:::.check_normal_residual_variation(scaled))
  }
})

test_that("a constant outcome and a saturated design are named for what they are", {
  flat <- .normal_stub(rep(2, 5), c(1, 2, 3, 4, 5))
  expect_error(mlumr:::.check_normal_residual_variation(flat), "constant")
  expect_error(mlumr:::.check_normal_residual_variation(flat), "improper")
  # Two rows, intercept plus one covariate: rank equals n. The posterior is
  # proper, but the data do not separate the residual SD from the
  # coefficients, so it is warned about as prior-sensitive. It is not "the
  # prior": with proper coefficient priors the marginal likelihood in sigma is
  # bounded at zero and falls as sigma^(-n), which is not flat.
  saturated <- .normal_stub(c(1, 3), c(0, 1))
  expect_warning(mlumr:::.check_normal_residual_variation(saturated),
                 "no residual degrees of freedom")
  expect_warning(mlumr:::.check_normal_residual_variation(saturated),
                 "sensitive to the coefficient priors")
  expect_error(mlumr:::.check_normal_residual_variation(saturated), NA)
  # A saturated CONSTANT outcome is still saturated, so it is the proper,
  # prior-sensitive case and not the improper constant one. The rank check has
  # to come first for that to hold.
  flat_saturated <- .normal_stub(c(2, 2), c(0, 1))
  expect_warning(mlumr:::.check_normal_residual_variation(flat_saturated),
                 "no residual degrees of freedom")
  expect_error(mlumr:::.check_normal_residual_variation(flat_saturated), NA)
})

test_that("a nearly exact fit warns instead of refusing", {
  set.seed(2026)
  x <- rnorm(60)
  y <- 2 + 3 * x + rnorm(60, sd = 1e-5)
  ratio <- sum(residuals(lm(y ~ x))^2) / sum((y - mean(y))^2)
  # The fixture has to land between the two thresholds for the test to mean
  # anything.
  expect_gt(ratio, 60 * .Machine$double.eps^2)
  expect_lt(ratio, 1e-6)
  expect_warning(mlumr:::.check_normal_residual_variation(.normal_stub(y, x)),
                 "concentrate near zero")
})

test_that("ordinary data passes silently", {
  set.seed(2026)
  x <- rnorm(100)
  y <- 1 + 0.5 * x + rnorm(100)
  expect_silent(mlumr:::.check_normal_residual_variation(.normal_stub(y, x)))
})

test_that("mlumr() refuses before it reaches the engine", {
  # No Stan model is compiled and no engine is chosen: the refusal happens in
  # validation, which is what makes it cheap and unambiguous. The backend is
  # replaced by a spy that fails loudly, so a refusal that came AFTER dispatch
  # would show as the spy's error rather than the guard's.
  local_mocked_bindings(
    .mlumr_fit_backend = function(...) stop("engine reached"),
    .package = "mlumr"
  )
  make_data <- function(y, link = "identity") {
    ipd <- set_ipd(data.frame(trt = "A", y = y, x = c(-0.5, -0.5, 0.5, 0.5)),
                   "trt", "y", "x", family = "normal")
    agd <- set_agd(data.frame(trt = "B", n_total = 100, y_mean = 0.4,
                              y_se = 0.1, x_mean = 0.1, x_sd = 0.5),
                   "trt", family = "normal", outcome_n = "n_total",
                   outcome_mean = "y_mean", outcome_se = "y_se",
                   cov_means = "x_mean", cov_sds = "x_sd")
    suppressWarnings(
      add_integration(combine_data(ipd, agd), n_int = 32,
                      x = distr(qnorm, mean = x_mean, sd = x_sd))
    )
  }
  expect_error(suppressWarnings(mlumr(make_data(c(0, 0, 1, 1)),
                                      family = "normal")),
               "improper")
  # The log-link case whose response-scale fit overflows: the verdict comes
  # from log(y) and still stops the dispatch.
  expect_error(suppressWarnings(mlumr(make_data(exp(400 * c(-1, -1, 1, 1))),
                                      family = "normal", link = "log")),
               "improper")
  # And ordinary data reaches the spy, which is what proves the spy is live.
  set.seed(2026)
  expect_error(suppressWarnings(mlumr(make_data(rnorm(4)), family = "normal")),
               "engine reached")
})

test_that("a log link is tested where an exact log-link fit would show", {
  # y = exp(eta) exactly. The residual sum of squares in y is not zero and the
  # identity-scale test sees nothing, but the posterior is improper all the
  # same, so the test goes on log(y).
  # Three or more distinct covariate values, so the exponential curve is not
  # also a straight line through the points and the two scales disagree.
  x <- c(-1, 0, 1, 2)
  y <- exp(0.5 + x)
  d <- .normal_stub(y, x)
  expect_silent(mlumr:::.check_normal_residual_variation(d, "identity"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"), "improper")

  # A non-positive observation cannot be matched by a positive mean, so no
  # exact fit exists and the posterior is proper. The observation itself is
  # valid under a log-link normal, which constrains the mean and not the data.
  nonpositive <- .normal_stub(c(-1, 0, 1, 2), x)
  expect_silent(mlumr:::.check_normal_residual_variation(nonpositive, "log"))
})

test_that("the log link is measured on the scale its likelihood uses", {
  # normal(exp(theta), sigma) puts the residual on the RESPONSE scale. Judging
  # it by the OLS residual of log(y) measures relative error instead: here the
  # log-scale residual is ordinary, while the response-scale fit reproduces
  # every large observation exactly and leaves almost nothing behind.
  x <- seq(-20, 20, length.out = 25)
  y <- exp(x)
  y[which.min(y)] <- exp(-10)

  log_ratio <- sum(residuals(lm(log(y) ~ x))^2) / sum((log(y) - mean(log(y)))^2)
  expect_gt(log_ratio, 1e-6)

  d <- list(ipd = list(data = data.frame(.outcome = y, x = x)),
            covariates = "x")
  # The moved point is genuinely off the curve, so this residual is real, not
  # rounding: it is 8.95e-27 where the same design fitted to points exactly on
  # the curve leaves 3.06e-29. The posterior is proper and this warns.
  expect_warning(mlumr:::.check_normal_residual_variation(d, "log"),
                 "concentrate near zero")

  # Its companion, and the pair is what pins the log branch: identical design,
  # every point on the curve, so the fit is exact and the posterior is not.
  on_curve <- list(ipd = list(data = data.frame(.outcome = exp(x), x = x)),
                   covariates = "x")
  expect_error(mlumr:::.check_normal_residual_variation(on_curve, "log"),
               "improper")
})

test_that("extreme units neither underflow nor overflow the test", {
  # Both sums are squares. Unscaled, 1e-170 underflows them to zero (a varying
  # outcome read as constant) and 1e160 overflows them to Inf (ratio NaN, and
  # the comparison that follows errors rather than deciding).
  set.seed(2026)
  x <- rnorm(40)
  y <- 1 + 0.5 * x + rnorm(40)
  for (scale in c(1e-170, 1e160)) {
    d <- .normal_stub(y * scale, x)
    expect_silent(mlumr:::.check_normal_residual_variation(d))
  }
  # And an exact fit is still caught at those units.
  xe <- c(-0.5, -0.5, 0.5, 0.5)
  ye <- c(0, 0, 1, 1)
  for (scale in c(1e-170, 1e160)) {
    d <- .normal_stub(ye * scale, xe)
    expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
  }
})

test_that("a near-collinear column is not dropped before the residual is taken", {
  # x2 differs from x1 by 1e-8 * z, and y is exactly that difference. The full
  # design reproduces y with coefficients (-1, 1); lm.fit()'s default pivot
  # tolerance discards x2 and measures a large residual against what is left.
  #
  # This case and "a small but real residual" below are the pair that fixes the
  # refusal floor. Both are ordinary data, their residual ratios are only about
  # 400 apart (3e-16 here, 1e-13 there), and they must be decided in opposite
  # directions. No fixed threshold does that. What separates them is the size
  # of the fitted coefficients: about 4e7 here, since the design reaches y only
  # by cancelling two huge terms, against 3 there. The rounding a residual can
  # carry scales with them.
  set.seed(2026)
  n <- 60
  x1 <- rnorm(n)
  z <- rnorm(n)
  x2 <- x1 + 1e-8 * z
  y <- x2 - x1
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("a small but real residual is proper, and warns rather than errors", {
  # Only an exactly zero residual is improper: for any positive one the
  # exp(-RSS / (2 sigma^2)) factor drives the density to zero as sigma does and
  # the integral converges. A ratio around 1e-13 is readily representable, so
  # refusing it would reject a valid, highly predictive dataset.
  set.seed(2026)
  n <- 50
  x <- rnorm(n)
  y <- 2 + 3 * x + rnorm(n, sd = 1e-6)
  ratio <- sum(residuals(lm(y ~ x))^2) / sum((y - mean(y))^2)
  expect_gt(ratio, n * .Machine$double.eps^2)
  expect_lt(ratio, 1e-6)

  d <- .normal_stub(y, x)
  expect_warning(mlumr:::.check_normal_residual_variation(d),
                 "concentrate near zero")
  expect_error(mlumr:::.check_normal_residual_variation(d), NA)
})

test_that("the verdict does not depend on the units of a predictor", {
  # Multiplying a predictor by 1e12 leaves the fitted mean surface and the
  # propriety question untouched. Taking kappa() on the raw design would let it
  # drive the floor to its cap and hard-reject a genuine residual.
  set.seed(2026)
  n <- 50
  x <- rnorm(n)
  y <- 2 + 3e-12 * (x * 1e12) + rnorm(n, sd = 1e-6)
  for (unit in c(1, 1e12)) {
    d <- .normal_stub(y, x * unit)
    expect_warning(mlumr:::.check_normal_residual_variation(d),
                   "concentrate near zero")
    expect_error(mlumr:::.check_normal_residual_variation(d), NA)
  }
})

test_that("a near-collinear column survives the log-link check too", {
  # glm.fit() takes its pivot tolerance as min(1e-7, epsilon / 1000), so the
  # default drops x2 and reports an ordinary residual for a design that fits
  # exactly through it.
  set.seed(2026)
  n <- 60
  x1 <- rnorm(n)
  z <- rnorm(n)
  x2 <- x1 + 1e-12 * z
  y <- exp(x2 - x1)
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"), "improper")
})

test_that("an outcome with a large offset keeps its exact fit", {
  # y is stored exactly and fitted exactly by the intercept and x. Dividing by
  # its own maximum would push the whole 147-wide spread into the last few
  # digits of a value near 1 and read it back as noise.
  x <- 0:49
  y <- 1e15 + 3 * x
  d <- .normal_stub(y, x)
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("a redundant column does not make the design look well conditioned", {
  # kappa() is singular here, and treating that as perfectly conditioned drops
  # the floor to the well-conditioned scale, where this exact fit's numerical
  # residual is large enough to pass.
  set.seed(2026)
  n <- 60
  x1 <- rnorm(n)
  z <- rnorm(n)
  x2 <- x1 + 1e-8 * z
  d <- list(
    ipd = list(data = data.frame(.outcome = x2 - x1, x1 = x1, x2 = x2,
                                 x3 = rep(1, n))),
    covariates = c("x1", "x2", "x3")
  )
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("an offset predictor does not inflate the numerical zero", {
  # Shifting a predictor does not change the fitted column space, but it does
  # force the intercept to cancel the offset. Counting that cancellation as
  # rounding pushes the floor above a genuine residual and refuses a proper
  # posterior.
  set.seed(2026)
  n <- 50
  x <- rnorm(n)
  y <- 2 + 3 * x + rnorm(n, sd = 1e-6)
  for (shift in c(0, 1e12)) {
    d <- .normal_stub(y, x + shift)
    expect_warning(mlumr:::.check_normal_residual_variation(d),
                   "concentrate near zero")
    expect_error(mlumr:::.check_normal_residual_variation(d), NA)
  }
})

test_that("an outcome spanning both extremes does not overflow the shift", {
  # y - min(y) is Inf here, and a non-finite response aborts the fit in a
  # low-level error rather than a diagnosis. Four distinct profiles, so no
  # structural rule decides it first and the numeric branch has to take the
  # shift itself.
  x <- c(-1.5, -0.5, 0.5, 1.5)
  y <- 6e307 * x
  expect_identical(max(y) - min(y), Inf)
  d <- .normal_stub(y, x)
  expect_error(mlumr:::.check_normal_residual_variation(d), "within rounding")
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("a wide log-link outcome is decided on log(y), where nothing overflows", {
  # Past about 700 log units of span the response-scale fit overflows
  # internally whatever the normalization, and there used to be no verdict at
  # all: the check warned that it could not run and the model went on to
  # sample an improper posterior. Whether y = exp(X b) has an exact solution
  # is whether log(y) lies in the column space of X, a linear question that
  # cannot overflow, and here it does: two profiles, two coefficients.
  x <- c(-1, -1, 1, 1)
  y <- exp(400 * x)
  expect_identical(min(y) / max(y), 0)
  d <- .normal_stub(y, x)
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"), "improper")
  expect_warning(
    tryCatch(mlumr:::.check_normal_residual_variation(d, "log"),
             error = function(e) NULL),
    NA
  )
})

test_that("replicate profiles with different outcomes prove the residual real", {
  # Two rows with identical covariates get identical fitted values under any
  # fit, so their outcomes differing leaves a residual no fit can remove. That
  # is a proof, and it decides the case where the rounding bound cannot: the
  # design is so ill conditioned that its coefficients run to 2^40, the bound
  # on an exact fit's rounding is about 1e-5 of the total, and the true
  # residual of 9e-13 sits far below it. Reading the bound as a converse would
  # refuse a proper posterior.
  csmall <- 2^-40
  enoise <- 2^-20
  x1 <- rep(c(-3, -1, 1, 3), each = 2)
  z <- rep(c(-1, 1, -1, 1), each = 2)
  x2 <- x1 + csmall * z
  y <- 2 + z + rep(c(-enoise, enoise), 4)
  # The residual no fit can remove: each pair's outcomes straddle their mean
  # by enoise, which is about 9e-13 of the total sum of squares.
  within <- sum((y - ave(y, x1, z))^2) / sum((y - mean(y))^2)
  expect_gt(within, 0)
  expect_lt(within, 1e-6)
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  expect_warning(mlumr:::.check_normal_residual_variation(d),
                 "concentrate near zero")
  expect_error(mlumr:::.check_normal_residual_variation(d), NA)
  # The same holds under a log link, where the replicate rows disagree just
  # the same.
  dl <- list(ipd = list(data = data.frame(.outcome = exp(y), x1 = x1,
                                          x2 = x2)),
             covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(dl, "log"), NA)
})

test_that("a residual below the rounding bound is refused as undecidable, not as proven", {
  # No replicate rows here, so nothing structural decides it, and the
  # computed residual sits inside what rounding alone can leave. The message
  # says the fit cannot be told from an exact one rather than that it is one.
  set.seed(2026)
  n <- 60
  x1 <- rnorm(n)
  z <- rnorm(n)
  x2 <- x1 + 1e-8 * z
  y <- x2 - x1
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d), "within rounding")
  expect_error(mlumr:::.check_normal_residual_variation(d), "undecidable|refused")
})

test_that("agreeing replicates on as many profiles as the rank are an exact fit", {
  # Two distinct profiles, intercept plus slope: the design reaches any pair
  # of values, and each profile's replicates agree, so the fit is exact by
  # construction and the refusal can say so.
  d <- .normal_stub(c(0.3, 0.3, 0.7, 0.7), c(-0.5, -0.5, 0.5, 0.5))
  expect_error(mlumr:::.check_normal_residual_variation(d),
               "distinct covariate profiles")
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("an identically zero outcome is refused under a log link", {
  # Zero is the one non-positive value a positive mean can approach, at the
  # boundary intercept -> -Inf, where the likelihood grows as sigma^(-n) and
  # only the intercept prior's tails decide whether a posterior exists. The
  # guard does not see the prior, so it refuses.
  x <- c(-1, 0, 1, 2)
  expect_error(mlumr:::.check_normal_residual_variation(.normal_stub(rep(0, 4), x),
                                                        "log"),
               "identically zero")
  # A negative value leaves a residual no positive mean removes.
  expect_silent(mlumr:::.check_normal_residual_variation(.normal_stub(c(-1, 0, 1, 2), x),
                                                         "log"))
})

test_that("zeros mixed with positives are refused only where the boundary is reachable", {
  # Two profiles, zeros on one and ones on the other. The positive rows are
  # fitted exactly by any coefficients with intercept + slope / 2 = 0, which
  # leaves the slope free to send the zero rows' predictor to -Inf while the
  # ones stay fitted: the boundary ray, where the likelihood grows without
  # bound and only the prior tails decide.
  d <- .normal_stub(c(0, 0, 1, 1), c(-0.5, -0.5, 0.5, 0.5))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "taking every zero row there")
  # A rank deficit alone is not a reachable boundary. Positive rows at x = 0
  # and zeros at x = -1 and x = 1: the one free direction moves the two zero
  # rows in opposite directions, so their means stay bounded away from zero
  # and the posterior is proper.
  d <- .normal_stub(c(1, 1, 0, 0), c(0, 0, -1, 1))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # Two free directions, decided by whether the zero rows' loadings share an
  # open half-plane: three zero rows spread around the positive profile do
  # not, three on one side do.
  around <- data.frame(.outcome = c(1, 1, 0, 0, 0),
                       x1 = c(0, 0, 1, -1, 0), x2 = c(0, 0, 0, -1, 1))
  d <- list(ipd = list(data = around), covariates = c("x1", "x2"))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  one_side <- data.frame(.outcome = c(1, 1, 0, 0, 0),
                         x1 = c(0, 0, 1, 1, 2), x2 = c(0, 0, 0, 1, 1))
  d <- list(ipd = list(data = one_side), covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "taking every zero row there")
  # A zero row on the positive rows' own profile is pinned outright, and a
  # negative zero is the same profile: the keys are exact in value, not in
  # spelling.
  d <- .normal_stub(c(1, 1, 0, 0), c(0, 0, 0, 1))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  d <- .normal_stub(c(1, 1, 0), c(0, 0, -0))
  expect_identical(sprintf("%a", -0), "-0x0p+0")
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # A zero row that is a combination of positive profiles, the midpoint of
  # two here, is pinned as surely as a duplicate: every direction that leaves
  # the positive predictors fixed leaves its own fixed too.
  mid <- data.frame(.outcome = c(1, 1, 0), x1 = c(-1, 1, 0), x2 = c(-1, 1, 0))
  d <- list(ipd = list(data = mid), covariates = c("x1", "x2"))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # A zero row off the span by 1e-20 of the column's size. When the model
  # centers, its design has lost that contrast: 1e-20 - 0.5 is -0.5
  # exactly, so the row is a bitwise duplicate of a positive profile in
  # the fitted design and is pinned as one. When the model fits the raw
  # design the row is free, exactly off the span, and its loadings are
  # rounding: neither pinned nor safely free, and the check declines. The
  # same at 1e-10 either way.
  near <- data.frame(.outcome = c(1, 1, 1, 1, 0),
                     x1 = c(0, 0, 1, 1, 0), x2 = c(0, 0, 1, 1, 1e-20))
  d <- list(ipd = list(data = near), covariates = c("x1", "x2"))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log",
                                                        center = FALSE),
               "could not be decided")
  near$x2[5] <- 1e-10
  d <- list(ipd = list(data = near), covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "could not be decided")
  expect_error(mlumr:::.check_normal_residual_variation(d, "log",
                                                        center = FALSE),
               "could not be decided")
  # A covariate no zero row loads on, here a constant, adds a null direction
  # without adding to the question: the opposite-sign geometry above is still
  # one-directional, not a two-direction problem with a gap of exactly pi.
  padded <- data.frame(.outcome = c(1, 1, 0, 0), x1 = c(0, 0, -1, 1),
                       x2 = c(0, 0, 0, 0))
  d <- list(ipd = list(data = padded), covariates = c("x1", "x2"))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # Two rows pointing opposite ways in a genuine two-direction problem cannot
  # both be lowered, whatever the third does. The computed angles put that
  # gap within rounding of pi, where a direction may or may not exist, and
  # the check declines rather than read the rounding either way.
  opposite <- data.frame(.outcome = c(1, 1, 0, 0, 0),
                         x1 = c(0, 0, 1, -1, 0), x2 = c(0, 0, 0, 0, 1))
  d <- list(ipd = list(data = opposite), covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "could not be decided")
  # A row that differs from the positive profile by 1e-6 of the column's
  # size is free, and with the other zero row it loads the one direction
  # with a common sign: reachable. The feasibility rows are scaled by powers
  # of two and never shifted, so no centering can round such a row onto the
  # profile; a row off it by 1e-20 is below the resolution stated on the
  # guard and reads as pinned, which is the case two blocks above.
  d <- .normal_stub(c(1, 1, 0, 0), c(0, 0, 1e-6, 1))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "taking every zero row there")
  # Beyond two effective directions the check does not attempt the question.
  # Three zero rows spanning three null directions is that case; a single
  # zero row with three free directions is not, since it loads on one.
  wide <- data.frame(.outcome = c(1, 1, 0, 0, 0), x1 = c(0, 0, 1, 0, 0),
                     x2 = c(0, 0, 0, 1, 0), x3 = c(0, 0, 0, 0, 1))
  d <- list(ipd = list(data = wide), covariates = c("x1", "x2", "x3"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "could not be decided")
  narrow <- data.frame(.outcome = c(1, 1, 0), x1 = c(0, 0, 1), x2 = c(0, 0, 1),
                       x3 = c(0, 0, 1))
  d <- list(ipd = list(data = narrow), covariates = c("x1", "x2", "x3"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "taking every zero row there")
  # Four positive rows on distinct x pin both coefficients, so the zero row's
  # mean is a fixed positive number and its square bounds the residual away
  # from zero. Proper, and silent: the positive rows do not fit exactly.
  d <- .normal_stub(c(0, 1, 2, 3, 5), 1:5)
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # Positive rows on an exact curve, and still pinned: the zero row's mean is
  # exp(eta) at its own x, fixed and positive.
  d <- .normal_stub(c(0, exp(1:4)), c(0, 1:4))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # A negative outcome anywhere settles it in exact arithmetic.
  d <- .normal_stub(c(0, 0, 1, -1), c(-0.5, -0.5, 0.5, 0.5))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # Settled as proper, it can still be near exact: positives at one profile
  # fitted exactly and a negative of 1e-6 at another, whose mean the slope
  # can drive toward zero, leave a residual near 1e-12 of the total. The
  # screen runs on negatives too and says so.
  d <- .normal_stub(c(1, 1, -1e-6), c(0, 0, 1))
  expect_warning(mlumr:::.check_normal_residual_variation(d, "log"),
                 "concentrate near zero")
})

test_that("the near-exact screen for mixed zeros sees the whole outcome", {
  # Positive replicates at 1e6 - 1 and 1e6 + 1 have an ordinary residual
  # against their own spread, and a residual of 2 against a total near 1e12
  # once the zero rows are counted: the posterior is proper and the fit is
  # near exact in the sense the screen exists to flag.
  d <- .normal_stub(c(1e6 - 1, 1e6 + 1, 0, 0), c(0, 0, 1, 1))
  expect_warning(mlumr:::.check_normal_residual_variation(d, "log"),
                 "concentrate near zero")
  # With the zero rows pinned to the positive profile their means are
  # positive numbers of the outcome's own size, and nothing is near exact.
  d <- .normal_stub(c(3, 5, 0, 0), c(0, 0, 0, 0))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # Positive outcomes spanning more than the exponent range cannot be
  # represented on one scale for the response-scale fit. The screen stands
  # aside and the structural verdict, proper, stands; it used to die on
  # log(0) after scaling the small end to zero.
  d <- .normal_stub(c(1e-200, 1e200, 0), c(0, 0, 1))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
})

test_that("a column spanning the double range does not fake a duplicate", {
  # Dividing a column that holds 1e308 by 2^1023 underflows an entry of
  # 1e-200 to zero, and the bitwise duplicate test then read a zero row at
  # 1e-200 as a positive row at 0. The duplicate test now sees the raw rows.
  # The row is still below the resolution stated on the guard relative to
  # the column, so it reads as pinned by the row-space test rather than as
  # a duplicate; this pins the reason, not the verdict.
  X_pos <- cbind(1, c(0, 0))
  X_zero <- cbind(1, c(1e-200, 1e308))
  scaled <- mlumr:::.scale_design(rbind(X_pos, X_zero))
  expect_identical(scaled[3, 2], 0)
  expect_false(any(mlumr:::.row_keys(X_zero) %in% mlumr:::.row_keys(X_pos)))
  expect_true(any(mlumr:::.row_keys(scaled[3:4, , drop = FALSE]) %in%
                    mlumr:::.row_keys(scaled[1:2, , drop = FALSE])))
})

test_that("a predictor at the largest double is scaled, not zeroed", {
  # log2() of the largest double rounds to 1024 and 2^1024 is Inf, so the
  # power-of-two scaling divided the column by Inf and every profile read as
  # the same one: two rows at +xmax and -xmax looked pinned to each other.
  xmax <- .Machine$double.xmax
  d <- .normal_stub(c(1, 1, 0, 0), c(xmax, xmax, -xmax, -xmax))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "taking every zero row there")
})

test_that("an outcome that varies below the resolution of its log is refused, not crashed", {
  # Four outcomes near 1e300 differing by units in the last place have
  # identical logarithms. The log-scale total sum of squares is then zero and
  # the ratio undefined, which used to reach `if (NA)`. It is the undecidable
  # regime, and says so.
  base <- 1e300
  up <- function(v, k) {
    for (i in seq_len(k)) v <- v + 2^(floor(log2(v)) - 52)
    v
  }
  y <- c(base, up(base, 1), base, up(base, 2))
  expect_false(all(y == y[1]))
  expect_true(all(log(y) == log(y[1])))
  d <- .normal_stub(y, 0:3)
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "resolution of its logarithm")
})

test_that("a predictor spanning both extremes does not overflow the centering", {
  # Centering on the mean overflows twice here: the column's sum, and the
  # shift of the far value from a mean near -1e308. Either sends a
  # non-finite design into qr() and the check dies in a low-level error
  # instead of deciding. The data are ordinary otherwise: a real residual on
  # a predictor with absurd units, which is proper and passes.
  set.seed(2026)
  n <- 40
  x <- c(rep(-1e308, n - 1), 1e308)
  y <- 1 + rnorm(n)
  d <- .normal_stub(y, x)
  expect_error(mlumr:::.check_normal_residual_variation(d), NA)
  # And an exact fit on such a predictor is still caught.
  x <- c(-1e308, -1e308, 1e308, 1e308)
  d <- .normal_stub(c(0, 0, 1, 1), x)
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
})

test_that("a predictor in tiny units is not dropped before the fit", {
  # The intercept is a column of ones and this predictor's whole range is
  # 2^-60, so a QR that judged columns against the largest one would discard
  # it as redundant, and an outcome reproduced exactly through it, y = 2^60 x,
  # would read as ordinary variation against the intercept alone. R's
  # dqrdc2 judges a column against its own original norm and keeps it, but
  # the design is scaled to unit columns before any factorization so the
  # verdict does not hang on that detail of one QR routine.
  x <- (0:8) * 2^-60
  d <- .normal_stub(0:8, x)
  expect_error(mlumr:::.check_normal_residual_variation(d), "improper")
  # And a real residual through such a column is still seen as small but
  # real, not as the whole outcome.
  set.seed(2026)
  y <- 2 + 2^60 * x + rnorm(9, sd = 1e-6)
  expect_warning(mlumr:::.check_normal_residual_variation(.normal_stub(y, x)),
                 "concentrate near zero")
})

test_that("an uncentered model is judged on the design it will fit", {
  # With center = TRUE the guard centers as the model will, and an offset of
  # 1e12 in a predictor costs nothing: the intercept absorbs it before the
  # fit. With center = FALSE the model fits the raw design, its linear
  # predictor carries the cancellation of two terms near 3e12, and the
  # rounding in that cancellation is larger than a residual of 1e-6. The
  # guard sees the same design the model will, and refuses it as undecidable
  # rather than promise a residual the likelihood cannot resolve.
  set.seed(2026)
  n <- 50
  x <- rnorm(n) + 1e12
  y <- 2 + 3 * (x - 1e12) + rnorm(n, sd = 1e-6)
  d <- .normal_stub(y, x)
  expect_warning(mlumr:::.check_normal_residual_variation(d),
                 "concentrate near zero")
  expect_error(mlumr:::.check_normal_residual_variation(d, center = FALSE),
               "within rounding")
})

test_that("the rank is the design's exact rank, not the one a QR resolves", {
  # x2 differs from x1 by 1e-20 on one row. A factorization at machine
  # precision drops it as redundant, but the raw design has exact rank 3:
  # one 3-by-3 minor is -1e-20, and the finite coefficients (0, -1e20, 1e20)
  # reproduce y = c(0, 1, 0, 0) exactly, with n = 4 above the rank. That is
  # an improper posterior for the model that fits the raw design, and the
  # guard used to pass it on the strength of the reduced fit. It is now
  # caught structurally: three distinct rows, agreeing replicates, rank 3.
  x1 <- c(0, 0, 1, 1)
  x2 <- c(0, 1e-20, 1, 1)
  y <- c(0, 1, 0, 0)
  X <- cbind(1, x1, x2)
  expect_identical(qr(X, tol = .Machine$double.eps)$rank, 2L)
  expect_identical(mlumr:::.exact_rank(X)$rank, 3L)
  expect_identical(as.vector(X %*% c(0, -1e20, 1e20)), y)
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  s <- mlumr:::.residual_variation_status(X, y, "identity")
  expect_identical(s$status, "exact")
  expect_error(mlumr:::.check_normal_residual_variation(d, "identity",
                                                        center = FALSE),
               "3 distinct covariate profiles for a design of rank 3")
  expect_error(mlumr:::.check_normal_residual_variation(d, "identity",
                                                        center = FALSE),
               "improper")

  # When the model centers, its design is the centered one, and there the
  # contrast is gone: 1e-20 - 0.5 is -0.5 exactly, so rows 1 and 2 are the
  # same row of the fitted design with outcomes 0 and 1. That proves the
  # residual positive for that model, and the guard says so.
  Xc <- cbind(1, x1 - 0.5, x2 - 0.5)
  expect_identical(Xc[1, ], Xc[2, ])
  expect_identical(mlumr:::.exact_rank(Xc)$rank, 2L)
  expect_identical(mlumr:::.residual_variation_status(Xc, y, "identity")$status,
                   "positive")
  expect_silent(mlumr:::.check_normal_residual_variation(d, "identity",
                                                         center = c(0.5, 0.5)))
})

test_that("a column within rounding of the others is refused as unresolved", {
  # Exactly independent columns that no factorization at machine precision
  # can separate. The outcome is not reproduced on the reduced design, and
  # whether it is on the full one cannot be decided, so the guard refuses
  # and says why rather than pass the reduced fit's residual as the model's.
  set.seed(2026)
  x1 <- rnorm(20)
  x2 <- x1
  # One element one unit in the last place away: representable, so the
  # columns are distinct numbers, and a contrast the QR cannot resolve.
  x2[1] <- x2[1] * (1 + .Machine$double.eps)
  expect_false(identical(x1, x2))
  y <- rnorm(20)
  X <- cbind(1, x1, x2)
  expect_identical(mlumr:::.exact_rank(X)$rank, 3L)
  expect_identical(qr(X, tol = .Machine$double.eps)$rank, 2L)
  s <- mlumr:::.residual_variation_status(X, y, "identity")
  expect_identical(s$status, "unresolved")
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  expect_error(mlumr:::.check_normal_residual_variation(d, center = FALSE),
               "3 exactly independent columns.*resolves only 2")
  d$ipd$data$.outcome <- exp(y)
  expect_error(mlumr:::.check_normal_residual_variation(d, "log",
                                                        center = FALSE),
               "resolves only 2")
  # An exactly dependent column is a different thing: it is not a column
  # the model can fit through, the exact rank drops with it, and the
  # ordinary verdict stands.
  X <- cbind(1, x1, 2 * x1)
  expect_identical(mlumr:::.exact_rank(X)$rank, 2L)
  expect_identical(mlumr:::.residual_variation_status(X, y, "identity")$status,
                   "positive")
})

test_that("the exact rank agrees with a QR wherever the QR is trustworthy", {
  set.seed(2026)
  for (i in 1:100) {
    n <- sample(2:12, 1)
    k <- sample(1:5, 1)
    r <- sample(1:min(n, k), 1)
    X <- matrix(sample(-5:5, n * r, TRUE), n, r) %*%
      matrix(sample(-3:3, r * k, TRUE), r, k)
    exact <- mlumr:::.exact_rank(X)
    expect_identical(exact$rank, qr(X)$rank)
    expect_identical(qr(X[, exact$pivots, drop = FALSE])$rank, exact$rank)
  }
  # Doubles are rationals, so an exact dependence survives any scaling by
  # powers of two and any sign, and a floating-point sum is not one.
  x <- rnorm(7)
  expect_identical(mlumr:::.exact_rank(cbind(1, x, -x / 8))$rank, 2L)
  expect_identical(mlumr:::.exact_rank(cbind(1, 3, x))$rank, 2L)
  expect_identical(mlumr:::.exact_rank(cbind(1, c(4.9e-324, 1e308, 0),
                                             c(2, 3, 4)))$rank, 3L)
  # Subnormal mantissas are read exactly: every subnormal is a multiple of
  # 2^-1074, so twice the smallest one is exactly dependent on it and three
  # times it beside twice is not.
  tiny <- 2^-1074
  expect_identical(mlumr:::.exact_rank(cbind(c(tiny, 2 * tiny),
                                             c(1, 2)))$rank, 1L)
  expect_identical(mlumr:::.exact_rank(cbind(c(tiny, 3 * tiny),
                                             c(1, 2)))$rank, 2L)
  wide <- c(tiny, 5e307)
  expect_identical(mlumr:::.exact_rank(cbind(wide, 2 * wide))$rank, 1L)
  # One unit in the last place off exact proportionality at the top of the
  # range is a distinct number, and the exact rank says so where a QR at
  # machine precision cannot. (Built by multiplication: whether the decimal
  # 1e308 parses to twice the decimal 5e307 differs between platforms.)
  nudged <- 2 * wide
  nudged[2] <- nudged[2] * (1 + .Machine$double.eps)
  expect_false(nudged[2] == 2 * wide[2])
  expect_identical(mlumr:::.exact_rank(cbind(wide, nudged))$rank, 2L)
  expect_identical(qr(cbind(wide, nudged))$rank, 1L)
  expect_identical(mlumr:::.exact_rank(matrix(0, 3, 2))$rank, 0L)
  expect_identical(mlumr:::.exact_rank(matrix(0, 0, 2))$rank, 0L)
})

test_that("a zero row within rounding of the positive rows' span is not pinned", {
  # The zero row at 2^-48 is sixteen machine epsilons off the span of the
  # positive rows, which are all at x = 0, and the direction (0, -1) leaves
  # those fixed while lowering both zero rows. A distance that small used to
  # read as membership of the span, and the boundary as unreachable. It is
  # now neither pinned nor safely free: the loadings that decide the rest
  # are rounding there, so the guard refuses as undecided.
  x <- c(0, 0, 2^-48, 1)
  y <- c(1, 1, 0, 0)
  X <- cbind(1, x)
  expect_identical(as.vector(X[y > 0, ] %*% c(0, -1)), c(0, 0))
  expect_true(all(X[y == 0, ] %*% c(0, -1) < 0))
  d <- .normal_stub(y, x)
  expect_error(mlumr:::.check_normal_residual_variation(d, "log",
                                                        center = FALSE),
               "could not be decided")
  expect_error(mlumr:::.check_normal_residual_variation(d, "log",
                                                        center = FALSE),
               "within rounding of the positive rows' span")
  # Well above the rounding band the same geometry is decided: reachable.
  d <- .normal_stub(y, c(0, 0, 1e-6, 1))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log",
                                                        center = FALSE),
               "taking every zero row there")
  # And a row exactly in the span is pinned however small the numbers that
  # put it there: the midpoint of two positive profiles, at machine size.
  x1 <- c(0, 2^-40, 2^-41, 2^-30)
  x2 <- c(0, 2^-40, 2^-41, 0)
  y <- c(1, 1, 0, 0)
  d <- list(ipd = list(data = data.frame(.outcome = y, x1 = x1, x2 = x2)),
            covariates = c("x1", "x2"))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log",
                                                         center = FALSE))
})

test_that("mlumr() hands the guard the centers its design will carry", {
  # The guard judges the design the model fits. With center = TRUE that is
  # the design centered on the model's own pooled means, and with
  # center = FALSE the raw one; a QR reparameterization spans the same
  # column space either way and changes nothing here.
  local_mocked_bindings(
    .check_normal_residual_variation = function(data, link, center = TRUE) {
      stop("guard saw center = ", paste(format(center), collapse = ", "))
    },
    .package = "mlumr"
  )
  make_data <- function() {
    ipd <- set_ipd(data.frame(trt = "A", y = c(1, 2, 3, 5),
                              x = c(-1.5, -0.5, 0.5, 1.5)),
                   "trt", "y", "x", family = "normal")
    agd <- set_agd(data.frame(trt = "B", n_total = 100, y_mean = 0.4,
                              y_se = 0.1, x_mean = 0.1, x_sd = 0.5),
                   "trt", family = "normal", outcome_n = "n_total",
                   outcome_mean = "y_mean", outcome_se = "y_se",
                   cov_means = "x_mean", cov_sds = "x_sd")
    suppressWarnings(
      add_integration(combine_data(ipd, agd), n_int = 32,
                      x = distr(qnorm, mean = x_mean, sd = x_sd))
    )
  }
  # Pooled center: (4 * 0 + 100 * 0.1) / 104.
  centered <- format(10 / 104)
  expect_error(suppressWarnings(mlumr(make_data(), family = "normal")),
               paste0("guard saw center = ", centered), fixed = TRUE)
  expect_error(suppressWarnings(mlumr(make_data(), family = "normal",
                                      qr = TRUE)),
               paste0("guard saw center = ", centered), fixed = TRUE)
  expect_error(suppressWarnings(mlumr(make_data(), family = "normal",
                                      center = FALSE)),
               "guard saw center = 0", fixed = TRUE)
  expect_error(suppressWarnings(mlumr(make_data(), family = "normal",
                                      center = FALSE, qr = TRUE)),
               "guard saw center = 0", fixed = TRUE)
})

test_that("the raw-design exact fit is refused through mlumr() itself", {
  # The public path with the backend replaced by a spy: the refusal must
  # come from the guard, before any engine is reached.
  local_mocked_bindings(
    .mlumr_fit_backend = function(...) stop("engine reached"),
    .package = "mlumr"
  )
  ipd <- set_ipd(data.frame(trt = "A", y = c(0, 1, 0, 0),
                            x1 = c(0, 0, 1, 1), x2 = c(0, 1e-20, 1, 1)),
                 "trt", outcome = "y", covariates = c("x1", "x2"),
                 family = "normal")
  agd <- set_agd(data.frame(trt = "B", y_mean = 0.5, y_se = 0.1,
                            x1_mean = 0, x1_sd = 1, x2_mean = 0, x2_sd = 1),
                 "trt", family = "normal", outcome_mean = "y_mean",
                 outcome_se = "y_se", cov_means = c("x1_mean", "x2_mean"),
                 cov_sds = c("x1_sd", "x2_sd"),
                 cov_types = c("continuous", "continuous"))
  d <- suppressWarnings(add_integration(
    combine_data(ipd, agd), n_int = 16, cor = diag(2), cor_adjust = "none",
    verbose = FALSE,
    x1 = distr(stats::qnorm, mean = x1_mean, sd = x1_sd),
    x2 = distr(stats::qnorm, mean = x2_mean, sd = x2_sd)
  ))
  expect_error(suppressWarnings(mlumr(d, model = "relaxed",
                                      link = "identity", center = FALSE,
                                      qr = FALSE, seed = 2026,
                                      verbose = FALSE)),
               "improper")
  # The same data with the model's centering merge rows 1 and 2, whose
  # outcomes differ, so the residual is provably positive and the fit
  # reaches the spy.
  expect_error(suppressWarnings(mlumr(d, model = "relaxed",
                                      link = "identity", center = TRUE,
                                      qr = FALSE, seed = 2026,
                                      verbose = FALSE)),
               "engine reached")
})

test_that("a boundary row within rounding of the span is refused through mlumr() too", {
  # The 2^-48 row with a heavy-tailed slope prior: along the direction
  # (0, -1) the likelihood grows without bound and a Cauchy prior does not
  # tame it, so the posterior is improper. The guard does not see the prior
  # and refuses the geometry as undecided before any backend is reached.
  local_mocked_bindings(
    .mlumr_fit_backend = function(...) stop("engine reached"),
    .package = "mlumr"
  )
  ip <- set_ipd(data.frame(trt = "A", y = c(1, 1, 0, 0),
                           x = c(0, 0, 2^-48, 1)),
                "trt", outcome = "y", covariates = "x", family = "normal")
  ag <- set_agd(data.frame(trt = "B", y_mean = 1, y_se = 0.1, x_mean = 0,
                           x_sd = 1),
                "trt", family = "normal", outcome_mean = "y_mean",
                outcome_se = "y_se", cov_means = "x_mean", cov_sds = "x_sd",
                cov_types = "continuous")
  d <- suppressWarnings(add_integration(
    combine_data(ip, ag), n_int = 16, verbose = FALSE,
    x = distr(stats::qnorm, mean = x_mean, sd = x_sd)
  ))
  expect_error(suppressWarnings(mlumr(
    d, model = "relaxed", link = "log", center = FALSE, qr = FALSE,
    prior_beta = prior_cauchy(0, 2.5),
    prior_beta_comparator = prior_normal(0, 2.5), seed = 2026,
    verbose = FALSE
  )), "could not be decided")
})
