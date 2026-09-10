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
  # A zero row on the positive rows' own profile is pinned outright.
  d <- .normal_stub(c(1, 1, 0, 0), c(0, 0, 0, 1))
  expect_silent(mlumr:::.check_normal_residual_variation(d, "log"))
  # Beyond two free directions the check does not attempt the question.
  wide <- data.frame(.outcome = c(1, 1, 0), x1 = c(0, 0, 1), x2 = c(0, 0, 1),
                     x3 = c(0, 0, 1))
  d <- list(ipd = list(data = wide), covariates = c("x1", "x2", "x3"))
  expect_error(mlumr:::.check_normal_residual_variation(d, "log"),
               "does not attempt")
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
