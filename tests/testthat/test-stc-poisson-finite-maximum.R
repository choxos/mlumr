# A Poisson STC whose likelihood has no finite maximum. The log-likelihood is
# sum(y eta - E exp(eta)); along a direction of the coefficients that leaves
# every positive-count row's rate fixed and lowers a zero-count row's, it
# rises without bound, and iterative reweighting stops anyway when the
# deviance stops changing. What comes back is where it stopped.

make_poisson <- function(y, x, binary = FALSE, target_mean = NULL) {
  source <- data.frame(trt = "A", y = y, x = x, E = 1)
  ip <- set_ipd(source, treatment = "trt", outcome = "y", covariates = "x",
                family = "poisson", exposure = "E")
  ad <- data.frame(trt = "B", r = 20, E = 100,
                   x_mean = if (is.null(target_mean)) mean(x) else target_mean,
                   x_sd = stats::sd(x))
  ag <- set_agd(ad, treatment = "trt", family = "poisson", outcome_r = "r",
                outcome_E = "E", cov_means = "x_mean",
                cov_sds = if (binary) NA_character_ else "x_sd",
                cov_types = if (binary) "binary" else "continuous")
  d <- combine_data(ip, ag)
  suppressWarnings(if (binary) {
    add_integration(d, n_int = 64, x = distr(qbern, prob = x_mean),
                    verbose = FALSE)
  } else {
    add_integration(d, n_int = 64, x = distr(stats::qnorm, mean = x_mean,
                                             sd = x_sd), verbose = FALSE)
  })
}

test_that("an index arm with no events is refused, not reported", {
  d <- make_poisson(rep(0L, 80), rep(c(-1, 1), each = 40))
  # The GLM itself stops with finite numbers and reports convergence, which
  # is the whole problem.
  fit <- suppressWarnings(glm(.outcome ~ x + offset(log(.exposure)),
                              family = poisson(), data = d$ipd$data))
  expect_true(fit$converged)
  expect_true(all(is.finite(coef(fit))))
  expect_error(suppressWarnings(stc(d)), "has no events: the Poisson")
})

test_that("a subgroup with no events beside events elsewhere is refused", {
  # Zero counts at x = 0 and positive counts at x = 1: the direction
  # (b0 - t, b1 + t) leaves the x = 1 rates fixed and lowers the x = 0 ones,
  # so the likelihood rises without bound. The target population sits at
  # the affected profile, and the transcribed iteration stops at a slope
  # near 21 with a standard error in the thousands.
  d <- make_poisson(c(rep(0L, 40), rep(1:4, 10)), rep(c(0, 1), each = 40),
                    binary = TRUE, target_mean = 0)
  expect_error(suppressWarnings(stc(d)), "no finite maximum")
  expect_error(suppressWarnings(stc(d)), "subgroup with no events")
})

test_that("ordinary Poisson data pass and the result says the maximum is finite", {
  set.seed(2026)
  x <- rep(c(-1, 0, 1), 20)
  y <- rpois(60, exp(0.2 + 0.5 * x))
  skip_if(any(tapply(y, x, sum) == 0))
  d <- make_poisson(y, x)
  s <- suppressWarnings(stc(d))
  expect_s3_class(s, "mlumr_stc")
  expect_identical(s$separation$status, "not_applicable")
  expect_match(s$separation$reason, "maximum likelihood estimate is finite")
  # An interior maximum's standard error, not the tens of thousands a
  # receding fit reported from where it stopped.
  expect_lt(s$se, 2)
  # Zero counts at the SAME profiles as positive counts, three profiles
  # each carrying both: the positive rows span the design, so no direction
  # moves a zero row while holding them.
  y2 <- y
  y2[seq(1, 60, by = 2)] <- 0L
  skip_if(any(tapply(y2, x, sum) == 0))
  expect_s3_class(suppressWarnings(stc(make_poisson(y2, x))), "mlumr_stc")
})

test_that("a pinned zero profile does not hide a receding one", {
  # Positive counts at x = 0 pin the zeros at x = 0: every direction that
  # holds the positive rates holds those too. The zeros at x = 1 still
  # recede along (b0 - t, b1 + t). The strict reading, which the normal
  # guard needs, calls the boundary unreachable as soon as a row is
  # pinned; the Poisson question drops the pinned row and refuses.
  y <- c(rep(1:2, 10), rep(0L, 40))
  x <- c(rep(0, 20), rep(0, 20), rep(1, 20))
  d <- make_poisson(y, x, binary = TRUE, target_mean = 0.5)
  expect_error(suppressWarnings(stc(d)), "no finite maximum")
})

test_that("the weak boundary question drops pinned rows and needs no strictness", {
  # Positive rows all at x = 0, zero rows at x = 0 and x = 1. The zero row
  # at x = 0 is pinned. For the normal log link that blocks the ray, since
  # every zero row must be lowered; for the Poisson likelihood the row
  # simply drops out and the x = 1 row still recedes.
  X_pos <- cbind(1, c(0, 0))
  X_zero <- cbind(1, c(0, 1))
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero), "unreachable")
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero, strict = FALSE),
                   "reachable")
  # Zero rows on opposite sides recede in opposite directions under either
  # reading.
  X_zero <- cbind(1, c(-1, 1))
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero, strict = FALSE),
                   "unreachable")
  # Positive rows spanning the design leave no free direction.
  X_pos <- cbind(1, c(0, 1))
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero, strict = FALSE),
                   "unreachable")
  # Two free directions with a gap of exactly pi: positives at the origin,
  # zeros at (1, 0), (-1, 0) and (0, 1). The direction (0, 0, -1) holds
  # the first two zero rows and lowers the third, so the Poisson
  # likelihood recedes, while no direction lowers all three. The two rows
  # bounding the gap are exactly opposite, which is decided exactly.
  X_pos <- cbind(1, c(0, 0), c(0, 0))
  X_zero <- cbind(1, c(1, -1, 0), c(0, 0, 1))
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero, strict = FALSE),
                   "reachable")
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero), "unreachable")
  # Rows all on one line through the origin load one direction, not two,
  # and opposite signs on it settle the question either way.
  X_zero <- cbind(1, c(1, -1, 2), c(0, 0, 0))
  expect_identical(mlumr:::.zero_boundary(X_pos, X_zero, strict = FALSE),
                   "unreachable")
})

test_that("the exactly opposite case is refused through stc() as receding", {
  # Events at the origin, zero counts at (1, 0), (-1, 0) and (0, 1) on two
  # binary-coded covariates: the third coordinate recedes while the first
  # two are held, and the public path must call that a missing maximum,
  # not an undecided question.
  ipd <- data.frame(trt = "A", y = c(rep(1:2, 5), rep(0L, 30)),
                    x1 = c(rep(0, 10), rep(1, 10), rep(-1, 10), rep(0, 10)),
                    x2 = c(rep(0, 10), rep(0, 10), rep(0, 10), rep(1, 10)),
                    E = 1)
  ip <- set_ipd(ipd, treatment = "trt", outcome = "y",
                covariates = c("x1", "x2"), family = "poisson",
                exposure = "E")
  ag <- set_agd(data.frame(trt = "B", r = 20, E = 100, x1_mean = 0,
                           x1_sd = 1, x2_mean = 0.5, x2_sd = 0.5),
                treatment = "trt", family = "poisson", outcome_r = "r",
                outcome_E = "E", cov_means = c("x1_mean", "x2_mean"),
                cov_sds = c("x1_sd", "x2_sd"),
                cov_types = c("continuous", "continuous"))
  d <- suppressWarnings(add_integration(
    combine_data(ip, ag), n_int = 64,
    x1 = distr(stats::qnorm, mean = x1_mean, sd = x1_sd),
    x2 = distr(stats::qnorm, mean = x2_mean, sd = x2_sd), verbose = FALSE
  ))
  expect_error(suppressWarnings(stc(d)), "no finite maximum")
})
