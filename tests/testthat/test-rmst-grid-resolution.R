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

test_that("a minority of badly resolved draws is not averaged away", {
  # Two draws in five collapse inside the first interval and three decay
  # later. The posterior mean curve puts 40 percent of its decay in the first
  # interval, under the threshold, while 40 percent of the RMST draws are
  # integrated from a single straight line. The share is per draw and the
  # criterion is the fraction of draws, so this is reported.
  times <- seq(0, 10, length.out = 100)
  fast <- matrix(exp(-100 * times), nrow = 1)
  slow <- matrix(exp(-0.3 * times), nrow = 1)
  s <- rbind(fast, fast, slow, slow, slow)
  shares <- .rmst_first_interval_share(s)
  expect_equal(sum(shares > 0.5), 2L)
  expect_lt(.rmst_first_interval_share(matrix(colMeans(s), nrow = 1)), 0.5)
  expect_warning(.rmst_from_surv_matrix(s, times), "In 40% of posterior draws")
  # One draw in a hundred is left alone.
  many <- rbind(fast, slow[rep(1, 99), , drop = FALSE])
  expect_silent(.rmst_from_surv_matrix(many, times))
})

test_that("a flat curve does not warn", {
  # No decay to apportion.
  flat <- matrix(1, nrow = 1, ncol = 5)
  expect_silent(.rmst_from_surv_matrix(flat, seq(0, 1, length.out = 5)))
})

test_that("a two-point grid warns whenever the curve decays at all", {
  # The first interval IS the whole horizon, so all of the decay lands before
  # the second point: this is the coarsest grid there is, not one with too
  # little interior to judge. `mlumr()` accepts `n_rmst_grid = 2`, and left
  # alone it integrates exponential curves with rates 100 and 200 over a
  # horizon of 10 to the same RMST of 5.
  expect_equal(.rmst_first_interval_share(matrix(c(1, 0.5), nrow = 1)), 1)
  expect_warning(.rmst_from_surv_matrix(matrix(c(1, 0.5), nrow = 1), c(0, 1)),
                 "In 100% of posterior draws")
  tau <- 10
  two <- c(0, tau)
  fast <- exp(-100 * two)
  faster <- exp(-200 * two)
  r_fast <- suppressWarnings(.rmst_from_surv_matrix(matrix(fast, nrow = 1), two))
  r_faster <- suppressWarnings(.rmst_from_surv_matrix(matrix(faster, nrow = 1),
                                                      two))
  expect_equal(unname(r_fast), tau / 2)
  expect_equal(unname(r_faster), tau / 2)
})

# The `rmst_*` draws are integrated in Stan on the same grid by the same rule,
# and the built-in populations read them straight from the draws. A synthetic
# exponential fit stands in for Stan here: one covariate, index rate 100 at the
# covariate mean, so nearly all of the decay lands before the second of 100
# grid points across a horizon of 10.
synthetic_survival_fit <- function(log_rate, n_grid = 100L, tau = 10) {
  set.seed(2026)
  n_draws <- 40L
  ipd <- data.frame(age = rnorm(30, 50, 5))
  x_int <- array(rnorm(2 * 16, 55, 5), dim = c(2L, 16L, 1L))
  center <- mean(ipd$age)
  structure(list(
    family = "survival",
    model = "spfa",
    data = list(covariates = "age", ipd = list(data = ipd),
                index_treatment = "A", comparator_treatment = "B"),
    draws = data.frame(mu_index = rep(log_rate, n_draws),
                       mu_comparator = rep(log_rate + log(2), n_draws),
                       `beta[1]` = rep(0.01, n_draws),
                       check.names = FALSE),
    surv_info = list(kind = "parametric", dist_code = 1L,
                     distribution = "exponential"),
    stan_data = list(rmst_grid_times = seq(0, tau, length.out = n_grid),
                     X_int = x_int - center, cov_center = center)
  ), class = "mlumr_fit")
}

test_that("the fit's own populations get the same check as a target", {
  coarse <- synthetic_survival_fit(log(100))
  expect_warning(.warn_coarse_rmst_grid_builtin(coarse),
                 "before the second point of the RMST grid")
  # The same fit with a hazard the grid resolves is silent.
  fine <- synthetic_survival_fit(log(0.2))
  expect_silent(.warn_coarse_rmst_grid_builtin(fine))
})

test_that("a fit without the pieces the check needs is left alone", {
  # Older fits, or fits whose stored data cannot be evaluated, must not turn a
  # working prediction into an error over a diagnostic.
  fit <- synthetic_survival_fit(log(100))
  fit$stan_data$X_int <- NULL
  expect_silent(.warn_coarse_rmst_grid_builtin(fit))
  fit <- synthetic_survival_fit(log(100))
  fit$draws$mu_index <- NULL
  expect_silent(.warn_coarse_rmst_grid_builtin(fit))
})

# The median has the same blind spot as the RMST integral, one interval earlier:
# when the curve is already at or below 0.5 at the first fitted prediction
# time, the median is interpolated from S(0) = 1 across that interval.

test_that("a median read off the first grid interval is reported per draw", {
  times <- c(0.2, 0.4, 0.6)
  early <- c(exp(-100 * times))              # median 0.0069, reported as 0.1
  late <- c(0.9, 0.7, 0.4)                   # median inside the grid
  expect_equal(.surv_median_from_draws(matrix(early, nrow = 1), times), 0.1,
               tolerance = 1e-6)
  expect_equal(.median_early_share(matrix(early, nrow = 1)), 1)
  expect_equal(.median_early_share(matrix(late, nrow = 1)), 0)
  mixed <- rbind(early, early, late, late, late)
  expect_equal(.median_early_share(mixed), 0.4)
  expect_warning(.warn_early_median(list(.median_early_share(mixed))),
                 "In 40% of posterior draws")
  expect_silent(.warn_early_median(list(.median_early_share(matrix(late, 1)))))
  # One draw in a hundred is left alone.
  many <- rbind(early, matrix(late, nrow = 99, ncol = 3, byrow = TRUE))
  expect_equal(.median_early_share(many), 0.01)
  expect_silent(.warn_early_median(list(.median_early_share(many))))
})
