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
