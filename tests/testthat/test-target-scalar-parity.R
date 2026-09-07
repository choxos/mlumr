# The scalar effect a survival fit reports must not depend on WHICH route the
# caller took. Adding `newdata` changes the population an effect is
# standardized to, not the set of effects that exist. These exercise the pieces
# directly rather than through a Stan fit, so they run everywhere and fail for
# the reason they name.

fake_aft_fit <- function(relaxed, mu_i, mu_c, beta_i, beta_c,
                         cov_center = c(0, 0)) {
  covs <- c("x1", "x2")
  draws <- data.frame(mu_index = mu_i, mu_comparator = mu_c)
  if (relaxed) {
    draws[["beta_index[1]"]] <- beta_i[, 1]
    draws[["beta_index[2]"]] <- beta_i[, 2]
    draws[["beta_comparator[1]"]] <- beta_c[, 1]
    draws[["beta_comparator[2]"]] <- beta_c[, 2]
  } else {
    draws[["beta[1]"]] <- beta_i[, 1]
    draws[["beta[2]"]] <- beta_i[, 2]
  }
  structure(
    list(model = if (relaxed) "relaxed" else "spfa",
         draws = draws,
         data = list(covariates = covs),
         stan_data = list(cov_center = cov_center),
         surv_info = list(is_ph = FALSE, distribution = "weibull-aft",
                          n_aux = 1L)),
    class = "mlumr_fit")
}

test_that(".target_delta_eta matches the closed-form location contrast", {
  set.seed(2026)
  n <- 50L
  mu_i <- rnorm(n); mu_c <- rnorm(n)
  bi <- cbind(rnorm(n), rnorm(n))
  bc <- cbind(rnorm(n), rnorm(n))
  fit <- fake_aft_fit(TRUE, mu_i, mu_c, bi, bc)

  nd <- data.frame(x1 = c(-1, 0.5, 2), x2 = c(0.25, 1, -3))
  got <- .target_delta_eta(fit, nd)

  # mean_m eta(x_m) is linear, so the contrast is the mean profile plugged in.
  xbar <- colMeans(as.matrix(nd))
  want <- (mu_i - mu_c) + as.numeric((bi - bc) %*% xbar)

  expect_equal(got, want, tolerance = 1e-12)
  expect_length(got, n)
})

test_that("centering is applied to the target rows", {
  set.seed(2026)
  n <- 20L
  bi <- cbind(rnorm(n), rnorm(n))
  bc <- cbind(rnorm(n), rnorm(n))
  ctr <- c(0.3, -0.7)
  fit <- fake_aft_fit(TRUE, rnorm(n), rnorm(n), bi, bc, cov_center = ctr)
  nd <- data.frame(x1 = c(1, 2), x2 = c(-1, 4))

  xbar <- colMeans(sweep(as.matrix(nd), 2, ctr))
  want <- (fit$draws$mu_index - fit$draws$mu_comparator) +
    as.numeric((bi - bc) %*% xbar)
  expect_equal(.target_delta_eta(fit, nd), want, tolerance = 1e-12)
})

test_that("a shared-coefficient AFT contrast is population invariant", {
  # This is the property that makes a shared-shape SPFA time ratio apply to
  # every target: the covariate term cancels draw by draw, so two completely
  # different targets give the identical answer.
  set.seed(2026)
  n <- 30L
  b <- cbind(rnorm(n), rnorm(n))
  fit <- fake_aft_fit(FALSE, rnorm(n), rnorm(n), b, b)

  a <- .target_delta_eta(fit, data.frame(x1 = c(0, 1), x2 = c(2, -2)))
  bb <- .target_delta_eta(fit, data.frame(x1 = c(-50, 100, 7), x2 = c(3, 3, 3)))

  expect_equal(a, bb, tolerance = 1e-12)
  expect_equal(a, fit$draws$mu_index - fit$draws$mu_comparator,
               tolerance = 1e-12)
})

test_that("a relaxed AFT contrast is NOT population invariant", {
  # The mirror of the test above: if this ever stops depending on the target,
  # the covariate term has been dropped and the transport is silently a no-op.
  set.seed(2026)
  n <- 30L
  fit <- fake_aft_fit(TRUE, rnorm(n), rnorm(n),
                      cbind(rnorm(n), rnorm(n)), cbind(rnorm(n), rnorm(n)))
  a <- .target_delta_eta(fit, data.frame(x1 = 0, x2 = 0))
  bb <- .target_delta_eta(fit, data.frame(x1 = 5, x2 = -5))
  expect_false(isTRUE(all.equal(a, bb)))
})

test_that("both routes resolve the same scalar name for the same fit", {
  # `.surv_scalar_effect_name(.surv_scalar_label(fit))` is now the single
  # source of the selector on the built-in and target routes alike, so the
  # mapping itself is what has to hold.
  expect_identical(.surv_scalar_effect_name("HR"), "hr")
  expect_identical(.surv_scalar_effect_name("TR"), "tr")
  expect_identical(.surv_scalar_effect_name("EXP_DELTA_ETA"), "exp_delta_eta")
  expect_error(.surv_scalar_effect_name("RMSTD"), "Unrecognized")

  shared <- fake_aft_fit(FALSE, 0, 0, cbind(0, 0), cbind(0, 0))
  relaxed <- fake_aft_fit(TRUE, 0, 0, cbind(0, 0), cbind(0, 0))
  expect_identical(.surv_scalar_label(shared)$label, "TR")
  expect_identical(.surv_scalar_label(relaxed)$label, "EXP_DELTA_ETA")
})
