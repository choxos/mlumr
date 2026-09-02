# Survival model fitting tests. Require Stan compilation; slow.

test_that("mlumr fits a Weibull PH survival SPFA model", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026)
  fit <- fit_survival_test(dat, model = "spfa", distribution = "weibull")

  expect_s3_class(fit, "mlumr_fit")
  expect_equal(fit$family, "survival")
  expect_equal(fit$distribution, "weibull")
  expect_true(all(c("delta_index", "delta_comparator", "delta_conditional") %in%
                    names(fit$draws)))
  expect_true(all(fit$summary$Rhat < 1.1, na.rm = TRUE),
              info = sprintf("max Rhat = %.3f", max(fit$summary$Rhat, na.rm = TRUE)))
  expect_equal(fit$diagnostics$n_divergent, 0)
})

test_that("survival SPFA recovers the known log hazard ratio at its stated time", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026, loghr = -0.5, n_ipd = 250, n_agd = 300)
  # `aux_by = "none"` is load-bearing, not tidiness. Under the default
  # `".study"` a Weibull fit has n_strata = 2, and Stan then overwrites
  # `delta_*` with `loghr_*[1]`, the marginal log HR at the first prediction
  # time. That is a different estimand from the t -> 0 limit this test compares
  # against, and it would only pass because the first grid time is early.
  fit <- fit_survival_test(dat, distribution = "weibull", aux_by = "none",
                           iter = 800, warmup = 400)

  # `delta_comparator` is the marginal log hazard ratio at the time the fit
  # states, which for shared baseline shapes is the t -> 0 limit. There the risk
  # sets are still the whole populations, so it coincides with the conditional
  # log hazard ratio the data were simulated under. The marginal log HR later in
  # follow-up does NOT equal `loghr` (see helper-survival.R), and no test should
  # expect it to.
  s <- fit$summary[fit$summary$variable == "delta_comparator", ]
  # Name the cause if the variable is absent: a zero-row `s` makes the quantile
  # lookups zero-length and `&&` then errors about its argument length instead.
  expect_equal(nrow(s), 1L)
  expect_true(s[["2.5%"]] <= -0.5 && s[["97.5%"]] >= -0.5,
              info = sprintf("delta_comparator 95%% CI = [%.2f, %.2f]",
                             s[["2.5%"]], s[["97.5%"]]))
})

test_that("survival relaxed model and exponential/gengamma run", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026)
  fit_rel <- fit_survival_test(dat, model = "relaxed", distribution = "weibull")
  expect_s3_class(fit_rel, "mlumr_fit")
  expect_true("delta_beta[1]" %in% names(fit_rel$draws))

  fit_exp <- fit_survival_test(dat, distribution = "exponential")
  expect_false("aux_raw[1]" %in% names(fit_exp$draws))  # no shape parameter

  fit_gg <- fit_survival_test(dat, distribution = "gengamma", iter = 400, warmup = 200)
  expect_s3_class(fit_gg, "mlumr_fit")
  # generalized-gamma marginal hazard / log-HR generated quantities must be
  # finite (regression test for the inf - inf cancellation in mean_haz()).
  expect_true(all(is.finite(predict(fit_gg, type = "hazard")$mean)))
  expect_true(all(is.finite(predict(fit_gg, type = "loghr")$mean)))
})

test_that("M-spline and piecewise-exponential survival models fit", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("splines2")

  dat <- sim_survival_data(seed = 2026)
  fit_ms <- fit_survival_test(dat, distribution = "mspline", n_knots = 4)
  expect_s3_class(fit_ms, "mlumr_fit")
  expect_true(any(grepl("^scoef\\[", names(fit_ms$draws))))
  # sigma_smooth is one SD per baseline stratum. The default (`aux_by = ".study"`)
  # gives one stratum per study, so this is a vector; it stays a length-1 vector
  # under `aux_by = "none"`.
  expect_true(any(grepl("^sigma_smooth(\\[1\\])?$", names(fit_ms$draws))))

  fit_pe <- fit_survival_test(dat, distribution = "pexp", n_knots = 4)
  expect_s3_class(fit_pe, "mlumr_fit")
})

test_that("survival LOO consumes the pointwise log-likelihood", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("loo")

  dat <- sim_survival_data(seed = 2026)
  fit <- fit_survival_test(dat, distribution = "weibull")
  l <- calculate_loo(fit)
  expect_true(is.finite(l$estimates["elpd_loo", "Estimate"]))
})

