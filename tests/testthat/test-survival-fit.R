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
  fit <- fit_survival_test(dat, distribution = "weibull", iter = 800, warmup = 400)

  # `delta_comparator` is the marginal log hazard ratio at the time the fit
  # states, which for shared baseline shapes is the t -> 0 limit. There the risk
  # sets are still the whole populations, so it coincides with the conditional
  # log hazard ratio the data were simulated under. The marginal log HR later in
  # follow-up does NOT equal `loghr` (see helper-survival.R), and no test should
  # expect it to.
  s <- fit$summary[fit$summary$variable == "delta_comparator", ]
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
  # The gengamma likelihood leans on Stan's regularized incomplete gamma, whose
  # gradient can fail to converge; a chain can terminate abnormally and both
  # backends then assemble the fit from the survivors. mlumr cannot prevent
  # that, but it must never hide it: the chain counts have to be recorded so
  # check_diagnostics() and summary() can report an incomplete posterior.
  expect_true(is.numeric(fit_gg$diagnostics$n_chains_requested))
  expect_true(is.numeric(fit_gg$diagnostics$n_chains_returned))
  expect_equal(fit_gg$diagnostics$n_chains_requested, 2)
  expect_equal(fit_gg$diagnostics$n_chains_returned,
               length(unique(fit_gg$chain_ids)))
  expect_lte(fit_gg$diagnostics$n_chains_returned,
             fit_gg$diagnostics$n_chains_requested)
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
