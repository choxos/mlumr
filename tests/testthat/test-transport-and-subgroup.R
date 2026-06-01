# Tests for ML-UMR paper-compliance features:
#  - arbitrary target-population transport via newdata (g-computation)
#  - subgroup-AgD identification of the relaxed model's beta_comparator
# Require Stan; skipped on CRAN.

test_that("transport to newdata = index covariates reproduces the index population", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(2026)
  n <- 200
  ipd <- data.frame(trt = "A", y = stats::rbinom(n, 1, 0.4),
                    age = stats::rnorm(n), bmi = stats::rnorm(n))
  agd <- data.frame(trt = "B", n_total = 300, n_events = 130,
                    age_mean = 0.3, age_sd = 1, bmi_mean = 0.2, bmi_sd = 1)
  dat <- combine_data(
    set_ipd(ipd, "trt", "y", c("age", "bmi")),
    set_agd(agd, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = c("age_mean", "bmi_mean"), cov_sds = c("age_sd", "bmi_sd"))
  )
  dat <- add_integration(dat, n_int = 32, verbose = FALSE,
                         age = distr(qnorm, mean = age_mean, sd = age_sd),
                         bmi = distr(qnorm, mean = bmi_mean, sd = bmi_sd))
  fit <- mlumr(dat, chains = 2, iter = 600, warmup = 300, seed = 2026, refresh = 0)

  ipd_cov <- ipd[, c("age", "bmi")]
  me_idx <- marginal_effects(fit, population = "index")
  me_tgt <- suppressMessages(marginal_effects(fit, newdata = ipd_cov))
  # g-computation over the IPD covariates == the Stan index generated quantities.
  for (eff in c("LOR", "RD", "RR")) {
    expect_equal(me_tgt$mean[me_tgt$effect == eff],
                 me_idx$mean[me_idx$effect == eff], tolerance = 1e-6,
                 info = eff)
  }
  expect_true(all(me_tgt$population == "Target"))

  pr_idx <- predict(fit, population = "index")
  pr_tgt <- predict(fit, newdata = ipd_cov)
  expect_equal(pr_tgt$mean, pr_idx$mean, tolerance = 1e-6)

  # A genuinely different target shifts the standardized effect.
  shifted <- data.frame(age = ipd$age + 3, bmi = ipd$bmi)
  me_shift <- suppressMessages(marginal_effects(fit, newdata = shifted, effect = "rd"))
  expect_false(isTRUE(all.equal(me_shift$mean, me_idx$mean[me_idx$effect == "RD"])))
})

test_that("survival transport (RMST) to newdata = index covariates matches index", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026, n_ipd = 120, n_agd = 120, n_int = 32)
  fit <- fit_survival_test(dat, distribution = "weibull")
  ipd_cov <- dat$ipd$data[, dat$covariates]

  ri <- predict(fit, population = "index", type = "rmst")
  rt <- predict(fit, newdata = ipd_cov, type = "rmst")
  m <- merge(ri[, c("treatment", "mean")], rt[, c("treatment", "mean")],
             by = "treatment", suffixes = c(".idx", ".tgt"))
  expect_equal(m$mean.tgt, m$mean.idx, tolerance = 1e-6)

  # RMST differences and time-specific marginal HRs both standardize to the
  # target covariate distribution. The latter is population-specific because
  # hazard ratios are non-collapsible.
  rd_idx <- marginal_effects(fit, population = "index", effect = "rmstd")
  rd_tgt <- marginal_effects(fit, newdata = ipd_cov, effect = "rmstd")
  expect_equal(rd_tgt$mean[1], rd_idx$mean[1], tolerance = 1e-6)
  hr_idx <- suppressMessages(
    marginal_effects(fit, population = "index", effect = "hr")
  )
  hr_tgt <- suppressMessages(
    marginal_effects(fit, newdata = ipd_cov, effect = "hr")
  )
  expect_equal(hr_tgt$mean[1], hr_idx$mean[1], tolerance = 1e-6)
  expect_equal(hr_tgt$at_time[1], hr_idx$at_time[1])
})

test_that("relaxed model: joint subgroup AgD identifies beta_comparator from data", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  # Comparator population split into 3 joint subgroups with a strong covariate
  # gradient in the event rate (logit increasing in age): this identifies
  # beta_comparator[age] from the subgroup variation (L_AgD = prod_s L_{AgD,s}).
  set.seed(2026)
  n <- 200
  ipd <- data.frame(trt = "A", y = stats::rbinom(n, 1, 0.4), age = stats::rnorm(n))
  sub <- data.frame(
    trt = "B",
    n_total = c(100, 100, 100),
    n_events = c(20, 40, 65),                # rising event rate across subgroups
    age_mean = c(-1.0, 0.0, 1.0),            # rising covariate mean
    age_sd = c(0.5, 0.5, 0.5)
  )
  dat <- combine_data(
    set_ipd(ipd, "trt", "y", "age"),
    set_agd(sub, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "age_mean", cov_sds = "age_sd")
  )
  dat <- add_integration(dat, n_int = 32, verbose = FALSE,
                         age = distr(qnorm, mean = age_mean, sd = age_sd))

  fit <- mlumr(dat, model = "relaxed", chains = 2, iter = 800, warmup = 400,
               seed = 2026, refresh = 0)
  bc <- fit$draws[["beta_comparator[1]"]]
  expect_false(is.null(bc))
  # Data (subgroups) inform beta_comparator: posterior is tighter than the
  # default prior (sd 2.5) and recovers the positive age gradient.
  expect_lt(stats::sd(bc), 2.5)
  expect_gt(mean(bc), 0)
})
