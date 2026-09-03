# Compare every summarized quantity, not just the posterior mean: a transported
# implementation can reproduce the mean while returning a wrong SD or wrong
# quantiles, and a mean-only assertion passes anyway.
expect_summary_equal <- function(target, reference, info = NULL) {
  cols <- intersect(names(reference), names(target))
  cols <- cols[vapply(reference[cols], is.numeric, logical(1))]
  testthat::expect_gt(length(cols), 1L)
  for (cl in cols) {
    testthat::expect_equal(target[[cl]], reference[[cl]], tolerance = 1e-6,
                           info = paste(info, cl))
  }
}

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
    tg <- me_tgt[me_tgt$effect == eff, , drop = FALSE]
    ix <- me_idx[me_idx$effect == eff, , drop = FALSE]
    expect_equal(nrow(tg), 1L, info = eff)
    expect_equal(nrow(ix), 1L, info = eff)
    expect_summary_equal(tg, ix, info = eff)
  }
  expect_true(all(me_tgt$population == "Target"))

  pr_idx <- predict(fit, population = "index")
  pr_tgt <- predict(fit, newdata = ipd_cov)
  expect_equal(nrow(pr_tgt), nrow(pr_idx))
  expect_equal(pr_tgt$treatment, pr_idx$treatment)
  expect_summary_equal(pr_tgt, pr_idx, info = "response")

  # A genuinely different target shifts the standardized effect. Require the
  # shifted value to be a single finite number first: `all.equal()` returns a
  # message rather than TRUE when either side is NA or NaN, so the inequality
  # alone is also satisfied by a `newdata` path that has stopped producing
  # numbers at all.
  shifted <- data.frame(age = ipd$age + 3, bmi = ipd$bmi)
  me_shift <- suppressMessages(marginal_effects(fit, newdata = shifted, effect = "rd"))
  expect_equal(nrow(me_shift), 1L)
  expect_true(is.finite(me_shift$mean))
  expect_true(is.finite(me_shift$sd))
  rd_idx <- me_idx$mean[me_idx$effect == "RD"]
  expect_false(isTRUE(all.equal(me_shift$mean, rd_idx)))
  # The risk difference is a probability contrast, so a standardized value
  # outside [-1, 1] would be a broken calculation rather than a shifted one.
  expect_gte(me_shift$mean, -1)
  expect_lte(me_shift$mean, 1)
})

test_that("survival transport (RMST) to newdata = index covariates matches index", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026, n_ipd = 120, n_agd = 120, n_int = 32)
  fit <- fit_survival_test(dat, distribution = "weibull")
  ipd_cov <- dat$ipd$data[, dat$covariates]

  ri <- predict(fit, population = "index", type = "rmst")
  rt <- predict(fit, newdata = ipd_cov, type = "rmst")
  # merge() inner-joins, so a transported row that went missing would be
  # dropped before the comparison and the survivors would still match. Assert
  # the keys and the cardinality first.
  expect_equal(nrow(rt), nrow(ri))
  expect_setequal(rt$treatment, ri$treatment)
  ord_t <- order(rt$treatment)
  ord_i <- order(ri$treatment)
  expect_summary_equal(rt[ord_t, , drop = FALSE], ri[ord_i, , drop = FALSE],
                       info = "rmst")

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

test_that("shared baseline shapes: the target HR is the same estimand as index", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  # `aux_by = "none"` is essential here, not incidental. With the default
  # ".study" both routes evaluate the marginal hazard ratio at pred_times[1] and
  # agree whatever the target route does, so the same test written the obvious
  # way proves nothing. With shared shapes the baseline cancels and the built-in
  # scalar is the closed-form t -> 0 limit, which is the estimand the target
  # route has to reproduce when it standardizes to the IPD covariates.
  dat <- sim_survival_data(seed = 2026, n_ipd = 120, n_agd = 120, n_int = 32)
  fit <- fit_survival_test(dat, distribution = "weibull", aux_by = "none")
  ipd_cov <- dat$ipd$data[, dat$covariates]

  hr_idx <- suppressMessages(
    marginal_effects(fit, population = "index", effect = "hr")
  )
  hr_tgt <- suppressMessages(
    marginal_effects(fit, newdata = ipd_cov, effect = "hr")
  )
  expect_equal(hr_tgt$mean[1], hr_idx$mean[1], tolerance = 1e-6)
  expect_equal(hr_tgt$at_time[1], 0)
  expect_equal(hr_idx$at_time[1], 0)

  # The evaluation time is not a knob when the shapes are shared, on either
  # route: there is no grid point to snap to. Both routes accept the one time
  # that is true, 0, and reject any other, so the same call behaves the same
  # way whether or not `newdata` is supplied.
  expect_error(
    suppressMessages(marginal_effects(fit, newdata = ipd_cov, effect = "hr",
                                      at_time = 2)),
    "different baseline shapes"
  )
  expect_error(
    suppressMessages(marginal_effects(fit, population = "index",
                                      effect = "hr", at_time = 2)),
    "different baseline shapes"
  )
  expect_equal(
    suppressMessages(marginal_effects(fit, population = "index", effect = "hr",
                                      at_time = 0))$mean[1],
    hr_idx$mean[1]
  )
  expect_equal(
    suppressMessages(marginal_effects(fit, newdata = ipd_cov, effect = "hr",
                                      at_time = 0))$mean[1],
    hr_tgt$mean[1]
  )

  # `tr` is an alias for `hr` on the built-in route; it has to be one here too.
  expect_equal(
    suppressMessages(
      marginal_effects(fit, newdata = ipd_cov, effect = "tr")
    )$mean[1],
    hr_tgt$mean[1]
  )
})

test_that("transported frames have the same shape as their built-in twins", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026, n_ipd = 120, n_agd = 120, n_int = 32)
  fit <- fit_survival_test(dat, distribution = "weibull")
  ipd_cov <- dat$ipd$data[, dat$covariates]

  # Scalar predictions: long, with `population`, `value` and (for RMST) the
  # horizon as a column rather than an attribute.
  for (ty in c("rmst", "median")) {
    raw_idx <- suppressMessages(
      predict(fit, population = "index", type = ty, summary = FALSE)
    )
    raw_tgt <- suppressMessages(
      predict(fit, newdata = ipd_cov, type = ty, summary = FALSE)
    )
    expect_equal(names(raw_tgt), names(raw_idx), info = ty)
    expect_false(is.null(raw_tgt$value), info = ty)
    sum_idx <- suppressMessages(predict(fit, population = "index", type = ty))
    sum_tgt <- suppressMessages(predict(fit, newdata = ipd_cov, type = ty))
    expect_equal(names(sum_tgt), names(sum_idx), info = ty)
  }

  # Curves: the label columns come first on both routes, and the default
  # survival curve starts at the origin.
  for (ty in c("survival", "cumhaz", "hazard")) {
    raw_idx <- suppressMessages(
      predict(fit, population = "index", type = ty, summary = FALSE)
    )
    raw_tgt <- suppressMessages(
      predict(fit, newdata = ipd_cov, type = ty, summary = FALSE)
    )
    expect_equal(names(raw_tgt), names(raw_idx), info = ty)
  }
  expect_true("t_0" %in% names(suppressMessages(
    predict(fit, newdata = ipd_cov, type = "survival", summary = FALSE)
  )))

  # `times` is refused for scalar summaries on both routes, not ignored.
  expect_error(
    suppressMessages(predict(fit, newdata = ipd_cov, type = "rmst",
                             times = c(1, 2))),
    "scalar summary"
  )

  # Raw effect draws carry the same column names, `_target` in place of
  # `_index`, so a caller can reach the same quantity by the same name.
  eff_tgt <- suppressMessages(
    marginal_effects(fit, newdata = ipd_cov, effect = "rmstd", summary = FALSE)
  )
  expect_equal(names(eff_tgt), "rmst_diff_target")
  eff_sum <- suppressMessages(
    marginal_effects(fit, newdata = ipd_cov, effect = "rmstd")
  )
  eff_idx <- marginal_effects(fit, population = "index", effect = "rmstd")
  expect_equal(names(eff_sum), names(eff_idx))

  # Asking for a few times must give the same answers as asking for all of
  # them. The transported hazard and log-HR routes evaluate only the requested
  # times, and the basis matrices carry one row per fitted time, so the row and
  # the time have to be selected together; getting that wrong would silently
  # evaluate the wrong times. Reuses the fit above rather than sampling again:
  # every extra fit in this file is another chance to hit the CmdStan CSV race.
  want <- fit$pred_times[c(2L, 5L)]
  for (ty in c("hazard", "loghr")) {
    full <- suppressMessages(predict(fit, newdata = ipd_cov, type = ty))
    part <- suppressMessages(predict(fit, newdata = ipd_cov, type = ty,
                                     times = want))
    expect_equal(sort(unique(part$time)), sort(want), info = ty)
    keep <- full[full$time %in% want, , drop = FALSE]
    expect_equal(part$mean, keep$mean, tolerance = 1e-10, info = ty)
  }
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

test_that("a numeric treatment label survives the synthetic origin row", {
  skip_if_not_installed("ggplot2")
  # The origin row overwrote every numeric column with the origin value, so a
  # numerically-labelled treatment became "1" on its own t = 0 row.
  values <- list(matrix(c(0.9, 0.8), nrow = 1), matrix(c(0.7, 0.6), nrow = 1))
  cells <- data.frame(treatment = c(10, 20), population = "Index")
  out <- mlumr:::.surv_result_frame(
    values, cells, type = "survival", summary = TRUE,
    probs = c(0.025, 0.975), times_out = c(1, 2), origin = 1
  )
  origin_rows <- out[out$time == 0, , drop = FALSE]
  expect_setequal(origin_rows$treatment, c(10, 20))
  expect_true(all(origin_rows$mean == 1))
})

test_that("survival predictions survive a numeric treatment label", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # set_ipd()/set_agd() accept numeric and factor treatment identifiers. The
  # shared-frame refactor built the label column with vapply(character(1)),
  # which rejects those before any prediction is assembled.
  dat <- sim_survival_data(seed = 2026, n_ipd = 120, n_agd = 120, n_int = 32)
  dat$index_treatment <- 1
  dat$comparator_treatment <- 0
  fit <- fit_survival_test(dat, distribution = "weibull")

  pr <- suppressMessages(predict(fit, population = "index", type = "survival"))
  expect_setequal(unique(pr$treatment), c(0, 1))
  expect_true(all(is.finite(pr$mean)))
  # The synthetic origin row must not take the origin value as its label.
  origin <- pr[pr$time == 0, , drop = FALSE]
  expect_setequal(origin$treatment, c(0, 1))
  expect_true(all(origin$mean == 1))
})
