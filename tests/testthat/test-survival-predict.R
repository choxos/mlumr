# Survival prediction / effect tests. Require Stan compilation; slow.

test_that("survival predict() returns valid curves, RMST, and median", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026)
  fit <- fit_survival_test(dat, distribution = "weibull")

  surv <- predict(fit, type = "survival")
  expect_true(all(c("treatment", "population", "time", "mean") %in% names(surv)))
  expect_true(all(surv$mean >= 0 & surv$mean <= 1))

  # survival is non-increasing in time within each treatment x population cell
  cell <- surv[surv$treatment == fit$data$index_treatment &
                 surv$population == "Index", ]
  cell <- cell[order(cell$time), ]
  expect_true(all(diff(cell$mean) <= 1e-6))

  haz <- predict(fit, type = "hazard")
  expect_true(all(haz$mean >= 0))

  cumhaz <- predict(fit, type = "cumhaz")
  expect_true(all(cumhaz$mean >= -1e-8))

  horizon <- max(fit$pred_times)
  rmst <- predict(fit, type = "rmst")
  expect_equal(nrow(rmst), 4)
  expect_true(all(rmst$mean <= horizon + 1e-6))
  # RMST is an integral to a restriction time, so the value alone is not the
  # estimand: the horizon actually integrated to travels with it. mlumr() can
  # narrow a requested rmst_horizon to the support both studies observed, so it
  # is read off the fitted grid rather than assumed.
  tau <- max(fit$stan_data$rmst_grid_times)
  expect_true("horizon" %in% names(rmst))
  expect_equal(unique(rmst$horizon), tau)

  med <- predict(fit, type = "median")
  expect_equal(nrow(med), 4)
  # Median summaries expose the posterior probability that the median is beyond
  # follow-up; the other summaries are conditional on the median being reached.
  expect_true("p_not_reached" %in% names(med))
  expect_true(all(med$p_not_reached >= 0 & med$p_not_reached <= 1))

  n_draws <- nrow(fit$draws)
  rmst_draws <- predict(fit, type = "rmst", summary = FALSE)
  expect_false("mean" %in% names(rmst_draws))
  expect_true("value" %in% names(rmst_draws))
  expect_equal(nrow(rmst_draws), 4L * n_draws)
  expect_equal(unique(rmst_draws$horizon), tau)

  # marginal_effects() carries it on the RMST rows only: the hazard ratio has an
  # evaluation time instead, and the two are different things.
  me_all <- suppressMessages(marginal_effects(fit, effect = "all"))
  expect_true("horizon" %in% names(me_all))
  expect_equal(unique(me_all$horizon[me_all$effect %in% c("RMSTD", "RMSTR")]),
               tau)
  expect_true(all(is.na(me_all$horizon[!me_all$effect %in% c("RMSTD", "RMSTR")])))
  expect_equal(attr(me_all, "rmst_horizon"), tau)
  me_raw <- suppressMessages(
    marginal_effects(fit, effect = "rmstd", summary = FALSE))
  expect_true(all(attr(me_raw, "horizon") == tau))

  median_draws <- predict(fit, type = "median", summary = FALSE)
  expect_false("mean" %in% names(median_draws))
  expect_true("value" %in% names(median_draws))
  expect_equal(nrow(median_draws), 4L * n_draws)

  lhr_draws <- predict(fit, type = "loghr", population = "comparator",
                       summary = FALSE)
  expect_false("mean" %in% names(lhr_draws))
  expect_true(all(startsWith(names(lhr_draws)[-1], "t_")))
  expect_equal(nrow(lhr_draws), n_draws)
})

test_that("survival predict() honors population and times arguments", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026)
  fit <- fit_survival_test(dat, distribution = "weibull")

  idx <- predict(fit, population = "index", type = "survival")
  expect_true(all(idx$population == "Index"))

  some_times <- stats::quantile(fit$pred_times, c(0.25, 0.5, 0.75))
  sub <- predict(fit, type = "survival", times = some_times)
  expect_equal(length(unique(sub$time)), length(some_times))
})

test_that("survival marginal and conditional effects are labeled correctly", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026)
  fit_ph <- fit_survival_test(dat, distribution = "weibull")
  me <- marginal_effects(fit_ph, effect = "hr")
  expect_true(all(me$effect == "HR"))
  rd <- marginal_effects(fit_ph, effect = "rmstd")
  expect_true(all(rd$effect == "RMSTD"))

  # Under the stratified default the exponentiated coefficient contrast is NOT
  # the conditional hazard ratio (the h0_index/h0_comparator factor does not
  # cancel), so it must not be labeled HR. Asking for `hr` by name is now an
  # ERROR rather than a differently-named substitute: returning one estimand
  # under the name of another is what the guard exists to prevent.
  expect_error(conditional_effects(fit_ph, effect = "hr"),
               "not available")
  # `tr` on a proportional-hazards fit fails for a second, prior reason: a PH
  # model has no constant time ratio at all, so the request names an estimand
  # that does not exist here rather than one this fit cannot isolate.
  expect_error(conditional_effects(fit_ph, effect = "tr"),
               "only available for accelerated failure time")
  # The default `effect = "all"` still returns it, under its own honest name.
  ce <- conditional_effects(fit_ph, effect = "all")
  expect_true(all(ce$effect == "EXP_ETA_CONTRAST"))
  # With one shared baseline the contrast is exact and the HR label is restored,
  # so the explicit request succeeds.
  fit_shared <- fit_survival_test(dat, distribution = "weibull",
                                  aux_by = "none")
  ce_shared <- conditional_effects(fit_shared, effect = "hr")
  expect_true(all(ce_shared$effect == "HR"))

  cp <- conditional_predict(fit_ph)
  expect_true(all(c("profile", "treatment", "time", "mean") %in% names(cp)))
  expect_true(all(cp$mean >= 0 & cp$mean <= 1))

  # An AFT contrast is a time ratio only when the shape/scale parameters are
  # shared. Stratified (the default) gives each study its own shape, and the
  # quantile ratio then picks up shape-dependent factors, so only the location
  # contrast remains and the TR label is withheld.
  fit_aft <- fit_survival_test(dat, distribution = "lognormal")
  # Naming an estimand the fit cannot produce is an error, and the message must
  # point at the one it can.
  expect_error(marginal_effects(fit_aft, effect = "tr"), "exp_delta_eta")
  expect_error(marginal_effects(fit_aft, effect = "hr"), "exp_delta_eta")
  me_aft <- marginal_effects(fit_aft, effect = "exp_delta_eta")
  expect_true(all(me_aft$effect == "EXP_DELTA_ETA"))
  expect_true(all(me_aft$mean > 0))
  # A location contrast has no evaluation time, so it must not carry one.
  expect_true(all(is.na(me_aft$at_time)) || is.null(me_aft$at_time))
  # `effect = "all"` is unaffected and still labels it honestly.
  expect_true("EXP_DELTA_ETA" %in% marginal_effects(fit_aft, effect = "all")$effect)

  # The EXP_ETA_CONTRAST relabeling happens whenever the shapes differ, so the
  # warning has to cover AFT as well as PH. Gating it on proportional hazards
  # let a stratified AFT fit relabel its estimand silently while the docs
  # promised otherwise.
  expect_warning(conditional_effects(fit_aft, effect = "all"),
                 "quantile-dependent factors")
  expect_warning(conditional_effects(fit_aft, effect = "all"),
                 "MARGINAL curve")
  ce_aft <- suppressWarnings(conditional_effects(fit_aft, effect = "all"))
  expect_true(all(ce_aft$effect == "EXP_ETA_CONTRAST"))

  fit_aft_shared <- fit_survival_test(dat, distribution = "lognormal",
                                      aux_by = "none")
  me_aft_shared <- marginal_effects(fit_aft_shared, effect = "tr")
  expect_true(all(me_aft_shared$effect == "TR"))
  # The selector is literal on the marginal route too. Asking a shared-shape AFT
  # fit for a hazard ratio used to return the time ratio labeled TR, so a
  # programmatic caller could request an HR and be handed a different estimand
  # under a name it never asked for.
  expect_error(marginal_effects(fit_aft_shared, effect = "hr"),
               "not available for this fit")
  expect_error(marginal_effects(fit_aft_shared, effect = "hr"), "time ratio")
  # With shared shapes the AFT time ratio exists and an explicit `tr` request
  # returns it. The mirrored `hr` request is an error: the conditional hazard
  # ratio of an AFT model varies with time, so there is no scalar to return and
  # handing back the time ratio under the name `hr` would report a different
  # estimand than the one asked for.
  ce_aft_shared <- conditional_effects(fit_aft_shared, effect = "tr")
  expect_true(all(ce_aft_shared$effect == "TR"))
  expect_error(conditional_effects(fit_aft_shared, effect = "hr"),
               "not a scalar conditional effect")
  # Shared shapes: exp_delta_eta is not the right name for it, and saying so is
  # the mirror image of the guard above.
  expect_error(marginal_effects(fit_aft_shared, effect = "exp_delta_eta"),
               "shapes differ by study")
  expect_error(marginal_effects(fit_aft_shared, effect = "exp_delta_eta"),
               "relaxed AFT")
})

test_that("relaxed-PH marginal HR is flagged and matches loghr at its stated time", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  skip_if_not_installed("withr")
  # Reset the one-per-session note so the message is emitted in this test.
  withr::local_options(list(mlumr.marginal_hr_note = NULL))

  dat <- sim_survival_data(seed = 2026)
  fit_rel <- fit_survival_test(dat, model = "relaxed", distribution = "weibull")

  # The scalar is flagged with the time it is evaluated at. Left to itself that
  # time comes from the OUTPUT GRID, so the note says so and points at the
  # argument that makes it an explicit estimand choice instead.
  expect_message(
    me <- marginal_effects(fit_rel, effect = "hr", population = "comparator"),
    "the first prediction time"
  )
  expect_true(all(me$effect == "HR"))
  expect_true("at_time" %in% names(me))
  expect_equal(unique(me$at_time), fit_rel$pred_times[1])

  # `at_time` selects the evaluation time directly. An exact grid time is used
  # as given, and the returned scalar is the loghr curve at that time, not a
  # re-scaled version of the first one.
  t3 <- fit_rel$pred_times[3]
  me3 <- suppressMessages(
    marginal_effects(fit_rel, effect = "hr", population = "comparator",
                     at_time = t3))
  expect_equal(unique(me3$at_time), t3)
  expect_equal(unname(me3$mean[1]),
               mean(exp(fit_rel$draws[["loghr_comparator[3]"]])),
               tolerance = 1e-8)
  # Asking for the first grid time reproduces the default exactly, which is the
  # cross-check that delta_* really is loghr_*[1] under a stratified baseline.
  me1 <- suppressMessages(
    marginal_effects(fit_rel, effect = "hr", population = "comparator",
                     at_time = fit_rel$pred_times[1]))
  expect_equal(me1$mean, me$mean, tolerance = 1e-10)
  # An off-grid request is snapped to the nearest fitted time, and says so.
  expect_message(
    me_off <- marginal_effects(fit_rel, effect = "hr",
                               population = "comparator",
                               at_time = t3 + diff(fit_rel$pred_times)[1] / 8),
    "using the nearest one")
  expect_equal(unique(me_off$at_time), t3)

  # The raw-draw frame carries the same time, so a time-specific hazard ratio
  # never travels without it.
  raw <- suppressMessages(
    marginal_effects(fit_rel, effect = "hr", population = "comparator",
                     summary = FALSE, at_time = t3))
  expect_equal(unname(attr(raw, "at_time")[["hr_comparator"]]), t3)
  # RMST is collapsible and carries no such dependence.
  rd_rel <- marginal_effects(fit_rel, effect = "rmstd", population = "comparator")
  expect_false("at_time" %in% names(rd_rel))
  # HR is reported on the natural scale (null 1), so it is strictly positive
  expect_true(all(me$mean > 0))

  # This fit has study-specific Weibull shapes (aux_by = ".study" is the
  # default), so delta_comparator is NOT the t -> 0 limit: Stan takes it from
  # the time-varying marginal loghr curve at the FIRST PREDICTION TIME. It must
  # therefore equal that curve's earliest fitted value exactly, not approximately.
  lhr <- predict(fit_rel, type = "loghr", population = "comparator")
  lhr <- lhr[order(lhr$time), ]
  delta_cmp <- mean(fit_rel$draws$delta_comparator)
  expect_equal(lhr$mean[1], delta_cmp, tolerance = 1e-8)

  # ...and the marginal log HR genuinely varies over follow-up (non-collapsible),
  # so the scalar is not representative of the whole curve.
  expect_gt(length(unique(round(lhr$mean, 8))), 1L)
  expect_true(all(is.finite(lhr$mean)))
})

test_that("survival reports natural-scale RMST ratio and time-varying log HR (poster estimands)", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026)
  fit <- fit_survival_test(dat, distribution = "weibull")

  # Natural-scale RMST ratio (null 1) as a marginal effect
  rr <- marginal_effects(fit, effect = "rmstr")
  expect_true(all(rr$effect == "RMSTR"))
  expect_true(all(rr$mean > 0))  # natural ratio is strictly positive
  expect_equal(nrow(rr), 2)  # index + comparator populations

  # `all` returns HR, RMSTD, and RMSTR
  me_all <- marginal_effects(fit, effect = "all")
  expect_setequal(unique(me_all$effect), c("HR", "RMSTD", "RMSTR"))

  # Time-varying marginal log hazard ratio at each fitted time, per population
  lhr <- predict(fit, type = "loghr")
  expect_true(all(c("population", "time", "mean") %in% names(lhr)))
  expect_equal(length(unique(lhr$time)), length(fit$pred_times))
  expect_true(all(is.finite(lhr$mean)))

  # Sanity: the RMST inputs to the ratio (RMST_index, RMST_comparator) are finite
  rmst <- predict(fit, type = "rmst")
  ri <- rmst$mean[rmst$treatment == fit$data$index_treatment &
                    rmst$population == "Index"]
  rc <- rmst$mean[rmst$treatment == fit$data$comparator_treatment &
                    rmst$population == "Index"]
  # posterior-mean ratio is close (not identical) to the mean of the log ratio;
  # just check the sign/scale are sensible and finite
  expect_true(is.finite(log(ri) - log(rc)))
})

test_that("survival print and summary methods render", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026)
  fit <- fit_survival_test(dat, distribution = "weibull")
  expect_output(print(fit), "Time-to-event")
  expect_output(summary(fit), "Hazard Ratios")
  expect_output(prior_summary(fit), "Survival auxiliary")
})

test_that("M-spline and piecewise-exponential loghr stay finite in the extreme tail", {
  # Guards mspline_log_mean_haz(): the flexible-baseline marginal log HR must be
  # finite even at late prediction times where the natural-scale marginal hazard
  # underflows.
  #
  # This used to carry skip_on_ci() as well, which meant the guard ran in NO
  # automated environment: a regression in mspline_log_mean_haz() would have
  # gone undetected everywhere. The cost came from full-length fits, but the
  # property under test is a property of the GENERATED QUANTITIES at extreme
  # prediction times, not of a converged posterior. A deliberately tiny,
  # unconverged fit exercises exactly the same arithmetic, so the guard now runs
  # wherever Stan is available.
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)
  max_t <- max(c(dat$ipd$data$.time, dat$agd$pseudo_ipd$.time))
  far <- sort(unique(c(seq(max_t / 50, max_t, length.out = 10),
                       max_t * c(5, 25, 120))))
  for (d in c("mspline", "pexp")) {
    args <- list(dat, distribution = d, pred_times = far,
                 chains = 1, iter = 120, warmup = 60)
    if (d == "mspline") args$n_knots <- 4
    fit <- suppressWarnings(do.call(fit_survival_test, args))
    expect_true(any(grepl("^loghr_", names(fit$draws))),
                info = sprintf("%s emits loghr_* columns", d))
    lhr <- predict(fit, type = "loghr")
    expect_true(all(is.finite(lhr$mean)),
                info = sprintf("%s loghr finite at extreme tail", d))
  }
})
