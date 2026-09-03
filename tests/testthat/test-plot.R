# Plot methods return ggplot objects, and the classed result objects remain
# data.frame subclasses (backward compatible). Structural tests on small
# hand-built classed objects -> no Stan fit needed, CRAN-fast.

test_that("classed result objects are still data.frames", {
  me <- mlumr:::.mlumr_result(
    data.frame(variable = "lor_index", effect = "LOR", population = "Index",
               mean = 0.2, sd = 0.1, q2.5 = 0, q50 = 0.2, q97.5 = 0.4),
    "mlumr_marginal_effects", family = "binomial")
  expect_s3_class(me, "mlumr_marginal_effects")
  expect_s3_class(me, "data.frame")
  expect_equal(me$mean, 0.2)
  expect_identical(attr(me, "family"), "binomial")
})

test_that("plot.mlumr_marginal_effects returns a ggplot", {
  skip_if_not_installed("ggplot2")
  me <- mlumr:::.mlumr_result(
    data.frame(variable = c("lor_index", "lor_comparator", "rr_index", "rr_comparator"),
               effect = c("LOR", "LOR", "RR", "RR"),
               population = rep(c("Index", "Comparator"), 2),
               mean = c(0.2, 0.3, 1.2, 1.3), sd = 0.1,
               q2.5 = c(0, 0.1, 0.9, 1.0), q50 = c(0.2, 0.3, 1.2, 1.3),
               q97.5 = c(0.4, 0.5, 1.6, 1.7)),
    "mlumr_marginal_effects", family = "binomial")
  p <- plot(me)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot.mlumr_prediction handles curve and scalar types", {
  skip_if_not_installed("ggplot2")
  surv <- mlumr:::.mlumr_result(
    data.frame(treatment = rep(c("A", "B"), each = 3), population = "Comparator",
               time = rep(1:3, 2), mean = c(.9, .7, .5, .95, .8, .6), sd = 0.05,
               q2.5 = c(.8, .6, .4, .9, .7, .5), q50 = c(.9, .7, .5, .95, .8, .6),
               q97.5 = c(.95, .8, .6, .99, .9, .7)),
    "mlumr_prediction", ptype = "survival")
  expect_s3_class(plot(surv), "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot(surv)))

  lhr <- mlumr:::.mlumr_result(
    data.frame(population = "Comparator", time = 1:3, mean = c(.1, .2, .3),
               sd = .05, q2.5 = c(0, .1, .2), q50 = c(.1, .2, .3), q97.5 = c(.2, .3, .4)),
    "mlumr_prediction", ptype = "loghr")
  expect_s3_class(plot(lhr), "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot(lhr)))

  rmst <- mlumr:::.mlumr_result(
    data.frame(treatment = c("A", "B"), population = "Comparator",
               mean = c(20, 24), sd = 2, q2.5 = c(16, 20), q50 = c(20, 24),
               q97.5 = c(24, 28)),
    "mlumr_prediction", ptype = "rmst")
  expect_s3_class(plot(rmst), "ggplot")
  expect_no_error(ggplot2::ggplot_build(plot(rmst)))
})

test_that("plot.mlumr_conditional_effects renders, not just constructs", {
  skip_if_not_installed("ggplot2")
  ce <- mlumr:::.mlumr_result(
    data.frame(profile = 1:3, effect = "HR", mean = c(.2, .4, .3), sd = .1,
               q2.5 = c(0, .2, .1), q50 = c(.2, .4, .3), q97.5 = c(.4, .6, .5)),
    "mlumr_conditional_effects", family = "survival")
  p <- plot(ce)
  expect_s3_class(p, "ggplot")
  # Regression guard: the credible-interval layer must map to columns that are
  # in the plot data. Building the plot exercises the aesthetics; a stale `.lo`
  # / `.hi` reference (assigned after ggplot() captured the data) errored here.
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_prior_posterior is exported and validates its input", {
  expect_true(is.function(plot_prior_posterior))
  expect_error(plot_prior_posterior(list()), regexp = "mlumr_fit|combine_data|fit")
})

test_that("geom_km() rejects left/interval-censored data (right-censored only)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survival")
  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)
  expect_s3_class(geom_km(dat)[[1]], "ggproto")
  dat_left <- dat
  dat_left$ipd$data$.status[1] <- 2L
  expect_error(geom_km(dat_left), "left- or interval-censored")
  dat_interval <- dat
  dat_interval$ipd$data$.status[1] <- 3L
  expect_error(geom_km(dat_interval), "left- or interval-censored")
})

test_that("mlumr_forest() returns a ggplot and validates its columns", {
  skip_if_not_installed("ggplot2")
  forest_df <- data.frame(
    label = c("Naive", "STC", "ML-UMR"),
    est = c(0.20, 0.30, 0.25),
    lo = c(-0.10, 0.00, 0.05),
    hi = c(0.50, 0.60, 0.45)
  )
  expect_s3_class(mlumr_forest(forest_df, ref_line = 0, x = "Log odds ratio"),
                  "ggplot")
  alt <- forest_df
  names(alt)[1] <- "method"
  expect_s3_class(mlumr_forest(alt), "ggplot")
  expect_error(mlumr_forest(data.frame(label = "x")), "column")
})

test_that("RMST prediction plots name the restriction time", {
  skip_if_not_installed("ggplot2")
  # RMST(tau) = integral of S(t) over [0, tau]. A figure that omits tau reads as
  # though the quantity were horizon-independent, and two forests drawn to
  # different horizons look comparable when they are different estimands.
  df <- data.frame(treatment = c("A", "B"), population = c("Index", "Index"),
                   horizon = c(64.8, 64.8), mean = c(30, 27),
                   q2.5 = c(28, 25), q97.5 = c(32, 29))
  pred <- structure(df, ptype = "rmst",
                    class = c("mlumr_prediction", "data.frame"))

  expect_equal(mlumr:::.prediction_rmst_horizon(pred, df), 64.8)
  p <- plot(pred)
  expect_s3_class(p, "gg")
  expect_match(p$labels$caption, "tau = 64.8")
  expect_match(p$labels$x, "tau = 64.8")

  # Two horizons on one axis is a category error, not a formatting choice.
  df2 <- df
  df2$horizon <- c(64.8, 70)
  pred2 <- structure(df2, ptype = "rmst",
                     class = c("mlumr_prediction", "data.frame"))
  expect_error(mlumr:::.prediction_rmst_horizon(pred2, df2),
               "different horizons")
  expect_error(plot(pred2), "different estimands")

  # A prediction that carries no horizon (any non-RMST type) is unaffected.
  df3 <- data.frame(treatment = c("A", "B"), mean = c(0.4, 0.5),
                    q2.5 = c(0.3, 0.4), q97.5 = c(0.5, 0.6))
  pred3 <- structure(df3, ptype = "response",
                     class = c("mlumr_prediction", "data.frame"))
  expect_s3_class(plot(pred3), "gg")
})


test_that("the conditional-effects forest reads its null line from the effect", {
  skip_if_not_installed("ggplot2")
  mk <- function(effect) {
    mlumr:::.mlumr_result(
      data.frame(profile = 1:2, effect = effect, mean = c(1.2, 1.1),
                 sd = c(.1, .1), q2.5 = c(.9, .8), q97.5 = c(1.6, 1.5)),
      "mlumr_conditional_effects", family = "poisson")
  }
  # The defect: ref_line defaulted to 0 for every effect, so a rate-ratio panel
  # drew its null off the plotted scale. Read the reference from the layer's own
  # data rather than from the built plot: an all-ratio panel is drawn on a log
  # axis, where the built coordinate of the null at 1 is log10(1) = 0, which
  # says nothing about whether the right null was chosen.
  vline_x <- function(p) {
    p$layers[[1]]$data$ref
  }
  expect_equal(unique(vline_x(plot(mk("RR")))), 1)
  expect_equal(unique(vline_x(plot(mk("HR")))), 1)
  expect_equal(unique(vline_x(plot(mk("EXP_ETA_CONTRAST")))), 1)
  expect_equal(unique(vline_x(plot(mk("RD")))), 0)
  expect_equal(unique(vline_x(plot(mk("MD")))), 0)
  # An explicit override still wins.
  expect_equal(unique(vline_x(plot(mk("RR"), ref_line = 2))), 2)
})

test_that("the marginal forest puts EXP_DELTA_ETA on a ratio axis", {
  skip_if_not_installed("ggplot2")
  me <- mlumr:::.mlumr_result(
    data.frame(variable = c("tr_index", "rmst_diff_index"),
               effect = c("EXP_DELTA_ETA", "RMSTD"),
               population = c("Index", "Index"),
               mean = c(1.3, 2.0), sd = c(.1, .3),
               q2.5 = c(1.0, 1.2), q97.5 = c(1.7, 2.8)),
    "mlumr_marginal_effects", family = "survival")
  d <- ggplot2::ggplot_build(plot(me))$data[[1]]
  expect_setequal(d$xintercept, c(1, 0))
})

test_that("the interval label reports the coverage actually plotted", {
  skip_if_not_installed("ggplot2")
  me <- function(lo, hi) {
    d <- data.frame(variable = "lor_index", effect = "LOR", population = "Index",
                    mean = 0.3, sd = 0.1)
    d[[paste0("q", lo)]] <- 0.1
    d[[paste0("q", hi)]] <- 0.5
    mlumr:::.mlumr_result(d, "mlumr_marginal_effects", family = "binomial")
  }
  expect_match(plot(me(2.5, 97.5))$labels$x, "95% credible interval", fixed = TRUE)
  # The label was hard-coded, so an 80% interval was announced as 95%.
  expect_match(plot(me(10, 90))$labels$x, "80% credible interval", fixed = TRUE)
})

test_that("an all-ratio forest is drawn on a log axis", {
  skip_if_not_installed("ggplot2")
  mk <- function(effects) {
    mlumr:::.mlumr_result(
      data.frame(variable = paste0("v", seq_along(effects)), effect = effects,
                 population = "Index", mean = rep(1.2, length(effects)),
                 sd = 0.1, q2.5 = 0.9, q97.5 = 1.6),
      "mlumr_marginal_effects", family = "survival")
  }
  is_log <- function(p) {
    any(vapply(p$scales$scales, function(s) identical(s$trans$name, "log-10"),
               logical(1)))
  }
  # 0.5 and 2 are the same effect in opposite directions and must sit at equal
  # distances from the null, which an identity axis does not do.
  expect_true(is_log(plot(mk(c("HR", "RMSTR")))))
  # A mixed panel set cannot: an additive measure has no log axis.
  expect_false(is_log(plot(mk(c("HR", "RMSTD")))))
})

test_that("a time-specific hazard ratio carries its evaluation time", {
  skip_if_not_installed("ggplot2")
  mk <- function(at) {
    mlumr:::.mlumr_result(
      data.frame(variable = "hr_index", effect = "HR", population = "Index",
                 at_time = at, mean = 0.8, sd = 0.1, q2.5 = 0.6, q97.5 = 1.1),
      "mlumr_marginal_effects", family = "survival")
  }
  lv <- levels(ggplot2::ggplot_build(plot(mk(12)))$plot$data$.facet)
  expect_true(any(grepl("t = 12", lv)))
  # Forests at two evaluation times were labelled identically although they are
  # different estimands.
  expect_false(identical(
    levels(ggplot2::ggplot_build(plot(mk(36)))$plot$data$.facet), lv
  ))
  mixed <- mlumr:::.mlumr_result(
    data.frame(variable = c("hr_index", "hr_comparator"), effect = "HR",
               population = c("Index", "Comparator"), at_time = c(12, 36),
               mean = 0.8, sd = 0.1, q2.5 = 0.6, q97.5 = 1.1),
    "mlumr_marginal_effects", family = "survival")
  expect_error(plot(mixed), "mix evaluation times")
})

test_that("mlumr_forest keeps its null line on a log axis and in view", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(label = c("A", "B"), est = c(0.5, 2),
                  lo = c(0.25, 1), hi = c(1, 4))
  # ref_line defaulted to 0, which log10() sends to -Inf, so no null was drawn.
  # Built coordinates are on the transformed scale, so the null at 1 lands on
  # log10(1) = 0; what matters is that it is finite and therefore drawn.
  vl <- ggplot2::ggplot_build(mlumr_forest(d, log_x = TRUE))$data[[1]]$xintercept
  expect_true(all(is.finite(vl)))
  expect_equal(unique(vl), 0)
  expect_error(mlumr_forest(d, log_x = TRUE, ref_line = 0), "must be positive")

  # Clipping is about one wide interval, not about hiding the null.
  far <- data.frame(label = c("A", "B", "C"), est = c(10, 11, 12),
                    lo = c(9, 10, -100), hi = c(11, 12, 100))
  p <- mlumr_forest(far, ref_line = 0)
  xr <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  expect_lte(xr[1], 0)
})

test_that("mlumr_forest refuses to put two effect scales on one axis", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(label = c("A", "B"), effect = c("LOG_HR", "HR"),
                  est = c(log(2), 2), lo = c(log(1.2), 1.2),
                  hi = c(log(3), 3))
  expect_error(mlumr_forest(d), "same effect scale")
})

test_that("plot_prior_posterior uses each parameter's own prior", {
  skip_if_not_installed("ggplot2")
  fit <- structure(
    list(
      family = "normal", model = "spfa", link = "identity",
      data = list(covariates = "age"),
      draws = data.frame(mu_index = stats::rnorm(200),
                         sigma = abs(stats::rnorm(200)) + 0.2),
      priors = list(intercept = prior_normal(0, 10),
                    sigma = prior_exponential(1))
    ),
    class = "mlumr_fit"
  )
  p <- plot_prior_posterior(fit, pars = c("mu_index", "sigma"))
  pd <- p$layers[[2]]$data
  # sigma is declared <lower=0>; the intercept prior put mass below zero there.
  expect_true(all(pd$density[pd$parameter == "sigma" & pd$value < 0] == 0) ||
                all(pd$value[pd$parameter == "sigma"] >= 0))
  expect_gt(max(pd$density[pd$parameter == "sigma"]),
            max(pd$density[pd$parameter == "mu_index"]))
  # A parameter with no recorded prior is refused rather than given someone
  # else's.
  fit$draws$nuisance <- stats::rnorm(200)
  expect_error(plot_prior_posterior(fit, pars = "nuisance"), "No prior is recorded")
})

test_that("the observed KM curves stay in the population they were measured in", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survival")
  dat <- sim_survival_data(seed = 2026, n_ipd = 60, n_agd = 60, n_int = 8)
  layers <- geom_km(dat)
  km <- layers[[1]]$data
  # plot() facets by population, and a layer with no `population` column is
  # drawn into EVERY facet, so both observed curves appeared in both panels.
  expect_true("population" %in% names(km))
  expect_setequal(unique(km$population), c("Index", "Comparator"))
  idx_trt <- dat$index_treatment
  expect_true(all(km$population[km$treatment == idx_trt] == "Index"))
  expect_true(all(km$population[km$treatment != idx_trt] == "Comparator"))
})

test_that("beta_comparator falls back to the beta prior when the fit shares it", {
  skip_if_not_installed("ggplot2")
  # The relaxed models apply the resolved `beta` prior to both coefficient
  # vectors unless the fit records a comparator-specific one, so refusing to
  # draw `beta_comparator[1]` would withhold a prior that is in fact known.
  fit <- structure(
    list(
      family = "binomial", model = "relaxed", link = "logit",
      data = list(covariates = "age"),
      draws = data.frame(`beta_comparator[1]` = stats::rnorm(200),
                         check.names = FALSE),
      priors = list(
        beta = prior_normal(0, 2.5),
        beta_resolved = list(covariate_names = "age", mean = 0, sd = 2.5,
                             dist = 0L, df = NA_real_, autoscale = FALSE,
                             sd_x = 1)
      )
    ),
    class = "mlumr_fit"
  )
  p <- plot_prior_posterior(fit, pars = "beta_comparator[1]")
  expect_s3_class(p, "ggplot")
  expect_gt(nrow(p$layers[[2]]$data), 0)
})

test_that("an all-ratio conditional forest is drawn on a log axis too", {
  skip_if_not_installed("ggplot2")
  mk <- function(effect) {
    mlumr:::.mlumr_result(
      data.frame(profile = 1:2, effect = effect, mean = c(1.2, 1.1),
                 sd = c(.1, .1), q2.5 = c(.9, .8), q97.5 = c(1.6, 1.5)),
      "mlumr_conditional_effects", family = "poisson")
  }
  is_log <- function(p) {
    any(vapply(p$scales$scales, function(s) identical(s$trans$name, "log-10"),
               logical(1)))
  }
  expect_true(is_log(plot(mk("RR"))))
  expect_false(is_log(plot(mk("RD"))))
})

test_that("each observed KM curve starts at the origin", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survival")
  dat <- sim_survival_data(seed = 2026, n_ipd = 60, n_agd = 60, n_int = 8)
  km <- geom_km(dat)[[1]]$data
  # S(0) = 1 exactly, per arm. Previously supplied by survival::survfit0(),
  # which older releases the package declares do not export.
  for (trt in unique(km$treatment)) {
    arm <- km[km$treatment == trt, , drop = FALSE]
    expect_equal(min(arm$time), 0, info = trt)
    expect_equal(arm$surv[which.min(arm$time)], 1, info = trt)
  }
  # The censoring marks are fitted observations, not the synthetic origin.
  cens <- geom_km(dat)[[2]]$data
  if (nrow(cens)) expect_true(all(cens$time > 0))
})

test_that("shared treatment labels still give two observed curves", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survival")
  # combine_data() permits the IPD and AgD arms to carry the same treatment
  # name. Stratifying the KM on that label merged the two cohorts into one
  # curve and left a facet empty; the population is what identifies them.
  dat <- sim_survival_data(seed = 2026, n_ipd = 60, n_agd = 60, n_int = 8)
  dat$comparator_treatment <- dat$index_treatment

  km <- geom_km(dat)[[1]]$data
  expect_setequal(unique(km$population), c("Index", "Comparator"))
  expect_gt(nrow(km[km$population == "Index", , drop = FALSE]), 1)
  expect_gt(nrow(km[km$population == "Comparator", , drop = FALSE]), 1)
})

test_that("plot_prior_posterior refuses a parameter it cannot find", {
  skip_if_not_installed("ggplot2")
  fit <- structure(
    list(
      family = "binomial", model = "spfa", link = "logit",
      data = list(covariates = "age"),
      draws = data.frame(mu_index = stats::rnorm(100)),
      priors = list(intercept = prior_normal(0, 10))
    ),
    class = "mlumr_fit"
  )
  # One real name and one absent name silently produced a plot of just the
  # real one, so a misspelling looked like success.
  expect_error(plot_prior_posterior(fit, pars = c("mu_index", "sigma")),
               "Not in the fit's posterior draws")
  expect_s3_class(plot_prior_posterior(fit, pars = "mu_index"), "ggplot")
})

test_that("population selects the cohort when the labels cannot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survival")
  dat <- sim_survival_data(seed = 2026, n_ipd = 60, n_agd = 60, n_int = 8)
  dat$comparator_treatment <- dat$index_treatment

  only_cmp <- geom_km(dat, population = "Comparator")[[1]]$data
  expect_setequal(unique(only_cmp$population), "Comparator")
  # The documented comparator-only overlay used the treatment label, which
  # selects both cohorts when the two labels coincide.
  expect_warning(
    both <- geom_km(dat, treatments = dat$comparator_treatment)[[1]]$data,
    "cannot tell them apart"
  )
  expect_setequal(unique(both$population), c("Index", "Comparator"))
  expect_error(geom_km(dat, population = "Target"), "must be")
})

test_that("a prediction whose two series share a label is refused", {
  skip_if_not_installed("ggplot2")
  # Colour and fill key on `treatment`, so identical labels put both arms of a
  # population in one ggplot2 group and the line connects alternating rows of
  # two different predictions.
  pred <- mlumr:::.mlumr_result(
    data.frame(treatment = rep("A", 4), population = rep("Index", 4),
               time = c(1, 1, 2, 2), mean = c(.9, .8, .7, .6), sd = .05,
               q2.5 = c(.85, .75, .65, .55), q97.5 = c(.95, .85, .75, .65)),
    "mlumr_prediction", ptype = "survival", family = "survival")
  expect_error(plot(pred), "cannot be drawn as separate curves")
})

test_that("the prior/posterior window shows the prior, not just the posterior", {
  skip_if_not_installed("ggplot2")
  # A posterior far narrower than its prior defined a window that omitted
  # almost all the prior mass, drawing it as an almost flat line.
  fit <- structure(
    list(
      family = "binomial", model = "spfa", link = "logit",
      data = list(covariates = "age"),
      draws = data.frame(mu_index = stats::rnorm(400, 0, 0.02)),
      priors = list(intercept = prior_normal(0, 10))
    ),
    class = "mlumr_fit"
  )
  pd <- plot_prior_posterior(fit, pars = "mu_index")$layers[[2]]$data
  expect_gt(max(pd$value), 5)
  expect_lt(min(pd$value), -5)
})

test_that("a single-quantile summary plots as points", {
  skip_if_not_installed("ggplot2")
  # marginal_effects(probs = 0.5) is a valid summary with no interval. The
  # other plot methods already degraded to points; this one errored.
  me <- mlumr:::.mlumr_result(
    data.frame(variable = "lor_index", effect = "LOR", population = "Index",
               mean = 0.3, sd = 0.1, q50 = 0.29),
    "mlumr_marginal_effects", family = "binomial")
  p <- plot(me)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_match(p$labels$x, "point estimate", fixed = TRUE)
})

test_that("a constrained prior below its bound still widens the window", {
  skip_if_not_installed("ggplot2")
  # Clamping the unconditional quantiles reversed the range when both fell
  # below the bound, so the grid never widened to show the prior.
  r <- mlumr:::.prior_quantile_range(prior_normal(-5, 1), lower = 0)
  expect_lte(r[1], r[2])
  expect_gte(r[1], 0)
  expect_true(all(is.finite(r)))
  # Unconstrained priors are unchanged.
  r2 <- mlumr:::.prior_quantile_range(prior_normal(0, 10), lower = -Inf)
  expect_lt(r2[1], 0)
  expect_gt(r2[2], 0)
})

test_that("the KM layer stays two curves without a population facet", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("survival")
  dat <- sim_survival_data(seed = 2026, n_ipd = 60, n_agd = 60, n_int = 8)
  dat$comparator_treatment <- dat$index_treatment
  # Colour is the treatment label, so with one label both fitted curves fell
  # into a single ggplot2 group and geom_step() joined their interleaved
  # points. Faceting by population hid it; a bare plot did not.
  p <- ggplot2::ggplot() + geom_km(dat)
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(length(unique(built$group)), 2L)
})
