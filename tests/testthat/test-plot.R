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
