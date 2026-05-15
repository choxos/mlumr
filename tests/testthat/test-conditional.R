# Tests for conditional_effects() and conditional_predict()
# These require a fitted model so they are slow.

make_conditional_fit <- function() {
  draws <- data.frame(
    mu_index = c(1000, 1),
    mu_comparator = c(-1000, 0),
    check.names = FALSE
  )
  draws[["beta[1]"]] <- c(0, 0.5)

  structure(
    list(
      draws = draws,
      family = "binomial",
      link = "logit",
      model = "spfa",
      data = list(
        covariates = "x",
        ipd = list(data = data.frame(x = c(0, 2))),
        index_treatment = "A",
        comparator_treatment = "B"
      )
    ),
    class = c("mlumr_fit", "list")
  )
}


test_that("conditional summaries validate arguments and newdata", {
  fit <- make_conditional_fit()

  expect_error(conditional_effects(list()), "`object` must be an mlumr_fit object")
  expect_error(conditional_effects(fit, effect = c("rd", "rr")),
               "`effect` must be a single non-missing string")
  expect_error(conditional_effects(fit, summary = NA),
               "`summary` must be TRUE or FALSE")
  expect_error(conditional_effects(fit, probs = c(0.5, 0.5)),
               "`probs` must be unique")

  expect_error(conditional_predict(fit, type = "r"), "`type` must be one of")
  expect_error(conditional_predict(fit, newdata = list(x = 0)),
               "`newdata` must be a data frame")
  expect_error(conditional_predict(fit, newdata = data.frame(x = numeric(0))),
               "`newdata` must contain at least one row")
  expect_error(conditional_predict(fit, newdata = data.frame(z = 0)),
               "Missing covariates")
  expect_error(conditional_predict(fit, newdata = data.frame(x = "0")),
               "must be numeric")
  expect_error(conditional_predict(fit, newdata = data.frame(x = Inf)),
               "must be finite")

  broken <- fit
  broken$draws[["beta[1]"]] <- NULL
  expect_error(conditional_effects(broken, newdata = data.frame(x = 0)),
               "Missing conditional parameter draw column")
})


test_that("conditional link effects use eta differences directly", {
  fit <- make_conditional_fit()

  effects <- conditional_effects(
    fit,
    newdata = data.frame(x = 0),
    effect = "link_effect",
    summary = FALSE
  )
  expect_equal(effects$link_effect, c(2000, 1))

  lor_alias <- conditional_effects(
    fit,
    newdata = data.frame(x = 0),
    effect = "lor",
    summary = FALSE
  )
  expect_equal(lor_alias$link_effect, c(2000, 1))

  rr <- conditional_effects(
    fit,
    newdata = data.frame(x = 0),
    effect = "rr",
    summary = FALSE
  )
  expect_true(all(is.finite(rr$rr)))

  link_pred <- conditional_predict(
    fit,
    newdata = data.frame(x = 0),
    type = "link",
    summary = FALSE
  )
  expect_equal(link_pred$index, c(1000, 1))
  expect_equal(link_pred$comparator, c(-1000, 0))
})


test_that("conditional_effects works with default and custom newdata", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(42)
  n <- 80
  x1 <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.3 + 0.8 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(trt = "B", n_total = 200, n_events = 70, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  fit <- mlumr(dat, model = "spfa", chains = 2, iter = 500,
               warmup = 250, refresh = 0, seed = 123)

  # Default newdata (IPD means)
  ce <- conditional_effects(fit)
  expect_s3_class(ce, "data.frame")
  expect_true(nrow(ce) > 0)
  expect_true(all(c("profile", "effect", "mean", "sd") %in% names(ce)))

  # Custom newdata
  profiles <- data.frame(x1 = c(0, 1))
  ce2 <- conditional_effects(fit, newdata = profiles)
  expect_s3_class(ce2, "data.frame")
  expect_true(nrow(ce2) >= 2)

  # summary = FALSE returns draws
  ce_draws <- conditional_effects(fit, summary = FALSE)
  expect_s3_class(ce_draws, "data.frame")
  expect_true("profile" %in% names(ce_draws))
  expect_true(nrow(ce_draws) > nrow(ce))
})


test_that("conditional_predict works with response and link types", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  set.seed(42)
  n <- 80
  x1 <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.3 + 0.8 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(trt = "B", n_total = 200, n_events = 70, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  fit <- mlumr(dat, model = "spfa", chains = 2, iter = 500,
               warmup = 250, refresh = 0, seed = 123)

  # Response type (default)
  cp <- conditional_predict(fit, type = "response")
  expect_s3_class(cp, "data.frame")
  expect_true(nrow(cp) > 0)
  expect_true(all(c("treatment", "mean") %in% names(cp)))

  # Link type
  cp_link <- conditional_predict(fit, type = "link")
  expect_s3_class(cp_link, "data.frame")
  expect_true(nrow(cp_link) > 0)

  # Custom newdata
  profiles <- data.frame(x1 = c(0, 1))
  cp2 <- conditional_predict(fit, newdata = profiles)
  expect_true(nrow(cp2) >= 4)  # 2 profiles x 2 treatments

  # summary = FALSE
  cp_draws <- conditional_predict(fit, summary = FALSE)
  expect_s3_class(cp_draws, "data.frame")
  expect_true(nrow(cp_draws) > nrow(cp))
})
