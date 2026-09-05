# Pure-R tests for calculate_loo(), calculate_waic(), and mixed-criterion
# compare_models(). No Stan compilation: we construct mock mlumr_fit
# objects whose $draws carries the vector-indexed `log_lik_ipd[i]` and
# `log_lik_agd[i]` columns that all ten shipping Stan models emit.

make_ll_fit <- function(model = "spfa", n_draws = 400,
                        n_ipd = 12L, n_agd = 4L, seed = 2026) {
  set.seed(seed)
  ll_ipd <- matrix(rnorm(n_draws * n_ipd, -5, 0.5), nrow = n_draws)
  ll_agd <- matrix(rnorm(n_draws * n_agd, -6, 0.5), nrow = n_draws)
  colnames(ll_ipd) <- sprintf("log_lik_ipd[%d]", seq_len(n_ipd))
  colnames(ll_agd) <- sprintf("log_lik_agd[%d]", seq_len(n_agd))

  draws <- data.frame(
    ll_ipd, ll_agd,
    lor_index = rnorm(n_draws, 0.5, 0.2),
    lor_comparator = rnorm(n_draws, 0.4, 0.2),
    check.names = FALSE
  )

  out <- list(
    draws = draws,
    summary = data.frame(
      variable = c("mu_index", "lor_index"),
      mean = c(-0.5, 0.5), sd = c(0.1, 0.2),
      Rhat = c(1.001, 1.002), n_eff = c(800, 700),
      stringsAsFactors = FALSE
    ),
    model = model,
    diagnostics = list(n_divergent = 0, n_max_treedepth = 0),
    sampling_args = list(adapt_delta = 0.95, max_treedepth = 15, chains = 4)
  )
  class(out) <- c("mlumr_fit", "list")
  out
}


# extract_log_lik is internal but we can probe it via mlumr:::
test_that("extract_log_lik orders columns and returns a numeric matrix", {
  fit <- make_ll_fit(n_ipd = 5L, n_agd = 2L)
  ll <- mlumr:::extract_log_lik(fit)
  expect_true(is.matrix(ll))
  expect_equal(dim(ll), c(400L, 7L))
  # The last IPD column (index 5) should precede the first AgD column
  expect_equal(colnames(ll)[5], "log_lik_ipd[5]")
  expect_equal(colnames(ll)[6], "log_lik_agd[1]")
})


test_that("extract_log_lik rejects fits with no pointwise columns", {
  fit <- make_ll_fit()
  fit$draws <- fit$draws[, c("lor_index", "lor_comparator")]
  expect_error(mlumr:::extract_log_lik(fit),
               "Pointwise log-likelihood columns not found")
})


test_that("calculate_loo returns a psis_loo object", {
  skip_if_not_installed("loo")
  fit <- make_ll_fit()
  result <- suppressWarnings(calculate_loo(fit))
  expect_s3_class(result, "psis_loo")
  expect_s3_class(result, "loo")
  # The `loo` object must expose `elpd_loo`, p_loo, looic
  expect_true(all(c("elpd_loo", "p_loo", "looic") %in%
                    rownames(result$estimates)))
})


test_that("calculate_loo uses stable relative efficiency for tiny likelihoods", {
  skip_if_not_installed("loo")
  fit <- make_ll_fit()
  ll_cols <- grep("^log_lik_", names(fit$draws), value = TRUE)
  fit$draws[, ll_cols] <- fit$draws[, ll_cols] - 10000

  result <- suppressWarnings(calculate_loo(fit))

  expect_s3_class(result, "psis_loo")
})


test_that("calculate_waic returns a waic object", {
  skip_if_not_installed("loo")
  fit <- make_ll_fit()
  result <- suppressWarnings(calculate_waic(fit))
  expect_s3_class(result, "waic")
  expect_s3_class(result, "loo")
  expect_true(all(c("elpd_waic", "p_waic", "waic") %in%
                    rownames(result$estimates)))
})


test_that("calculate_loo errors informatively without pointwise log_lik", {
  fit <- make_ll_fit()
  fit$draws <- fit$draws[, c("lor_index", "lor_comparator")]
  expect_error(calculate_loo(fit),
               "Pointwise log-likelihood columns not found")
})


test_that("compare_models with criterion = 'loo' returns a compare.loo", {
  skip_if_not_installed("loo")
  fit1 <- make_ll_fit("spfa", seed = 2026)
  fit2 <- make_ll_fit("relaxed", seed = 2026)
  out <- suppressWarnings(
    capture.output(cmp <- compare_models(fit1, fit2, criterion = "loo"))
  )
  expect_s3_class(cmp, "compare.loo")
  # Two rows, one per model
  expect_equal(nrow(cmp), 2L)
  expect_true(any(grepl("Model Comparison \\(LOO\\)", out)))
})


test_that("compare_models with criterion = 'waic' returns a compare.loo", {
  skip_if_not_installed("loo")
  fit1 <- make_ll_fit("spfa", seed = 2026)
  fit2 <- make_ll_fit("relaxed", seed = 2026)
  out <- suppressWarnings(
    capture.output(cmp <- compare_models(fit1, fit2, criterion = "waic"))
  )
  expect_s3_class(cmp, "compare.loo")
  expect_equal(nrow(cmp), 2L)
})


test_that("compare_models rejects mixed mlumr_fit + mlumr_dic for LOO/WAIC", {
  skip_if_not_installed("loo")
  fit <- make_ll_fit("spfa")
  dic <- calculate_dic(fit)
  expect_error(
    compare_models(fit, dic, criterion = "loo"),
    "all arguments must be mlumr_fit"
  )
})


test_that(".chain_id recovers chain ids in chain-major order", {
  fit <- make_ll_fit(n_draws = 400)  # chains = 4 -> 100 per chain
  ids <- mlumr:::.chain_id(fit)
  expect_equal(length(ids), 400L)
  expect_equal(sort(unique(ids)), 1:4)
  # First and last 100 draws belong to different chains
  expect_equal(ids[1], 1L)
  expect_equal(ids[400], 4L)
})

test_that("survival LOO/WAIC can group the comparator pseudo-IPD by arm/aggregate", {
  glb <- mlumr:::.survival_log_lik_by_unit
  draws <- data.frame(
    `log_lik_ipd[1]` = c(-1, -2), `log_lik_ipd[2]` = c(-1.5, -2.5),
    `log_lik_agd[1]` = c(-0.1, -0.2), `log_lik_agd[2]` = c(-0.3, -0.4),
    `log_lik_agd[3]` = c(-0.5, -0.6), `log_lik_agd[4]` = c(-0.7, -0.8),
    check.names = FALSE
  )
  obj <- list(family = "survival", draws = draws,
              stan_data = list(agd_arm = c(1L, 1L, 2L, 2L)))
  arm <- glb(obj, "arm")
  expect_equal(ncol(arm), 4L)
  expect_equal(unname(arm[, 3]), c(-0.1 - 0.3, -0.2 - 0.4))
  expect_equal(unname(arm[, 4]), c(-0.5 - 0.7, -0.6 - 0.8))
  agg <- glb(obj, "aggregate")
  expect_equal(ncol(agg), 3L)
  expect_equal(unname(agg[, 3]), c(-1.6, -2.0))
  bad <- obj
  bad$stan_data$agd_arm <- c(1L, 2L)
  expect_error(glb(bad, "arm"), "arm map")
})


# Pointwise criteria are paired: loo_compare() differences the matrices column
# by column, and loo can only check that they have the same shape. Two fits of
# different data with the same number of rows, or the same rows in another
# order, compared without complaint. Fits carry their data, so identity is
# checked, and a fit that carries none is reported rather than assumed.

with_data <- function(fit, outcome, agd_r = c(3L, 5L, 2L, 4L)) {
  fit$data <- list(
    ipd = list(data = data.frame(.study = "A", .trt = rep(c("x", "y"), 6L),
                                 .outcome = outcome, age = seq_along(outcome))),
    agd = list(data = data.frame(.study = "B", .trt = "z", .r = agd_r,
                                 .n = 10L, age_mean = 50))
  )
  fit
}

test_that("compare_models refuses fits built on different observations", {
  skip_if_not_installed("loo")
  y <- rep(c(0L, 1L), 6L)
  fit1 <- with_data(make_ll_fit("spfa", seed = 2026), y)
  fit2 <- with_data(make_ll_fit("relaxed", seed = 2026), y)
  # Same observations, different covariate sets: comparable.
  fit2$data$ipd$data$age <- NULL
  out <- suppressWarnings(capture.output(
    compare_models(fit1, fit2, criterion = "loo")
  ))
  expect_true(length(out) > 0L)
  # Same length, different outcomes.
  fit3 <- with_data(make_ll_fit("relaxed", seed = 2026), rev(y))
  fit3$data$ipd$data$.outcome[1] <- 1L - fit3$data$ipd$data$.outcome[1]
  expect_error(compare_models(fit1, fit3, criterion = "loo"),
               "not built on the same observations")
  # Same rows, different order.
  fit4 <- with_data(make_ll_fit("relaxed", seed = 2026), y)
  fit4$data$ipd$data <- fit4$data$ipd$data[c(2:12, 1), ]
  expect_error(compare_models(fit1, fit4, criterion = "dic"),
               "not built on the same observations")
  # A different aggregate row set is a different data set too.
  fit5 <- with_data(make_ll_fit("relaxed", seed = 2026), y, agd_r = c(3L, 5L, 2L, 5L))
  expect_error(compare_models(fit1, fit5, criterion = "waic"),
               "not built on the same observations")
})

test_that("the observation check compares values, not their representation", {
  skip_if_not_installed("loo")
  y <- rep(c(0L, 1L), 6L)
  fit1 <- with_data(make_ll_fit("spfa", seed = 2026), y)
  # The same treatments coded as a factor with an unused level, the same
  # outcomes as doubles, the same counts read as doubles: one data set.
  fit2 <- with_data(make_ll_fit("relaxed", seed = 2026), as.numeric(y),
                    agd_r = c(3, 5, 2, 4))
  fit2$data$ipd$data$.trt <- factor(fit2$data$ipd$data$.trt,
                                    levels = c("y", "x", "unused"))
  fit2$data$agd$data$.n <- 10
  # Only the internal names are reserved, so a covariate may begin with a dot;
  # one that appears in a single model is a covariate, not an observation.
  fit2$data$ipd$data$.age <- seq_len(12L)
  # Counts are accepted within rounding tolerance and rounded before Stan
  # sees them; the same rounding applies here.
  fit2$data$agd$data$.r[1] <- 2.9999999999
  expect_true(.same_observations(.observation_frames(fit1),
                                 .observation_frames(fit2)))
  out <- suppressWarnings(capture.output(
    compare_models(fit1, fit2, criterion = "loo")
  ))
  expect_true(length(out) > 0L)
})

test_that("a row swap is a mismatch exactly when a shared covariate moves", {
  skip_if_not_installed("loo")
  # Rows 1 and 3 have the same study, treatment and outcome and differ only
  # in age. Swapping them leaves every observation column unchanged.
  y <- rep(c(0L, 1L), 6L)
  fit1 <- with_data(make_ll_fit("spfa", seed = 2026), y)
  fit2 <- with_data(make_ll_fit("relaxed", seed = 2026), y)
  fit2$data$ipd$data <- fit2$data$ipd$data[c(3L, 2L, 1L, 4:12), ]
  # Both models use age, so their pointwise likelihoods for the two rows are
  # different numbers, and the swap pairs each with the wrong one.
  expect_error(compare_models(fit1, fit2, criterion = "loo"),
               "not built on the same observations")
  # One model does not use age: for it the two rows are exchangeable, its
  # pointwise likelihood is the same for both, and either pairing gives the
  # same comparison.
  fit2$data$ipd$data$age <- NULL
  expect_true(.same_observations(.observation_frames(fit1),
                                 .observation_frames(fit2)))
  out <- suppressWarnings(capture.output(
    compare_models(fit1, fit2, criterion = "loo")
  ))
  expect_true(length(out) > 0L)
})

test_that("compare_models says when it cannot verify the observations", {
  skip_if_not_installed("loo")
  fit1 <- make_ll_fit("spfa", seed = 2026)
  fit2 <- make_ll_fit("relaxed", seed = 2026)
  run <- function() {
    suppressWarnings(capture.output(compare_models(fit1, fit2, criterion = "loo")))
  }
  expect_message(run(), "Could not verify")
})
