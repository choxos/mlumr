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

test_that("survival comparators are compared on both their frames", {
  skip_if_not_installed("loo")
  # A survival comparator stores its covariate summaries on the aggregate
  # rows and its times and status on the reconstructed pseudo-individuals.
  # Either changing is a different comparator likelihood.
  y <- rep(c(0L, 1L), 6L)
  surv <- function() {
    fit <- with_data(make_ll_fit("spfa", seed = 2026), y)
    fit$data$agd$data <- data.frame(.study = "B", .trt = "z", .arm = "B",
                                    age_mean = 50, age_sd = 8)
    fit$data$agd$pseudo_ipd <- data.frame(.study = "B", .trt = "z", .arm = "B",
                                          .time = c(3, 5, 8), .status = c(1L, 0L, 1L))
    fit
  }
  fit1 <- surv()
  fit2 <- surv()
  expect_true(.same_observations(.observation_frames(fit1),
                                 .observation_frames(fit2)))
  fit2$data$agd$data$age_mean <- 70
  expect_false(.same_observations(.observation_frames(fit1),
                                  .observation_frames(fit2)))
  fit3 <- surv()
  fit3$data$agd$pseudo_ipd$.status[2] <- 1L
  expect_false(.same_observations(.observation_frames(fit1),
                                  .observation_frames(fit3)))
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

test_that("a reordered source is caught even when the fits share no covariate", {
  skip_if_not_installed("loo")
  # Rows 1 and 3 agree on treatment and outcome and differ in age and sex.
  # A model on age and a model on sex share no covariate, so after a swap
  # nothing in the stored columns can show that both models' pointwise
  # likelihoods moved. The key the setup functions record from the whole
  # source row can.
  src <- data.frame(trt = "x", y = rep(c(0L, 1L), 6L),
                    age = c(30, 41, 52, 63, 74, 85, 36, 47, 58, 69, 70, 81),
                    sex = c(1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L),
                    stringsAsFactors = FALSE)
  agd <- data.frame(trt = "z", n = 10L, r = c(3L, 5L, 2L, 4L),
                    age_mean = 50, age_sd = 8, sex_prop = 0.5)
  from <- function(model, ipd, covs, agd_covs, agd_sds = NULL) {
    fit <- make_ll_fit(model, seed = 2026)
    fit$data <- list(
      ipd = suppressWarnings(set_ipd(ipd, treatment = "trt", outcome = "y",
                                     covariates = covs, family = "binomial")),
      agd = set_agd(agd, treatment = "trt", outcome_n = "n", outcome_r = "r",
                    cov_means = agd_covs, cov_sds = agd_sds, family = "binomial")
    )
    fit
  }
  fit_age <- from("spfa", src, "age", c(age = "age_mean"), c(age = "age_sd"))
  fit_sex <- from("relaxed", src[c(3L, 2L, 1L, 4:12), ], "sex", c(sex = "sex_prop"))
  expect_true(all(c(".source_key") %in% names(fit_age$data$ipd$data)))
  expect_error(compare_models(fit_age, fit_sex, criterion = "loo"),
               "not built on the same observations")
  # The same source in the same order compares as the same, whatever the
  # covariate sets.
  fit_sex2 <- from("relaxed", src, "sex", c(sex = "sex_prop"))
  expect_true(.same_observations(.observation_frames(fit_age),
                                 .observation_frames(fit_sex2)))
  # A source that gained a column between the fits gives different keys, and
  # then the keys cannot judge: the decision falls back to the columns.
  src2 <- src
  src2$age_dec <- src2$age / 10
  fit_dec <- from("relaxed", src2, "age_dec", c(age_dec = "age_mean"),
                  c(age_dec = "age_sd"))
  expect_true(.same_observations(.observation_frames(fit_age),
                                 .observation_frames(fit_dec)))
  # Column order in the source is not part of the key.
  fit_cols <- from("relaxed", src[, c("sex", "age", "y", "trt")], "sex",
                   c(sex = "sex_prop"))
  expect_identical(fit_cols$data$ipd$data$.source_key,
                   fit_age$data$ipd$data$.source_key)
  # The key keeps nothing of the source: an unused identifier column changes
  # the digest and appears nowhere in the fit.
  src3 <- src
  src3$patient <- sprintf("Patient %02d", seq_len(nrow(src3)))
  keys <- from("relaxed", src3, "sex", c(sex = "sex_prop"))$data$ipd$data$.source_key
  expect_match(keys, "^[0-9a-f]{32}:[0-9]+$")
  expect_false(any(grepl("Patient", keys, fixed = TRUE)))
  expect_false(identical(keys, fit_age$data$ipd$data$.source_key))
  # Identical rows share a rank, so swapping them is not a reordering.
  src4 <- rbind(src[1, ], src[1, ], src[3:12, ])
  k4 <- .source_row_keys(src4)
  expect_identical(k4[1], k4[2])
  expect_identical(.source_row_keys(src4[c(2L, 1L, 3:12), ]), k4)
})

test_that("every pair of compared fits is checked, not each against the first", {
  # Sharing a covariate is not transitive: A carries none, so A matches both
  # B and C, while B and C share age and differ in it.
  y <- rep(c(0L, 1L), 6L)
  a <- with_data(make_ll_fit("spfa", seed = 2026), y)
  a$data$ipd$data$age <- NULL
  b <- with_data(make_ll_fit("relaxed", seed = 2026), y)
  c3 <- with_data(make_ll_fit("relaxed", seed = 2026), y)
  c3$data$ipd$data$age[1:2] <- c3$data$ipd$data$age[2:1]
  expect_true(.same_observations(.observation_frames(a), .observation_frames(b)))
  expect_true(.same_observations(.observation_frames(a), .observation_frames(c3)))
  expect_error(compare_models(a, b, c3), "not built on the same observations")
})

test_that("a DIC object carries its observations into the check", {
  y <- rep(c(0L, 1L), 6L)
  fit1 <- with_data(make_ll_fit("spfa", seed = 2026), y)
  dic1 <- calculate_dic(fit1)
  expect_true(.same_observations(dic1$observations, .observation_frames(fit1)))
  fit2 <- with_data(make_ll_fit("relaxed", seed = 2026), y)
  expect_no_message(capture.output(compare_models(dic1, fit2)))
  fit3 <- with_data(make_ll_fit("relaxed", seed = 2026), rev(y))
  fit3$data$ipd$data$.outcome[1] <- 1L - fit3$data$ipd$data$.outcome[1]
  expect_error(compare_models(dic1, fit3), "not built on the same observations")
  expect_error(compare_models(dic1, calculate_dic(fit3)),
               "not built on the same observations")
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
