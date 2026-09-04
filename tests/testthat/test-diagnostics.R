# Test DIC and model comparison using mock fit objects
# (no Stan compilation needed)

make_mock_fit <- function(model = "spfa", n_draws = 100,
                          n_ipd = 10L, n_agd = 3L) {
  set.seed(2026)
  # Stan models produce pointwise log_lik as vectors: log_lik_ipd[1..n_ipd]
  # and log_lik_agd[1..n_agd]. The test mock mirrors that contract so
  # extract_log_lik() / calculate_dic() parse the columns correctly.
  ll_ipd <- matrix(rnorm(n_draws * n_ipd, -5, 0.5), nrow = n_draws)
  ll_agd <- matrix(rnorm(n_draws * n_agd, -6, 0.5), nrow = n_draws)
  colnames(ll_ipd) <- sprintf("log_lik_ipd[%d]", seq_len(n_ipd))
  colnames(ll_agd) <- sprintf("log_lik_agd[%d]", seq_len(n_agd))

  draws <- data.frame(
    ll_ipd,
    ll_agd,
    lor_index = rnorm(n_draws, 0.5, 0.2),
    lor_comparator = rnorm(n_draws, 0.4, 0.2),
    check.names = FALSE
  )

  summary_df <- data.frame(
    variable = c("mu_index", "mu_comparator", "lor_index", "lor_comparator"),
    mean = c(-0.5, -0.8, 0.5, 0.4),
    sd = c(0.1, 0.1, 0.2, 0.2),
    Rhat = c(1.001, 1.002, 1.003, 1.002),
    n_eff = c(800, 750, 600, 650),
    stringsAsFactors = FALSE
  )

  out <- list(
    draws = draws,
    summary = summary_df,
    model = model,
    diagnostics = list(n_divergent = 0, n_max_treedepth = 0),
    sampling_args = list(adapt_delta = 0.95, max_treedepth = 15, chains = 4)
  )
  class(out) <- c("mlumr_fit", "list")
  out
}


test_that("calculate_dic returns correct structure", {
  fit <- make_mock_fit()
  dic <- calculate_dic(fit)

  expect_s3_class(dic, "mlumr_dic")
  expect_true(is.numeric(dic$DIC))
  expect_true(is.numeric(dic$pD))
  expect_true(is.numeric(dic$D_bar))
  expect_true(dic$pD >= 0)
  expect_equal(dic$model, "SPFA")
})

test_that("calculate_dic uses variance-based pD", {
  fit <- make_mock_fit()
  dic <- calculate_dic(fit)

  # Manually compute expected values from the vector-indexed columns
  ipd_cols <- grep("^log_lik_ipd\\[", colnames(fit$draws), value = TRUE)
  agd_cols <- grep("^log_lik_agd\\[", colnames(fit$draws), value = TRUE)
  ll_total <- rowSums(fit$draws[, c(ipd_cols, agd_cols), drop = FALSE])
  D <- -2 * ll_total
  expected_pd <- 0.5 * var(D)
  expected_D_bar <- mean(D)

  expect_equal(dic$pD, expected_pd)
  expect_equal(dic$D_bar, expected_D_bar)
  expect_equal(dic$DIC, expected_D_bar + expected_pd)
})

test_that("extract_log_lik orders shuffled double-digit columns", {
  fit <- make_mock_fit(n_ipd = 12L, n_agd = 2L)
  set.seed(2026)
  fit$draws <- fit$draws[, sample(names(fit$draws)), drop = FALSE]

  ll <- mlumr:::extract_log_lik(fit)

  expect_equal(colnames(ll)[1], "log_lik_ipd[1]")
  expect_equal(colnames(ll)[10], "log_lik_ipd[10]")
  expect_equal(colnames(ll)[13], "log_lik_agd[1]")
})

test_that("extract_log_lik rejects malformed pointwise values", {
  fit <- make_mock_fit()
  fit$draws[1, "log_lik_ipd[1]"] <- NA_real_
  expect_error(mlumr:::extract_log_lik(fit), "finite")

  fit <- make_mock_fit()
  fit$draws[1, "log_lik_ipd[1]"] <- -Inf
  expect_error(mlumr:::extract_log_lik(fit), "finite")
})

test_that("calculate_dic requires at least two posterior draws", {
  fit <- make_mock_fit(n_draws = 1L)
  expect_error(calculate_dic(fit), "at least two posterior draws")
})

test_that("calculate_dic rejects non-mlumr_fit objects", {
  expect_error(calculate_dic(list()), "mlumr_fit")
})

test_that("print.mlumr_dic works", {
  fit <- make_mock_fit()
  dic <- calculate_dic(fit)
  expect_output(print(dic), "DIC for ML-UMR")
  expect_output(print(dic), "SPFA")
})

test_that("compare_models compares two models", {
  fit1 <- make_mock_fit("spfa")
  fit2 <- make_mock_fit("relaxed")

  expect_output(result <- compare_models(fit1, fit2), "Model Comparison")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("Delta_DIC" %in% names(result))
  # Best model should have Delta_DIC = 0
  expect_equal(min(result$Delta_DIC), 0)
})

test_that("compare_models validates model count and criterion", {
  fit1 <- make_mock_fit("spfa")
  fit2 <- make_mock_fit("relaxed")

  expect_error(compare_models(fit1), "at least two models")
  expect_error(compare_models(fit1, fit2, criterion = "l"), "`criterion`")
})

test_that("compare_models uses supplied names when present", {
  fit1 <- make_mock_fit("spfa")
  fit2 <- make_mock_fit("relaxed")

  expect_output(
    result <- compare_models(reference = fit1, alternative = fit2),
    "Model Comparison"
  )
  expect_setequal(result$Model, c("reference", "alternative"))
})

test_that("compare_models accepts mlumr_dic objects", {
  fit1 <- make_mock_fit("spfa")
  dic1 <- calculate_dic(fit1)

  fit2 <- make_mock_fit("relaxed")

  # Mix of fit and dic objects
  expect_output(compare_models(dic1, fit2), "Model Comparison")
})

test_that(".chain_id falls back to one chain when metadata is invalid", {
  fit <- make_mock_fit(n_draws = 40)
  fit$sampling_args$chains <- NA_real_

  ids <- mlumr:::.chain_id(fit)

  expect_equal(length(ids), 40L)
  expect_true(all(ids == 1L))
})

test_that("the tail-ESS check can actually fire", {
  # The guard tests for an `ess_tail` column that neither backend produced, so
  # it was dead for every fit the package made. Drive it directly.
  fit <- structure(
    list(summary = data.frame(variable = c("a", "b"),
                              n_eff = c(1200, 1100),
                              ess_tail = c(1000, 120),
                              Rhat = c(1.001, 1.002)),
         diagnostics = list(n_divergent = 0, n_max_treedepth = 0)),
    class = "mlumr_fit"
  )
  expect_warning(check_diagnostics(fit), "tail-ESS values < 400")

  fit$summary$ess_tail <- c(1000, 900)
  expect_no_warning(check_diagnostics(fit))
})

test_that(".chain_id() prefers stored real chain ids over reconstruction", {
  fake <- list(
    draws = data.frame(a = 1:6),
    chain_ids = c(1L, 1L, 2L, 2L, 3L, 3L),
    sampling_args = list(chains = 2L)
  )
  expect_equal(mlumr:::.chain_id(fake), c(1L, 1L, 2L, 2L, 3L, 3L))

  fake2 <- list(draws = data.frame(a = 1:6), sampling_args = list(chains = 2L))
  expect_equal(mlumr:::.chain_id(fake2), c(1L, 1L, 1L, 2L, 2L, 2L))

  fake3 <- list(draws = data.frame(a = 1:6), chain_ids = c(1L, 2L),
                sampling_args = list(chains = 3L))
  expect_equal(mlumr:::.chain_id(fake3), c(1L, 1L, 2L, 2L, 3L, 3L))
})

test_that("diagnostics refuse a collapsed AgD log-likelihood", {
  # Tie aggregation (staged for a later version) keeps one AgD row per distinct
  # likelihood key and carries the multiplicity in `stan_data$agd_count`. The
  # pointwise `log_lik_agd` is then one UNWEIGHTED value per UNIQUE row, and
  # `stan_data$agd_arm` is the collapsed arm map. Both still agree in length, so
  # nothing downstream errors on its own: LOO/WAIC/DIC would quietly count each
  # tied observation once instead of `agd_count` times. Fail closed instead.
  mk <- function(cnt, n_agd_cols) {
    d <- as.data.frame(
      setNames(as.list(rep(0, n_agd_cols)),
               paste0("log_lik_agd[", seq_len(n_agd_cols), "]")),
      check.names = FALSE)
    d[["log_lik_ipd[1]"]] <- 0
    structure(list(family = "survival",
                   stan_data = list(agd_count = cnt),
                   draws = d),
              class = "mlumr_fit")
  }

  # Un-expanded: one column per unique row while a multiplicity exceeds one.
  expect_error(extract_log_lik(mk(c(2L, 1L, 3L), 3L)), "one value per")
  expect_error(mlumr:::.survival_log_lik_by_unit(mk(c(2L, 1L, 3L), 3L), "arm"),
               "one value per")

  # No-ops in every case that is not a collapse: v0.2.0 fits carry no
  # `agd_count`, an uncollapsed run carries all ones, and an already-expanded
  # matrix has one column per original observation (2 + 1 + 3 = 6).
  expect_silent(mlumr:::.assert_agd_loglik_per_observation(mk(NULL, 5L)))
  expect_silent(mlumr:::.assert_agd_loglik_per_observation(mk(rep(1L, 5), 5L)))
  expect_silent(mlumr:::.assert_agd_loglik_per_observation(mk(c(2L, 1L, 3L), 6L)))
})

test_that("comparator centering weights count collapsed rows by multiplicity", {
  # The weight a comparator row carries is the number of pseudo-individuals it
  # represents, not the number of retained rows. Reading `agd_count` here is
  # what makes the center independent of whether tied rows were collapsed
  # before or after centering; getting it wrong would silently move the center
  # and with it the induced raw-scale intercept prior.
  w <- mlumr:::.agd_center_weights
  base <- list(agd_arm = c(1L, 1L, 2L), n_agd = 3L)
  expect_equal(w(base, "survival", 2L), c(2, 1))
  expect_equal(w(c(base, list(agd_count = c(2L, 1L, 4L))), "survival", 2L),
               c(3, 4))
  # All-ones multiplicities must reproduce the uncollapsed weights exactly.
  expect_equal(w(c(base, list(agd_count = rep(1L, 3))), "survival", 2L),
               w(base, "survival", 2L))
  expect_error(w(c(base, list(agd_count = c(1L, 0L, 2L))), "survival", 2L),
               "positive multiplicity")
})
