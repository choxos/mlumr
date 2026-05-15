test_that("set_ipd validates inputs", {
  df <- data.frame(trt = "A", outcome = c(0, 1, 1, 0), x1 = rnorm(4))

  ipd <- set_ipd(df, treatment = "trt", outcome = "outcome",
                 covariates = "x1")
  expect_s3_class(ipd, "mlumr_ipd")
  expect_equal(ipd$n, 4)
  expect_equal(ipd$n_events, 2)
  expect_equal(ipd$family, "binomial")

  # Missing column
  expect_error(set_ipd(df, "trt", "outcome", "nonexistent"), "Missing columns")

  # Non-binary outcome
  df_bad <- data.frame(trt = "A", outcome = c(0, 1, 2, 3), x1 = rnorm(4))
  expect_error(set_ipd(df_bad, "trt", "outcome", "x1"), "binary")
})


test_that("set_ipd rejects unsafe binomial outcome encodings", {
  df_factor <- data.frame(trt = "A", outcome = factor(c(0, 1, 1, 0)),
                          x1 = rnorm(4))
  expect_error(
    set_ipd(df_factor, "trt", "outcome", "x1"),
    "numeric or logical"
  )

  df_logical <- data.frame(trt = "A", outcome = c(TRUE, FALSE),
                           x1 = c(0, 1))
  ipd <- set_ipd(df_logical, "trt", "outcome", "x1")
  expect_equal(ipd$data$.outcome, c(1L, 0L))
})


test_that("set_ipd requires non-empty unique covariates", {
  df <- data.frame(trt = "A", outcome = c(0, 1), x1 = c(0, 1))

  expect_error(
    set_ipd(df, "trt", "outcome", character(0)),
    "non-empty character vector"
  )
  expect_error(
    set_ipd(df, "trt", "outcome", c("x1", "x1")),
    "Duplicate covariates"
  )
})


test_that("set_ipd drops missing treatment rows before treatment validation", {
  df <- data.frame(trt = c("A", NA, "A"), outcome = c(0, 1, 1),
                   x1 = c(0, 1, 1))

  expect_warning(
    ipd <- set_ipd(df, "trt", "outcome", "x1"),
    "missing values"
  )
  expect_equal(ipd$n, 2)
  expect_equal(ipd$treatment, "A")
})


test_that("set_ipd warns on zero-variation covariates after filtering", {
  df <- data.frame(trt = "A", outcome = c(0, 1, 0, 1), x1 = c(1, 1, 1, 1))

  expect_warning(
    ipd <- set_ipd(df, "trt", "outcome", "x1"),
    "zero empirical variation"
  )
  expect_s3_class(ipd, "mlumr_ipd")
})


test_that("set_ipd does not warn on varying covariates", {
  df <- data.frame(trt = "A", outcome = c(0, 1, 0, 1), x1 = c(0, 1, 0, 1))

  expect_warning(
    ipd <- set_ipd(df, "trt", "outcome", "x1"),
    NA
  )
  expect_s3_class(ipd, "mlumr_ipd")
})


test_that("set_ipd errors when no complete rows remain", {
  df <- data.frame(trt = "A", outcome = c(NA_real_, NA_real_),
                   x1 = c(NA_real_, NA_real_))

  expect_error(
    suppressWarnings(set_ipd(df, "trt", "outcome", "x1", family = "normal")),
    "No complete IPD rows remain"
  )
})


test_that("set_agd validates inputs", {
  df <- data.frame(trt = "B", n_total = 200, n_events = 60, x1_mean = 0.3)

  agd <- set_agd(df, treatment = "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  expect_s3_class(agd, "mlumr_agd")
  expect_equal(agd$covariates, "x1")
  expect_equal(agd$family, "binomial")

  # Missing columns
  expect_error(
    set_agd(df, "trt", outcome_n = "n_total", outcome_r = "missing_col",
            cov_means = "x1_mean"),
    "Missing columns"
  )
})


test_that("set_agd rejects non-integer aggregate counts", {
  df_n <- data.frame(trt = "B", n_total = 200.5, n_events = 60,
                     x1_mean = 0.3)
  expect_error(
    set_agd(df_n, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean"),
    "integer sample sizes"
  )

  df_r <- data.frame(trt = "B", n_total = 200, n_events = 60.5,
                     x1_mean = 0.3)
  expect_error(
    set_agd(df_r, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean"),
    "integer counts"
  )

  df_p <- data.frame(trt = "B", n_events = 60.5, pyears = 100,
                     x1_mean = 0.3)
  expect_error(
    set_agd(df_p, "trt", family = "poisson", outcome_r = "n_events",
            outcome_E = "pyears", cov_means = "x1_mean"),
    "integer counts"
  )
})


test_that("set_agd requires non-empty covariate summaries", {
  df <- data.frame(trt = "B", n_total = 200, n_events = 60, x1_mean = 0.3)

  expect_error(
    set_agd(df, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = character(0)),
    "non-empty character vector"
  )
})


test_that("combine_data validates and combines", {
  ipd_df <- data.frame(trt = "A", outcome = c(0, 1, 1, 0),
                       x1 = c(1, 0, 1, 0))
  agd_df <- data.frame(trt = "B", n_total = 200, n_events = 60,
                       x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")

  dat <- combine_data(ipd, agd)
  expect_s3_class(dat, "mlumr_data")
  expect_equal(dat$n_covariates, 1)
  expect_equal(dat$index_treatment, "A")
  expect_equal(dat$comparator_treatment, "B")
  expect_equal(dat$family, "binomial")
  expect_false(dat$has_integration)

  # Wrong class
  expect_error(combine_data("not_ipd", agd), "set_ipd")
  expect_error(combine_data(ipd, "not_agd"), "set_agd")
})


test_that("set_agd handles multi-row AgD", {
  agd_df <- data.frame(
    trt = c("B", "B"),
    n_total = c(100, 200),
    n_events = c(40, 80),
    x1_mean = c(0.3, 0.5)
  )

  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  expect_s3_class(agd, "mlumr_agd")
  expect_equal(nrow(agd$data), 2)
})


test_that("set_ipd with study column", {
  df <- data.frame(trt = "A", outcome = c(0, 1, 1, 0), x1 = rnorm(4),
                   study = c("S1", "S1", "S2", "S2"))

  ipd <- set_ipd(df, "trt", "outcome", "x1", study = "study")
  expect_s3_class(ipd, "mlumr_ipd")
  expect_true(".study" %in% names(ipd$data))
  expect_equal(unique(ipd$data$.study), c("S1", "S2"))
})


test_that("set_ipd warns about missing values", {
  df <- data.frame(trt = "A", outcome = c(0, 1, NA, 0), x1 = rnorm(4))
  expect_warning(set_ipd(df, "trt", "outcome", "x1"), "missing values")
})


test_that("set_ipd rejects non-finite covariates and outcomes", {
  # Infinite covariate
  df_inf <- data.frame(trt = "A", outcome = c(0, 1, 1, 0),
                       x1 = c(1, Inf, 0, 1))
  expect_error(set_ipd(df_inf, "trt", "outcome", "x1"), "non-finite")

  # -Inf covariate
  df_neginf <- data.frame(trt = "A", outcome = c(0, 1, 1, 0),
                          x1 = c(1, -Inf, 0, 1))
  expect_error(set_ipd(df_neginf, "trt", "outcome", "x1"), "non-finite")

  # Infinite Poisson exposure
  df_exp <- data.frame(trt = "A", events = c(1L, 0L, 2L, 1L),
                       pyears = c(1.5, Inf, 1.0, 3.0), x1 = rnorm(4))
  expect_error(
    set_ipd(df_exp, "trt", "events", "x1", family = "poisson",
            exposure = "pyears"),
    "non-finite"
  )
})


test_that("set_agd rejects NA and non-finite outcomes", {
  # NA in binomial outcome_r
  df_na <- data.frame(trt = "B", n_total = 200, n_events = NA, x1_mean = 0.3)
  expect_error(
    set_agd(df_na, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean"),
    "must not contain NA"
  )

  # NA in normal outcome_se
  df_na_se <- data.frame(trt = "B", y_mean = 2.0, se = NA, x1_mean = 0.3)
  expect_error(
    set_agd(df_na_se, "trt", family = "normal", outcome_mean = "y_mean",
            outcome_se = "se", cov_means = "x1_mean"),
    "must not contain NA"
  )

  # NA in Poisson outcome_E
  df_na_E <- data.frame(trt = "B", n_events = 50, pyears = NA, x1_mean = 0.4)
  expect_error(
    set_agd(df_na_E, "trt", family = "poisson", outcome_r = "n_events",
            outcome_E = "pyears", cov_means = "x1_mean"),
    "must not contain NA"
  )

  # Infinite covariate mean
  df_inf <- data.frame(trt = "B", n_total = 200, n_events = 60, x1_mean = Inf)
  expect_error(
    set_agd(df_inf, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean"),
    "non-finite"
  )

  # Negative covariate SD
  df_neg_sd <- data.frame(trt = "B", n_total = 200, n_events = 60,
                          x1_mean = 0.3, x1_sd = -2)
  expect_error(
    set_agd(df_neg_sd, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean", cov_sds = "x1_sd"),
    "non-negative"
  )
})


test_that("set_agd validates binary covariate domains", {
  # Binary mean > 1
  df_hi <- data.frame(trt = "B", n_total = 200, n_events = 60, x1_mean = 1.2)
  expect_error(
    set_agd(df_hi, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean", cov_types = "binary"),
    "Binary covariate.*must be in \\[0, 1\\]"
  )

  # Binary mean < 0
  df_lo <- data.frame(trt = "B", n_total = 200, n_events = 60, x1_mean = -0.1)
  expect_error(
    set_agd(df_lo, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean", cov_types = "binary"),
    "Binary covariate.*must be in \\[0, 1\\]"
  )

  # Impossible binary SD: p=0.9, max SD = sqrt(0.09) ~ 0.3, but SD=2
  df_sd <- data.frame(trt = "B", n_total = 200, n_events = 60,
                      x1_mean = 0.9, x1_sd = 2)
  expect_error(
    set_agd(df_sd, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean", cov_sds = "x1_sd", cov_types = "binary"),
    "SD exceeds Bernoulli maximum"
  )

  # Invalid cov_types value
  df_ok <- data.frame(trt = "B", n_total = 200, n_events = 60, x1_mean = 0.5)
  expect_error(
    set_agd(df_ok, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean", cov_types = "ordinal"),
    "Invalid cov_types"
  )

  # Valid binary mean without SD passes
  expect_no_error(
    set_agd(df_ok, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean", cov_types = "binary")
  )
})


test_that("print.mlumr_data works", {
  ipd_df <- data.frame(trt = "A", outcome = c(0, 1, 1, 0), x1 = c(1, 0, 1, 0))
  agd_df <- data.frame(trt = "B", n_total = 200, n_events = 60, x1_mean = 0.3)
  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_output(print(dat), "Unanchored Comparison")
  expect_output(print(dat), "add_integration")
})


# ---- Normal family tests ----

test_that("set_ipd works with normal family", {
  df <- data.frame(trt = "A", score = c(2.1, 3.5, 1.8, 4.2), x1 = rnorm(4))

  ipd <- set_ipd(df, "trt", "score", "x1", family = "normal")
  expect_s3_class(ipd, "mlumr_ipd")
  expect_equal(ipd$family, "normal")
  expect_equal(ipd$n, 4)
  expect_equal(ipd$mean_outcome, mean(c(2.1, 3.5, 1.8, 4.2)))
  expect_true(!is.null(ipd$sd_outcome))
  expect_true(is.numeric(ipd$data$.outcome))

  # Non-numeric outcome should error
  df_bad <- data.frame(trt = "A", score = c("a", "b", "c", "d"), x1 = rnorm(4))
  expect_error(set_ipd(df_bad, "trt", "score", "x1", family = "normal"), "numeric")
})


test_that("set_agd works with normal family", {
  df <- data.frame(trt = "B", y_mean = 5.2, se = 0.3, n = 200, x1_mean = 0.5)

  agd <- set_agd(df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 outcome_n = "n", cov_means = "x1_mean")
  expect_s3_class(agd, "mlumr_agd")
  expect_equal(agd$family, "normal")
  expect_equal(agd$data$.y, 5.2)
  expect_equal(agd$data$.se, 0.3)
  expect_equal(agd$data$.n, 200)

  # Missing required columns
  expect_error(
    set_agd(df, "trt", family = "normal", cov_means = "x1_mean"),
    "outcome_mean.*outcome_se.*required"
  )

  df_bad_n <- data.frame(trt = "B", y_mean = 5.2, se = 0.3, n = 200.5,
                         x1_mean = 0.5)
  expect_error(
    set_agd(df_bad_n, "trt", family = "normal",
            outcome_mean = "y_mean", outcome_se = "se",
            outcome_n = "n", cov_means = "x1_mean"),
    "integer sample sizes"
  )
})


test_that("combine_data with normal family", {
  ipd_df <- data.frame(trt = "A", score = rnorm(10), x1 = rnorm(10))
  agd_df <- data.frame(trt = "B", y_mean = 2.0, se = 0.5, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "score", "x1", family = "normal")
  agd <- set_agd(agd_df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 cov_means = "x1_mean")

  dat <- combine_data(ipd, agd)
  expect_equal(dat$family, "normal")
})


test_that("combine_data rejects family mismatch", {
  ipd_df <- data.frame(trt = "A", outcome = c(0, 1, 1, 0), x1 = rnorm(4))
  agd_df <- data.frame(trt = "B", y_mean = 2.0, se = 0.5, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1", family = "binomial")
  agd <- set_agd(agd_df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 cov_means = "x1_mean")

  expect_error(combine_data(ipd, agd), "Family mismatch")
})


test_that("print.mlumr_data works for normal family", {
  ipd_df <- data.frame(trt = "A", score = rnorm(10), x1 = rnorm(10))
  agd_df <- data.frame(trt = "B", y_mean = 2.0, se = 0.5, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "score", "x1", family = "normal")
  agd <- set_agd(agd_df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_output(print(dat), "Continuous")
  expect_output(print(dat), "Mean outcome")
})


# ---- Poisson family tests ----

test_that("set_ipd works with poisson family", {
  df <- data.frame(trt = "A", events = c(2L, 0L, 3L, 1L),
                   pyears = c(1.5, 2.0, 1.0, 3.0), x1 = rnorm(4))

  ipd <- set_ipd(df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  expect_s3_class(ipd, "mlumr_ipd")
  expect_equal(ipd$family, "poisson")
  expect_equal(ipd$total_events, 6L)
  expect_equal(ipd$total_exposure, 7.5)
  expect_true(".exposure" %in% names(ipd$data))

  # Missing exposure
  expect_error(
    set_ipd(df, "trt", "events", "x1", family = "poisson"),
    "exposure.*required"
  )

  # Negative counts
  df_bad <- data.frame(trt = "A", events = c(-1L, 0L, 3L, 1L),
                       pyears = c(1.5, 2.0, 1.0, 3.0), x1 = rnorm(4))
  expect_error(
    set_ipd(df_bad, "trt", "events", "x1", family = "poisson",
            exposure = "pyears"),
    "non-negative"
  )
})


test_that("set_agd works with poisson family", {
  df <- data.frame(trt = "B", n_events = 50, pyears = 500, x1_mean = 0.4)

  agd <- set_agd(df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  expect_s3_class(agd, "mlumr_agd")
  expect_equal(agd$family, "poisson")
  expect_equal(agd$data$.r, 50)
  expect_equal(agd$data$.E, 500)

  # Missing required
  expect_error(
    set_agd(df, "trt", family = "poisson", cov_means = "x1_mean"),
    "outcome_r.*outcome_E.*required"
  )
})


test_that("combine_data with poisson family", {
  ipd_df <- data.frame(trt = "A", events = c(2L, 0L, 3L, 1L),
                       pyears = c(1.5, 2.0, 1.0, 3.0), x1 = rnorm(4))
  agd_df <- data.frame(trt = "B", n_events = 50, pyears = 500, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")

  dat <- combine_data(ipd, agd)
  expect_equal(dat$family, "poisson")
})


test_that("print.mlumr_data works for poisson family", {
  ipd_df <- data.frame(trt = "A", events = c(2L, 0L, 3L, 1L),
                       pyears = c(1.5, 2.0, 1.0, 3.0), x1 = rnorm(4))
  agd_df <- data.frame(trt = "B", n_events = 50, pyears = 500, x1_mean = 0.4)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_output(print(dat), "Count")
  expect_output(print(dat), "Total events")
})


test_that("set_ipd rejects covariate names that collide with internal columns", {
  df <- data.frame(trt = "A", outcome = c(0, 1, 1, 0),
                   .outcome = rnorm(4), .trt = rnorm(4),
                   .study = rnorm(4), .exposure = rnorm(4))

  for (bad in c(".outcome", ".trt", ".study", ".exposure")) {
    expect_error(
      set_ipd(df, "trt", "outcome", covariates = bad),
      "reserved internal columns"
    )
  }
})


test_that("set_agd rejects mean/SD column names that collide with internal columns", {
  df <- data.frame(trt = "B", n_total = 200, n_events = 60,
                   .r = 0.3, .n = 0.3, .y = 0.3, .se = 0.1,
                   .study = 0.1, .trt = 0.1)

  for (bad in c(".r", ".n", ".y", ".se", ".study", ".trt")) {
    expect_error(
      set_agd(df, "trt", outcome_n = "n_total", outcome_r = "n_events",
              cov_means = bad),
      "reserved internal columns"
    )
  }
})


test_that("set_agd rejects duplicate cov_names after suffix-stripping", {
  df <- data.frame(trt = "B", n_total = 200, n_events = 60,
                   age_mean = 55, age = 55)

  # "age_mean" strips to "age"; "age" stays "age" -> duplicate
  expect_error(
    set_agd(df, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = c("age_mean", "age")),
    "collision after stripping"
  )
})


test_that("set_ipd accepts ordinary covariate names (sanity check)", {
  df <- data.frame(trt = "A", outcome = c(0, 1, 1, 0),
                   age = c(50, 60, 55, 65), bmi = c(22, 25, 27, 30))
  expect_s3_class(
    set_ipd(df, "trt", "outcome", covariates = c("age", "bmi")),
    "mlumr_ipd"
  )
})
