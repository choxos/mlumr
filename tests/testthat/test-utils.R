test_that("qbern returns correct values", {
  expect_equal(qbern(0.3, prob = 0.5), qbinom(0.3, size = 1, prob = 0.5))
  expect_equal(qbern(0, prob = 0.5), 0L)
  expect_equal(qbern(1, prob = 0.5), 1L)
  # P(X <= 0) = 1 - prob = 0.8, so qbern(0.7, 0.2) = 0 (0.7 < 0.8)
  expect_equal(qbern(0.7, prob = 0.2), 0L)
  # qbern(0.9, 0.2) = 1 (0.9 > 0.8)
  expect_equal(qbern(0.9, prob = 0.2), 1L)
})

test_that("pbern returns correct values", {
  expect_equal(pbern(0, prob = 0.3), 1 - 0.3)
  expect_equal(pbern(1, prob = 0.3), 1)
})

test_that("dbern returns correct values", {
  expect_equal(dbern(1, prob = 0.4), 0.4)
  expect_equal(dbern(0, prob = 0.4), 0.6)
  expect_equal(dbern(1, prob = 0.4, log = TRUE), log(0.4))
})

test_that("distr creates valid distribution specification", {
  d <- distr(qnorm, mean = 0, sd = 1)
  expect_s3_class(d, "mlumr_distr")
  expect_true(is.function(d$qfun))
  expect_equal(d$qfun_name, "qnorm")
  expect_true("mean" %in% names(d$args))
  expect_true("sd" %in% names(d$args))
})

test_that("distr rejects non-quantile functions", {
  expect_error(distr(function(x) x), "inverse CDF")
})

test_that("eval_distr evaluates correctly with data context", {
  d <- distr(qbern, prob = my_prob)
  result <- eval_distr(d, p = c(0.3, 0.7), data = list(my_prob = 0.5))
  expect_equal(result, qbern(c(0.3, 0.7), prob = 0.5))
})

test_that("eval_distr gives informative error on missing column", {
  d <- distr(qbern, prob = nonexistent_col)
  expect_error(
    eval_distr(d, p = 0.5, data = list(x_mean = 0.3)),
    "Error evaluating distribution argument"
  )
  expect_error(
    eval_distr(d, p = 0.5, data = list(x_mean = 0.3)),
    "Available data columns"
  )
})

test_that("get_distribution_type classifies correctly", {
  d_norm <- distr(qnorm, mean = 0, sd = 1)
  d_bern <- distr(qbern, prob = 0.5)

  types <- get_distribution_type(x1 = d_norm, x2 = d_bern)
  expect_equal(types[["x1"]], "continuous")
  expect_equal(types[["x2"]], "binary")
})

test_that("cor_adjust_spearman returns valid correlation matrix", {
  rho <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
  types <- c("continuous", "binary")
  result <- cor_adjust_spearman(rho, types)

  expect_equal(diag(result), c(1, 1))
  expect_true(isSymmetric(result))
  # Adjusted values should differ from input
  expect_false(result[1, 2] == 0.3)
})

test_that("cor_adjust_pearson returns valid correlation matrix", {
  rho <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
  types <- c("continuous", "binary")
  result <- cor_adjust_pearson(rho, types)

  expect_equal(diag(result), c(1, 1))
  expect_true(isSymmetric(result))
})
