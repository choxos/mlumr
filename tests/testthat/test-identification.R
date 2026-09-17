# check_identification(): does the aggregate data pin down beta_comparator?
#
# Two conditions, and each alone is insufficient. There must be at least K + 1
# rows (the comparator intercept is unknown too), AND the rows must differ in
# every covariate direction. The second is the one that bites: a cross-tab of
# categorical covariates can satisfy the count while leaving a continuous
# covariate at nearly the same mean in every cell.

test_that("the geometry helper separates spread from collapsed designs", {
  ref <- c(1, 0.5, 0.5)

  # Rows spanning all three directions.
  spread <- rbind(c(-1, 0, 0), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  g <- mlumr:::.subgroup_geometry(spread, ref)
  expect_gt(g$cond_inv, 0.2)

  # Rows on a single line: however many there are, one direction.
  line <- cbind(seq(-2, 2, length.out = 8), 0.4, 0.6)
  g2 <- mlumr:::.subgroup_geometry(line, ref)
  expect_lt(g2$cond_inv, 1e-8)

  # Enough rows, but the third column barely moves: the count is satisfied and
  # the geometry is not.
  flat3 <- rbind(c(-1, 0, 0.50), c(1, 0, 0.50),
                 c(0, 1, 0.50), c(0, -1, 0.5001))
  g3 <- mlumr:::.subgroup_geometry(flat3, ref)
  expect_lt(g3$cond_inv, 0.01)

  # A single row has no geometry at all.
  g4 <- mlumr:::.subgroup_geometry(matrix(c(0, 0.5, 0.5), nrow = 1), ref)
  expect_equal(g4$cond_inv, 0)
})

test_that("centering costs one dimension, which is the K + 1 rule", {
  # K rows can span at most K - 1 directions after centering, so K + 1 rows are
  # needed before the count can possibly suffice.
  ref <- rep(1, 3)
  three <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  expect_lt(mlumr:::.subgroup_geometry(three, ref)$cond_inv, 1e-8)
  four <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(-1, -1, -1))
  expect_gt(mlumr:::.subgroup_geometry(four, ref)$cond_inv, 0.2)
})

test_that("check_identification validates its input", {
  expect_error(check_identification(list()), "mlumr_data")
  expect_error(check_identification(NULL), "mlumr_data")
})

test_that("check_identification reports both failure modes and the pass", {
  set.seed(2026)
  n <- 200
  mk <- function(agd) {
    ipd <- data.frame(trt = "A", y = stats::rbinom(n, 1, 0.4),
                      x1 = stats::rnorm(n), x2 = stats::rbinom(n, 1, 0.5),
                      x3 = stats::rbinom(n, 1, 0.4))
    io <- set_ipd(ipd, "trt", "y", c("x1", "x2", "x3"), family = "binomial")
    ao <- set_agd(agd, "trt", family = "binomial", outcome_r = "r",
                  outcome_n = "n",
                  cov_means = c("x1_mean", "x2_prop", "x3_prop"),
                  cov_sds = c("x1_sd", NA, NA),
                  cov_types = c("continuous", "binary", "binary"))
    suppressWarnings(add_integration(
      combine_data(io, ao), n_int = 32,
      x1 = distr(qnorm, mean = x1_mean, sd = x1_sd),
      x2 = distr(qbern, prob = x2_mean),
      x3 = distr(qbern, prob = x3_mean), verbose = FALSE))
  }
  row <- function(x1, p2, p3) data.frame(trt = "B", x1_mean = x1, x1_sd = 1,
                                         x2_prop = p2, x3_prop = p3,
                                         r = 40, n = 100)

  # (a) Too few rows.
  one <- suppressMessages(check_identification(mk(row(0, 0.5, 0.5)),
                                               verbose = FALSE))
  expect_equal(one$n_rows, 1L)
  expect_equal(one$n_rows_needed, 4L)
  expect_true(one$flagged)

  # (b) Enough rows, collapsed geometry: subgroups by the continuous covariate
  # only, so both proportions are identical in every row.
  coll <- suppressMessages(check_identification(
    mk(do.call(rbind, lapply(c(-1.5, -0.5, 0.5, 1.5, 2.5),
                             function(v) row(v, 0.5, 0.4)))), verbose = FALSE))
  expect_gte(coll$n_rows, coll$n_rows_needed)   # count is satisfied
  expect_lt(coll$cond_inv, 0.2)                 # geometry is not
  expect_true(is.na(coll$flagged))
  expect_equal(coll$diagnostic_scope, "descriptive")

  # (c) The recommended pattern: cross-tab the two categorical covariates, then
  # split on the continuous one so the cells differ there too. (A 2x2 cross-tab
  # ALONE scores about 0.05 here, because all four cells share nearly the same
  # continuous mean; that is failure mode (b) wearing a respectable-looking
  # four-row table.)
  good <- suppressMessages(check_identification(
    mk(rbind(row(-1, 0, 0), row(-1, 1, 0), row(-1, 0, 1), row(-1, 1, 1),
             row(1, 0, 0), row(1, 1, 1))), verbose = FALSE))
  expect_gte(good$n_rows, good$n_rows_needed)
  expect_gt(good$cond_inv, 0.2)
  expect_true(is.na(good$flagged))
  expect_equal(good$diagnostic_scope, "descriptive")
})

test_that("identity-link normal means retain the exact geometry screen", {
  set.seed(2026)
  ipd <- set_ipd(
    data.frame(trt = "A", y = stats::rnorm(100), x = stats::rnorm(100)),
    "trt", "y", "x", family = "normal"
  )
  agd <- set_agd(
    data.frame(trt = c("B", "B"), y = c(0, 1), se = c(0.2, 0.2),
               n = c(100, 100), x_mean = c(-1, 1), x_sd = c(1, 1)),
    "trt", family = "normal", outcome_mean = "y", outcome_se = "se",
    outcome_n = "n", cov_means = "x_mean", cov_sds = "x_sd"
  )
  dat <- suppressWarnings(add_integration(
    combine_data(ipd, agd), n_int = 32,
    x = distr(qnorm, mean = x_mean, sd = x_sd), verbose = FALSE
  ))

  out <- check_identification(dat, verbose = FALSE)
  expect_equal(out$diagnostic_scope, "identity")
  expect_false(out$flagged)
})

test_that("check_identification refuses survival data", {
  # A reconstructed curve contributes a likelihood term at every event and
  # censoring time, not one scalar summary per row, so the row geometry does
  # not apply. The guard runs before anything else is read.
  dat <- list(family = "survival", covariates = "x1",
              ipd = list(data = data.frame(x1 = c(0, 1))),
              agd = list(data = data.frame(x1_mean = 0.5)))
  class(dat) <- "mlumr_data"

  expect_error(check_identification(dat), "not valid for reconstructed survival")
  expect_error(check_identification(dat), "prior_sensitivity")

  # A minimal binomial object may fail further in for its own reasons, so
  # assert only that it is not this error.
  dat$family <- "binomial"
  err <- tryCatch({
    check_identification(dat, verbose = FALSE)
    NULL
  }, error = conditionMessage)
  expect_false(!is.null(err) && grepl("reconstructed survival", err))
})

# ---- a repeated row is not evidence ----------------------------------------

test_that("rows repeating an integration grid are counted once", {
  mk <- function(rows) {
    a <- array(0, dim = c(length(rows), 4L, 2L))
    for (i in seq_along(rows)) a[i, , ] <- rows[[i]]
    list(integration_points = a,
         agd = list(data = data.frame(trt = rep("B", length(rows)))))
  }
  expect_equal(mlumr:::.agd_distinct_profiles(mk(list(1, 1, 1))), 1L)
  expect_equal(mlumr:::.agd_distinct_profiles(mk(list(1, 1, 2))), 2L)
  expect_equal(mlumr:::.agd_distinct_profiles(mk(list(1, 2, 3))), 3L)
  # With nothing to compare, the row count is all there is.
  none <- list(integration_points = NULL,
               agd = list(data = data.frame(trt = rep("B", 5))))
  expect_equal(mlumr:::.agd_distinct_profiles(none), 5L)
})

# ---- rank is judged on a scale that can be judged --------------------------

test_that("profile rank survives an offset and fails closed", {
  # qr() would call a column on a large offset negligible.
  expect_equal(mlumr:::.profile_rank(matrix(1e7 + c(0, 1, 2), ncol = 1), 1), 2L)
  expect_equal(mlumr:::.profile_rank(matrix(c(0, 1, 2), ncol = 1), 1), 2L)
  # Duplicated profiles carry one direction however many rows there are.
  expect_equal(
    mlumr:::.profile_rank(matrix(rep(c(1, 2), each = 3), ncol = 2), c(1, 1)), 1L
  )
  # A design that cannot be decomposed fails closed.
  expect_equal(mlumr:::.profile_rank(matrix(c(NA_real_, 1), ncol = 1), 1), 0L)
})


test_that("profile rank is judged against the IPD scale, not its own", {
  # Scaling by the IPD SD, not by each column's own spread.
  collapsed <- matrix(c(-1e-10, 1e-10), ncol = 1)
  expect_equal(mlumr:::.profile_rank(collapsed, 10), 1L)
  # A real separation on the same covariate still counts.
  expect_equal(mlumr:::.profile_rank(matrix(c(40, 60), ncol = 1), 10), 2L)
  # A non-positive or missing reference SD falls back to 1 rather than dividing
  # by zero.
  expect_equal(mlumr:::.profile_rank(matrix(c(0, 1, 2), ncol = 1), 0), 2L)
  expect_equal(mlumr:::.profile_rank(matrix(c(0, 1, 2), ncol = 1), NA_real_), 2L)
  # The floor is the value check_identification() screens `spread` on, so the
  # two diagnostics cannot disagree about the same design.
  expect_equal(mlumr:::.profile_rank(matrix(c(-0.02, 0.02), ncol = 1), 1), 1L)
  expect_equal(mlumr:::.profile_rank(matrix(c(-0.2, 0.2), ncol = 1), 1), 2L)
})

# ---- absolute scale, not only relative balance -----------------------------

test_that("the geometry helper reports spread as well as balance", {
  # With one covariate cond_inv is 1 for any nonzero separation; spread is
  # what sees the scale.
  tiny <- mlumr:::.subgroup_geometry(matrix(c(0, 1e-12), ncol = 1), 1)
  expect_equal(tiny$cond_inv, 1)
  expect_lt(tiny$spread, 1e-10)

  real <- mlumr:::.subgroup_geometry(matrix(c(0, 1), ncol = 1), 1)
  expect_equal(real$cond_inv, 1)
  expect_gt(real$spread, 0.05)
})

test_that("a single row comes back centered and scaled, as documented", {
  g <- mlumr:::.subgroup_geometry(matrix(10, ncol = 1), 2)
  expect_equal(as.numeric(g$means), 0)
  expect_equal(g$spread, 0)
})

# ---- the geometry the likelihood sees --------------------------------------

test_that("declared means that the integration grid does not reproduce are caught", {
  # Declared `<covariate>_mean` columns are preferred while they describe the
  # design being fitted; a `distr()` that ignores them does not.
  mk <- function(realized_means) {
    n <- length(realized_means)
    x <- array(0, dim = c(n, 8L, 1L))
    for (i in seq_len(n)) x[i, , 1L] <- realized_means[[i]]
    out <- list(family = "normal", covariates = "x1",
                ipd = list(data = data.frame(x1 = stats::rnorm(20))),
                agd = list(data = data.frame(x1_mean = c(-1, 1))),
                integration_points = x)
    class(out) <- "mlumr_data"
    out
  }
  # Integration grids that do reproduce the declared spread: declared wins,
  # silently.
  ok <- mk(list(-1, 1))
  expect_silent(got <- mlumr:::.agd_mean_profiles(ok))
  expect_equal(as.numeric(got), c(-1, 1))

  # Both rows built from the same fixed distribution: the declared profiles
  # span a direction the likelihood does not have.
  bad <- mk(list(0, 0))
  expect_warning(got <- mlumr:::.agd_mean_profiles(bad),
                 "do not reproduce the declared")
  expect_equal(as.numeric(got), c(0, 0))
})
