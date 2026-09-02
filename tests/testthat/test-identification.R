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
  expect_gt(g$eff_dim, 2)

  # Rows on a single line: however many there are, one direction.
  line <- cbind(seq(-2, 2, length.out = 8), 0.4, 0.6)
  g2 <- mlumr:::.subgroup_geometry(line, ref)
  expect_lt(g2$cond_inv, 1e-8)
  expect_equal(g2$eff_dim, 1, tolerance = 1e-6)

  # Enough rows, but the third column barely moves: the count is satisfied and
  # the geometry is not.
  flat3 <- rbind(c(-1, 0, 0.50), c(1, 0, 0.50),
                 c(0, 1, 0.50), c(0, -1, 0.5001))
  g3 <- mlumr:::.subgroup_geometry(flat3, ref)
  expect_lt(g3$cond_inv, 0.01)

  # A single row has no geometry at all.
  g4 <- mlumr:::.subgroup_geometry(matrix(c(0, 0.5, 0.5), nrow = 1), ref)
  expect_equal(g4$cond_inv, 0)
  expect_equal(g4$eff_dim, 0)
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
  # The row geometry counts one scalar constraint per aggregate summary. A
  # reconstructed comparator curve contributes a likelihood term at every event
  # and censoring time instead, so what it identifies is model- and
  # covariate-distribution-dependent: one binary covariate under exponential
  # proportional hazards gives a known-weight mixture whose component rates the
  # curve separates, while several continuous covariates can leave the curve
  # nearly invariant to rotations of the coefficient vector. A row count neither
  # bounds nor certifies identification there, so the diagnostic must refuse
  # rather than report a number that would be read as either.
  #
  # The guard runs before anything else is read, so a minimal object carrying
  # the family label exercises it. It was previously written against a
  # `sim_survival_data()` that does not exist here, behind a
  # `skip_if_not(exists(...))`, so it never ran at all.
  dat <- structure(
    list(family = "survival", covariates = "x1",
         ipd = list(data = data.frame(x1 = c(0, 1))),
         agd = list(data = data.frame(x1_mean = 0.5))),
    class = "mlumr_data")

  expect_error(check_identification(dat), "not valid for reconstructed survival")
  expect_error(check_identification(dat), "prior_sensitivity")

  # The guard is specific to the survival family. A minimal object may still
  # fail further in for its own reasons, so assert only that it is not this
  # error, rather than that there is none.
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
  expect_equal(
    mlumr:::.agd_distinct_profiles(
      list(integration_points = NULL,
           agd = list(data = data.frame(trt = rep("B", 5))))),
    5L)
})

# ---- rank is judged on a scale that can be judged --------------------------

test_that("profile rank survives an offset and fails closed", {
  # qr() calls a column negligible relative to the norms it is handed, so an
  # uncentered covariate on a large offset collapses to rank 1.
  expect_equal(mlumr:::.profile_rank(matrix(1e7 + c(0, 1, 2), ncol = 1)), 2L)
  expect_equal(mlumr:::.profile_rank(matrix(c(0, 1, 2), ncol = 1)), 2L)
  # Duplicated profiles carry one direction however many rows there are.
  expect_equal(mlumr:::.profile_rank(matrix(rep(c(1, 2), each = 3), ncol = 2)), 1L)
  # A design qr() cannot decompose used to fall back to the aggregate row
  # count, which is the quantity the rank replaced: a padded table then looked
  # full rank and suppressed its warning.
  expect_equal(mlumr:::.profile_rank(matrix(c(NA_real_, 1), ncol = 1)), 0L)
})

# ---- absolute scale, not only relative balance -----------------------------

test_that("the geometry helper reports spread as well as balance", {
  # cond_inv compares directions with one another, so with a single covariate
  # there is one singular value and the ratio is 1 for any nonzero separation.
  # Subgroup means 1e-12 apart scored a perfect 1.
  tiny <- mlumr:::.subgroup_geometry(matrix(c(0, 1e-12), ncol = 1), 1)
  expect_equal(tiny$cond_inv, 1)
  expect_lt(tiny$spread, 1e-10)

  real <- mlumr:::.subgroup_geometry(matrix(c(0, 1), ncol = 1), 1)
  expect_equal(real$cond_inv, 1)
  expect_gt(real$spread, 0.05)
})

test_that("the spectrum survives a covariate on an extreme scale", {
  # eff_dim is scale-free by construction, but sum(d^2)^2 and sum(d^4) are not:
  # both overflowed and returned NaN for an ordinary spectrum.
  for (a in c(1e-200, 1, 1e200)) {
    g <- mlumr:::.subgroup_geometry(matrix(c(-a, a), ncol = 1), 1)
    expect_false(is.nan(g$eff_dim), info = format(a))
    expect_equal(g$eff_dim, 1, tolerance = 1e-8, info = format(a))
  }
})

test_that("a single row comes back centered and scaled, as documented", {
  g <- mlumr:::.subgroup_geometry(matrix(10, ncol = 1), 2)
  expect_equal(as.numeric(g$means), 0)
  expect_equal(g$spread, 0)
})
