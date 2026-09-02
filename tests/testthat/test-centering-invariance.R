# Covariate centering must not depend on how the aggregate evidence is
# TABULATED. Centering is likelihood-invariant (the intercept absorbs the
# shift) but NOT prior-invariant: the intercept prior is placed on the centered
# intercept, so a center that moves when one comparator subgroup is split into
# two statistically equivalent rows silently changes the induced raw-scale
# prior, and with it the sparse-data posterior.

make_sd <- function(n_ipd, ipd_mean, agd_means, agd_weights, n_int = 8,
                    field = "n_agd") {
  X_ipd <- matrix(ipd_mean, nrow = n_ipd, ncol = 1)
  n_rows <- length(agd_means)
  X_int <- array(0, dim = c(n_rows, n_int, 1))
  for (r in seq_len(n_rows)) X_int[r, , 1] <- agd_means[r]
  sd <- list(X_ipd = X_ipd, X_int = X_int)
  sd[[field]] <- agd_weights
  sd
}

center_of <- function(sd, family = "binomial", agd_means = NULL) {
  mlumr:::.mlumr_center_covariates(
    sd, center = TRUE, family = family, agd_means = agd_means
  )$cov_center
}

test_that("centering is invariant to splitting one AgD row into equivalent rows", {
  # 100 IPD patients at covariate mean 0; comparator population of 200 at mean 10.
  whole <- make_sd(100, 0, agd_means = 10, agd_weights = 200)
  split2 <- make_sd(100, 0, agd_means = c(10, 10), agd_weights = c(100, 100))
  split10 <- make_sd(100, 0, agd_means = rep(10, 10), agd_weights = rep(20, 10))

  expect_equal(center_of(whole), center_of(split2), tolerance = 1e-12)
  expect_equal(center_of(whole), center_of(split10), tolerance = 1e-12)

  # Unequal but summing splits are equivalent too.
  uneven <- make_sd(100, 0, agd_means = c(10, 10, 10),
                    agd_weights = c(150, 30, 20))
  expect_equal(center_of(whole), center_of(uneven), tolerance = 1e-12)
})

test_that("row-count weighting would NOT have been invariant", {
  # Documents the defect this replaces: with a per-row weight of 1 the center moves
  # from ~0.099 to ~0.909 purely by re-tabulating the same evidence.
  row_weighted <- function(n_ipd, ipd_mean, agd_means) {
    (n_ipd * ipd_mean + sum(agd_means)) / (n_ipd + length(agd_means))
  }
  expect_equal(round(row_weighted(100, 0, 10), 3), 0.099)
  expect_equal(round(row_weighted(100, 0, rep(10, 10)), 3), 0.909)
  # The shipped weighting gives the same answer for both tabulations.
  whole <- center_of(make_sd(100, 0, 10, 200))
  split <- center_of(make_sd(100, 0, rep(10, 10), rep(20, 10)))
  expect_equal(whole, split, tolerance = 1e-12)
})

test_that("weights fall back to equal when no usable field is present", {
  sd <- make_sd(100, 0, agd_means = c(10, 10), agd_weights = c(NA, NA))
  w <- mlumr:::.agd_center_weights(sd, "binomial", 2L)
  expect_equal(w, c(1, 1))
  # Non-positive weights are rejected too.
  sd2 <- make_sd(100, 0, agd_means = c(10, 10), agd_weights = c(0, 5))
  expect_equal(mlumr:::.agd_center_weights(sd2, "binomial", 2L), c(1, 1))
})

test_that("each family reads its own comparator weight field", {
  # normal is absent here on purpose: it has no comparator weight field yet, so
  # it falls back to equal weights and is not split-invariant. It joins this
  # loop with the change that gives it one.
  for (fam in c("binomial", "poisson")) {
    field <- mlumr:::get_family_config(fam)$comp_weight_field
    sd <- make_sd(100, 0, agd_means = c(10, 10), agd_weights = c(30, 70),
                  field = field)
    expect_equal(mlumr:::.agd_center_weights(sd, fam, 2L), c(30, 70))
  }
})

test_that("declared AgD means make centering independent of the QMC realization", {
  low <- make_sd(100, 0, agd_means = 9.5, agd_weights = 200, n_int = 8)
  high <- make_sd(100, 0, agd_means = 10.2, agd_weights = 200, n_int = 128)

  expect_equal(center_of(low, agd_means = 10),
               center_of(high, agd_means = 10), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(center_of(low), center_of(high))))
})


test_that("qr = TRUE rejects a design it cannot invert", {
  # set_ipd() permits a constant covariate with a warning. In a relaxed model
  # that covariate's index column is exactly collinear with the index
  # intercept, so the combined design loses rank and solve() on R would fail
  # with an opaque LAPACK error. The model is still estimable with qr = FALSE.
  X_ipd <- cbind(a = rep(2, 5), b = c(0, 1, 0, 1, 0))
  X_int <- array(0, dim = c(1, 4, 2))
  X_int[1, , 1] <- c(1, 2, 3, 4)
  X_int[1, , 2] <- c(0, 1, 1, 0)
  sd <- list(X_ipd = X_ipd, X_int = X_int)

  expect_error(
    mlumr:::.mlumr_qr_design(sd, model = "relaxed", qr = TRUE),
    "full-rank design"
  )
  expect_silent(mlumr:::.mlumr_qr_design(sd, model = "relaxed", qr = FALSE))
  expect_equal(mlumr:::.mlumr_qr_design(sd, model = "relaxed", qr = FALSE)$nB, 6L)

  tiny <- list(X_ipd = matrix(c(1, 2), nrow = 1),
               X_int = array(c(1, 2), dim = c(1, 1, 2)))
  expect_error(
    mlumr:::.mlumr_qr_design(tiny, model = "relaxed", qr = TRUE),
    "at least as many rows"
  )
})
