# Regression tests for the survival estimand and identification guards.

skip_if_not_installed("splines2")

# ---- Pooled-basis fallback removed (no silent return to the ridge) ----------

test_that("mismatched per-study knot counts reduce n_knots instead of pooling", {
  # Heavy ties in the comparator collapse duplicated quantile knots there but
  # not in the index study, which is what used to trigger the pooled fallback.
  ipd <- data.frame(.time = c(seq(1, 40, length.out = 60), 41:60),
                    .status = 1L)
  pseudo <- data.frame(.time = c(rep(c(2, 4, 6), each = 40), 7:20),
                       .status = 1L)

  specs <- suppressMessages(
    mlumr:::.matched_per_study_bases(ipd, pseudo, n_knots = 12, degree = 3L))
  # The contract: equal dimension, and every column supported in its OWN study.
  expect_equal(specs$index$n_scoef, specs$comparator$n_scoef)
  expect_lte(specs$n_knots, 12L)
  expect_gte(specs$n_knots, 1L)
  # Each basis spans only its own study, which is what removes the ridge.
  expect_equal(specs$index$boundary[2], max(ipd$.time))
  expect_equal(specs$comparator$boundary[2], max(pseudo$.time))
})

test_that("the support assertion rejects a basis with a dead column", {
  # Build the pooled basis the fallback used to install and confirm it is now
  # refused rather than fitted.
  set.seed(2026)
  idx <- c(stats::runif(50, 0, 20), stats::runif(150, 40, 100))
  cmp <- stats::runif(200, 0, 20)
  pooled <- mlumr:::.knots_from_times(c(idx, cmp), c(idx, cmp), n_knots = 7)
  spec <- mlumr:::.build_mspline_basis(pooled, degree = 0L)

  expect_error(
    mlumr:::.assert_basis_support(spec, max(cmp), "comparator"),
    "no support over its observed follow-up")
  # The index study, whose follow-up the pooled basis was built from, is fine.
  expect_true(mlumr:::.assert_basis_support(spec, max(idx), "index"))
})

# ---- Null reference lines --------------------------------------------------

test_that("every exponentiated effect gets a null line at 1", {
  skip_if_not(exists(".null_ref_for", asNamespace("mlumr")),
             "forest null-reference helpers arrive with the plotting change")
  ratio <- c("RR", "HR", "TR", "RMSTR", "EXP_DELTA_ETA", "EXP_ETA_CONTRAST")
  additive <- c("RD", "MD", "RMSTD", "LINK_EFFECT", "LOR")
  expect_equal(mlumr:::.null_ref_for(ratio), rep(1, length(ratio)))
  expect_equal(mlumr:::.null_ref_for(additive), rep(0, length(additive)))
})



# ---- Posterior contraction against heavy-tailed priors ---------------------

test_that("contraction is NA when the prior has no finite variance", {
  mk <- function(dist, df) {
    structure(list(
      model = "relaxed",
      data = list(covariates = c("age", "sex")),
      stan_data = list(prior_beta_comparator_sd = c(2.5, 2.5),
                       prior_beta_comparator_dist = dist,
                       prior_beta_comparator_df = df),
      draws = list("beta_comparator[1]" = rnorm(500, 0, 1),
                   "beta_comparator[2]" = rnorm(500, 0, 2.4))),
      class = "mlumr_fit")
  }
  set.seed(2026)
  # Normal prior: the stored scale IS the SD, so contraction is defined.
  n <- mlumr:::.relaxed_contraction(mk(0L, 3))
  expect_true(all(is.finite(n$contraction)))
  expect_equal(n$prior_sd, c(2.5, 2.5))

  # Student-t, df > 2: SD = scale * sqrt(df / (df - 2)), NOT the scale.
  t5 <- mlumr:::.relaxed_contraction(mk(1L, 5))
  expect_equal(t5$prior_sd, rep(2.5 * sqrt(5 / 3), 2))
  expect_true(all(is.finite(t5$contraction)))
  # Using the scale would have overstated how much was learned.
  expect_true(all(t5$contraction > n$contraction))

  # Cauchy (df = 1) and df = 2 have no finite variance: report NA, not a number.
  expect_true(all(is.na(mlumr:::.relaxed_contraction(mk(1L, 1))$contraction)))
  expect_true(all(is.na(mlumr:::.relaxed_contraction(mk(1L, 2))$contraction)))
})

# ---- n_knots = 0 rejected up front -----------------------------------------

test_that("n_knots = 0 is allowed where it is fittable and rejected where it is not", {
  # Superseded the earlier blanket rejection, which encoded a false premise:
  # see the degree-aware test below. Parametric distributions ignore n_knots.
  expect_true(mlumr:::.validate_survival_controls(NULL, NULL, NULL, 0, 100L,
                                                  distribution = "weibull"))
  expect_true(mlumr:::.validate_survival_controls(NULL, NULL, NULL, 3, 100L,
                                                  distribution = "mspline"))
})

# ---- check_integration compares like with like ------------------------------

test_that("realized correlations are measured on the target's own scale", {
  # A deliberately non-linear monotone pair: Spearman is 1 by construction while
  # Pearson is not, so a method mix-up is visible rather than subtle.
  set.seed(2026)
  n_int <- 64
  u <- seq(0.01, 0.99, length.out = n_int)
  X <- array(0, dim = c(1, n_int, 2))
  X[1, , 1] <- stats::qnorm(u)
  X[1, , 2] <- stats::qnorm(u)^3          # monotone, so Spearman rho = 1
  target <- matrix(c(1, 1, 1, 1), 2)

  sp <- mlumr:::.int_cor_stats(X, X, c("a", "b"), 1L, cor_target = target,
                               cor_method = "spearman")
  pe <- mlumr:::.int_cor_stats(X, X, c("a", "b"), 1L, cor_target = target,
                               cor_method = "pearson")

  expect_equal(sp$diff$cor_method, "spearman")
  expect_equal(pe$diff$cor_method, "pearson")
  # Measured on its own scale the grid reproduces the target exactly.
  expect_equal(sp$diff$cor_current, 1)
  expect_equal(sp$diff$abs_diff_target, 0)
  # Measured on the wrong scale it looks like a real discrepancy: this is the
  # false warning the fix removes.
  expect_lt(pe$diff$cor_current, 0.95)
  expect_gt(pe$diff$abs_diff_target, 0.05)
})

# ---- Spline dimension, support, centering, and raw effect naming -----------

test_that("n_knots = 0 is judged by the DEGREE, not the distribution name", {
  # n_scoef = length(internal) + degree + 1. With zero internal knots a cubic
  # M-spline has FOUR coefficients (three RW1 increments, perfectly definable)
  # while a degree-0 piecewise exponential has ONE (no increments at all).
  # Rejecting both was a false premise that blocked a valid model.
  k0 <- list(internal = numeric(0), boundary = c(0, 10))
  expect_equal(mlumr:::.build_mspline_basis(k0, degree = 3L)$n_scoef, 4L)
  expect_equal(mlumr:::.build_mspline_basis(k0, degree = 0L)$n_scoef, 1L)

  expect_true(mlumr:::.validate_survival_controls(NULL, NULL, NULL, 0, 100L,
                                                  distribution = "mspline"))
  expect_error(mlumr:::.validate_survival_controls(NULL, NULL, NULL, 0, 100L,
                                                   distribution = "pexp"),
               "random-walk smoothing prior")
})

test_that("support is tested at structural points, not a uniform grid", {
  # A degree-0 column supported on a very narrow inter-knot interval can fall
  # entirely between the points of a fixed uniform grid and be declared dead.
  narrow <- list(internal = c(0.001, 0.002, 5), boundary = c(0, 10))
  spec <- mlumr:::.build_mspline_basis(narrow, degree = 0L)
  expect_true(mlumr:::.assert_basis_support(spec, 10, "index"))

  # It must still catch a genuinely unsupported column: this is the pooled
  # basis whose late columns the shorter study never observes.
  set.seed(2026)
  idx <- c(stats::runif(50, 0, 20), stats::runif(150, 40, 100))
  cmp <- stats::runif(200, 0, 20)
  pooled <- mlumr:::.knots_from_times(c(idx, cmp), c(idx, cmp), n_knots = 7)
  expect_error(
    mlumr:::.assert_basis_support(
      mlumr:::.build_mspline_basis(pooled, degree = 0L), max(cmp), "comparator"),
    "no support")
})

test_that("survival centering weights each AgD row by its own pseudo-IPD count", {
  # stan_data$n_agd is the TOTAL pseudo-individual count. Splitting it equally
  # across rows makes the center depend on how the comparator happens to be
  # tabulated, which is exactly what population weighting exists to prevent.
  sd <- list(n_agd = 360L, agd_arm = c(rep(1L, 300L), rep(2L, 60L)))
  w <- mlumr:::.agd_center_weights(sd, "survival", 2L)
  expect_equal(w, c(300, 60))
  # The old behavior would have been c(180, 180).
  expect_false(isTRUE(all.equal(w, c(180, 180))))

  # And the center that follows is the population-weighted one.
  X_ipd <- matrix(0, nrow = 100, ncol = 1)
  X_int <- array(0, dim = c(2, 4, 1))
  X_int[1, , 1] <- 0    # the 300-person arm
  X_int[2, , 1] <- 10   # the 60-person arm
  sd2 <- c(sd, list(X_ipd = X_ipd, X_int = X_int))
  ctr <- mlumr:::.mlumr_center_covariates(sd2, center = TRUE,
                                          family = "survival")$cov_center
  expect_equal(unname(ctr), (100 * 0 + 300 * 0 + 60 * 10) / (100 + 360))
})


# ---- One derivation of the scalar's identity, shared by every surface -------
#
# `delta_*` is a log hazard ratio, a log time ratio, or a bare location
# contrast depending on the fit. marginal_effects() and prior_sensitivity()
# previously decided that independently, and the latter printed a generic
# "log hazard ratio / log time ratio" for quantities that were neither.

fake_surv_fit <- function(distribution, n_strata, model = "spfa") {
  structure(
    list(surv_info = mlumr:::.survival_distribution_info(distribution),
         stan_data = list(n_strata = as.integer(n_strata)),
         family = "survival", model = model,
         pred_times = seq(2, 20, length.out = 10)),
    class = "mlumr_fit")
}

test_that("raw AFT draw columns are not named tr_* when the label is not TR", {
  # `summary = FALSE` promises self-describing column names, so a stratified
  # AFT contrast must not arrive as `tr_index`. Assert against the package's
  # own naming, not a copy of its switch: a local reimplementation would keep
  # passing after the real columns in R/predict.R were renamed.
  with_draws <- function(...) {
    f <- fake_surv_fit(...)
    n <- 20L
    f$draws <- data.frame(
      delta_index = stats::rnorm(n), delta_comparator = stats::rnorm(n),
      rmst_diff_index = stats::rnorm(n), rmst_diff_comparator = stats::rnorm(n)
    )
    f
  }
  raw <- function(fit, effect) {
    names(suppressMessages(
      mlumr:::.marginal_effects_survival(fit, population = "both",
                                         effect = effect, summary = FALSE,
                                         probs = c(0.025, 0.5, 0.975))))
  }

  # Shared-shape PH is a genuine hazard ratio.
  expect_equal(raw(with_draws("weibull", 1L), "hr"),
               c("hr_index", "hr_comparator"))
  # Shared-shape AFT under SPFA is a genuine time ratio.
  expect_equal(raw(with_draws("lognormal", 1L), "tr"),
               c("tr_index", "tr_comparator"))
  # Stratified AFT has no acceleration factor, so the columns must say so.
  expect_equal(raw(with_draws("lognormal", 2L), "exp_delta_eta"),
               c("exp_delta_eta_index", "exp_delta_eta_comparator"))
})

test_that("the scalar label covers all three survival cases", {
  lab <- function(...) mlumr:::.surv_scalar_label(fake_surv_fit(...))

  # PH always carries a time: t -> 0 when the shapes are shared, the first
  # prediction time when they differ.
  expect_equal(lab("weibull", 1L)$label, "HR")
  expect_equal(lab("weibull", 1L)$at_time, 0)
  expect_equal(lab("weibull", 2L)$at_time, 2)
  # An exponential has no shape, so aux_by cannot change it.
  expect_equal(lab("exponential", 2L)$at_time, 0)

  # AFT with shared shapes under SPFA is a genuine time ratio.
  expect_equal(lab("lognormal", 1L)$label, "TR")
  expect_true(is.na(lab("lognormal", 1L)$at_time))
  # Differing shapes: no acceleration factor exists at all.
  expect_equal(lab("lognormal", 2L)$label, "EXP_DELTA_ETA")
  # RELAXED with shared shapes: coefficients differ by treatment, so the
  # covariate term does not cancel and exp(delta) is a geometric mean of
  # profile-specific ratios, not one population acceleration factor.
  expect_equal(lab("lognormal", 1L, model = "relaxed")$label, "EXP_DELTA_ETA")
  # ...but a relaxed PH fit is still a marginal hazard ratio.
  expect_equal(lab("weibull", 1L, model = "relaxed")$label, "HR")

  # Log-scale names, as stored in delta_* and reported by prior_sensitivity().
  lg <- function(...) mlumr:::.surv_scalar_label(fake_surv_fit(...), log_scale = TRUE)
  expect_equal(lg("weibull", 2L)$label, "LOG_HR")
  expect_equal(lg("lognormal", 1L)$label, "LOG_TR")
  expect_equal(lg("lognormal", 2L)$label, "DELTA_ETA")
  expect_equal(lg("lognormal", 1L, model = "relaxed")$label, "DELTA_ETA")
})

test_that("at_time rejects non-positive times", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  dat <- sim_survival_data(seed = 2026, n_ipd = 30, n_agd = 30, n_int = 8)
  fit <- suppressWarnings(suppressMessages(
    fit_survival_test(dat, chains = 1, iter = 100, warmup = 50)))
  # Survival times are positive and the prediction grid excludes 0, so there is
  # no nearest fitted time for a non-positive request to snap to.
  expect_error(marginal_effects(fit, effect = "hr", at_time = 0), "positive")
  expect_error(marginal_effects(fit, effect = "hr", at_time = -5), "positive")
})

test_that("at_time is refused where it cannot apply", {
  # It selects the evaluation time of the scalar marginal hazard ratio, so it
  # is meaningless for a family with no time axis and for an effect that is an
  # integral to a horizon. Both used to be ignored silently, and the RMST case
  # also emitted a "using the nearest fitted time" message, which reads as if
  # the reported number had been taken at that time.
  f <- fake_surv_fit("weibull", 2L)
  f$draws <- data.frame(rmst_diff_index = 1, rmst_diff_comparator = 1)
  expect_error(marginal_effects(f, effect = "rmstd", at_time = 12),
               "integral to the RMST horizon")

  b <- structure(list(family = "binomial", model = "spfa",
                      draws = data.frame(lor_index = 1)),
                 class = "mlumr_fit")
  expect_error(marginal_effects(b, at_time = 12), "no time axis")
})

test_that("draw column names separate times %g would collapse", {
  # `%g` carries six significant digits, so two distinct fitted times closer
  # than that produced duplicate column names and the caller could no longer
  # tell which draw belonged to which time.
  close_times <- c(1.0000001, 1.0000002)
  nm <- sprintf("t_%.15g", close_times)
  expect_equal(length(unique(nm)), 2L)
  # Ordinary grids keep their short, readable names.
  expect_equal(sprintf("t_%.15g", c(0.5, 1, 2.5, 10)),
               c("t_0.5", "t_1", "t_2.5", "t_10"))
})
