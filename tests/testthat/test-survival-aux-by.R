# aux_by: the baseline hazard estimated separately per study (NULL or
# ".study", the default) or shared across both studies ("none"), the unanchored
# analogue of multinma::nma()'s aux_by. multinma always stratifies by study and
# mlumr matches it, so NULL resolves to ".study" rather than to a shared
# baseline; "none" is the mlumr-only spelling for the shared-shape model.

test_that("NULL means .study, exactly as in multinma", {
  # multinma/R/nma.R: `if (quo_is_null(aux_by)) aux_by <- ".study"`, and
  # get_aux_id(add_study = TRUE) forces .study into the grouping regardless. A
  # multinma user writing aux_by = NULL must not get the opposite model here.
  expect_equal(mlumr:::.resolve_aux_strata(NULL), 2L)
  expect_equal(mlumr:::.resolve_aux_strata(".study"), 2L)
  expect_equal(mlumr:::.resolve_aux_strata("none"), 1L)
})

test_that("stratifying per study is the package default", {
  expect_identical(formals(mlumr::mlumr)$aux_by, ".study")
})

test_that("an invalid aux_by fails loudly rather than silently sharing", {
  expect_error(mlumr:::.resolve_aux_strata("study"), "aux_by")
  expect_error(mlumr:::.resolve_aux_strata("shared"), "aux_by")
  expect_error(mlumr:::.resolve_aux_strata(c(".study", "none")), "aux_by")
  expect_error(mlumr:::.resolve_aux_strata(2), "aux_by")
  # .trt is rejected with an explanation rather than silently aliased
  expect_error(mlumr:::.resolve_aux_strata(".trt"), "same as")
})

test_that("the simplex pins H0(T) = 1, which is what identifies the intercepts", {
  # The I-spline basis is all ones at the upper boundary knot, so for ANY
  # simplex scoef, H0(T) = ibasis(T) . scoef = sum(scoef) = 1. Both studies are
  # therefore anchored at the same cumulative hazard even when their shapes are
  # free, which is why mu_index - mu_comparator stays identified.
  bk <- c(0, 60); ik <- c(10, 20, 35)
  ib <- as.numeric(splines2::iSpline(bk[2], knots = ik, degree = 3,
                                     Boundary.knots = bk, intercept = TRUE))
  expect_true(all(abs(ib - 1) < 1e-12))
  set.seed(2026)
  for (i in 1:5) {
    w <- stats::rexp(length(ib)); simplex <- w / sum(w)
    expect_equal(sum(ib * simplex), 1, tolerance = 1e-12)
  }
})

test_that("aux_by reaches Stan as n_strata, for both baseline kinds", {
  skip_on_cran()
  # Regression test for aux_by being a free variable inside
  # .mlumr_build_stan_data(): it was resolved by lexical lookup, so it never
  # reached the Stan data and every survival fit errored with
  # "object 'aux_by' not found". Go through mlumr() rather than the internal so
  # the whole argument chain is exercised.
  expect_true("aux_by" %in% names(formals(mlumr::mlumr)))
  expect_true("aux_by" %in% names(formals(mlumr:::.mlumr_build_stan_data)))

  dat <- sim_survival_data(seed = 2026)
  for (d in c("weibull", "mspline")) {
    shared <- fit_survival_test(dat, distribution = d, n_knots = 4,
                                aux_by = "none")
    strat  <- fit_survival_test(dat, distribution = d, n_knots = 4)
    expect_equal(shared$stan_data$n_strata, 1L, info = d)
    expect_equal(strat$stan_data$n_strata, 2L, info = d)
  }
})

test_that("NULL and the default give the same fit; \"none\" gives the shared one", {
  skip_on_cran()
  dat <- sim_survival_data(seed = 2026)
  a <- fit_survival_test(dat, distribution = "weibull")                  # default
  b <- fit_survival_test(dat, distribution = "weibull", aux_by = NULL)   # multinma spelling
  d <- fit_survival_test(dat, distribution = "weibull", aux_by = "none") # opt out
  expect_equal(a$stan_data$n_strata, 2L)
  expect_equal(b$stan_data$n_strata, 2L)
  expect_equal(d$stan_data$n_strata, 1L)
  expect_equal(a$summary$mean, b$summary$mean, tolerance = 1e-8)
})

test_that("stratifying gives each study its own baseline and still converges", {
  skip_on_cran()
  dat <- sim_survival_data(seed = 2026)

  fit <- fit_survival_test(dat, distribution = "weibull")   # stratified by default
  expect_equal(fit$stan_data$n_strata, 2L)
  # two shape parameters, not one
  expect_true(all(c("aux_val", "aux_val_cmp") %in% names(fit$draws)))
  expect_true(all(fit$summary$Rhat < 1.1, na.rm = TRUE))
  # the estimand survives
  expect_true(all(is.finite(predict(fit, type = "rmst")$mean)))
  expect_true(all(is.finite(predict(fit, type = "survival")$mean)))

  ms <- fit_survival_test(dat, distribution = "mspline", n_knots = 4)
  expect_equal(ms$stan_data$n_strata, 2L)
  # scoef is now a [n_scoef, 2] matrix
  expect_true(any(grepl("^scoef\\[1,2\\]$", names(ms$draws))))
  expect_true(all(is.finite(predict(ms, type = "survival")$mean)))
  # and the two strata are genuinely separate parameters
  s1 <- mlumr:::.surv_scoef_draws(ms, "index")
  s2 <- mlumr:::.surv_scoef_draws(ms, "comparator")
  expect_equal(dim(s1), dim(s2))
  expect_false(isTRUE(all.equal(as.numeric(s1), as.numeric(s2))))
  # each stratum is still a simplex
  expect_equal(unname(rowSums(s1)), rep(1, nrow(s1)), tolerance = 1e-8)
  expect_equal(unname(rowSums(s2)), rep(1, nrow(s2)), tolerance = 1e-8)
})


test_that("the R readers find the baseline under every draw-name layout", {
  # scoef is a matrix, so draws are named scoef[j,s] and the per-treatment
  # views scoef_idx[j] / scoef_cmp[j] are emitted alongside. Both must resolve.
  mk <- function(nms) {
    d <- as.data.frame(matrix(seq_along(nms) * 1.0, nrow = 2,
                              ncol = length(nms), byrow = TRUE))
    names(d) <- nms
    structure(list(draws = d, stan_data = list(n_scoef = 3L, n_strata = 2L),
                   surv_info = list(kind = "mspline")), class = "mlumr_fit")
  }
  views  <- mk(c(paste0("scoef_idx[", 1:3, "]"), paste0("scoef_cmp[", 1:3, "]")))
  matrix_names <- mk(c(paste0("scoef[", 1:3, ",1]"), paste0("scoef[", 1:3, ",2]")))

  for (o in list(views, matrix_names)) {
    a <- mlumr:::.surv_scoef_draws(o, "index")
    b <- mlumr:::.surv_scoef_draws(o, "comparator")
    expect_equal(dim(a), c(2L, 3L))
    expect_false(isTRUE(all.equal(as.numeric(a), as.numeric(b))))
  }
  # A plain vector `scoef[j]` is not a layout any shipped fit has.
  expect_error(mlumr:::.surv_scoef_draws(mk(paste0("scoef[", 1:3, "]")), "index"),
               "Could not find spline coefficient draws")
})

test_that("a stratified conditional hazard ratio warns instead of misreporting", {
  skip_on_cran()
  dat <- sim_survival_data(seed = 2026)
  fit <- fit_survival_test(dat, distribution = "weibull")
  nd <- as.data.frame(as.list(colMeans(dat$ipd$data[, dat$covariates,
                                                    drop = FALSE])))
  # exp(eta_i - eta_c) drops h0_index(t)/h0_comparator(t) once the shapes differ.
  # Match the baseline-ratio phrase, not a bare "does not": that substring
  # appears in ordinary English and any unrelated warning would satisfy it, so
  # the test would pass with the stratified-baseline warning absent.
  expect_warning(conditional_effects(fit, newdata = nd),
                 "baseline ratio h0_index")
})
