# Tests for prior_sensitivity() — exercises the argument-validation path
# and the internal .rescale_prior_beta helper. The full refit path calls
# mlumr() and requires Stan, which is covered by the smoke-level tests in
# test-mlumr.R; here we stay pure-R so the suite stays fast.

test_that("prior_sensitivity rejects non-mlumr_fit input", {
  expect_error(prior_sensitivity(list()), "mlumr_fit")
  expect_error(prior_sensitivity(NULL), "mlumr_fit")
})


test_that("prior_sensitivity rejects invalid prior_beta_scales", {
  fit <- structure(list(priors = list(beta = prior_normal(0, 2.5))),
                   class = "mlumr_fit")
  expect_error(prior_sensitivity(fit, prior_beta_scales = c(-1, 2)),
               "positive finite")
  expect_error(prior_sensitivity(fit, prior_beta_scales = c(0, 1)),
               "positive finite")
  expect_error(prior_sensitivity(fit, prior_beta_scales = c(NA, 1)),
               "positive finite")
  expect_error(prior_sensitivity(fit, prior_beta_scales = "2.5"),
               "positive finite")
})


test_that("prior_sensitivity errors when priors$beta is missing", {
  fit <- structure(list(priors = list()), class = "mlumr_fit")
  expect_error(prior_sensitivity(fit, prior_beta_scales = 2.5),
               "Original `prior_beta` not found")
})


test_that(".rescale_prior_beta preserves family and replaces scale", {
  p <- prior_normal(0, 2.5)
  r <- mlumr:::.rescale_prior_beta(p, new_scale = 5)
  expect_equal(r$sd, 5)
  # Mean and distribution preserved
  expect_equal(r$mean, 0)
  expect_equal(r$distribution, "normal")
  # Default tags are stripped
  expect_null(r$default)
  expect_null(r$version)
})


test_that(".rescale_prior_beta preserves student_t df", {
  p <- prior_student_t(df = 5, mean = 0, sd = 2.5)
  r <- mlumr:::.rescale_prior_beta(p, new_scale = 1)
  expect_equal(r$sd, 1)
  expect_equal(r$df, 5)
  expect_equal(r$distribution, "student_t")
})


test_that(".rescale_prior_beta falls back to normal for exponential input", {
  # Exponential on beta is not supported at fit time, but the helper
  # should not blow up if a pathological fit carries one.
  p <- prior_exponential(rate = 1)
  r <- mlumr:::.rescale_prior_beta(p, new_scale = 2.5)
  expect_equal(r$distribution, "normal")
  expect_equal(r$sd, 2.5)
})


test_that(".rescale_prior_beta rescales per-coefficient priors element-wise", {
  priors <- list(prior_normal(0, 2.5), prior_normal(0, 1))
  r <- mlumr:::.rescale_prior_beta(priors, new_scale = 3)
  expect_length(r, 2L)
  expect_true(all(vapply(r, function(x) x$sd, numeric(1)) == 3))
  expect_null(r[[1]]$default)
})

test_that("prior_sensitivity() omits the survival controls for other families", {
  # mlumr() rejects `aux_by` for non-survival families using missing(), not a
  # NULL test, so naming it in the refit call at all breaks every binomial,
  # normal and Poisson fit. Assert on the argument set the refit actually
  # builds rather than paying for a sampling run.
  surv_only <- c("aux_by", "distribution", "n_knots", "knots",
                 "mspline_degree", "pred_times", "rmst_horizon",
                 "n_rmst_grid", "prior_aux", "prior_smooth")
  mk <- function(family) {
    structure(
      list(family = family, model = "spfa", link = NULL, engine = "rstan",
           data = list(covariates = "age"),
           distribution = if (family == "survival") "weibull" else NULL,
           sampling_args = list(chains = 2, iter = 600, warmup = 300),
           model_controls = list(center = TRUE, qr = FALSE),
           surv_controls = list(aux_by = ".study", n_knots = 7L,
                                mspline_degree = NA_integer_),
           priors = list(intercept = prior_normal(0, 10),
                         beta = prior_normal(0, 2.5))),
      class = "mlumr_fit"
    )
  }
  args_bin <- mlumr:::.prior_sensitivity_args(mk("binomial"),
                                              prior_normal(0, 1), FALSE)
  for (a in surv_only) {
    expect_false(a %in% names(args_bin),
                 info = paste(a, "is passed for a non-survival fit"))
  }

  args_surv <- mlumr:::.prior_sensitivity_args(mk("survival"),
                                               prior_normal(0, 1), FALSE)
  expect_true(all(surv_only %in% names(args_surv)))
  # NA controls are normalized back to NULL, which mlumr() accepts where it
  # rejects NA.
  expect_null(args_surv$mspline_degree)
})

test_that("prior_sensitivity() lets `...` override the sampler controls", {
  # `...` is documented as the way to pass sampler and backend controls, but
  # the refit named chains/iter/... itself, so any such override matched the
  # same formal twice and R refused the call before sampling.
  body_txt <- paste(deparse(body(prior_sensitivity)), collapse = "\n")
  # dots are merged into the argument list, not concatenated onto it.
  expect_match(body_txt, "call_args\\[names\\(dots\\)\\] <- dots")
  expect_false(grepl("do.call(mlumr, c(args, list(...)))", body_txt, fixed = TRUE))
})
