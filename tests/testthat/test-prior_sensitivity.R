# Tests for prior_sensitivity(): exercises the argument-validation path
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

test_that("prior_sensitivity() validates prior_beta_comparator_scales", {
  mock_fit <- structure(list(), class = c("mlumr_fit", "list"))
  expect_error(
    prior_sensitivity(mock_fit, prior_beta_scales = c(1, 2.5),
                      prior_beta_comparator_scales = c(-1, 2), verbose = FALSE),
    "positive finite"
  )
  expect_error(
    prior_sensitivity(mock_fit, prior_beta_scales = c(1, 2.5),
                      prior_beta_comparator_scales = c(1), verbose = FALSE),
    "same length"
  )
})

test_that("prior_sensitivity() refuses to vary anything but the prior", {
  # A prior-sensitivity sweep is a one-factor experiment. If `...` could change
  # the parameterization or the survival baseline, the movement across scales
  # would no longer be attributable to the prior, and the `scale` label on each
  # row would be a lie. These are rejected before any refit, so no Stan needed.
  fit <- structure(list(priors = list(beta = prior_normal(0, 2.5))),
                   class = "mlumr_fit")
  scenario <- c("center", "qr", "n_knots", "mspline_degree", "pred_times",
                "rmst_horizon", "n_rmst_grid", "aux_by", "data", "model",
                "link", "distribution", "prior_intercept", "prior_sigma",
                "prior_aux", "prior_smooth")
  for (nm in scenario) {
    args <- list(fit, prior_beta_scales = c(1, 2.5))
    args[[nm]] <- if (nm %in% c("center", "qr")) FALSE else 1
    expect_error(do.call(prior_sensitivity, args),
                 "scenario-defining", info = nm)
  }
  # `prior_beta_comparator` never reaches the dots at all: R partial-matches it
  # to the `prior_beta_comparator_scales` formal, so it is rejected one step
  # earlier by that argument's own validator. Either way it cannot silently
  # replace the swept comparator prior.
  expect_error(
    prior_sensitivity(fit, prior_beta_scales = c(1, 2.5),
                      prior_beta_comparator = prior_normal(0, 1)),
    "positive finite")
  # Sampler and backend controls are not scenario-defining and still pass the
  # dots check; this fake fit then fails later, on its absent data.
  err <- tryCatch(
    prior_sensitivity(fit, prior_beta_scales = 2.5, chains = 1, verbose = FALSE),
    error = function(e) conditionMessage(e))
  expect_false(grepl("scenario-defining", err))
})

test_that(".summarize_sensitivity records the comparator prior scale it was fitted under", {
  # prior_beta_comparator_scales pairs a comparator scale with each index scale.
  # Recording only the index scale labels a refit by half the prior it actually
  # used, which for a relaxed fit is the half that matters least: the
  # index-population estimand is driven by beta_comparator.
  fake_fit <- structure(
    list(
      family = "binomial",
      model = "relaxed",
      draws = data.frame(lor_index = rnorm(200, -0.5, 0.2),
                         lor_comparator = rnorm(200, -0.3, 0.2))
    ),
    class = c("mlumr_fit", "list")
  )
  probs <- c(0.025, 0.5, 0.975)

  paired <- mlumr:::.summarize_sensitivity(fake_fit, scale = 0.5,
                                           scale_comparator = 2.5, probs = probs)
  expect_true(all(c("scale", "scale_comparator") %in% names(paired)))
  expect_equal(unique(paired$scale), 0.5)
  expect_equal(unique(paired$scale_comparator), 2.5)
  expect_setequal(paired$parameter, c("lor_index", "lor_comparator"))

  # SPFA has no comparator coefficient prior; the column is NA and the caller
  # drops it rather than printing an all-NA column.
  spfa <- mlumr:::.summarize_sensitivity(fake_fit, scale = 1, probs = probs)
  expect_true(all(is.na(spfa$scale_comparator)))
})
