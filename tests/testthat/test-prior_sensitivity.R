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

test_that("prior_sensitivity() omits the survival controls for other families", {
  # mlumr() rejects `aux_by` for non-survival families using missing(), not a
  # NULL test, so naming it in the refit call at all breaks every binomial,
  # normal and Poisson fit. Assert on the argument set the refit actually
  # builds rather than paying for a sampling run.
  surv_only <- c("aux_by", "distribution", "n_knots", "knots",
                 "mspline_degree", "pred_times", "rmst_horizon",
                 "n_rmst_grid", "prior_aux", "prior_aux2", "prior_smooth")
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

test_that("prior_sensitivity() validates prior_beta_comparator_scales", {
  # Validation of the comparator scales only applies to a relaxed fit; an SPFA
  # fit declines the argument with a warning instead, so the stub must be
  # relaxed for the validator to be reached at all.
  mock_fit <- structure(list(model = "relaxed"), class = c("mlumr_fit", "list"))
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
  fit <- structure(list(model = "relaxed",
                        priors = list(beta = prior_normal(0, 2.5))),
                   class = "mlumr_fit")
  scenario <- c("center", "qr", "n_knots", "mspline_degree", "pred_times",
                "rmst_horizon", "n_rmst_grid", "aux_by", "data", "model",
                "link", "distribution", "prior_intercept", "prior_sigma",
                "prior_aux", "prior_aux2", "prior_smooth")
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


test_that("prior_sensitivity replays a parametric survival baseline", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # surv_controls records mspline_degree as NULL for a parametric baseline, so
  # every reader gets "not applicable" right rather than only the one that
  # normalizes NA. It was NA once, and because mlumr() rejects NA where it
  # accepts NULL and `%||%` does not catch NA, the refit then aborted with
  # "`mspline_degree` must be a non-negative integer" on every parametric
  # survival fit. prior_sensitivity() still normalizes NA for older fits.
  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)
  fit <- suppressWarnings(suppressMessages(
    fit_survival_test(dat, distribution = "weibull", chains = 1,
                      iter = 120, warmup = 60)
  ))
  expect_null(fit$surv_controls$mspline_degree)

  out <- suppressWarnings(suppressMessages(
    prior_sensitivity(fit, prior_beta_scales = c(1, 2.5), verbose = FALSE,
                      chains = 1, iter = 120, warmup = 60)
  ))
  expect_s3_class(out, "data.frame")
  expect_setequal(out$scale, c(1, 2.5))
  expect_true(all(is.finite(out$mean)))
})

test_that("prior_sensitivity does not forward aux_by to non-survival refits", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # The refit goes through do.call(mlumr, base_args). mlumr() guards `aux_by`
  # with !missing(), which is TRUE even when the value passed is NULL, so a
  # survival-only argument cannot be neutralized by setting it to NULL: it has
  # to be left out of the argument list entirely. Forwarding it for a binomial
  # fit aborts with "`aux_by` is only used for family = 'survival'".
  set.seed(2026)
  n <- 60
  ipd <- data.frame(trt = "A", y = rbinom(n, 1, 0.4), age = rnorm(n))
  agd <- data.frame(trt = "B", n = 100, r = 35, age_mean = 0.2, age_sd = 1)
  dat <- suppressWarnings(suppressMessages(add_integration(
    combine_data(
      set_ipd(ipd, "trt", outcome = "y", covariates = "age"),
      set_agd(agd, "trt", outcome_n = "n", outcome_r = "r",
              cov_means = "age_mean", cov_sds = "age_sd",
              cov_types = "continuous")
    ),
    n_int = 8, age = distr(qnorm, mean = age_mean, sd = age_sd), verbose = FALSE
  )))
  fit <- suppressWarnings(suppressMessages(
    mlumr(dat, model = "spfa", chains = 1, iter = 120, warmup = 60,
          seed = 2026, refresh = 0, verbose = FALSE)
  ))

  out <- suppressWarnings(suppressMessages(
    prior_sensitivity(fit, prior_beta_scales = c(1, 2.5), verbose = FALSE,
                      chains = 1, iter = 120, warmup = 60)
  ))
  expect_setequal(out$scale, c(1, 2.5))
  # SPFA has no comparator coefficient prior, so the paired column is dropped.
  expect_null(out$scale_comparator)
})
