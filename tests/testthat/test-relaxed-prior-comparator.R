# Tests for the relaxed-model comparator prior (prior_beta_comparator) and the
# index-population note emitted by marginal_effects(). The note logic and the
# argument validation are pure R and run on CRAN; the end-to-end fitting
# assertions are Stan-gated.

# ---- .relaxed_index_note (pure R, no Stan) --------------------------------

test_that(".relaxed_index_note fires for relaxed index/both, silent otherwise", {
  relaxed <- structure(list(model = "relaxed"), class = c("mlumr_fit", "list"))
  spfa <- structure(list(model = "spfa"), class = c("mlumr_fit", "list"))
  old <- options(mlumr.quiet_relaxed_index = FALSE)
  on.exit(options(old), add = TRUE)

  # The note explains why the index-population effect is fragile without
  # claiming that every nonlinear aggregate design has the same rank. It no
  # longer says "prefer the comparator-population effect", which contradicted
  # the index population being the decision-relevant target.
  expect_message(mlumr:::.relaxed_index_note(relaxed, "index"),
                 "model- and design-dependent")
  expect_message(mlumr:::.relaxed_index_note(relaxed, "both"),
                 "report both populations")
  expect_silent(mlumr:::.relaxed_index_note(relaxed, "comparator"))
  expect_silent(mlumr:::.relaxed_index_note(spfa, "index"))
  expect_silent(mlumr:::.relaxed_index_note(spfa, "both"))
})

test_that(".relaxed_contraction reports per-covariate posterior contraction", {
  # 1 - (posterior sd / prior sd)^2: near 1 means the data moved the
  # coefficient, near 0 means it is still the prior. This is the identification
  # question that matters for index-population transport, and an event count
  # cannot answer it.
  n <- 4000
  set.seed(2026)
  fit <- structure(list(
    model = "relaxed",
    data = list(covariates = c("learned", "unlearned")),
    stan_data = list(prior_beta_comparator_sd = c(2.5, 2.5)),
    draws = data.frame(
      "beta_comparator[1]" = stats::rnorm(n, 0, 0.5),   # strongly contracted
      "beta_comparator[2]" = stats::rnorm(n, 0, 2.5),   # not contracted
      check.names = FALSE)
  ), class = c("mlumr_fit", "list"))

  ct <- mlumr:::.relaxed_contraction(fit)
  expect_s3_class(ct, "data.frame")
  expect_equal(ct$covariate, c("learned", "unlearned"))
  expect_gt(ct$contraction[1], 0.9)                 # sd 0.5 vs prior 2.5
  expect_lt(abs(ct$contraction[2]), 0.15)           # posterior ~ prior
  # The note names the weakly identified coefficient.
  old <- options(mlumr.quiet_relaxed_index = FALSE)
  on.exit(options(old), add = TRUE)
  expect_message(mlumr:::.relaxed_index_note(fit, "index"), "unlearned")
})

test_that(".relaxed_contraction returns NULL when inputs are unavailable", {
  # Degrades gracefully rather than erroring: the note then omits the detail.
  bare <- structure(list(model = "relaxed"), class = c("mlumr_fit", "list"))
  expect_null(mlumr:::.relaxed_contraction(bare))
  spfa <- structure(list(model = "spfa"), class = c("mlumr_fit", "list"))
  expect_null(mlumr:::.relaxed_contraction(spfa))
})

test_that("options(mlumr.quiet_relaxed_index) suppresses the note", {
  relaxed <- structure(list(model = "relaxed"), class = c("mlumr_fit", "list"))
  old <- options(mlumr.quiet_relaxed_index = TRUE)
  on.exit(options(old), add = TRUE)
  expect_silent(mlumr:::.relaxed_index_note(relaxed, "index"))
})

# ---- prior_beta_comparator validation (pure R, no Stan) -------------------

make_binary_dat <- function(n_int = 16) {
  set.seed(2026)
  n <- 80
  x1 <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.3 + 0.8 * x1))
  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(trt = "B", n_total = 200, n_events = 70, x1_mean = 0.4)
  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  add_integration(dat, n_int = n_int, x1 = distr(qbern, prob = x1_mean))
}

test_that("prior_beta_comparator rejects exponential priors (before fitting)", {
  dat <- make_binary_dat()
  expect_error(
    mlumr(dat, model = "relaxed", prior_beta_comparator = prior_exponential(1)),
    "exponential"
  )
})

test_that("prior_beta_comparator rejects malformed input (before fitting)", {
  dat <- make_binary_dat()
  expect_error(
    mlumr(dat, model = "relaxed", prior_beta_comparator = 42),
    "prior list"
  )
})

# ---- end-to-end: prior_beta_comparator reaches Stan (Stan-gated) ----------

test_that("prior_beta_comparator is honored and surfaced (relaxed fit)", {
  skip_on_cran()
  skip_if_not_installed("rstan")

  dat <- make_binary_dat(n_int = 32)

  # SPFA ignores the comparator prior, with a warning.
  expect_warning(
    mlumr(dat, model = "spfa", prior_beta_comparator = prior_normal(0, 1),
          chains = 1, iter = 300, warmup = 150, refresh = 0, seed = 2026,
          verbose = FALSE),
    "ignored for the SPFA model"
  )

  fit_tight <- mlumr(dat, model = "relaxed",
                     prior_beta_comparator = prior_normal(0, 0.5),
                     chains = 2, iter = 600, warmup = 300, refresh = 0,
                     seed = 2026, verbose = FALSE)

  # prior_summary surfaces the comparator block and flags it user-specified.
  expect_true(!is.null(fit_tight$priors$beta_comparator_resolved))
  expect_true(isTRUE(fit_tight$priors$beta_comparator_resolved$user_specified))
  out <- capture.output(prior_summary(fit_tight))
  expect_true(any(grepl("Comparator regression coefficients", out)))

  # A tighter comparator prior should not widen the index-population CI versus
  # a wide one (regularization narrows the extrapolated estimand).
  fit_wide <- mlumr(dat, model = "relaxed",
                    prior_beta_comparator = prior_normal(0, 5),
                    chains = 2, iter = 600, warmup = 300, refresh = 0,
                    seed = 2026, verbose = FALSE)
  suppressMessages({
    w_tight <- marginal_effects(fit_tight, effect = "lor", population = "index")
    w_wide <- marginal_effects(fit_wide, effect = "lor", population = "index")
  })
  width_tight <- w_tight$q97.5 - w_tight$q2.5
  width_wide <- w_wide$q97.5 - w_wide$q2.5
  expect_lte(width_tight, width_wide * 1.05)
})

# ---- the prior REQUESTED is the prior that reaches Stan --------------------

test_that("comparator prior family, df, mean and scale all reach the Stan data", {
  # Passing only the comparator mean/scale while reusing the index family or df
  # would fit a different model from the one the user asked for, and printed
  # metadata (prior_summary) would still show the requested prior. Inspect the
  # actual Stan data instead, which is what the sampler sees.
  dat <- make_binary_dat(n_int = 32)
  build <- function(...) {
    mlumr:::.mlumr_build_stan_data(
      data = dat, family = "binomial",
      link_info = mlumr:::check_link("binomial", "logit"),
      prior_intercept = default_prior_intercept(),
      prior_sigma = default_prior_sigma(),
      model = "relaxed", ...)$stan_data
  }

  # Deliberately distinguishable: different family, different df, different
  # mean, different scale. Autoscaling off so the scales are compared as given.
  sd_diff <- build(
    prior_beta = prior_normal(0, 2.5, autoscale = FALSE),
    prior_beta_comparator = prior_student_t(1.5, 0.4, df = 7,
                                            autoscale = FALSE))

  expect_equal(sd_diff$prior_beta_dist, 0L)             # normal
  expect_equal(sd_diff$prior_beta_comparator_dist, 1L)  # student_t
  expect_equal(sd_diff$prior_beta_comparator_df, 7)
  expect_false(identical(sd_diff$prior_beta_df,
                         sd_diff$prior_beta_comparator_df))
  expect_equal(as.numeric(sd_diff$prior_beta_comparator_mean),
               rep(1.5, dat$n_covariates))
  expect_equal(as.numeric(sd_diff$prior_beta_comparator_sd),
               rep(0.4, dat$n_covariates))
  # The index block is untouched by the comparator specification.
  expect_equal(as.numeric(sd_diff$prior_beta_mean), rep(0, dat$n_covariates))
  expect_equal(as.numeric(sd_diff$prior_beta_sd), rep(2.5, dat$n_covariates))

  # Omitting the comparator prior falls back to the index prior exactly.
  sd_same <- build(prior_beta = prior_student_t(0, 2.5, df = 3,
                                                autoscale = FALSE))
  expect_equal(sd_same$prior_beta_comparator_dist, sd_same$prior_beta_dist)
  expect_equal(sd_same$prior_beta_comparator_df, sd_same$prior_beta_df)
  expect_equal(as.numeric(sd_same$prior_beta_comparator_mean),
               as.numeric(sd_same$prior_beta_mean))
  expect_equal(as.numeric(sd_same$prior_beta_comparator_sd),
               as.numeric(sd_same$prior_beta_sd))
})

test_that("every relaxed Stan model reads the comparator prior family and df", {
  # The R side can pass all four fields and still be ignored if a Stan model
  # hands the INDEX dist/df to log_prior_vector() for beta_comparator. Check the
  # model sources directly: this is the step that decides which prior is fitted.
  stan_dir <- system.file("stan", package = "mlumr")
  skip_if(stan_dir == "" || !dir.exists(stan_dir))
  files <- list.files(stan_dir, pattern = "_relaxed\\.stan$", full.names = TRUE)
  expect_gt(length(files), 0L)
  for (f in files) {
    src <- paste(readLines(f, warn = FALSE), collapse = " ")
    src <- gsub("[[:space:]]+", " ", src)
    expect_match(
      src,
      paste0("log_prior_vector\\( *beta_comparator, *",
             "prior_beta_comparator_mean, *prior_beta_comparator_sd, *",
             "prior_beta_comparator_dist, *prior_beta_comparator_df *\\)"),
      info = basename(f))
  }
})
