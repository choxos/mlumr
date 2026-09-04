# Regression tests for the effect-scale API, the native logit-normal
# parameterization, the second generalized-gamma auxiliary prior, and the
# prediction-time grid snapping. All pure R: none of these fit a model.

# ---- the survival scalar effect selector is literal ------------------------

test_that("each survival fit has exactly one legitimate scalar effect name", {
  expect_equal(mlumr:::.surv_scalar_effect_name("HR"), "hr")
  expect_equal(mlumr:::.surv_scalar_effect_name("TR"), "tr")
  expect_equal(mlumr:::.surv_scalar_effect_name("EXP_DELTA_ETA"), "exp_delta_eta")
  # A label the mapping does not know must not fall through to a default: that
  # would silently name one estimand with another's selector, which is the whole
  # failure mode this mapping exists to prevent.
  expect_error(mlumr:::.surv_scalar_effect_name("LOG_HR"), "Unrecognized")
})

test_that("asking for the wrong survival scale names the one the fit has", {
  err <- mlumr:::.surv_effect_scale_error
  # PH fit asked for a time ratio.
  msg <- err("tr", "HR", "hr", stratified = FALSE,
             valid_effects = c("all", "hr", "rmstd", "rmstr"))
  expect_match(msg, "proportional-hazards")
  expect_match(msg, 'effect = "hr"', fixed = TRUE)

  # Shared-shape AFT fit asked for a hazard ratio. This is the case that used to
  # succeed and hand back a time ratio labeled TR.
  msg <- err("hr", "TR", "tr", stratified = FALSE,
             valid_effects = c("all", "tr", "rmstd", "rmstr"))
  expect_match(msg, "time ratio")
  expect_match(msg, 'effect = "tr"', fixed = TRUE)

  # Stratified and relaxed AFT keep their distinct explanations.
  expect_match(err("hr", "EXP_DELTA_ETA", "exp_delta_eta", TRUE, character(0)),
               "own AFT shape")
  expect_match(err("tr", "EXP_DELTA_ETA", "exp_delta_eta", FALSE, character(0)),
               "relaxed fit")

  # And the mirror image: the location contrast is not the right name for a fit
  # that has a real hazard ratio or time ratio.
  msg <- err("exp_delta_eta", "TR", "tr", stratified = FALSE,
             valid_effects = c("all", "tr", "rmstd", "rmstr"))
  expect_match(msg, "shapes differ by study")
  expect_match(msg, "relaxed AFT")

  # A name that is not a scalar effect at all falls back to the plain list.
  expect_match(err("nonsense", "HR", "hr", FALSE, c("all", "hr", "rmstd")),
               "must be one of")
})

# ---- native logit-normal parameters are validated --------------------------

test_that("the native logit-normal parameterization rejects unusable sigma", {
  # One invalid `sigma` used to give three different answers: dlogitnorm()
  # returned Inf at sigma = 0, plogitnorm() returned 1, qlogitnorm() returned
  # 0.5, and all three were finite and unflagged.
  for (f in list(dlogitnorm, plogitnorm, qlogitnorm)) {
    expect_error(f(0.5, mu = 0, sigma = 0), "strictly positive")
    expect_error(f(0.5, mu = 0, sigma = -1), "strictly positive")
    expect_error(f(0.5, mu = 0, sigma = Inf), "finite")
    expect_error(f(0.5, mu = Inf, sigma = 1), "finite")
    expect_error(f(0.5, mu = 0, sigma = NA), "must not be missing")
    expect_error(f(0.5, mu = 0, sigma = "1"), "numeric")
  }
  # Partial recycling paired each value with the wrong parameter.
  expect_error(dlogitnorm(0.5, mu = c(0, 1), sigma = c(1, 1, 1)), "same length")
  expect_error(plogitnorm(0.5, mu = c(0, 1), sigma = c(1, 1, 1)), "same length")

  # The valid paths are untouched, including scalar-against-vector recycling.
  expect_equal(qlogitnorm(0.5, mu = 0, sigma = 1), 0.5)
  expect_equal(plogitnorm(0.5, mu = 0, sigma = 1), 0.5)
  expect_equal(dlogitnorm(0.5, mu = 0, sigma = c(1, 2)),
               stats::dnorm(0, 0, c(1, 2)) / 0.25)
  expect_equal(dlogitnorm(numeric(0), mu = 0, sigma = 1), numeric(0))
})

# ---- the second generalized-gamma auxiliary prior ---------------------------

test_that("prior_aux2 reaches Stan and defaults to prior_aux", {
  skip_if_not_installed("splines2")
  dat <- sim_survival_data(n_ipd = 40, n_agd = 50, n_int = 8)
  info <- mlumr:::.survival_distribution_info("gengamma")
  horizon <- min(max(dat$ipd$data$.time), max(dat$agd$pseudo_ipd$.time))

  build_surv <- mlumr:::.build_stan_data_survival
  build <- function(...) {
    build_surv(list(), dat, info, pred_times = horizon, n_knots = 7,
               knots = NULL, rmst_horizon = horizon,
               prior_smooth = default_prior_smooth(), n_strata = 1L, ...)
  }

  # Unspecified: the two auxiliaries share one specification, which is what the
  # package did before `prior_aux2` existed.
  sd_only <- build(prior_aux = prior_normal(0, 2))
  expect_equal(sd_only$prior_aux2_location, sd_only$prior_aux_location)
  expect_equal(sd_only$prior_aux2_scale, sd_only$prior_aux_scale)

  # Specified: Stan has always read the aux2 slots separately; they were simply
  # being fed the same numbers.
  both <- build(prior_aux = prior_normal(0, 2),
                prior_aux2 = prior_normal(0, 0.5))
  expect_equal(both$prior_aux_scale, 2)
  expect_equal(both$prior_aux2_scale, 0.5)
  expect_false(isTRUE(all.equal(both$prior_aux_scale, both$prior_aux2_scale)))

  # A different family, not only a different scale, must survive the trip.
  fam <- build(prior_aux = prior_normal(0, 2),
               prior_aux2 = prior_exponential(1))
  expect_false(identical(fam$prior_aux_dist, fam$prior_aux2_dist))
})

# ---- snapped prediction times are announced --------------------------------

test_that("prediction times moved onto the fitted grid are reported", {
  grid <- c(1, 2, 3, 4, 5)
  sel <- mlumr:::.surv_time_selection

  # An exact hit is not an approximation and must stay quiet.
  expect_silent(idx <- sel(c(2, 4), grid))
  expect_equal(idx, c(2L, 4L))

  # A moved time says what was asked for and what was used.
  expect_message(sel(c(2.4, 4.9), grid), "2.4 -> 2")
  expect_message(sel(c(2.4, 4.9), grid), "4.9 -> 5")

  # Two requested times collapsing to one grid point silently produced a result
  # with fewer rows than the request had times.
  expect_message(sel(c(2.1, 2.2), grid), "share a grid point")
  expect_equal(suppressMessages(sel(c(2.1, 2.2), grid)), 2L)

  # The whole-grid default is not a request and is not reported on.
  expect_silent(sel(NULL, grid))

  # A time asked for twice is a duplicate request, not a grid approximation.
  # Deduplicating it changes the row count but not the answer, so advising a
  # refit with a grid that already contains that exact time is wrong.
  msg_of <- function(times) {
    capture.output(sel(times, grid), type = "message")
  }
  expect_message(sel(c(2, 2), grid), "Duplicate prediction time")
  expect_false(any(grepl("Refit with", msg_of(c(2, 2)))))
  # Two DISTINCT times landing on one point does lose information, so that case
  # keeps the refit advice.
  expect_true(any(grepl("Refit with", msg_of(c(2.1, 2.2)))))
})

# ---- the mlumr() signature does not rebind positional arguments -------------

test_that("new formals are appended, so positional calls keep their meaning", {
  # A formal inserted in the middle silently rebinds every argument after it:
  # `prior_aux2` first landed next to `prior_aux`, which moved `prior_smooth`,
  # `n_knots` and everything downstream by one position. Pin the prefix so the
  # next insertion fails here rather than in a user's script.
  f <- names(formals(mlumr))
  # This prefix is the released signature, read off the base branch, not a
  # transcription of whatever the current file happens to say.
  released <- paste0(
                     "data model link prior_intercept prior_beta prior_sigma ",
                     "distribution prior_aux prior_smooth n_knots knots ",
                     "mspline_degree aux_by pred_times rmst_horizon n_rmst_grid ",
                     "center qr chains iter")
  released_prefix <- strsplit(released, " ")[[1]]
  expect_identical(f[seq_along(released_prefix)], released_prefix)
  # Arguments added after 0.1.0 live at the end, ahead of `...` only.
  expect_identical(tail(f, 3), c("prior_beta_comparator", "prior_aux2", "..."))
})

# ---- prior_sensitivity() quantile column names ------------------------------

test_that("quantile columns are named by percentage and cannot collide", {
  # `round(100 * probs)` labelled the defaults `q2` / `q98` and mapped 0.024 and
  # 0.025 both to `q2`, where the second silently overwrote the first.
  draws <- data.frame(lor_index = stats::qnorm(seq(0.001, 0.999, length.out = 400)),
                      lor_comparator = stats::qnorm(seq(0.001, 0.999,
                                                        length.out = 400)))
  fit <- structure(list(draws = draws, family = "binomial", model = "spfa"),
                   class = "mlumr_fit")
  out <- mlumr:::.summarize_sensitivity(fit, scale = 1, probs = c(0.025, 0.5, 0.975))
  expect_true(all(c("q2.5", "q50", "q97.5") %in% names(out)))
  expect_false(any(c("q2", "q98") %in% names(out)))

  # Nearby probabilities must stay distinct rather than overwriting each other.
  out2 <- mlumr:::.summarize_sensitivity(fit, scale = 1, probs = c(0.024, 0.025))
  expect_true(all(c("q2.4", "q2.5") %in% names(out2)))
  expect_false(isTRUE(all.equal(out2$q2.4[1], out2$q2.5[1])))
})

test_that("prior_aux2 warns when the distribution has no second auxiliary", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # Every survival fit defaulted and validated `prior_aux2`, but only the
  # generalized gamma has a parameter for it to reach. A Weibull fit accepted a
  # supplied value, applied it to nothing, and left no trace in prior_summary().
  set.seed(2026)
  d <- sim_survival_data(n_ipd = 40, n_agd = 50, n_int = 8)
  # A short fit emits several sampling warnings, so capture by message rather
  # than asserting on "the" warning.
  w <- NULL
  run <- function() {
    mlumr(d, distribution = "weibull", prior_aux2 = prior_normal(0, 1),
          chains = 1, iter = 10, warmup = 5, refresh = 0, seed = 2026,
          verbose = FALSE)
  }
  capture <- function(x) {
    if (grepl("prior_aux2", conditionMessage(x))) w <<- conditionMessage(x)
    invokeRestart("muffleWarning")
  }
  withCallingHandlers(try(suppressMessages(run()), silent = TRUE),
                      warning = capture)
  expect_false(is.null(w))
  expect_match(w, "gengamma")
  expect_match(w, "ignored")
})

test_that("an ignored prior_aux2 is not validated", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # An argument documented as ignored has to be ignored. A well-formed value in
  # a position it does not apply to was dropped quietly while a MALFORMED one in
  # the same position aborted the fit, so the same argument carried two
  # contracts depending on a distribution it never reaches. A Weibull fit has
  # one auxiliary parameter, so `prior_aux2` applies to nothing here and the
  # garbage below must not decide whether the fit runs.
  set.seed(2026)
  d <- sim_survival_data(n_ipd = 40, n_agd = 50, n_int = 8)
  run <- function() {
    mlumr(d, distribution = "weibull", prior_aux2 = "not a prior at all",
          chains = 1, iter = 10, warmup = 5, refresh = 0, seed = 2026,
          verbose = FALSE)
  }
  # Unconditional, so the assertion runs whether or not a short fit errors for
  # some unrelated reason. A conditional version registered no expectation at
  # all when the fit succeeded, and testthat reported the test as skipped.
  blamed_prior_aux2 <- FALSE
  warned <- FALSE
  withCallingHandlers(
    suppressMessages(tryCatch(run(), error = function(e) {
      blamed_prior_aux2 <<- grepl("prior_aux2", conditionMessage(e),
                                  fixed = TRUE)
    })),
    warning = function(w) {
      if (grepl("prior_aux2", conditionMessage(w), fixed = TRUE)) warned <<- TRUE
      invokeRestart("muffleWarning")
    })
  # The value really was in the ignored position ...
  expect_true(warned)
  # ... and its contents did not decide whether the fit ran.
  expect_false(blamed_prior_aux2)
})

# ---- the tie-aggregation precondition rejects an unrecognized shape ---------

test_that("the agd_count guard only passes a genuinely expanded likelihood", {
  g <- mlumr:::.assert_agd_loglik_per_observation
  mk <- function(cnt, n_cols) {
    draws <- as.data.frame(matrix(0, nrow = 2, ncol = n_cols))
    names(draws) <- paste0("log_lik_agd[", seq_len(n_cols), "]")
    structure(list(stan_data = list(agd_count = cnt), draws = draws),
              class = "mlumr_fit")
  }
  cnt <- c(2L, 1L, 4L)                     # 3 retained rows, 7 observations
  # Collapsed: one column per retained row. Fail closed.
  expect_error(g(mk(cnt, 3L)), "collapsed tied aggregate rows")
  # Expanded: one column per observation. Pass.
  expect_true(g(mk(cnt, 7L)))
  # Anything else is an unrecognized state. It used to be waved through as
  # "already expanded" purely because the count differed from the row count.
  expect_error(g(mk(cnt, 5L)), "neither one per")
  # No multiplicities at all is the ordinary case and stays silent.
  expect_true(g(mk(rep(1L, 3), 3L)))

  # A multiplicity is a count of observations. A fractional one passed the
  # expanded-shape test through `sum(cnt)` while `.agd_center_weights()`
  # truncated it with `as.integer()`, so the likelihood columns and the arm map
  # disagreed; `all(cnt <= 1)` also returned early on fractional and zero counts.
  expect_error(g(mk(c(1.5, 1.5), 3L)), "whole-number")
  expect_error(g(mk(c(0.5, 0.5), 1L)), "whole-number")
  expect_error(g(mk(c(0, 2), 2L)), "whole-number")
  expect_error(g(mk(c(2, NA), 2L)), "whole-number")
  expect_error(g(mk(c(2, Inf), 2L)), "whole-number")
})

test_that("prior_aux2 reaches Stan through the public mlumr() call", {
  skip_on_cran()
  skip_if_not_installed("rstan")
  # The builder test above calls `.build_stan_data_survival()` directly, which
  # cannot catch a break in the threading between mlumr(), the stan-data
  # assembler and the survival branch. This exercises the public path, which is
  # also what the signature move had to leave working.
  set.seed(2026)
  d <- sim_survival_data(n_ipd = 40, n_agd = 50, n_int = 8)
  fit_gg <- function(...) {
    suppressWarnings(suppressMessages(
      mlumr(d, distribution = "gengamma", chains = 1, iter = 10, warmup = 5,
            refresh = 0, seed = 2026, verbose = FALSE, ...)
    ))
  }

  a <- fit_gg(prior_aux = prior_normal(0, 2))
  expect_equal(a$stan_data$prior_aux_scale, 2)
  expect_equal(a$stan_data$prior_aux2_scale, 2)   # unspecified reuses prior_aux

  b <- fit_gg(prior_aux = prior_normal(0, 2), prior_aux2 = prior_normal(0, 0.5))
  expect_equal(b$stan_data$prior_aux_scale, 2)
  expect_equal(b$stan_data$prior_aux2_scale, 0.5)

  # And prior_summary() names what each auxiliary is, since "auxiliary 2" alone
  # does not tell a reader what scale to calibrate the prior on.
  txt <- capture.output(prior_summary(b))
  expect_true(any(grepl("gengamma sigma", txt, fixed = TRUE)))
  expect_true(any(grepl("k = 1 / Q^2", txt, fixed = TRUE)))
})

test_that("exponential-aft keeps `tr` under aux_by = '.study'", {
  # `aux_by = ".study"` only stratifies a baseline that HAS a shape. The
  # exponential AFT has none, so an SPFA fit keeps a genuine time ratio and the
  # `tr` selector, where every shape-bearing AFT would move to
  # `exp_delta_eta`. The documentation used to send these users to a selector
  # that then errors.
  mk <- function(dist, model = "spfa") {
    structure(list(surv_info = mlumr:::.survival_distribution_info(dist),
                   stan_data = list(n_strata = 2L), model = model,
                   pred_times = c(1, 2, 3)), class = "mlumr_fit")
  }
  name_of <- function(fit) {
    mlumr:::.surv_scalar_effect_name(mlumr:::.surv_scalar_label(fit)$label)
  }
  expect_equal(mk("exponential-aft")$surv_info$n_aux, 0L)
  expect_equal(name_of(mk("exponential-aft")), "tr")
  # The shape-bearing AFTs do move, which is what makes the exception one.
  expect_equal(name_of(mk("weibull-aft")), "exp_delta_eta")
  expect_equal(name_of(mk("lognormal")), "exp_delta_eta")
  # And a relaxed exponential-aft still loses the time ratio, because there the
  # covariate term fails to cancel for a reason unrelated to the shape.
  expect_equal(name_of(mk("exponential-aft", model = "relaxed")),
               "exp_delta_eta")
})
