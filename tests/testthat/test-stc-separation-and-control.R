# Two guards that a fit can walk straight past unless the test is written
# against the symptom rather than against the way the symptom usually shows up.

test_that("a separated outcome model is refused at either boundary", {
  eps <- .Machine$double.eps^0.5

  # An arm with no events: every fitted probability sits at 0.
  set.seed(2026)
  no_events <- data.frame(y = rep(0L, 100), x = stats::rnorm(100))
  fit_zero <- stats::glm(y ~ x, family = stats::binomial(), data = no_events)
  # The fitting reports nothing wrong, which is the whole difficulty.
  expect_true(fit_zero$converged)
  expect_true(all(is.finite(stats::coef(fit_zero))))
  expect_error(mlumr:::.stc_refuse_separation(fit_zero), "separated")

  # A covariate that perfectly separates: the two groups go to OPPOSITE
  # boundaries, so no single boundary holds all of them. A test written as
  # `all(mu < eps) || all(mu > 1 - eps)` accepts this fit, and it is the
  # ordinary presentation of separation rather than an exotic one.
  split <- data.frame(y = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L),
                      x = c(0, 0, 0, 0, 1, 1, 1, 1))
  fit_split <- suppressWarnings(
    stats::glm(y ~ x, family = stats::binomial(), data = split)
  )
  mu <- stats::fitted(fit_split)
  expect_false(all(mu < eps) || all(mu > 1 - eps))
  expect_true(all(mu < eps | mu > 1 - eps))
  expect_error(mlumr:::.stc_refuse_separation(fit_split), "separated")
})

test_that("an ordinary fit and a genuinely rare event are still accepted", {
  set.seed(2026)
  n <- 200
  x <- stats::rnorm(n)

  ordinary <- data.frame(y = stats::rbinom(n, 1, stats::plogis(0.3 * x)), x = x)
  fit_ok <- stats::glm(y ~ x, family = stats::binomial(), data = ordinary)
  expect_silent(mlumr:::.stc_refuse_separation(fit_ok))

  # A small rate is not separation. The test is on the boundary rather than on
  # smallness precisely so this keeps fitting.
  rare <- data.frame(y = stats::rbinom(n, 1, stats::plogis(-4 + 0.3 * x)), x = x)
  skip_if(sum(rare$y) == 0L, "draw produced no events; that is the other case")
  fit_rare <- stats::glm(y ~ x, family = stats::binomial(), data = rare)
  expect_silent(mlumr:::.stc_refuse_separation(fit_rare))

  # Families without a probability boundary are untouched.
  pois <- data.frame(y = stats::rpois(n, 2), x = x)
  fit_pois <- stats::glm(y ~ x, family = stats::poisson(), data = pois)
  expect_silent(mlumr:::.stc_refuse_separation(fit_pois))
})

test_that("a caller's sampler control is merged, never forwarded beside ours", {
  merge <- mlumr:::.merge_sampler_control

  # Nothing supplied: mlumr's own settings, and no `control` left in the dots
  # to collide with them.
  bare <- merge(0.8, 10, list())
  expect_equal(bare$control, list(adapt_delta = 0.8, max_treedepth = 10))
  expect_false("control" %in% names(bare$dots))

  # Supplied: the caller's entry wins, and settings they did not name survive.
  given <- merge(0.8, 10, list(control = list(adapt_delta = 0.99)))
  expect_equal(given$control$adapt_delta, 0.99)
  expect_equal(given$control$max_treedepth, 10)
  expect_false("control" %in% names(given$dots))

  # `control = NULL` is an element that is present and NULL. Testing the value
  # rather than the name left it in the dots, and `rstan::sampling()` then got
  # `control` twice: the collision this function exists to prevent.
  explicit_null <- merge(0.8, 10, list(control = NULL, chain_id = 3))
  expect_equal(explicit_null$control, list(adapt_delta = 0.8, max_treedepth = 10))
  expect_false("control" %in% names(explicit_null$dots))
  # Everything else the caller passed is still forwarded.
  expect_equal(explicit_null$dots$chain_id, 3)

  # A non-list is refused rather than silently merged.
  expect_error(merge(0.8, 10, list(control = "adapt_delta = 0.99")),
               "must be a list")
})

test_that("treedepth hits are counted against the limit the sampler ran under", {
  merge <- mlumr:::.merge_sampler_control
  count <- mlumr:::.count_treedepth_hits

  # Two chains whose transitions stopped at depths 9 and 10.
  sp <- list(
    cbind(treedepth__ = c(9, 10, 10, 8), divergent__ = c(0, 0, 0, 0)),
    cbind(treedepth__ = c(10, 7, 9, 10), divergent__ = c(0, 0, 0, 0))
  )

  # mlumr's argument says 15; the caller lowered it to 10 through `control`.
  # The sampler ran at 10, so four transitions hit the maximum. Counting
  # against 15 reports none of them, which is the reading that made a capped
  # run look clean.
  merged <- merge(0.8, 15, list(control = list(max_treedepth = 10)))
  expect_equal(merged$control$max_treedepth, 10)
  expect_equal(count(sp, merged$control$max_treedepth), 4)
  expect_equal(count(sp, 15), 0)

  # With no override the merged limit is the argument, so nothing changes for
  # the ordinary path.
  plain <- merge(0.8, 10, list())
  expect_equal(count(sp, plain$control$max_treedepth), 4)
})

test_that("the deep-tail gamma series is normalized without a cancelling subtraction", {
  # The series sums terms relative to the first and divides by gamma(k + 1) at
  # the end. Carrying -log(k) inside the sum and finishing with -lgamma(k)
  # subtracts two quantities that both grow like -log(k) as the shape shrinks,
  # and the rounding in that difference swamps the answer.
  series <- function(k, log_x, safe) {
    log_term <- if (safe) 0 else -log(k)
    log_total <- log_term
    for (i in 1:300) {
      log_term <- log_term + log_x - log(k + i)
      m <- max(log_total, log_term)
      log_total <- m + log(exp(log_total - m) + exp(log_term - m))
      if (exp(log_term - log_total) < 1e-14) break
    }
    -exp(log_x) + k * log_x - lgamma(if (safe) k + 1 else k) + log_total
  }

  # At this shape the two terms are each 41.4465 and the answer is -1e-15, so
  # the cancelling form returns exactly 0 and the caller reads a survival of
  # zero: an artificial wall in a region the sampler can reach, since aux2 is
  # declared only `<lower=0>`.
  expect_gte(series(1e-18, -1000, safe = FALSE), 0)
  expect_lt(series(1e-18, -1000, safe = TRUE), 0)
  expect_equal(series(1e-18, -1000, safe = TRUE), -1e-15, tolerance = 1e-3)

  # The two agree once the shape is large enough for the subtraction to be
  # harmless, so this is a fix at the boundary and not a change of definition.
  for (k in c(1e-8, 1e-3, 0.5, 3)) {
    expect_equal(series(k, -1000, safe = TRUE), series(k, -1000, safe = FALSE),
                 tolerance = 1e-9)
  }
  # And where x does not underflow the series still matches R's own function.
  for (k in c(0.5, 1, 3)) {
    expect_equal(series(k, -1, safe = TRUE),
                 stats::pgamma(exp(-1), shape = k, lower.tail = TRUE,
                               log.p = TRUE),
                 tolerance = 1e-12)
  }

  # The Stan source carries the safe form, which is what the fitted models use.
  stan <- testthat::test_path("..", "..", "inst", "stan", "include",
                              "survival_functions.stan")
  skip_if_not(file.exists(stan), "run from a source checkout")
  src <- paste(readLines(stan, warn = FALSE), collapse = "\n")
  body <- sub(".*real log_gamma_p_series\\(real k, real log_x\\) \\{", "", src)
  body <- sub("\\n\\}.*", "", body)
  expect_true(grepl("lgamma(k + 1)", body, fixed = TRUE))
  expect_false(grepl("lgamma(k))", body, fixed = TRUE))
})

test_that("the fit records the sampler settings that were in force", {
  # `check_diagnostics()` reads `sampling_args` to tell a user which limit to
  # raise. Once the treedepth count moved to the merged limit, quoting the
  # argument there would have advised raising a number the caller had already
  # raised, so the backend reports what it ran under and the fit stores that.
  merged <- mlumr:::.merge_sampler_control(
    0.8, 15, list(control = list(adapt_delta = 0.99, max_treedepth = 10))
  )
  expect_equal(merged$control$adapt_delta, 0.99)
  expect_equal(merged$control$max_treedepth, 10)

  # The fallback path: a backend that reports nothing leaves the arguments in
  # place, which is the cmdstanr case, where the two cannot differ.
  `%||%` <- function(a, b) if (is.null(a)) b else a
  no_report <- list()
  expect_equal(no_report$adapt_delta_used %||% 0.8, 0.8)
  expect_equal(no_report$max_treedepth_used %||% 15, 15)

  reported <- list(adapt_delta_used = 0.99, max_treedepth_used = 10)
  expect_equal(reported$adapt_delta_used %||% 0.8, 0.99)
  expect_equal(reported$max_treedepth_used %||% 15, 10)

  # `prior_sensitivity()` replays a fit from the same record, so an argument
  # stored there instead of the effective value would have refit the sweep
  # under different sampler settings from the fit it is a sweep of.
  replayed <- mlumr:::.prior_sensitivity_args(
    list(data = "DATA", model = "spfa", link = "identity", engine = "rstan",
         priors = list(intercept = NULL, sigma = NULL),
         sampling_args = list(chains = 4, iter = 2000, warmup = 1000,
                              seed = 2026, adapt_delta = 0.99,
                              max_treedepth = 10)),
    prior_beta_i = NULL, verbose = FALSE
  )
  expect_equal(replayed$adapt_delta, 0.99)
  expect_equal(replayed$max_treedepth, 10)
})

test_that("a prior sweep replays every sampler control, not only the two named", {
  # A caller can set anything rstan accepts, `adapt_engaged` and `stepsize`
  # among them. Recording only adapt_delta and max_treedepth would refit the
  # sweep under a different sampler configuration from the fit it sweeps,
  # which changes more than the prior the sweep is varying.
  full <- list(adapt_delta = 0.99, max_treedepth = 10,
               adapt_engaged = FALSE, stepsize = 0.05)
  args <- mlumr:::.prior_sensitivity_args(
    list(data = "DATA", model = "spfa", link = "identity", engine = "rstan",
         priors = list(intercept = NULL, sigma = NULL),
         sampling_args = list(chains = 4, iter = 2000, warmup = 1000,
                              seed = 2026, adapt_delta = 0.99,
                              max_treedepth = 10, control = full)),
    prior_beta_i = NULL, verbose = FALSE
  )
  expect_equal(args$control, full)
  # The scalars come from the same merged list, so the two cannot disagree.
  expect_equal(args$adapt_delta, args$control$adapt_delta)
  expect_equal(args$max_treedepth, args$control$max_treedepth)

  # cmdstanr has no `control` argument, and its backend records none, so
  # nothing is forwarded down that path.
  cmd <- mlumr:::.prior_sensitivity_args(
    list(data = "DATA", model = "spfa", link = "identity", engine = "cmdstanr",
         priors = list(intercept = NULL, sigma = NULL),
         sampling_args = list(chains = 4, iter = 2000, warmup = 1000,
                              seed = 2026, adapt_delta = 0.9,
                              max_treedepth = 12)),
    prior_beta_i = NULL, verbose = FALSE
  )
  expect_false("control" %in% names(cmd))
  expect_equal(cmd$adapt_delta, 0.9)
})

test_that("a caller's sampler overrides refine the recorded control", {
  # The recorded control describes the original fit. Assigning a caller's
  # wholesale dropped every recorded entry they did not name, and a scalar
  # they passed lost to the recorded entry of the same name, because the merge
  # downstream lets the control win.
  recorded <- list(adapt_delta = 0.99, max_treedepth = 10,
                   adapt_engaged = FALSE, stepsize = 0.05)
  # The engine is named rather than left out, because the merge resolves an
  # absent one through `get_engine()`: on a machine configured for cmdstanr
  # the control would be dropped and every assertion below would read NULL.
  refine <- function(dots) {
    mlumr:::.prior_sensitivity_merge_dots(
      list(adapt_delta = 0.99, max_treedepth = 10, engine = "rstan",
           control = recorded),
      dots
    )$control
  }

  expect_equal(refine(list()), recorded)

  # A partial control keeps what it does not name.
  partial <- refine(list(control = list(adapt_delta = 0.9)))
  expect_equal(partial$adapt_delta, 0.9)
  expect_false(partial$adapt_engaged)
  expect_equal(partial$stepsize, 0.05)
  expect_equal(partial$max_treedepth, 10)

  # A scalar override reaches the sampler instead of losing to the record.
  scalar <- refine(list(adapt_delta = 0.85))
  expect_equal(scalar$adapt_delta, 0.85)
  expect_equal(scalar$stepsize, 0.05)

  # Their control still beats their scalar, the order mlumr() itself uses.
  both <- refine(list(adapt_delta = 0.85, control = list(adapt_delta = 0.7)))
  expect_equal(both$adapt_delta, 0.7)
})

test_that("a recorded rstan control is not carried onto another engine", {
  # `control` is rstan's argument. Restoring the recorded one after a caller
  # switched engines forwarded it to cmdstanr's `$sample()`, which has no such
  # argument, so every refit would have failed before sampling.
  recorded <- list(adapt_delta = 0.99, max_treedepth = 10, stepsize = 0.05)
  resolve <- function(dots, fit_engine) {
    mlumr:::.prior_sensitivity_merge_dots(
      list(engine = fit_engine, control = recorded), dots
    )
  }
  # Changing something unrelated keeps the record. The passed settings have to
  # be non-empty for the merge to run at all: with nothing passed the sweep
  # replays the fit's own arguments and there is nothing to resolve.
  expect_equal(resolve(list(iter = 4000), "rstan")$control, recorded)
  # Switching engines drops it rather than handing cmdstanr an argument it
  # does not have. Dropping is right for a control this function inherited:
  # it describes the fit's backend and means nothing to another one.
  switched <- resolve(list(engine = "cmdstanr"), "rstan")
  expect_equal(switched$engine, "cmdstanr")
  expect_null(switched$control)
})

test_that("a control the caller passed is refused, not dropped, on another engine", {
  # An inherited control is this function's own doing. A control the caller
  # passed through the documented forwarding path is a request, and `$sample()`
  # has no such argument, so it can be neither honored nor silently ignored.
  fake <- list(
    data = "D", model = "spfa", link = "identity", engine = "rstan",
    family = "normal",
    priors = list(intercept = default_prior_intercept(),
                  beta = prior_normal(0, 1),
                  sigma = default_prior_sigma()),
    sampling_args = list(chains = 4, iter = 2000, warmup = 1000, seed = 2026,
                         adapt_delta = 0.99, max_treedepth = 10,
                         control = list(adapt_delta = 0.99, stepsize = 0.05))
  )
  class(fake) <- c("mlumr_fit", "list")
  expect_error(
    prior_sensitivity(fake, prior_beta_scales = 1, verbose = FALSE,
                      engine = "cmdstanr", control = list(adapt_delta = 0.9)),
    "`control` is an rstan setting"
  )
  # The message names the way to ask for the same thing on either backend.
  err <- tryCatch(
    prior_sensitivity(fake, prior_beta_scales = 1, verbose = FALSE,
                      engine = "cmdstanr", control = list(adapt_delta = 0.9)),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "adapt_delta")
  expect_match(err, "max_treedepth")

  # The same switch without an explicit control passes this check and goes on,
  # so the refusal is about the caller's request and not about the engine
  # switch itself. It then fails further along on this stub fit, which is what
  # shows it got past here.
  inherited <- tryCatch(
    prior_sensitivity(fake, prior_beta_scales = 1, verbose = FALSE,
                      engine = "cmdstanr"),
    error = function(e) conditionMessage(e)
  )
  expect_no_match(inherited, "`control` is an rstan setting")

  # `control = NULL` is a request for nothing, which is how
  # `.merge_sampler_control()` reads it too. Testing the name rather than the
  # value would refuse a wrapper that builds its arguments programmatically,
  # over a request nobody made.
  explicit_null <- tryCatch(
    prior_sensitivity(fake, prior_beta_scales = 1, verbose = FALSE,
                      engine = "cmdstanr", control = NULL),
    error = function(e) conditionMessage(e)
  )
  expect_no_match(explicit_null, "`control` is an rstan setting")
})

test_that("an explicit NULL engine resolves before the control is judged", {
  # `engine = NULL` is a documented way to ask for the configured default.
  # Assigning it into the call removes the element entirely, so reading the
  # engine back off the call returned NULL, which is not "rstan", and the
  # recorded controls were dropped. `mlumr()` then resolved NULL to the same
  # default, so the refits ran on the same backend with a different sampler
  # configuration: the failure is silent rather than an error.
  recorded <- list(adapt_delta = 0.99, max_treedepth = 10, stepsize = 0.05)
  kept <- function(dots, fit_engine) {
    mlumr:::.prior_sensitivity_merge_dots(
      list(engine = fit_engine, control = recorded), dots
    )$control
  }

  # Both configurations are exercised rather than whichever the machine
  # happens to be set to. Asserting that NULL and omission agree holds only
  # while the default matches the fit's engine, so it would pass here and fail
  # on a runner configured for cmdstanr.
  previous <- getOption("mlumr.stan_engine")
  on.exit(options(mlumr.stan_engine = previous), add = TRUE)

  for (configured in c("rstan", "cmdstanr")) {
    options(mlumr.stan_engine = configured)
    expect_equal(mlumr:::get_engine(), configured)

    # An explicit NULL, and a fit that recorded no engine at all, both resolve
    # to the configured default, so the record survives exactly when that
    # default is the backend that has a `control` argument.
    if (identical(configured, "rstan")) {
      expect_equal(kept(list(engine = NULL), "rstan"), recorded)
      expect_equal(kept(list(iter = 4000), NULL), recorded)
    } else {
      expect_null(kept(list(engine = NULL), "rstan"))
      expect_null(kept(list(iter = 4000), NULL))
    }

    # Omitting it keeps the engine the fit was made with, whatever the default.
    expect_equal(kept(list(iter = 4000), "rstan"), recorded)
    # A named engine wins over both.
    expect_null(kept(list(engine = "cmdstanr"), "rstan"))
    expect_equal(kept(list(engine = "rstan"), "cmdstanr"), recorded)
  }
})

test_that("a NULL entry does not delete a setting mlumr depends on", {
  merge <- mlumr:::.merge_sampler_control
  count <- mlumr:::.count_treedepth_hits
  sp <- list(cbind(treedepth__ = c(9, 10, 10, 8)),
             cbind(treedepth__ = c(10, 7, 9, 10)))

  # `modifyList()` deletes an entry whose replacement is NULL. That is right
  # for a setting rstan defaults on its own, and wrong for these two: mlumr
  # always supplies them, counts treedepth against one and records both on the
  # fit. Deleted, the count compared every transition against NULL, which
  # matches nothing and reports zero hits out of four.
  for (supplied in list(list(max_treedepth = NULL),
                        list(adapt_delta = NULL),
                        list(adapt_delta = NULL, max_treedepth = NULL))) {
    m <- merge(0.8, 10, list(control = supplied))
    expect_equal(m$control$adapt_delta, 0.8)
    expect_equal(m$control$max_treedepth, 10)
    expect_equal(count(sp, m$control$max_treedepth), 4)
  }

  # A real value still wins, and a NULL for an entry mlumr does not name is
  # left to rstan, which is what deleting it means there.
  given <- merge(0.8, 10, list(control = list(adapt_delta = 0.99,
                                              adapt_engaged = NULL)))
  expect_equal(given$control$adapt_delta, 0.99)
  expect_false("adapt_engaged" %in% names(given$control))
})

test_that("a sampler setting is validated the same through either door", {
  merge <- mlumr:::.merge_sampler_control
  # `mlumr()` checks these two when they arrive as arguments. Reaching the
  # sampler through `control` skipped every check, so `adapt_delta = NA` or 5
  # went to rstan and into the recorded metadata unchallenged, and the
  # treedepth count then compared transitions against whatever arrived.
  for (bad in list(NA_real_, 5, 0, -1, "x", c(0.8, 0.9), numeric(0))) {
    expect_error(merge(0.8, 10, list(control = list(adapt_delta = bad))),
                 "single finite number between 0 and 1")
  }
  expect_error(merge(0.8, 10, list(control = list(max_treedepth = 0))),
               "max_treedepth")
  expect_error(merge(0.8, 10, list(control = list(max_treedepth = 2.5))),
               "max_treedepth")

  # The message is the argument path's own, because both call one validator.
  arg_msg <- tryCatch(mlumr:::.validate_mlumr_adapt_delta(NA),
                      error = function(e) conditionMessage(e))
  ctl_msg <- tryCatch(merge(0.8, 10, list(control = list(adapt_delta = NA))),
                      error = function(e) conditionMessage(e))
  expect_identical(arg_msg, ctl_msg)

  # Valid settings still pass, and the defaults are still validated.
  ok <- merge(0.8, 10, list(control = list(adapt_delta = 0.99)))
  expect_equal(ok$control$adapt_delta, 0.99)
  expect_equal(ok$control$max_treedepth, 10)
})
