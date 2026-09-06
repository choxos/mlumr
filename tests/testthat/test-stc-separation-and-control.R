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

test_that("the log link is fitted from several starts, only where R will not", {
  cands <- mlumr:::.stc_start_candidates
  fitg <- mlumr:::.stc_fit_glm
  fam <- stats::gaussian(link = "log")

  # All outcomes positive: R initializes this itself, per observation, better
  # than anything offered here, so nothing is offered and nothing changes.
  expect_equal(cands(list(.outcome = c(1, 2, 3)), fam), list())
  wide_pos <- data.frame(.outcome = c(1, 2, 3, 1e7, 2e7, 3e7),
                         x = rep(0:1, each = 3))
  expect_equal(cands(wide_pos, fam), list())
  by_r <- fitg(.outcome ~ x, fam, wide_pos)
  expect_equal(unname(stats::fitted(by_r)[1]), 2, tolerance = 1e-6)

  # Where R refuses, no single start is reliable, and the two failures point
  # opposite ways. Each is checked against the start that loses it.
  wide <- data.frame(.outcome = c(0, 2, 4, 1e7, 2e7, 3e7),
                     x = rep(0:1, each = 3))
  pooled <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = wide,
    start = c(log(mean(wide$.outcome)), 0)
  ))
  expect_true(pooled$converged)
  expect_gt(unname(stats::fitted(pooled)[1]), 100)   # 168, where the optimum is 2

  ulp <- data.frame(.outcome = c(-1, 1.00000001, -1, 1.00000001),
                    x = c(-1, -1, 1, 1))
  floored <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = ulp,
    mustart = pmax(ulp$.outcome, min(ulp$.outcome[ulp$.outcome > 0])),
    control = list(epsilon = 1e-14, maxit = 5000)
  ))
  expect_gt(unname(stats::fitted(floored)[1]), 1e-8) # 1.8e-8, where it is 5e-9

  # Taking the lowest deviance among the candidates reaches both optima.
  expect_gt(length(cands(wide, fam)), 1L)
  expect_equal(unname(stats::fitted(fitg(.outcome ~ x, fam, wide))[1]),
               2, tolerance = 1e-6)
  expect_equal(unname(stats::fitted(fitg(.outcome ~ x, fam, wide))[4]),
               2e7, tolerance = 1e-6)
  expect_equal(unname(stats::fitted(fitg(.outcome ~ x, fam, ulp))[1]),
               5e-9, tolerance = 1e-6)

  # Mixed signs reach the exact group means.
  mixed <- data.frame(.outcome = c(-1, 1, 2, 2, 3, 4), x = rep(0:1, each = 3))
  fm <- fitg(.outcome ~ x, fam, mixed)
  expect_equal(unname(stats::fitted(fm)[1]), 2 / 3, tolerance = 1e-6)
  expect_equal(unname(stats::fitted(fm)[4]), 3, tolerance = 1e-6)

  # Nothing positive to scale by, and other families, are left to R.
  expect_equal(cands(list(.outcome = c(0, 0, 0)), fam), list())
  expect_equal(cands(list(.outcome = c(-1, -2)), fam), list())
  expect_equal(cands(mixed, stats::gaussian(link = "identity")), list())
  expect_equal(cands(mixed, stats::binomial()), list())
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

test_that("a log-link mean fit with no interior optimum is refused", {
  fam <- stats::gaussian(link = "log")
  guard <- mlumr:::.stc_refuse_boundary_mean

  # Every fitted mean must be positive, and this data pulls all of them down,
  # so the criterion is minimized only as the intercept runs to negative
  # infinity. Nothing reports it: the deviance stops changing and `glm()`
  # returns convergence with coefficients set by the tolerance.
  d <- data.frame(.outcome = c(-1, 1, -1, 1), x = c(-1, -1, 1, 1))
  fit <- suppressWarnings(stats::glm(.outcome ~ x, family = fam, data = d,
                                     start = c(0, 0)))
  expect_true(fit$converged)
  expect_true(all(is.finite(stats::coef(fit))))
  # The estimate is a property of `epsilon`, not of the data.
  eps_fit <- function(e) {
    suppressWarnings(stats::glm(.outcome ~ x, family = fam, data = d,
                                start = c(0, 0),
                                control = list(epsilon = e, maxit = 100)))
  }
  expect_equal(unname(stats::coef(eps_fit(1e-8))[1]), -11, tolerance = 1e-6)
  expect_equal(unname(stats::coef(eps_fit(1e-12))[1]), -15, tolerance = 1e-6)

  # The fit buys nothing over setting every mean to zero, which is the test.
  expect_equal(stats::deviance(fit), sum(d$.outcome^2))
  expect_error(guard(fit), "did not reach a usable optimum")

  # Ordinary fits pass, at any scale, since the comparison is a ratio.
  ok <- data.frame(.outcome = c(-1, 1, 2, 2, 3, 4), x = rep(0:1, each = 3))
  fit_ok <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = ok,
    start = c(log(mean(ok$.outcome)), 0)
  ))
  expect_lt(stats::deviance(fit_ok), sum(ok$.outcome^2))
  expect_silent(guard(fit_ok))

  tiny <- data.frame(.outcome = c(1, 2, 3, 10, 20, 30) * 1e-6,
                     x = rep(0:1, each = 3))
  fit_tiny <- suppressWarnings(stats::glm(.outcome ~ x, family = fam,
                                          data = tiny))
  expect_silent(guard(fit_tiny))

  # The separation guard cannot cover this: it needs a probability boundary,
  # and this family has none. Other families and links are untouched here.
  expect_silent(guard(stats::glm(.outcome ~ x, family = stats::gaussian(),
                                 data = d)))
  expect_silent(guard(stats::glm(y ~ x, family = stats::poisson(),
                                 data = data.frame(y = c(1, 2, 3, 4),
                                                   x = c(0, 0, 1, 1)))))
})

test_that("an all-zero outcome under a log link is the clearest boundary case", {
  fam <- stats::gaussian(link = "log")
  # `sum(y^2)` is zero here, and reading that as an unavailable comparison let
  # the fit through. A log link keeps every mean strictly positive, so it can
  # only approach these observations by sending the intercept to negative
  # infinity: this is the boundary, not a missing measurement of it.
  d <- data.frame(.outcome = c(0, 0, 0, 0), x = c(0, 0, 1, 1))
  fit <- suppressWarnings(stats::glm(.outcome ~ x, family = fam, data = d,
                                     start = c(0, 0)))
  expect_true(fit$converged)
  expect_true(all(is.finite(stats::coef(fit))))
  expect_equal(sum(d$.outcome^2), 0)
  # This branch keeps the stronger wording: with every outcome zero there is
  # certainly no finite optimum, rather than possibly one the fitting missed.
  expect_error(mlumr:::.stc_refuse_boundary_mean(fit), "no finite optimum")

  # A stratum that is genuinely small, rather than at the boundary, still
  # fits: both group means are positive, so the optimum is interior at a
  # fitted mean of 1.5e-9. Refusing this would be worse than the gap it
  # closes, and every summary statistic tried put it on the same side as the
  # boundary cases, which is why the guard does not use one.
  small <- data.frame(.outcome = c(1e-9, 2e-9, 5, 6), x = c(0, 0, 1, 1))
  fit_small <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = small, start = c(0, 0),
    control = list(epsilon = 1e-14, maxit = 5000)
  ))
  expect_true(fit_small$converged)
  expect_lt(stats::deviance(fit_small), sum(small$.outcome^2))
  expect_silent(mlumr:::.stc_refuse_boundary_mean(fit_small))
})

test_that("a caller's sampler overrides refine the recorded control", {
  # The recorded control describes the original fit. Assigning a caller's
  # wholesale dropped every recorded entry they did not name, and a scalar
  # they passed lost to the recorded entry of the same name, because the merge
  # downstream lets the control win.
  recorded <- list(adapt_delta = 0.99, max_treedepth = 10,
                   adapt_engaged = FALSE, stepsize = 0.05)
  refine <- function(dots) {
    call_args <- list(adapt_delta = 0.99, max_treedepth = 10,
                      control = recorded)
    ctl <- call_args$control
    if (!is.null(ctl)) {
      for (nm in intersect(names(dots), c("adapt_delta", "max_treedepth"))) {
        ctl[[nm]] <- dots[[nm]]
      }
    }
    if (!is.null(dots$control)) {
      ctl <- if (is.null(ctl)) dots$control else
        utils::modifyList(ctl, dots$control)
    }
    call_args[names(dots)] <- dots
    if (!is.null(ctl)) call_args$control <- ctl
    call_args$control
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

test_that("a small but real improvement over the boundary is still an optimum", {
  fam <- stats::gaussian(link = "log")
  # Each stratum's optimum here is a fitted mean of 5e-6, worth 1e-10 of
  # deviance against the all-zero boundary. That is an optimum, and a relative
  # margin on the comparison refused it as though it were the boundary. The
  # comparison is exact for that reason: the boundary is the optimum precisely
  # when no positive mean improves on zero, and then the deviance exceeds
  # `sum(y^2)` rather than merely approaching it.
  d <- data.frame(.outcome = c(-1, 1.00001, -1, 1.00001), x = c(-1, -1, 1, 1))
  expect_true(all(tapply(d$.outcome, d$x, mean) > 0))
  fit <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = d, start = c(0, 0),
    control = list(epsilon = 1e-14, maxit = 5000)
  ))
  expect_lt(stats::deviance(fit), sum(d$.outcome^2))
  expect_silent(mlumr:::.stc_refuse_boundary_mean(fit))

  # The genuine boundary sits on the other side of the same exact comparison,
  # so no slack is needed to tell them apart.
  b <- data.frame(.outcome = c(-1, 1, -1, 1), x = c(-1, -1, 1, 1))
  fit_b <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = b, start = c(0, 0),
    control = list(epsilon = 1e-14, maxit = 5000)
  ))
  expect_gte(stats::deviance(fit_b), sum(b$.outcome^2))
  expect_error(mlumr:::.stc_refuse_boundary_mean(fit_b), "did not reach a usable optimum")
})

test_that("a recorded rstan control is not carried onto another engine", {
  # `control` is rstan's argument. Restoring the recorded one after a caller
  # switched engines forwarded it to cmdstanr's `$sample()`, which has no such
  # argument, so every refit would have failed before sampling.
  recorded <- list(adapt_delta = 0.99, max_treedepth = 10, stepsize = 0.05)
  `%||%` <- function(a, b) if (is.null(a)) b else a
  resolve <- function(dots, fit_engine) {
    call_args <- list(engine = fit_engine, control = recorded)
    ctl <- call_args$control
    call_args[names(dots)] <- dots
    engine_used <- dots$engine %||% call_args$engine
    if (!is.null(ctl) && identical(engine_used, "rstan")) {
      call_args$control <- ctl
    } else if (!identical(engine_used, "rstan")) {
      call_args$control <- NULL
    }
    call_args
  }
  # Staying on rstan keeps the record.
  expect_equal(resolve(list(), "rstan")$control, recorded)
  # Switching engines drops it rather than handing cmdstanr an argument it
  # does not have.
  switched <- resolve(list(engine = "cmdstanr"), "rstan")
  expect_equal(switched$engine, "cmdstanr")
  expect_null(switched$control)
})

test_that("an explicit NULL engine resolves before the control is judged", {
  # `engine = NULL` is a documented way to ask for the configured default.
  # Assigning it into the call removes the element entirely, so reading the
  # engine back off the call returned NULL, which is not "rstan", and the
  # recorded controls were dropped. `mlumr()` then resolved NULL to the same
  # default, so the refits ran on the same backend with a different sampler
  # configuration: the failure is silent rather than an error.
  resolve <- function(dots, fit_engine) {
    fe <- fit_engine
    supplied <- if ("engine" %in% names(dots)) dots$engine else fe
    mlumr:::.validate_engine_name(supplied %||% mlumr:::get_engine())
  }
  `%||%` <- function(a, b) if (is.null(a)) b else a

  default <- mlumr:::get_engine()
  # Passing NULL must land on the same engine as passing nothing.
  expect_equal(resolve(list(engine = NULL), "rstan"),
               resolve(list(), "rstan"))
  expect_equal(resolve(list(engine = NULL), "rstan"), "rstan")
  # A named engine still wins over both.
  expect_equal(resolve(list(engine = "cmdstanr"), "rstan"), "cmdstanr")
  expect_equal(resolve(list(engine = "rstan"), "cmdstanr"), "rstan")
  # And the resolution is the package's own, not a second copy of the rule.
  expect_equal(resolve(list(), NULL), default)
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

test_that("an improvement smaller than an ulp of the sums is still an improvement", {
  fam <- stats::gaussian(link = "log")
  # The improvement over the boundary is `sum(y^2) - sum((y - mu)^2)`, which
  # expands to `sum(mu * (2 * y - mu))`. Read as the difference of the two
  # sums it cannot represent anything smaller than an ulp of them, and a real
  # optimum can be smaller than that.
  d <- data.frame(.outcome = c(-1, 1.00000001, -1, 1.00000001),
                  x = c(-1, -1, 1, 1))
  expect_true(all(tapply(d$.outcome, d$x, mean) > 0))
  # From the starting values the package itself supplies. This matters: from
  # zeros the fit lands at 4.4e-8, which is worse than the boundary, and is
  # then refused correctly. The point being pinned is the arithmetic at the
  # optimum, so the fit has to actually reach it.
  fit <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = d,
    start = c(log(mean(d$.outcome)), 0),
    control = list(epsilon = 1e-14, maxit = 5000)
  ))
  y <- d$.outcome
  mu <- stats::fitted(fit)
  expect_equal(unname(mu), rep(5e-9, 4), tolerance = 1e-6)

  # Both sums round to the same double, so subtracting them returns zero.
  expect_equal(sum(y^2) - stats::deviance(fit), 0)
  # The closed form recovers the improvement that the subtraction destroyed.
  expect_gt(sum(mu * (2 * y - mu)), 0)
  expect_silent(mlumr:::.stc_refuse_boundary_mean(fit))

  # The genuine boundary has a negative improvement by the same measure, so
  # one expression decides both without a threshold.
  b <- data.frame(.outcome = c(-1, 1, -1, 1), x = c(-1, -1, 1, 1))
  fit_b <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = b, start = c(0, 0),
    control = list(epsilon = 1e-14, maxit = 5000)
  ))
  expect_lt(sum(stats::fitted(fit_b) * (2 * b$.outcome - stats::fitted(fit_b))), 0)
  expect_error(mlumr:::.stc_refuse_boundary_mean(fit_b),
               "did not reach a usable optimum")

  # And the two ways of computing it agree wherever subtraction is safe.
  ok <- data.frame(.outcome = c(-1, 1, 2, 2, 3, 4), x = rep(0:1, each = 3))
  fit_ok <- suppressWarnings(stats::glm(
    .outcome ~ x, family = fam, data = ok,
    start = c(log(mean(ok$.outcome)), 0)
  ))
  mo <- stats::fitted(fit_ok)
  expect_equal(sum(mo * (2 * ok$.outcome - mo)),
               sum(ok$.outcome^2) - stats::deviance(fit_ok),
               tolerance = 1e-8)
})
