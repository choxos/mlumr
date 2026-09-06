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

test_that("the log link starts from a valid mean even when the sample mean is not positive", {
  starts <- mlumr:::.stc_start_values

  fam <- stats::gaussian(link = "log")

  # All outcomes positive: R initializes this itself, from log(y) per
  # observation, and that start is better than any pooled one. Taking over
  # here does not fail loudly, it just lands somewhere worse: on two groups a
  # factor of 1e7 apart, a pooled start reaches a fitted mean of 168 where R
  # reaches 2, and BOTH report convergence.
  pos <- list(.outcome = c(1, 2, 3))
  expect_null(starts(pos, character(0), fam))
  expect_null(starts(pos, c("a", "b"), fam))

  wide <- data.frame(.outcome = c(1, 2, 3, 1e7, 2e7, 3e7),
                     x = rep(0:1, each = 3))
  expect_null(starts(wide, "x", fam))
  by_r <- suppressWarnings(stats::glm(.outcome ~ x, family = fam, data = wide))
  pooled <- suppressWarnings(
    stats::glm(.outcome ~ x, family = fam, data = wide,
               start = c(log(mean(wide$.outcome)), 0))
  )
  expect_equal(unname(stats::fitted(by_r)[1]), 2, tolerance = 1e-6)
  expect_gt(unname(stats::fitted(pooled)[1]), 100)
  expect_true(by_r$converged && pooled$converged)

  # A sample mean of exactly zero. `exp(0) = 1` is a valid mean for this link
  # whatever the data, and a finite fit exists here: `y = c(-2, 1, 1)` on
  # `x = c(-1, 0, 1)` fits from zeros. Returning NULL instead handed the
  # problem back to the initialization that refuses any y <= 0, which is the
  # failure this helper exists to avoid.
  zero_mean <- list(.outcome = c(-2, 1, 1))
  expect_equal(mean(zero_mean$.outcome), 0)
  expect_equal(starts(zero_mean, "x", fam), c(0, 0))
  expect_false(is.null(starts(zero_mean, "x", fam)))

  # A nonpositive outcome with a positive mean takes the mean.
  mixed <- list(.outcome = c(-1, 1, 2, 2, 3, 4))
  expect_equal(starts(mixed, "x", fam), c(log(mean(mixed$.outcome)), 0))

  fit <- stats::glm(y ~ x, family = fam,
                    data = data.frame(y = c(-2, 1, 1), x = c(-1, 0, 1)),
                    start = starts(zero_mean, "x", fam))
  expect_true(all(is.finite(stats::coef(fit))))
  expect_true(all(stats::fitted(fit) > 0))

  # Other families and links are left to R's own initialization.
  expect_null(starts(mixed, "x", stats::gaussian(link = "identity")))
  expect_null(starts(mixed, "x", stats::binomial()))
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
