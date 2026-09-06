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
