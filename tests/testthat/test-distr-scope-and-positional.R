# `distr()` stores its arguments unevaluated so they can see a row of aggregate
# data later. Two things were missing from that contract, and both failed
# silently rather than loudly.

test_that("positional distribution arguments are honored", {
  # Evaluation walked `names(args)`. A fully positional specification had no
  # names, so nothing was passed to the quantile function and the requested
  # distribution was quietly replaced by that function's defaults:
  # `distr(qnorm, 10, 2)` evaluated to 0 at the median instead of 10.
  expect_equal(eval_distr(distr(qnorm, 10, 2), 0.5), 10)
  expect_equal(eval_distr(distr(qnorm, 10, 2), 0.975), qnorm(0.975, 10, 2))

  # Partly named, matching R's own rule: the unnamed value takes the first
  # parameter not already claimed by name.
  expect_equal(eval_distr(distr(qnorm, 10, sd = 2), 0.975), qnorm(0.975, 10, 2))
  expect_equal(eval_distr(distr(qnorm, sd = 2, 10), 0.5), 10)

  # Fully named is unchanged.
  expect_equal(eval_distr(distr(qnorm, mean = 10, sd = 2), 0.5), 10)

  # The stored specification is explicit about what each argument is, so a
  # reader of the object sees the same thing the evaluator does.
  expect_setequal(names(distr(qnorm, 10, 2)$args), c("mean", "sd"))
})

test_that("more positional arguments than parameters is an error", {
  expect_error(distr(qbern, 0.3, 1, 2, 3), "unnamed argument")
})

test_that("a specification can see the scope it was written in", {
  # Arguments were evaluated with `enclos = parent.frame(2)`, which from inside
  # the package is a package frame rather than the user's. A specification
  # built in a function, or returned by one, could not resolve its own local
  # variables even though it read as ordinary R.
  factory <- function() {
    local_sd <- 3
    distr(qnorm, mean = 0, sd = local_sd)
  }
  spec <- factory()
  expect_equal(eval_distr(spec, 0.975), qnorm(0.975, 0, 3))

  wrapper <- function(s) {
    inner <- 7
    eval_distr(distr(qnorm, mean = inner, sd = s), 0.5)
  }
  expect_equal(wrapper(1), 7)
})

test_that("row data still takes precedence over the calling scope", {
  # This is the primary mechanism and must not be weakened by the fallback: a
  # column of the aggregate row shadows a variable of the same name.
  shadowed <- function() {
    age_mean <- -99
    distr(qnorm, mean = age_mean, sd = 1)
  }
  spec <- shadowed()
  expect_equal(eval_distr(spec, 0.5, data = list(age_mean = 5)), 5)
  # With no such column, the caller's value is used.
  expect_equal(eval_distr(spec, 0.5), -99)
})

test_that("a specification stored without an environment still evaluates", {
  # Objects created before the environment was recorded must keep working.
  spec <- distr(qnorm, mean = 2, sd = 1)
  spec$envir <- NULL
  expect_equal(eval_distr(spec, 0.5), 2)
})
