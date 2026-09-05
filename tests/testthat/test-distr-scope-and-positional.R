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

test_that("formals after `...` are not matched positionally", {
  # R matches unnamed arguments only against formals declared BEFORE `...`.
  # Anything after it is name-only, and an unnamed value goes into the dots.
  # Treating a post-dots formal as positionally available would silently
  # replace its default: `distr(qdots, 999)` would bind 999 to `scale`.
  qdots <- function(p, ..., scale = 2) stats::qnorm(p) * scale

  expect_equal(eval_distr(distr(qdots, 999), 0.975),
               stats::qnorm(0.975) * 2)
  # By name it does reach `scale`.
  expect_equal(eval_distr(distr(qdots, scale = 5), 0.975),
               stats::qnorm(0.975) * 5)
  # The unnamed value is kept unnamed, so it travels through the dots.
  spec <- distr(qdots, 999)
  expect_true(is.null(names(spec$args)) || !nzchar(names(spec$args)[[1]]))
})

test_that("a function without dots rejects unmatched positional arguments", {
  # Nowhere for the value to go, so say so at construction rather than letting
  # the quantile function produce a confusing error later.
  expect_error(distr(qbern, 0.3, 1, 2, 3), "no remaining parameter")
})

test_that("a partial name claims its formal before positional filling", {
  # R lets a caller abbreviate an argument name. `m` claims `mean`, so the
  # unnamed value must go to `sd`; assigning it to `mean` as well handed the
  # quantile function both `m` and `mean` and it failed with "matched by
  # multiple actual arguments".
  expect_equal(eval_distr(distr(qnorm, m = 10, 2), 0.5), 10)
  expect_equal(eval_distr(distr(qnorm, m = 10, 2), stats::pnorm(1)),
               10 + 2)
  # An exact name still wins over a partial one for the same formal.
  expect_equal(eval_distr(distr(qnorm, sd = 3, 10), 0.5), 10)
  # R resolves every exact name before any partial one, so with formals `mean`
  # and `method` an abbreviated `m` beside an exact `mean` reaches `method`.
  # Matching partials against the full list gave `m` nothing, handed the
  # unnamed 2 to `method`, and moved the median from 30 to 22 without a word.
  q <- function(p, mean = 0, method = 1, ...) stats::qnorm(p, mean + method)
  expect_equal(eval_distr(distr(q, m = 10, mean = 20, 2), 0.5),
               q(0.5, m = 10, mean = 20, 2))
  expect_equal(eval_distr(distr(q, m = 10, mean = 20, 2), 0.5), 30)
})

test_that("an abbreviation that fits two formals is refused, as R refuses it", {
  # R rejects `q(0.5, a = 1, 2)` because `a` is ambiguous before the positional
  # value is bound. Binding 2 to `alpha` first would leave `a` a unique match
  # for `alpine` when the quantile function is finally called, so a call R
  # refuses evaluated here with both parameters set, to values the caller
  # never named.
  q <- function(p, alpha = 0, alpine = 0, ...) stats::qnorm(p, alpha + 10 * alpine)
  expect_error(q(0.5, a = 1, 2), "matches multiple formal arguments")
  expect_error(distr(q, a = 1, 2), "matches more than one parameter")
  # An abbreviation that fits exactly one formal still works, and the
  # positional value takes the other.
  expect_equal(eval_distr(distr(q, alph = 1, 2), 0.5), q(0.5, alph = 1, 2))
  expect_equal(eval_distr(distr(q, alph = 1, 2), 0.5), 21)
})

test_that("classifying a qbinom margin sees the specification's scope", {
  # `eval_distr()` resolves a local variable of the function that built the
  # spec; the `qbinom` shortcut in get_distribution_type() evaluated `size`
  # from the package's own frame and failed with "object 'n_trials' not
  # found" for the same specification.
  make_spec <- function() {
    n_trials <- 1L
    distr(qbinom, size = n_trials, prob = x_mean)
  }
  sp <- make_spec()
  expect_equal(eval_distr(sp, 0.5, list(x_mean = 0.3)), 0)
  expect_equal(unname(get_distribution_type(x = sp, data = list(x_mean = 0.3))),
               "binary")
  make_many <- function() {
    n_trials <- 5L
    distr(qbinom, size = n_trials, prob = x_mean)
  }
  expect_equal(unname(get_distribution_type(x = make_many(),
                                            data = list(x_mean = 0.3))),
               "discrete")
})

test_that("an abbreviated argument is stored under its full name", {
  # R completes `si` to `size` when the quantile function is called, so the
  # specification evaluated with five trials. Everything that reads the stored
  # arguments BY NAME did not get that completion: the margin classification
  # looked up `args$size`, found nothing, and `all(NULL == 1)` is TRUE, so a
  # five-trial binomial was labeled binary. The stored name must be the one
  # the quantile function will bind.
  sp <- distr(qbinom, si = 5, pr = 0.5)
  expect_named(sp$args, c("size", "prob"))
  expect_equal(eval_distr(sp, 0.5), stats::qbinom(0.5, size = 5, prob = 0.5))
  expect_equal(unname(get_distribution_type(x = sp)), "discrete")
  expect_equal(unname(get_distribution_type(x = distr(qbinom, si = 1, pr = 0.5))),
               "binary")
  # The same completion with a positional value for the other formal.
  sp2 <- distr(qgamma, sh = 2, 3)
  expect_named(sp2$args, c("shape", "rate"))
  expect_equal(eval_distr(sp2, 0.5), stats::qgamma(0.5, shape = 2, rate = 3))
  # An exact name after `...` is left alone: R never completes those.
  q <- function(p, ..., scale = 1) stats::qnorm(p) * scale
  sp3 <- distr(q, scale = 2)
  expect_named(sp3$args, "scale")
  expect_equal(eval_distr(sp3, 0.975), stats::qnorm(0.975) * 2)
})
