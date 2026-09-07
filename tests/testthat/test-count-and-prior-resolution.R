# Three defects that all share a shape: something is validated or stored
# correctly and then read back through the wrong path, so the check passes and
# the value the model or the reader receives is still wrong.

test_that("an accepted near-integer count keeps the count it was accepted for", {
  # The validators accept anything within sqrt(.Machine$double.eps) of a whole
  # number. `as.integer()` truncates toward zero. Those two rules disagree, and
  # the gap is not theoretical: 0.999999999 is accepted AS the count 1 and was
  # then coerced to 0, so the package silently changed an event count it had
  # just approved.
  x <- c(0.999999999, 2.000000001, 5, 0)
  expect_equal(as.integer(x), c(0L, 2L, 5L, 0L))          # the old behavior
  expect_equal(.as_count_integer(x), c(1L, 2L, 5L, 0L))   # the intended count

  # The value really does pass the check that precedes the coercion, which is
  # what makes the disagreement reachable rather than hypothetical.
  expect_true(.is_whole_number_count(0.999999999))

  # Exact integers are untouched, including the boundaries.
  exact <- c(0, 1, 2, 1e6)
  expect_equal(.as_count_integer(exact), as.integer(exact))
  expect_type(.as_count_integer(exact), "integer")
})

test_that("a near-integer IPD count reaches the model as the intended integer", {
  set.seed(2026)
  n <- 20L
  # An unanchored index study carries a single arm.
  d <- data.frame(
    trt = "A",
    age = rnorm(n),
    # One count is a hair below 3 rather than exactly 3.
    y = c(2.999999999, rep(3, n - 1L)),
    expo = 1
  )
  ipd <- set_ipd(d, treatment = "trt", outcome = "y", covariates = "age",
                 family = "poisson", exposure = "expo")
  # Truncation would have stored 2 here.
  expect_equal(ipd$data$.outcome[1], 3L)
  expect_type(ipd$data$.outcome, "integer")
})

test_that("the second auxiliary resolves to its own prior", {
  # `prior_aux2` is stored separately for the generalized gamma because the two
  # auxiliaries govern different features of the hazard and can be given
  # deliberately different priors. The plot looked both of them up in
  # `priors$aux`, so it drew the FIRST prior against the SECOND posterior,
  # which is exactly the comparison the plot exists to make.
  first <- prior_normal(0, 2)
  second <- prior_exponential(7)
  obj <- list(priors = list(aux = first, aux2 = second))

  expect_identical(.parameter_prior(obj, "aux_val")$prior, first)
  expect_identical(.parameter_prior(obj, "aux_val_cmp")$prior, first)
  expect_identical(.parameter_prior(obj, "aux2_val")$prior, second)
  expect_identical(.parameter_prior(obj, "aux2_val_cmp")$prior, second)

  # A fit stored before `aux2` existed has only `aux`, and must still resolve.
  legacy <- list(priors = list(aux = first))
  expect_identical(.parameter_prior(legacy, "aux2_val")$prior, first)

  # All four are positive-constrained.
  for (nm in c("aux_val", "aux_val_cmp", "aux2_val", "aux2_val_cmp")) {
    expect_equal(.parameter_prior(obj, nm)$lower, 0)
  }
})

test_that("the model-comparison message does not read se_diff as evidence", {
  # A large standard error is uncertainty about a difference, not support for
  # one. The old text said "se_diff > 2 is the conventional threshold for a
  # meaningful difference", under which an elpd_diff of 0.1 with se_diff 3
  # would qualify.
  # Assert what users see, by calling the thing that prints it. A source-text
  # check was fooled by the comment explaining the old wording, which quoted
  # the very string it was forbidding.
  printed <- paste(.model_comparison_interpretation(), collapse = " ")
  expect_false(grepl("conventional threshold", printed, fixed = TRUE))
  expect_true(grepl("uncertainty about that difference, not evidence for it",
                    printed, fixed = TRUE))
})
