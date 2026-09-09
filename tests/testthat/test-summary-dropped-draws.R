# A posterior summary computed over some of the draws must not read like a
# summary computed over all of them.

test_that("a vector summary reports the draws it dropped", {
  x <- c(rnorm(90), rep(NA_real_, 10))
  expect_warning(s <- mlumr:::.summarize_draw_vector(x, probs = c(0.025, 0.975)),
                 "10 of 100 posterior draws")
  # The summary is still returned, and still describes the usable draws.
  expect_equal(unname(s[["mean"]]), mean(x, na.rm = TRUE))
})

test_that("NaN counts as a dropped draw, and an infinity does not", {
  expect_warning(mlumr:::.summarize_draw_vector(c(1, 2, NaN), probs = 0.5),
                 "1 of 3 posterior draws")
  # An infinite draw propagates into the mean, so it announces itself.
  expect_silent(s <- mlumr:::.summarize_draw_vector(c(1, 2, Inf), probs = 0.5))
  expect_true(is.infinite(s[["mean"]]))
})

test_that("complete draws are summarized without a warning", {
  expect_silent(mlumr:::.summarize_draw_vector(rnorm(50), probs = 0.5))
  expect_silent(mlumr:::.summarize_draw_matrix(matrix(rnorm(50), ncol = 5),
                                               probs = 0.5))
})

test_that("a matrix summary reports once for the whole matrix", {
  m <- matrix(rnorm(100), ncol = 4)
  m[1:3, 2] <- NA
  m[1:7, 4] <- NaN
  w <- capture_warnings(mlumr:::.summarize_draw_matrix(m, probs = 0.5))
  # One warning, not one per column, and it names the worst loss.
  expect_length(w, 1L)
  expect_match(w, "2 of 4 summarized quantities")
  expect_match(w, "worst loses 7 of 25 draws")
})

test_that("the reported counts are the ones actually dropped", {
  m <- matrix(rnorm(60), ncol = 3)
  m[1:5, 1] <- NA
  expect_warning(s <- mlumr:::.summarize_draw_matrix(m, probs = 0.5))
  expect_equal(s$mean[1], mean(m[, 1], na.rm = TRUE))
  expect_equal(s$mean[2], mean(m[, 2]))
})

test_that("an expected missing median is not reported as a lost draw", {
  # A draw whose fitted survival never reaches 0.5 on the grid has no median.
  # That is an outcome, not a loss, and `p_not_reached` plus
  # .median_not_reached_note() already report it with their own switch. The
  # generic warning would repeat it once per cell, ignore that switch, and
  # under options(warn = 2) turn an ordinary result into an error.
  m <- matrix(c(1.2, NA, 1.4, 1.1), ncol = 1)
  expect_silent(mlumr:::.summarize_draw_matrix(m, probs = 0.5, warn = FALSE))
  # The default is still to report.
  expect_warning(mlumr:::.summarize_draw_matrix(m, probs = 0.5),
                 "1 of 4 posterior draws")
})

test_that("the loop tally counts quantities and reports once", {
  tally <- mlumr:::.draw_tally()
  tally$add(data.frame(a = c(1, NA, 3), b = c(4, 5, 6)))
  tally$add(c(NA, 2, 3, 4))
  expect_equal(tally$units, 3)
  expect_equal(tally$affected, 2)

  # One warning for the whole loop, naming what was looped over.
  w <- capture_warnings(tally$report("conditional profiles"))
  expect_length(w, 1L)
  expect_match(w, "2 of 3 summarized quantities across the conditional profiles")
})

test_that("a loss concentrated in one summary is not diluted by the others", {
  # 99 complete profiles and one that keeps 10 of its 100 draws. Pooling the
  # counts reports 90 of 10000 and reads as rounding; the number that matters
  # is that one returned summary rests on 10 draws.
  tally <- mlumr:::.draw_tally()
  for (i in 1:99) tally$add(matrix(rnorm(100), ncol = 1))
  bad <- matrix(rnorm(100), ncol = 1)
  bad[1:90] <- NA
  tally$add(bad)

  w <- capture_warnings(tally$report("conditional profiles"))
  expect_length(w, 1L)
  expect_match(w, "1 of 100 summarized quantities")
  expect_match(w, "worst loses 90 of 100 draws")
  expect_false(any(grepl("10000", w, fixed = TRUE)))
})

test_that("a clean tally says nothing", {
  tally <- mlumr:::.draw_tally()
  tally$add(c(1, 2, 3))
  expect_silent(tally$report("conditional profiles"))
})

# A warning is for the session. A summary that outlives it, saved to disk or
# handed to a report, has to carry its own accounting, or a mean over a third
# of the chain reads exactly like a mean over all of it once the warning has
# scrolled away.

test_that("every summary carries how many draws it was offered and how many it used", {
  v <- mlumr:::.summarize_draw_vector(c(1, NA, 3, NaN), probs = 0.5,
                                      warn = FALSE)
  expect_equal(v[["n_draws"]], 4)
  expect_equal(v[["n_draws_used"]], 2)

  m <- cbind(a = c(1, NA, 3), b = c(4, 5, 6))
  s <- mlumr:::.summarize_draw_matrix(m, probs = c(0.1, 0.9), warn = FALSE)
  expect_equal(names(s), c("mean", "sd", "q10", "q90", "n_draws",
                           "n_draws_used"))
  expect_equal(s$n_draws, c(3, 3))
  expect_equal(s$n_draws_used, c(2, 3))
})

test_that("the accounting survives a public call and a round trip through disk", {
  # A stub fit with one draw missing in the index arm: the public path warns,
  # and the returned summaries say which rows the loss touched, in columns
  # that saveRDS() keeps.
  draws <- data.frame(mu_index = c(1, NA, 2), mu_comparator = c(0, 0.5, 1),
                      check.names = FALSE)
  draws[["beta[1]"]] <- c(0, 0.5, 0.2)
  fit <- structure(
    list(draws = draws, family = "binomial", link = "logit", model = "spfa",
         data = list(covariates = "x",
                     ipd = list(data = data.frame(x = c(0, 2))),
                     index_treatment = "A", comparator_treatment = "B")),
    class = c("mlumr_fit", "list")
  )

  expect_warning(ce <- conditional_effects(fit, newdata = data.frame(x = 0)),
                 "dropped from their summaries")
  expect_true(all(c("n_draws", "n_draws_used") %in% names(ce)))
  expect_equal(unique(ce$n_draws), 3)
  expect_equal(unique(ce$n_draws_used), 2)

  expect_warning(cp <- conditional_predict(fit, newdata = data.frame(x = 0),
                                           type = "link"),
                 "dropped from their summaries")
  expect_equal(cp$n_draws, c(3, 3))
  expect_equal(cp$n_draws_used[cp$treatment == "A"], 2)
  expect_equal(cp$n_draws_used[cp$treatment == "B"], 3)

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(ce, path)
  back <- readRDS(path)
  expect_equal(back$n_draws_used, ce$n_draws_used)
  expect_equal(back$n_draws, ce$n_draws)
})

test_that("a missing median is accounted for without being called a loss", {
  # The survival frame exempts the median from the dropped-draw warning,
  # because a median the grid never reaches is an outcome with its own
  # diagnostic. The accounting still shows the summary rests on the draws
  # that reached it, next to the probability that one does not.
  m <- matrix(c(1.2, NA, 1.4, 1.1), ncol = 1)
  cells <- data.frame(treatment = "A", population = "index",
                      stringsAsFactors = FALSE)
  # Its own diagnostic is a message, not the dropped-draw warning.
  expect_no_warning(
    out <- suppressMessages(
      mlumr:::.surv_result_frame(list(m), cells, "median", summary = TRUE,
                                 probs = 0.5)
    )
  )
  expect_equal(out$n_draws, 4)
  expect_equal(out$n_draws_used, 3)
  expect_equal(out$p_not_reached, 0.25)

  # A survival curve keeps the accounting on every time. The origin row
  # neither takes the origin value nor inherits the first time's loss: every
  # draw contributes S(0) = 1 exactly, so it uses all of them.
  expect_warning(
    curve <- mlumr:::.surv_result_frame(
      list(cbind(c(1, NA, 0.9, 0.8), c(0.5, 0.4, NA, 0.3))),
      cells, "survival", summary = TRUE, probs = 0.5, times_out = c(1, 2),
      origin = 1
    ),
    "dropped from their summaries"
  )
  expect_equal(curve$time, c(0, 1, 2))
  expect_equal(curve$n_draws, c(4, 4, 4))
  expect_equal(curve$n_draws_used, c(4, 3, 3))
  expect_equal(curve$mean[1], 1)
})
