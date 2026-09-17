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
  set.seed(2026)
  m <- matrix(rnorm(60), ncol = 3)
  m[1:5, 1] <- NA
  expect_warning(s <- mlumr:::.summarize_draw_matrix(m, probs = 0.5),
                 "1 of 3 summarized quantities.*5 of 20 draws")
  expect_equal(s$mean[1], mean(m[, 1], na.rm = TRUE))
  expect_equal(s$mean[2], mean(m[, 2]))
  expect_equal(names(s), c("mean", "sd", "q50"))
})

test_that("an expected missing median is not reported as a lost draw", {
  m <- matrix(c(1.2, NA, 1.4, 1.1), ncol = 1)
  expect_silent(mlumr:::.summarize_draw_matrix(m, probs = 0.5, warn = FALSE))
  # The default is still to report.
  expect_warning(mlumr:::.summarize_draw_matrix(m, probs = 0.5),
                 "1 of 4 posterior draws")
})

test_that("a public call warns once for the whole set of profiles", {
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
  profiles <- data.frame(x = c(0, 1, 2))
  w <- capture_warnings(ce <- conditional_effects(fit, newdata = profiles))
  expect_length(w, 1L)
  expect_match(w, "dropped from their summaries")
  expect_equal(names(ce), c("profile", "effect", "mean", "sd",
                            "q2.5", "q50", "q97.5"))
  w <- capture_warnings(cp <- conditional_predict(fit, newdata = profiles,
                                                  type = "link"))
  expect_length(w, 1L)
  expect_match(w, "dropped from their summaries")
  expect_equal(nrow(cp), 6L)
})

test_that("a missing median is not called a loss, and the origin row is exact", {
  m <- matrix(c(1.2, NA, 1.4, 1.1), ncol = 1)
  cells <- data.frame(treatment = "A", population = "index",
                      stringsAsFactors = FALSE)
  expect_no_warning(
    out <- suppressMessages(
      mlumr:::.surv_result_frame(list(m), cells, "median", summary = TRUE,
                                 probs = 0.5)
    )
  )
  expect_equal(out$p_not_reached, 0.25)

  expect_warning(
    curve <- mlumr:::.surv_result_frame(
      list(cbind(c(1, NA, 0.9, 0.8), c(0.5, 0.4, NA, 0.3))),
      cells, "survival", summary = TRUE, probs = 0.5, times_out = c(1, 2),
      origin = 1
    ),
    "dropped from their summaries"
  )
  expect_equal(curve$time, c(0, 1, 2))
  expect_equal(curve$mean[1], 1)
  expect_equal(curve$sd[1], 0)
})
