test_that("survival predict() summary = FALSE returns raw scalar and loghr draws", {
  draws <- data.frame(
    "rmst_index_index" = c(2.0, 2.1, 2.2),
    "rmst_comparator_index" = c(1.8, 1.9, 2.0),
    "rmst_index_comparator" = c(2.3, 2.4, 2.5),
    "rmst_comparator_comparator" = c(1.7, 1.8, 1.9),
    "surv_index_index[1]" = c(0.9, 0.88, 0.86),
    "surv_index_index[2]" = c(0.6, 0.58, 0.56),
    "surv_index_index[3]" = c(0.4, 0.38, 0.36),
    "surv_comparator_index[1]" = c(0.9, 0.89, 0.88),
    "surv_comparator_index[2]" = c(0.55, 0.54, 0.53),
    "surv_comparator_index[3]" = c(0.35, 0.34, 0.33),
    "surv_index_comparator[1]" = c(0.92, 0.91, 0.9),
    "surv_index_comparator[2]" = c(0.62, 0.61, 0.6),
    "surv_index_comparator[3]" = c(0.42, 0.41, 0.4),
    "surv_comparator_comparator[1]" = c(0.89, 0.88, 0.87),
    "surv_comparator_comparator[2]" = c(0.52, 0.51, 0.5),
    "surv_comparator_comparator[3]" = c(0.32, 0.31, 0.3),
    "haz_index_comparator[1]" = c(0.10, 0.11, 0.12),
    "haz_index_comparator[2]" = c(0.12, 0.13, 0.14),
    "haz_index_comparator[3]" = c(0.14, 0.15, 0.16),
    "haz_comparator_comparator[1]" = c(0.08, 0.09, 0.10),
    "haz_comparator_comparator[2]" = c(0.10, 0.11, 0.12),
    "haz_comparator_comparator[3]" = c(0.12, 0.13, 0.14),
    check.names = FALSE
  )
  fit <- structure(
    list(
      family = "survival",
      draws = draws,
      pred_times = c(1, 2, 3),
      data = list(index_treatment = "A", comparator_treatment = "B")
    ),
    class = "mlumr_fit"
  )

  rmst <- predict(fit, type = "rmst", summary = FALSE)
  expect_false("mean" %in% names(rmst))
  expect_true("value" %in% names(rmst))
  expect_equal(nrow(rmst), 12L)

  median <- predict(fit, type = "median", summary = FALSE)
  expect_false("mean" %in% names(median))
  expect_true("value" %in% names(median))
  expect_equal(nrow(median), 12L)

  loghr <- predict(fit, type = "loghr", population = "comparator",
                   summary = FALSE)
  expect_false("mean" %in% names(loghr))
  expect_equal(names(loghr), c("population", "t_1", "t_2", "t_3"))
  expect_equal(nrow(loghr), 3L)
})

test_that("survival/cumhaz predict() curves are anchored at the t = 0 origin", {
  draws <- data.frame(
    "surv_index_comparator[1]" = c(0.92, 0.91, 0.90),
    "surv_index_comparator[2]" = c(0.62, 0.61, 0.60),
    "surv_comparator_comparator[1]" = c(0.89, 0.88, 0.87),
    "surv_comparator_comparator[2]" = c(0.52, 0.51, 0.50),
    "cumhaz_index_comparator[1]" = c(0.08, 0.09, 0.10),
    "cumhaz_index_comparator[2]" = c(0.48, 0.49, 0.50),
    "cumhaz_comparator_comparator[1]" = c(0.11, 0.12, 0.13),
    "cumhaz_comparator_comparator[2]" = c(0.65, 0.66, 0.67),
    "haz_index_comparator[1]" = c(0.10, 0.11, 0.12),
    "haz_index_comparator[2]" = c(0.12, 0.13, 0.14),
    "haz_comparator_comparator[1]" = c(0.08, 0.09, 0.10),
    "haz_comparator_comparator[2]" = c(0.10, 0.11, 0.12),
    check.names = FALSE
  )
  fit <- structure(
    list(
      family = "survival",
      draws = draws,
      pred_times = c(1.5, 3),
      data = list(index_treatment = "A", comparator_treatment = "B")
    ),
    class = "mlumr_fit"
  )

  # Survival starts at (time = 0, survival = 1) in every treatment cell.
  surv <- predict(fit, population = "comparator", type = "survival")
  first <- do.call(rbind, by(surv, surv$treatment,
                             function(d) d[which.min(d$time), ]))
  expect_true(all(first$time == 0))
  expect_true(all(first$mean == 1))
  expect_true(all(surv$mean >= 0 & surv$mean <= 1))

  # Cumulative hazard starts at (time = 0, value = 0).
  cumhaz <- predict(fit, population = "comparator", type = "cumhaz")
  first_ch <- do.call(rbind, by(cumhaz, cumhaz$treatment,
                                function(d) d[which.min(d$time), ]))
  expect_true(all(first_ch$time == 0))
  expect_true(all(first_ch$mean == 0))

  # Hazard is NOT anchored at 0 (no universal value there).
  haz <- predict(fit, population = "comparator", type = "hazard")
  expect_false(any(haz$time == 0))

  # An explicit `times` argument suppresses the injected origin.
  sub <- predict(fit, population = "comparator", type = "survival", times = 3)
  expect_false(any(sub$time == 0))

  # summary = FALSE prepends a t_0 column of 1s for survival.
  raw <- predict(fit, population = "comparator", type = "survival",
                 summary = FALSE)
  expect_true("t_0" %in% names(raw))
  expect_true(all(raw$t_0 == 1))
})

test_that("median survival NA handling distinguishes 'not reached' draws", {
  # .surv_median_from_draws() returns NA when survival never reaches 0.5 over the
  # grid; the predict() median path surfaces that NA fraction as `p_not_reached`
  # rather than silently dropping the draws.
  times <- c(1, 2, 3, 4, 5)
  surv_mat <- rbind(
    c(0.9, 0.8, 0.6, 0.4, 0.2),
    c(0.99, 0.98, 0.97, 0.96, 0.95)
  )
  med <- mlumr:::.surv_median_from_draws(surv_mat, times)
  expect_false(is.na(med[1]))
  expect_true(med[1] > 3 && med[1] < 4)
  expect_true(is.na(med[2]))
  expect_equal(mean(is.na(med)), 0.5)
})

test_that("median survival before the first grid point interpolates from S(0)=1", {
  # The prediction grid excludes time zero. When S(times[1]) <= 0.5 the median
  # lies before the first grid point; returning times[1] is an upward-biased
  # bound, not an interpolant. Anchor on the known exact point S(0) = 1.
  med <- mlumr:::.surv_median_from_draws
  times <- c(2, 4, 6, 8)

  # Exponential truth: median = log(2) / rate.
  for (rate in c(0.5, 0.35)) {
    s <- matrix(exp(-rate * times), nrow = 1)
    got <- med(s, times)
    expect_lte(got, times[1])                       # never exceeds the grid start
    expect_lt(abs(got - log(2) / rate),             # closer to truth than the bound
              abs(times[1] - log(2) / rate))
  }

  # S(times[1]) exactly 0.5 must return times[1] itself.
  s_half <- matrix(c(0.5, 0.4, 0.3, 0.2), nrow = 1)
  expect_equal(med(s_half, times), times[1])

  # Median beyond observed follow-up stays NA.
  expect_true(is.na(med(matrix(c(0.9, 0.8, 0.7, 0.6), nrow = 1), times)))

  # Ordinary interior case is unchanged.
  expect_equal(med(matrix(c(0.9, 0.6, 0.4, 0.2), nrow = 1), times), 5)
})
