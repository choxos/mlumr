test_that("stc computes LOR with delta-method SE", {
  set.seed(2026)
  n <- 200

  # Generate IPD with known effect
  x1 <- rbinom(n, 1, 0.4)
  x2 <- rbinom(n, 1, 0.6)
  logit_p <- -0.5 + 1.0 * x1 - 0.5 * x2
  outcome <- rbinom(n, 1, plogis(logit_p))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1, x2 = x2)
  agd_df <- data.frame(
    trt = "B", n_total = 300, n_events = 120,
    x1_mean = 0.3, x2_mean = 0.7
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = c("x1_mean", "x2_mean"))
  dat <- combine_data(ipd, agd)
  dat <- add_integration(
    dat, n_int = 32,
    x1 = distr(qbern, prob = x1_mean),
    x2 = distr(qbern, prob = x2_mean)
  )

  result <- stc(dat)

  expect_s3_class(result, "mlumr_stc")
  expect_true(is.finite(result$estimate))
  expect_gt(result$se, 0)
  expect_lt(result$ci_lower, result$estimate)
  expect_gt(result$ci_upper, result$estimate)

  # GLM fit should be stored
  expect_s3_class(result$glm_fit, "glm")

  # Print method works
  expect_output(print(result), "Simulated Treatment Comparison")
})


test_that("stc works with integration points", {
  set.seed(2026)
  n <- 100
  x1 <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.3 + 0.8 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(
    trt = "B", n_total = 200, n_events = 70, x1_mean = 0.4
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  # Add integration points
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  result <- stc(dat)
  expect_s3_class(result, "mlumr_stc")
  expect_true(is.finite(result$estimate))
})


test_that("stc respects custom conf_level", {
  set.seed(2026)
  n <- 200
  x1 <- rbinom(n, 1, 0.4)
  outcome <- rbinom(n, 1, plogis(-0.5 + 1.0 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(trt = "B", n_total = 300, n_events = 120, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  r95 <- stc(dat, conf_level = 0.95)
  r90 <- stc(dat, conf_level = 0.90)

  expect_lt(r90$ci_upper - r90$ci_lower, r95$ci_upper - r95$ci_lower)
})


test_that("stc validates data and conf_level", {
  ipd_df <- data.frame(trt = "A", outcome = c(1, 0, 1, 0, 1, 0),
                       x1 = c(0, 1, 0, 1, 0, 1))
  agd_df <- data.frame(trt = "B", n_total = 20, n_events = 8, x1_mean = 0.5)

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_error(stc(data.frame()), "combine_data")
  expect_error(stc(dat, conf_level = 1), "`conf_level`")
  expect_error(stc(dat, conf_level = c(0.90, 0.95)), "`conf_level`")
  expect_error(stc(dat, conf_level = NA_real_), "`conf_level`")
})


test_that("stc supports non-syntactic covariate names", {
  set.seed(2026)
  n <- 120
  x <- rbinom(n, 1, 0.4)
  outcome <- rbinom(n, 1, plogis(-0.2 + 0.7 * x))
  ipd_df <- data.frame(trt = "A", outcome = outcome, check.names = FALSE)
  ipd_df[["x one"]] <- x
  agd_df <- data.frame(trt = "B", n_total = 180, n_events = 70,
                       check.names = FALSE)
  agd_df[["x one_mean"]] <- 0.3

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x one")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x one_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         `x one` = distr(qbern, prob = `x one_mean`))

  result <- stc(dat)
  expect_s3_class(result, "mlumr_stc")
  expect_true(is.finite(result$estimate))
  expect_true(is.finite(result$se))
})


test_that("stc handles multi-row AgD with weighted integration", {
  set.seed(2026)
  n <- 150
  x1 <- rbinom(n, 1, 0.5)
  outcome <- rbinom(n, 1, plogis(-0.3 + 0.8 * x1))

  ipd_df <- data.frame(trt = "A", outcome = outcome, x1 = x1)
  agd_df <- data.frame(
    trt = c("B", "B"),
    n_total = c(100, 200),
    n_events = c(35, 80),
    x1_mean = c(0.3, 0.5)
  )

  ipd <- set_ipd(ipd_df, "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total",
                 outcome_r = "n_events", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  expect_error(stc(dat), "requires comparator-population integration")

  # With integration points
  dat <- add_integration(dat, n_int = 32, x1 = distr(qbern, prob = x1_mean))
  result_int <- stc(dat)
  expect_s3_class(result_int, "mlumr_stc")
  expect_true(is.finite(result_int$estimate))
})


# ---- Normal family ----

test_that("stc works with normal family", {
  set.seed(2026)
  n <- 200
  x1 <- rnorm(n, 0, 1)
  score <- 2.0 + 0.5 * x1 + rnorm(n, 0, 1)

  ipd_df <- data.frame(trt = "A", score = score, x1 = x1)
  agd_df <- data.frame(trt = "B", y_mean = 1.5, se = 0.2, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "score", "x1", family = "normal")
  agd <- set_agd(agd_df, "trt", family = "normal",
                 outcome_mean = "y_mean", outcome_se = "se",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)

  result <- stc(dat)
  expect_s3_class(result, "mlumr_stc")
  expect_equal(result$family, "normal")
  expect_true(is.finite(result$estimate))
  expect_gt(result$se, 0)

  # Print works
  expect_output(print(result), "Mean Difference")
})

test_that("normal STC combines aggregate strata using population weights", {
  ipd_df <- data.frame(
    trt = "A", y = 0:5, x = -2:3
  )
  agd_df <- data.frame(
    trt = c("B", "B"), n = c(10, 90), y = c(0, 10),
    se = c(0.05, 5), x_mean = c(-1, 1)
  )
  dat <- combine_data(
    set_ipd(ipd_df, "trt", "y", "x", family = "normal"),
    set_agd(
      agd_df, "trt", family = "normal", outcome_n = "n",
      outcome_mean = "y", outcome_se = "se", cov_means = "x_mean",
      cov_types = "continuous"
    )
  )

  result <- stc(dat)
  population_weights <- agd_df$n / sum(agd_df$n)
  target_profiles <- data.frame(x = agd_df$x_mean)
  target_predictions <- predict(result$glm_fit, target_profiles, type = "response")

  expect_equal(result$y_hat_index,
               weighted.mean(target_predictions, agd_df$n), tolerance = 1e-12)
  expect_equal(result$y_comparator, weighted.mean(agd_df$y, agd_df$n),
               tolerance = 1e-12)
  expect_equal(result$y_comparator_se,
               sqrt(sum(population_weights^2 * agd_df$se^2)),
               tolerance = 1e-12)
  expect_gt(abs(result$y_comparator -
                  weighted.mean(agd_df$y, 1 / agd_df$se^2)), 1)

  expect_error(
    set_agd(
      agd_df, "trt", family = "normal", outcome_mean = "y",
      outcome_se = "se", cov_means = "x_mean", cov_types = "continuous"
    ),
    "outcome_n.*multiple normal aggregate rows"
  )
})


# ---- Poisson family ----

test_that("stc works with poisson family", {
  set.seed(2026)
  n <- 200
  x1 <- rbinom(n, 1, 0.4)
  exposure <- runif(n, 0.5, 2.0)
  events <- rpois(n, exp(0.5 + 0.3 * x1) * exposure)

  ipd_df <- data.frame(trt = "A", events = events, pyears = exposure, x1 = x1)
  agd_df <- data.frame(trt = "B", n_events = 80, pyears = 400, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  result <- stc(dat)
  expect_s3_class(result, "mlumr_stc")
  expect_equal(result$family, "poisson")
  expect_true(is.finite(result$estimate))
  expect_gt(result$se, 0)

  # Print works
  expect_output(print(result), "Log Rate Ratio")
})


test_that("stc handles zero-event Poisson comparator data", {
  set.seed(2026)
  n <- 200
  x1 <- rbinom(n, 1, 0.4)
  exposure <- runif(n, 0.5, 2.0)
  events <- rpois(n, exp(0.3 + 0.2 * x1) * exposure)

  ipd_df <- data.frame(trt = "A", events = events, pyears = exposure, x1 = x1)
  agd_df <- data.frame(trt = "B", n_events = 0, pyears = 400, x1_mean = 0.3)

  ipd <- set_ipd(ipd_df, "trt", "events", "x1", family = "poisson",
                 exposure = "pyears")
  agd <- set_agd(agd_df, "trt", family = "poisson",
                 outcome_r = "n_events", outcome_E = "pyears",
                 cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat, n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))

  result <- stc(dat)
  expect_true(is.finite(result$estimate))
  expect_true(is.finite(result$se))
  expect_equal(result$rate_comparator, 0)
  expect_equal(result$events_comparator_adjusted, 0.5)
})

test_that("print.mlumr_stc distinguishes disabled / partial / full bootstrap", {
  mk <- function(se, req, okn) {
    has_ci <- !is.na(se)
    structure(list(
      family = "survival",
      data = list(index_treatment = "A", comparator_treatment = "B"),
      approximated = FALSE, distribution = "weibull",
      distribution_fit = "weibull", horizon = 10,
      rmst_index = 6, rmst_comparator = 5, estimate = 1, se = se,
      conf_level = 0.95,
      ci_lower = if (has_ci) 0.5 else NA_real_,
      ci_upper = if (has_ci) 1.5 else NA_real_,
      n_boot = okn, n_boot_requested = req, n_boot_ok = okn, glm_fit = NULL
    ), class = c("mlumr_stc", "list"))
  }
  grab <- function(x) paste(utils::capture.output(print(x)), collapse = "\n")
  expect_match(grab(mk(0.2, 200L, 200L)), "200 reps")
  partial <- grab(mk(0.2, 200L, 150L))
  expect_match(partial, "150 of 200")
  expect_match(partial, "50 failed")
  expect_match(grab(mk(NA_real_, 0L, 0L)), "bootstrap disabled")
  expect_match(grab(mk(NA_real_, 200L, 0L)), "all 200 bootstrap")
})

test_that("stc() validates n_boot and the distribution name", {
  skip_if_not_installed("flexsurv")
  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)

  # sd() of a single resample is NA, which downstream is indistinguishable from
  # "every resample failed": stc's print method reported a successful single
  # replicate as a total bootstrap failure. Reject the input instead.
  expect_error(stc(dat, n_boot = 1), "must be 0 .* or at least 2")
  expect_error(stc(dat, n_boot = 1), "single resample is undefined")

  # A misspelled distribution used to fall through switch()'s unnamed default
  # and silently fit a Weibull, while the returned object still reported the
  # name the user typed and approximated = FALSE. Wrong model, wrong label, no
  # warning.
  expect_error(stc(dat, distribution = "weibul", n_boot = 0), "must be one of")
  expect_error(stc(dat, distribution = c("weibull", "gamma"), n_boot = 0),
               "must be one of")
  expect_error(stc(dat, distribution = NA_character_, n_boot = 0), "must be one of")

  # 0 (bootstrap off) and a valid name remain valid input.
  fit <- stc(dat, distribution = "weibull", n_boot = 0)
  expect_s3_class(fit, "mlumr_stc")
  expect_true(is.na(fit$se))
})

test_that("survival STC records the shape parameter mlumr constrains", {
  skip_on_cran()
  skip_if_not_installed("flexsurv")
  dat <- sim_survival_data(seed = 2026, n_ipd = 40, n_agd = 40, n_int = 8)

  # mlumr's Bayesian Gompertz constrains the shape to be positive and its
  # generalized gamma is the positive-Q subfamily; flexsurv admits the negative
  # branch of both. Record the fitted value so an STC benchmark that leaves the
  # family its label names can say so instead of passing as like-for-like.
  gg <- suppressWarnings(stc(dat, distribution = "gengamma", n_boot = 0))
  expect_equal(gg$family_par_name %||% "Q", "Q")
  expect_named(gg$family_par, c("index", "comparator"))
  expect_true(all(is.finite(gg$family_par)))

  # A distribution with no such constraint records nothing and is never flagged.
  wb <- stc(dat, distribution = "weibull", n_boot = 0)
  expect_null(wb$family_par)
  expect_null(wb$out_of_family)
})

test_that("stc print reports partial bootstrap success and out-of-family fits", {
  # sd() is NA both when every resample failed and when exactly one survived.
  # Those are different events and must not print the same sentence.
  # `rmst_diff` is what .effect_measures_df() dispatches on to tell an STC
  # survival result from a naive Cox one, and .stc_survival() always returns it.
  # Omitting it here sent the fixture down the naive branch, so the test printed
  # "Log hazard ratio (Cox)" rows and never exercised the survival STC table it
  # was meant to cover.
  base <- list(family = "survival", distribution = "weibull", horizon = 40,
               rmst_index = 30, rmst_comparator = 27, estimate = 3,
               rmst_diff = 3,
               se = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
               conf_level = 0.95, log_chr = 0.1, chr = exp(0.1),
               log_chr_se = NA_real_, log_chr_lower = NA_real_,
               log_chr_upper = NA_real_, n_index = 40, n_comparator = 40)

  none <- structure(c(base, list(n_boot_requested = 200L, n_boot_ok = 0L)),
                    class = c("mlumr_stc", "list"))
  expect_output(print(none), "all 200 bootstrap resample\\(s\\) failed")

  one <- structure(c(base, list(n_boot_requested = 200L, n_boot_ok = 1L)),
                   class = c("mlumr_stc", "list"))
  expect_output(print(one), "1 of 200 bootstrap resample\\(s\\) succeeded")
  expect_output(print(one), "too few for a standard error")

  off <- structure(c(base, list(n_boot_requested = 0L, n_boot_ok = 0L)),
                   class = c("mlumr_stc", "list"))
  expect_output(print(off), "bootstrap disabled")

  # A fit outside mlumr's parameter space is named as such, not as a plain
  # distribution and not as the Weibull flexible-baseline approximation.
  # Build by assignment, not c(): c() keeps BOTH entries when a name repeats and
  # `$` then returns the first, so overriding `distribution` this way silently
  # left the base "weibull" in place.
  oof <- base
  oof$distribution <- "gengamma"
  oof$distribution_fit <- "flexsurv gengamma, unrestricted Q"
  oof$approximated <- TRUE
  oof$out_of_family <- "Q"
  oof$family_par <- c(index = -0.4, comparator = 0.3)
  oof$n_boot_requested <- 0L
  oof$n_boot_ok <- 0L
  oof <- structure(oof, class = c("mlumr_stc", "list"))
  expect_output(print(oof), "outside mlumr\\(\\)'s 'gengamma' parameter space")
  expect_output(print(oof), "-0.4")

  # The effect table must be the survival STC one: an RMST difference plus the
  # cumulative-hazard-ratio rows, and never the naive Cox rows.
  expect_output(print(none), "RMST difference")
  expect_output(print(none), "Log cumulative-hazard ratio \\(at horizon\\)")
  expect_output(print(none), "Cumulative-hazard ratio \\(at horizon\\)")
  expect_failure(expect_output(print(none), "Hazard ratio \\(Cox\\)"))
})
