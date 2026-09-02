# Frequentist survival benchmark tests (naive Cox, STC via flexsurv).

test_that("naive survival gives a Cox log hazard ratio", {
  skip_if_not_installed("survival")

  dat <- sim_survival_data(seed = 2026)
  res <- naive(dat)
  expect_s3_class(res, "mlumr_naive")
  expect_equal(res$family, "survival")
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se))

  # matches a direct coxph on the pooled data
  ipd <- dat$ipd$data
  pseudo <- dat$agd$pseudo_ipd
  pooled <- data.frame(
    time = c(ipd$.time, pseudo$.time),
    event = as.integer(c(ipd$.status, pseudo$.status) == 1L),
    arm = factor(c(rep("index", nrow(ipd)), rep("comparator", nrow(pseudo))),
                 levels = c("comparator", "index"))
  )
  cox <- survival::coxph(survival::Surv(time, event) ~ arm, data = pooled)
  expect_equal(res$estimate, unname(stats::coef(cox)[1]), tolerance = 1e-6)

  expect_output(print(res), "Hazard Ratio")
})

test_that("survival STC returns an RMST difference via flexsurv", {
  skip_on_cran()
  skip_if_not_installed("flexsurv")

  dat <- sim_survival_data(seed = 2026)
  res <- stc(dat, distribution = "weibull")
  expect_s3_class(res, "mlumr_stc")
  expect_equal(res$family, "survival")
  expect_true(is.finite(res$rmst_index))
  expect_true(is.finite(res$rmst_comparator))
  expect_true(is.finite(res$estimate))
  expect_output(print(res), "RMST Difference")
})

test_that("survival STC flags flexible baselines as a Weibull approximation", {
  skip_on_cran()
  skip_if_not_installed("flexsurv")

  dat <- sim_survival_data(seed = 2026)
  # "mspline"/"pexp" have no parametric flexsurv analogue: STC must approximate
  # with Weibull, warn, and report the substitution honestly instead of claiming
  # it fitted the requested flexible baseline.
  expect_warning(res <- stc(dat, distribution = "mspline", n_boot = 0),
                 "Weibull G-computation")
  expect_true(res$approximated)
  expect_equal(res$distribution, "mspline")     # requested value preserved
  expect_equal(res$distribution_fit, "weibull") # what was actually fitted
  expect_output(print(res), "Weibull approximation")

  # the same Weibull fit, requested explicitly, must not warn or flag
  expect_warning(res_w <- stc(dat, distribution = "weibull", n_boot = 0), NA)
  expect_false(res_w$approximated)
  expect_equal(res_w$distribution_fit, "weibull")
  expect_equal(res$estimate, res_w$estimate)    # identical underlying Weibull fit
})

test_that("survival STC is reproducible with a seed and preserves the RNG stream", {
  skip_on_cran()
  skip_if_not_installed("flexsurv")

  dat <- sim_survival_data(seed = 2026)
  r1 <- stc(dat, distribution = "weibull", n_boot = 15, seed = 2026)
  r2 <- stc(dat, distribution = "weibull", n_boot = 15, seed = 2026)
  expect_equal(r1$se, r2$se)
  expect_equal(r1$ci_lower, r2$ci_lower)
  expect_equal(r1$n_boot, 15L)
  # the point estimate is independent of the bootstrap RNG
  expect_equal(r1$estimate, r2$estimate)

  # the internal bootstrap seed must not perturb the caller's global RNG stream
  set.seed(99)
  ref <- stats::runif(3)
  set.seed(99)
  invisible(stc(dat, distribution = "weibull", n_boot = 8, seed = 2026))
  expect_equal(stats::runif(3), ref)
})

test_that("survival STC n_boot = 0 gives a point estimate with no interval", {
  skip_on_cran()
  skip_if_not_installed("flexsurv")

  dat <- sim_survival_data(seed = 2026)
  res <- stc(dat, distribution = "weibull", n_boot = 0)
  expect_true(is.finite(res$estimate))
  expect_true(is.na(res$se))
  expect_true(is.na(res$ci_lower) && is.na(res$ci_upper))
  expect_equal(res$n_boot, 0L)
  expect_output(print(res), "point estimate only")
})

test_that("survival STC rejects censoring forms it cannot model", {
  skip_on_cran()
  skip_if_not_installed("flexsurv")

  dat_left <- sim_survival_data(seed = 2026)
  dat_left$ipd$data$.status[1] <- 2L
  expect_error(
    stc(dat_left, distribution = "weibull", n_boot = 0),
    "right-censored data without delayed entry"
  )

  dat_delayed <- sim_survival_data(seed = 2026)
  dat_delayed$ipd$data$.delay_time[1] <- dat_delayed$ipd$data$.time[1] / 2
  expect_error(
    stc(dat_delayed, distribution = "weibull", n_boot = 0),
    "right-censored data without delayed entry"
  )
})

test_that("survival STC rejects an invalid n_boot", {
  dat <- sim_survival_data(seed = 2026)
  expect_error(stc(dat, distribution = "weibull", n_boot = -5), "n_boot")
})

test_that("stc() integrates RMST to the requested horizon", {
  skip_on_cran()
  skip_if_not_installed("flexsurv")
  # RMST is an integral to a restriction time, so an STC result is comparable
  # with an mlumr() fit only when both use the same one. stc() defaults to the
  # pooled maximum while a stratified flexible baseline defaults to the common
  # follow-up, so without this argument the two cannot be put on one axis.
  dat <- sim_survival_data(seed = 2026, n_int = 16)
  obs_max <- max(c(dat$ipd$data$.time, dat$agd$pseudo_ipd$.time))
  tau <- obs_max * 0.6

  d <- stc(dat, distribution = "weibull", n_boot = 0L, seed = 2026,
           rmst_horizon = tau)
  expect_equal(d$horizon, tau)

  # The default is unchanged.
  d0 <- stc(dat, distribution = "weibull", n_boot = 0L, seed = 2026)
  expect_equal(d0$horizon, obs_max)
  # A shorter horizon integrates less survival time, so the two differ.
  expect_false(isTRUE(all.equal(d$rmst_diff, d0$rmst_diff)))

  expect_error(stc(dat, distribution = "weibull", n_boot = 0L,
                   rmst_horizon = -1), "single positive finite time")
  expect_error(stc(dat, distribution = "weibull", n_boot = 0L,
                   rmst_horizon = c(1, 2)), "single positive finite time")
  expect_warning(stc(dat, distribution = "weibull", n_boot = 0L, seed = 2026,
                     rmst_horizon = obs_max * 1.5), "beyond the largest")
})

test_that("survival STC reports each bootstrap quantity's own replicate count", {
  skip_on_cran()
  skip_if_not_installed("flexsurv")

  # Gompertz: flexsurv admits a negative shape, whereas mlumr()'s Bayesian model
  # does not. Stable log-survival calculations keep the cumulative-hazard ratio
  # finite in the same resamples as the RMST difference.
  dat <- sim_survival_data(seed = 2026, n_ipd = 80, n_agd = 90)
  res <- suppressWarnings(
    stc(dat, distribution = "gompertz", n_boot = 25, seed = 2026))

  # The two quantities are counted separately even when all resamples succeed.
  expect_equal(res$n_boot_ok, 25L)
  expect_equal(res$n_boot_ok_log_chr, res$n_boot_ok)
  expect_true(is.finite(res$log_chr_se))
  expect_gt(res$n_boot_out_of_family, 0L)
  expect_lte(res$n_boot_out_of_family, 25L)
  expect_equal(res$family_par_name, "shape")

  # The parameter-space mismatch remains visible, but there is no spurious
  # warning about failed cumulative-hazard calculations.
  msgs <- character(0)
  withCallingHandlers(
    invisible(stc(dat, distribution = "gompertz", n_boot = 25, seed = 2026)),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_false(any(grepl("Bootstrap successes", msgs, fixed = TRUE)))
  expect_true(any(grepl("outside the parameter space", msgs, fixed = TRUE)))

  # The parameter-space qualification remains visible in the printout.
  expect_output(print(res), "outside mlumr\\(\\)'s 'gompertz' parameter space")

  # A distribution with no sign-constrained parameter reports NA, not zero: the
  # question was never asked, which is not the same as asked and answered no.
  res_w <- suppressWarnings(
    stc(dat, distribution = "weibull", n_boot = 25, seed = 2026))
  expect_true(is.na(res_w$n_boot_out_of_family))
  expect_null(res_w$family_par_name)
  expect_false(grepl("parameter space",
                     paste(utils::capture.output(print(res_w)), collapse = "\n")))
})

test_that("a log-link normal STC reports the difference it computed", {
  # .stc_normal() standardizes on the response scale and returns y_hat_A - y_B
  # for every link. Labelling that a log mean ratio and exponentiating it turned
  # a difference of 2 into a reported "mean ratio" of 7.39 when the true ratio
  # was 1.2, so the label must not vary with the link.
  mk <- function(link) {
    structure(list(family = "normal", link = link, estimate = 2, se = 0.5,
                   ci_lower = 1, ci_upper = 3, conf_level = 0.95,
                   n_index = 10, n_comparator = 10),
              class = c("mlumr_stc", "list"))
  }
  for (lk in c("identity", "log")) {
    df <- mlumr:::.effect_measures_df(mk(lk))
    expect_equal(df$Measure, "Mean difference", info = lk)
    expect_equal(df$Estimate, 2, info = lk)
    # Nothing exponentiated: exp(2) = 7.389 must not appear anywhere.
    expect_false(any(abs(unlist(df[, -1]) - exp(2)) < 1e-6, na.rm = TRUE),
                 info = lk)
  }
})

test_that("a boundary comparator arm still carries uncertainty", {
  # With the raw proportion, p(1 - p) = 0 for a zero-event arm, so the
  # comparator contributed no variance: p_comparator_se was 0, its interval
  # collapsed to [0, 0], and the risk-difference SE ignored the comparator
  # entirely, although 0/100 alone is consistent with p up to about 0.03.
  set.seed(2026)
  ip <- set_ipd(data.frame(trt = "A", x = stats::rnorm(100),
                           r = c(rep(1, 50), rep(0, 50))),
                "trt", outcome = "r", covariates = "x", family = "binomial")
  ag <- set_agd(data.frame(trt = "B", r = 0, n = 100, x_mean = 0, x_sd = 1),
                "trt", family = "binomial", outcome_r = "r", outcome_n = "n",
                cov_means = "x_mean", cov_sds = "x_sd",
                cov_types = "continuous")
  res <- suppressWarnings(suppressMessages(naive(combine_data(ip, ag))))

  expect_gt(res$p_comparator_se, 0)
  expect_gt(res$p_comparator_upper, res$p_comparator_lower)
  # The risk-difference SE must exceed the index-only SE, i.e. the comparator
  # actually contributes.
  expect_gt(res$rd_se, res$p_index_se)
})

test_that("the naive effect SE does not depend on how AgD rows are tabulated", {
  # bound_probability() shifts a boundary arm by min_count / (n + 2 min_count),
  # which depends on that row's n. Applied per row, splitting one aggregate arm
  # into two identical halves moved the standard error by a factor of 1.40 on
  # the same data while leaving the estimate unchanged.
  set.seed(2026)
  mk <- function(agd) {
    ip <- set_ipd(data.frame(trt = "A", x = stats::rnorm(100),
                             r = c(rep(1, 50), rep(0, 50))),
                  "trt", outcome = "r", covariates = "x", family = "binomial")
    ag <- set_agd(agd, "trt", family = "binomial", outcome_r = "r",
                  outcome_n = "n", cov_means = "x_mean", cov_sds = "x_sd",
                  cov_types = "continuous")
    suppressWarnings(suppressMessages(naive(combine_data(ip, ag))))
  }
  one <- mk(data.frame(trt = "B", r = 0, n = 100, x_mean = 0, x_sd = 1))
  two <- mk(data.frame(trt = "B", r = c(0, 0), n = c(50, 50),
                       x_mean = c(0, 0), x_sd = c(1, 1)))

  expect_equal(one$estimate, two$estimate)
  expect_equal(one$se, two$se)
  expect_equal(one$rd_se, two$rd_se)
})
