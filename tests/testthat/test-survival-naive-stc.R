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
