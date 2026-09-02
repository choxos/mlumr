devtools::load_all(quiet = TRUE)

fields <- stan_prior_fields_beta(
  prior_normal(2, 4, autoscale = TRUE), n_cov = 1, sd_x = 2
)
stopifnot(identical(fields$mean, 1), identical(fields$sd, 2))
cat("autoscale_mean=", fields$mean, " autoscale_sd=", fields$sd, "\n", sep = "")

sd_low <- list(
  X_ipd = matrix(0, 100, 1), X_int = array(9.5, c(1, 8, 1)), n_agd = 200
)
sd_high <- list(
  X_ipd = matrix(0, 100, 1), X_int = array(10.2, c(1, 128, 1)), n_agd = 200
)
c_low <- mlumr:::.mlumr_center_covariates(
  sd_low, agd_means = 10
)$cov_center
c_high <- mlumr:::.mlumr_center_covariates(
  sd_high, agd_means = 10
)$cov_center
stopifnot(identical(c_low, c_high))
cat("declared_center=", c_low, "\n", sep = "")

set.seed(11)
ipd <- set_ipd(
  data.frame(trt = "A", y = rbinom(60, 1, 0.5), x1 = rlnorm(60), x2 = rnorm(60)),
  "trt", "y", c("x1", "x2")
)
agd <- set_agd(
  data.frame(trt = "B", n = 100, r = 40,
             x1_mean = 1.5, x1_sd = 1, x2_mean = 0, x2_sd = 1),
  "trt", outcome_n = "n", outcome_r = "r",
  cov_means = c("x1_mean", "x2_mean"), cov_sds = c("x1_sd", "x2_sd")
)
pearson_error <- tryCatch({
  add_integration(
    combine_data(ipd, agd), cor = matrix(c(1, 0.5, 0.5, 1), 2),
    cor_adjust = "pearson", x1 = distr(qlnorm, meanlog = 0, sdlog = 1),
    x2 = distr(qnorm, mean = x2_mean, sd = x2_sd), verbose = FALSE
  )
  NA_character_
}, error = conditionMessage)
stopifnot(grepl("non-Gaussian continuous margins", pearson_error))
cat("pearson_guard=", pearson_error, "\n", sep = "")

source("tests/testthat/helper-survival.R")
surv <- sim_survival_data(n_ipd = 40, n_agd = 50, n_int = 8)
info <- mlumr:::.survival_distribution_info("mspline")
shared_knots <- make_knots(surv, n_knots = 3, type = "equal")
common_time <- min(max(surv$ipd$data$.time), max(surv$agd$pseudo_ipd$.time))
stan_surv <- mlumr:::.build_stan_data_survival(
  list(), surv, info, pred_times = common_time, n_knots = 7,
  knots = shared_knots, rmst_horizon = common_time,
  prior_aux = default_prior_aux(), prior_smooth = default_prior_smooth(),
  n_strata = 1L
)
stopifnot(stan_surv$n_scoef == length(shared_knots$internal) + 4L)
cat("custom_knots_n_scoef=", stan_surv$n_scoef, "\n", sep = "")

cat("manual_scenarios=PASS\n")
