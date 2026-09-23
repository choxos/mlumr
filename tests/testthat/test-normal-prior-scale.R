# The normal family's intercepts and coefficients (identity link) and its
# residual SD (either link) are in the outcome's units, so the package
# defaults, and autoscaled coefficient priors, are read in units of the IPD
# outcome SD. Priors the user writes out are used as given.

normal_prior_data <- function(scale = 20, n_ipd = 120, seed = 2026) {
  set.seed(seed)
  age <- stats::rnorm(n_ipd, 60, 8)
  ipd_df <- data.frame(trt = "A", age = age,
                       y = 140 + 0.5 * (age - 60) + stats::rnorm(n_ipd, 0, scale))
  agd_df <- data.frame(trt = "B", n = 80, y_mean = 145, y_se = 2,
                       age_mean = 65, age_sd = 8)
  ipd <- set_ipd(ipd_df, treatment = "trt", outcome = "y", covariates = "age",
                 family = "normal")
  agd <- set_agd(agd_df, treatment = "trt", family = "normal", outcome_n = "n",
                 outcome_mean = "y_mean", outcome_se = "y_se",
                 cov_means = "age_mean", cov_sds = "age_sd")
  dat <- suppressWarnings(
    add_integration(combine_data(ipd, agd), n_int = 32, verbose = FALSE,
                    age = distr(qnorm, mean = age_mean, sd = age_sd))
  )
  list(dat = dat, sd_y = stats::sd(ipd_df$y), sd_x = stats::sd(age))
}

build_normal <- function(dat, link, prior_intercept = default_prior_intercept(),
                         prior_beta = default_prior_beta(),
                         prior_sigma = default_prior_sigma()) {
  .mlumr_build_stan_data(dat, "normal", check_link("normal", link),
                         prior_intercept = prior_intercept,
                         prior_beta = prior_beta, prior_sigma = prior_sigma)
}

test_that("normal identity-link defaults are multiples of the IPD outcome SD", {
  d <- normal_prior_data()
  sd <- build_normal(d$dat, "identity")$stan_data
  expect_equal(sd$prior_intercept_sd, 10 * d$sd_y)
  expect_equal(as.numeric(sd$prior_beta_sd), 2.5 * d$sd_y)
  expect_equal(sd$prior_sigma_scale, 2.5 * d$sd_y)
  expect_equal(sd$prior_intercept_mean, 0)
})

test_that("under the log link only the residual SD prior is in outcome units", {
  d <- normal_prior_data()
  sd <- build_normal(d$dat, "log")$stan_data
  expect_equal(sd$prior_intercept_sd, 10)
  expect_equal(as.numeric(sd$prior_beta_sd), 2.5)
  expect_equal(sd$prior_sigma_scale, 2.5 * d$sd_y)
})

test_that("priors the user writes out are used as given", {
  d <- normal_prior_data()
  sd <- build_normal(d$dat, "identity",
                     prior_intercept = prior_normal(0, 10),
                     prior_beta = prior_normal(0, 2.5),
                     prior_sigma = prior_normal(0, 2.5))$stan_data
  expect_equal(sd$prior_intercept_sd, 10)
  expect_equal(as.numeric(sd$prior_beta_sd), 2.5)
  expect_equal(sd$prior_sigma_scale, 2.5)
})

test_that("autoscale includes the outcome SD under the normal identity link only", {
  d <- normal_prior_data()
  auto <- prior_normal(0, 1, autoscale = TRUE)
  ident <- build_normal(d$dat, "identity", prior_beta = auto)$stan_data
  logl <- build_normal(d$dat, "log", prior_beta = auto)$stan_data
  expect_equal(as.numeric(ident$prior_beta_sd), d$sd_y / d$sd_x)
  expect_equal(as.numeric(logl$prior_beta_sd), 1 / d$sd_x)
})

test_that("unit-free families keep their nominal default scales", {
  f <- stan_prior_fields_beta(default_prior_beta(), 2L, sd_x = c(3, 4))
  expect_equal(f$sd, c(2.5, 2.5))
  expect_equal(f$sd_y, c(1, 1))
  expect_identical(.outcome_scaled_prior(default_prior_intercept(), 1),
                   default_prior_intercept())
})

test_that("the fit records the priors the model used and reports them", {
  d <- normal_prior_data()
  prep <- build_normal(d$dat, "identity")
  priors <- .mlumr_prior_metadata(
    d$dat, "normal",
    prior_intercept = default_prior_intercept(),
    prior_beta = default_prior_beta(),
    prior_sigma = default_prior_sigma(),
    beta_fields = prep$beta_fields,
    sd_x = prep$sd_x,
    intercept_resolved = prep$prior_intercept,
    sigma_resolved = prep$prior_sigma,
    sd_y = prep$sd_y
  )
  # What the user passed stays, so a refit replays it; the used scale is kept too.
  expect_true(isTRUE(priors$intercept$default))
  expect_equal(priors$intercept$sd, 10)
  expect_equal(priors$intercept_resolved$sd, 10 * d$sd_y)
  expect_equal(priors$sigma_resolved$sd, 2.5 * d$sd_y)
  expect_equal(priors$beta_resolved$sd, 2.5 * d$sd_y)

  fit <- structure(list(priors = priors), class = "mlumr_fit")
  out <- capture.output(prior_summary(fit))
  expect_true(any(grepl(sprintf("used as normal(0, %s)",
                                format(10 * d$sd_y, digits = 3)),
                        out, fixed = TRUE)))
  expect_true(any(grepl("the IPD outcome SD", out)))
  # The overlay draws the prior the sampler saw.
  expect_equal(.parameter_prior(fit, "mu_index")$prior$sd, 10 * d$sd_y)
  expect_equal(.parameter_prior(fit, "sigma")$prior$sd, 2.5 * d$sd_y)
})

test_that("a prior-sensitivity sweep keeps the default's outcome units", {
  sd_y <- 7
  base <- stan_prior_fields_beta(default_prior_beta(), 1L, sd_x = 1, sd_y = sd_y)
  swept <- stan_prior_fields_beta(.rescale_prior_beta(default_prior_beta(), 2.5),
                                  1L, sd_x = 1, sd_y = sd_y)
  # The row at the original scale reproduces the original fit's prior.
  expect_equal(swept$sd, base$sd)
  user <- stan_prior_fields_beta(.rescale_prior_beta(prior_normal(0, 2.5), 2.5),
                                 1L, sd_x = 1, sd_y = sd_y)
  expect_equal(user$sd, 2.5)
})

test_that("default priors no longer pull a normal-outcome effect toward zero", {
  skip_on_cran()
  # Blood pressure in mmHg with a small comparator trial: the comparator
  # intercept rests on one aggregate mean, so a fixed normal(0, 10) prior
  # shrank it five times as much as the index intercept and moved the mean
  # difference by about two posterior SDs. The frequentist STC shares the
  # estimand and carries no prior.
  set.seed(2026)
  age_i <- stats::rnorm(300, 60, 8)
  age_c <- stats::rnorm(60, 65, 8)
  y_c <- 140 + 0.5 * (age_c - 60) + stats::rnorm(60, 0, 15)
  ipd <- set_ipd(data.frame(trt = "A", age = age_i,
                            y = 135 + 0.5 * (age_i - 60) + stats::rnorm(300, 0, 15)),
                 treatment = "trt", outcome = "y", covariates = "age",
                 family = "normal")
  agd <- set_agd(data.frame(trt = "B", n = 60, y_mean = mean(y_c),
                            y_se = stats::sd(y_c) / sqrt(60),
                            age_mean = mean(age_c), age_sd = stats::sd(age_c)),
                 treatment = "trt", family = "normal", outcome_n = "n",
                 outcome_mean = "y_mean", outcome_se = "y_se",
                 cov_means = "age_mean", cov_sds = "age_sd")
  dat <- suppressWarnings(
    add_integration(combine_data(ipd, agd), n_int = 64, verbose = FALSE,
                    age = distr(qnorm, mean = age_mean, sd = age_sd))
  )
  fit <- suppressWarnings(
    mlumr(dat, model = "spfa", chains = 2, iter = 1000, warmup = 500,
          seed = 2026, refresh = 0, verbose = FALSE)
  )
  md <- mean(fit$draws$delta_index)
  # Before the defaults were put on the outcome scale this was about +2.9
  # against STC's -1.35.
  expect_lt(abs(md - stc(dat)$md), 0.5)
})
