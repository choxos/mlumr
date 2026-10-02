# Code from vignette("data-preparation"): Preparing data and integration points
# mlumr GitHub main at 95c5bbd, extracted with knitr::purl(). Tables print as
# plain R output and the vignette's knitr setup chunk is left out; otherwise
# the code is the vignette's, unchanged. Chunks the vignette shows but does
# not run are commented out.

## ----eval = TRUE--------------------------------------------------------------
library(mlumr)
set.seed(2026)

# IPD (index): one row per patient
trial_a <- data.frame(
  treatment = "Drug_A",
  response = rbinom(500, 1, 0.6),
  age_group = rbinom(500, 1, 0.4),
  sex = rbinom(500, 1, 0.55)
)
ipd <- set_ipd(trial_a, treatment = "treatment", outcome = "response",
               covariates = c("age_group", "sex"))

# AgD (comparator): one row per study, with covariate summaries
trial_b <- data.frame(
  treatment = "Drug_B", n_total = 400, n_events = 160,
  age_group_mean = 0.35, sex_prop = 0.50
)
agd <- set_agd(trial_b, treatment = "treatment",
               outcome_n = "n_total", outcome_r = "n_events",
               cov_means = c("age_group_mean", "sex_prop"),
               cov_types = c("binary", "binary"))

dat <- combine_data(ipd, agd)
dat


## ----eval = TRUE--------------------------------------------------------------
dat <- add_integration(
  dat,
  n_int = 64,
  age_group = distr(qbern, prob = age_group_mean),
  sex = distr(qbern, prob = sex_mean)
)


## -----------------------------------------------------------------------------
# add_integration(
#   dat, n_int = 64,
#   age = distr(qbern, prob = age_mean),                  # binary
#   bmi = distr(qnorm, mean = bmi_mean, sd = bmi_sd),     # continuous (normal)
#   biomarker = distr(qgamma, mean = bio_mean, sd = bio_sd),  # right-skewed
#   bsa = distr(qlogitnorm, mean = bsa_mean, sd = bsa_sd)     # proportion on (0, 1)
# )


## -----------------------------------------------------------------------------
# my_cor <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
# add_integration(dat, n_int = 64, cor = my_cor,
#                 age_group = distr(qbern, prob = age_group_mean),
#                 sex = distr(qbern, prob = sex_mean))
# 
# add_integration(dat, n_int = 64, cor_adjust = "none",  # no adjustment
#                 age_group = distr(qbern, prob = age_group_mean),
#                 sex = distr(qbern, prob = sex_mean))


## ----eval = TRUE--------------------------------------------------------------
head(unnest_integration(dat))     # one row per point, covariates in columns


## ----eval = TRUE--------------------------------------------------------------
check_integration(
  dat,
  age_group = distr(qbern, prob = age_group_mean),
  sex = distr(qbern, prob = sex_mean)
)


## ----viz-int, eval = TRUE, fig.width = 7, fig.height = 4, fig.align = "center"----
set.seed(2026)
ipd_c <- data.frame(treatment = "Drug_A", response = rbinom(400, 1, 0.5),
                    age = rnorm(400, 55, 10))
agd_c <- data.frame(treatment = "Drug_B", n_total = 300, n_events = 120,
                    age_mean = 60, age_sd = 9)
dat_c <- combine_data(
  set_ipd(ipd_c, treatment = "treatment", outcome = "response", covariates = "age"),
  set_agd(agd_c, treatment = "treatment", outcome_n = "n_total",
          outcome_r = "n_events", cov_means = "age_mean", cov_sds = "age_sd",
          cov_types = "continuous"))
dat_c <- add_integration(dat_c, n_int = 128,
                         age = distr(qnorm, mean = age_mean, sd = age_sd))
pts <- unnest_integration(dat_c)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  ggplot(pts, aes(age)) +
    geom_histogram(aes(y = after_stat(density)), bins = 25,
                   fill = "#3B6B9A", alpha = 0.5) +
    stat_function(fun = dnorm, args = list(mean = 60, sd = 9),
                  color = "darkred", linewidth = 0.8) +
    labs(x = "Age (comparator integration points)", y = "Density",
         title = "QMC integration points reproduce the requested Normal(60, 9)") +
    theme_minimal()
}


## ----eval = TRUE--------------------------------------------------------------
dat_no_int <- combine_data(ipd, agd)
naive(dat_no_int)
stc(dat)

