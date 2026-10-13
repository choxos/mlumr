# Code from vignette("binary-outcomes"): Binary outcomes: an unanchored PASI 75 comparison
# mlumr GitHub main at 6a366a0, extracted with knitr::purl(). Tables print as
# plain R output and the vignette's knitr setup chunk is left out; otherwise
# the code is the vignette's, unchanged. Chunks the vignette shows but does
# not run are commented out.

## ----packages, message = FALSE------------------------------------------------
library(mlumr)
library(ggplot2)
options(mc.cores = parallel::detectCores())


## ----data---------------------------------------------------------------------
# IPD + AgD bundled with mlumr (copied from multinma's plaque_psoriasis, GPL-3).
data("psoriasis_ipd")
data("psoriasis_agd")

covs <- c("age", "bsa", "weight", "prevsys")   # adjustment set for this example

# --- Index IPD: UNCOVER-2, ixekizumab Q4W -----------------------------------
ipd <- psoriasis_ipd
ipd$bsa     <- ipd$bsa / 100      # body-surface area: % -> proportion
ipd <- ipd[ipd$study == "UNCOVER-2" & ipd$treatment == "IXE_Q4W", ]
ipd <- ipd[stats::complete.cases(ipd[, c("pasi75", covs)]), ]

# --- Comparator AgD: FIXTURE, secukinumab 300 mg ----------------------------
agd <- psoriasis_agd
agd$bsa_mean    <- agd$bsa_mean / 100
agd$bsa_sd      <- agd$bsa_sd / 100
agd <- agd[agd$study == "FIXTURE" & agd$treatment == "SEC_300", ]


## ----data-head----------------------------------------------------------------
head(ipd[, c("study", "treatment", "pasi75", covs)])
agd[, c("study", "treatment", "pasi75_r", "pasi75_n",
           "age_mean", "bsa_mean", "weight_mean", "prevsys_prop")]


## ----balance------------------------------------------------------------------
balance <- data.frame(
  Covariate  = c("Age (years)", "Body-surface area (prop.)",
                 "Weight (kg)", "Previous systemic (prop.)"),
  Index_IXE  = c(mean(ipd$age), mean(ipd$bsa), mean(ipd$weight), mean(ipd$prevsys)),
  Comparator_SEC = c(agd$age_mean, agd$bsa_mean, agd$weight_mean, agd$prevsys_prop)
)
balance


## ----setup-data---------------------------------------------------------------
ipd_obj <- set_ipd(ipd, treatment = "treatment", outcome = "pasi75", covariates = covs)

agd_obj <- set_agd(agd, treatment = "treatment",
                   outcome_n = "pasi75_n", outcome_r = "pasi75_r",
                   cov_means = c("age_mean", "bsa_mean", "weight_mean", "prevsys_prop"),
                   cov_sds   = c("age_sd", "bsa_sd", "weight_sd", NA),
                   cov_types = c("continuous", "continuous", "continuous", "binary"))

dat <- combine_data(ipd_obj, agd_obj)
dat


## ----integration--------------------------------------------------------------
dat <- add_integration(
  dat, n_int = 64,
  # Marginals follow multinma's own plaque-psoriasis example: gamma for the
  # right-skewed continuous covariates, logit-normal for body surface area
  # (a proportion), Bernoulli for the binary one. mlumr's qgamma() and
  # qlogitnorm() accept a mean and sd directly, so the published baseline
  # table can be used as printed.
  age     = distr(qgamma,     mean = age_mean,    sd = age_sd),
  bsa     = distr(qlogitnorm, mean = bsa_mean,    sd = bsa_sd),
  weight  = distr(qgamma,     mean = weight_mean, sd = weight_sd),
  prevsys = distr(qbern,      prob = prevsys_mean)
)


## ----check-int----------------------------------------------------------------
check_integration(
  dat,
  age     = distr(qgamma,     mean = age_mean,    sd = age_sd),
  bsa     = distr(qlogitnorm, mean = bsa_mean,    sd = bsa_sd),
  weight  = distr(qgamma,     mean = weight_mean, sd = weight_sd),
  prevsys = distr(qbern,      prob = prevsys_mean)
)


## ----benchmarks---------------------------------------------------------------
res_naive <- naive(dat)
res_stc   <- stc(dat)
res_naive
res_stc


## ----fit----------------------------------------------------------------------
fit_spfa <- mlumr(dat, model = "spfa", link = "logit",
                  prior_beta = prior_normal(0, 2.5, autoscale = TRUE),
                  chains = 4, iter = 2000, warmup = 1000,
                  seed = 2026, refresh = 0)

fit_relaxed <- mlumr(dat, model = "relaxed", link = "logit",
                     prior_beta = prior_normal(0, 2.5, autoscale = TRUE),
                     chains = 4, iter = 2000, warmup = 1000,
                     seed = 2026, refresh = 0)

summary(fit_spfa)


## ----priors-------------------------------------------------------------------
prior_summary(fit_spfa)


## ----prior-post, fig.height = 2.6---------------------------------------------
plot_prior_posterior(fit_spfa, pars = c("mu_index", "mu_comparator"))


## ----separation, eval = FALSE-------------------------------------------------
# # Student-t coefficients (df 3-7 is a robust default), autoscaled as above:
# fit_sep <- mlumr(dat, model = "spfa",
#                  prior_beta = prior_student_t(df = 5, 0, 2.5, autoscale = TRUE))
# # prior_cauchy(0, 2.5) is the df = 1 special case (heaviest tails).


## ----diagnostics--------------------------------------------------------------
data.frame(
  n_divergent  = fit_spfa$diagnostics$n_divergent,
  max_treedepth = fit_spfa$diagnostics$n_max_treedepth,
  max_Rhat     = round(max(fit_spfa$summary$Rhat, na.rm = TRUE), 3),
  min_ESS      = round(min(fit_spfa$summary$n_eff, na.rm = TRUE))
)


## ----effects------------------------------------------------------------------
me_spfa <- marginal_effects(fit_spfa, effect = "all")
me_spfa


## ----forest, fig.height = 3.6-------------------------------------------------
me_spfa_b <- marginal_effects(fit_spfa, effect = "lor", population = "both")
me_rel_b  <- marginal_effects(fit_relaxed, effect = "lor", population = "both")
# One row per (model, population); `pop()` pulls the requested one.
pop <- function(d, which) d[d$population == which, ]
forest_df <- data.frame(
  label = c("Naive (unstandardized)", "STC (comparator)",
            "ML-UMR SPFA (index)", "ML-UMR SPFA (comparator)",
            "ML-UMR relaxed (index)", "ML-UMR relaxed (comparator)"),
  est = c(res_naive$link_effect, res_stc$link_effect,
          pop(me_spfa_b, "Index")$mean, pop(me_spfa_b, "Comparator")$mean,
          pop(me_rel_b, "Index")$mean, pop(me_rel_b, "Comparator")$mean),
  lo  = c(res_naive$ci_lower, res_stc$ci_lower,
          pop(me_spfa_b, "Index")$q2.5, pop(me_spfa_b, "Comparator")$q2.5,
          pop(me_rel_b, "Index")$q2.5, pop(me_rel_b, "Comparator")$q2.5),
  hi  = c(res_naive$ci_upper, res_stc$ci_upper,
          pop(me_spfa_b, "Index")$q97.5, pop(me_spfa_b, "Comparator")$q97.5,
          pop(me_rel_b, "Index")$q97.5, pop(me_rel_b, "Comparator")$q97.5)
)
mlumr_forest(forest_df, ref_line = 0,
             x = "Log odds ratio",
             title = "PASI 75: ixekizumab vs secukinumab",
             subtitle = "Unadjusted vs population-adjusted, in both target populations")


## ----effects-both-------------------------------------------------------------
marginal_effects(fit_relaxed, effect = "all", population = "both")


## ----posterior, fig.height = 2.6----------------------------------------------
plot(marginal_effects(fit_spfa, effect = "lor"))


## ----predict------------------------------------------------------------------
predict(fit_spfa, population = "both", type = "response")


## ----predict-plot, fig.height = 3---------------------------------------------
plot(predict(fit_spfa, population = "both", type = "response"))


## ----conditional--------------------------------------------------------------
# Weight stays in kilograms in this vignette (IPD mean about 94), unlike
# `vignette("choosing-a-method")`, which divides it by 10. Profiles must be on
# the scale the model was fitted on, or the contrast is evaluated outside the
# covariate support.
profiles <- data.frame(age = c(40, 55, 70), bsa = c(0.15, 0.30, 0.45),
                       weight = c(75, 90, 110), prevsys = c(0, 1, 1))
conditional_effects(fit_spfa, newdata = profiles)


## ----compare------------------------------------------------------------------
compare_models(SPFA = fit_spfa, Relaxed = fit_relaxed, criterion = "loo")
compare_models(SPFA = fit_spfa, Relaxed = fit_relaxed, criterion = "dic")

