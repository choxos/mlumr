# Code from vignette("continuous-outcomes"): Continuous outcomes: an unanchored mean-difference comparison
# mlumr GitHub main at 95c5bbd, extracted with knitr::purl(). Tables print as
# plain R output and the vignette's knitr setup chunk is left out; otherwise
# the code is the vignette's, unchanged. Chunks the vignette shows but does
# not run are commented out.

## ----packages, message = FALSE------------------------------------------------
library(mlumr)
library(ggplot2)
options(mc.cores = parallel::detectCores())


## ----data---------------------------------------------------------------------
data("shoulder_ipd")     # index IPD (ASD), bundled with mlumr
data("shoulder_agd")     # comparator AgD (ET)

covariates <- c("age", "baseline_vas")   # baseline pain is the key prognostic


## ----data-head----------------------------------------------------------------
head(shoulder_ipd)
shoulder_agd[, c("treatment", "n", "y_mean", "y_se",
                              "age_mean", "baseline_vas_mean")]


## ----balance------------------------------------------------------------------
balance <- data.frame(
  Covariate = c("Pain VAS on activity (24m)", "Age (years)", "Baseline pain VAS"),
  Index_ASD = c(mean(shoulder_ipd$pain_vas_activity), mean(shoulder_ipd$age),
                mean(shoulder_ipd$baseline_vas)),
  Comparator_ET = c(shoulder_agd$y_mean, shoulder_agd$age_mean,
                    shoulder_agd$baseline_vas_mean)
)
balance


## ----setup-data---------------------------------------------------------------
ipd <- set_ipd(shoulder_ipd, treatment = "treatment", outcome = "pain_vas_activity",
               family = "normal", covariates = covariates)
agd <- set_agd(shoulder_agd, treatment = "treatment", family = "normal",
               outcome_n = "n", outcome_mean = "y_mean", outcome_se = "y_se",
               cov_means = c("age_mean", "baseline_vas_mean"),
               cov_sds   = c("age_sd", "baseline_vas_sd"),
               cov_types = c("continuous", "continuous"))
dat <- combine_data(ipd, agd)
dat


## ----integration--------------------------------------------------------------
dat <- add_integration(dat, n_int = 64,
                       age          = distr(qnorm, mean = age_mean, sd = age_sd),
                       baseline_vas = distr(qnorm, mean = baseline_vas_mean,
                                            sd = baseline_vas_sd))


## ----check-int----------------------------------------------------------------
check_integration(
  dat,
  age          = distr(qnorm, mean = age_mean, sd = age_sd),
  baseline_vas = distr(qnorm, mean = baseline_vas_mean, sd = baseline_vas_sd)
)


## ----benchmarks---------------------------------------------------------------
res_naive <- naive(dat)
res_stc   <- stc(dat)
res_naive
res_stc


## ----fit----------------------------------------------------------------------
fit_spfa <- mlumr(dat, model = "spfa", link = "identity",
                  prior_beta = prior_normal(0, 1, autoscale = TRUE),
                  chains = 4, iter = 2000, warmup = 1000, seed = 2026, refresh = 0)
fit_relaxed <- mlumr(dat, model = "relaxed", link = "identity",
                     prior_beta = prior_normal(0, 0.75, autoscale = TRUE),
                     chains = 4, iter = 2000, warmup = 1000,
                     adapt_delta = 0.95, seed = 2026, refresh = 0)
summary(fit_spfa)


## ----priors-------------------------------------------------------------------
prior_summary(fit_spfa)


## ----prior-post, fig.height = 2.6---------------------------------------------
plot_prior_posterior(fit_spfa, pars = c("mu_index", "mu_comparator"))


## ----diagnostics--------------------------------------------------------------
data.frame(
  n_divergent  = fit_spfa$diagnostics$n_divergent,
  max_treedepth = fit_spfa$diagnostics$n_max_treedepth,
  max_Rhat     = round(max(fit_spfa$summary$Rhat, na.rm = TRUE), 3),
  min_ESS      = round(min(fit_spfa$summary$n_eff, na.rm = TRUE))
)


## ----effects------------------------------------------------------------------
marginal_effects(fit_spfa)


## ----forest, fig.height = 3.6-------------------------------------------------
md_spfa <- marginal_effects(fit_spfa, population = "both")
md_rel  <- marginal_effects(fit_relaxed, population = "both")
# One row per (model, population); `pop()` pulls the requested one.
pop <- function(d, which) d[d$population == which, ]
forest_df <- data.frame(
  label = c("Naive (unstandardized)", "STC (comparator)",
            "ML-UMR SPFA (index)", "ML-UMR SPFA (comparator)",
            "ML-UMR relaxed (index)", "ML-UMR relaxed (comparator)"),
  est = c(res_naive$estimate, res_stc$estimate,
          pop(md_spfa, "Index")$mean, pop(md_spfa, "Comparator")$mean,
          pop(md_rel, "Index")$mean, pop(md_rel, "Comparator")$mean),
  lo  = c(res_naive$ci_lower, res_stc$ci_lower,
          pop(md_spfa, "Index")$q2.5, pop(md_spfa, "Comparator")$q2.5,
          pop(md_rel, "Index")$q2.5, pop(md_rel, "Comparator")$q2.5),
  hi  = c(res_naive$ci_upper, res_stc$ci_upper,
          pop(md_spfa, "Index")$q97.5, pop(md_spfa, "Comparator")$q97.5,
          pop(md_rel, "Index")$q97.5, pop(md_rel, "Comparator")$q97.5)
)
mlumr_forest(forest_df, ref_line = 0,
             x = "Mean difference in pain VAS on activity",
             title = "Shoulder pain on activity: ASD vs ET",
             subtitle = "Unadjusted vs population-adjusted, in both target populations")


## ----effects-both-------------------------------------------------------------
rbind(cbind(Model = "SPFA", md_spfa),
                   cbind(Model = "Relaxed", md_rel))


## ----posterior-areas, fig.height = 2.6----------------------------------------
plot(marginal_effects(fit_spfa))


## ----predict------------------------------------------------------------------
predict(fit_spfa, population = "both", type = "response")


## ----predict-plot, fig.height = 3---------------------------------------------
plot(predict(fit_spfa, population = "both", type = "response"))


## ----conditional--------------------------------------------------------------
profiles <- data.frame(age = c(45, 55, 65), baseline_vas = c(50, 70, 90))
conditional_effects(fit_spfa, newdata = profiles)


## ----compare------------------------------------------------------------------
compare_models(SPFA = fit_spfa, Relaxed = fit_relaxed, criterion = "loo")
compare_models(SPFA = fit_spfa, Relaxed = fit_relaxed, criterion = "dic")

