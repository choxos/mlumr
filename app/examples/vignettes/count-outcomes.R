# Code from vignette("count-outcomes"): Count outcomes: an unanchored rate-ratio comparison
# mlumr GitHub main at 95c5bbd, extracted with knitr::purl(). Tables print as
# plain R output and the vignette's knitr setup chunk is left out; otherwise
# the code is the vignette's, unchanged. Chunks the vignette shows but does
# not run are commented out.

## ----packages, message = FALSE------------------------------------------------
library(mlumr)
library(ggplot2)
options(mc.cores = parallel::detectCores())


## ----data---------------------------------------------------------------------
data("caries_ipd")     # index IPD (SDF), bundled with mlumr
data("caries_agd")     # comparator AgD (NSF)

covariates <- c("age", "log_cfu")   # age carries the prognostic signal here

data.frame(
  Arm = c("Index (SDF, IPD)", "Comparator (NSF, AgD)"),
  Children = c(nrow(caries_ipd), caries_agd$n),
  Total_dmft = c(sum(caries_ipd$dmft), caries_agd$r),
  Mean_dmft = c(mean(caries_ipd$dmft), caries_agd$r / caries_agd$E)
)


## ----data-head----------------------------------------------------------------
head(caries_ipd)


## ----balance------------------------------------------------------------------
balance <- data.frame(
  Covariate     = c("Age (years)", "Baseline bacterial load (log CFU)"),
  Index_SDF     = c(mean(caries_ipd$age), mean(caries_ipd$log_cfu)),
  Comparator_NSF = c(caries_agd$age_mean, caries_agd$log_cfu_mean)
)
balance


## ----setup-data---------------------------------------------------------------
ipd <- set_ipd(caries_ipd, treatment = "treatment", outcome = "dmft",
               family = "poisson", exposure = "exposure",
               covariates = covariates)
agd <- set_agd(caries_agd, treatment = "treatment", family = "poisson",
               outcome_r = "r", outcome_E = "E",
               cov_means = c("age_mean", "log_cfu_mean"),
               cov_sds   = c("age_sd", "log_cfu_sd"),
               cov_types = c("continuous", "continuous"))
dat <- combine_data(ipd, agd)
dat


## ----integration--------------------------------------------------------------
dat <- add_integration(dat, n_int = 64,
                       age     = distr(qnorm, mean = age_mean, sd = age_sd),
                       log_cfu = distr(qnorm, mean = log_cfu_mean, sd = log_cfu_sd))


## ----check-int----------------------------------------------------------------
check_integration(
  dat,
  age     = distr(qnorm, mean = age_mean, sd = age_sd),
  log_cfu = distr(qnorm, mean = log_cfu_mean, sd = log_cfu_sd)
)


## ----benchmarks---------------------------------------------------------------
res_naive <- naive(dat)
res_stc   <- stc(dat)
res_naive
res_stc


## ----fit----------------------------------------------------------------------
fit_spfa <- mlumr(dat, model = "spfa", link = "log",
                  prior_beta = prior_normal(0, 1, autoscale = TRUE),
                  chains = 4, iter = 2000, warmup = 1000, seed = 2026, refresh = 0)
fit_relaxed <- mlumr(dat, model = "relaxed", link = "log",
                     prior_beta = prior_normal(0, 0.5, autoscale = TRUE),
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
rr_spfa <- marginal_effects(fit_spfa, population = "both")
rr_rel  <- marginal_effects(fit_relaxed, population = "both")
# One row per (model, population); `pop()` pulls the requested one.
pop <- function(d, which) d[d$population == which, ]
forest_df <- data.frame(
  label = c("Naive (unstandardized)", "STC (comparator)",
            "ML-UMR SPFA (index)", "ML-UMR SPFA (comparator)",
            "ML-UMR relaxed (index)", "ML-UMR relaxed (comparator)"),
  est = c(exp(res_naive$estimate), exp(res_stc$estimate),
          pop(rr_spfa, "Index")$mean, pop(rr_spfa, "Comparator")$mean,
          pop(rr_rel, "Index")$mean, pop(rr_rel, "Comparator")$mean),
  lo  = c(exp(res_naive$ci_lower), exp(res_stc$ci_lower),
          pop(rr_spfa, "Index")$q2.5, pop(rr_spfa, "Comparator")$q2.5,
          pop(rr_rel, "Index")$q2.5, pop(rr_rel, "Comparator")$q2.5),
  hi  = c(exp(res_naive$ci_upper), exp(res_stc$ci_upper),
          pop(rr_spfa, "Index")$q97.5, pop(rr_spfa, "Comparator")$q97.5,
          pop(rr_rel, "Index")$q97.5, pop(rr_rel, "Comparator")$q97.5)
)
mlumr_forest(forest_df, ref_line = 1, log_x = TRUE,
             x = "Rate ratio (SDF vs NSF)",
             title = "dmft count rate ratio",
             subtitle = "Unadjusted vs population-adjusted, in both target populations")


## ----effects-both-------------------------------------------------------------
rbind(cbind(Model = "SPFA", rr_spfa),
                   cbind(Model = "Relaxed", rr_rel))


## ----posterior-areas, fig.height = 2.6----------------------------------------
plot(marginal_effects(fit_spfa))


## ----predict------------------------------------------------------------------
predict(fit_spfa, population = "both", type = "response")


## ----predict-plot, fig.height = 3---------------------------------------------
plot(predict(fit_spfa, population = "both", type = "response"))


## ----conditional--------------------------------------------------------------
profiles <- data.frame(age = c(4, 5, 6), log_cfu = c(9, 11, 13))
conditional_effects(fit_spfa, newdata = profiles)


## ----compare------------------------------------------------------------------
compare_models(SPFA = fit_spfa, Relaxed = fit_relaxed, criterion = "loo")
compare_models(SPFA = fit_spfa, Relaxed = fit_relaxed, criterion = "dic")

