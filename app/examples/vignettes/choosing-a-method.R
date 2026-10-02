# Code from vignette("choosing-a-method"): Choosing a method for single-arm indirect comparisons: ML-UMR, STC, and naive
# mlumr GitHub main at 95c5bbd, extracted with knitr::purl(). Tables print as
# plain R output and the vignette's knitr setup chunk is left out; otherwise
# the code is the vignette's, unchanged. Chunks the vignette shows but does
# not run are commented out.

## ----packages, message = FALSE------------------------------------------------
library(mlumr)
library(ggplot2)
options(mc.cores = parallel::detectCores())


## ----data---------------------------------------------------------------------
data("psoriasis_ipd")   # bundled with mlumr (from multinma, GPL-3)
data("psoriasis_agd")

covs <- c("age", "bsa", "weight")
ipd <- psoriasis_ipd
ipd$bsa <- ipd$bsa / 100; ipd$weight <- ipd$weight / 10
ipd <- ipd[ipd$study == "UNCOVER-2" & ipd$treatment == "IXE_Q4W", ]
ipd <- ipd[stats::complete.cases(ipd[, c("pasi75", covs)]), ]

agd <- psoriasis_agd
agd$bsa_mean <- agd$bsa_mean / 100; agd$bsa_sd <- agd$bsa_sd / 100
agd$weight_mean <- agd$weight_mean / 10; agd$weight_sd <- agd$weight_sd / 10
agd <- agd[agd$study == "FIXTURE" & agd$treatment == "SEC_300", ]

dat <- combine_data(
  set_ipd(ipd, treatment = "treatment", outcome = "pasi75", covariates = covs),
  set_agd(agd, treatment = "treatment", outcome_n = "pasi75_n", outcome_r = "pasi75_r",
          cov_means = c("age_mean", "bsa_mean", "weight_mean"),
          cov_sds   = c("age_sd", "bsa_sd", "weight_sd"),
          cov_types = c("continuous", "continuous", "continuous")))
# `bsa` was rescaled to a proportion above, so its marginal has to respect
# (0, 1). A normal marginal puts integration points outside that range;
# `vignette("binary-outcomes")` uses the logit-normal for the same reason.
dat <- add_integration(dat, n_int = 64,
  age    = distr(qnorm,      mean = age_mean, sd = age_sd),
  bsa    = distr(qlogitnorm, mean = bsa_mean, sd = bsa_sd),
  weight = distr(qgamma, shape = weight_mean^2 / weight_sd^2,
                 rate = weight_mean / weight_sd^2))


## ----freq---------------------------------------------------------------------
res_naive <- naive(dat)
res_stc   <- stc(dat)
res_naive
res_stc


## ----bayes--------------------------------------------------------------------
fit_spfa <- mlumr(dat, model = "spfa",
                  prior_beta = prior_normal(0, 2.5, autoscale = TRUE),
                  chains = 4, iter = 2000, warmup = 1000, seed = 2026, refresh = 0)
fit_relaxed <- mlumr(dat, model = "relaxed",
                     prior_beta = prior_normal(0, 2.5, autoscale = TRUE),
                     chains = 4, iter = 2000, warmup = 1000, seed = 2026, refresh = 0)


## ----comparison---------------------------------------------------------------
me_spfa <- marginal_effects(fit_spfa, effect = "lor", population = "both")
me_rel  <- marginal_effects(fit_relaxed, effect = "lor", population = "both")
# One row per (model, population); `pop()` pulls the requested one.
pop <- function(d, which) d[d$population == which, ]
comparison <- data.frame(
  Method = c("Naive", "STC", "ML-UMR SPFA", "ML-UMR SPFA",
             "ML-UMR relaxed", "ML-UMR relaxed"),
  Population = c("unstandardized", "comparator", "index", "comparator",
                 "index", "comparator"),
  LOR      = c(res_naive$link_effect, res_stc$link_effect,
               pop(me_spfa, "Index")$mean, pop(me_spfa, "Comparator")$mean,
               pop(me_rel, "Index")$mean, pop(me_rel, "Comparator")$mean),
  CI_lower = c(res_naive$ci_lower, res_stc$ci_lower,
               pop(me_spfa, "Index")$q2.5, pop(me_spfa, "Comparator")$q2.5,
               pop(me_rel, "Index")$q2.5, pop(me_rel, "Comparator")$q2.5),
  CI_upper = c(res_naive$ci_upper, res_stc$ci_upper,
               pop(me_spfa, "Index")$q97.5, pop(me_spfa, "Comparator")$q97.5,
               pop(me_rel, "Index")$q97.5, pop(me_rel, "Comparator")$q97.5)
)
comparison


## ----forest, fig.height = 3.6-------------------------------------------------
forest_df <- with(comparison,
  data.frame(label = paste0(Method, " (", Population, ")"),
             est = LOR, lo = CI_lower, hi = CI_upper))
mlumr_forest(forest_df, ref_line = 0,
             x = "Log odds ratio",
             title = "Methods compared: ixekizumab vs secukinumab",
             subtitle = "ML-UMR shown in both target populations")


## ----posterior-both, fig.height = 2.6-----------------------------------------
plot(marginal_effects(fit_spfa, effect = "lor"))


## ----compare------------------------------------------------------------------
compare_models(SPFA = fit_spfa, Relaxed = fit_relaxed, criterion = "loo")
compare_models(SPFA = fit_spfa, Relaxed = fit_relaxed)   # DIC-based (the default)

