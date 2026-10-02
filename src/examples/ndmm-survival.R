# Survival outcome: lenalidomide (McCarthy2012, patient data) against
# thalidomide (Morgan2012, reconstructed from the published Kaplan-Meier
# curve) on progression-free survival in newly diagnosed multiple myeloma.
# This follows vignette("survival-outcomes") with a Weibull baseline and the
# browser app's settings: 32 integration points, 4 chains of 300 warmup and
# 300 sampling iterations. Survival models are heavy; the chains run in
# parallel workers, and Background Jobs shows each chain's progress.
library(mlumr)

# Data -----------------------------------------------------------------------
data("ndmm_ipd")
data("ndmm_agd")
data("ndmm_agd_covs")
covs <- c("age", "iss_stage3", "response_cr_vgpr", "male")

ipd_one <- ndmm_ipd[ndmm_ipd$study == "McCarthy2012" & ndmm_ipd$treatment == "Len", ]
agd_one <- ndmm_agd[ndmm_agd$study == "Morgan2012" & ndmm_agd$treatment == "Thal", ]
cv <- ndmm_agd_covs[ndmm_agd_covs$study == "Morgan2012" & ndmm_agd_covs$treatment == "Thal", ]
agd_one$age_mean <- cv$age_mean
agd_one$age_sd <- cv$age_sd
agd_one$iss_stage3_prop <- cv$iss_stage3_prop
agd_one$response_cr_vgpr_prop <- cv$response_cr_vgpr_prop
agd_one$male_prop <- cv$male_prop

ipd <- set_ipd(ipd_one, treatment = "treatment", covariates = covs,
               family = "survival", time = "eventtime", status = "status")
agd <- set_agd_surv(agd_one, treatment = "treatment",
                    time = "eventtime", status = "status",
                    cov_means = c("age_mean", "iss_stage3_prop", "response_cr_vgpr_prop", "male_prop"),
                    cov_sds = c("age_sd", NA, NA, NA),
                    cov_types = c("continuous", "binary", "binary", "binary"))
dat <- combine_data(ipd, agd)

# Integration points -----------------------------------------------------------
dat <- add_integration(dat, n_int = 32,
  age = distr(qgamma, mean = age_mean, sd = age_sd),
  iss_stage3 = distr(qbern, prob = iss_stage3_mean),
  response_cr_vgpr = distr(qbern, prob = response_cr_vgpr_mean),
  male = distr(qbern, prob = male_mean))

# Restricted mean survival time is compared up to the shorter follow-up.
tau_rmst <- min(max(ipd_one$eventtime), max(agd_one$eventtime))

# Benchmark: unadjusted Cox model --------------------------------------------------
naive(dat)

# Bayesian ML-UMR, Weibull proportional hazards ------------------------------------
fit_weibull <- mlumr(dat, model = "spfa", distribution = "weibull",
                     rmst_horizon = tau_rmst,
                     chains = 4, iter = 600, warmup = 300, seed = 2026)
summary(fit_weibull)

# Effects and plots --------------------------------------------------------------------
hr <- marginal_effects(fit_weibull, effect = "hr")
hr
marginal_effects(fit_weibull, effect = "rmstd")
plot(predict(fit_weibull, type = "survival"))
