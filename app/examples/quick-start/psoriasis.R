# Binary outcome: ixekizumab Q4W (UNCOVER-2, patient data) against
# secukinumab 300 mg (FIXTURE, published aggregate data) on PASI 75 at week 12.
# This follows vignette("binary-outcomes"). Run it line by line with
# Ctrl+Enter, or all at once with Ctrl+Shift+Enter.
library(mlumr)

# Data -----------------------------------------------------------------------
data("psoriasis_ipd")
data("psoriasis_agd")
covs <- c("age", "bsa", "weight", "prevsys")

ipd <- psoriasis_ipd
ipd$bsa <- ipd$bsa / 100
ipd <- ipd[ipd$study == "UNCOVER-2" & ipd$treatment == "IXE_Q4W", ]
ipd <- ipd[stats::complete.cases(ipd[, c("pasi75", covs)]), ]

agd <- psoriasis_agd
agd$bsa_mean <- agd$bsa_mean / 100
agd$bsa_sd <- agd$bsa_sd / 100
agd <- agd[agd$study == "FIXTURE" & agd$treatment == "SEC_300", ]

ipd_obj <- set_ipd(ipd, treatment = "treatment", outcome = "pasi75", covariates = covs)
agd_obj <- set_agd(agd, treatment = "treatment",
                   outcome_n = "pasi75_n", outcome_r = "pasi75_r",
                   cov_means = c("age_mean", "bsa_mean", "weight_mean", "prevsys_prop"),
                   cov_sds = c("age_sd", "bsa_sd", "weight_sd", NA),
                   cov_types = c("continuous", "continuous", "continuous", "binary"))
dat <- combine_data(ipd_obj, agd_obj)

# Integration points -----------------------------------------------------------
dat <- add_integration(dat, n_int = 64,
  age = distr(qgamma, mean = age_mean, sd = age_sd),
  bsa = distr(qlogitnorm, mean = bsa_mean, sd = bsa_sd),
  weight = distr(qgamma, mean = weight_mean, sd = weight_sd),
  prevsys = distr(qbern, prob = prevsys_mean))

# Frequentist benchmarks ---------------------------------------------------------
naive(dat)
stc(dat)

# Bayesian ML-UMR ------------------------------------------------------------------
# The vignette's settings: 4 chains of 2000 iterations. Each chain runs in its
# own browser worker (watch Background Jobs or the status bar).
fit <- mlumr(dat, model = "spfa", link = "logit",
             prior_beta = prior_normal(0, 2.5, autoscale = TRUE),
             chains = 4, iter = 2000, warmup = 1000, seed = 2026)
summary(fit)

# Effects and plots ------------------------------------------------------------------
me <- marginal_effects(fit, effect = "all")
me
plot(me)
plot_prior_posterior(fit, pars = c("mu_index", "mu_comparator"))
