# Code from vignette("fitting-and-diagnostics"): Fitting, priors, and diagnostics
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
ipd$bsa <- ipd$bsa / 100          # body-surface area: % -> proportion
ipd <- ipd[ipd$study == "UNCOVER-2" & ipd$treatment == "IXE_Q4W", ]
ipd <- ipd[stats::complete.cases(ipd[, c("pasi75", covs)]), ]

agd <- psoriasis_agd
agd$bsa_mean <- agd$bsa_mean / 100; agd$bsa_sd <- agd$bsa_sd / 100
agd <- agd[agd$study == "FIXTURE" & agd$treatment == "SEC_300", ]

dat <- combine_data(
  set_ipd(ipd, treatment = "treatment", outcome = "pasi75", covariates = covs),
  set_agd(agd, treatment = "treatment", outcome_n = "pasi75_n", outcome_r = "pasi75_r",
          cov_means = c("age_mean", "bsa_mean", "weight_mean"),
          cov_sds   = c("age_sd", "bsa_sd", "weight_sd"),
          cov_types = c("continuous", "continuous", "continuous")))
dat <- add_integration(dat, n_int = 64,
  age    = distr(qgamma,     mean = age_mean,    sd = age_sd),
  bsa    = distr(qlogitnorm, mean = bsa_mean,    sd = bsa_sd),
  weight = distr(qgamma,     mean = weight_mean, sd = weight_sd))

fit <- mlumr(dat, model = "spfa",
             prior_beta = prior_normal(0, 2.5, autoscale = TRUE),
             chains = 4, iter = 2000, warmup = 1000, seed = 2026, refresh = 0)


## ----sampler, eval = FALSE----------------------------------------------------
# fit <- mlumr(
#   dat, model = "spfa",
#   chains = 4,            # number of MCMC chains
#   iter = 4000,           # total iterations per chain
#   warmup = 2000,         # warmup iterations
#   seed = 2026,           # reproducibility
#   adapt_delta = 0.99,    # raise toward 1 to remove divergences
#   max_treedepth = 15,    # raise if treedepth is saturated
#   refresh = 500          # progress printing (0 = silent)
# )


## ----backend, eval = FALSE----------------------------------------------------
# fit_cmd <- mlumr(dat, model = "spfa", engine = "cmdstanr", seed = 2026, refresh = 0)


## ----priors-------------------------------------------------------------------
prior_summary(fit)


## ----prior-post, fig.height = 2.6---------------------------------------------
plot_prior_posterior(fit, pars = c("mu_index", "mu_comparator"))


## ----inspect------------------------------------------------------------------
print(fit)
summary(fit)


## ----effects------------------------------------------------------------------
marginal_effects(fit, effect = "lor")


## ----predict-both-------------------------------------------------------------
predict(fit, type = "response")


## ----diagnostics--------------------------------------------------------------
fit$diagnostics$n_divergent
fit$diagnostics$n_max_treedepth
max(fit$summary$Rhat, na.rm = TRUE)
min(fit$summary$n_eff, na.rm = TRUE)


## ----intervals, fig.height = 2.4----------------------------------------------
plot(marginal_effects(fit, effect = "lor"))


## ----trace, fig.height = 3----------------------------------------------------
pars <- c("lor_index", "lor_comparator")
chain <- fit$chain_ids
# NULL means the backend could not label the draws, which is itself worth
# knowing: without labels there is no chain comparison to draw.
stopifnot(!is.null(chain))

# `split()` keeps each chain's draws in iteration order, which is what a trace
# needs. Indexing by a chain's own label rather than by arithmetic on the row
# count avoids assuming the chains are equal length or stored in order.
rows_by_chain <- split(seq_along(chain), chain)
n_iter <- unique(lengths(rows_by_chain))
stopifnot(length(n_iter) == 1L)

draws_array <- array(
  NA_real_,
  dim = c(n_iter, length(rows_by_chain), length(pars)),
  dimnames = list(NULL, paste("chain", names(rows_by_chain)), pars)
)
for (k in seq_along(rows_by_chain)) {
  draws_array[, k, ] <- as.matrix(fit$draws[rows_by_chain[[k]], pars])
}

bayesplot::mcmc_trace(draws_array) +
  ggplot2::labs(title = "Trace plots")


## ----sensitivity--------------------------------------------------------------
prior_sensitivity(fit, prior_beta_scales = c(1, 2.5, 5))

