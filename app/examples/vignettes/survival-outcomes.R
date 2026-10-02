# Code from vignette("survival-outcomes"): Survival (time-to-event) outcomes
# mlumr GitHub main at 95c5bbd, extracted with knitr::purl(). Tables print as
# plain R output and the vignette's knitr setup chunk is left out; otherwise
# the code is the vignette's, unchanged. Chunks the vignette shows but does
# not run are commented out.

## ----packages, message = FALSE------------------------------------------------
library(mlumr)
library(ggplot2)
options(mc.cores = parallel::detectCores())


## ----censoring, eval = FALSE--------------------------------------------------
# library(survival)
# Surv(time, status)                      # right censoring (status 1 = event)
# Surv(entry_time, time, status)          # delayed entry / left truncation
# Surv(lower, upper, type = "interval2")  # interval censoring


## ----data---------------------------------------------------------------------
data("ndmm_ipd")        # bundled with mlumr (copied from multinma, GPL-3)
data("ndmm_agd")        # reconstructed pseudo-IPD (KM)
data("ndmm_agd_covs")   # covariate moments for the AgD arm

covs <- c("age", "iss_stage3", "response_cr_vgpr", "male")

# Index IPD: McCarthy2012, lenalidomide. Age stays in years, as in multinma's
# own NDMM example, so the coefficient reads as a per-year log hazard ratio.
ipd_one <- ndmm_ipd[ndmm_ipd$study == "McCarthy2012" & ndmm_ipd$treatment == "Len", ]

# Comparator AgD: Morgan2012, thalidomide (pseudo-IPD + covariate moments).
agd_one <- ndmm_agd[ndmm_agd$study == "Morgan2012" & ndmm_agd$treatment == "Thal", ]
cv <- ndmm_agd_covs[ndmm_agd_covs$study == "Morgan2012" & ndmm_agd_covs$treatment == "Thal", ]
agd_one$age_mean         <- cv$age_mean
agd_one$age_sd           <- cv$age_sd
agd_one$iss_stage3_prop       <- cv$iss_stage3_prop        # proportions (binary covariates)
agd_one$response_cr_vgpr_prop <- cv$response_cr_vgpr_prop
agd_one$male_prop             <- cv$male_prop


## ----balance------------------------------------------------------------------
balance <- data.frame(
  Covariate = c("Age (years)", "ISS stage III (prop.)",
                "CR/VGPR response (prop.)", "Male (prop.)"),
  Index_Len = c(mean(ipd_one$age), mean(ipd_one$iss_stage3),
                mean(ipd_one$response_cr_vgpr), mean(ipd_one$male)),
  Comparator_Thal = c(agd_one$age_mean[1], agd_one$iss_stage3_prop[1],
                      agd_one$response_cr_vgpr_prop[1], agd_one$male_prop[1])
)
balance


## ----km, fig.height = 4.9-----------------------------------------------------
km_obs <- rbind(
  data.frame(eventtime = ipd_one$eventtime, status = ipd_one$status,
             arm = "Lenalidomide"),
  data.frame(eventtime = agd_one$eventtime, status = agd_one$status,
             arm = "Thalidomide")
)
# The displayed observed curves use ggsurvfit: step lines with censoring marks
# and a numbers-at-risk table, the standard survival-reporting layout. (The
# predicted-vs-observed overlay further down draws its KM with mlumr's geom_km().)
arm_cols <- c("Lenalidomide" = "#00BA38",   # green (index)
              "Thalidomide" = "#F8766D")    # red (comparator)
ggsurvfit::survfit2(survival::Surv(eventtime, status) ~ arm, data = km_obs) |>
  ggsurvfit::ggsurvfit(linewidth = 0.7) +
  ggsurvfit::add_censor_mark(shape = 3, size = 1.6, alpha = 0.7) +
  ggsurvfit::add_risktable(risktable_stats = "n.risk", size = 3.2) +
  scale_color_manual(values = arm_cols) +
  scale_fill_manual(values = arm_cols) +
  scale_x_continuous(breaks = seq(0, 60, 10)) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),                       # gridline at every 0.1
    labels = function(b) ifelse(round(b * 10) %% 2 == 0,  # label 0, 0.2, .., 1
                                sprintf("%.1f", b), ""),
    limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Progression-free survival (months)", y = "Survival probability",
       title = "Observed Kaplan-Meier curves") +
  theme_minimal(base_size = 11) + theme(legend.position = "bottom")


## ----setup-data---------------------------------------------------------------
ipd <- set_ipd(ipd_one, treatment = "treatment", covariates = covs,
               family = "survival", time = "eventtime", status = "status")

agd <- set_agd_surv(agd_one, treatment = "treatment",
                    time = "eventtime", status = "status",
                    cov_means = c("age_mean", "iss_stage3_prop", "response_cr_vgpr_prop", "male_prop"),
                    cov_sds   = c("age_sd", NA, NA, NA),
                    cov_types = c("continuous", "binary", "binary", "binary"))

dat <- suppressWarnings(combine_data(ipd, agd))
dat <- suppressWarnings(add_integration(
  dat, n_int = 64,
  # Age is right-skewed, so a gamma marginal, matching multinma's NDMM example.
  age              = distr(qgamma, mean = age_mean, sd = age_sd),
  iss_stage3       = distr(qbern, prob = iss_stage3_mean),
  response_cr_vgpr = distr(qbern, prob = response_cr_vgpr_mean),
  male             = distr(qbern, prob = male_mean)))
dat


## ----horizon------------------------------------------------------------------
# The follow-up both arms in THIS analysis actually observed, taken from the two
# subsets that were combined above rather than from the full bundled datasets.
# Every RMST estimate in this vignette is integrated to this time, so the forest
# plot compares like with like.
tau_rmst <- min(max(ipd_one$eventtime), max(agd_one$eventtime))
tau_rmst


## ----benchmarks---------------------------------------------------------------
res_naive <- naive(dat)                            # unadjusted Cox log HR
res_stc   <- stc(dat, distribution = "weibull", seed = 2026,
                 rmst_horizon = tau_rmst)          # G-computation RMST diff
res_naive
res_stc


## ----fit-mspline--------------------------------------------------------------
# Primary: cubic M-spline baseline (the multinma NDMM survival model)
fit_mspline <- mlumr(dat, model = "spfa", distribution = "mspline", n_knots = 7,
                     rmst_horizon = tau_rmst,
                     chains = 4, iter = 2000, warmup = 1000, seed = 2026, refresh = 0)


## ----fit-mspline-relaxed------------------------------------------------------
# Relaxed M-spline: treatment-specific prognostic effects (effect modification),
# the unanchored analogue of multinma's ~ (covariates) * .trt. beta_comparator is
# identified only by the reconstructed comparator likelihood, so we regularize it
# with a tighter autoscaled prior (see the caveat at the end of this vignette).
fit_mspline_relaxed <- mlumr(dat, model = "relaxed", distribution = "mspline",
                             n_knots = 7, rmst_horizon = tau_rmst,
                             prior_beta_comparator = prior_normal(0, 1, autoscale = TRUE),
                             chains = 4, iter = 2000, warmup = 1000, seed = 2026,
                             refresh = 0)


## ----fit-weibull--------------------------------------------------------------
# Parametric comparison: Weibull proportional hazards
fit_weibull <- mlumr(dat, model = "spfa", distribution = "weibull",
                     rmst_horizon = tau_rmst,
                     chains = 4, iter = 2000, warmup = 1000, seed = 2026, refresh = 0)


## ----fit-summary--------------------------------------------------------------
summary(fit_mspline)


## ----priors-------------------------------------------------------------------
prior_summary(fit_mspline)


## ----prior-post, fig.height = 2.6---------------------------------------------
plot_prior_posterior(fit_mspline, pars = c("mu_index", "mu_comparator"))


## ----diagnostics--------------------------------------------------------------
data.frame(
  n_divergent  = fit_mspline$diagnostics$n_divergent,
  max_treedepth = fit_mspline$diagnostics$n_max_treedepth,
  max_Rhat     = round(max(fit_mspline$summary$Rhat, na.rm = TRUE), 3),
  min_ESS      = round(min(fit_mspline$summary$n_eff, na.rm = TRUE))
)


## ----effects------------------------------------------------------------------
marginal_effects(fit_mspline, effect = "all")


## ----loghr, fig.height = 3.5--------------------------------------------------
plot(predict(fit_mspline, type = "loghr"))


## ----forest, fig.height = 4.2-------------------------------------------------
# Every ML-UMR fit, in both target populations, on the collapsible RMST scale.
# Each row must be integrated to the SAME restriction time or the plot compares
# different estimands, so check that before drawing rather than assuming it.
rd <- function(fit) marginal_effects(fit, effect = "rmstd", population = "both")
pop <- function(d, which) d[d$population == which, ]
rd_ms  <- rd(fit_mspline)
rd_rel <- rd(fit_mspline_relaxed)
rd_wb  <- rd(fit_weibull)
stopifnot(all.equal(unique(c(rd_ms$horizon, rd_rel$horizon, rd_wb$horizon,
                             res_stc$horizon)), tau_rmst))
row <- function(d, which) pop(d, which)[, c("mean", "q2.5", "q97.5")]
bayes <- rbind(row(rd_ms, "Index"),  row(rd_ms, "Comparator"),
               row(rd_rel, "Index"), row(rd_rel, "Comparator"),
               row(rd_wb, "Index"),  row(rd_wb, "Comparator"))
forest_df <- data.frame(
  label = c("STC (flexsurv, comparator pop.)",
            "ML-UMR M-spline SPFA (index pop.)",
            "ML-UMR M-spline SPFA (comparator pop.)",
            "ML-UMR M-spline relaxed (index pop.)",
            "ML-UMR M-spline relaxed (comparator pop.)",
            "ML-UMR Weibull SPFA (index pop.)",
            "ML-UMR Weibull SPFA (comparator pop.)"),
  est = c(res_stc$rmst_diff, bayes$mean),
  lo  = c(res_stc$ci_lower,  bayes$q2.5),
  hi  = c(res_stc$ci_upper,  bayes$q97.5)
)
mlumr_forest(forest_df, ref_line = 0,
             x = "RMST difference (months)",
             title = "Restricted mean survival time difference",
             subtitle = sprintf(paste0("Every ML-UMR fit and the STC benchmark,",
                                       " integrated to tau = %.4g months"),
                                tau_rmst))


## ----rmst-median--------------------------------------------------------------
predict(fit_mspline, type = "rmst")
predict(fit_mspline, type = "median")


## ----surv-curves, fig.height = 4----------------------------------------------
surv_cols <- c(Len = "#00BA38", Thal = "#F8766D")   # green index, red comparator
plot(predict(fit_mspline, type = "survival")) +
  scale_color_manual(values = surv_cols, aesthetics = c("colour", "fill"))


## ----surv-curves-km, fig.height = 3.4-----------------------------------------
plot(predict(fit_mspline, population = "index", type = "survival")) +
  geom_km(dat, dat$index_treatment, marks = FALSE) +
  scale_color_manual(values = surv_cols, aesthetics = c("colour", "fill")) +
  ggplot2::labs(subtitle = "Index population, with the observed Len KM")

plot(predict(fit_mspline, population = "comparator", type = "survival")) +
  geom_km(dat, dat$comparator_treatment, marks = FALSE) +
  scale_color_manual(values = surv_cols, aesthetics = c("colour", "fill")) +
  ggplot2::labs(subtitle = "Comparator population, with the observed Thal KM")


## ----hazard-curve, fig.height = 4---------------------------------------------
plot(predict(fit_mspline, type = "hazard")) +
  scale_color_manual(values = surv_cols, aesthetics = c("colour", "fill"))


## ----relaxed------------------------------------------------------------------
marginal_effects(fit_mspline_relaxed, effect = "rmstd")


## ----compare------------------------------------------------------------------
compare_models(Weibull = fit_weibull, MSpline = fit_mspline, criterion = "loo")


## ----weibull-effects----------------------------------------------------------
marginal_effects(fit_weibull, effect = "all")


## ----weibull-surv, fig.height = 4---------------------------------------------
plot(predict(fit_weibull, type = "survival")) +
  scale_color_manual(values = surv_cols, aesthetics = c("colour", "fill"))


## ----anchor-constancy---------------------------------------------------------
data("ndmm_ipd", package = "multinma")
data("ndmm_agd", package = "multinma")
data("ndmm_agd_covs", package = "multinma")

tau <- res_stc$horizon   # the RMST horizon used above
mcc_pbo <- ndmm_ipd[ndmm_ipd$studyf == "McCarthy2012" & ndmm_ipd$trtf == "Pbo", ]
mor_pbo <- ndmm_agd[ndmm_agd$studyf == "Morgan2012" & ndmm_agd$trtf == "Pbo", ]
mor_cov <- ndmm_agd_covs[ndmm_agd_covs$studyf == "Morgan2012" &
                           ndmm_agd_covs$trtf == "Pbo", ]

# Conditional constancy is the assumption ML-UMR cannot test from the unanchored
# data alone. With the discarded arms it becomes testable: fit the survival model
# in the arm both trials share, transport it to the other trial's population, and
# compare against what that trial actually observed for the same arm.
set.seed(2026)
m_pbo <- flexsurv::flexsurvreg(
  survival::Surv(eventtime, status) ~ age + iss_stage3 + response_cr_vgpr + male,
  data = mcc_pbo, dist = "weibullPH")
n <- 5e4
nd <- data.frame(age = rnorm(n, mor_cov$age_mean, mor_cov$age_sd),
                 iss_stage3 = rbinom(n, 1, mor_cov$iss_stage3),
                 response_cr_vgpr = rbinom(n, 1, mor_cov$response_cr_vgpr),
                 male = rbinom(n, 1, mor_cov$male))
km_rmst <- function(d) unname(summary(
  survival::survfit(survival::Surv(eventtime, status) ~ 1, data = d),
  rmean = tau)$table["rmean"])

# Name these so the prose below cites the same numbers the table prints,
# rather than literals that drift the next time the vignette is re-knitted.
rmst_own       <- km_rmst(mcc_pbo)
rmst_transport <- mean(summary(m_pbo, newdata = nd, type = "rmst",
                               t = tau, ci = FALSE, tidy = TRUE)$est)
rmst_observed  <- km_rmst(mor_pbo)
rmst_gap       <- rmst_observed - rmst_transport

data.frame(
  Quantity = c("McCarthy2012 placebo, observed in its own population",
               "McCarthy2012 placebo model, transported to Morgan2012",
               "Morgan2012 placebo, observed"),
  `RMST (months)` = c(rmst_own, rmst_transport, rmst_observed),
  check.names = FALSE)


## ----anchor-bucher------------------------------------------------------------
cox_lhr <- function(d, active) {
  d <- d[d$trtf %in% c("Pbo", active), ]
  d$trt <- factor(as.character(d$trtf), levels = c("Pbo", active))
  f <- survival::coxph(survival::Surv(eventtime, status) ~ trt, data = d)
  c(est = unname(coef(f)), se = unname(sqrt(diag(vcov(f)))))
}
lhr_mcc <- cox_lhr(ndmm_ipd[ndmm_ipd$studyf == "McCarthy2012", ], "Len")
lhr_mor <- cox_lhr(ndmm_agd[ndmm_agd$studyf == "Morgan2012", ], "Thal")
anchored <- unname(c(lhr_mcc["est"] - lhr_mor["est"],
                     sqrt(lhr_mcc["se"]^2 + lhr_mor["se"]^2)))
anchored_ci <- anchored[1] + c(-1.96, 1.96) * anchored[2]


## ----anchor-mlnmr, message = FALSE--------------------------------------------
# Reload explicitly so this chunk stands on its own: `ndmm_ipd` is bundled with
# mlumr as a single arm, and the anchored check needs multinma's full network.
data("ndmm_ipd", package = "multinma")
data("ndmm_agd", package = "multinma")
data("ndmm_agd_covs", package = "multinma")

mcc <- droplevels(ndmm_ipd[ndmm_ipd$studyf == "McCarthy2012", ])
mor <- droplevels(ndmm_agd[ndmm_agd$studyf == "Morgan2012", ])
mor_covs <- droplevels(ndmm_agd_covs[ndmm_agd_covs$studyf == "Morgan2012", ])

net <- multinma::combine_network(
  multinma::set_ipd(mcc, study = studyf, trt = trtf,
                    Surv = survival::Surv(eventtime, status)),
  multinma::set_agd_surv(mor, study = studyf, trt = trtf,
                         Surv = survival::Surv(eventtime, status),
                         covariates = mor_covs))
net <- multinma::add_integration(net,
  age              = multinma::distr(multinma::qgamma, mean = age_mean, sd = age_sd),
  iss_stage3       = multinma::distr(multinma::qbern, iss_stage3),
  response_cr_vgpr = multinma::distr(multinma::qbern, response_cr_vgpr),
  male             = multinma::distr(multinma::qbern, male),
  n_int = 64)

nmr_fit <- function(form) {
  multinma::nma(net, likelihood = "mspline", n_knots = 7,
                trt_effects = "fixed", regression = form,
                prior_intercept = multinma::normal(0, 10),
                prior_trt = multinma::normal(0, 10),
                prior_reg = multinma::normal(0, 2.5),
                prior_aux = multinma::half_normal(1),
                QR = TRUE, adapt_delta = 0.9,
                chains = 4, iter = 2000, warmup = 1000,
                seed = 2026, refresh = 0)
}

# Prognostic factors only: the anchored analogue of the SPFA model above.
fit_nmr <- nmr_fit(~ age + iss_stage3 + response_cr_vgpr + male)

# Full interaction, exactly multinma's own NDMM specification: the anchored
# analogue of the relaxed model.
fit_nmr_em <- nmr_fit(~ (age + iss_stage3 + response_cr_vgpr + male) * .trt)

# This vignette suppresses warnings, so report the sampler diagnostics for the
# anchored fit explicitly rather than letting them go unseen.
for (nm in c("prognostic only", "with * .trt")) {
  f <- if (nm == "prognostic only") fit_nmr else fit_nmr_em
  cat(sprintf("ML-NMR (%s) divergent transitions: %d of %d draws\n", nm,
              rstan::get_num_divergent(f$stanfit), nrow(as.matrix(f$stanfit))))
}

# multinma reports Thal vs Len; flip to match this vignette's direction.
nmr_contrast <- function(f, study = NULL) {
  d <- as.data.frame(multinma::relative_effects(f, all_contrasts = TRUE)$summary)
  d <- d[grepl("Thal", d$parameter) & grepl("Len", d$parameter), ]
  # Once covariates interact with treatment the contrast is population-specific,
  # so multinma returns one row per study: McCarthy2012 is the index population
  # and Morgan2012 the comparator, the same pair ML-UMR reports.
  if (nrow(d) > 1 && !is.null(study)) d <- d[grepl(study, d$parameter), ]
  d[1, ]
}
nmr      <- nmr_contrast(fit_nmr)
nmr_em_i <- nmr_contrast(fit_nmr_em, study = "McCarthy2012")
nmr_em_c <- nmr_contrast(fit_nmr_em, study = "Morgan2012")

# ML-UMR reports a log HR in each population; pull both from the fit summary.
lhr_row <- function(fit, var) fit$summary[fit$summary$variable == var, ]

# `fit$summary` stores coefficients under Stan's positional names, `beta[1]`
# and so on; only `print()` relabels them as `beta[age]`. Look them up by
# position so the prose below cannot silently cite an empty result.
beta_mean <- function(fit, cov) {
  k <- match(cov, fit$data$covariates)
  # Fail loudly. An unmatched name gives `beta[NA]`, which matches nothing and
  # renders as blank prose with no error, which is the failure this helper
  # exists to avoid. A relaxed fit stores `beta_index[k]` and
  # `beta_comparator[k]`, so `beta[k]` would be empty there too.
  stopifnot(length(k) == 1L, !is.na(k), fit$model == "spfa")
  out <- fit$summary$mean[fit$summary$variable == sprintf("beta[%d]", k)]
  stopifnot(length(out) == 1L)
  out
}

lhr_i     <- lhr_row(fit_mspline, "delta_index")
lhr_c     <- lhr_row(fit_mspline, "delta_comparator")
lhr_rel_i <- lhr_row(fit_mspline_relaxed, "delta_index")
lhr_rel_c <- lhr_row(fit_mspline_relaxed, "delta_comparator")

data.frame(
  Method = c("Naive Cox (unadjusted)",
             "ML-UMR M-spline (SPFA, prognostic only)",
             "ML-UMR M-spline (SPFA, prognostic only)",
             "ML-UMR M-spline (relaxed, effect modification)",
             "ML-UMR M-spline (relaxed, effect modification)",
             "Anchored: Bucher / fixed-effect NMA",
             "Anchored: ML-NMR, prognostic only",
             "Anchored: ML-NMR, with * .trt (multinma's own model)",
             "Anchored: ML-NMR, with * .trt (multinma's own model)"),
  Anchored = c("no", "no", "no", "no", "no", "yes", "yes", "yes", "yes"),
  # The naive Cox row is standardized to no population; it contrasts the two
  # crude arms. `vignette("choosing-a-method")` uses the same label.
  Population = c("unstandardized", "index", "comparator", "index", "comparator",
                 "not population-specific", "not population-specific",
                 "index", "comparator"),
  `log HR` = c(res_naive$estimate,
               lhr_i$mean, lhr_c$mean, lhr_rel_i$mean, lhr_rel_c$mean,
               anchored[1], -nmr$mean, -nmr_em_i$mean, -nmr_em_c$mean),
  `2.5%`   = c(res_naive$ci_lower,
               lhr_i[["2.5%"]], lhr_c[["2.5%"]],
               lhr_rel_i[["2.5%"]], lhr_rel_c[["2.5%"]],
               anchored_ci[1], -nmr[["97.5%"]],
               -nmr_em_i[["97.5%"]], -nmr_em_c[["97.5%"]]),
  `97.5%`  = c(res_naive$ci_upper,
               lhr_i[["97.5%"]], lhr_c[["97.5%"]],
               lhr_rel_i[["97.5%"]], lhr_rel_c[["97.5%"]],
               anchored_ci[2], -nmr[["2.5%"]],
               -nmr_em_i[["2.5%"]], -nmr_em_c[["2.5%"]]),
  check.names = FALSE)

