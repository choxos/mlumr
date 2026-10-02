# Code from vignette("binary-outcomes"): Binary outcomes: an unanchored PASI 75 comparison
# mlumr GitHub main at 95c5bbd, extracted with knitr::purl(). Tables print as
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


## ----anchor-constancy---------------------------------------------------------
data("plaque_psoriasis_ipd", package = "multinma")
data("plaque_psoriasis_agd", package = "multinma")

u2 <- plaque_psoriasis_ipd[plaque_psoriasis_ipd$studyc == "UNCOVER-2", ]
u2$bsa <- u2$bsa / 100
u2$weight <- u2$weight / 10
u2$prevsys <- as.integer(u2$prevsys)
fx <- plaque_psoriasis_agd[plaque_psoriasis_agd$studyc == "FIXTURE", ]
fx$bsa_mean <- fx$bsa_mean / 100
fx$bsa_sd <- fx$bsa_sd / 100
fx$weight_mean <- fx$weight_mean / 10
fx$weight_sd <- fx$weight_sd / 10
fx$prevsys <- fx$prevsys / 100

# Conditional constancy is the assumption ML-UMR cannot test from the unanchored
# data alone. With the discarded arms it becomes testable: fit the outcome model
# in an arm both trials share, transport it to the FIXTURE population, and
# compare against what FIXTURE actually reported for that same arm.
set.seed(2026)
transport_check <- function(arm) {
  ip <- u2[u2$trtc == arm, ]
  ip <- ip[stats::complete.cases(ip[, c("pasi75", covs)]), ]
  m <- glm(pasi75 ~ age + bsa + weight + prevsys, binomial, data = ip)
  ag <- fx[fx$trtc == arm, ]
  n <- 5e4
  nd <- data.frame(age = rnorm(n, ag$age_mean, ag$age_sd),
                   bsa = rnorm(n, ag$bsa_mean, ag$bsa_sd),
                   weight = rnorm(n, ag$weight_mean, ag$weight_sd),
                   prevsys = rbinom(n, 1, ag$prevsys))
  data.frame(Arm = arm,
             `UNCOVER-2 observed` = mean(ip$pasi75),
             `Transported to FIXTURE` = mean(predict(m, nd, type = "response")),
             `FIXTURE observed` = ag$pasi75_r / ag$pasi75_n,
             check.names = FALSE)
}
do.call(rbind, lapply(c("PBO", "ETN"), transport_check))


## ----anchor-bucher------------------------------------------------------------
lor <- function(r1, n1, r0, n0)
  c(est = log(r1 / (n1 - r1)) - log(r0 / (n0 - r0)),
    se  = sqrt(1 / r1 + 1 / (n1 - r1) + 1 / r0 + 1 / (n0 - r0)))

u2_tab <- table(u2$trtc[!is.na(u2$pasi75)], u2$pasi75[!is.na(u2$pasi75)])
lor_u2 <- lor(u2_tab["IXE_Q4W", "1"], sum(u2_tab["IXE_Q4W", ]),
              u2_tab["ETN", "1"],     sum(u2_tab["ETN", ]))
lor_fx <- lor(fx$pasi75_r[fx$trtc == "SEC_300"], fx$pasi75_n[fx$trtc == "SEC_300"],
              fx$pasi75_r[fx$trtc == "ETN"],     fx$pasi75_n[fx$trtc == "ETN"])
anchored <- unname(c(lor_u2["est"] - lor_fx["est"],
                     sqrt(lor_u2["se"]^2 + lor_fx["se"]^2)))
anchored_ci <- anchored[1] + c(-1.96, 1.96) * anchored[2]


## ----anchor-mlnmr, message = FALSE--------------------------------------------
keep_u2 <- u2[u2$trtc %in% c("ETN", "IXE_Q4W"), ]
keep_u2 <- keep_u2[stats::complete.cases(keep_u2[, c("pasi75", covs)]), ]
keep_u2$prevsys <- as.numeric(keep_u2$prevsys)
keep_fx <- fx[fx$trtc %in% c("ETN", "SEC_300"), ]

net <- multinma::combine_network(
  multinma::set_ipd(keep_u2, study = studyc, trt = trtc, r = pasi75),
  multinma::set_agd_arm(keep_fx, study = studyc, trt = trtc,
                        r = pasi75_r, n = pasi75_n))
net <- multinma::add_integration(net,
  age     = multinma::distr(multinma::qgamma, mean = age_mean, sd = age_sd),
  bsa     = multinma::distr(multinma::qlogitnorm, mean = bsa_mean, sd = bsa_sd),
  weight  = multinma::distr(multinma::qgamma, mean = weight_mean, sd = weight_sd),
  prevsys = multinma::distr(multinma::qbern, prob = prevsys),
  n_int = 64)

nmr_fit <- function(form) {
  multinma::nma(net, trt_effects = "fixed", link = "logit", regression = form,
                prior_intercept = multinma::normal(0, 10),
                prior_trt = multinma::normal(0, 10),
                prior_reg = multinma::normal(0, 2.5),
                QR = TRUE, chains = 4, iter = 2000, warmup = 1000,
                seed = 2026, refresh = 0)
}

# Prognostic factors only: the anchored analogue of the SPFA model above.
fit_nmr <- nmr_fit(~ age + bsa + weight + prevsys)

# Full interaction, the formula multinma's own plaque-psoriasis example uses:
# the anchored analogue of the relaxed model. multinma fits it with treatment
# classes sharing their interactions; this sub-network has no classes, and
# SEC_300 appears only in the aggregate study.
fit_nmr_em <- nmr_fit(~ (age + bsa + weight + prevsys) * .trt)

# multinma reports SEC_300 vs IXE_Q4W; flip to match this vignette's direction.
nmr_contrast <- function(f, study = NULL) {
  d <- as.data.frame(multinma::relative_effects(f, all_contrasts = TRUE)$summary)
  d <- d[grepl("SEC_300", d$parameter) & grepl("IXE_Q4W", d$parameter), ]
  # Once covariates interact with treatment the contrast is population-specific,
  # so multinma returns one row per study: UNCOVER-2 is the index population and
  # FIXTURE the comparator, the same pair ML-UMR reports.
  if (nrow(d) > 1 && !is.null(study)) d <- d[grepl(study, d$parameter), ]
  d[1, ]
}
nmr      <- nmr_contrast(fit_nmr)
nmr_em_i <- nmr_contrast(fit_nmr_em, study = "UNCOVER-2")
nmr_em_c <- nmr_contrast(fit_nmr_em, study = "FIXTURE")

# ML-UMR is reported in both target populations so the anchored rows, which
# are comparator-population estimands, can be read against a like-for-like row.
me_spfa_i <- pop(me_spfa_b, "Index")
me_spfa_c <- pop(me_spfa_b, "Comparator")
me_rel_i  <- pop(me_rel_b, "Index")
me_rel_c  <- pop(me_rel_b, "Comparator")

data.frame(
  Method = c("Naive (unadjusted)", "STC (G-computation)",
             "ML-UMR SPFA (prognostic only)",
             "ML-UMR SPFA (prognostic only)",
             "ML-UMR relaxed (effect modification)",
             "ML-UMR relaxed (effect modification)",
             "Anchored: Bucher / fixed-effect NMA",
             "Anchored: ML-NMR, prognostic only",
             "Anchored: ML-NMR, with * .trt (multinma's own model)",
             "Anchored: ML-NMR, with * .trt (multinma's own model)"),
  Anchored = c("no", "no", "no", "no", "no", "no", "yes", "yes", "yes", "yes"),
  # `naive()` contrasts the two crude outcomes; it is standardized to no
  # population, which is the point of comparing it against the adjusted rows.
  # `vignette("choosing-a-method")` uses the same label for the same reason.
  Population = c("unstandardized", "comparator", "index", "comparator",
                 "index", "comparator", "not population-specific",
                 "not population-specific", "index", "comparator"),
  LOR   = c(res_naive$link_effect, res_stc$link_effect,
            me_spfa_i$mean, me_spfa_c$mean, me_rel_i$mean, me_rel_c$mean,
            anchored[1], -nmr$mean, -nmr_em_i$mean, -nmr_em_c$mean),
  `2.5%`  = c(res_naive$ci_lower, res_stc$ci_lower,
              me_spfa_i$q2.5, me_spfa_c$q2.5, me_rel_i$q2.5, me_rel_c$q2.5,
              anchored_ci[1], -nmr[["97.5%"]],
              -nmr_em_i[["97.5%"]], -nmr_em_c[["97.5%"]]),
  `97.5%` = c(res_naive$ci_upper, res_stc$ci_upper,
              me_spfa_i$q97.5, me_spfa_c$q97.5, me_rel_i$q97.5, me_rel_c$q97.5,
              anchored_ci[2], -nmr[["2.5%"]],
              -nmr_em_i[["2.5%"]], -nmr_em_c[["2.5%"]]),
  check.names = FALSE)

