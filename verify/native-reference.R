# Reference values for checking the WebAssembly models against native CmdStan.
#
# For each case: mlumr (installed natively) builds the Stan data exactly as
# mlumr() would; native CmdStan, compiled from the same committed Stan sources
# the WebAssembly build used, runs a short chain to find a plausible point; then
# one fixed-point step (no warmup, no adaptation, step size 1e-12, tree depth 1)
# from that point records lp__ and every transformed parameter and generated
# quantity. check-models.mjs repeats that step in TinyStan and compares.
#
# Usage: Rscript native-reference.R <stan source dir> <output dir>
args <- commandArgs(trailingOnly = TRUE)
stan_dir <- normalizePath(args[1])
out_root <- args[2]
suppressPackageStartupMessages({
  library(mlumr)
  library(cmdstanr)
})
set.seed(2026)

grab <- function(dat, ...) {
  ns <- asNamespace("mlumr")
  orig <- get(".mlumr_fit_backend", ns)
  on.exit(assignInNamespace(".mlumr_fit_backend", orig, "mlumr"))
  assignInNamespace(".mlumr_fit_backend", function(...) {
    a <- list(...)
    stop(structure(class = c("stan_data_grab", "error", "condition"),
                   list(message = "grab", stan_data = a$stan_data, model = a$model_name)))
  }, "mlumr")
  tryCatch(suppressWarnings(mlumr(dat, verbose = FALSE, seed = 2026, ...)),
           stan_data_grab = function(e) e)
}

psoriasis <- function() {
  data("psoriasis_ipd", "psoriasis_agd", package = "mlumr", envir = environment())
  covs <- c("age", "bsa", "weight", "prevsys")
  ipd <- psoriasis_ipd; ipd$bsa <- ipd$bsa / 100
  ipd <- ipd[ipd$study == "UNCOVER-2" & ipd$treatment == "IXE_Q4W", ]
  ipd <- ipd[stats::complete.cases(ipd[, c("pasi75", covs)]), ]
  agd <- psoriasis_agd; agd$bsa_mean <- agd$bsa_mean / 100; agd$bsa_sd <- agd$bsa_sd / 100
  agd <- agd[agd$study == "FIXTURE" & agd$treatment == "SEC_300", ]
  dat <- combine_data(
    set_ipd(ipd, treatment = "treatment", outcome = "pasi75", covariates = covs),
    set_agd(agd, treatment = "treatment", outcome_n = "pasi75_n", outcome_r = "pasi75_r",
            cov_means = c("age_mean", "bsa_mean", "weight_mean", "prevsys_prop"),
            cov_sds = c("age_sd", "bsa_sd", "weight_sd", NA),
            cov_types = c("continuous", "continuous", "continuous", "binary")))
  suppressWarnings(add_integration(dat, n_int = 64,
    age = distr(qgamma, mean = age_mean, sd = age_sd),
    bsa = distr(qlogitnorm, mean = bsa_mean, sd = bsa_sd),
    weight = distr(qgamma, mean = weight_mean, sd = weight_sd),
    prevsys = distr(qbern, prob = prevsys_mean)))
}

shoulder <- function() {
  data("shoulder_ipd", "shoulder_agd", package = "mlumr", envir = environment())
  dat <- combine_data(
    set_ipd(shoulder_ipd, treatment = "treatment", outcome = "pain_vas_activity",
            family = "normal", covariates = c("age", "baseline_vas")),
    set_agd(shoulder_agd, treatment = "treatment", family = "normal",
            outcome_n = "n", outcome_mean = "y_mean", outcome_se = "y_se",
            cov_means = c("age_mean", "baseline_vas_mean"),
            cov_sds = c("age_sd", "baseline_vas_sd"),
            cov_types = c("continuous", "continuous")))
  suppressWarnings(add_integration(dat, n_int = 64,
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    baseline_vas = distr(qnorm, mean = baseline_vas_mean, sd = baseline_vas_sd)))
}

caries <- function() {
  data("caries_ipd", "caries_agd", package = "mlumr", envir = environment())
  dat <- combine_data(
    set_ipd(caries_ipd, treatment = "treatment", outcome = "dmft", family = "poisson",
            exposure = "exposure", covariates = c("age", "log_cfu")),
    set_agd(caries_agd, treatment = "treatment", family = "poisson",
            outcome_r = "r", outcome_E = "E",
            cov_means = c("age_mean", "log_cfu_mean"),
            cov_sds = c("age_sd", "log_cfu_sd"),
            cov_types = c("continuous", "continuous")))
  suppressWarnings(add_integration(dat, n_int = 64,
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    log_cfu = distr(qnorm, mean = log_cfu_mean, sd = log_cfu_sd)))
}

ndmm <- function(delayed_entry = FALSE, interval = FALSE) {
  data("ndmm_ipd", "ndmm_agd", "ndmm_agd_covs", package = "mlumr", envir = environment())
  covs <- c("age", "iss_stage3", "response_cr_vgpr", "male")
  ipd <- ndmm_ipd[ndmm_ipd$study == "McCarthy2012" & ndmm_ipd$treatment == "Len", ]
  agd <- ndmm_agd[ndmm_agd$study == "Morgan2012" & ndmm_agd$treatment == "Thal", ]
  cv <- ndmm_agd_covs[ndmm_agd_covs$study == "Morgan2012" & ndmm_agd_covs$treatment == "Thal", ]
  agd$age_mean <- cv$age_mean; agd$age_sd <- cv$age_sd
  agd$iss_stage3_prop <- cv$iss_stage3_prop
  agd$response_cr_vgpr_prop <- cv$response_cr_vgpr_prop
  agd$male_prop <- cv$male_prop
  entry <- NULL
  if (delayed_entry) {
    # A third of the index patients enter the risk set late.
    late <- seq_len(nrow(ipd)) %% 3 == 0
    ipd$entry <- ifelse(late, ipd$eventtime * 0.3, 0)
    entry <- "entry"
  }
  ipd_obj <- if (interval) {
    # A third of the observed events are only known to lie in an interval.
    wide <- seq_len(nrow(ipd)) %% 3 == 0 & ipd$status == 1
    lower <- ifelse(wide, ipd$eventtime * 0.7, ipd$eventtime)
    upper <- ifelse(ipd$status == 1, ipd$eventtime, NA)
    set_ipd(ipd, treatment = "treatment", covariates = covs, family = "survival",
            Surv = survival::Surv(lower, upper, type = "interval2"))
  } else {
    set_ipd(ipd, treatment = "treatment", covariates = covs, family = "survival",
            time = "eventtime", status = "status", entry_time = entry)
  }
  dat <- combine_data(
    ipd_obj,
    set_agd_surv(agd, treatment = "treatment", time = "eventtime", status = "status",
                 cov_means = c("age_mean", "iss_stage3_prop", "response_cr_vgpr_prop", "male_prop"),
                 cov_sds = c("age_sd", NA, NA, NA),
                 cov_types = c("continuous", "binary", "binary", "binary")))
  dat <- suppressWarnings(add_integration(dat, n_int = 32,
    age = distr(qgamma, mean = age_mean, sd = age_sd),
    iss_stage3 = distr(qbern, prob = iss_stage3_mean),
    response_cr_vgpr = distr(qbern, prob = response_cr_vgpr_mean),
    male = distr(qbern, prob = male_mean)))
  attr(dat, "tau") <- min(max(ipd$eventtime), max(agd$eventtime))
  dat
}

cases <- list(
  binary_spfa = list(psoriasis, model = "spfa"),
  binary_relaxed = list(psoriasis, model = "relaxed"),
  normal_spfa = list(shoulder, model = "spfa"),
  normal_relaxed = list(shoulder, model = "relaxed"),
  poisson_spfa = list(caries, model = "spfa"),
  poisson_relaxed = list(caries, model = "relaxed"),
  survival_weibull_spfa = list(ndmm, model = "spfa", distribution = "weibull"),
  survival_weibull_relaxed = list(ndmm, model = "relaxed", distribution = "weibull"),
  survival_gengamma_spfa = list(ndmm, model = "spfa", distribution = "gengamma"),
  survival_weibull_delayed_spfa = list(function() ndmm(TRUE), model = "spfa", distribution = "weibull"),
  survival_interval_weibull_spfa = list(function() ndmm(interval = TRUE), model = "spfa", distribution = "weibull"),
  survival_interval_gengamma_spfa = list(function() ndmm(interval = TRUE), model = "spfa", distribution = "gengamma"),
  survival_mspline_spfa = list(ndmm, model = "spfa", distribution = "mspline"),
  survival_mspline_relaxed = list(ndmm, model = "relaxed", distribution = "mspline")
)
only <- if (length(args) > 2) args[-(1:2)] else names(cases)

exe_dir <- file.path(dirname(stan_dir), "native-exe")
dir.create(exe_dir, showWarnings = FALSE, recursive = TRUE)
for (name in only) {
  spec <- cases[[name]]
  dat <- spec[[1]]()
  extra <- spec[-1]
  if (!is.null(attr(dat, "tau"))) extra$rmst_horizon <- attr(dat, "tau")
  g <- do.call(grab, c(list(dat), extra))
  dir <- file.path(out_root, name)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write_stan_json(g$stan_data, file.path(dir, "data.json"))
  mod <- cmdstan_model(file.path(stan_dir, paste0(g$model, ".stan")),
                       include_paths = stan_dir, dir = exe_dir, quiet = TRUE)
  fit0 <- mod$sample(data = file.path(dir, "data.json"), chains = 1, iter_warmup = 150,
                     iter_sampling = 5, seed = 2026, refresh = 0, show_messages = FALSE)
  pars <- mod$variables()$parameters
  # Zero-length parameters (unused auxiliary parameters) have no draws.
  present <- vapply(names(pars), function(p) any(grepl(paste0("^", p, "(\\[|$)"), fit0$metadata()$variables)), TRUE)
  pars <- pars[present]
  last <- as.data.frame(fit0$draws(variables = names(pars), format = "df"))
  last <- last[nrow(last), setdiff(names(last), c(".chain", ".iteration", ".draw")), drop = FALSE]
  init <- list()
  for (p in names(pars)) {
    cols <- grep(paste0("^", p, "(\\[|$)"), names(last), value = TRUE)
    vals <- as.numeric(unlist(last[1, cols]))
    d <- pars[[p]]$dimensions
    init[[p]] <- if (d == 0) vals else if (d == 1) array(vals) else {
      idx <- do.call(rbind, lapply(strsplit(sub(".*\\[(.*)\\]", "\\1", cols), ","), as.integer))
      array(vals, dim = apply(idx, 2, max))
    }
  }
  write_stan_json(init, file.path(dir, "init.json"))
  fit1 <- mod$sample(data = file.path(dir, "data.json"), init = file.path(dir, "init.json"),
                     chains = 1, iter_warmup = 0, iter_sampling = 1, adapt_engaged = FALSE,
                     step_size = 1e-12, max_treedepth = 1, seed = 2026, refresh = 0, sig_figs = 18,
                     show_messages = FALSE)
  ref <- as.data.frame(fit1$draws(format = "df"))
  ref <- ref[1, setdiff(names(ref), c(".chain", ".iteration", ".draw")), drop = FALSE]
  writeLines(jsonlite::toJSON(list(model = g$model, values = as.list(ref)),
                              auto_unbox = TRUE, digits = NA),
             file.path(dir, "expected.json"))
  cat(sprintf("%-32s %-32s %d outputs, lp__ = %.10g\n", name, g$model, ncol(ref), ref$lp__))
}
