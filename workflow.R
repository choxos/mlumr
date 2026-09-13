#!/usr/bin/env Rscript
# Simulated teaching example for mlumr 0.1.0.9000, the planned 0.2.0 API.
# Run from any directory. All numbers describe fictional data, not clinical evidence.
args <- commandArgs(trailingOnly = TRUE)
known <- grepl("^--(help|fit|engine=(cmdstanr|rstan)|source=.+)$", args)
if (!all(known)) stop("Unknown argument. Run with --help.")
if ("--help" %in% args) {
  cat("Usage: Rscript workflow.R [--source=DIR] [--fit] [--engine=cmdstanr|rstan]\n",
      "Default: simulate, prepare IPD/AgD, integrate, inspect geometry, run benchmarks.\n",
      "--source=DIR: load the mlumr checkout at DIR with pkgload (needs a built DLL).\n",
      "--fit: fit SPFA and relaxed models; print diagnostics and effect summaries.\n",
      "Fits use four chains, 1000 warmup + 1000 retained iterations per chain.\n",
      "This is a teaching run; examine diagnostics before interpreting a posterior.\n",
      sep = "")
  quit(status = 0)
}
if (sum(grepl("^--engine=", args)) > 1L) stop("Choose one Stan engine.")
engine <- if ("--engine=cmdstanr" %in% args) "cmdstanr" else "rstan"
if (any(grepl("^--source=", args))) {
  if (!requireNamespace("pkgload", quietly = TRUE)) stop("Install pkgload for --source.")
  root <- normalizePath(sub("^--source=", "", grep("^--source=", args, value = TRUE)), mustWork = TRUE)
  pkgload::load_all(root, compile = FALSE, quiet = TRUE)
} else {
  library(mlumr)
}
cat("Package version:", as.character(packageVersion("mlumr")), "\n")
if (packageVersion("mlumr") < package_version("0.1.0.9000")) {
  stop("This lesson needs the development checkout (0.1.0.9000) or a compatible later release.")
}

# One standardized continuous prognostic covariate. Distinct trials receive
# different treatments, so treatment and study are inseparable in these data.
set.seed(2026)
trial_a <- data.frame(trt = "A", study = "index", x = rnorm(300, -0.3, 1))
trial_a$event <- rbinom(nrow(trial_a), 1, plogis(-0.8 + 0.8 * trial_a$x))

# Every comparator participant belongs to exactly one cell. Generate hidden
# comparator IPD solely to construct its published summaries; never give it to mlumr.
cell_size <- c(150L, 180L, 170L)
cell_mean <- c(-0.7, 0.3, 1.3)
hidden_b <- data.frame(cell = rep(seq_along(cell_size), cell_size))
hidden_b$x <- rnorm(nrow(hidden_b), cell_mean[hidden_b$cell], 0.65)
hidden_b$event <- rbinom(nrow(hidden_b), 1, plogis(-0.25 + 0.8 * hidden_b$x))
trial_b <- do.call(rbind, lapply(split(hidden_b, hidden_b$cell), function(d) {
  data.frame(trt = "B", study = "comparator", cell = d$cell[1],
             n = nrow(d), events = sum(d$event), x_mean = mean(d$x), x_sd = sd(d$x))
}))
stopifnot(sum(trial_b$n) == nrow(hidden_b), !anyDuplicated(trial_b$cell),
          all(trial_b$events >= 0 & trial_b$events <= trial_b$n))
rm(hidden_b)
cat("\nPublished comparator partition (simulated):\n")
print(trial_b, row.names = FALSE)

ipd <- set_ipd(trial_a, treatment = "trt", outcome = "event", covariates = "x",
               family = "binomial", study = "study")
agd <- set_agd(trial_b, treatment = "trt", family = "binomial",
               outcome_n = "n", outcome_r = "events", cov_means = "x_mean",
               cov_sds = "x_sd", cov_types = "continuous", study = "study")
dat <- combine_data(ipd, agd)
dat <- add_integration(dat, n_int = 512,
                       x = distr(qnorm, mean = x_mean, sd = x_sd))
integration <- check_integration(dat, x = distr(qnorm, mean = x_mean, sd = x_sd))
identification <- check_identification(dat, link = "logit")
stopifnot(isTRUE(dat$has_integration), identification$n_rows == 3L,
          identification$n_cov == 1L)
cat("\nIntegration verdicts (heuristics, not posterior error bounds):\n")
print(integration$verdict)
cat("Nonlinear mean-profile geometry does not certify global identification.\n")

# Naive compares each arm in its own observed population. STC fits only A's
# regression, averages its predictions in B, then contrasts B's observed outcome.
cat("\nNaive benchmark, distinct observed populations:\n")
naive_result <- naive(dat, link = "logit")
print(naive_result)
cat("\nSTC benchmark, comparator population:\n")
stc_result <- stc(dat, link = "logit")
print(stc_result)
stopifnot(is.finite(naive_result$estimate), is.finite(stc_result$estimate))

# A deterministic target distribution, not a single average patient.
target <- data.frame(x = qnorm((seq_len(400) - 0.5) / 400, mean = 0.4, sd = 0.8))
profiles <- data.frame(x = c(-1, 0, 1))
known_risks <- c(A = mean(plogis(-0.8 + 0.8 * target$x)),
                 B = mean(plogis(-0.25 + 0.8 * target$x)))
cat("\nKnown generating risks in the synthetic target (not fitted estimates):\n")
print(known_risks)
cat("Known target risk difference A minus B:", known_risks["A"] - known_risks["B"], "\n")
# Runnable check of the lesson's nonlinearity mechanism.
stopifnot(abs(known_risks["A"] - plogis(-0.8 + 0.8 * mean(target$x))) > 0.001)

if ("--fit" %in% args) {
  for (model in c("spfa", "relaxed")) {
    cat("\nFitting", model, "with", engine, "\n")
    fit <- mlumr(dat, model = model, link = "logit", engine = engine,
                 prior_intercept = prior_normal(0, 2.5),
                 prior_beta = prior_normal(0, 1),
                 chains = 4, iter = 2000, warmup = 1000, seed = 2026,
                 adapt_delta = 0.95, refresh = 0, verbose = FALSE)
    # mlumr() checks the chains itself and warns; summary() prints those checks again.
    summary(fit)
    cat("\nAbsolute risks standardized to each built-in population:\n")
    print(predict(fit, population = "both", type = "response"))
    cat("\nMarginal contrasts (A minus B or A divided by B):\n")
    print(marginal_effects(fit, population = "both"))
    cat("\nConditional contrasts at three individual covariate profiles:\n")
    print(conditional_effects(fit, newdata = profiles))
    cat("\nAbsolute risks and marginal effects in the supplied target distribution:\n")
    print(predict(fit, newdata = target, type = "response"))
    target_effect <- marginal_effects(fit, newdata = target, effect = "rd")
    print(target_effect)
    stopifnot(all(is.finite(target_effect$mean)))
  }
  cat("\nFor analysis: assess overlap, model fit, prior sensitivity, and refit at\n",
      "larger n_int to check the final estimand; sampler success alone is insufficient.\n", sep = "")
} else {
  cat("\nPreparation and benchmarks passed. Add --fit for real Stan inference.\n")
}
