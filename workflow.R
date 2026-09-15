#!/usr/bin/env Rscript
# Simulated teaching example for mlumr 0.1.0.9000, the planned 0.2.0 API.
# Run from any directory. All numbers describe fictional data, not clinical evidence.
args <- commandArgs(trailingOnly = TRUE)
known <- grepl("^--(help|fit|sensitivity|record=.+|engine=(cmdstanr|rstan)|source=.+)$", args)
if (!all(known)) stop("Unknown argument. Run with --help.")
if ("--help" %in% args) {
  cat("Usage: Rscript workflow.R [--source=DIR] [--fit] [--sensitivity] [--record=FILE] [--engine=cmdstanr|rstan]\n",
      "Default: simulate, prepare IPD/AgD, integrate, inspect geometry, run benchmarks.\n",
      "--source=DIR: load the mlumr checkout at DIR with pkgload (needs a built DLL).\n",
      "--fit: fit SPFA and relaxed models; print diagnostics and effect summaries.\n",
      "--sensitivity: refit at a larger grid and under other comparator slope priors, and\n",
      "  evaluate other targets, extracting the prespecified target effect from every refit.\n",
      "  It runs its own base fits. Each refit uses its own seed, 2026 plus the scenario number.\n",
      "--record=FILE: write the fitted target effects and the sensitivity table as JSON;\n",
      "  needs both --fit and --sensitivity so the record is complete.\n",
      "Fits use four chains, 1000 warmup + 1000 retained iterations per chain.\n",
      "This is a teaching run; examine diagnostics before interpreting a posterior.\n",
      sep = "")
  quit(status = 0)
}
if (sum(grepl("^--engine=", args)) > 1L) stop("Choose one Stan engine.")
record_file <- sub("^--record=", "", grep("^--record=", args, value = TRUE))
if (length(record_file) > 1L) stop("Choose one --record file.")
if (length(record_file) && !all(c("--fit", "--sensitivity") %in% args)) {
  stop("--record needs both --fit and --sensitivity so the record holds the fits and the sensitivity table.")
}
record <- list()
engine <- if ("--engine=cmdstanr" %in% args) "cmdstanr" else "rstan"
if (any(grepl("^--source=", args))) {
  if (!requireNamespace("pkgload", quietly = TRUE)) stop("Install pkgload for --source.")
  root <- normalizePath(sub("^--source=", "", grep("^--source=", args, value = TRUE)), mustWork = TRUE)
  pkgload::load_all(root, compile = FALSE, quiet = TRUE)
} else {
  library(mlumr)
}
cat("Package version:", as.character(packageVersion("mlumr")), "\n")

# The package version alone cannot tell one development checkout from another,
# so the record names the exact commit and whether the tree had local changes.
git_out <- function(root, ...) {
  out <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(root), ...), stdout = TRUE, stderr = FALSE)),
                  error = function(e) character())
  if (!is.null(attr(out, "status"))) character() else out
}
source_identity <- if (exists("root")) {
  head_sha <- git_out(root, "rev-parse", "HEAD")
  if (length(head_sha) == 1L && grepl("^[0-9a-f]{40}$", head_sha)) {
    list(loaded_from = "source", commit = head_sha, dirty = length(git_out(root, "status", "--porcelain")) > 0L)
  } else {
    list(loaded_from = "source", commit = NULL, dirty = NULL)
  }
} else {
  sha <- packageDescription("mlumr")$RemoteSha
  list(loaded_from = "installed", commit = if (is.null(sha)) NULL else sha, dirty = NULL)
}
tree_state <- if (isTRUE(source_identity$dirty)) {
  "(tree has local changes)"
} else if (isFALSE(source_identity$dirty)) {
  "(clean tree)"
} else {
  ""
}
cat("Package commit:", if (is.null(source_identity$commit)) "unknown" else source_identity$commit, tree_state, "\n")
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
record$truth <- list(target_rd = unname(known_risks["A"] - known_risks["B"]), target_risks = as.list(known_risks))
interval_of <- function(result) list(estimate = result$estimate, lower = result$ci_lower, upper = result$ci_upper)
record$benchmarks <- list(naive_lor = interval_of(naive_result), stc_lor = interval_of(stc_result))
# Runnable check of the lesson's nonlinearity mechanism.
stopifnot(abs(known_risks["A"] - plogis(-0.8 + 0.8 * mean(target$x))) > 0.001)

# A fit that lost a chain keeps the surviving draws, so its summaries would
# describe fewer chains than requested. Nothing incomplete is recorded.
complete <- function(fit) {
  d <- fit$diagnostics
  if (!identical(as.integer(d$n_chains_returned), as.integer(d$n_chains_requested))) {
    stop(sprintf("Only %d of %d chains returned; the fit is not recorded.", d$n_chains_returned, d$n_chains_requested))
  }
  if (is.null(fit$chain_ids) || length(fit$chain_ids) != nrow(fit$draws)) {
    stop("The fit carries no per-draw chain labels matching its draws.")
  }
  fit
}

if ("--fit" %in% args) {
  for (model in c("spfa", "relaxed")) {
    cat("\nFitting", model, "with", engine, "\n")
    fit <- complete(mlumr(dat, model = model, link = "logit", engine = engine,
                          prior_intercept = prior_normal(0, 2.5),
                          prior_beta = prior_normal(0, 1),
                          chains = 4, iter = 2000, warmup = 1000, seed = 2026,
                          adapt_delta = 0.95, refresh = 0, verbose = FALSE))
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
    record$fit[[model]] <- list(
      n_int = 512L, chains = fit$diagnostics$n_chains_returned, kept_draws = nrow(fit$draws), seed = 2026L,
      divergences = fit$diagnostics$n_divergent, max_rhat = max(fit$summary$Rhat, na.rm = TRUE),
      target_rd = list(mean = target_effect$mean[1], lower = target_effect$q2.5[1], upper = target_effect$q97.5[1])
    )
  }
  cat("\nFor analysis: assess overlap, model fit, prior sensitivity, and refit at\n",
      "larger n_int to check the final estimand; sampler success alone is insufficient.\n", sep = "")
} else if (!"--sensitivity" %in% args) {
  cat("\nPreparation and benchmarks passed. Add --fit for real Stan inference.\n")
}
if ("--sensitivity" %in% args) {
  # The sensitivity loop for the prespecified target. Every scenario refits
  # or re-evaluates, then extracts the effect in the SAME 400-row target
  # again. prior_sensitivity() is not used here: it summarizes the built-in
  # populations and, for a relaxed fit, forwards its extra arguments to
  # mlumr(), not to marginal_effects(), so an external target must be
  # re-extracted from each refit explicitly.
  if (!requireNamespace("posterior", quietly = TRUE)) stop("Install posterior for --sensitivity.")
  chains <- 4L
  # Each refit gets its own seed (2026 plus the scenario number), so the Monte
  # Carlo error of one row is independent of another's and the MCSE of a
  # difference between rows combines the two MCSEs. The base fits keep 2026
  # and therefore equal the --fit fits.
  fit_with <- function(data, model, comparator_scale = 1, seed = 2026L) {
    complete(mlumr(data, model = model, link = "logit", engine = engine,
                   prior_intercept = prior_normal(0, 2.5),
                   prior_beta = prior_normal(0, 1),
                   prior_beta_comparator = prior_normal(0, comparator_scale),
                   chains = chains, iter = 2000, warmup = 1000, seed = seed,
                   adapt_delta = 0.95, refresh = 0, verbose = FALSE))
  }
  # Posterior mean, its Monte Carlo standard error, and the 95% interval of
  # the target risk difference. One draw per row of the fit's draws, folded
  # into an iterations by chains matrix by the fit's own chain labels.
  target_rd <- function(fit, newdata = target) {
    draws <- marginal_effects(fit, newdata = newdata, effect = "rd", summary = FALSE)$rd_target
    stopifnot(length(draws) == length(fit$chain_ids))
    parts <- split(draws, fit$chain_ids)
    stopifnot(length(unique(lengths(parts))) == 1L)
    m <- do.call(cbind, parts)
    c(mean = mean(draws), mcse = posterior::mcse_mean(m), ess_bulk = posterior::ess_bulk(m),
      lower = unname(stats::quantile(draws, 0.025)), upper = unname(stats::quantile(draws, 0.975)))
  }
  checks <- function(fit) {
    list(divergences = fit$diagnostics$n_divergent, max_rhat = max(fit$summary$Rhat, na.rm = TRUE))
  }
  rows <- list()
  row_of <- function(scenario, model, n_int, comparator_scale, target_name, fit, newdata = target, seed = 2026L) {
    est <- target_rd(fit, newdata)
    cbind(data.frame(scenario = scenario, model = model, n_int = n_int, comparator_scale = comparator_scale,
                     target = target_name, seed = seed, row.names = NULL),
          as.data.frame(t(est)))
  }

  cat("\n== Sensitivity loop for the prespecified target ==\n")
  cat("Base fits: n_int 512, prior_beta and prior_beta_comparator both normal(0, 1).\n")
  base <- list(spfa = fit_with(dat, "spfa"), relaxed = fit_with(dat, "relaxed"))
  for (model in names(base)) rows[[length(rows) + 1L]] <- row_of("base", model, 512L, 1, "prespecified", base[[model]])

  cat("\n-- 1. Integration refit: n_int 2048 against 512, same priors, seed 2027 --\n")
  dat_big <- add_integration(dat, n_int = 2048L, x = distr(qnorm, mean = x_mean, sd = x_sd))
  big <- list(spfa = fit_with(dat_big, "spfa", seed = 2027L), relaxed = fit_with(dat_big, "relaxed", seed = 2027L))
  for (model in names(big)) {
    rows[[length(rows) + 1L]] <- row_of("integration", model, 2048L, 1, "prespecified", big[[model]], seed = 2027L)
  }
  cat("Read the difference between the two posterior means against the MCSE of\n",
      "each, not as an exact number: two independent sets of chains differ by\n",
      "Monte Carlo noise even when the grid is already fine enough.\n", sep = "")

  cat("\n-- 2. Comparator slope prior, relaxed model, prior_beta fixed at normal(0, 1) --\n")
  prior_checks <- list()
  scales <- c(0.25, 0.5, 2.5, 5)
  for (k in seq_along(scales)) {
    scale <- scales[k]
    seed <- 2027L + k
    fit <- fit_with(dat, "relaxed", comparator_scale = scale, seed = seed)
    prior_checks[[as.character(scale)]] <- checks(fit)
    rows[[length(rows) + 1L]] <- row_of("comparator prior", "relaxed", 512L, scale, "prespecified", fit, seed = seed)
    cat(sprintf("prior_beta_comparator normal(0, %s): built-in populations\n", scale))
    print(marginal_effects(fit, population = "both", effect = "rd"))
  }

  cat("\n-- 3. Transport: the same fits evaluated in other targets --\n")
  overlap <- function(newdata) {
    inside <- newdata$x >= stats::quantile(trial_a$x, 0.025) &
      newdata$x <= stats::quantile(trial_a$x, 0.975)
    mean(inside)
  }
  targets <- list(
    prespecified = target,
    "shifted, mean 1.0" = data.frame(x = qnorm((seq_len(400) - 0.5) / 400, mean = 1.0, sd = 0.8)),
    "extrapolating, mean 2.2" = data.frame(x = qnorm((seq_len(400) - 0.5) / 400, mean = 2.2, sd = 0.5))
  )
  record$transport_overlap <- lapply(targets, overlap)
  for (name in names(targets)) {
    cat(sprintf("%s: share of target rows inside trial A's central 95%% covariate range = %.2f\n",
                name, overlap(targets[[name]])))
    for (model in names(base)) {
      rows[[length(rows) + 1L]] <- row_of("transport", model, 512L, 1, name, base[[model]], targets[[name]])
    }
  }
  cat("With one covariate there is no correlation between covariates to carry\n",
      "from trial A to trial B, so no dependence scenario applies to this example.\n", sep = "")

  table <- do.call(rbind, rows)
  cat("\n== Target risk difference, A minus B, under every scenario ==\n")
  print(table, digits = 4, row.names = FALSE)
  cat("\nSampling checks of every refit (divergences, largest R-hat):\n")
  all_checks <- list(spfa_base = checks(base$spfa), relaxed_base = checks(base$relaxed),
                     spfa_n_int_2048 = checks(big$spfa), relaxed_n_int_2048 = checks(big$relaxed))
  for (i in seq_along(prior_checks)) {
    all_checks[[paste0("relaxed_comparator_scale_", names(prior_checks)[i])]] <- prior_checks[[i]]
  }
  for (name in names(all_checks)) cat(name, ":", unlist(all_checks[[name]]), "\n")
  record$sensitivity <- table
  record$sensitivity_checks <- all_checks
  cat("\nHow to read this: the integration and prior rows should move the\n",
      "prespecified target by less than a few MCSEs if the analysis is ready to\n",
      "report; a change inside that noise is consistent with an adequate grid,\n",
      "not proof of one. A target that extrapolates beyond trial A's covariates widens and\n",
      "separates the two models; for such a target the defensible conclusion is\n",
      "that the evidence does not support a headline number, not the narrowest\n",
      "interval.\n", sep = "")
}

if (length(record_file)) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install jsonlite for --record.")
  record$about <- "Fitted results of workflow.R on simulated data. Not clinical evidence."
  has_cmdstan <- engine == "cmdstanr" && requireNamespace("cmdstanr", quietly = TRUE)
  record$package <- c(list(version = as.character(packageVersion("mlumr")), engine = engine,
                           r = R.version.string,
                           cmdstan = if (has_cmdstan) as.character(cmdstanr::cmdstan_version()) else NULL),
                      source_identity)
  script_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
  record$script_sha256 <- if (requireNamespace("digest", quietly = TRUE)) {
    digest::digest(file = script_path, algo = "sha256")
  } else {
    NULL
  }
  record$run <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  writeLines(jsonlite::toJSON(record, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null"), record_file)
  cat("\nWrote", record_file, "\n")
}
