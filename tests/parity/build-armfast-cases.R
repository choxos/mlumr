#!/usr/bin/env Rscript
# Build the Stan data fixtures for the armfast parity gate.
#
# Usage:
#   Rscript tests/parity/build-armfast-cases.R OUTPUT.rds
#
# Every fixture comes from the public route (set_ipd(), set_agd_surv(),
# combine_data(), add_integration(), mlumr()) with the sampler replaced by a
# capture, so the Stan data are exactly what a user's call would send and no
# MCMC runs. A fixture the package refuses aborts the builder; nothing is
# skipped. The gate (armfast-parity-gate.R) reads the result in a process that
# does not load mlumr, because cmdstanr's model methods cannot be loaded next
# to the package's own copy of TBB.
#
# The multi-arm fixtures are assembled by hand from a captured single-arm data
# list: set_agd_surv() refuses more than one comparator arm, so they check the
# Stan array code defensively and are not a supported public workflow.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript build-armfast-cases.R OUTPUT.rds")
out_file <- args[[1L]]


script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (length(script) != 1L) stop("Run this script with Rscript.")
source(file.path(dirname(script), "armfast-fixtures.R"))

specs <- list()
add <- function(id, model, distribution, fixture = list(), fit = list()) {
  specs[[length(specs) + 1L]] <<- list(id = id, model = model,
                                       distribution = distribution,
                                       fixture = fixture, fit = fit)
}

parametric <- c("exponential", "weibull", "gompertz", "exponential-aft",
                "weibull-aft", "lognormal", "loglogistic", "gamma", "gengamma")
for (d in c(parametric, "mspline", "pexp")) {
  for (m in c("spfa", "relaxed")) add(sprintf("%s_%s_base", d, m), m, d)
}
# One baseline shared by both studies instead of the default two strata.
add("weibull_spfa_aux_none", "spfa", "weibull", fit = list(aux_by = "none"))
add("lognormal_relaxed_aux_none", "relaxed", "lognormal", fit = list(aux_by = "none"))
add("mspline_spfa_aux_none", "spfa", "mspline", fit = list(aux_by = "none"))
add("pexp_relaxed_aux_none", "relaxed", "pexp", fit = list(aux_by = "none"))
# Centering and QR controls.
add("weibull_spfa_no_center", "spfa", "weibull", fit = list(center = FALSE))
add("weibull_spfa_qr", "spfa", "weibull", fit = list(qr = TRUE))
add("gengamma_relaxed_qr", "relaxed", "gengamma", fit = list(qr = TRUE))
add("mspline_relaxed_qr", "relaxed", "mspline", fit = list(qr = TRUE))
add("gompertz_spfa_qr_no_center", "spfa", "gompertz",
    fit = list(qr = TRUE, center = FALSE))
# Covariate dimension.
add("exponential_spfa_one_covariate", "spfa", "exponential",
    fixture = list(n_cov = 1))
add("weibull-aft_relaxed_three_covariates", "relaxed", "weibull-aft",
    fixture = list(n_cov = 3))
add("pexp_spfa_three_covariates", "spfa", "pexp", fixture = list(n_cov = 3))
# Censoring statuses and delayed entry.
add("weibull_spfa_mixed_censoring_entry", "spfa", "weibull",
    fixture = list(censoring = "mixed", entry = TRUE))
add("gengamma_relaxed_mixed_censoring_entry", "relaxed", "gengamma",
    fixture = list(censoring = "mixed", entry = TRUE))
add("lognormal_spfa_mixed_censoring", "spfa", "lognormal",
    fixture = list(censoring = "mixed"))
add("loglogistic_relaxed_entry", "relaxed", "loglogistic",
    fixture = list(entry = TRUE))
add("mspline_spfa_mixed_censoring_entry", "spfa", "mspline",
    fixture = list(censoring = "mixed", entry = TRUE))
add("pexp_relaxed_mixed_censoring_entry", "relaxed", "pexp",
    fixture = list(censoring = "mixed", entry = TRUE))
# Integration resolution.
add("weibull_spfa_n_int_64", "spfa", "weibull", fixture = list(n_int = 64))
add("mspline_relaxed_n_int_64", "relaxed", "mspline", fixture = list(n_int = 64))
add("gamma_spfa_n_int_8", "spfa", "gamma", fixture = list(n_int = 8))
# Knot count.
add("mspline_spfa_three_knots", "spfa", "mspline", fit = list(n_knots = 3))
add("pexp_relaxed_ten_knots", "relaxed", "pexp", fit = list(n_knots = 10))
# A comparator prior with its own family and degrees of freedom.
add("weibull_relaxed_comparator_student_t", "relaxed", "weibull",
    fit = list(prior_beta_comparator = prior_student_t(df = 4, mean = 0, sd = 1)))
add("mspline_relaxed_comparator_student_t", "relaxed", "mspline",
    fit = list(prior_beta_comparator = prior_student_t(df = 3, mean = 0.1, sd = 0.8)))
# A larger fixture.
add("weibull_spfa_larger", "spfa", "weibull",
    fixture = list(n_ipd = 150, n_agd = 300))

cases <- vector("list", length(specs))
for (i in seq_along(specs)) {
  s <- specs[[i]]
  fixture <- c(list(seed = 2026 + i), s$fixture)
  dat <- do.call(make_data, fixture)
  cond <- do.call(capture_stan_data, c(
    list(dat = dat, model = s$model, distribution = s$distribution), s$fit))
  sd <- cond$stan_data
  stopifnot(sd$n_agd_rows == 1L)
  if (identical(s$fixture$censoring, "mixed")) {
    stopifnot(all(0:3 %in% sd$ipd_status), all(0:3 %in% sd$agd_status))
  }
  if (isTRUE(s$fixture$entry)) {
    stopifnot(any(sd$ipd_delay_time > 0), any(sd$agd_delay_time > 0))
  }
  cases[[i]] <- list(id = s$id, stem = cond$model_name, model = s$model,
                     distribution = s$distribution, fixture = fixture,
                     fit = s$fit, arms = 1L, data = sd)
  cat(sprintf("%-42s %-32s n_ipd=%3d n_agd=%3d n_int=%2d qr=%d strata=%d\n",
              s$id, cond$model_name, sd$n_ipd, sd$n_agd, sd$n_int, sd$qr,
              sd$n_strata))
}

ids <- vapply(cases, `[[`, character(1), "id")
for (base in c("weibull_spfa_base", "weibull_relaxed_base",
               "mspline_spfa_base", "mspline_relaxed_base")) {
  src <- cases[[match(base, ids)]]
  multi <- src
  multi$id <- paste0(base, "_multiarm_defensive")
  multi$arms <- 3L
  multi$data <- make_multi_arm(src$data)
  cases[[length(cases) + 1L]] <- multi
  cat(sprintf("%-42s %-32s three interleaved arms, %s rows each\n", multi$id,
              multi$stem, paste(tabulate(multi$data$agd_arm), collapse = "/")))
}

stopifnot(!anyDuplicated(vapply(cases, `[[`, character(1), "id")))
# Record which source tree built the fixtures: armfast-fixtures.R loads the
# checkout this script sits in, so its commit and whether it had local edits.
root <- normalizePath(file.path(dirname(script), "..", ".."))
git_head <- tryCatch(system2("git", c("-C", shQuote(root), "rev-parse", "HEAD"),
                             stdout = TRUE), error = function(e) NA_character_)
git_dirty <- tryCatch(length(system2("git", c("-C", shQuote(root), "status",
                                              "--porcelain"), stdout = TRUE)) > 0,
                      error = function(e) NA)
meta <- list(created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
             mlumr_version = as.character(utils::packageVersion("mlumr")),
             source_root = root, git_head = git_head, git_dirty = git_dirty,
             r_version = R.version.string,
             n_cases = length(cases))
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(list(meta = meta, cases = cases), out_file)
cat(sprintf("Wrote %d cases to %s (mlumr %s, %s%s)\n", length(cases), out_file,
            meta$mlumr_version, git_head,
            if (isTRUE(git_dirty)) ", with local edits" else ""))
