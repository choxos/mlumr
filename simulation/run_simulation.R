#!/usr/bin/env Rscript
# Runs the ML-UMR simulation study. Each replicate of each method is saved to
# its own file, so the run can be stopped and resumed and split across
# sessions or machines; summarize_results.R reads whatever has been run.
#
# Usage (from this folder):
#   Rscript run_simulation.R [--scenarios=1:12] [--reps=1:500]
#                            [--methods=bench,spfa,relaxed] [--workers=3]
#                            [--omit=x1]
#
#   --methods  bench (naive, STC and MAIC together; fast), spfa, relaxed
#   --omit     a covariate the analyses leave out (x1, the prognostic factor);
#              the same datasets and seeds; results in results/omit_x1
#              instead of results/main
#
# The full study is 12 scenarios x 500 replicates: 6,000 benchmark sets
# (minutes) and 12,000 ML-UMR fits (about a minute each per worker).

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
}
here <- normalizePath(".")
for (f in list.files(file.path(here, "R"), "[.]R$", full.names = TRUE)) source(f)

scenario_ids <- eval(parse(text = opt("scenarios", "1:12")))
rep_ids <- eval(parse(text = opt("reps", paste0("1:", config$n_replications))))
run_methods <- strsplit(opt("methods", "bench,spfa,relaxed"), ",")[[1]]
workers <- as.integer(opt("workers", "3"))
config$omit <- setdiff(strsplit(opt("omit", ""), ",")[[1]], "")
stopifnot(all(run_methods %in% c("bench", "spfa", "relaxed")),
          all(config$omit %in% c("x1", "x2")), length(config$omit) < 2)

out_dir <- file.path(here, "results", if (length(config$omit)) paste0("omit_", config$omit) else "main")
dir.create(file.path(out_dir, "truth"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "reps"), recursive = TRUE, showWarnings = FALSE)
scenarios <- scenario_grid(config)
saveRDS(config, file.path(out_dir, "config.rds"))

#' Write an RDS file atomically: to a temporary name unique to this process,
#' then renamed, so an interrupted or concurrent run never leaves a partial file.
save_atomic <- function(x, f) {
  tmp <- sprintf("%s.%d.part", f, Sys.getpid())
  saveRDS(x, tmp)
  if (!file.rename(tmp, f)) stop("could not write ", f, call. = FALSE)
}

# True effects, computed once per scenario.
for (s in scenario_ids) {
  f <- file.path(out_dir, "truth", sprintf("s%02d.rds", s))
  if (!file.exists(f)) {
    message(sprintf("Truth for scenario %d (%s)...", s, scenarios$scenario_name[s]))
    save_atomic(list(simulated = compute_truth(scenarios[s, ], config)), f)
  }
}

rep_file <- function(s, r, m) file.path(out_dir, "reps", sprintf("s%02d_r%04d_%s.rds", s, r, m))
tasks <- expand.grid(rep = rep_ids, method = run_methods, scenario = scenario_ids, stringsAsFactors = FALSE)
tasks <- tasks[!file.exists(rep_file(tasks$scenario, tasks$rep, tasks$method)), ]
message(sprintf("%d tasks to run on %d workers (%s).", nrow(tasks), workers, basename(out_dir)))
if (!nrow(tasks)) quit(save = "no")

run_task <- function(i) {
  t <- tasks[i, ]
  sc <- scenarios[t$scenario, ]
  d <- generate_rep_data(sc, t$rep, config)
  covs <- analysis_covariates(config)
  t0 <- proc.time()[["elapsed"]]
  res <- switch(t$method,
    bench = tryCatch(run_benchmarks(d, config), error = function(e) list(error = conditionMessage(e))),
    spfa = fit_mlumr(build_network(d$ipd, d$agd, config$n_int_spfa, covs), "spfa", d$seed, config),
    relaxed = fit_mlumr(build_network(d$ipd, d$agd_subgroups, config$n_int_relaxed, covs), "relaxed",
                        d$seed + 50000, config)
  )
  res$seconds <- proc.time()[["elapsed"]] - t0
  save_atomic(res, rep_file(t$scenario, t$rep, t$method))
  t$method
}

started <- Sys.time()
if (workers > 1) {
  cl <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  # Functions first, then this run's settings (which override 00_config.R's).
  parallel::clusterExport(cl, "here")
  parallel::clusterEvalQ(cl, {
    for (f in list.files(file.path(here, "R"), "[.]R$", full.names = TRUE)) source(f)
    NULL
  })
  parallel::clusterExport(cl, c("config", "scenarios", "tasks", "out_dir", "rep_file", "save_atomic", "run_task"))
  done <- parallel::parLapplyLB(cl, seq_len(nrow(tasks)), run_task)
} else {
  done <- lapply(seq_len(nrow(tasks)), run_task)
}
message(sprintf("Finished %d tasks in %.1f minutes.", length(done),
                as.numeric(difftime(Sys.time(), started, units = "mins"))))
