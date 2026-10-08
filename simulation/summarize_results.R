#!/usr/bin/env Rscript
# Summarizes the saved replicates of each analysis in results/ (main and,
# if run, omit_x1) into the published table layout and figures:
#   results/scenarios.csv
#   results/<analysis>/metrics_detailed_n150.csv   one row per scenario,
#       quantity, method and population (bias, empirical SE, RMSE, coverage,
#       interval width and their Monte Carlo SEs)
#   results/<analysis>/true_values.csv
#   results/<analysis>/figures/{lor,log_rr}_{bias,coverage}.png
# A scenario and method is summarized only once all its replicates exist.
#
# Usage (from this folder): Rscript summarize_results.R

here <- normalizePath(".")
for (f in list.files(file.path(here, "R"), "[.]R$", full.names = TRUE)) source(f)
scenarios <- scenario_grid(config)
n_rep <- config$n_replications
write.csv(scenarios, file.path(here, "results", "scenarios.csv"), row.names = FALSE)

read_rep <- function(dir, s, r, m) {
  f <- file.path(dir, "reps", sprintf("s%02d_r%04d_%s.rds", s, r, m))
  if (file.exists(f)) tryCatch(readRDS(f), error = function(e) NULL) else NULL
}

for (analysis in c("main", "omit_x1")) {
  dir <- file.path(here, "results", analysis)
  if (!dir.exists(file.path(dir, "reps"))) next
  metrics <- list()
  truths <- list()
  for (s in scenarios$scenario_id) {
    tf <- file.path(dir, "truth", sprintf("s%02d.rds", s))
    if (!file.exists(tf)) next
    tr <- readRDS(tf)$simulated
    sc <- scenarios[s, ]
    truths[[length(truths) + 1]] <- do.call(rbind, lapply(c("index", "comparator"), function(pop) {
      data.frame(sc[, c("scenario_id", "scenario_name", "sample_size", "effect_modification", "population_imbalance",
                        "covariate_correlation_index", "covariate_correlation_comparator", "covariate_type")],
                 population = pop, lor = tr[[paste0("lor_", pop)]], p_A = tr[[paste0("p_A_", pop)]],
                 p_B = tr[[paste0("p_B_", pop)]], rd = tr[[paste0("rd_", pop)]],
                 log_rr = tr[[paste0("log_rr_", pop)]], row.names = NULL)
    }))
    bench <- lapply(seq_len(n_rep), function(r) read_rep(dir, s, r, "bench"))
    if (all(!vapply(bench, is.null, TRUE))) for (m in c("naive", "stc", "maic")) {
      metrics[[length(metrics) + 1]] <- method_metrics(lapply(bench, `[[`, m), tr, sc, m, n_rep)
    }
    for (m in c("spfa", "relaxed")) {
      fits <- lapply(seq_len(n_rep), function(r) read_rep(dir, s, r, m))
      if (all(!vapply(fits, is.null, TRUE))) metrics[[length(metrics) + 1]] <- method_metrics(fits, tr, sc, m, n_rep)
    }
  }
  if (!length(metrics)) next
  metrics <- do.call(rbind, metrics)
  metrics <- metrics[order(metrics$scenario_id, match(metrics$model, methods), metrics$quantity, metrics$population), ]
  write.csv(metrics, file.path(dir, "metrics_detailed_n150.csv"), row.names = FALSE)
  write.csv(do.call(rbind, truths), file.path(dir, "true_values.csv"), row.names = FALSE)
  dir.create(file.path(dir, "figures"), showWarnings = FALSE)
  for (qty in c("lor", "log_rr")) for (what in c("bias", "coverage")) {
    p <- plot_metric(metrics, qty, what, n_rep)
    ggplot2::ggsave(file.path(dir, "figures", sprintf("%s_%s.png", qty, what)), p, width = 10, height = 6.2, dpi = 110)
  }
  message(sprintf("%s: %d rows from %d scenarios.", analysis, nrow(metrics), length(unique(metrics$scenario_id))))
}
