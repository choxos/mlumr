# Performance measures, with the columns of the published
# metrics_detailed_n150.csv.

quantities <- c("lor", "p_A", "p_B", "rd", "log_rr")
methods <- c("spfa", "relaxed", "naive", "stc", "maic")

#' Metrics for one scenario and method.
#'
#' @param reps List of per-replicate method results (NULL for a missing or
#'   failed replicate), in replicate order.
#' @param truth Named list of true values (compute_truth()).
#' @param n_total Number of replicates the scenario was meant to have.
method_metrics <- function(reps, truth, scenario, method, n_total) {
  rows <- list()
  for (q in quantities) for (pop in c("index", "comparator")) {
    f <- paste0(q, "_", pop)
    get <- function(suffix) {
      vapply(reps, function(x) {
        if (is.null(x) || !isTRUE(x$converged)) return(NA_real_)
        v <- x[[paste0(f, suffix)]]
        if (is.null(v)) NA_real_ else as.numeric(v)
      }, numeric(1))
    }
    est <- get("")
    lo <- get("_lower")
    hi <- get("_upper")
    tr <- truth[[f]]
    ok <- !is.na(est)
    n_valid <- sum(ok)
    m <- list(bias = NA, abs_bias = NA, rel_bias_pct = NA, emp_se = NA, rmse = NA, coverage = NA, ci_width = NA,
              mcse_bias = NA, mcse_emp_se = NA, mcse_coverage = NA, mcse_rel_bias_pct = NA)
    if (n_valid > 0) {
      err <- est[ok] - tr
      m$bias <- mean(err)
      m$abs_bias <- abs(m$bias)
      m$emp_se <- sd(est[ok])
      m$rmse <- sqrt(mean(err^2))
      ci <- ok & !is.na(lo) & !is.na(hi)
      if (any(ci)) {
        cov <- mean(tr >= lo[ci] & tr <= hi[ci])
        m$coverage <- 100 * cov
        m$ci_width <- mean(hi[ci] - lo[ci])
        m$mcse_coverage <- 100 * sqrt(cov * (1 - cov) / sum(ci))
      }
      if (abs(tr) > 1e-4) {
        m$rel_bias_pct <- 100 * m$bias / abs(tr)
        rb <- err / abs(tr)
        m$mcse_rel_bias_pct <- 100 * sqrt(sum((rb - mean(rb))^2) / (n_valid * (n_valid - 1)))
      }
      m$mcse_bias <- sqrt(sum((err - m$bias)^2) / (n_valid * (n_valid - 1)))
      m$mcse_emp_se <- m$emp_se / sqrt(2 * (n_valid - 1))
    }
    keys <- c("scenario_id", "scenario_name", "sample_size", "effect_modification", "population_imbalance",
              "covariate_correlation_index", "covariate_correlation_comparator", "covariate_type")
    rows[[length(rows) + 1]] <- data.frame(
      scenario[, keys],
      quantity = q, model = method, population = pop, n_valid = n_valid, n_total = n_total,
      convergence_pct = 100 * n_valid / n_total, as.data.frame(m), row.names = NULL
    )
  }
  do.call(rbind, rows)
}
