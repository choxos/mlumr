# Figures in the layout of the paper's Figs. 1 to 4: bias and coverage of the
# log odds ratio and log risk ratio by method, effect modification,
# imbalance, population and comparator correlation (n = 150).

method_labels <- c(maic = "MAIC", stc = "STC", spfa = "ML-UMR (SPFA)", relaxed = "ML-UMR (Relaxed)", naive = "Naive")
method_colors <- c("MAIC" = "#e377c2", "STC" = "#1f77b4", "ML-UMR (SPFA)" = "#552c6f",
                   "ML-UMR (Relaxed)" = "#ff0021", "Naive" = "#2ca02c")

label_metrics <- function(m) {
  m <- m[m$model != "naive", , drop = FALSE]
  m$method <- factor(method_labels[m$model], levels = method_labels[c("maic", "stc", "spfa", "relaxed")])
  m$em <- factor(m$effect_modification, c("none", "weak", "strong"), c("None", "Weak", "Strong"))
  m$x <- as.numeric(m$em) + (as.numeric(m$method) - 2.5) * 0.17
  m$imbalance <- factor(m$population_imbalance, c("low", "high"), c("Moderate imbalance", "High imbalance"))
  m$panel <- factor(paste0(ifelse(m$population == "index", "Index", "Comparator"), " population\n",
                           ifelse(m$covariate_correlation_comparator == 0.5, "Common (0.50)",
                                  "Misspecified (0.50 vs. 0.25)")),
                    levels = c("Index population\nCommon (0.50)", "Index population\nMisspecified (0.50 vs. 0.25)",
                               "Comparator population\nCommon (0.50)",
                               "Comparator population\nMisspecified (0.50 vs. 0.25)"))
  m
}

#' @param metrics method_metrics() rows.
#' @param qty "lor" or "log_rr".
#' @param what "bias" or "coverage".
#' @param n_rep Replicates per scenario (for the coverage band).
plot_metric <- function(metrics, qty, what, n_rep = 500) {
  m <- label_metrics(metrics[metrics$quantity == qty, ])
  mc <- if (what == "bias") m$mcse_bias else m$mcse_coverage
  m$lo <- m[[what]] - 1.96 * mc
  m$hi <- m[[what]] + 1.96 * mc
  ref <- if (what == "bias") 0 else 95
  p <- ggplot2::ggplot(m, ggplot2::aes(x, .data[[what]], color = method))
  if (what == "coverage") {
    hw <- 1.96 * sqrt(0.95 * 0.05 / n_rep) * 100
    p <- p + ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 95 - hw, ymax = 95 + hw,
                               fill = "grey80", alpha = 0.3)
  }
  p +
    ggplot2::geom_hline(yintercept = ref, linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.08, linewidth = 0.4) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_grid(imbalance ~ panel) +
    ggplot2::scale_x_continuous(breaks = 1:3, labels = c("None", "Weak", "Strong")) +
    ggplot2::scale_color_manual(values = method_colors, drop = FALSE) +
    ggplot2::labs(x = "Strength of effect modification", y = if (what == "bias") "Bias" else "Coverage (%)",
                  color = "Method", caption = "Bars: 1.96 Monte Carlo SE.") +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey95"), legend.position = "bottom")
}
