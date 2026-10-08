# Data generation and true effects of the simulation study: each replicate is
# the dataset the published results were computed from.

#' Outcome model coefficients: logit P(Y = 1 | X, k) = alpha_k + X' beta_k.
#' Effect modification changes the X2 coefficient under treatment B; X1 is a
#' prognostic factor only.
get_beta_params <- function(em) {
  list(
    alpha_A = 1.0,
    alpha_B = 0.25,
    beta_A = c(-1.0, -2.0),
    beta_B = c(-1.0, switch(em, none = -2.00, weak = -1.75, strong = -1.00))
  )
}

#' P(X_j = 1) in each population: index 0.6 ("low", the paper's moderate
#' imbalance) or 0.8 ("high"); comparator 0.4 or 0.2. These are the values the
#' published results were computed with.
get_imbalance_params <- function(level) {
  switch(level,
    low = list(prob_index = 0.6, prob_comparator = 0.4),
    high = list(prob_index = 0.8, prob_comparator = 0.2)
  )
}

#' Correlated binary covariates, as in the published study: the first n
#' points of an unscrambled Sobol sequence, through a Gaussian copula with
#' correlation sin(pi r / 2), thresholded at `prob`.
sim_covariates <- function(n, p, prob, cor_matrix) {
  copula_cor <- sin(pi * cor_matrix / 2)
  diag(copula_cor) <- 1
  u <- randtoolbox::sobol(n = n, dim = p)
  cop <- copula::normalCopula(copula::P2p(copula_cor), dim = p, dispstr = "un")
  (copula::cCopula(u, copula = cop, inverse = TRUE) < prob) * 1
}

cor_matrix <- function(r, p) {
  m <- matrix(r, p, p)
  diag(m) <- 1
  m
}

#' Comparator AgD for the subgroups formed by the covariates in `by` (column
#' indices; relaxed SPFA): the four X1 by X2 cells, or two cells when a
#' covariate is omitted from the analysis.
subgroup_agd <- function(X, y, by = 1:2) {
  group <- 1 + as.vector((X[, by, drop = FALSE] > 0.5) %*% 2^(seq_along(by) - 1))
  do.call(rbind, lapply(sort(unique(group)), function(g) {
    i <- which(group == g)
    data.frame(study = paste0("Comparator_", g), treatment = "B",
               n_total = length(i), n_events = sum(y[i]),
               x1_mean = mean(X[i, 1]), x2_mean = mean(X[i, 2]))
  }))
}

#' One replicate: index IPD, comparator AgD overall (for SPFA, STC, MAIC and
#' the naive comparison) and by subgroup (for relaxed SPFA). Both AgD forms
#' come from the same comparator sample. `config$omit` changes only the
#' subgroups, never the data.
generate_rep_data <- function(scenario, rep_id, config) {
  seed <- config$base_seed + scenario$scenario_id * 10000 + rep_id
  bp <- get_beta_params(scenario$effect_modification)
  ip <- get_imbalance_params(scenario$population_imbalance)
  n <- scenario$sample_size
  p <- scenario$n_covariates

  set.seed(seed)
  X_index <- sim_covariates(n, p, ip$prob_index, cor_matrix(scenario$covariate_correlation_index, p))
  X_comp <- sim_covariates(n, p, ip$prob_comparator, cor_matrix(scenario$covariate_correlation_comparator, p))
  y_index <- rbinom(n, 1, plogis(bp$alpha_A + X_index %*% bp$beta_A))
  y_comp <- rbinom(n, 1, plogis(bp$alpha_B + X_comp %*% bp$beta_B))

  ipd <- data.frame(study = "Index", treatment = "A", outcome = y_index,
                    x1 = X_index[, 1], x2 = X_index[, 2])
  agd <- data.frame(study = "Comparator", treatment = "B", n_total = n, n_events = sum(y_comp),
                    x1_mean = mean(X_comp[, 1]), x2_mean = mean(X_comp[, 2]))
  by <- match(setdiff(c("x1", "x2"), config$omit), c("x1", "x2"))
  list(seed = seed, ipd = ipd, agd = agd, agd_subgroups = subgroup_agd(X_comp, y_comp, by),
       X_index = X_index, X_comparator = X_comp)
}

#' True marginal effects in each population, on populations of `truth_n`
#' covariate vectors from the same generator. The truths do not depend on
#' which covariates an analysis adjusts for.
compute_truth <- function(scenario, config) {
  bp <- get_beta_params(scenario$effect_modification)
  ip <- get_imbalance_params(scenario$population_imbalance)
  p <- scenario$n_covariates
  X_i <- sim_covariates(config$truth_n, p, ip$prob_index, cor_matrix(scenario$covariate_correlation_index, p))
  X_c <- sim_covariates(config$truth_n, p, ip$prob_comparator, cor_matrix(scenario$covariate_correlation_comparator, p))
  truth_from_probs(
    p_A_index = mean(plogis(bp$alpha_A + X_i %*% bp$beta_A)),
    p_B_index = mean(plogis(bp$alpha_B + X_i %*% bp$beta_B)),
    p_A_comparator = mean(plogis(bp$alpha_A + X_c %*% bp$beta_A)),
    p_B_comparator = mean(plogis(bp$alpha_B + X_c %*% bp$beta_B))
  )
}

truth_from_probs <- function(p_A_index, p_B_index, p_A_comparator, p_B_comparator) {
  list(
    p_A_index = p_A_index, p_B_index = p_B_index,
    p_A_comparator = p_A_comparator, p_B_comparator = p_B_comparator,
    lor_index = qlogis(p_A_index) - qlogis(p_B_index),
    lor_comparator = qlogis(p_A_comparator) - qlogis(p_B_comparator),
    rd_index = p_A_index - p_B_index,
    rd_comparator = p_A_comparator - p_B_comparator,
    log_rr_index = log(p_A_index) - log(p_B_index),
    log_rr_comparator = log(p_A_comparator) - log(p_B_comparator)
  )
}
