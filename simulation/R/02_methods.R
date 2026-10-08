# The five methods of the study: ML-UMR under SPFA and with SPFA relaxed
# (fitted with mlumr), and the naive, STC and MAIC benchmarks, as in the
# published study.
#
# Every method returns a named list with, for each quantity q in lor, p_A,
# p_B, rd, log_rr and population P in index, comparator: the estimate
# `q_P`, its standard error or posterior SD `q_P_sd`, and the 95% interval
# `q_P_lower`, `q_P_upper`; plus `converged`.

z975 <- qnorm(0.975)

#' Wald interval fields for one quantity.
wald <- function(name, est, se) {
  stats::setNames(list(est, se, est - z975 * se, est + z975 * se),
                  paste0(name, c("", "_sd", "_lower", "_upper")))
}

# ---------------------------------------------------------------- ML-UMR

#' An mlumr network with integration points: `agd` is the overall row (SPFA)
#' or the subgroup rows (relaxed). The comparator's covariate correlation is
#' taken from the IPD. `covs` are the covariates the analysis adjusts for
#' (both, unless a prognostic factor is omitted).
build_network <- function(ipd, agd, n_int, covs = c("x1", "x2")) {
  net <- mlumr::combine_data(
    mlumr::set_ipd(ipd, treatment = "treatment", outcome = "outcome",
                   covariates = covs, family = "binomial"),
    mlumr::set_agd(agd, treatment = "treatment", family = "binomial",
                   outcome_n = "n_total", outcome_r = "n_events",
                   cov_means = paste0(covs, "_mean"), cov_types = rep("binary", length(covs)))
  )
  dists <- lapply(covs, function(v) eval(bquote(mlumr::distr(mlumr::qbern, prob = .(as.name(paste0(v, "_mean")))))))
  do.call(mlumr::add_integration, c(list(net, n_int = n_int, verbose = FALSE), stats::setNames(dists, covs)))
}

#' Fit ML-UMR with the paper's settings. `center = FALSE` keeps the intercept
#' prior at the uncentered covariates, as in the published study. Returns the
#' summaries, or `converged = FALSE` with the error if the fit failed.
fit_mlumr <- function(network, model, seed, config) {
  warn <- character()
  fit <- tryCatch(
    withCallingHandlers(
      mlumr::mlumr(
        network, model = model,
        prior_intercept = mlumr::prior_normal(0, config$prior_sd),
        prior_beta = mlumr::prior_normal(0, config$prior_sd),
        chains = config$chains, iter = config$iter, warmup = config$warmup,
        seed = seed, adapt_delta = config$adapt_delta,
        max_treedepth = config$max_treedepth, refresh = 0,
        verbose = FALSE, center = FALSE, engine = "cmdstanr"
      ),
      warning = function(w) {
        warn <<- c(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(list(message = conditionMessage(e)), class = "fit_error")
  )
  if (inherits(fit, "fit_error")) return(list(converged = FALSE, error = fit$message))
  out <- summarize_fit(fit)
  out$warnings <- unique(warn)
  drop_cmdstan_output(fit)
  out
}

#' The paper's summaries of one fit: posterior mean, SD and 2.5% and 97.5%
#' quantiles of each quantity; convergence is max Rhat < 1.1.
summarize_fit <- function(fit) {
  d <- fit$draws
  draws <- list(
    lor_index = d$lor_index, lor_comparator = d$lor_comparator,
    p_A_index = d$p_index_index, p_B_index = d$p_comparator_index,
    p_A_comparator = d$p_index_comparator, p_B_comparator = d$p_comparator_comparator,
    rd_index = d$rd_index, rd_comparator = d$rd_comparator,
    log_rr_index = log(d$rr_index), log_rr_comparator = log(d$rr_comparator)
  )
  out <- list()
  for (k in names(draws)) {
    x <- as.vector(draws[[k]])
    out[[k]] <- mean(x, na.rm = TRUE)
    out[[paste0(k, "_sd")]] <- sd(x, na.rm = TRUE)
    out[[paste0(k, "_lower")]] <- quantile(x, 0.025, na.rm = TRUE, names = FALSE)
    out[[paste0(k, "_upper")]] <- quantile(x, 0.975, na.rm = TRUE, names = FALSE)
  }
  out$max_rhat <- max(fit$summary$Rhat, na.rm = TRUE)
  out$min_ess_bulk <- min(fit$summary$n_eff, na.rm = TRUE)
  out$converged <- out$max_rhat < 1.1
  out
}

#' CmdStan writes one draws CSV per chain into the session's temporary
#' directory; delete this fit's files so a long run does not fill the disk.
drop_cmdstan_output <- function(fit) {
  sf <- fit$stanfit
  if (!inherits(sf, "CmdStanFit")) return(invisible(NULL))
  f <- tryCatch(sf$output_files(), error = function(e) character(0))
  unlink(f[file.exists(f)])
  invisible(NULL)
}

# ---------------------------------------------------------------- benchmarks

#' Naive comparison of the observed proportions; the same estimates serve
#' both populations.
naive_estimate <- function(ipd, agd) {
  n_A <- nrow(ipd)
  n_B <- sum(agd$n_total)
  p_A <- mean(ipd$outcome)
  p_B <- sum(agd$n_events) / n_B
  p_A <- pmin(pmax(p_A, 0.5 / n_A), 1 - 0.5 / n_A)
  p_B <- pmin(pmax(p_B, 0.5 / n_B), 1 - 0.5 / n_B)
  q <- c(
    wald("lor", qlogis(p_A) - qlogis(p_B), sqrt(1 / (n_A * p_A * (1 - p_A)) + 1 / (n_B * p_B * (1 - p_B)))),
    wald("p_A", p_A, sqrt(p_A * (1 - p_A) / n_A)),
    wald("p_B", p_B, sqrt(p_B * (1 - p_B) / n_B)),
    wald("rd", p_A - p_B, sqrt(p_A * (1 - p_A) / n_A + p_B * (1 - p_B) / n_B)),
    wald("log_rr", log(p_A) - log(p_B), sqrt((1 - p_A) / (n_A * p_A) + (1 - p_B) / (n_B * p_B)))
  )
  c(both_populations(q), converged = TRUE)
}

#' Copy population-free fields (`lor`, `lor_sd`, ...) to `lor_index`,
#' `lor_comparator`, ...
both_populations <- function(q, populations = c("index", "comparator")) {
  out <- list()
  for (nm in names(q)) {
    stem <- sub("(_sd|_lower|_upper)$", "", nm)
    suffix <- substr(nm, nchar(stem) + 1, nchar(nm))
    for (pop in populations) out[[paste0(stem, "_", pop, suffix)]] <- q[[nm]]
  }
  out
}

#' STC by g-computation: logistic regression on the IPD, averaged over the
#' comparator's integration points, delta-method variance. The relative
#' effects found in the comparator population are applied to the index
#' population unchanged, as in the paper.
stc_estimate <- function(ipd, integration_points, agd, covs = c("x1", "x2")) {
  fit <- glm(reformulate(covs, "outcome"), family = binomial, data = ipd)
  b <- coef(fit)
  V <- vcov(fit)
  X_int <- do.call(rbind, lapply(seq_len(dim(integration_points)[1]), function(k) {
    matrix(integration_points[k, , ], ncol = dim(integration_points)[3])
  }))
  D <- cbind(1, X_int)
  p_int <- plogis(as.vector(D %*% b))
  n_int <- nrow(D)
  p_A <- pmin(pmax(mean(p_int), 0.5 / n_int), 1 - 0.5 / n_int)
  n_B <- sum(agd$n_total)
  p_B <- pmin(pmax(sum(agd$n_events) / n_B, 0.5 / n_B), 1 - 0.5 / n_B)

  m <- mean(p_int)
  grad_m <- colMeans(p_int * (1 - p_int) * D)
  var_lor_A <- as.numeric(t(grad_m / (m * (1 - m))) %*% V %*% (grad_m / (m * (1 - m))))
  var_p_A <- as.numeric(t(grad_m) %*% V %*% grad_m)
  var_p_B <- p_B * (1 - p_B) / n_B
  log_rr <- log(p_A) - log(p_B)
  var_log_rr <- var_p_A / p_A^2 + (1 - p_B) / (n_B * p_B)

  rel <- c(
    wald("lor", qlogis(p_A) - qlogis(p_B), sqrt(var_lor_A + 1 / (n_B * p_B * (1 - p_B)))),
    wald("rd", p_A - p_B, sqrt(var_p_A + var_p_B)),
    wald("log_rr", log_rr, sqrt(var_log_rr))
  )
  idx <- index_probabilities(ipd, b, V, log_rr, var_log_rr, covs)
  c(both_populations(rel),
    wald("p_A_comparator", p_A, sqrt(var_p_A)),
    wald("p_B_comparator", p_B, sqrt(var_p_B)),
    idx, converged = TRUE)
}

#' Index population event probabilities for STC and MAIC: p_A by
#' g-computation over the IPD with coefficients `b` (variance `V`), p_B from
#' p_A and the transported log risk ratio.
index_probabilities <- function(ipd, b, V, log_rr, var_log_rr, covs = c("x1", "x2")) {
  n <- nrow(ipd)
  D <- cbind(1, as.matrix(ipd[, covs]))
  p <- plogis(as.vector(D %*% b))
  p_A <- pmin(pmax(mean(p), 0.5 / n), 1 - 0.5 / n)
  p_B <- pmin(pmax(p_A / exp(log_rr), 0.5 / n), 1 - 0.5 / n)
  g <- colMeans(p * (1 - p) * D)
  var_p_A <- as.numeric(t(g) %*% V %*% g)
  var_p_B <- (1 / exp(log_rr))^2 * var_p_A + p_B^2 * var_log_rr
  c(wald("p_A_index", p_A, sqrt(var_p_A)), wald("p_B_index", p_B, sqrt(var_p_B)))
}

#' MAIC: method-of-moments (entropy balancing) weights matching the
#' comparator's covariate means, HC3 sandwich variance; relative effects
#' transported to the index population unchanged.
maic_estimate <- function(ipd, agd, covs = c("x1", "x2")) {
  means <- vapply(covs, function(v) weighted.mean(agd[[paste0(v, "_mean")]], agd$n_total), 1)
  X <- as.matrix(ipd[, covs])
  Xc <- sweep(X, 2, means)
  opt <- optim(rep(0, length(covs)), fn = function(a) sum(exp(Xc %*% a)),
               gr = function(a) as.vector(crossprod(Xc, exp(Xc %*% a))),
               method = "BFGS", control = list(maxit = 1000))
  w <- as.vector(exp(Xc %*% opt$par))
  n <- nrow(ipd)
  ipd$w <- w / sum(w) * n
  fit <- suppressWarnings(glm(outcome ~ 1, family = binomial, weights = w, data = ipd))
  p_A <- pmin(pmax(plogis(coef(fit)[[1]]), 0.5 / n), 1 - 0.5 / n)
  se_logit_A <- sqrt(sandwich::vcovHC(fit, type = "HC3")[1, 1])
  n_B <- sum(agd$n_total)
  p_B <- pmin(pmax(sum(agd$n_events) / n_B, 0.5 / n_B), 1 - 0.5 / n_B)

  var_p_A <- (p_A * (1 - p_A))^2 * se_logit_A^2
  var_p_B <- p_B * (1 - p_B) / n_B
  log_rr <- log(p_A) - log(p_B)
  var_log_rr <- var_p_A / p_A^2 + (1 - p_B) / (n_B * p_B)
  rel <- c(
    wald("lor", qlogis(p_A) - qlogis(p_B), sqrt(se_logit_A^2 + 1 / (n_B * p_B * (1 - p_B)))),
    wald("rd", p_A - p_B, sqrt(var_p_A + var_p_B)),
    wald("log_rr", log_rr, sqrt(var_log_rr))
  )
  full <- suppressWarnings(glm(reformulate(covs, "outcome"), family = binomial, weights = w, data = ipd))
  idx <- index_probabilities(ipd, coef(full), sandwich::vcovHC(full, type = "HC3"), log_rr, var_log_rr, covs)
  c(both_populations(rel),
    wald("p_A_comparator", p_A, sqrt(var_p_A)),
    wald("p_B_comparator", p_B, sqrt(var_p_B)),
    idx, converged = opt$convergence == 0, ess = sum(w)^2 / sum(w^2))
}

#' All three benchmarks for one replicate. STC uses the SPFA network's
#' integration points (the comparator's covariates are not observed).
run_benchmarks <- function(d, config) {
  covs <- analysis_covariates(config)
  net <- build_network(d$ipd, d$agd, config$n_int_spfa, covs)
  list(naive = naive_estimate(d$ipd, d$agd),
       stc = stc_estimate(d$ipd, net$integration_points, d$agd, covs),
       maic = maic_estimate(d$ipd, d$agd, covs))
}

#' The covariates the analyses adjust for: both, less any omitted.
analysis_covariates <- function(config) setdiff(c("x1", "x2"), config$omit)
