# Simulation study behind the "Simulation evidence" section of the
# subgroup-identification vignette.
#
# It answers two questions the vignette raises but cannot settle structurally:
#
#   Q1. Does the mean-profile geometry of the aggregate rows predict the
#       precision of the index-population effect better than the row count?
#   Q2. Do one-variable-at-a-time subgroups and bare categorical cross-tabs
#       fail in the same way or in different ways?
#
# The vignette's own list of what a proper study owes is the specification for
# this one: reconstruct the actual within-stratum distributions, use the
# correct exposure-weighted target for rates, report failures and Monte Carlo
# standard errors, and use enough replications for the claimed precision.
#
# DESIGN CONTROL. The comparator is ONE population of fixed total size, split
# across the rows of whichever design is in play, so a design with more rows
# has proportionally smaller rows. Without this control "more rows is better"
# would hold by construction and the comparison would be vacuous.
#
# WITHIN-STRATUM DISTRIBUTIONS. The comparator is generated at the individual
# level inside each stratum and then aggregated. Each aggregate row reports the
# REALIZED means and standard deviations of its own members, as a real report
# would, rather than a stratum mean pushed through the link.
#
# ESTIMAND. Every family reports the INDEX minus the COMPARATOR: the Stan
# models define delta_index = y_index_index - y_comparator_index, and lor_index
# likewise. The comparison scale is the mean difference (normal), the marginal
# log odds ratio (binomial), and the log marginal rate ratio (poisson, whose
# delta_index is a natural ratio and is logged here so that an interval width
# means the same thing across the three families).
#
# REPLICATIONS. Chosen so the Monte Carlo standard error of a cell's mean log
# interval width is at most 0.05, and of a cell's coverage at most 0.02. A
# pilot of 10 replications per cell put the largest within-cell SD of log width
# at 0.82, which needs 268 replications; 300 is used. Widths span an order of
# magnitude across designs, so the log scale is the one on which a relative
# precision target means anything. The achieved standard errors are reported
# per cell rather than assumed.
#
# Run from the package root (about six hours on six cores):
#   Rscript data-raw/simulate_subgroup_identification.R
#
# It writes data-raw/subgroup_identification_results.csv, one row per fit, and
# checkpoints each cell so an interrupted run resumes where it stopped.

suppressMessages(library(mlumr))
options(mlumr.quiet_relaxed_index = TRUE)

# Engine choice is not cosmetic here. The same fit takes about 4 seconds through
# cmdstanr and about 27 through the rstan backend on this machine, which is the
# difference between a study that runs overnight and one that does not finish in
# a working week. Fall back rather than stop, but say so.
if (requireNamespace("cmdstanr", quietly = TRUE) &&
      !inherits(try(cmdstanr::cmdstan_path(), silent = TRUE), "try-error")) {
  mlumr_engine("cmdstanr")
} else {
  warning("cmdstanr is not available, so this runs on the rstan backend. ",
          "Expect roughly six times the runtime.", call. = FALSE)
}

BETA_A <- c(0.40, 0.30, -0.20)      # index coefficients (age, sex, prior)
BETA_B <- c(0.25, 0.45, -0.35)      # comparator coefficients
MU_A   <- 0.20
MU_B   <- 0.55
N_IPD  <- 200L
N_CMP  <- 600L                      # total comparator size, split across rows
SIGMA  <- 1.0                       # normal residual SD
EXPO   <- 1.5                       # poisson person-time per subject
N_INT  <- 128L                      # package heuristic for 3 covariates
K      <- 3L
REF_SD <- c(age = 1, sex = 0.5, prior = 0.49)
COVS   <- c("age", "sex", "prior")
LINK   <- list(normal = "identity", binomial = "logit", poisson = "log")

DESIGNS <- list(
  ovat_age_6 = cbind(age = seq(-1.5, 1.5, length.out = 6), sex = 0.5, prior = 0.4),
  ovat_age_3 = cbind(age = seq(-1.2, 1.2, length.out = 3), sex = 0.5, prior = 0.4),
  crosstab_4 = cbind(age = 0, sex = c(0, 1, 0, 1), prior = c(0, 0, 1, 1)),
  minimal_4  = cbind(age = c(-1, 1, 0, 0), sex = c(0, 0, 1, 0), prior = c(0, 0, 0, 1)),
  joint_6    = cbind(age = c(-1, -1, -1, -1, 1, 1), sex = c(0, 1, 0, 1, 0, 1),
                     prior = c(0, 0, 1, 1, 0, 1)),
  joint_8    = cbind(age = rep(c(-1, 1), each = 4), sex = rep(c(0, 1, 0, 1), 2),
                     prior = rep(c(0, 0, 1, 1), 2))
)

geometry <- function(M) {
  Ms <- sweep(scale(as.matrix(M), center = TRUE, scale = FALSE), 2, REF_SD, "/")
  d <- svd(Ms)$d
  if (length(d) < K) d <- c(d, rep(0, K - length(d)))
  c(cond_inv = min(d) / max(d), eff_dim = sum(d^2)^2 / sum(d^4))
}

draw_covariates <- function(n, m, age_sd = 1) {
  cbind(age = rnorm(n, m[1], age_sd),
        sex = rbinom(n, 1, m[2]),
        prior = rbinom(n, 1, m[3]))
}

linpred <- function(X, mu, beta) as.numeric(mu + X %*% beta)

# Truth on the scale the package reports for each family, evaluated over the
# index covariate distribution by large-sample Monte Carlo. Poisson is on the
# log rate-ratio scale so that an interval width means the same thing as it does
# for the log odds ratio and the mean difference.
true_effect <- function(family, X_index) {
  ea <- linpred(X_index, MU_A, BETA_A)
  eb <- linpred(X_index, MU_B, BETA_B)
  # Every family reports the INDEX minus the COMPARATOR: the Stan models define
  # delta_index = y_index_index - y_comparator_index and lor_index likewise.
  switch(family,
    normal   = mean(ea) - mean(eb),
    binomial = { pa <- mean(plogis(ea)); pb <- mean(plogis(eb))
                 qlogis(pa) - qlogis(pb) },
    poisson  = log(mean(exp(ea))) - log(mean(exp(eb))))
}

# The estimand is a property of the index population, not of a replication, so
# it is evaluated ONCE on a large fixed draw with its own seed. Recomputing it
# inside each replication gave the target its own Monte Carlo error, which then
# varied from cell to cell: bias and coverage were being measured against a
# moving reference rather than a constant.
TRUTH <- local({
  set.seed(20260L)
  X <- draw_covariates(2000000L, c(0, 0.5, 0.4))
  stats::setNames(lapply(c("normal", "binomial", "poisson"),
                         function(f) true_effect(f, X)),
                  c("normal", "binomial", "poisson"))
})

# ---- one replication ---------------------------------------------------------
#' Remove one fit's CmdStan draws files.
#'
#' `native_fit` is the cmdstanr fit the backend keeps; `output_files()` names the
#' CSVs it wrote. The rstan backend has neither, and returns without doing
#' anything. The draws are already in memory by the time this runs, so the
#' summaries extracted below are unaffected.
.drop_cmdstan_output <- function(fit) {
  nf <- tryCatch(fit$native_fit, error = function(e) NULL)
  if (is.null(nf) || !is.function(nf$output_files)) return(invisible(NULL))
  f <- tryCatch(nf$output_files(), error = function(e) character(0))
  unlink(f[file.exists(f)])
  invisible(NULL)
}

run_one <- function(family, design_name, rep_id, seed) {
  set.seed(seed)
  M <- DESIGNS[[design_name]]
  S <- nrow(M)
  n_s <- N_CMP %/% S

  # Index study.
  Xi <- draw_covariates(N_IPD, c(0, 0.5, 0.4))
  eta_i <- linpred(Xi, MU_A, BETA_A)
  ipd_df <- data.frame(trt = "A", age = Xi[, 1], sex = Xi[, 2], prior = Xi[, 3])
  ipd_df$y <- switch(family,
    normal   = rnorm(N_IPD, eta_i, SIGMA),
    binomial = rbinom(N_IPD, 1, plogis(eta_i)),
    poisson  = rpois(N_IPD, EXPO * exp(eta_i)))
  if (family == "poisson") ipd_df$expo <- EXPO

  # Comparator strata, individual level, then aggregated.
  rows <- lapply(seq_len(S), function(s) {
    Xs <- draw_covariates(n_s, M[s, ])
    eta <- linpred(Xs, MU_B, BETA_B)
    out <- list(age_mean = mean(Xs[, 1]), age_sd = stats::sd(Xs[, 1]),
                sex_mean = mean(Xs[, 2]), prior_mean = mean(Xs[, 3]))
    if (family == "normal") {
      y <- rnorm(n_s, eta, SIGMA)
      out$ybar <- mean(y); out$yse <- stats::sd(y) / sqrt(n_s); out$nn <- n_s
    } else if (family == "binomial") {
      y <- rbinom(n_s, 1, plogis(eta))
      out$r <- sum(y); out$nn <- n_s
    } else {
      y <- rpois(n_s, EXPO * exp(eta))
      out$r <- sum(y); out$EE <- n_s * EXPO
    }
    as.data.frame(out)
  })
  agd_df <- do.call(rbind, rows)
  agd_df$trt <- "B"

  # A stratum defined as, say, all-male has a realized proportion of exactly 0,
  # and that is the honest summary: nudging it off the boundary would make the
  # declared moments disagree with the integration grid, which the package then
  # reports as a contradiction. Record degenerate margins instead of hiding them.
  degenerate <- sum(agd_df$sex_mean %in% c(0, 1)) +
    sum(agd_df$prior_mean %in% c(0, 1))

  ipd <- set_ipd(ipd_df, treatment = "trt", outcome = "y", covariates = COVS,
                 family = family,
                 exposure = if (family == "poisson") "expo" else NULL)
  agd <- switch(family,
    normal = set_agd(agd_df, treatment = "trt", family = family,
                     outcome_mean = "ybar", outcome_se = "yse", outcome_n = "nn",
                     cov_means = c("age_mean", "sex_mean", "prior_mean"),
                     cov_sds = c("age_sd", NA, NA),
                     cov_types = c("continuous", "binary", "binary")),
    binomial = set_agd(agd_df, treatment = "trt", family = family,
                       outcome_r = "r", outcome_n = "nn",
                       cov_means = c("age_mean", "sex_mean", "prior_mean"),
                       cov_sds = c("age_sd", NA, NA),
                       cov_types = c("continuous", "binary", "binary")),
    poisson = set_agd(agd_df, treatment = "trt", family = family,
                      outcome_r = "r", outcome_E = "EE",
                      cov_means = c("age_mean", "sex_mean", "prior_mean"),
                      cov_sds = c("age_sd", NA, NA),
                      cov_types = c("continuous", "binary", "binary")))

  dat <- combine_data(ipd, agd)
  dat <- add_integration(dat,
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    sex = distr(qbern, prob = sex_mean),
    prior = distr(qbern, prob = prior_mean), n_int = N_INT)

  # The diagnostic runs on the family's own link. Only the identity link makes
  # it an exact statement about the design matrix; for logit and log the package
  # reports it as descriptive, a summary of the spread of the declared means.
  # The geometry itself is link-free, which is why it is computed separately and
  # is the quantity the study tests as a predictor.
  ci <- check_identification(dat, link = LINK[[family]], verbose = FALSE)
  g <- geometry(M)

  truth <- TRUTH[[family]]

  fit <- try(suppressWarnings(mlumr(dat, model = "relaxed", chains = 2,
                                    iter = 3000, warmup = 1500, seed = seed,
                                    refresh = 0)), silent = TRUE)
  base <- data.frame(
    family = family, design = design_name, rep = rep_id, seed = seed,
    n_rows = S, n_per_row = n_s,
    cond_inv = unname(g["cond_inv"]), eff_dim = unname(g["eff_dim"]),
    pkg_cond_inv = ci$cond_inv, flagged = isTRUE(ci$flagged),
    scope = ci$diagnostic_scope,
    degenerate_margins = degenerate, truth = truth, stringsAsFactors = FALSE)
  if (inherits(fit, "try-error")) {
    return(cbind(base, ok = FALSE, est = NA_real_, lo = NA_real_, hi = NA_real_,
                 width = NA_real_, covered = NA, max_rhat = NA_real_,
                 min_ess = NA_real_))
  }

  # CmdStan writes one draws CSV per chain into the session temp directory and
  # nothing removes them: a forked worker exits without running R's tempdir
  # finalizer, so they accumulate for the whole run. At roughly 7 MB per chain
  # this study produced 51 GB and filled the disk partway through. Delete this
  # fit's own files by name, taken from the fit object, so a sibling worker's
  # live chains in the shared temp directory are never touched.
  on.exit(.drop_cmdstan_output(fit), add = TRUE)

  me <- marginal_effects(fit, population = "index")
  var_name <- if (family == "binomial") "lor_index" else "delta_index"
  r <- me[me$variable == var_name, ][1, ]
  on_log <- family == "poisson"          # delta_index is a natural rate ratio
  est <- if (on_log) log(r$mean) else r$mean
  lo  <- if (on_log) log(r$q2.5) else r$q2.5
  hi  <- if (on_log) log(r$q97.5) else r$q97.5

  # Convergence is judged on the model parameters and the reported effect, not
  # on the hundreds of pointwise log-likelihood entries, which would drown them.
  s <- fit$summary
  key <- grepl("^(mu|beta|sigma|delta_|lor_|rd_|rr_)", s$variable)
  cbind(base, ok = TRUE, est = est, lo = lo, hi = hi, width = hi - lo,
        covered = truth >= lo && truth <= hi,
        max_rhat = suppressWarnings(max(s$Rhat[key], na.rm = TRUE)),
        min_ess = suppressWarnings(min(s$n_eff[key], na.rm = TRUE)))
}

# ---- run the study -----------------------------------------------------------

N_REP <- 300L
OUT_CSV <- file.path("data-raw", "subgroup_identification_results.csv")
CELLS <- paste0(OUT_CSV, ".cells")
dir.create(CELLS, showWarnings = FALSE, recursive = TRUE)

grid <- expand.grid(family = c("normal", "binomial", "poisson"),
                    design = names(DESIGNS), rep = seq_len(N_REP),
                    stringsAsFactors = FALSE)
# One seed per cell, derived from a single root, so the whole study is
# reproducible from that root alone.
grid$seed <- 2026L + seq_len(nrow(grid)) * 7L

cores <- max(1L, min(parallel::detectCores() - 2L, 6L))
cat("cells:", nrow(grid), " cores:", cores, "\n")
utils::flush.console()

t0 <- Sys.time()
invisible(parallel::mclapply(seq_len(nrow(grid)), function(i) {
  # Each cell writes its own file the moment it finishes, so a worker that dies
  # costs one cell rather than the run, and a rerun skips what is already done.
  f <- file.path(CELLS, sprintf("%05d.rds", i))
  if (file.exists(f)) return(NULL)
  r <- try(suppressWarnings(suppressMessages(
    run_one(grid$family[i], grid$design[i], grid$rep[i], grid$seed[i])
  )), silent = TRUE)
  if (!inherits(r, "try-error")) {
    # Write then rename, so a worker killed mid-write leaves no truncated file
    # to be mistaken for a finished cell and to break the assembly below.
    tmp <- paste0(f, ".part")
    saveRDS(r, tmp)
    file.rename(tmp, f)
  }
  NULL
}, mc.cores = cores, mc.preschedule = FALSE))

files <- list.files(CELLS, pattern = "\\.rds$", full.names = TRUE)
res <- do.call(rbind, lapply(files, readRDS))
write.csv(res, OUT_CSV, row.names = FALSE)
cat("completed:", length(files), "of", nrow(grid),
    " minutes:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    "\nwrote", OUT_CSV, "\n")
