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
# DESIGN CONTROL. Every design is a PARTITION of one comparator cohort. Each
# replication draws a single cohort of N_CMP people and then tabulates that
# same cohort six ways, so the designs differ only in how the evidence is cut
# up. Holding the total N fixed is not enough on its own: profiles built
# around separate target means also change the pooled covariate distribution,
# the distance from the index population, and the expected outcome level, and
# a width difference could then come from any of those rather than from the
# geometry. Under a partition all of them are identical by construction.
#
# COMMON RANDOM NUMBERS. Because the six designs share one index study and one
# comparator cohort within a replication, design contrasts are PAIRED. The
# replication-level difference between two designs removes the draw-to-draw
# variation that both share, so a contrast is far better resolved than the
# difference of two independently estimated cell means, and its Monte Carlo
# standard error is computed from the paired differences.
#
# WITHIN-STRATUM DISTRIBUTIONS. The cohort is generated at the individual
# level and each aggregate row reports the REALIZED means and standard
# deviations of the members that fall in it, as a real report would, rather
# than a stratum mean pushed through the link.
#
# ESTIMAND, AND WHAT COVERAGE IS SCORED AGAINST. The Stan models define
# delta_index as a mean over the REALIZED index rows: in the normal model,
# `delta_index = mean(mu_index + X_ipd * beta_index) - mean(mu_comparator +
# X_ipd * beta_comparator)`. That is a property of the index sample in hand,
# not of the superpopulation it came from, so the target moves from one
# replication to the next. The truth is therefore recomputed on each
# replication's own X_ipd, which makes it EXACT for that replication rather
# than noisy: it evaluates the very same average the generated quantity
# estimates. An earlier version scored every interval against one fixed
# superpopulation constant, which measured coverage of a different quantity.
#
# COMPARISON SCALE. The mean difference (normal), the marginal log odds ratio
# (binomial), and the log marginal rate ratio (poisson, whose delta_index is a
# natural ratio and is logged here) so that an interval width means the same
# thing across the three families.
#
# REPLICATIONS. Chosen so the Monte Carlo standard error of a cell's mean log
# interval width is at most 0.05, and of a cell's coverage at most 0.02. A
# pilot of 10 replications per cell put the largest within-cell SD of log width
# at 0.82, which needs 268 replications; 300 is used. Widths span an order of
# magnitude across designs, so the log scale is the one on which a relative
# precision target means anything. The achieved standard errors are reported
# per cell rather than assumed, and the paired design contrasts carry their own.
#
# PLATFORM. Fork parallelism, so Unix only; on Windows it runs serially.
#
# Run from the package root:
#   Rscript data-raw/simulate_subgroup_identification.R
#
# It writes data-raw/subgroup_identification_results.csv, one row per fit, and
# checkpoints each fit so an interrupted run resumes where it stopped.

suppressMessages(library(mlumr))
# Base R gained `%||%` in 4.4.0 and mlumr does not export its own copy, so on
# an older R the diagnostics below would stop with "could not find function".
`%||%` <- function(x, y) if (is.null(x)) y else x
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
# Recorded in CONFIG below, so a resumed run cannot silently continue a cmdstanr
# study on the rstan backend and assemble one CSV out of two fitting procedures.
# The software versions very nearly catch this on their own, because
# `cmdstan_version()` fails exactly when `cmdstan_path()` does, but that is a
# coupling inside another package rather than something this study states.
ENGINE <- getOption("mlumr.stan_engine", "rstan")

BETA_A <- c(0.40, 0.30, -0.20)      # index coefficients (age, sex, prior)
BETA_B <- c(0.25, 0.45, -0.35)      # comparator coefficients
MU_A   <- 0.20
MU_B   <- 0.55
N_IPD  <- 200L
N_CMP  <- 600L                      # ONE comparator cohort, partitioned
SIGMA  <- 1.0                       # normal residual SD
EXPO   <- 1.5                       # poisson person-time per subject
N_INT  <- 128L                      # package heuristic for 3 covariates
K      <- 3L
COVS   <- c("age", "sex", "prior")
LINK   <- list(normal = "identity", binomial = "logit", poisson = "log")

# The two populations differ, which is the setting the method exists for. They
# differ in the same way for every design, because every design partitions the
# same cohort.
POP_INDEX      <- c(age = 0.0, sex = 0.50, prior = 0.40)
POP_COMPARATOR <- c(age = 0.5, sex = 0.45, prior = 0.55)

# Reference scale for the geometry: the index covariate SDs, matching what
# `check_identification()` divides by, so the study's geometry and the
# package's are the same quantity on the same scale.
REF_SD <- c(age = 1, sex = sqrt(0.5 * 0.5), prior = sqrt(0.4 * 0.6))

# Fitting settings. These live here rather than inline in the call because the
# checkpoint guard has to pin them: a cell fitted at iter = 3000 and a cell
# fitted at iter = 6000 are not results from the same study, and nothing
# downstream can tell them apart once they are rows in one CSV.
MODEL  <- "relaxed"
CHAINS <- 4L
ITER   <- 3000L
WARMUP <- 1500L

# ---- the designs, as partitions of one cohort --------------------------------
#
# Each function labels every member of the cohort with the row it is reported
# in. The labels must be exhaustive and mutually exclusive; `check_partition()`
# below enforces that rather than trusting it.
#
# The three "deficient" designs span fewer than three directions because of how
# they cut the cohort, not because a profile was declared constant: an age band
# contains men and women in roughly the population proportion, so its sex mean
# barely moves from band to band.
PARTITIONS <- list(
  # Age bands only. Sex and prior sit near their population means in every
  # band, so the profiles trace one direction.
  ovat_age_3 = function(X) {
    .band(X[, "age"], 3L)
  },
  ovat_age_6 = function(X) {
    .band(X[, "age"], 6L)
  },
  # The bare cross-tab of the two binaries. Age sits near its population mean
  # in every cell, so the profiles span the two binary directions only.
  crosstab_4 = function(X) {
    1L + as.integer(X[, "sex"]) + 2L * as.integer(X[, "prior"])
  },
  # Four rows that still span all three directions: split the men by age and
  # the women by prior treatment.
  minimal_4 = function(X) {
    hi <- as.integer(X[, "age"] >= stats::median(X[, "age"]))
    ifelse(X[, "sex"] == 0, 1L + hi, 3L + as.integer(X[, "prior"]))
  },
  # Six rows spanning all three: the younger half cross-tabulated fully, the
  # older half by sex only.
  joint_6 = function(X) {
    hi <- X[, "age"] >= stats::median(X[, "age"])
    ifelse(!hi,
           1L + as.integer(X[, "sex"]) + 2L * as.integer(X[, "prior"]),
           5L + as.integer(X[, "sex"]))
  },
  # The full three-way cross-tab.
  joint_8 = function(X) {
    hi <- as.integer(X[, "age"] >= stats::median(X[, "age"]))
    1L + hi + 2L * as.integer(X[, "sex"]) + 4L * as.integer(X[, "prior"])
  }
)

# Equal-count bands of a continuous covariate, which is how a report tabulates
# one. Sample quantiles, so the cut points are part of the replication.
.band <- function(x, k) {
  br <- stats::quantile(x, probs = seq(0, 1, length.out = k + 1L), names = FALSE)
  br[1] <- -Inf
  br[length(br)] <- Inf
  as.integer(cut(x, breaks = br, labels = FALSE))
}

# A design is only a fair comparison if it really is a partition. Silent
# violations (an empty row, a dropped member) would reintroduce exactly the
# confound this rewrite exists to remove.
check_partition <- function(lab, n, name) {
  if (anyNA(lab) || length(lab) != n) {
    stop("partition `", name, "` did not label every member", call. = FALSE)
  }
  if (!identical(sort(unique(lab)), seq_len(max(lab)))) {
    stop("partition `", name, "` left an empty row", call. = FALSE)
  }
  invisible(TRUE)
}

geometry <- function(M) {
  Ms <- sweep(scale(as.matrix(M), center = TRUE, scale = FALSE), 2, REF_SD, "/")
  d <- svd(Ms)$d
  if (length(d) < K) d <- c(d, rep(0, K - length(d)))
  c(cond_inv = min(d) / max(d), eff_dim = sum(d^2)^2 / sum(d^4))
}

draw_covariates <- function(n, m) {
  cbind(age = stats::rnorm(n, m[1], 1),
        sex = stats::rbinom(n, 1, m[2]),
        prior = stats::rbinom(n, 1, m[3]))
}

linpred <- function(X, mu, beta) as.numeric(mu + X %*% beta)

# Truth on the scale the package reports, evaluated over the covariate rows
# supplied. Called with the replication's own X_ipd, which is exactly the
# matrix the Stan generated quantities average over.
true_effect <- function(family, X_index) {
  ea <- linpred(X_index, MU_A, BETA_A)
  eb <- linpred(X_index, MU_B, BETA_B)
  switch(family,
    normal   = mean(ea) - mean(eb),
    binomial = { pa <- mean(stats::plogis(ea)); pb <- mean(stats::plogis(eb))
                 stats::qlogis(pa) - stats::qlogis(pb) },
    poisson  = log(mean(exp(ea))) - log(mean(exp(eb))))
}

# The geometry each design has IN EXPECTATION, from partitioning a cohort large
# enough that the realized profiles are the population ones. This is the
# design's own structural property, the thing a reader can know before seeing
# data; `pkg_cond_inv` below records what each replication actually produced.
DESIGN_GEOMETRY <- local({
  set.seed(20260L)
  X <- draw_covariates(2000000L, POP_COMPARATOR)
  out <- lapply(names(PARTITIONS), function(nm) {
    lab <- PARTITIONS[[nm]](X)
    check_partition(lab, nrow(X), nm)
    M <- do.call(rbind, lapply(sort(unique(lab)), function(s) {
      colMeans(X[lab == s, , drop = FALSE])
    }))
    c(geometry(M), n_rows = nrow(M))
  })
  stats::setNames(out, names(PARTITIONS))
})

# ---- one replication ---------------------------------------------------------
#' Remove one fit's CmdStan draws files.
#'
#' The fitted object stores the backend fit in `$stanfit`. It is a `CmdStanFit`
#' under cmdstanr, whose `output_files()` names the per-chain CSVs it wrote, and
#' an S4 `stanfit` under rstan, which writes no such files. `$` on an S4 object
#' is not safe to probe blindly, so dispatch on the class rather than on whether
#' a member happens to look like a function.
#'
#' The draws are already in memory by the time this runs, so every summary
#' extracted below is unaffected.
.drop_cmdstan_output <- function(fit) {
  nf <- fit$stanfit
  if (!inherits(nf, "CmdStanFit")) return(invisible(NULL))
  f <- tryCatch(nf$output_files(), error = function(e) character(0))
  unlink(f[file.exists(f)])
  invisible(NULL)
}

# Every diagnostic the package's own contract names, not just Rhat. A claim
# that non-convergence tracks identification is only as good as the set of
# diagnostics it was checked against, and divergences and treedepth are
# invisible in `fit$summary`.
#
# All of it comes from `fit$diagnostics`, which mlumr fills for BOTH backends,
# rather than from the raw backend object. Reaching into the CmdStanFit instead
# was wrong twice over: it left divergences and treedepth missing under rstan,
# and `num_chains()` there is `num_procs()`, the number of chains STARTED. A run
# that lost a chain still reported the full count, so the one column meant to
# notice a lost chain could never have noticed one. `n_chains_returned` counts
# the chains whose draws actually came back.
.sampler_diagnostics <- function(fit) {
  s <- fit$summary
  key <- grepl("^(mu|beta|sigma|delta_|lor_|rd_|rr_)", s$variable)
  d <- fit$diagnostics
  out <- list(
    max_rhat = suppressWarnings(max(s$Rhat[key], na.rm = TRUE)),
    min_ess  = suppressWarnings(min(s$n_eff[key], na.rm = TRUE)),
    min_ess_tail = if ("ess_tail" %in% names(s)) {
      suppressWarnings(min(s$ess_tail[key], na.rm = TRUE))
    } else {
      NA_real_
    },
    divergent = d$n_divergent %||% NA_real_,
    max_treedepth = d$n_max_treedepth %||% NA_real_,
    n_chains_ok = d$n_chains_returned %||% NA_real_,
    n_chains_requested = d$n_chains_requested %||% NA_real_)
  lapply(out, function(x) if (length(x) && is.finite(x)) as.numeric(x) else NA_real_)
}

# The shared draw for one replication: one index study and one comparator
# cohort. Every design in the replication is a tabulation of THIS cohort.
#
# It is regenerated per fitted design rather than computed once and passed
# around, so that a design can be its own parallel task. The seed fixes the
# draw completely and the generation order below never varies, so the six
# designs of a replication see byte-identical data; the cost is a few hundred
# random numbers against a fit measured in seconds.
make_replication <- function(family, seed) {
  set.seed(seed)

  # Index study.
  Xi <- draw_covariates(N_IPD, POP_INDEX)
  eta_i <- linpred(Xi, MU_A, BETA_A)
  ipd_df <- data.frame(trt = "A", age = Xi[, 1], sex = Xi[, 2], prior = Xi[, 3])
  ipd_df$y <- switch(family,
    normal   = stats::rnorm(N_IPD, eta_i, SIGMA),
    binomial = stats::rbinom(N_IPD, 1, stats::plogis(eta_i)),
    poisson  = stats::rpois(N_IPD, EXPO * exp(eta_i)))
  if (family == "poisson") ipd_df$expo <- EXPO

  # The truth is a property of THIS index sample, because the estimand is.
  truth <- true_effect(family, Xi)

  # ONE comparator cohort. Outcomes are drawn per person, once, so every
  # design reports the same people and the same events, cut differently.
  Xc <- draw_covariates(N_CMP, POP_COMPARATOR)
  eta_c <- linpred(Xc, MU_B, BETA_B)
  yc <- switch(family,
    normal   = stats::rnorm(N_CMP, eta_c, SIGMA),
    binomial = stats::rbinom(N_CMP, 1, stats::plogis(eta_c)),
    poisson  = stats::rpois(N_CMP, EXPO * exp(eta_c)))

  ipd <- set_ipd(ipd_df, treatment = "trt", outcome = "y", covariates = COVS,
                 family = family,
                 exposure = if (family == "poisson") "expo" else NULL)

  list(ipd = ipd, Xc = Xc, yc = yc, truth = truth)
}

run_design <- function(design_name, family, rep_id, seed) {
  d <- make_replication(family, seed)
  ipd <- d$ipd; Xc <- d$Xc; yc <- d$yc; truth <- d$truth
  lab <- PARTITIONS[[design_name]](Xc)
  check_partition(lab, nrow(Xc), design_name)
  strata <- sort(unique(lab))

  rows <- lapply(strata, function(s) {
    idx <- lab == s
    Xs <- Xc[idx, , drop = FALSE]
    ys <- yc[idx]
    n_s <- nrow(Xs)
    out <- list(age_mean = mean(Xs[, 1]), age_sd = stats::sd(Xs[, 1]),
                sex_mean = mean(Xs[, 2]), prior_mean = mean(Xs[, 3]))
    if (family == "normal") {
      out$ybar <- mean(ys); out$yse <- stats::sd(ys) / sqrt(n_s); out$nn <- n_s
    } else if (family == "binomial") {
      out$r <- sum(ys); out$nn <- n_s
    } else {
      out$r <- sum(ys); out$EE <- n_s * EXPO
    }
    as.data.frame(out)
  })
  agd_df <- do.call(rbind, rows)
  agd_df$trt <- "B"

  # A cell that happens to contain only men has a realized proportion of
  # exactly 0, and that is the honest summary: nudging it off the boundary
  # would make the declared moments disagree with the integration grid, which
  # the package then reports as a contradiction. Record degenerate margins
  # instead of hiding them.
  degenerate <- sum(agd_df$sex_mean %in% c(0, 1)) +
    sum(agd_df$prior_mean %in% c(0, 1))

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
  # The generator draws the three covariates independently, so the true copula
  # is the identity. Left to itself the package estimates a correlation matrix
  # from the index sample and transports that random matrix to every comparator
  # row, which is a second misspecification varying from replication to
  # replication and would be attributed to the design. Pass the truth.
  dat <- add_integration(dat,
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    sex = distr(qbern, prob = sex_mean),
    prior = distr(qbern, prob = prior_mean), n_int = N_INT,
    cor = diag(K), cor_adjust = "none")

  # The diagnostic runs on the family's own link. Only the identity link makes
  # it an exact statement about the design matrix; for logit and log the
  # package reports it as descriptive and returns NA for `flagged` when the row
  # count is adequate. That NA is preserved: collapsing it to FALSE would
  # publish an identification verdict the package explicitly declines to give.
  ci <- check_identification(dat, link = LINK[[family]], verbose = FALSE)
  g <- DESIGN_GEOMETRY[[design_name]]

  base <- data.frame(
    family = family, design = design_name, rep = rep_id, seed = seed,
    n_rows = unname(g["n_rows"]), n_min_row = min(table(lab)),
    n_max_row = max(table(lab)),
    cond_inv = unname(g["cond_inv"]), eff_dim = unname(g["eff_dim"]),
    pkg_cond_inv = ci$cond_inv, pkg_spread = ci$spread,
    flagged = ci$flagged, scope = ci$diagnostic_scope,
    target_in_span = ci$target_in_span,
    degenerate_margins = degenerate, truth = truth, stringsAsFactors = FALSE)

  # One chain process per fit. The package mirrors rstan and defaults
  # `parallel_chains` to the chain count, so with the worker pool below every
  # fit would fan out to CHAINS processes and the machine would run
  # cores * CHAINS chains at once. That oversubscription made a fit that takes
  # three seconds alone take over two minutes. Parallelism belongs at the cell
  # level here, where it is already.
  fit <- try(suppressWarnings(mlumr(dat, model = MODEL, chains = CHAINS,
                                    iter = ITER, warmup = WARMUP, seed = seed,
                                    refresh = 0, parallel_chains = 1)),
             silent = TRUE)
  na_diag <- list(max_rhat = NA_real_, min_ess = NA_real_,
                  min_ess_tail = NA_real_, divergent = NA_real_,
                  max_treedepth = NA_real_, n_chains_ok = NA_real_,
                  n_chains_requested = NA_real_)
  if (inherits(fit, "try-error")) {
    # A failure that is only a count cannot be told apart from a transient one.
    # Keep what it was, so the vignette can say whether exclusions are
    # systematic rather than assert that they are not.
    cond <- attr(fit, "condition")
    return(cbind(base, ok = FALSE,
                 err_class = paste(class(cond), collapse = "/"),
                 err_message = conditionMessage(cond),
                 est = NA_real_, lo = NA_real_, hi = NA_real_,
                 width = NA_real_, covered = NA, as.data.frame(na_diag)))
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
  # The point estimate is the posterior MEDIAN, not the mean, and uniformly so.
  # Quantiles are the only summaries that survive the log: the median of
  # log(RR) is the log of the median RR, whereas log(E[RR]) is not E[log RR],
  # and that gap is widest in exactly the weakly identified cells this study is
  # about. A median also matches the interval, which is already quantile based.
  est <- if (on_log) log(r$q50) else r$q50
  lo  <- if (on_log) log(r$q2.5) else r$q2.5
  hi  <- if (on_log) log(r$q97.5) else r$q97.5

  cbind(base, ok = TRUE, err_class = NA_character_, err_message = NA_character_,
        est = est, lo = lo, hi = hi, width = hi - lo,
        covered = truth >= lo && truth <= hi,
        as.data.frame(.sampler_diagnostics(fit)))
}

# ---- run the study -----------------------------------------------------------

N_REP <- 300L
OUT_CSV <- file.path("data-raw", "subgroup_identification_results.csv")
MANIFEST <- file.path("data-raw", "subgroup_identification_manifest.dcf")
SCRIPT_PATH <- file.path("data-raw", "simulate_subgroup_identification.R")
CELLS <- paste0(OUT_CSV, ".cells")
dir.create(CELLS, showWarnings = FALSE, recursive = TRUE)

# A cell is one fitted design, so that every fit is its own parallel task. The
# SEED, though, belongs to the replication: it is a function of family and
# replication only, never of the design. That is what keeps the six designs of
# a replication on identical data and the contrasts between them paired.
reps <- expand.grid(family = c("normal", "binomial", "poisson"),
                    rep = seq_len(N_REP), stringsAsFactors = FALSE)
# One seed per replication, derived from a single root, so the whole study is
# reproducible from that root alone.
reps$seed <- 2026L + seq_len(nrow(reps)) * 7L
grid <- merge(reps, data.frame(design = names(PARTITIONS),
                               stringsAsFactors = FALSE), by = NULL)
grid <- grid[order(grid$family, grid$rep, grid$design), ]
rownames(grid) <- NULL
# Checkpoints are named by the cell's IDENTITY, not by its position in the grid.
# A positional name silently reuses a stale file whenever N_REP, the design
# list or the family list changes: index 42 becomes a different cell, its old
# .rds is taken as finished work, and the assembled CSV mixes two
# configurations without an error anywhere.
grid$key <- sprintf("%s__%s__rep%04d", grid$family, grid$design, grid$rep)

# A name is not a configuration. A partition rule can be edited and keep its
# name, and reordering `expand.grid()` changes every seed while every key stays
# put, so a stale checkpoint would still be accepted. Pin the whole
# configuration next to the cells and refuse to reuse them when it differs.
# `identical()` rather than a hash: this is a handful of small objects, so
# there is no reason to accept collision risk for brevity.
#
# Everything that moves an estimate has to be in here or the guard is
# decorative: the constants, the partition rules, the model, the sampler
# settings, the source of the cell computation itself, and the software that
# supplies the likelihood and the integration. A patch release of mlumr, or a
# different CmdStan, is a different fitting procedure for this purpose, and
# rerunning is cheaper than a CSV whose rows came from two of them.
.software <- function() {
  cmdstan <- tryCatch(as.character(cmdstanr::cmdstan_version()),
                      error = function(e) NA_character_)
  cmdstanr_v <- tryCatch(as.character(utils::packageVersion("cmdstanr")),
                         error = function(e) NA_character_)
  # The version string does not identify a development build: every revision
  # between releases reports 0.1.0.9000, and this study calls the INSTALLED
  # package, so two runs months apart can claim the same software. The lazy-load
  # database is the compiled R code that actually ran, so its digest separates
  # them. It is a provenance fingerprint, not a security control.
  rdb <- tryCatch(
    unname(tools::md5sum(file.path(find.package("mlumr"), "R", "mlumr.rdb"))),
    error = function(e) NA_character_)
  # The Stan sources are the other material input the installed package
  # supplies, and the lazy-load database says nothing about them. One digest
  # over every installed .stan file, in a fixed order, records which models
  # ran.
  stan <- tryCatch({
    dir <- system.file("stan", package = "mlumr")
    files <- sort(list.files(dir, pattern = "[.]stan$", recursive = TRUE,
                             full.names = TRUE))
    tmp <- tempfile("mlumr-stan-")
    on.exit(unlink(tmp), add = TRUE)
    writeLines(paste(basename(files), unname(tools::md5sum(files))), tmp)
    unname(tools::md5sum(tmp))
  }, error = function(e) NA_character_)
  list(mlumr = as.character(utils::packageVersion("mlumr")),
       mlumr_code = if (length(rdb) && !is.na(rdb)) rdb else NA_character_,
       mlumr_stan = if (length(stan) && !is.na(stan)) stan else NA_character_,
       r = paste0(R.version$major, ".", R.version$minor),
       cmdstanr = cmdstanr_v, cmdstan = cmdstan,
       platform = R.version$platform)
}

# Provenance for the MANIFEST only, deliberately NOT part of `CONFIG`. The
# checkpoint guard compares configurations with `identical()`, so a commit hash
# in there would invalidate every checkpoint on any commit, including ones that
# change nothing the study computes.
#
# Version numbers alone cannot identify the code that produced these results.
# Several development revisions share "0.1.0.9000", and the script loads the
# INSTALLED package, so a reader cannot tell from the version whether the rows
# came from the tree in front of them. The library path and the working tree's
# commit close that gap. `git` may be absent; that is recorded, not fatal.
.provenance <- function() {
  git1 <- function(args) {
    out <- tryCatch(suppressWarnings(
      system2("git", args, stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0))
    if (!length(out)) NA_character_ else out[[1]]
  }
  status <- tryCatch(suppressWarnings(
    system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0))
  c(.software(),
    list(mlumr_lib = tryCatch(dirname(find.package("mlumr")),
                              error = function(e) NA_character_),
         commit = git1(c("rev-parse", "HEAD")),
         tree = git1(c("rev-parse", "HEAD^{tree}")),
         dirty = length(status) > 0L,
         # Which paths were dirty, so a reader can tell a dirty vignette from a
         # dirty Stan file; "dirty: true" alone cannot.
         dirty_paths = if (length(status)) trimws(status) else character(0)))
}

CONFIG <- list(
  partitions = lapply(PARTITIONS, function(f) deparse(body(f))),
  design_geometry = DESIGN_GEOMETRY,
  n_rep = N_REP, families = sort(unique(grid$family)),
  beta_a = BETA_A, beta_b = BETA_B, mu_a = MU_A, mu_b = MU_B,
  n_ipd = N_IPD, n_cmp = N_CMP, sigma = SIGMA, expo = EXPO, n_int = N_INT,
  k = K, ref_sd = REF_SD, covs = COVS, link = LINK,
  pop_index = POP_INDEX, pop_comparator = POP_COMPARATOR,
  model = MODEL, chains = CHAINS, iter = ITER, warmup = WARMUP,
  make_replication = deparse(body(make_replication)),
  run_design = deparse(body(run_design)),
  band = deparse(body(.band)), software = .software(), engine = ENGINE,
  # Every other function whose body decides what a recorded number MEANS.
  # Pinning only the two big ones left the rest free to change underneath a
  # resumed run: a different `true_effect()` redefines `truth`, and so silently
  # redefines `covered`, while a different `.sampler_diagnostics()` redefines
  # which fits count as converged. Either would assemble one CSV out of two
  # definitions, with nothing in the file to show it.
  helpers = lapply(list(true_effect = true_effect,
                        sampler_diagnostics = .sampler_diagnostics,
                        geometry = geometry,
                        check_partition = check_partition,
                        draw_covariates = draw_covariates,
                        linpred = linpred),
                   function(f) deparse(body(f))),
  seeds = grid$seed[order(grid$key)]
)
config_file <- file.path(CELLS, "_config.rds")
if (file.exists(config_file)) {
  # An interrupted write leaves a truncated file behind. Left bare, `readRDS()`
  # then aborts every later run with an unserialization error that says nothing
  # about what to do, so name the recovery step here instead.
  old <- tryCatch(readRDS(config_file), error = function(e) {
    stop("`", config_file, "` is unreadable (", conditionMessage(e), "). It ",
         "was probably written by an interrupted run. Delete `", CELLS,
         "` to start a clean study; the checkpoints beside it cannot be ",
         "trusted without the configuration that describes them.",
         call. = FALSE)
  })
  if (!identical(old, CONFIG)) {
    differs <- names(CONFIG)[!vapply(names(CONFIG), function(nm)
      identical(old[[nm]], CONFIG[[nm]]), logical(1))]
    stop("`", CELLS, "` holds checkpoints from a DIFFERENT configuration. ",
         "Reusing them would assemble one CSV out of two studies. ",
         if (length(differs)) paste0("Changed: ", paste(differs, collapse = ", "),
                                     ". ") else "",
         "Move or delete that directory to start a clean run.", call. = FALSE)
  }
} else {
  # Write then rename, as the cell checkpoints already do. A rename within one
  # directory is atomic, so an interruption leaves either no configuration or a
  # complete one, never a half-written file that blocks every rerun.
  tmp <- paste0(config_file, ".tmp", Sys.getpid())
  saveRDS(CONFIG, tmp)
  if (!file.rename(tmp, config_file)) {
    unlink(tmp)
    stop("could not write ", config_file, call. = FALSE)
  }
}

# `detectCores()` returns NA on some systems, and fork parallelism does not
# exist on Windows; both must degrade to a serial run rather than error.
detected <- parallel::detectCores()
if (!is.finite(detected)) detected <- 1L
# Each worker runs its chains serially, so one worker is one busy core and the
# pool can fill the machine rather than a third of it.
cores <- if (.Platform$OS.type == "windows") 1L else {
  max(1L, detected - 1L)
}
cat("cells:", nrow(grid), "fits over", nrow(reps), "replications  cores:",
    cores, "\n")
utils::flush.console()

t0 <- Sys.time()
invisible(parallel::mclapply(seq_len(nrow(grid)), function(i) {
  # Each cell writes its own file the moment it finishes, so a worker that dies
  # costs one cell rather than the run, and a rerun skips what is already done.
  f <- file.path(CELLS, paste0(grid$key[i], ".rds"))
  if (file.exists(f)) return(NULL)
  r <- try(suppressWarnings(suppressMessages(
    run_design(grid$design[i], grid$family[i], grid$rep[i], grid$seed[i])
  )), silent = TRUE)
  if (!inherits(r, "try-error")) {
    # Write then rename, so a worker killed mid-write leaves no truncated file
    # to be mistaken for a finished cell and to break the assembly below.
    tmp <- paste0(f, ".part")
    saveRDS(r, tmp)
    if (!file.rename(tmp, f)) unlink(tmp)
  }
  NULL
}, mc.cores = cores, mc.preschedule = FALSE))

# Read only the cells THIS grid defines. Globbing the directory would sweep in
# leftovers from a larger previous grid and silently append them.
files <- file.path(CELLS, paste0(grid$key, ".rds"))
present <- file.exists(files)
# One unreadable checkpoint must not destroy the assembly of every other cell.
rows <- lapply(files[present], function(f) {
  r <- try(readRDS(f), silent = TRUE)
  if (inherits(r, "try-error")) {
    unlink(f)
    NULL
  } else {
    r
  }
})
dropped <- sum(vapply(rows, is.null, logical(1)))
res <- do.call(rbind, rows)

# The printed count is not a release gate; this is. An incomplete study must
# not be publishable as a complete one, so refuse to write the CSV at all
# unless every replication is present and every design appears exactly once in
# each.
# Counting is not checking. A count of unique triples equal to the expected
# count is satisfied by a set that has the right SIZE and the wrong MEMBERS: one
# stale checkpoint carrying rep 301 keeps 5400 distinct triples while the rep it
# replaced is missing. Compare the assembled identities against the grid itself,
# and require the seeds to match too, so a cell computed under a different draw
# cannot pass as the cell it is standing in for.
expected <- nrow(grid)
key_of <- function(d) paste(d$family, d$design, d$rep, sep = "\r")
want <- key_of(grid)
got <- if (is.null(res)) character(0) else key_of(res)
missing <- setdiff(want, got)
unexpected <- setdiff(got, want)
dupes <- got[duplicated(got)]
# Only meaningful once the key sets agree; `==` rather than `identical()`
# because a seed read back from a checkpoint may be integer or double.
seed_ok <- TRUE
if (!is.null(res) && !length(missing) && !length(unexpected) && !length(dupes)) {
  seed_ok <- all(res$seed[match(want, got)] == grid$seed)
}
complete <- !is.null(res) && nrow(res) == expected &&
  !length(missing) && !length(unexpected) && !length(dupes) && seed_ok
if (!complete) {
  cat("INCOMPLETE:", if (is.null(res)) 0L else nrow(res), "of", expected,
      "fits. The CSV was NOT written.\n")
  if (length(missing)) {
    cat("  missing", length(missing), "cells, first:",
        sub("\r", "/", utils::head(missing, 3L)), "\n")
  }
  if (length(unexpected)) {
    cat("  ", length(unexpected), "cells do not belong to this grid, first:",
        sub("\r", "/", utils::head(unexpected, 3L)),
        "\n  Those are checkpoints from another study; delete", CELLS, "\n")
  }
  if (length(dupes)) cat("  ", length(dupes), "duplicated cells\n")
  if (!seed_ok) {
    cat("  at least one cell was computed under a seed the grid does not",
        "assign it\n")
  }
  quit(status = 1L)
}

# Publish atomically. The gate above validates what is in memory, and a plain
# `write.csv()` to the published path truncates it first: an interruption, or a
# full disk, then leaves a partial results file where a complete one used to be,
# and the digest in the article no longer matches anything. Write beside it and
# rename, which is atomic within a directory.
csv_tmp <- paste0(OUT_CSV, ".tmp", Sys.getpid())
write.csv(res, csv_tmp, row.names = FALSE)
if (nrow(utils::read.csv(csv_tmp)) != expected) {
  unlink(csv_tmp)
  stop("the results written to disk do not read back complete", call. = FALSE)
}
if (!file.rename(csv_tmp, OUT_CSV)) {
  unlink(csv_tmp)
  stop("could not publish ", OUT_CSV, call. = FALSE)
}

# Provenance, so the shipped vignette and the results it reports cannot drift
# apart silently. The vignette prints the digest of the file it read, and a test
# compares that against the digest of the file on disk: regenerate the results
# without re-knitting and the two stop matching. The rest of the manifest
# records what produced them, since a number is not reproducible without the
# software that made it.
#
# `tools::md5sum()` is base R, so this costs no dependency. It is a
# drift detector, not a security control.
write_manifest <- function(csv, rows) {
  sw <- .provenance()
  manifest <- c(
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Script: %s", basename(SCRIPT_PATH)),
    sprintf("Script-MD5: %s", unname(tools::md5sum(SCRIPT_PATH))),
    sprintf("Results: %s", basename(csv)),
    sprintf("Results-MD5: %s", unname(tools::md5sum(csv))),
    sprintf("Rows: %d", rows),
    sprintf("Replications: %d", N_REP),
    sprintf("Designs: %s", paste(names(PARTITIONS), collapse = ", ")),
    sprintf("Chains: %d", CHAINS),
    sprintf("Iter: %d", ITER),
    sprintf("Warmup: %d", WARMUP),
    sprintf("mlumr: %s", sw$mlumr),
    sprintf("mlumr-code-MD5: %s", sw$mlumr_code),
    sprintf("mlumr-stan-MD5: %s", sw$mlumr_stan),
    sprintf("mlumr-library: %s", sw$mlumr_lib),
    sprintf("Commit: %s", sw$commit),
    sprintf("Tree: %s", sw$tree),
    sprintf("Working-tree-dirty: %s", tolower(as.character(isTRUE(sw$dirty)))),
    sprintf("Dirty-paths: %s", if (length(sw$dirty_paths)) {
      paste(sw$dirty_paths, collapse = "; ")
    } else {
      "none"
    }),
    sprintf("R: %s", sw$r),
    sprintf("cmdstanr: %s", sw$cmdstanr),
    sprintf("CmdStan: %s", sw$cmdstan),
    sprintf("Platform: %s", sw$platform))
  writeLines(manifest, MANIFEST)
  cat("wrote", MANIFEST, "\n")
}
write_manifest(OUT_CSV, nrow(res))

cat("completed:", nrow(reps), "replications,", nrow(res), "fits",
    " minutes:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
    "\nwrote", OUT_CSV, "\n")
