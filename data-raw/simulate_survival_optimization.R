#!/usr/bin/env Rscript
# Simulation study behind the article "Survival fits with and without optimization"
# (vignettes/survival-optimization).
#
# It measures what `mlumr(optimization = TRUE)` saves. In the four survival
# models the comparator likelihood of pseudo-individual j in arm a integrates
# over the arm's n_int integration points, with the linear predictor
# eta = Xq_int[a] * beta_tilde. Without the shortcut (`optimization = FALSE`,
# and every release before 0.2.1) eta is recomputed for every
# pseudo-individual; with it, once per arm and parameter evaluation. Both
# settings run in ONE executable, selected by the data flag `reuse_arm_eta`.
#
# AIMS
#   A1. The time saved per evaluation of the log density and its gradient, the
#       unit of work the sampler repeats, and how it depends on the fitted
#       distribution, the model, the number of comparator pseudo-individuals
#       (n_agd), integration points (n_int) and covariates (n_cov).
#   A2. How much of that reaches a complete mlumr() fit.
#   A3. That both settings compute the same thing (the parity gate in
#       tests/parity is the proof; here the agreement is recorded at every
#       point timed and every fit, as a check on the runs themselves).
#
# DATA-GENERATING MECHANISM (one, fixed). Index IPD of N_IPD people and a
# comparator cohort of n_agd people, simulated the same way: K covariates,
# alternating continuous (normal, SD 1) and binary; Weibull proportional
# hazards with shape SHAPE and coefficients BETA; the index treatment lowers
# the hazard by LOGHR; the comparator population is shifted by CMP_SHIFT on
# the continuous covariates and has prevalence BIN_CMP on the binary ones.
# Independent exponential censoring at rate CENS_RATE and administrative
# censoring at ADMIN months; right censoring only, no delayed entry. The
# comparator cohort's times and statuses are passed to set_agd_surv() as its
# pseudo-IPD, with its realized covariate means, SDs and proportions; for the
# computation this is the same as a digitized Kaplan-Meier curve. Integration
# points come from add_integration() with its default Gaussian copula.
#
# The fitted model is not the generating one: the timing depends on the code
# path of the FITTED distribution, and on the data only through the mix of
# event and censoring statuses, which is held fixed here.
#
# METHODS compared (`arm`):
#   reuse      the 0.2.1 models, reuse_arm_eta = 1 (optimization = TRUE)
#   no_reuse   the 0.2.1 models, reuse_arm_eta = 0 (optimization = FALSE)
#   v0.2.0     the frozen 0.2.0 models in tests/parity/survival-baseline-6a366a0
#              (Level A only), so the article can compare with the release
#              users ran before, not only with the switch turned off.
#
# LEVEL A, fixed-point timing. Each workload is a fitted distribution, model,
# n_agd, n_int and n_cov; each has N_DATASETS data sets. The cost of one
# evaluation is measured INSIDE the CmdStan executable the sampler uses, with
# CmdStan's `log_prob` method, which evaluates the log density and its
# gradient at every row of a file of unconstrained parameter values. A run
# with N rows minus a run with N_POINTS rows, divided by N - N_POINTS, is the
# cost of one evaluation with process start, data input and model construction
# removed. The time is the CPU time of the CmdStan process (user plus system),
# which other work on the machine slows but does not inflate the way it
# inflates elapsed time; elapsed time is recorded as well. The points are
# N_POINTS draws from CmdStan's Laplace approximation at the posterior mode of
# the data set, so they lie where the sampler spends its time; when the mode
# cannot be found the points are the origin and small perturbations of it,
# and the row says so. Every block times every arm once, in a random order;
# the arms of one block are a paired comparison.
#
# LEVEL B, complete fits. A few scenarios, each fitted with optimization TRUE
# and FALSE on the same data and the same seed (so the same initial values),
# through mlumr(engine = "cmdstanr") with its default sampler settings and the
# four chains run in parallel. The order of the two fits alternates between
# data sets. Recorded per chain: warmup and sampling seconds, leapfrog steps
# in sampling, divergences, saturated trees, E-BFMI; per fit: the elapsed time
# of the mlumr() call, bulk and tail ESS of rmst_diff_comparator, max R-hat;
# per pair: the first sampling iteration at which the two fits' parameter
# draws differ in each chain (they are written with 18 significant digits).
#
# Run from the package root with the branch's mlumr INSTALLED, the machine
# plugged in and otherwise idle:
#
#   Rscript data-raw/simulate_survival_optimization.R pilot     # not results
#   Rscript data-raw/simulate_survival_optimization.R fixed
#   Rscript data-raw/simulate_survival_optimization.R fits
#   Rscript data-raw/simulate_survival_optimization.R assemble
#
# Every unit writes its own checkpoint, so an interrupted stage resumes where
# it stopped. `assemble` refuses to publish unless every unit is present, then
# writes the two results files and the manifest beside them.

stage <- commandArgs(trailingOnly = TRUE)
if (length(stage) != 1L ||
      !stage %in% c("pilot", "fixed", "fits", "assemble")) {
  stop("Usage: Rscript data-raw/simulate_survival_optimization.R ",
       "pilot|fixed|fits|assemble", call. = FALSE)
}

suppressPackageStartupMessages({
  library(mlumr)
  library(survival)
})
for (pkg in c("cmdstanr", "posterior", "jsonlite", "withr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("This study needs the '", pkg, "' package.", call. = FALSE)
  }
}

SCRIPT_PATH <- file.path("data-raw", "simulate_survival_optimization.R")
if (!file.exists(SCRIPT_PATH)) stop("Run from the package root.", call. = FALSE)
BASELINE_DIR <- file.path("tests", "parity", "survival-baseline-6a366a0")
FIXED_CSV <- file.path("data-raw", "survival_optimization_fixed.csv")
FITS_CSV <- file.path("data-raw", "survival_optimization_fits.csv")
MANIFEST <- file.path("data-raw", "survival_optimization_manifest.dcf")
PILOT_DIR <- file.path(tempdir(), "survival-optimization-pilot")
CELLS_FIXED <- if (stage == "pilot") file.path(PILOT_DIR, "fixed") else
  paste0(FIXED_CSV, ".cells")
CELLS_FITS <- if (stage == "pilot") file.path(PILOT_DIR, "fits") else
  paste0(FITS_CSV, ".cells")
EXE_DIR <- file.path(tools::R_user_dir("mlumr", "cache"), "survival-optimization",
                     paste0("cmdstan-", cmdstanr::cmdstan_version()))

ROOT_SEED <- 2026L

# ---- data-generating mechanism -----------------------------------------------

N_IPD <- 200L
BETA <- c(0.3, -0.2, 0.15, -0.1, 0.25, -0.15)
SHAPE <- 1.2
MU_INDEX <- log(0.1)       # median survival near five months at the reference
LOGHR <- -0.4              # index versus comparator, conditional
CMP_SHIFT <- 0.2
BIN_INDEX <- 0.5
BIN_CMP <- 0.45
CENS_RATE <- 0.02
ADMIN <- 24
RMST_HORIZON <- 18
PRED_TIMES <- c(6, 12, 18)

cov_names <- function(k) paste0("x", seq_len(k))
is_binary <- function(k) seq_len(k) %% 2L == 0L

simulate_arm <- function(n, k, mu, shift, prev) {
  X <- vapply(seq_len(k), function(j) {
    if (is_binary(k)[j]) stats::rbinom(n, 1, prev) else stats::rnorm(n, shift, 1)
  }, numeric(n))
  X <- matrix(X, n, k, dimnames = list(NULL, cov_names(k)))
  eta <- mu + drop(X %*% BETA[seq_len(k)])
  t_event <- (-log(stats::runif(n)) / exp(eta))^(1 / SHAPE)
  t_cens <- pmin(stats::rexp(n, CENS_RATE), ADMIN)
  data.frame(time = pmin(t_event, t_cens),
             status = as.integer(t_event <= t_cens), X)
}

# One data set, before integration. The seed fixes the people; the same data
# set is shared by every fitted distribution and model of its size, so those
# comparisons are on common random numbers.
make_data <- function(n_agd, n_cov, seed) {
  set.seed(seed)
  covs <- cov_names(n_cov)
  idx <- simulate_arm(N_IPD, n_cov, MU_INDEX + LOGHR, 0, BIN_INDEX)
  idx$trt <- "A"
  ipd <- set_ipd(idx, "trt", covariates = covs, family = "survival",
                 time = "time", status = "status")
  cmp <- simulate_arm(n_agd, n_cov, MU_INDEX, CMP_SHIFT, BIN_CMP)
  agd <- data.frame(trt = "B", time = cmp$time, status = cmp$status)
  for (j in seq_len(n_cov)) {
    x <- cmp[[covs[j]]]
    if (is_binary(n_cov)[j]) {
      agd[[paste0(covs[j], "_prop")]] <- mean(x)
    } else {
      agd[[paste0(covs[j], "_mean")]] <- mean(x)
      agd[[paste0(covs[j], "_sd")]] <- stats::sd(x)
    }
  }
  bin <- is_binary(n_cov)
  agd_obj <- set_agd_surv(
    agd, "trt", time = "time", status = "status",
    cov_means = ifelse(bin, paste0(covs, "_prop"), paste0(covs, "_mean")),
    cov_sds = ifelse(bin, NA, paste0(covs, "_sd")),
    cov_types = ifelse(bin, "binary", "continuous"))
  combine_data(ipd, agd_obj)
}

integrate_data <- function(dat, n_int) {
  covs <- dat$covariates
  # distr() captures its arguments unevaluated, as column names of the
  # aggregate data, so each call is built with the column's name spliced in.
  # The names come from this script, never from input.
  ds <- lapply(seq_along(covs), function(j) {
    m <- as.name(paste0(covs[j], "_mean"))
    if (is_binary(length(covs))[j]) {
      eval(bquote(distr(qbern, prob = .(m))))
    } else {
      s <- as.name(paste0(covs[j], "_sd"))
      eval(bquote(distr(qnorm, mean = .(m), sd = .(s))))
    }
  })
  names(ds) <- covs
  suppressWarnings(do.call(add_integration,
                           c(list(data = dat, n_int = n_int, verbose = FALSE), ds)))
}

# The Stan data a user's call would send, without sampling: mlumr() runs every
# guard and builds the complete data list before it reaches the backend, which
# is replaced here by a capture for the duration of the call.
capture_stan_data <- function(dat, model, distribution) {
  ns <- asNamespace("mlumr")
  real <- get(".mlumr_fit_backend", envir = ns)
  capture <- function(engine, model_name, stan_data, ...) {
    stop(structure(class = c("stan_data_capture", "condition"),
                   list(message = "captured", call = NULL,
                        stan_data = stan_data, model_name = model_name)))
  }
  unlockBinding(".mlumr_fit_backend", ns)
  assign(".mlumr_fit_backend", capture, envir = ns)
  on.exit({
    assign(".mlumr_fit_backend", real, envir = ns)
    lockBinding(".mlumr_fit_backend", ns)
  }, add = TRUE)
  res <- tryCatch(
    suppressWarnings(mlumr(dat, model = model, distribution = distribution,
                           rmst_horizon = RMST_HORIZON, pred_times = PRED_TIMES,
                           chains = 1, iter = 2, warmup = 1, refresh = 0,
                           seed = ROOT_SEED, verbose = FALSE,
                           engine = "cmdstanr")),
    stan_data_capture = function(cond) cond)
  if (!inherits(res, "stan_data_capture")) {
    stop("mlumr() returned without reaching the sampler.", call. = FALSE)
  }
  list(stan_data = res$stan_data, model_name = res$model_name)
}

# ---- executables -------------------------------------------------------------

STEMS <- c(spfa_parametric = "mlumr_survival_spfa",
           relaxed_parametric = "mlumr_survival_relaxed",
           spfa_flexible = "mlumr_survival_mspline_spfa",
           relaxed_flexible = "mlumr_survival_mspline_relaxed")

installed_stan <- function(file) {
  path <- system.file("stan", file, package = "mlumr")
  if (!nzchar(path)) stop("The installed mlumr has no ", file, call. = FALSE)
  path
}

# The installed models must be the ones in this checkout: the fits below run
# the installed package, and the article describes the checkout.
stan_sources <- function() {
  files <- c(paste0(STEMS, ".stan"),
             file.path("include", list.files(file.path("inst", "stan", "include"))))
  checkout <- tools::md5sum(file.path("inst", "stan", files))
  installed <- tools::md5sum(vapply(files, installed_stan, character(1)))
  if (!identical(unname(checkout), unname(installed))) {
    stop("The installed mlumr's Stan sources differ from this checkout. ",
         "Install the checkout first (R CMD INSTALL .).", call. = FALSE)
  }
  setNames(unname(checkout), files)
}

# The 0.2.1 models are compiled exactly as mlumr's cmdstanr backend compiles
# them, into the same cache, so Level A and Level B time one executable.
candidate_model <- function(stem) {
  stan_file <- installed_stan(paste0(stem, ".stan"))
  cmdstanr::cmdstan_model(stan_file,
                          dir = mlumr:::.cmdstanr_compile_dir(stem, stan_file),
                          quiet = TRUE)
}
baseline_model <- function(stem) {
  dir <- file.path(EXE_DIR, "v0.2.0")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cmdstanr::cmdstan_model(file.path(BASELINE_DIR, paste0(stem, ".stan")),
                          include_paths = BASELINE_DIR, dir = dir, quiet = TRUE)
}

# ---- parameter points --------------------------------------------------------

# Unconstrained coordinates in declaration order, matrices column-major, which
# is also the order CmdStan writes them. The four models declare only
# unconstrained and lower-bounded (at zero) parameters.
unconstrain <- function(draws, stem) {
  vars <- if (grepl("mspline", stem, fixed = TRUE)) {
    list(beta_tilde = identity, u_lscoef = identity, sigma_smooth = log)
  } else {
    list(beta_tilde = identity, aux_raw = log, aux2_raw = log)
  }
  cols <- colnames(draws)
  pieces <- lapply(names(vars), function(v) {
    hit <- cols[cols == v | startsWith(cols, paste0(v, "["))]
    vars[[v]](draws[, hit, drop = FALSE])
  })
  u <- do.call(cbind, pieces)
  unname(u)
}

write_points <- function(u, n_rows, file) {
  rows <- u[rep_len(seq_len(nrow(u)), n_rows), , drop = FALSE]
  jsonlite::write_json(list(params_r = rows), file, digits = NA)
  file
}

read_log_prob <- function(file) {
  out <- utils::read.csv(file, comment.char = "#")
  as.matrix(out)
}

# Laplace draws at the mode, on the unconstrained scale. The comparison of
# CmdStan's own log density at each draw with the one log_prob reports there
# checks the unconstraining: the two may differ by a constant only.
make_points <- function(mod, data_file, stem, n_par, seed, tmp) {
  fallback <- function(why) {
    u <- withr::with_seed(seed, rbind(
      rep(0, n_par),
      matrix(stats::rnorm((N_POINTS - 1L) * n_par, sd = 0.3), N_POINTS - 1L)))
    list(u = u, source = paste("origin:", why))
  }
  opt <- tryCatch(suppressWarnings(suppressMessages(mod$optimize(
    data = data_file, jacobian = TRUE, seed = seed, refresh = 0,
    iter = 10000, show_messages = FALSE, show_exceptions = FALSE,
    output_dir = tmp))), error = function(e) NULL)
  if (is.null(opt) || !isTRUE(opt$return_codes() == 0)) {
    return(fallback("no mode"))
  }
  lap <- tryCatch(suppressWarnings(suppressMessages(mod$laplace(
    data = data_file, mode = opt, draws = N_POINTS, seed = seed, refresh = 0,
    show_messages = FALSE, show_exceptions = FALSE, output_dir = tmp))),
    error = function(e) NULL)
  if (is.null(lap)) return(fallback("no Laplace draws"))
  draws <- posterior::as_draws_matrix(lap$draws())
  u <- unconstrain(draws, stem)
  if (ncol(u) != n_par || any(!is.finite(u))) return(fallback("bad draws"))
  # cmdstanr renames CmdStan's log_p__ to lp__ in Laplace draws.
  list(u = u, source = "laplace", log_p = as.numeric(draws[, "lp__"]))
}

# Length of the unconstrained parameter vector: the coefficients, then one
# shape per stratum (two for the generalized gamma), or for a flexible
# baseline n_scoef - 1 innovations and one smoothing SD per stratum.
n_unconstrained <- function(sd, distribution) {
  if (distribution %in% c("mspline", "pexp")) {
    return(sd$nB + sd$n_scoef * sd$n_strata)
  }
  shapes <- if (distribution %in% c("exponential", "exponential-aft")) 0L else 1L
  sd$nB + (shapes + (distribution == "gengamma")) * sd$n_strata
}

# ---- timing ------------------------------------------------------------------

# One CmdStan log_prob run. CPU time is the child's user plus system time,
# which proc.time() reports once system2() has waited for it.
run_log_prob <- function(exe, data_file, points_file, out_file) {
  args <- c("log_prob", paste0("unconstrained_params=", points_file),
            "jacobian=1", "data", paste0("file=", data_file),
            "output", paste0("file=", out_file), "sig_figs=18")
  t0 <- proc.time()
  status <- system2(exe, args, stdout = FALSE, stderr = FALSE)
  t1 <- proc.time()
  if (!identical(status, 0L)) {
    stop("CmdStan log_prob failed (status ", status, ") for ", exe, call. = FALSE)
  }
  c(elapsed = t1[["elapsed"]] - t0[["elapsed"]],
    cpu = (t1[["user.child"]] + t1[["sys.child"]]) -
      (t0[["user.child"]] + t0[["sys.child"]]))
}

machine_state <- function() {
  load <- tryCatch(system2("sysctl", c("-n", "vm.loadavg"), stdout = TRUE),
                   error = function(e) NA_character_)
  load <- suppressWarnings(as.numeric(strsplit(gsub("[{}]", "", load), " +")[[1]]))
  load <- load[is.finite(load)]
  c(load1 = if (length(load)) load[[1]] else NA_real_)
}

# A unit starts only when the one-minute load average is below LOAD_MAX, so the
# study measures this code rather than whatever else the machine is doing. The
# load is also recorded with every timed run. The average still carries the
# previous unit's own process for a minute after it ends (four chains after a
# fit pair), so the threshold sits above that residual plus routine operating
# system activity while still refusing any competing heavy workload.
LOAD_MAX <- 6
wait_for_idle <- function() {
  waited <- FALSE
  repeat {
    load <- machine_state()[["load1"]]
    if (!is.finite(load) || load < LOAD_MAX) {
      if (waited) cat(sprintf("[%s] load %.1f, resuming\n",
                              format(Sys.time(), "%H:%M:%S"), load))
      return(invisible(load))
    }
    if (!waited) {
      cat(sprintf("[%s] load %.1f, waiting for it to fall below %g\n",
                  format(Sys.time(), "%H:%M:%S"), load, LOAD_MAX))
      utils::flush.console()
      waited <- TRUE
    }
    Sys.sleep(60)
  }
}

ARMS <- c("reuse", "no_reuse", "v0.2.0")
N_POINTS <- 5L
TARGET_SECONDS <- 0.5
N_BLOCKS <- 8L
N_DATASETS <- 2L
DISTRIBUTIONS <- c("weibull", "lognormal", "gengamma", "mspline")
MODELS <- c("spfa", "relaxed")
N_AGD <- c(100L, 500L, 2000L)
N_INT <- c(16L, 64L, 256L)
N_COV <- c(1L, 3L, 6L)

fixed_grid <- function() {
  sizes <- expand.grid(n_agd = N_AGD, n_int = N_INT, n_cov = N_COV,
                       dataset = seq_len(N_DATASETS))
  # A data set's seed belongs to its size and replicate, not to the fitted
  # model, so every distribution and model of one size sees the same people.
  sizes$seed <- ROOT_SEED + 10L * seq_len(nrow(sizes))
  g <- merge(sizes, expand.grid(distribution = DISTRIBUTIONS, model = MODELS,
                                stringsAsFactors = FALSE), by = NULL)
  g$key <- with(g, sprintf("%s_%s_agd%04d_int%03d_cov%d_d%d", distribution,
                           model, n_agd, n_int, n_cov, dataset))
  g <- g[order(g$key), ]
  rownames(g) <- NULL
  g
}

time_workload <- function(w, models, tmp) {
  dat <- integrate_data(make_data(w$n_agd, w$n_cov, w$seed), w$n_int)
  cap <- capture_stan_data(dat, w$model, w$distribution)
  stem <- cap$model_name
  sd_on <- cap$stan_data
  if (!identical(sd_on$reuse_arm_eta, 1L)) stop("Captured data lack the flag.")
  sd_off <- sd_on
  sd_off$reuse_arm_eta <- 0L
  data_files <- c(reuse = file.path(tmp, "on.json"),
                  no_reuse = file.path(tmp, "off.json"))
  cmdstanr::write_stan_json(sd_on, data_files[["reuse"]])
  cmdstanr::write_stan_json(sd_off, data_files[["no_reuse"]])
  # The frozen models declare no flag; CmdStan ignores the extra entry.
  data_files[["v0.2.0"]] <- data_files[["reuse"]]
  exes <- c(reuse = models$candidate[[stem]]$exe_file(),
            no_reuse = models$candidate[[stem]]$exe_file(),
            v0.2.0 = models$baseline[[stem]]$exe_file())

  n_par <- n_unconstrained(sd_on, w$distribution)
  pts <- make_points(models$candidate[[stem]], data_files[["reuse"]], stem,
                     n_par, w$seed + 1L, tmp)
  base_file <- write_points(pts$u, N_POINTS, file.path(tmp, "p_base.json"))

  # Warm-up and agreement: every arm once at the base points.
  outs <- lapply(ARMS, function(a) {
    f <- file.path(tmp, paste0("lp_", a, ".csv"))
    run_log_prob(exes[[a]], data_files[[a]], base_file, f)
    read_log_prob(f)
  })
  names(outs) <- ARMS
  if (any(!is.finite(outs$reuse))) stop("Nonfinite log density in ", w$key)
  if (ncol(outs$reuse) != n_par + 1L) {
    stop("Expected ", n_par, " gradient coordinates in ", w$key, ", got ",
         ncol(outs$reuse) - 1L)
  }
  gap <- function(a, b) max(abs(outs[[a]] - outs[[b]]))
  if (!is.null(pts$log_p)) {
    shift <- outs$reuse[, "lp__"] - pts$log_p
    if (stats::sd(shift) > 1e-6 * max(1, abs(pts$log_p))) {
      stop("The Laplace points were unconstrained wrongly for ", w$key)
    }
  }

  # Rows per timed run: enough for about TARGET_SECONDS, a multiple of
  # N_POINTS, the same for every arm.
  probe_file <- write_points(pts$u, 10L * N_POINTS, file.path(tmp, "p_probe.json"))
  probe <- run_log_prob(exes[["reuse"]], data_files[["reuse"]], probe_file,
                        file.path(tmp, "probe.csv"))
  base <- run_log_prob(exes[["reuse"]], data_files[["reuse"]], base_file,
                       file.path(tmp, "base.csv"))
  per_eval <- max((probe[["cpu"]] - base[["cpu"]]) / (9L * N_POINTS), 1e-6)
  n_rows <- N_POINTS * min(1000L, max(4L, ceiling(TARGET_SECONDS / per_eval /
                                                    N_POINTS)))
  rows_file <- write_points(pts$u, n_rows, file.path(tmp, "p_rows.json"))

  perms <- withr::with_seed(w$seed + 2L, {
    all <- list(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2),
                c(3, 2, 1))
    sample(c(all, all)[seq_len(N_BLOCKS)])
  })
  # The fixed cost of a run (start, data input, model construction, output)
  # does not depend on the number of rows; each arm's is the median of three
  # runs over the base points, and is subtracted from every timed run of that
  # arm.
  fixed_cost <- lapply(setNames(ARMS, ARMS), function(a) {
    r <- vapply(1:3, function(i) run_log_prob(exes[[a]], data_files[[a]], base_file,
                                              file.path(tmp, "small.csv")),
                numeric(2))
    apply(r, 1, stats::median)
  })
  timings <- list()
  for (b in seq_len(N_BLOCKS)) {
    for (pos in seq_along(ARMS)) {
      a <- ARMS[perms[[b]][pos]]
      big <- run_log_prob(exes[[a]], data_files[[a]], rows_file,
                          file.path(tmp, "big.csv"))
      timings[[length(timings) + 1L]] <- data.frame(
        block = b, position = pos, arm = a, n_rows = n_rows,
        cpu_rows = big[["cpu"]], cpu_base = fixed_cost[[a]][["cpu"]],
        elapsed_rows = big[["elapsed"]],
        elapsed_base = fixed_cost[[a]][["elapsed"]],
        load1 = machine_state()[["load1"]],
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, timings)
  out$ms_per_eval_cpu <- 1000 * (out$cpu_rows - out$cpu_base) /
    (out$n_rows - N_POINTS)
  out$ms_per_eval_elapsed <- 1000 * (out$elapsed_rows - out$elapsed_base) /
    (out$n_rows - N_POINTS)
  cbind(w[rep(1L, nrow(out)), c("key", "distribution", "model", "n_agd",
                                "n_int", "n_cov", "dataset", "seed")],
        stem = stem, n_ipd = N_IPD, n_par = n_par, nB = sd_on$nB,
        n_events_agd = sum(sd_on$agd_status == 1L),
        points = pts$source,
        max_abs_diff_no_reuse = gap("no_reuse", "reuse"),
        max_abs_diff_v020 = gap("v0.2.0", "reuse"),
        out, row.names = NULL)
}

# ---- complete fits -----------------------------------------------------------

SCENARIOS <- data.frame(
  scenario = c("weibull_n500", "mspline_n300"),
  distribution = c("weibull", "mspline"),
  model = "spfa", n_agd = c(500L, 300L), n_int = 64L, n_cov = 3L,
  # Fixed before any fit was run: an M-spline fit at these settings takes
  # tens of minutes, so it gets half the data sets.
  n_datasets = c(10L, 5L),
  stringsAsFactors = FALSE)
CHAINS <- 4L
ITER <- 2000L
WARMUP <- 1000L
PARALLEL_CHAINS <- 4L
ADAPT_DELTA <- 0.95
MAX_TREEDEPTH <- 15L

fits_grid <- function(pilot = FALSE) {
  g <- do.call(rbind, lapply(seq_len(nrow(SCENARIOS)), function(i) {
    s <- SCENARIOS[i, ]
    n <- if (pilot) 1L else s$n_datasets
    data.frame(s[rep(1L, n), setdiff(names(s), "n_datasets")],
               dataset = seq_len(n), row.names = NULL)
  }))
  g$seed <- ROOT_SEED + 100000L + 10L * seq_len(nrow(g))
  # Which setting runs first alternates, so drift on the machine falls on
  # both settings alike.
  g$first <- ifelse(g$dataset %% 2L == 1L, "reuse", "no_reuse")
  g$key <- sprintf("%s_d%02d", g$scenario, g$dataset)
  g
}

param_columns <- function(names) {
  names[grepl("^(beta_tilde|aux_raw|aux2_raw|u_lscoef|sigma_smooth)\\[", names)]
}

fit_once <- function(dat, f, optimization, iter, warmup) {
  t0 <- proc.time()
  fit <- suppressWarnings(mlumr(
    dat, model = f$model, distribution = f$distribution,
    rmst_horizon = RMST_HORIZON, pred_times = PRED_TIMES,
    chains = CHAINS, iter = iter, warmup = warmup, seed = f$seed,
    adapt_delta = ADAPT_DELTA, max_treedepth = MAX_TREEDEPTH, refresh = 0,
    engine = "cmdstanr", verbose = FALSE, optimization = optimization,
    parallel_chains = PARALLEL_CHAINS, sig_figs = 18))
  wall <- (proc.time() - t0)[["elapsed"]]
  cs <- fit$stanfit
  times <- cs$time()$chains
  sd_df <- posterior::as_draws_df(cs$sampler_diagnostics())
  diag <- cs$diagnostic_summary(quiet = TRUE)
  draws <- posterior::as_draws_df(cs$draws())
  target <- posterior::extract_variable_matrix(cs$draws(), "rmst_diff_comparator")
  list(
    wall = wall,
    chains = data.frame(
      chain = times$chain_id, warmup_s = times$warmup, sampling_s = times$sampling,
      leapfrog_sampling = vapply(times$chain_id, function(ch)
        sum(sd_df$n_leapfrog__[sd_df$.chain == ch]), numeric(1)),
      divergent = diag$num_divergent, max_treedepth_hits = diag$num_max_treedepth,
      ebfmi = diag$ebfmi),
    ess_bulk = posterior::ess_bulk(target), ess_tail = posterior::ess_tail(target),
    rhat_max = max(fit$summary$Rhat, na.rm = TRUE),
    mean = mean(target), sd = stats::sd(target), mcse = posterior::mcse_mean(target),
    chains_returned = fit$diagnostics$n_chains_returned,
    draws = draws)
}

# The first sampling iteration at which two fits' parameter draws differ by
# more than rounding in a chain (relative 1e-6), NA when they never do, and the
# largest difference in the chain. The two settings' gradients differ in their
# last digits, so draws that follow one path still differ there, by up to about
# 1e-7 after a few hundred iterations; paths that have separated differ by the
# order of a posterior standard deviation.
DIVERGENCE_TOL <- 1e-6
first_divergence <- function(a, b) {
  cols <- param_columns(names(a))
  do.call(rbind, lapply(sort(unique(a$.chain)), function(ch) {
    x <- as.matrix(a[a$.chain == ch, cols])
    y <- as.matrix(b[b$.chain == ch, cols])
    gap <- abs(x - y)
    hit <- which(rowSums(gap > DIVERGENCE_TOL * pmax(1, abs(x))) > 0)
    data.frame(first_diverging_iteration = if (length(hit)) hit[[1]] else NA_integer_,
               max_abs_param_diff = max(gap))
  }))
}

run_fit_pair <- function(f, pilot = FALSE) {
  iter <- if (pilot) 400L else ITER
  warmup <- if (pilot) 200L else WARMUP
  dat <- integrate_data(make_data(f$n_agd, f$n_cov, f$seed), f$n_int)
  order <- if (f$first == "reuse") c(TRUE, FALSE) else c(FALSE, TRUE)
  res <- list()
  for (k in seq_along(order)) {
    arm <- if (order[k]) "reuse" else "no_reuse"
    started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    load1 <- machine_state()[["load1"]]
    res[[arm]] <- fit_once(dat, f, order[k], iter, warmup)
    res[[arm]]$order <- k
    res[[arm]]$started <- started
    res[[arm]]$load1 <- load1
  }
  diverged <- first_divergence(res$reuse$draws, res$no_reuse$draws)
  lp_gap <- max(abs(res$reuse$draws$lp__ - res$no_reuse$draws$lp__))
  rows <- lapply(c("reuse", "no_reuse"), function(arm) {
    r <- res[[arm]]
    data.frame(key = f$key, scenario = f$scenario, distribution = f$distribution,
               model = f$model, n_agd = f$n_agd, n_int = f$n_int, n_cov = f$n_cov,
               n_ipd = N_IPD, dataset = f$dataset, seed = f$seed, arm = arm,
               order = r$order, started = r$started, load1 = r$load1,
               iter = iter, warmup = warmup, wall_s = r$wall,
               r$chains, diverged, max_abs_lp_diff = lp_gap,
               ess_bulk = r$ess_bulk, ess_tail = r$ess_tail,
               rhat_max = r$rhat_max, post_mean = r$mean, post_sd = r$sd,
               post_mcse = r$mcse, chains_returned = r$chains_returned,
               row.names = NULL)
  })
  do.call(rbind, rows)
}

# ---- configuration and checkpoints ---------------------------------------------

.software <- function() {
  rdb <- tryCatch(
    unname(tools::md5sum(file.path(find.package("mlumr"), "R", "mlumr.rdb"))),
    error = function(e) NA_character_)
  clang <- tryCatch(system2("clang++", "--version", stdout = TRUE)[1],
                    error = function(e) NA_character_)
  list(mlumr = as.character(utils::packageVersion("mlumr")), mlumr_code = rdb,
       r = paste0(R.version$major, ".", R.version$minor),
       cmdstanr = as.character(utils::packageVersion("cmdstanr")),
       cmdstan = as.character(cmdstanr::cmdstan_version()),
       make_local = paste(cmdstanr::cmdstan_make_local(), collapse = "; "),
       compiler = clang, platform = R.version$platform)
}

# Everything that decides what a recorded number means. A resumed stage
# refuses checkpoints written under anything else.
CONFIG <- list(
  dgm = list(N_IPD, BETA, SHAPE, MU_INDEX, LOGHR, CMP_SHIFT, BIN_INDEX, BIN_CMP,
             CENS_RATE, ADMIN, RMST_HORIZON, PRED_TIMES, ROOT_SEED),
  fixed = list(ARMS, N_POINTS, TARGET_SECONDS, N_BLOCKS, N_DATASETS,
               DISTRIBUTIONS, MODELS, N_AGD, N_INT, N_COV),
  fits = list(SCENARIOS, CHAINS, ITER, WARMUP, PARALLEL_CHAINS, ADAPT_DELTA,
              MAX_TREEDEPTH),
  code = lapply(list(simulate_arm, make_data, integrate_data, capture_stan_data,
                     unconstrain, make_points, run_log_prob, time_workload,
                     fit_once, first_divergence, run_fit_pair, fixed_grid,
                     fits_grid, n_unconstrained), function(f) deparse(body(f))),
  divergence_tol = DIVERGENCE_TOL, load_max = LOAD_MAX,
  stan = NULL, baseline = NULL, software = NULL)

guard_cells <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(dir, "_config.rds")
  if (file.exists(f)) {
    old <- readRDS(f)
    if (!identical(old, CONFIG)) {
      differs <- names(CONFIG)[!vapply(names(CONFIG), function(nm)
        identical(old[[nm]], CONFIG[[nm]]), logical(1))]
      stop("`", dir, "` holds checkpoints from a different configuration ",
           "(changed: ", paste(differs, collapse = ", "), "). Move or delete it ",
           "to start again.", call. = FALSE)
    }
  } else {
    stale <- setdiff(list.files(dir), "_config.rds")
    if (length(stale)) {
      stop("`", dir, "` holds checkpoints with no configuration.", call. = FALSE)
    }
    tmp <- paste0(f, ".tmp")
    saveRDS(CONFIG, tmp)
    file.rename(tmp, f)
  }
}

save_cell <- function(dir, key, value) {
  f <- file.path(dir, paste0(key, ".rds"))
  tmp <- paste0(f, ".part")
  saveRDS(value, tmp)
  if (!file.rename(tmp, f)) unlink(tmp)
}

run_units <- function(grid, dir, fun, label) {
  todo <- grid[!file.exists(file.path(dir, paste0(grid$key, ".rds"))), ]
  # Units visited in random order, so slow drift on the machine is spread over
  # the factors instead of following the grid.
  todo <- todo[withr::with_seed(ROOT_SEED + 3L, sample(nrow(todo))), ]
  cat(sprintf("%s: %d of %d units to run\n", label, nrow(todo), nrow(grid)))
  for (i in seq_len(nrow(todo))) {
    wait_for_idle()
    t0 <- Sys.time()
    value <- tryCatch(fun(todo[i, ]), error = function(e) e)
    if (inherits(value, "error")) {
      cat(sprintf("[%s] %s FAILED: %s\n", format(Sys.time(), "%H:%M:%S"),
                  todo$key[i], conditionMessage(value)))
      next
    }
    save_cell(dir, todo$key[i], value)
    cat(sprintf("[%s] %d/%d %s (%.0f s)\n", format(Sys.time(), "%H:%M:%S"), i,
                nrow(todo), todo$key[i],
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    utils::flush.console()
  }
}

collect <- function(grid, dir) {
  files <- file.path(dir, paste0(grid$key, ".rds"))
  missing <- grid$key[!file.exists(files)]
  if (length(missing)) {
    stop(length(missing), " unit(s) missing from ", dir, ", for example ",
         paste(utils::head(missing, 3), collapse = ", "), call. = FALSE)
  }
  out <- do.call(rbind, lapply(files, readRDS))
  if (!setequal(unique(out$key), grid$key)) {
    stop("Assembled units do not match the grid in ", dir, call. = FALSE)
  }
  out
}

# ---- provenance ----------------------------------------------------------------

git1 <- function(args) {
  out <- tryCatch(suppressWarnings(system2("git", args, stdout = TRUE,
                                           stderr = FALSE)),
                  error = function(e) character(0))
  if (!length(out)) NA_character_ else out[[1]]
}
sysctl <- function(name) {
  tryCatch(system2("sysctl", c("-n", name), stdout = TRUE)[1],
           error = function(e) NA_character_)
}

# Taken before anything is written, so `dirty` describes the tree the run
# started from.
provenance <- function() {
  status <- tryCatch(suppressWarnings(system2("git", c("status", "--porcelain"),
                                              stdout = TRUE, stderr = FALSE)),
                     error = function(e) character(0))
  power <- tryCatch(system2("pmset", c("-g", "batt"), stdout = TRUE)[1],
                    error = function(e) NA_character_)
  list(script_md5 = unname(tools::md5sum(SCRIPT_PATH)),
       commit = git1(c("rev-parse", "HEAD")),
       tree = git1(c("rev-parse", "HEAD^{tree}")),
       dirty = length(status) > 0L,
       cpu = sysctl("machdep.cpu.brand_string"),
       cores_performance = sysctl("hw.perflevel0.physicalcpu"),
       cores_efficiency = sysctl("hw.perflevel1.physicalcpu"),
       memory_gb = suppressWarnings(as.numeric(sysctl("hw.memsize")) / 2^30),
       power = power)
}

# ---- stages --------------------------------------------------------------------

CONFIG$software <- .software()
if (stage != "assemble") {
  CONFIG$stan <- stan_sources()
  CONFIG$baseline <- unname(tools::md5sum(list.files(BASELINE_DIR, recursive = TRUE,
                                                     full.names = TRUE)))
}

if (stage %in% c("pilot", "fixed")) {
  grid <- fixed_grid()
  if (stage == "pilot") {
    grid <- grid[grid$dataset == 1L & grid$n_cov == 3L & grid$n_int == 64L &
                   grid$n_agd == 500L, ]
  }
  guard_cells(CELLS_FIXED)
  cat("Compiling models\n")
  models <- list(
    candidate = lapply(setNames(STEMS, STEMS), candidate_model),
    baseline = lapply(setNames(STEMS, STEMS), baseline_model))
  tmp <- tempfile("survival-optimization-")
  dir.create(tmp)
  run_units(grid, CELLS_FIXED, function(w) time_workload(w, models, tmp),
            "Level A")
}

if (stage %in% c("pilot", "fits")) {
  grid <- fits_grid(pilot = stage == "pilot")
  guard_cells(CELLS_FITS)
  # Compiled before any timed call, so no fit pays for compilation.
  invisible(lapply(STEMS[c("spfa_parametric", "spfa_flexible")], candidate_model))
  run_units(grid, CELLS_FITS, function(f) run_fit_pair(f, pilot = stage == "pilot"),
            "Level B")
}

if (stage == "pilot") {
  fixed <- do.call(rbind, lapply(list.files(CELLS_FIXED, "^[^_].*[.]rds$",
                                            full.names = TRUE), readRDS))
  print(stats::aggregate(ms_per_eval_cpu ~ distribution + model + arm, fixed,
                         stats::median))
  fits <- do.call(rbind, lapply(list.files(CELLS_FITS, "^[^_].*[.]rds$",
                                           full.names = TRUE), readRDS))
  print(fits[, c("key", "arm", "chain", "wall_s", "warmup_s", "sampling_s",
                 "leapfrog_sampling", "first_diverging_iteration",
                 "max_abs_param_diff")])
  cat("Pilot output is in", PILOT_DIR, "and is not part of the results.\n")
}

if (stage == "assemble") {
  prov <- provenance()
  CONFIG$stan <- stan_sources()
  CONFIG$baseline <- unname(tools::md5sum(list.files(BASELINE_DIR, recursive = TRUE,
                                                     full.names = TRUE)))
  for (d in c(CELLS_FIXED, CELLS_FITS)) guard_cells(d)
  fixed <- collect(fixed_grid(), CELLS_FIXED)
  fits <- collect(fits_grid(), CELLS_FITS)
  utils::write.csv(fixed, FIXED_CSV, row.names = FALSE)
  utils::write.csv(fits, FITS_CSV, row.names = FALSE)
  sw <- CONFIG$software
  manifest <- c(
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Script: %s", basename(SCRIPT_PATH)),
    sprintf("Script-MD5: %s", prov$script_md5),
    sprintf("Results-Fixed: %s", basename(FIXED_CSV)),
    sprintf("Results-Fixed-MD5: %s", unname(tools::md5sum(FIXED_CSV))),
    sprintf("Results-Fixed-Rows: %d", nrow(fixed)),
    sprintf("Results-Fits: %s", basename(FITS_CSV)),
    sprintf("Results-Fits-MD5: %s", unname(tools::md5sum(FITS_CSV))),
    sprintf("Results-Fits-Rows: %d", nrow(fits)),
    sprintf("Workloads: %d", length(unique(sub("_d[0-9]+$", "", fixed$key)))),
    sprintf("Datasets-Per-Workload: %d", N_DATASETS),
    sprintf("Blocks: %d", N_BLOCKS),
    sprintf("Arms: %s", paste(ARMS, collapse = ", ")),
    sprintf("Scenarios: %s", paste(SCENARIOS$scenario, collapse = ", ")),
    sprintf("Chains: %d", CHAINS), sprintf("Iter: %d", ITER),
    sprintf("Warmup: %d", WARMUP), sprintf("Parallel-chains: %d", PARALLEL_CHAINS),
    sprintf("Load-max: %g", LOAD_MAX),
    sprintf("mlumr: %s", sw$mlumr),
    sprintf("mlumr-code-MD5: %s", sw$mlumr_code),
    sprintf("Stan-sources: %s", paste(names(CONFIG$stan), CONFIG$stan,
                                      collapse = "; ")),
    sprintf("Baseline: %s", BASELINE_DIR),
    sprintf("Commit: %s", prov$commit), sprintf("Tree: %s", prov$tree),
    sprintf("Working-tree-dirty: %s", tolower(as.character(prov$dirty))),
    sprintf("R: %s", sw$r), sprintf("cmdstanr: %s", sw$cmdstanr),
    sprintf("CmdStan: %s", sw$cmdstan),
    sprintf("CmdStan-make-local: %s", if (nzchar(sw$make_local)) sw$make_local else "none"),
    sprintf("Compiler: %s", sw$compiler), sprintf("Platform: %s", sw$platform),
    sprintf("CPU: %s", prov$cpu),
    sprintf("Cores: %s performance, %s efficiency", prov$cores_performance,
            prov$cores_efficiency),
    sprintf("Memory-GB: %.0f", prov$memory_gb),
    sprintf("Power-at-assembly: %s", prov$power))
  writeLines(manifest, MANIFEST)
  cat(sprintf("Wrote %s (%d rows), %s (%d rows) and %s\n", FIXED_CSV,
              nrow(fixed), FITS_CSV, nrow(fits), MANIFEST))
}
