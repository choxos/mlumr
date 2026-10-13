#!/usr/bin/env Rscript
# Native parity gate for the four survival models: the production models in
# inst/stan, which compute the comparator linear predictor once per arm, against
# the frozen pre-reuse models in survival-baseline-6a366a0/.
#
# Usage:
#   Rscript tests/parity/armfast-parity-gate.R CASES.rds OUTPUT_DIR
#          [--mutations] [--exe-dir DIR] [--only PATTERN]
#
# CASES.rds comes from build-armfast-cases.R. This script does not load mlumr:
# cmdstanr's model methods are built against a second copy of TBB, and loading
# them next to the package's RcppParallel copy fails on macOS and crashes on
# Linux, so the fixtures are built in one process and compared in this one.
#
# What is compared, at the same unconstrained points, for every case:
#   * the log density and its native reverse-mode gradient, with and without
#     the Jacobian adjustment;
#   * the constrained parameter mapping, exactly;
#   * every generated quantity, including each log_lik_ipd[i] and
#     log_lik_agd[j] in declaration order.
# Ordinary fixtures must be finite on both sides. The acceptance rule is
# componentwise, |x - y| <= atol + rtol * max(|x|, |y|), with atol = 1e-8 and
# rtol = 1e-10; every comparison's worst coordinate is written to
# OUTPUT_DIR/parity.csv.
#
# What fails the run instead of being skipped: a missing input, a frozen
# baseline or include file whose hash differs from its MANIFEST.txt,
# production includes that differ from the frozen ones, a candidate identical
# to its baseline, a parameter-count or output-schema change, a nonfinite
# value, a mismatch.
#
# --mutations additionally compiles three deliberately wrong candidates and
# checks that the gate rejects each for the right reason, and exercises the
# missing-file, identical-pair, and nonfinite rules directly.
#
# The baseline is compiled against the frozen includes in
# survival-baseline-6a366a0/include, the candidate against the production
# includes in inst/stan, which must hash to the same files. Compiled
# executables and their generated C++ are kept in --exe-dir (default: the mlumr
# user cache, one directory per CmdStan version) and reused while neither the
# model nor any include is newer than the executable. A release gate should
# pass a fresh --exe-dir.

args <- commandArgs(trailingOnly = TRUE)
take_flag <- function(name) {
  hit <- match(name, args)
  if (is.na(hit)) return(FALSE)
  args <<- args[-hit]
  TRUE
}
take_option <- function(name) {
  hit <- match(name, args)
  if (is.na(hit)) return(NULL)
  value <- args[hit + 1L]
  args <<- args[-c(hit, hit + 1L)]
  value
}
do_mutations <- take_flag("--mutations")
exe_dir <- take_option("--exe-dir")
only <- take_option("--only")
if (length(args) != 2L) {
  stop("Usage: Rscript armfast-parity-gate.R CASES.rds OUTPUT_DIR ",
       "[--mutations] [--exe-dir DIR] [--only PATTERN]")
}
cases_file <- args[[1L]]
out <- args[[2L]]

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (length(script) != 1L) stop("Run this script with Rscript.")
parity_dir <- normalizePath(dirname(script), mustWork = TRUE)
root <- normalizePath(file.path(parity_dir, "..", ".."), mustWork = TRUE)
baseline_dir <- file.path(parity_dir, "survival-baseline-6a366a0")
# The candidates are the production models themselves.
production_dir <- file.path(root, "inst", "stan")
candidate_dir <- production_dir
candidate_file <- function(stem) paste0(stem, ".stan")

if (!requireNamespace("cmdstanr", quietly = TRUE)) stop("cmdstanr is required.")
if (!nzchar(Sys.which("git"))) stop("git is required for source hashes.")
cmdstan_version <- cmdstanr::cmdstan_version()
if (is.null(exe_dir)) {
  exe_dir <- file.path(tools::R_user_dir("mlumr", "cache"), "armfast-gate",
                       paste0("cmdstan-", cmdstan_version))
}
dir.create(out, recursive = TRUE, showWarnings = FALSE)
if ("mlumr" %in% loadedNamespaces()) stop("mlumr must not be loaded here.")

stems <- c("mlumr_survival_spfa", "mlumr_survival_relaxed",
           "mlumr_survival_mspline_spfa", "mlumr_survival_mspline_relaxed")
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- inputs and hashes -------------------------------------------------------

require_file <- function(path) {
  if (!file.exists(path)) stop("Missing required file: ", path)
  invisible(path)
}
git_blob <- function(path) {
  require_file(path)
  v <- system2("git", c("hash-object", shQuote(path)), stdout = TRUE)
  if (length(v) != 1L || !is.null(attr(v, "status"))) stop("Hash failed: ", path)
  v
}
require_distinct <- function(baseline, candidate) {
  if (identical(git_blob(baseline), git_blob(candidate))) {
    stop("Baseline and candidate are the same source: ", basename(candidate))
  }
  invisible(TRUE)
}

manifest <- read.table(require_file(file.path(baseline_dir, "MANIFEST.txt")),
                       col.names = c("sha", "path"), stringsAsFactors = FALSE)
pin <- manifest$path[manifest$sha == "pin"]
manifest <- manifest[manifest$sha != "pin", ]
for (i in seq_len(nrow(manifest))) {
  frozen <- file.path(baseline_dir, manifest$path[i])
  if (git_blob(frozen) != manifest$sha[i]) {
    stop("Frozen baseline file changed since the pin: ", manifest$path[i])
  }
  if (startsWith(manifest$path[i], "include/")) {
    current <- file.path(production_dir, manifest$path[i])
    if (git_blob(current) != manifest$sha[i]) {
      stop("Production include differs from the frozen one: ", manifest$path[i],
           ". A parity result would then mix an include change with the ",
           "predictor reuse; refresh the frozen baseline deliberately first.")
    }
  }
}
candidate_hashes <- vapply(stems, function(s) {
  base <- file.path(baseline_dir, paste0(s, ".stan"))
  cand <- require_file(file.path(candidate_dir, candidate_file(s)))
  require_distinct(base, cand)
  git_blob(cand)
}, character(1))

# ---- comparison rule ---------------------------------------------------------

near <- function(a, b, atol = 1e-8, rtol = 1e-10) {
  if (length(a) != length(b)) {
    return(list(pass = FALSE, reason = "length", n = length(a),
                max_abs = NA_real_, max_scaled = NA_real_, worst = NA_character_))
  }
  if (!is.numeric(a) || !is.numeric(b) || !all(is.finite(a)) || !all(is.finite(b))) {
    return(list(pass = FALSE, reason = "nonfinite or non-numeric", n = length(a),
                max_abs = NA_real_, max_scaled = NA_real_, worst = NA_character_))
  }
  if (!length(a)) {
    return(list(pass = TRUE, reason = "", n = 0L, max_abs = 0, max_scaled = 0,
                worst = NA_character_))
  }
  gap <- abs(a - b)
  tol <- atol + rtol * pmax(abs(a), abs(b))
  scaled <- ifelse(tol > 0, gap / tol, ifelse(gap == 0, 0, Inf))
  w <- which.max(scaled)
  list(pass = all(gap <= tol), reason = "", n = length(a), max_abs = max(gap),
       max_scaled = max(scaled), worst = names(a)[w] %||% as.character(w))
}

records <- list()
record <- function(case, quantity, point, jacobian, res) {
  records[[length(records) + 1L]] <<- data.frame(
    case = case, quantity = quantity, point = point,
    jacobian = if (is.na(jacobian)) "" else as.character(jacobian),
    n = res$n, max_abs_err = res$max_abs, max_scaled_err = res$max_scaled,
    worst = res$worst %||% NA_character_, pass = res$pass, reason = res$reason,
    stringsAsFactors = FALSE)
  invisible(res$pass)
}
fail <- function(case, quantity, reason) {
  record(case, quantity, NA_integer_, NA, list(pass = FALSE, reason = reason,
         n = NA_integer_, max_abs = NA_real_, max_scaled = NA_real_,
         worst = NA_character_))
}
write_records <- function() {
  if (length(records)) {
    write.csv(do.call(rbind, records), file.path(out, "parity.csv"),
              row.names = FALSE)
  }
}

# Flatten a named list of arrays to one named vector in the model's variable
# declaration order, column-major within each variable, so the two sides are
# compared coordinate by coordinate under the same names.
flatten <- function(x, order) {
  pieces <- lapply(order, function(nm) {
    v <- x[[nm]]
    if (is.null(v) || !length(v)) return(setNames(numeric(), character()))
    d <- dim(v) %||% length(v)
    idx <- apply(arrayInd(seq_along(v), d), 1L, paste, collapse = ",")
    setNames(as.numeric(v), paste0(nm, "[", idx, "]"))
  })
  unlist(pieces)
}

# ---- compilation -------------------------------------------------------------

source(file.path(dirname(script), "armfast-cmdstan.R"))

parameter_names <- function(stem) {
  if (grepl("mspline", stem, fixed = TRUE)) {
    c("beta_tilde", "u_lscoef", "sigma_smooth")
  } else {
    c("beta_tilde", "aux_raw", "aux2_raw")
  }
}

# ---- one case ----------------------------------------------------------------

run_case <- function(pair, case, tag = case$id) {
  sd <- case$data
  if (!is.list(sd)) stop("Case has no Stan data list: ", tag)
  old <- skeleton(pair$old, sd)
  new <- skeleton(pair$new, sd)

  vars_old <- old$metadata()$stan_variables
  vars_new <- new$metadata()$stan_variables
  if (!identical(vars_old, vars_new) ||
        !identical(old$metadata()$stan_variable_sizes,
                   new$metadata()$stan_variable_sizes)) {
    fail(tag, "schema", "generated-output schema differs")
    return(invisible(FALSE))
  }
  pars <- parameter_names(case$stem)
  sizes <- old$metadata()$stan_variable_sizes
  np <- sum(vapply(pars, function(n) {
    s <- sizes[[n]]
    if (is.null(s)) 0 else prod(s)
  }, numeric(1)))
  if (np < 1) stop("No unconstrained parameters found for ", tag)

  # Drawn under a restored RNG state on purpose: cmdstanr names its temporary
  # CSV files from R's RNG, so a global set.seed() here would give two fits
  # in one minute the same file name, and the finalizer of the older fit
  # would delete the newer fit's file.
  U <- case$upars
  if (is.null(U)) {
    U <- withr::with_seed(2026, rbind(
      rep(0, np), matrix(stats::rnorm(6L * np, sd = 0.3), 6L, np)))
  }
  if (!is.matrix(U) || ncol(U) != np || !nrow(U)) {
    stop("Invalid unconstrained test vectors for ", tag)
  }
  pointwise <- c(paste0("log_lik_ipd[", seq_len(sd$n_ipd), "]"),
                 paste0("log_lik_agd[", seq_len(sd$n_agd), "]"))

  for (k in seq_len(nrow(U))) {
    u <- U[k, ]
    for (jac in c(TRUE, FALSE)) {
      a <- as.numeric(old$log_prob(u, jacobian = jac))
      b <- as.numeric(new$log_prob(u, jacobian = jac))
      names(a) <- names(b) <- "lp"
      record(tag, "log_prob", k, jac, near(a, b))
      ga <- as.numeric(old$grad_log_prob(u, jacobian = jac))
      gb <- as.numeric(new$grad_log_prob(u, jacobian = jac))
      if (length(ga) != np || length(gb) != np) {
        fail(tag, "gradient", "gradient length differs from the parameter count")
        next
      }
      names(ga) <- names(gb) <- paste0("u[", seq_len(np), "]")
      record(tag, "gradient", k, jac, near(ga, gb))
    }
    po <- flatten(old$constrain_variables(u, transformed_parameters = FALSE,
                                          generated_quantities = FALSE), pars)
    pn <- flatten(new$constrain_variables(u, transformed_parameters = FALSE,
                                          generated_quantities = FALSE), pars)
    if (!identical(names(po), names(pn)) || length(po) != np) {
      fail(tag, "constrained_parameters", "parameter names or count differ")
      next
    }
    record(tag, "constrained_parameters", k, NA, near(po, pn, atol = 0, rtol = 0))
    qo <- flatten(old$constrain_variables(u, transformed_parameters = TRUE,
                                          generated_quantities = TRUE), vars_old)
    qn <- flatten(new$constrain_variables(u, transformed_parameters = TRUE,
                                          generated_quantities = TRUE), vars_new)
    if (!identical(names(qo), names(qn))) {
      fail(tag, "generated_quantities", "output names or order differ")
      next
    }
    record(tag, "generated_quantities", k, NA, near(qo, qn))
    if (!all(pointwise %in% names(qo))) {
      fail(tag, "pointwise_log_lik", "pointwise likelihood columns missing")
      next
    }
    record(tag, "pointwise_log_lik", k, NA, near(qo[pointwise], qn[pointwise]))
  }
  invisible(TRUE)
}

# ---- run ---------------------------------------------------------------------

bundle <- readRDS(require_file(cases_file))
cases <- bundle$cases
if (!is.list(cases) || !length(cases)) stop("No test cases in ", cases_file)
ids <- vapply(cases, `[[`, character(1), "id")
if (anyDuplicated(ids)) stop("Duplicate case identifiers.")
if (!is.null(only)) {
  cases <- cases[grepl(only, ids)]
  if (!length(cases)) stop("No case matches --only ", only)
}
used_stems <- unique(vapply(cases, `[[`, character(1), "stem"))
if (!all(used_stems %in% stems)) stop("Unknown model stem in the cases.")

started <- Sys.time()
writeLines(c(paste("Frozen baseline pin:", pin),
             paste("CmdStan:", cmdstan_version),
             paste("cmdstanr:", as.character(utils::packageVersion("cmdstanr"))),
             paste("Cases built:", bundle$meta$created, "with mlumr",
                   bundle$meta$mlumr_version, "at", bundle$meta$git_head),
             capture.output(sessionInfo())),
           file.path(out, "sessionInfo.txt"))

compiled <- list()
for (s in used_stems) {
  t0 <- Sys.time()
  compiled[[s]] <- list(
    old = compile_model(file.path(baseline_dir, paste0(s, ".stan")),
                        baseline_dir, file.path(exe_dir, "baseline")),
    new = compile_model(file.path(candidate_dir, candidate_file(s)),
                        production_dir, file.path(exe_dir, "candidate")))
  cat(sprintf("compiled %s pair in %.0f s\n", s,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

for (case in cases) {
  t0 <- Sys.time()
  run_case(compiled[[case$stem]], case)
  write_records()
  cat(sprintf("%-46s %s (%.0f s)\n", case$id,
              if (all(vapply(records, function(r) r$case != case$id || r$pass,
                             logical(1)))) "pass" else "FAIL",
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
write_records()
parity <- do.call(rbind, records)
parity_failed <- any(!parity$pass)

# ---- mutation controls -------------------------------------------------------

mutations <- list()
note_mutation <- function(name, case, expectation, observed, pass) {
  mutations[[length(mutations) + 1L]] <<- data.frame(
    mutation = name, case = case, expectation = expectation,
    observed = observed, pass = pass, stringsAsFactors = FALSE)
}

if (do_mutations) {
  mutate_source <- function(lines, kind) {
    gq <- grep("^generated quantities", lines)
    if (length(gq) != 1L) stop("Cannot locate the generated quantities block.")
    x <- lines
    tail_idx <- gq:length(x)
    if (kind == "M1_arm_one_everywhere") {
      x <- gsub("eta_agd_arm[a][m]", "eta_agd_arm[1][m]", x, fixed = TRUE)
    } else if (kind == "M2_gq_wrong_grid_point") {
      x[tail_idx] <- gsub("eta_agd_arm[a][m]", "eta_agd_arm[a][1]", x[tail_idx],
                          fixed = TRUE)
    } else if (kind == "M3_gq_reversed_rows") {
      x[tail_idx] <- gsub("log_lik_agd[j] =", "log_lik_agd[n_agd + 1 - j] =",
                          x[tail_idx], fixed = TRUE)
    } else {
      stop("Unknown mutation ", kind)
    }
    if (identical(x, lines)) stop("Mutation ", kind, " changed nothing.")
    x
  }
  # Outcome of a run against a mutant, by quantity.
  outcome <- function(case_id) {
    r <- do.call(rbind, records)
    r <- r[r$case == case_id, ]
    tapply(r$pass, r$quantity, all)
  }
  pick <- function(pattern) {
    hit <- cases[grepl(pattern, ids[match(vapply(cases, `[[`, character(1), "id"), ids)])]
    if (!length(hit)) stop("No case matches ", pattern)
    hit[[1L]]
  }
  stem <- "mlumr_survival_spfa"
  single <- pick("^weibull_spfa_base$")
  multi <- pick("^weibull_spfa_base_multiarm_defensive$")
  src <- readLines(file.path(candidate_dir, candidate_file(stem)),
                   warn = FALSE)
  base_mod <- compiled[[stem]]$old

  for (kind in c("M1_arm_one_everywhere", "M2_gq_wrong_grid_point",
                 "M3_gq_reversed_rows")) {
    mdir <- file.path(exe_dir, "mutants", kind)
    dir.create(mdir, recursive = TRUE, showWarnings = FALSE)
    mfile <- file.path(mdir, candidate_file(stem))
    writeLines(mutate_source(src, kind), mfile)
    mutant <- compile_model(mfile, production_dir, mdir)
    pair <- list(old = base_mod, new = mutant)
    if (kind == "M1_arm_one_everywhere") {
      run_case(pair, single, tag = paste0(kind, ":single_arm"))
      o <- outcome(paste0(kind, ":single_arm"))
      note_mutation(kind, single$id, "passes on one arm (the lookup is then correct)",
                    paste(names(o)[!o], collapse = ","), all(o))
      run_case(pair, multi, tag = paste0(kind, ":multi_arm"))
      o <- outcome(paste0(kind, ":multi_arm"))
      note_mutation(kind, multi$id, "fails log_prob and gradient on three arms",
                    paste(names(o)[!o], collapse = ","),
                    !o[["log_prob"]] && !o[["gradient"]])
    } else {
      run_case(pair, single, tag = kind)
      o <- outcome(kind)
      note_mutation(kind, single$id,
                    "log_prob, gradient, and parameters pass; generated quantities and pointwise fail",
                    paste(names(o)[!o], collapse = ","),
                    o[["log_prob"]] && o[["gradient"]] &&
                      o[["constrained_parameters"]] &&
                      !o[["generated_quantities"]] && !o[["pointwise_log_lik"]])
    }
  }
  # Rules exercised directly.
  r4 <- tryCatch(require_file(file.path(parity_dir, "no-such-model.stan")),
                 error = function(e) "error")
  note_mutation("M4_missing_required_file", "", "stops", format(r4),
                identical(r4, "error"))
  same <- file.path(candidate_dir, candidate_file(stem))
  r5 <- tryCatch(require_distinct(same, same), error = function(e) "error")
  note_mutation("M5_identical_pair", "", "stops", format(r5), identical(r5, "error"))
  r6 <- near(c(a = 1, b = Inf), c(a = 1, b = Inf))
  note_mutation("M6_nonfinite_both_sides", "", "fails", r6$reason, !r6$pass)
  r7 <- near(c(a = 1, b = NaN), c(a = 1, b = NaN))
  note_mutation("M7_nan_both_sides", "", "fails", r7$reason, !r7$pass)

  mut <- do.call(rbind, mutations)
  write.csv(mut, file.path(out, "mutations.csv"), row.names = FALSE)
  print(mut[, c("mutation", "expectation", "observed", "pass")], row.names = FALSE)
  # Mutant rows are not parity evidence; keep them in parity.csv for the
  # record but judge the parity verdict on the real cases only.
  parity_failed <- any(!parity$pass[!grepl("^M[0-9]_", parity$case)])
  write_records()
}

# ---- verdict -----------------------------------------------------------------

real <- parity[!grepl("^M[0-9]_", parity$case), ]
cat(sprintf("\n%d cases, %d comparisons, %d failed; max abs error %.3g, max scaled error %.3g\n",
            length(cases), nrow(real), sum(!real$pass),
            max(real$max_abs_err, na.rm = TRUE),
            max(real$max_scaled_err, na.rm = TRUE)))
mutations_failed <- length(mutations) && !all(vapply(mutations, `[[`, logical(1), "pass"))
if (parity_failed || mutations_failed) {
  stop("Parity gate failed; see ", file.path(out, "parity.csv"))
}
writeLines(c(
  sprintf("Native parity gate passed on %d cases (%d comparisons) in %.0f s.",
          length(cases), nrow(real),
          as.numeric(difftime(Sys.time(), started, units = "secs"))),
  paste("Frozen baseline pin:", pin),
  paste("CmdStan", cmdstan_version, "cmdstanr",
        as.character(utils::packageVersion("cmdstanr"))),
  paste("Acceptance: |x - y| <= 1e-8 + 1e-10 * max(|x|, |y|) componentwise;",
        "constrained parameters exact"),
  sprintf("Max absolute error %.3g, max scaled error %.3g",
          max(real$max_abs_err, na.rm = TRUE), max(real$max_scaled_err, na.rm = TRUE)),
  if (length(mutations)) sprintf("Mutation controls: %d of %d behaved as expected",
                                 sum(vapply(mutations, `[[`, logical(1), "pass")),
                                 length(mutations)),
  "Candidate sources:",
  paste0("  ", candidate_hashes, " inst/stan/", candidate_file(names(candidate_hashes))),
  "Cases:",
  paste0("  ", vapply(cases, `[[`, character(1), "id"))),
  file.path(out, "SUCCESS.txt"))
cat("SUCCESS\n")
