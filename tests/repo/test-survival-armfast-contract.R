# Source contract for the four survival models: each production model in
# inst/stan is the frozen pre-reuse model in
# tests/parity/survival-baseline-6a366a0/ with one change, the comparator linear
# predictor computed once per arm before the pseudo-individual loop and reused
# inside it, in the likelihood and in the generated quantities. Since the data
# flag `reuse_arm_eta` (mlumr(optimization =)) selects between the two, the
# contract is checked for each setting: resolving the flag to 0 on the
# production text must give back the frozen text, and resolving it to 1 must
# give the frozen text plus the reuse, token for token, ignoring comments and
# whitespace. Pure R, no Stan compilation; the numerical parity gate is
# tests/parity/armfast-parity-gate.R.
#
# Missing inputs fail rather than skip: a skipped contract would let CI pass
# with no contract checked.

.root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
.baseline_dir <- file.path(.root, "tests", "parity", "survival-baseline-6a366a0")
.production_dir <- file.path(.root, "inst", "stan")

.stems <- c("mlumr_survival_spfa", "mlumr_survival_relaxed",
            "mlumr_survival_mspline_spfa", "mlumr_survival_mspline_relaxed")

# Executable text: comments removed, whitespace normalized, no spaces next to
# punctuation, so that a line break inside an argument list does not count.
.executable_text <- function(path) {
  x <- paste(readLines(path, warn = FALSE), collapse = "\n")
  x <- gsub("/\\*.*?\\*/", " ", x, perl = TRUE)
  x <- gsub("//[^\n]*", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- gsub(" ?([][(){},;=*+/<>-]) ?", "\\1", x, perl = TRUE)
  trimws(x)
}

# Replace every occurrence of `from` by `to`, failing unless there were exactly
# `n`: a pattern that stops matching would otherwise make the contract vacuous.
.swap <- function(text, from, to, n, info) {
  hits <- lengths(regmatches(text, gregexpr(from, text, fixed = TRUE)))
  expect_identical(hits, as.integer(n), info = paste(info, from))
  gsub(from, to, text, fixed = TRUE)
}

.flag_data <- "int<lower=0,upper=1>reuse_arm_eta;"
.fill <- "for(a in 1:n_agd_rows)eta_agd_arm[a]=Xq_int[a]*beta_tilde;"
.per_row <- "if(!reuse_arm_eta)eta_agd_arm[a]=Xq_int[a]*beta_tilde;"

# The text the model executes with reuse_arm_eta = 1.
.resolve_on <- function(text, info) {
  text <- .swap(text, .flag_data, "", 1, info)
  text <- .swap(text, paste0("if(reuse_arm_eta)", .fill), .fill, 2, info)
  .swap(text, .per_row, "", 2, info)
}

# The text the model executes with reuse_arm_eta = 0: no cache, and the
# per-row predictor read under its old name.
.resolve_off <- function(text, info) {
  text <- .swap(text, .flag_data, "", 1, info)
  text <- .swap(text, paste0("if(reuse_arm_eta)", .fill), "", 2, info)
  text <- .swap(text, "array[n_agd_rows]vector[n_int]eta_agd_arm;", "", 2, info)
  text <- .swap(text, .per_row, "vector[n_int]eta=Xq_int[a]*beta_tilde;", 2,
                info)
  .swap(text, "eta_agd_arm[a][m]", "eta[m]", 2, info)
}

# Reverse the predictor reuse in the reuse-only text: drop the per-arm cache,
# restore the per-row predictor after the arm lookup, and read from it again.
.undo_reuse <- function(text) {
  cut <- c("array[n_agd_rows]vector[n_int]eta_agd_arm;", .fill)
  for (s in cut) text <- gsub(s, "", text, fixed = TRUE)
  text <- gsub("eta_agd_arm[a][m]", "eta[m]", text, fixed = TRUE)
  gsub("int a=agd_arm[j];", "int a=agd_arm[j];vector[n_int]eta=Xq_int[a]*beta_tilde;",
       text, fixed = TRUE)
}

.git_blob <- function(path) {
  system2("git", c("hash-object", shQuote(path)), stdout = TRUE)
}

test_that("the frozen baseline matches its manifest", {
  expect_true(nzchar(Sys.which("git")), info = "git is required for blob hashes")
  m <- read.table(file.path(.baseline_dir, "MANIFEST.txt"),
                  col.names = c("sha", "path"), stringsAsFactors = FALSE)
  expect_identical(m$path[1], "6a366a045b597b6659917badf600e50afedff41a")
  m <- m[-1, ]
  expect_length(m$path, 12L)
  for (i in seq_len(nrow(m))) {
    f <- file.path(.baseline_dir, m$path[i])
    expect_true(file.exists(f), info = m$path[i])
    expect_identical(.git_blob(f), m$sha[i], info = m$path[i])
  }
})

test_that("with the flag off each production model is the frozen baseline", {
  for (s in .stems) {
    base <- file.path(.baseline_dir, paste0(s, ".stan"))
    cand <- file.path(.production_dir, paste0(s, ".stan"))
    expect_true(file.exists(base), info = s)
    expect_true(file.exists(cand), info = s)
    expect_identical(.resolve_off(.executable_text(cand), s),
                     .executable_text(base), info = s)
  }
})

test_that("with the flag on each production model is the baseline plus the reuse", {
  for (s in .stems) {
    base <- file.path(.baseline_dir, paste0(s, ".stan"))
    cand <- file.path(.production_dir, paste0(s, ".stan"))
    on <- .resolve_on(.executable_text(cand), s)
    expect_identical(.undo_reuse(on), .executable_text(base), info = s)
    # The reuse is present in both places it belongs: the model block and the
    # generated quantities. Nothing else is cached.
    expect_identical(lengths(regmatches(
      on, gregexpr("array[n_agd_rows]vector[n_int]eta_agd_arm;", on,
                   fixed = TRUE))), 2L, info = s)
    expect_false(grepl("exp(eta_agd_arm", on, fixed = TRUE), info = s)
    expect_false(grepl("surv_ll_status_pre", on, fixed = TRUE), info = s)
  }
})

test_that("the survival models keep the stratified production contract", {
  files <- file.path(.production_dir, paste0(.stems, ".stan"))
  expect_true(all(file.exists(files)))
  expect_length(files, 4L)
  for (f in files) {
    code <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_true(grepl("n_strata", code, fixed = TRUE), info = basename(f))
    if (grepl("mspline", basename(f), fixed = TRUE)) {
      expect_true(grepl("matrix[n_scoef - 1, n_strata] lscoef_prior_mean",
                        code, fixed = TRUE), info = basename(f))
      expect_true(grepl("mspline_ll_status", code, fixed = TRUE),
                  info = basename(f))
    } else {
      expect_true(grepl("matrix<lower=0>[nonexp, n_strata] aux_raw",
                        code, fixed = TRUE), info = basename(f))
      expect_true(grepl("surv_ll_status", code, fixed = TRUE),
                  info = basename(f))
    }
  }
})
