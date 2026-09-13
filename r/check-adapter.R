#!/usr/bin/env Rscript
# Checks the browser's mlumr adapter natively, using the same R files the
# browser loads (build/site/r after `lesson.sh build`).
#
#   Rscript r/check-adapter.R SITE_R_DIR CELL_CODE [--native=CHECKOUT] [--write=DIR]
#
# SITE_R_DIR  the built r/ folder: R/mlumr.R, NAMESPACE, DESCRIPTION, inst/stan, load.R.
# CELL_CODE   a file holding the workflow cell's R code (lesson.sh extracts it).
# --native    an mlumr checkout at the pinned commit with a built DLL in src/.
#             Its own mlumr() is run with the backend replaced by a capture, so
#             the browser's Stan data and conditions are compared with the
#             package's, not with themselves.
# --write     write the browser Stan data for each model, for browser-qa.mjs.
args <- commandArgs(trailingOnly = TRUE)
flag <- function(name) sub(paste0("^--", name, "="), "", grep(paste0("^--", name, "="), args, value = TRUE))
positional <- args[!grepl("^--", args)]
if (length(positional) != 2L) stop("Usage: Rscript r/check-adapter.R SITE_R_DIR CELL_CODE [--native=DIR] [--write=DIR]")

check <- function(ok, what) {
  cat(if (isTRUE(ok)) "ok  " else "FAIL", what, "\n")
  if (!isTRUE(ok)) quit(status = 1)
}

# The browser has mlumr's sources but no installed package. Hide an installed
# copy here, or a lookup of the package that fails in webR would pass natively.
hidden <- file.path(tempdir(), "library-without-mlumr")
dir.create(hidden, showWarnings = FALSE)
for (lib in .libPaths()) {
  for (pkg in setdiff(list.files(lib), "mlumr")) {
    if (!file.exists(file.path(hidden, pkg))) file.symlink(file.path(lib, pkg), file.path(hidden, pkg))
  }
}
# .libPaths(new) always appends R's own library, which can hold mlumr too, so
# the search path is set where .libPaths() keeps it.
search_path <- function(paths) assign(".lib.loc", paths, envir = environment(.libPaths))
search_path(hidden)
check(!nzchar(system.file(package = "mlumr")), "no installed mlumr is visible, as in the browser")

options(lesson.mlumr_dir = normalizePath(positional[1], mustWork = TRUE))
source(file.path(getOption("lesson.mlumr_dir"), "load.R"))
search_path(unique(c(.libPaths()[1], hidden)))
source(positional[2], local = globalenv())

# Everything the browser side does runs before a native copy is loaded, so no
# native namespace can stand in for something the browser lacks.
browser <- function(data, model) jsonlite::fromJSON(lesson_prepare_fit(data, model), simplifyVector = FALSE)
models <- c("spfa", "relaxed")
stan_data <- list()
for (model in models) {
  prepared <- browser(dat, model)
  check(isTRUE(prepared$ok), paste(model, "prepares through mlumr()", if (!isTRUE(prepared$ok)) prepared$error))
  check(isTRUE(prepared$benchmarks$naive$valid) && isTRUE(prepared$benchmarks$stc$valid),
        paste(model, "benchmarks are finite"))
  stan_data[[model]] <- lesson_stan_data(dat, model)$stan_data
  direct <- .mlumr_build_stan_data(
    data = dat, family = "binomial", link_info = check_link("binomial", NULL),
    prior_intercept = prior_normal(0, 2.5), prior_beta = prior_normal(0, 1),
    prior_beta_comparator = NULL, prior_sigma = default_prior_sigma(),
    surv_info = NULL, prior_aux = NULL, prior_aux2 = NULL, prior_smooth = NULL,
    n_knots = 7L, knots = NULL, pred_times = NULL, rmst_horizon = NULL,
    n_rmst_grid = 100L, aux_by = ".study", model = model, center = TRUE, qr = FALSE
  )$stan_data
  check(identical(stan_data[[model]], direct), paste(model, "Stan data equal the internal builder's"))
  if (length(flag("write"))) {
    dir.create(flag("write"), showWarnings = FALSE, recursive = TRUE)
    writeLines(prepared$stan, file.path(flag("write"), paste0("stan-data-", model, ".json")))
  }
}

# The same refusals and warnings as a native fit.
one_row <- set_agd(trial_b[1, ], treatment = "trt", family = "binomial",
                   outcome_n = "n", outcome_r = "events", cov_means = "x_mean",
                   cov_sds = "x_sd", cov_types = "continuous", study = "study")
cases <- list(
  "no dat object" = list(data = NULL, model = "spfa", refuse = TRUE),
  "no integration points" = list(data = combine_data(ipd, agd), model = "spfa", refuse = TRUE),
  "relaxed with one aggregate row" = list(
    data = suppressMessages(add_integration(combine_data(ipd, one_row), n_int = 64,
                                            x = distr(qnorm, mean = x_mean, sd = x_sd))),
    model = "relaxed", refuse = FALSE, warn = TRUE)
)
got <- lapply(cases, function(case) browser(case$data, case$model))
for (name in names(cases)) {
  check(isTRUE(!got[[name]]$ok) == cases[[name]]$refuse, paste(name, if (cases[[name]]$refuse) "is refused" else "is accepted"))
  if (isTRUE(cases[[name]]$warn)) check(length(got[[name]]$warnings) > 0, paste(name, "warns"))
}
typed <- tryCatch(mlumr(dat, seed = 2026), error = conditionMessage)
check(grepl("Use the Fit button", typed), "a typed mlumr() call explains the browser limit")

if (length(flag("native"))) {
  # Not attached, so the unqualified calls above keep reaching the browser's copy.
  pkgload::load_all(flag("native"), compile = FALSE, quiet = TRUE, export_all = FALSE, attach = FALSE)
  ns <- asNamespace("mlumr")
  unlockBinding(".mlumr_fit_backend", ns)
  assign(".mlumr_fit_backend", function(engine, model_name, stan_data, ...) {
    stop(structure(class = c("native_stan_data", "error", "condition"),
                   list(message = "", call = NULL, stan_data = stan_data)))
  }, envir = ns)
  native <- function(data, model) {
    warns <- character()
    value <- tryCatch(withCallingHandlers(
      ns$mlumr(data, model = model, prior_intercept = ns$prior_normal(0, 2.5),
               prior_beta = ns$prior_normal(0, 1), chains = 2L, iter = 1000L,
               warmup = 500L, seed = 2026L, adapt_delta = 0.95,
               max_treedepth = 15L, engine = "rstan", verbose = FALSE),
      message = function(m) invokeRestart("muffleMessage"),
      warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
      native_stan_data = function(c) c, error = function(e) e)
    list(stan_data = if (inherits(value, "native_stan_data")) value$stan_data,
         error = if (!inherits(value, "native_stan_data")) conditionMessage(value),
         warnings = warns)
  }
  for (model in models) {
    check(identical(stan_data[[model]], native(dat, model)$stan_data),
          paste(model, "Stan data equal the native package's mlumr()"))
  }
  for (name in names(cases)) {
    nat <- native(cases[[name]]$data, cases[[name]]$model)
    check(identical(is.null(nat$error), isTRUE(got[[name]]$ok)), paste(name, "matches the native accept or refuse"))
    if (!isTRUE(got[[name]]$ok)) check(identical(nat$error, got[[name]]$error), paste(name, "gives the native message"))
    check(identical(as.character(unlist(got[[name]]$warnings)), nat$warnings), paste(name, "gives the native warnings"))
  }
}
cat("All adapter checks passed.\n")
