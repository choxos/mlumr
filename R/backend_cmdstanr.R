#' Check that a CmdStan run left draws behind before reading them
#'
#' cmdstanr reports a queued chain's intended CSV path as readable although
#' the chain never ran, and `fit$draws()` then aborts inside `checkmate` on a
#' path under `tempdir()`. A chain that ran and failed is dropped by cmdstanr
#' and is not an error here.
#'
#' @param files The output paths the fit reports as readable.
#' @param chains The number of chains requested.
#' @param retrieval_error The message from asking the fit for its output
#'   paths, when that itself failed, or `NULL`.
#' @param return_codes CmdStan's per-chain return codes, or `NULL`.
#' @return `TRUE` invisibly. Stops when there is nothing to read.
#' @noRd
.assert_cmdstan_output <- function(files, chains, retrieval_error = NULL,
                                   return_codes = NULL) {
  files <- as.character(files)
  absent <- files[!file.exists(files)]
  if (!length(absent) && length(files)) return(invisible(TRUE))
  detail <- if (length(absent)) {
    sprintf("%d of %d chain(s) reported output that is not on disk (%s)",
            length(absent), chains,
            paste(basename(absent), collapse = ", "))
  } else {
    sprintf("none of the %d chain(s) produced output", chains)
  }
  evidence <- character(0)
  if (length(retrieval_error) && nzchar(retrieval_error[1L])) {
    evidence <- c(evidence, paste0("asking cmdstanr for the output paths ",
                                   "failed with: ", retrieval_error[1L]))
  }
  codes <- suppressWarnings(as.integer(return_codes))
  codes <- codes[!is.na(codes)]
  if (length(codes)) {
    evidence <- c(evidence, paste0("CmdStan return code(s) ",
                                   paste(codes, collapse = ", ")))
  }
  evidence <- if (length(evidence)) {
    paste0(" What is known about it: ", paste(evidence, collapse = "; "), ".")
  } else {
    " Nothing further was reported about it."
  }
  stop(sprintf(paste0(
    "The Stan run left no draws to read: %s.%s Re-run with `verbose = TRUE` ",
    "to see CmdStan's own messages, and set `output_dir` to keep them."
  ), detail, evidence), call. = FALSE)
}

#' Fit a Stan model using cmdstanr
#' @noRd
fit_cmdstanr <- function(model_name, stan_data, chains, iter, warmup,
                         seed, adapt_delta, max_treedepth, refresh,
                         verbose = TRUE, ...) {

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("cmdstanr is required for engine = 'cmdstanr' but is not installed; see ?mlumr_engine.",
         call. = FALSE)
  }

  # Locate .stan file from installed package
  stan_file <- system.file("stan", paste0(model_name, ".stan"), package = "mlumr")
  if (stan_file == "") {
    # Development mode fallback
    stan_file <- file.path("inst", "stan", paste0(model_name, ".stan"))
  }
  if (!file.exists(stan_file)) {
    stop(sprintf("Cannot find Stan model file: %s.stan", model_name), call. = FALSE)
  }

  # Keep CmdStan executables out of inst/stan.
  compile_dir <- .cmdstanr_compile_dir(model_name, stan_file)
  mod <- cmdstanr::cmdstan_model(stan_file, dir = compile_dir)

  # Mirror rstan's mc.cores default for parallel_chains unless overridden.
  dots <- list(...)
  if (!"parallel_chains" %in% names(dots)) {
    dots$parallel_chains <- min(chains,
                                max(1L, as.integer(getOption("mc.cores", 1L))))
  }
  # cmdstanr's chain banner goes to stdout, so `verbose` controls it here;
  # `show_exceptions` is newer than `show_messages`, so ask what the method
  # accepts.
  sample_formals <- names(formals(mod$sample))
  for (nm in intersect(c("show_messages", "show_exceptions"), sample_formals)) {
    if (!nm %in% names(dots)) dots[[nm]] <- isTRUE(verbose)
  }

  sample_args <- c(
    list(
      data = stan_data,
      chains = chains,
      iter_sampling = iter - warmup,
      iter_warmup = warmup,
      seed = seed,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth,
      refresh = refresh
    ),
    dots
  )
  fit <- do.call(mod$sample, sample_args)
  # `return_codes()` is missing from older cmdstanr and can throw itself.
  retrieval <- NULL
  produced <- tryCatch(fit$output_files(include_failed = FALSE),
                       error = function(e) {
                         retrieval <<- conditionMessage(e)
                         character(0)
                       })
  .assert_cmdstan_output(
    produced, chains,
    retrieval_error = retrieval,
    return_codes = tryCatch(fit$return_codes(), error = function(e) NULL)
  )

  # Keep the per-draw chain id before dropping the metadata columns.
  draws_df <- as.data.frame(fit$draws(format = "df"))
  chain_ids <- if (".chain" %in% names(draws_df)) {
    as.integer(draws_df$.chain)
  } else {
    NULL
  }
  meta_cols <- c(".chain", ".iteration", ".draw")
  draws_df <- draws_df[, !names(draws_df) %in% meta_cols, drop = FALSE]

  # Summary with rstan's column names. `n_eff` is posterior::ess_bulk here and
  # rstan's classic n_eff there, so the two engines' ESS columns differ slightly.
  if (requireNamespace("posterior", quietly = TRUE)) {
    cmdstan_summ <- fit$summary(
      variables = NULL,
      mean = mean,
      # The Monte Carlo SE of the mean needs the ESS of the draws themselves;
      # bulk ESS is computed on rank-normalized draws, a different quantity.
      se_mean = posterior::mcse_mean,
      sd = stats::sd,
      `2.5%` = function(.x) stats::quantile(.x, 0.025),
      `25%` = function(.x) stats::quantile(.x, 0.25),
      `50%` = function(.x) stats::quantile(.x, 0.50),
      `75%` = function(.x) stats::quantile(.x, 0.75),
      `97.5%` = function(.x) stats::quantile(.x, 0.975),
      n_eff = posterior::ess_bulk,
      ess_tail = posterior::ess_tail,
      Rhat = posterior::rhat
    )
    summary_df <- as.data.frame(cmdstan_summ)
  } else {
    warning("posterior package not installed. n_eff, ess_tail, and Rhat will be NA.",
            call. = FALSE)
    var_names <- colnames(draws_df)
    summary_df <- data.frame(
      variable = var_names,
      mean = vapply(draws_df, mean, numeric(1)),
      se_mean = rep(NA_real_, length(var_names)),
      sd = vapply(draws_df, stats::sd, numeric(1)),
      `2.5%` = vapply(draws_df, function(.x) stats::quantile(.x, 0.025), numeric(1)),
      `25%` = vapply(draws_df, function(.x) stats::quantile(.x, 0.25), numeric(1)),
      `50%` = vapply(draws_df, function(.x) stats::quantile(.x, 0.50), numeric(1)),
      `75%` = vapply(draws_df, function(.x) stats::quantile(.x, 0.75), numeric(1)),
      `97.5%` = vapply(draws_df, function(.x) stats::quantile(.x, 0.975), numeric(1)),
      n_eff = rep(NA_real_, length(var_names)),
      ess_tail = rep(NA_real_, length(var_names)),
      Rhat = rep(NA_real_, length(var_names)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  # Diagnostics
  diag <- fit$diagnostic_summary(quiet = TRUE)
  n_divergent <- sum(diag$num_divergent)
  n_max_td <- sum(diag$num_max_treedepth)

  list(
    native_fit = fit,
    draws = draws_df,
    chain_ids = chain_ids,
    summary_df = summary_df,
    n_divergent = n_divergent,
    n_max_td = n_max_td,
    n_chains_requested = as.integer(chains),
    n_chains_returned = .n_chains_returned(chain_ids, chains)
  )
}


#' Number of chains present in the returned draws
#'
#' Both backends return a fit assembled from the surviving chains when one
#' terminates abnormally. `NA` when the draws could not be labeled by chain.
#' @noRd
.n_chains_returned <- function(chain_ids, chains) {
  if (is.null(chain_ids) || !length(chain_ids)) {
    return(NA_integer_)
  }
  length(unique(chain_ids))
}


#' Cache directory for cmdstanr-compiled model executables
#' @noRd
.cmdstanr_compile_dir <- function(model_name, stan_file) {
  stan_dir <- dirname(stan_file)
  include_dir <- file.path(stan_dir, "include")
  include_files <- if (dir.exists(include_dir)) {
    sort(list.files(include_dir, pattern = "[.]stan$", full.names = TRUE))
  } else {
    character()
  }

  source_files <- c(stan_file, include_files)
  cache_key <- .cmdstanr_cache_key(source_files)
  cache_dir <- file.path(.cmdstanr_cache_root(), paste0(model_name, "-", cache_key))

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(cache_dir) || file.access(cache_dir, mode = 2L) != 0L) {
    stop("Could not create a writable cmdstanr cache directory.", call. = FALSE)
  }
  cache_dir
}


#' Resolve a writable cmdstanr cache root
#' @noRd
.cmdstanr_cache_root <- function() {
  candidates <- c(
    file.path(tools::R_user_dir("mlumr", "cache"), "cmdstanr"),
    file.path(tempdir(), "mlumr", "cmdstanr")
  )

  selected <- NULL
  for (cache_root in candidates) {
    if (!dir.exists(cache_root)) {
      dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
    }
    if (dir.exists(cache_root) && file.access(cache_root, mode = 2L) == 0L) {
      selected <- cache_root
      break
    }
  }

  if (!is.null(selected)) {
    selected
  } else {
    stop("Could not create a writable cmdstanr cache root.", call. = FALSE)
  }
}


#' Cache key covering every Stan source the model is built from
#'
#' A digest of every source file's content plus the CmdStan version, its
#' path and its `make/local`, so an edited include or a different build is a
#' different executable.
#' @param source_files Character vector of `.stan` paths; the main model first,
#'   then its includes in a stable order.
#' @return A 32-character key.
#' @noRd
.cmdstanr_cache_key <- function(source_files) {
  digests <- unname(tools::md5sum(source_files))
  # A missing file must not silently collapse to a shared key.
  digests[is.na(digests)] <- "MISSING"
  cmdstan <- tryCatch(as.character(cmdstanr::cmdstan_version()),
                      error = function(e) "unknown")
  cmdstan_path <- tryCatch(cmdstanr::cmdstan_path(), error = function(e) "unknown")
  make_local <- file.path(cmdstan_path, "make", "local")
  make_local <- if (file.exists(make_local)) {
    unname(tools::md5sum(make_local))
  } else {
    "none"
  }
  payload <- c(paste0("cmdstan=", cmdstan),
               paste0("cmdstan_path=", cmdstan_path),
               paste0("make_local=", make_local),
               paste0(basename(source_files), "=", digests))
  tmp <- tempfile("mlumr-cache-key-")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(payload, tmp)
  key <- unname(tools::md5sum(tmp))
  if (is.na(key)) {
    # A distinct directory recompiles, which is slow but correct.
    key <- paste0("nokey-", as.integer(Sys.time()), "-", Sys.getpid())
  }
  key
}
