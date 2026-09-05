#' Fit a Stan model using cmdstanr
#' @keywords internal
fit_cmdstanr <- function(model_name, stan_data, chains, iter, warmup,
                         seed, adapt_delta, max_treedepth, refresh,
                         verbose = TRUE, ...) {

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("cmdstanr is required but not installed. Run mlumr_engine('cmdstanr') to set up.",
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

  # Keep CmdStan executables out of inst/stan. cmdstanr's default executable
  # path is next to the .stan file, which pollutes the source tree in
  # development and installed package directories in some workflows.
  compile_dir <- .cmdstanr_compile_dir(model_name, stan_file)
  mod <- cmdstanr::cmdstan_model(stan_file, dir = compile_dir)

  # Sample (note arg name differences from rstan).
  # Default parallel_chains to mirror rstan's behavior (which uses mc.cores).
  # cmdstanr defaults parallel_chains = 1, so chains run serially unless we
  # set it explicitly. Cap at `chains` so we never request more workers than
  # chains. Honor any user override passed through `...`.
  dots <- list(...)
  if (!"parallel_chains" %in% names(dots)) {
    dots$parallel_chains <- min(chains,
                                max(1L, as.integer(getOption("mc.cores", 1L))))
  }
  # cmdstanr writes its "Running MCMC with N chains / Chain k finished in ..."
  # banner to STDOUT, not through the condition system, so `refresh = 0` does
  # not stop it and neither does suppressMessages(). That is fifteen lines per
  # fit, which buries anything real in a loop of a few hundred fits. `verbose`
  # is the argument a caller already reaches for, so honor it here too, while
  # leaving an explicit `show_messages` / `show_exceptions` in `...` to win.
  # `show_exceptions` was added to cmdstanr later than `show_messages`, so ask
  # the method what it accepts rather than assuming a version.
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

  # Extract draws as plain data.frame (drop metadata columns). Keep the real
  # per-draw chain id before dropping it, so diagnostics (loo r_eff) use the
  # actual chain labels rather than reconstructing them from row ordering.
  draws_df <- as.data.frame(fit$draws(format = "df"))
  chain_ids <- if (".chain" %in% names(draws_df)) {
    as.integer(draws_df$.chain)
  } else {
    NULL
  }
  meta_cols <- c(".chain", ".iteration", ".draw")
  draws_df <- draws_df[, !names(draws_df) %in% meta_cols, drop = FALSE]

  # Build summary matching rstan column names:
  # variable, mean, se_mean, sd, 2.5%, 25%, 50%, 75%, 97.5%, n_eff, Rhat
  #
  # Note: `n_eff` and `se_mean` are necessarily engine-defined. Here n_eff is
  # posterior::ess_bulk and se_mean = sd / sqrt(ess_bulk); rstan reports its own
  # classic n_eff and Monte Carlo se_mean. The two are close but not identical,
  # so head-to-head ESS / se_mean tables across engines are not exactly
  # comparable. All downstream estimates/CIs are recomputed from raw draws and
  # are unaffected.
  if (requireNamespace("posterior", quietly = TRUE)) {
    cmdstan_summ <- fit$summary(
      variables = NULL,
      mean = mean,
      se_mean = function(.x) stats::sd(.x) / sqrt(posterior::ess_bulk(.x)),
      sd = stats::sd,
      `2.5%` = function(.x) stats::quantile(.x, 0.025),
      `25%` = function(.x) stats::quantile(.x, 0.25),
      `50%` = function(.x) stats::quantile(.x, 0.50),
      `75%` = function(.x) stats::quantile(.x, 0.75),
      `97.5%` = function(.x) stats::quantile(.x, 0.975),
      n_eff = posterior::ess_bulk,
      # Bulk ESS does not speak for the tails, and the reported 2.5% / 97.5%
      # quantiles are tail quantities. check_diagnostics() tests this column;
      # without it that check was dead for every fit the package produced.
      ess_tail = posterior::ess_tail,
      Rhat = posterior::rhat
    )
    summary_df <- as.data.frame(cmdstan_summ)
  } else {
    # Fallback when posterior is not installed: basic summary with NA for
    # ESS and Rhat. We deliberately do not impute n_eff with the total
    # draw count (or any other plug-in) because that would silently report
    # wildly optimistic sample sizes and mask autocorrelation. Installing
    # 'posterior' is required for valid convergence diagnostics.
    warning(
      "posterior package not installed. ESS (n_eff) and Rhat will be NA. ",
      "Install with: install.packages('posterior') for valid diagnostics.",
      call. = FALSE
    )
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


#' Number of chains actually present in the returned draws
#'
#' A chain can terminate abnormally (for example a numerically fragile
#' likelihood whose gradient fails to evaluate). Both backends then return a fit
#' assembled from the surviving chains only, so the posterior silently
#' represents fewer chains than were requested. Counting the distinct chain ids
#' in the draws is the one check that catches this on either engine.
#'
#' @param chain_ids Per-draw chain labels, or `NULL` when unavailable.
#' @param chains Number of chains requested.
#' @return Integer count of chains present.
#' @keywords internal
.n_chains_returned <- function(chain_ids, chains) {
  # `NULL` means the backend could not label the draws by chain, which happens
  # exactly when the draw count does not divide by the fitted chain count: the
  # abnormal layout this diagnostic exists to notice. Returning the REQUESTED
  # count there asserts that every chain came back, which is the one thing not
  # known. Report it as unknown and let the caller say so.
  if (is.null(chain_ids) || !length(chain_ids)) {
    return(NA_integer_)
  }
  length(unique(chain_ids))
}


#' Cache directory for cmdstanr-compiled model executables
#' @keywords internal
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
#' @keywords internal
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
#' The key used to be `substr(paste0(md5s), 1, 32)`. An MD5 digest is already
#' 32 characters, so that expression returns the FIRST digest and discards
#' every other one: the key was the main model's hash alone, and editing an
#' include left it unchanged. A user upgrading to a version whose only change
#' was inside an include, with the main file untouched and no newer than the
#' cached executable, could keep running a binary compiled from the old
#' likelihood.
#'
#' The key is now a digest of a canonical payload naming every source and its
#' content, plus the CmdStan version, since the same sources built against a
#' different CmdStan are a different executable. Hashing goes through a temp
#' file so this needs no dependency beyond base R.
#'
#' @param source_files Character vector of `.stan` paths; the main model first,
#'   then its includes in a stable order.
#' @return A 32-character key.
#' @keywords internal
.cmdstanr_cache_key <- function(source_files) {
  digests <- unname(tools::md5sum(source_files))
  # A missing file must not silently collapse to a shared key.
  digests[is.na(digests)] <- "MISSING"
  cmdstan <- tryCatch(as.character(cmdstanr::cmdstan_version()),
                      error = function(e) "unknown")
  payload <- c(paste0("cmdstan=", cmdstan),
               paste0(basename(source_files), "=", digests))
  tmp <- tempfile("mlumr-cache-key-")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(payload, tmp)
  key <- unname(tools::md5sum(tmp))
  if (is.na(key)) {
    # Never fall back to a key that ignores the includes; a distinct random
    # directory recompiles, which is slow but correct.
    key <- paste0("nokey-", as.integer(Sys.time()), "-", Sys.getpid())
  }
  key
}
