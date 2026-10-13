# cmdstanr helpers shared by the parity gate and the benchmark: compile a
# model with its model methods available across R sessions, and open a
# data-bound fit skeleton without sampling. Not a test file; source it from a
# script that does not load mlumr (see armfast-parity-gate.R for why).

compile_model <- function(stan_file, include_path, dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  stem <- sub("[.]stan$", "", basename(stan_file))
  exe <- file.path(dir, stem)
  hpp <- file.path(dir, paste0(stem, ".hpp"))
  # An include edit changes the model as surely as a model edit, so the
  # executable is reused only while it is newer than both.
  sources <- c(stan_file, list.files(file.path(include_path, "include"),
                                     pattern = "[.]stan$", full.names = TRUE))
  reuse <- file.exists(exe) && file.exists(hpp) &&
    file.mtime(exe) >= max(file.mtime(sources))
  mod <- cmdstanr::cmdstan_model(stan_file, include_paths = include_path,
                                 dir = dir, compile_model_methods = !reuse,
                                 force_recompile = !reuse, quiet = TRUE)
  if (!reuse) mod$save_hpp_file(dir)
  # cmdstanr keeps the generated C++ only for a model compiled in the same
  # session and refuses model methods without it. Hand it the copy saved next
  # to the executable and expose the methods once, on the model, so that every
  # fit copies them instead of rebuilding them. (cmdstanr 0.9.0 internals.)
  p <- mod$.__enclos_env__$private
  if (is.null(p$model_methods_env_)) p$model_methods_env_ <- new.env()
  env <- p$model_methods_env_
  if (length(env$hpp_code_) == 0) env$hpp_code_ <- readLines(hpp, warn = FALSE)
  if (is.null(env$model_ptr)) {
    cmdstanr:::expose_model_methods(env, verbose = FALSE)
  }
  mod
}

skeleton <- function(mod, sd) {
  # One output directory per fit. cmdstanr names a CSV in the session
  # tempdir from a timestamp and a draw from R's RNG, and a garbage-collected
  # run deletes its files, so two fits whose RNG states coincide within one
  # minute would otherwise share a file name and the older fit's finalizer
  # would remove the newer fit's output.
  out_dir <- tempfile("skeleton-")
  dir.create(out_dir)
  fit <- mod$sample(data = sd, chains = 1, iter_warmup = 0, iter_sampling = 1,
                    fixed_param = TRUE, refresh = 0, seed = 2026,
                    output_dir = out_dir, show_messages = FALSE,
                    show_exceptions = FALSE)
  files <- fit$output_files(include_failed = FALSE)
  if (length(files) != 1L || !file.exists(files)) {
    stop("The fixed_param chain for ", mod$model_name(), " wrote no output:\n",
         paste(unlist(fit$output()), collapse = "\n"), call. = FALSE)
  }
  fit$init_model_methods(verbose = FALSE)
  fit
}
