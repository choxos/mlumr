# Loads mlumr's R sources (without the Stan backends) into the browser's R
# session. S3 lookup does not search attached environments, so the methods the
# NAMESPACE declares are registered explicitly; otherwise print(naive(dat))
# would print a raw list.
local({
  # check-adapter.R points this at a native copy of the same files.
  dir <- getOption("lesson.mlumr_dir", "/home/web_user/mlumr")
  # mlumr tags its default priors with utils::packageVersion("mlumr"), and
  # mlumr() evaluates one of them on every fit. There is no installed package
  # in the browser, so a library entry holding the pinned DESCRIPTION answers
  # that lookup with the true version.
  # It lives in the session's temporary directory, never beside the site files.
  library_dir <- file.path(tempdir(), "mlumr-library")
  dir.create(file.path(library_dir, "mlumr"), recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(dir, "DESCRIPTION"), file.path(library_dir, "mlumr", "DESCRIPTION"), overwrite = TRUE)
  .libPaths(c(library_dir, .libPaths()))
  ml <- new.env()
  for (file in c("R/mlumr.R", "lesson-helpers.R")) {
    sys.source(file.path(dir, file), envir = ml, keep.source = FALSE)
  }
  # The excluded backend files are what .mlumr_fit_backend() calls. In their
  # place, hand the Stan data that mlumr() prepared back to lesson_stan_data().
  # A learner who types mlumr() in the cell gets an explanation, not a missing
  # function error.
  assign(".mlumr_fit_backend", function(engine, model_name, stan_data, ...) {
    signalCondition(structure(
      class = c("lesson_stan_data", "condition"),
      list(message = "", call = NULL, model_name = model_name, stan_data = stan_data)
    ))
    stop("mlumr() cannot sample here: rstan and cmdstanr do not run in the ",
         "browser. Your data passed mlumr()'s checks. Use the Fit button below ",
         "to sample the same Stan model in this page.", call. = FALSE)
  }, envir = ml)
  namespace <- readLines(file.path(dir, "NAMESPACE"), warn = FALSE)
  for (m in regmatches(namespace, regexec("^S3method\\(([^,]+),([^)]+)\\)", namespace))) {
    if (length(m)) registerS3method(m[2], m[3], get(paste0(m[2], ".", m[3]), envir = ml), envir = ml)
  }
  attach(ml, name = "mlumr", warn.conflicts = FALSE)
  options(lesson.stan_dir = file.path(dir, "inst", "stan"))
})
