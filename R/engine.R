#' Get or set the Stan engine
#'
#' mlumr fits its models through rstan by default. `mlumr_engine("cmdstanr")`
#' switches to cmdstanr for the session; the choice is stored in
#' `options(mlumr.stan_engine)`, so a permanent default belongs in
#' `.Rprofile`. When cmdstanr or CmdStan is missing, an interactive session is
#' offered their installation (cmdstanr from stan-dev's maintained
#' repository); otherwise the install commands are printed and the engine is
#' left unchanged.
#'
#' @param engine `"rstan"` or `"cmdstanr"`, matched exactly. `NULL` (the
#'   default) returns the current engine without changing it.
#' @return The current engine, invisibly when setting.
#' @export
#' @examples
#' mlumr_engine()
#' \dontrun{
#' mlumr_engine("cmdstanr")
#' }
mlumr_engine <- function(engine = NULL) {
  if (is.null(engine)) return(get_engine())
  engine <- .validate_engine_name(engine)
  if (engine == "cmdstanr" && !.install_cmdstanr()) {
    current <- getOption("mlumr.stan_engine", "rstan")
    message("Engine unchanged (", current, ").")
    return(invisible(current))
  }
  options(mlumr.stan_engine = engine)
  message("mlumr engine set to: ", engine)
  invisible(engine)
}


#' Install cmdstanr and CmdStan when they are missing, asking first
#'
#' cmdstanr comes from stan-dev's maintained repository, not the one pinned in
#' `Additional_repositories`, which serves a cmdstanr too old to build CmdStan
#' on Windows with current R.
#' @return `TRUE` when cmdstanr and CmdStan are available afterwards.
#' @keywords internal
.install_cmdstanr <- function() {
  repos <- c("https://stan-dev.r-universe.dev", getOption("repos"))
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    message("cmdstanr is not installed.")
    if (!interactive() ||
          utils::menu(c("Yes", "No"), title = "Install cmdstanr from stan-dev.r-universe.dev?") != 1L) {
      message("Install it with:\n",
              '  install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))')
      return(FALSE)
    }
    utils::install.packages("cmdstanr", repos = repos)
    if (!requireNamespace("cmdstanr", quietly = TRUE)) return(FALSE)
  }
  if (!.cmdstan_available()) {
    message("CmdStan is not installed.")
    if (!interactive() ||
          utils::menu(c("Yes", "No"), title = "Install CmdStan with cmdstanr::install_cmdstan()?") != 1L) {
      message("Install it with:\n  cmdstanr::install_cmdstan()")
      return(FALSE)
    }
    cmdstanr::install_cmdstan()
  }
  .cmdstan_available()
}


#' Get the current Stan engine (internal)
#' @keywords internal
get_engine <- function() {
  .validate_engine_name(getOption("mlumr.stan_engine", "rstan"))
}


#' Validate a Stan engine name
#' @keywords internal
.validate_engine_name <- function(engine) {
  valid <- is.character(engine) &&
    length(engine) == 1L &&
    !is.na(engine) &&
    nzchar(engine)

  if (!valid || !engine %in% .supported_engines()) {
    stop("`engine` must be 'rstan' or 'cmdstanr'.", call. = FALSE)
  }

  engine
}


#' Supported Stan engines
#' @keywords internal
.supported_engines <- function() {
  c("rstan", "cmdstanr")
}


#' Check whether CmdStan is configured for cmdstanr
#' @keywords internal
.cmdstan_available <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    return(FALSE)
  }

  isTRUE(tryCatch(
    nzchar(cmdstanr::cmdstan_path()),
    error = function(e) FALSE
  ))
}
