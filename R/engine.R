#' Get or set the Stan engine
#'
#' mlumr fits its models through rstan by default. `mlumr_engine("cmdstanr")`
#' switches to cmdstanr for the session; the choice is stored in
#' `options(mlumr.stan_engine)`, so a permanent default belongs in
#' `.Rprofile`. cmdstanr and CmdStan are installed separately:
#'
#' ```
#' install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
#' cmdstanr::install_cmdstan()
#' ```
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
  if (engine == "cmdstanr" && !.cmdstan_available()) {
    message("cmdstanr with CmdStan is not installed. Install them with:\n",
            '  install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))\n',
            "  cmdstanr::install_cmdstan()")
  }
  options(mlumr.stan_engine = engine)
  message("mlumr engine set to: ", engine)
  invisible(engine)
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
