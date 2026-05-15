#' Get or Set the Stan Engine
#'
#' Control which Stan backend mlumr uses for model fitting. The default engine
#' is `"rstan"` (compiled C++ models via rstantools). Users who prefer cmdstanr
#' can switch engines after installation.
#'
#' @param engine Character string: `"rstan"` or `"cmdstanr"`. If `NULL`
#'   (default), returns the current engine without changing it.
#'
#' @details
#' When switching to `"cmdstanr"`, this function checks whether the cmdstanr
#' package and CmdStan toolchain are installed. If either is missing, it offers
#' to install them interactively.
#'
#' Engine names must be matched exactly. Partial strings such as `"c"` are not
#' accepted.
#'
#' The engine preference is stored as `options(mlumr.stan_engine = ...)` and
#' persists for the current R session. To set a permanent default, add to your
#' `.Rprofile`:
#'
#' ```
#' options(mlumr.stan_engine = "cmdstanr")
#' ```
#'
#' @return The current engine (character), returned invisibly when setting.
#' @export
#'
#' @examples
#' # Check current engine
#' mlumr_engine()
#'
#' # Switch to cmdstanr (interactive)
#' \dontrun{
#' mlumr_engine("cmdstanr")
#' }
mlumr_engine <- function(engine = NULL) {
  if (is.null(engine)) {
    return(get_engine())
  }

  engine <- .validate_engine_name(engine)

  if (engine == "rstan") {
    options(mlumr.stan_engine = "rstan")
    message("mlumr engine set to: rstan")
    return(invisible(engine))
  }

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    message("cmdstanr is not installed.")
    if (interactive()) {
      choice <- utils::menu(
        c("Yes", "No"),
        title = "Install cmdstanr from r-universe?"
      )
      if (choice == 1L) {
        install_ok <- tryCatch(
          {
            utils::install.packages(
              "cmdstanr",
              repos = c("https://stan-dev.r-universe.dev", getOption("repos"))
            )
            TRUE
          },
          error = function(e) {
            message("cmdstanr installation failed: ", conditionMessage(e))
            FALSE
          }
        )
        if (!install_ok) {
          return(.message_engine_unchanged())
        }
      } else {
        return(.message_engine_unchanged())
      }
    } else {
      message(
        "Install cmdstanr with:\n",
        '  install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))'
      )
      return(.message_engine_unchanged())
    }

    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      message("cmdstanr installation failed.")
      return(.message_engine_unchanged())
    }
  }

  if (!.cmdstan_available()) {
    message("CmdStan is not installed.")
    if (interactive()) {
      choice <- utils::menu(
        c("Yes", "No"),
        title = "Install CmdStan via cmdstanr::install_cmdstan()?"
      )
      if (choice == 1L) {
        install_ok <- tryCatch(
          {
            cmdstanr::install_cmdstan()
            TRUE
          },
          error = function(e) {
            message("CmdStan installation failed: ", conditionMessage(e))
            FALSE
          }
        )
        if (!install_ok) {
          return(.message_engine_unchanged())
        }
      } else {
        return(.message_engine_unchanged())
      }
    } else {
      message(
        "Install CmdStan with:\n",
        "  cmdstanr::install_cmdstan()"
      )
      return(.message_engine_unchanged())
    }

    if (!.cmdstan_available()) {
      message("CmdStan installation failed.")
      return(.message_engine_unchanged())
    }
  }

  options(mlumr.stan_engine = "cmdstanr")
  message("mlumr engine set to: cmdstanr")
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


#' Report that the current engine was not changed
#' @keywords internal
.message_engine_unchanged <- function() {
  current <- tryCatch(
    get_engine(),
    error = function(e) NA_character_
  )
  label <- if (is.na(current)) "invalid current option" else current
  message(sprintf("Engine unchanged (%s).", label))
  invisible(current)
}
