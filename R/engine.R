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

  # The installation route offered here is stan-dev's maintained repository,
  # which is NOT the repository DESCRIPTION names in `Additional_repositories`.
  # That is deliberate and the two are answering different questions.
  #
  # `Additional_repositories` is a hint for `R CMD check` and for build systems
  # resolving the whole dependency graph. Naming stan-dev there exposes rstan
  # and StanHeaders to development snapshots as well, and a development rstan
  # beside a released StanHeaders fails to build; the older repository is
  # pinned there precisely because its versions can never outrank CRAN, which
  # keeps that resolution deterministic.
  #
  # A user installing cmdstanr on its own is not resolving that graph, and the
  # pinned repository serves cmdstanr 0.8.0, which cannot build CmdStan on
  # Windows with current R. So the route offered to a person is the maintained
  # one. The message below says which repository it is using, so the difference
  # is visible rather than something to discover from DESCRIPTION.
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    message("cmdstanr is not installed.")
    if (interactive()) {
      choice <- utils::menu(
        c("Yes", "No"),
        title = paste0("Install cmdstanr from stan-dev.r-universe.dev, the ",
                       "maintained repository?")
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
        '  install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))',
        "\n\nThis is stan-dev's maintained repository, which is not the one ",
        "DESCRIPTION lists in Additional_repositories. That field is a ",
        "dependency-resolution hint for build systems and is pinned to an ",
        "older repository so rstan and StanHeaders keep resolving together; ",
        "for installing cmdstanr itself, use the maintained one above."
      )
      return(.message_engine_unchanged())
    }

    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      message("cmdstanr installation failed.")
      return(.message_engine_unchanged())
    }
  }

  # Before asking whether CmdStan is present: a CmdStan directory left over
  # from an earlier R survives an R upgrade, and the pinned cmdstanr still
  # cannot compile a model against the new Rtools. Gating this on CmdStan
  # being absent would have enabled the backend and let the first fit fail
  # instead of giving the advice.
  #
  # The pinned repository's cmdstanr cannot build CmdStan on Windows with
  # current R: it does not recognize the current Rtools, and
  # install_cmdstan() fails with "Rtools was not found but is required".
  # Offering that installation anyway sends the user into the failure the
  # NEWS entry warns about. Say what to do first instead.
  if (.cmdstanr_too_old_for_windows()) {
    message(.cmdstanr_upgrade_advice(),
            "\nthen run mlumr_engine(\"cmdstanr\") again.")
    return(.message_engine_unchanged())
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


#' Is the installed cmdstanr too old to build CmdStan on this Windows R?
#'
#' The repository DESCRIPTION pins serves cmdstanr 0.8.0, which does not
#' recognize the Rtools that current R versions use and so refuses to build
#' CmdStan there; 0.9.0 from the maintained repository does. The version alone
#' does not decide it: 0.8.x with an older R and its matching Rtools, R 4.4
#' with Rtools44 say, builds fine. So the decision is cmdstanr's own toolchain
#' check: only on Windows, only for a cmdstanr older than 0.9.0, and only when
#' that check fails with its "was not found but is required" message, is the
#' user told to upgrade rather than offered an installation that fails.
#' Arguments exist so the decision can be tested off Windows.
#' @param os `.Platform$OS.type`.
#' @param version The installed cmdstanr version, or `NULL` when it is absent.
#' @param check A function that runs the toolchain check and errors when it
#'   fails, by default cmdstanr's.
#' @keywords internal
.cmdstanr_too_old_for_windows <- function(os = .Platform$OS.type,
                                          version = .installed_cmdstanr_version(),
                                          check = .cmdstan_toolchain_check) {
  if (!identical(os, "windows") || is.null(version) ||
        utils::compareVersion(as.character(version), "0.9.0") >= 0) {
    return(FALSE)
  }
  msg <- tryCatch({
    check()
    NULL
  }, error = function(e) conditionMessage(e))
  !is.null(msg) &&
    grepl("was not found but is required to run CmdStan", msg, fixed = TRUE)
}

#' The advice that goes with a positive `.cmdstanr_too_old_for_windows()`.
#' Shared by `mlumr_engine()`, which reports it and leaves the engine alone,
#' and by the fit-time engine resolution, which stops with it.
#' @keywords internal
.cmdstanr_upgrade_advice <- function() {
  paste0(
    "cmdstanr ", utils::packageVersion("cmdstanr"), " is installed, and ",
    "on this Windows R it cannot build CmdStan or compile a model: its ",
    "toolchain check does not recognize the installed Rtools, which a ",
    "cmdstanr older than 0.9.0 does for current R versions. Upgrade it ",
    "first from stan-dev's maintained repository:\n",
    '  install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))'
  )
}

#' @keywords internal
.cmdstan_toolchain_check <- function() {
  cmdstanr::check_cmdstan_toolchain(fix = FALSE, quiet = TRUE)
}

#' @keywords internal
.installed_cmdstanr_version <- function() {
  tryCatch(utils::packageVersion("cmdstanr"), error = function(e) NULL)
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
