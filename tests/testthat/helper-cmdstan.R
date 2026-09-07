# Is CmdStan actually usable, not merely pointed at?
#
# `set_cmdstan_path()` accepts any existing directory, so `cmdstan_path()` can
# return a path with no CmdStan in it and no error, and a guard built on it
# lets the test through to a compile that then fails. `cmdstan_version()` reads
# the installation and gives NA when there is nothing to read.
cmdstan_is_usable <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) return(FALSE)
  tryCatch(
    !is.na(cmdstanr::cmdstan_version(error_on_NA = FALSE)),
    error = function(e) FALSE
  )
}
