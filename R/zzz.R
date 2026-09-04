.onLoad <- function(libname, pkgname) {
  op <- options()
  op_mlumr <- list(mlumr.stan_engine = "rstan")
  toset <- !(names(op_mlumr) %in% names(op))
  if (any(toset)) options(op_mlumr[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  # A single `mlumr.quiet` option suppresses the startup banner (in addition to
  # the standard suppressPackageStartupMessages()), giving scripts one switch for
  # a quiet session.
  if (isTRUE(getOption("mlumr.quiet", FALSE))) {
    return(invisible())
  }
  version <- utils::packageVersion(pkgname)
  packageStartupMessage(sprintf(
    "mlumr %s: Bayesian multilevel unanchored meta-regression. GitHub: https://github.com/choxos/mlumr",
    version
  ))
  invisible()
}
