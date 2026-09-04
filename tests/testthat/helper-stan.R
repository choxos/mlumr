# Resolve a file under inst/stan whether the package is installed or loaded
# from source. `system.file()` returns "" in the second case unless pkgload has
# shimmed it, and `readLines("")` aborts rather than reporting what is wrong.
#
# The file is never optional. Skipping on a missing Stan source would hide
# exactly the packaging defect these checks exist to catch, so callers assert
# `file.exists()` and let a genuine absence fail.
stan_source_path <- function(...) {
  path <- system.file("stan", ..., package = "mlumr")
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path("..", "..", "inst", "stan", ...)
  }
  path
}
