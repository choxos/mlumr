# The only authority on the contents of the source package is the source
# package: build it and look inside. Files also leave the tarball through rules
# that live nowhere in `.Rbuildignore` (an empty directory is dropped,
# `.Rinstignore` applies at install time, vignette handling rewrites
# `inst/doc`), so a reading of the patterns is not a reading of the tarball.
test_that("the built source tarball contains what the package promises", {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("MLUMR_SKIP_TARBALL_TEST")),
          "tarball inspection disabled by MLUMR_SKIP_TARBALL_TEST")
  pkg <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(file.exists(file.path(pkg, "DESCRIPTION")),
              "run from a source checkout, not an installed package")
  skip_if_not(nzchar(Sys.which("tar")), "tar not available")

  # Build INSIDE a fresh directory. `R CMD build` writes the archive to the
  # working directory, so an earlier version ran it in whatever directory the
  # tests happened to start in, inspected the first matching archive found
  # there, and then deleted every `mlumr_*.tar.gz` in it. That could inspect a
  # stale tarball and destroy unrelated ones.
  out <- file.path(tempdir(), paste0("tarball-", Sys.getpid()))
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)
  old_wd <- setwd(out)
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)

  # Vignettes are precompiled, so building them again would rerun Stan for
  # tens of minutes and prove nothing about which files ship.
  res <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(pkg)),
    stdout = TRUE, stderr = TRUE, env = c("R_TESTS=")
  ))
  status <- attr(res, "status") %||% 0L
  # Only what this build just produced, in this directory, which is empty
  # otherwise.
  tarball <- list.files(out, pattern = "^mlumr_.*\\.tar\\.gz$",
                        full.names = TRUE)
  # A build that fails is a FAILURE of the release contract, not a reason to
  # skip. Skipping here is how a required gate disappears while the job stays
  # green, which is the whole thing this file exists to prevent.
  expect_equal(status, 0L,
               info = paste("R CMD build failed:",
                            paste(utils::tail(res, 5L), collapse = " | ")))
  expect_length(tarball, 1L)
  if (status != 0L || length(tarball) != 1L) {
    return(invisible(NULL))
  }

  files <- utils::untar(tarball[[1]], list = TRUE)
  # Paths inside the tarball are prefixed with the package directory.
  files <- sub("^mlumr/", "", files)

  must_ship <- c(
    "DESCRIPTION", "NAMESPACE", "NEWS.md",
    "R/predict.R", "R/survival.R",
    "inst/stan/mlumr_binary_spfa.stan",
    "data/psoriasis_ipd.rda",
    "vignettes/binary-outcomes.html",
    "vignettes/binary-outcomes.html.asis"
  )
  for (path in must_ship) {
    expect_true(path %in% files,
                info = paste("missing from the source tarball:", path))
  }

  # The development-only trees. `reporting/` is proprietary and
  # `documentation/` and `inst/future/` are unreleased work; any of them
  # reaching a tarball is a disclosure, not a size problem.
  must_not_ship <- c("^data-raw/", "^reporting/", "^documentation/",
                     "^inst/future/", "^\\.github/")
  for (pattern in must_not_ship) {
    hits <- grep(pattern, files, value = TRUE)
    expect_length(hits, 0L)
  }
})
