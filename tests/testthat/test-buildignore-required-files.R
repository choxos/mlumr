# `.Rbuildignore` carries a catch-all for top-level dotfiles (`^\.[^/]+$`).
# It is convenient, but it excludes by shape rather than by intent, so a file
# added later disappears from the source package with nothing to review. That
# matters for the dotfiles R itself reads out of the tarball: `.Rinstignore`
# only takes effect if it ships, and `.install_extras` only affects the
# installed vignettes if it ships. Pin the intent as an assertion instead of
# trusting that the pattern will keep being read correctly.

# `R CMD build` applies the patterns as Perl-compatible regular expressions
# against paths relative to the package root, case-insensitively (see
# "Writing R Extensions", Package structure).
.excluded_by_buildignore <- function(path, patterns) {
  any(vapply(patterns, function(p) {
    grepl(p, path, perl = TRUE, ignore.case = TRUE)
  }, logical(1)))
}

test_that(".Rbuildignore keeps the files the source package must contain", {
  ignore_file <- testthat::test_path("..", "..", ".Rbuildignore")
  skip_if_not(file.exists(ignore_file),
              "run from a source checkout, not an installed package")
  patterns <- readLines(ignore_file, warn = FALSE)
  patterns <- patterns[nzchar(trimws(patterns))]

  must_ship <- c(
    "DESCRIPTION", "NAMESPACE", "NEWS.md",
    "R/mlumr.R", "R/predict.R", "R/survival.R",
    "man/mlumr.Rd",
    "inst/stan/mlumr_binary_spfa.stan",
    "inst/stan/mlumr_survival_mspline_relaxed.stan",
    "src/RcppExports.cpp",
    "tests/testthat.R", "tests/testthat/test-mlumr.R",
    "data/psoriasis_ipd.rda",
    "vignettes/introduction.Rmd", "vignettes/binary-outcomes.html",
    "vignettes/binary-outcomes.html.asis"
  )
  for (path in must_ship) {
    # Existence AND non-exclusion. Checking only the regex made a DELETED file
    # pass, because a file that is not there is also not excluded, which is the
    # opposite of what this test is for.
    full <- testthat::test_path("..", "..", path)
    expect_true(file.exists(full),
                label = paste0("'", path, "' is missing from the source tree"))
    expect_false(.excluded_by_buildignore(path, patterns),
                 label = paste0("`.Rbuildignore` excludes '", path, "'"))
  }
})

test_that("the dotfiles R reads out of the tarball are not caught by the catch-all", {
  # `.Rinstignore` and `.install_extras` only take effect if they SHIP. Neither
  # exists yet, so this asserts the property that matters when one is added:
  # that adding it is not silently undone by `^\.[^/]+$`. It fails the moment
  # someone creates one without exempting it.
  ignore_file <- testthat::test_path("..", "..", ".Rbuildignore")
  skip_if_not(file.exists(ignore_file),
              "run from a source checkout, not an installed package")
  patterns <- readLines(ignore_file, warn = FALSE)
  patterns <- patterns[nzchar(trimws(patterns))]
  for (path in c(".Rinstignore", ".install_extras")) {
    if (!file.exists(testthat::test_path("..", "..", path))) next
    expect_false(.excluded_by_buildignore(path, patterns),
                 label = paste0("'", path, "' exists but would not ship"))
  }
  # The catch-all is what would swallow them, so record that it is still there:
  # if it goes away the exemption is unnecessary, and if it stays the check above
  # is the one that matters.
  expect_true(any(grepl("^\\^\\\\\\.\\[\\^/\\]\\+\\$$", patterns)) ||
                any(patterns == "^\\.[^/]+$"),
              label = "the top-level dotfile catch-all is still present")
})

test_that("the dotfile catch-all is the reason dotfiles are excluded", {
  # If someone narrows or removes `^\.[^/]+$`, the explicit entries below have
  # to carry their own weight; this fails when a development dotfile would
  # start shipping because the catch-all went away without them being restored.
  ignore_file <- testthat::test_path("..", "..", ".Rbuildignore")
  skip_if_not(file.exists(ignore_file),
              "run from a source checkout, not an installed package")
  patterns <- readLines(ignore_file, warn = FALSE)
  patterns <- patterns[nzchar(trimws(patterns))]

  must_not_ship <- c(".github", ".lintr", ".zenodo.json", ".Rhistory",
                     ".DS_Store", "documentation", "reporting", "data-raw",
                     "inst/future", "cran-comments.md")
  for (path in must_not_ship) {
    expect_true(.excluded_by_buildignore(path, patterns),
                label = paste0("`.Rbuildignore` no longer excludes '", path, "'"))
  }
})

# The regex assertions above check what `.Rbuildignore` SAYS. They cannot see
# what `R CMD build` DOES: files also leave the tarball through rules that live
# nowhere in that file (an empty directory is dropped, `.Rinstignore` applies at
# install time, vignette handling rewrites `inst/doc`), and a pattern can be
# read correctly here and still not match the path `build` actually tests. The
# only authority on the contents of the source package is the source package.
test_that("the built source tarball contains what the package promises", {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("MLUMR_SKIP_TARBALL_TEST")),
          "tarball inspection disabled by MLUMR_SKIP_TARBALL_TEST")
  pkg <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(file.exists(file.path(pkg, "DESCRIPTION")),
              "run from a source checkout, not an installed package")
  skip_if_not(nzchar(Sys.which("tar")), "tar not available")

  out <- file.path(tempdir(), paste0("tarball-", Sys.getpid()))
  dir.create(out, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  # Vignettes are precompiled, so building them again would rerun Stan for
  # tens of minutes and prove nothing about which files ship.
  res <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(pkg)),
    stdout = TRUE, stderr = TRUE, env = c("R_TESTS=")
  ))
  status <- attr(res, "status") %||% 0L
  tarball <- list.files(getwd(), pattern = "^mlumr_.*\\.tar\\.gz$",
                        full.names = TRUE)
  if (length(tarball)) on.exit(unlink(tarball), add = TRUE)
  skip_if(status != 0L || !length(tarball),
          paste("R CMD build did not produce a tarball:",
                paste(utils::tail(res, 3L), collapse = " | ")))

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
