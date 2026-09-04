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
