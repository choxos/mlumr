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
    expect_false(.excluded_by_buildignore(path, patterns),
                 label = paste0("`.Rbuildignore` excludes '", path, "'"))
  }
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
