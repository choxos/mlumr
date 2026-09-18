# `R CMD check` fails a package whose tests reach into an undeclared namespace:
#
#   checking for unstated dependencies in 'tests' ... WARNING
#   '::' or ':::' import not declared from: 'xfun'
#
# It is an easy one to introduce, because the package in question is usually
# installed: `xfun` arrives with knitr, which is already suggested here, so the
# test that used it ran perfectly well locally and on every developer machine.
# Nothing said otherwise until a check run on another platform, fifteen minutes
# into CI, with `error_on = "warning"` turning it into a failed job.
#
# The check itself is a string comparison, so it belongs where it costs a
# second rather than a CI round trip.

test_that("every namespace the tests reach into is declared", {
  desc <- testthat::test_path("..", "..", "DESCRIPTION")
  tdir <- testthat::test_path(".")
  skip_if_not(file.exists(desc),
              "run from a source checkout, not an installed package")

  files <- list.files(tdir, pattern = "[.][Rr]$", full.names = TRUE)
  skip_if(length(files) == 0L, "no test files")
  txt <- unlist(lapply(files, readLines, warn = FALSE))
  # Strip comments first: a package named only in a comment is not a dependency,
  # and several comments here quote the very warning above.
  txt <- sub("#.*$", "", txt)
  used <- unlist(regmatches(txt, gregexpr("\\b[a-zA-Z][a-zA-Z0-9.]*(?=:::?)",
                                          txt, perl = TRUE)))
  used <- unique(used)

  d <- read.dcf(desc)
  fields <- intersect(c("Depends", "Imports", "Suggests", "LinkingTo"),
                      colnames(d))
  declared <- trimws(sub("\\(.*", "",
                         unlist(strsplit(paste(d[1, fields], collapse = ","),
                                         ","))))
  declared <- declared[nzchar(declared)]

  # Base and recommended-priority packages need no declaration, and neither
  # does the package under test.
  exempt <- c(rownames(utils::installed.packages(priority = "base")),
              "mlumr", "R")
  missing <- setdiff(used, c(declared, exempt))

  expect_equal(
    length(missing), 0L,
    info = paste0("tests call ", paste(missing, collapse = ", "),
                  " with :: but DESCRIPTION does not declare it. Add it to ",
                  "Suggests, or drop the call."))
})
