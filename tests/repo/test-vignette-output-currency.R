# BLOCKER class: a precompiled vignette carries captured output, so a change to
# what a function PRINTS leaves the shipped article asserting the old text while
# every check stays green. This pins the one printed block whose wording is a
# claim about the method rather than a number, so it cannot drift again without
# a test failing.

test_that("the shipped vignette shows the interpretation the code prints", {
  rmd <- testthat::test_path("..", "..", "vignettes",
                             "fitting-and-diagnostics.Rmd")
  html <- testthat::test_path("..", "..", "vignettes",
                              "fitting-and-diagnostics.html")
  skip_if_not(file.exists(rmd) && file.exists(html),
              "run from a source checkout, not an installed package")

  # What the function prints now, taken by CALLING it. An earlier version
  # located these lines by matching the source text and skipped when the
  # markers moved, so editing the printed wording, the one change this gate
  # exists to catch, made the gate disappear instead of fail.
  printed <- .prior_sensitivity_interpretation()
  printed <- printed[nzchar(printed)]
  expect_gt(length(printed), 3L)

  rmd_txt <- readLines(rmd, warn = FALSE)
  html_txt <- paste(readLines(html, warn = FALSE), collapse = "\n")
  for (line in printed) {
    expect_true(any(grepl(line, rmd_txt, fixed = TRUE)),
                label = paste0("the .Rmd is missing the printed line: ", line))
    expect_true(grepl(line, html_txt, fixed = TRUE),
                label = paste0("the .html is missing the printed line: ", line))
  }

  # And the superseded claim must be gone from both, not merely joined by the
  # new one.
  stale <- "the inference is data-driven rather than prior-driven"
  expect_false(any(grepl(stale, rmd_txt, fixed = TRUE)))
  expect_false(grepl(stale, html_txt, fixed = TRUE))
})
