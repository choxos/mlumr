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

  # What the function prints now, captured rather than transcribed.
  src <- readLines(testthat::test_path("..", "..", "R", "prior_sensitivity.R"))
  b <- grep("cat\\(\"\\\\nInterpretation:", src)
  e <- grep("otherwise\\.\\\\n\"\\)", src)
  skip_if(length(b) != 1L || length(e) != 1L,
          "the interpretation block moved; update this test with it")
  printed <- capture.output(eval(parse(text = paste(src[b:e], collapse = "\n"))))
  printed <- printed[nzchar(printed)]

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
