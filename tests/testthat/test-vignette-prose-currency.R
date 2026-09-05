# These articles are precompiled: `.Rmd.orig` is the source an author edits, and
# `knitr::knit()` turns it into the `.Rmd` that is rendered and shipped. Nothing
# in the build connects them. Editing the source and not re-knitting leaves the
# shipped article saying the old thing while every check stays green, because
# the `.Rmd` is a committed file that no longer derives from anything.
#
# That is not hypothetical here. Several methodological corrections in this
# release are prose-only, and the cheap way to make one is to edit both files by
# hand and re-render; a correction applied to `.Rmd.orig` alone would ship
# nothing while looking done in the diff.
#
# The invariant is one-directional on purpose. Every line an author wrote must
# survive into the knitted article, but the knitted article legitimately holds
# much more: kable tables, figure divs, captured console output. So this asserts
# that the source is a SUBSET of what shipped, not that the two are equal.

test_that("every line of authored prose reached the knitted article", {
  vdir <- testthat::test_path("..", "..", "vignettes")
  skip_if_not(dir.exists(vdir),
              "run from a source checkout, not an installed package")
  origs <- list.files(vdir, pattern = "[.]Rmd[.]orig$", full.names = TRUE)
  skip_if(length(origs) == 0L, "no precompiled articles")

  # Prose is everything outside a fenced block. Chunk bodies are code, and their
  # output is generated, so neither is comparable line for line.
  prose <- function(path) {
    lines <- readLines(path, warn = FALSE)
    fenced <- cumsum(grepl("^```", lines)) %% 2L == 1L
    keep <- !fenced & !grepl("^```", lines)
    out <- trimws(lines[keep], which = "right")
    # Inline `r ...` is evaluated on the way through, so the two sides differ by
    # design: the source holds the expression, the article holds its value.
    out <- out[!grepl("`r ", out, fixed = TRUE)]
    # Rendered figures are markup the source never had.
    out <- out[!grepl("^\\s*<img ", out)]
    out[nzchar(trimws(out))]
  }

  checked <- 0L
  for (orig in origs) {
    rmd <- sub("[.]orig$", "", orig)
    # A source with no knitted counterpart is the drift this test exists for,
    # not a reason to look elsewhere: the shipped article, if any, derives from
    # nothing. The other pairs passing must not hide it.
    expect_true(file.exists(rmd),
                info = paste0(basename(orig), " has no knitted counterpart; ",
                              "run vignettes/precompile.R"))
    if (!file.exists(rmd)) next
    src <- prose(orig)
    knitted <- prose(rmd)
    missing <- setdiff(src, knitted)
    expect_equal(
      length(missing), 0L,
      info = paste0(
        basename(orig), " has ", length(missing), " prose line(s) that never ",
        "reached ", basename(rmd), ", so the source was edited without ",
        "re-knitting. First: ", paste(utils::head(missing, 2L), collapse = " | ")))
    checked <- checked + 1L
  }
  expect_gt(checked, 0L)
})
