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

  # Code is compared line by line as well. Every chunk is echoed, so each line
  # of a chunk's body reappears verbatim inside a fenced ``` r block of the
  # knitted article, interleaved with the output it produced. A fitting call
  # edited in the source without re-knitting therefore leaves a line that no
  # echoed block contains, which is the change the prose check cannot see.
  code_lines <- function(path) {
    lines <- readLines(path, warn = FALSE)
    out <- character(0)
    inside <- FALSE
    hidden <- FALSE
    for (line in lines) {
      if (!inside && grepl("^```\\{r", line)) {
        inside <- TRUE
        hidden <- grepl("(include|echo)\\s*=\\s*FALSE", line)
        next
      }
      if (inside && grepl("^```", line)) {
        inside <- FALSE
        next
      }
      if (inside && !hidden) out <- c(out, line)
    }
    out <- trimws(out, which = "right")
    out[nzchar(trimws(out))]
  }
  echoed_lines <- function(path) {
    lines <- readLines(path, warn = FALSE)
    out <- character(0)
    inside <- FALSE
    for (line in lines) {
      if (!inside && grepl("^```\\s*r\\s*$", line)) {
        inside <- TRUE
        next
      }
      if (inside && grepl("^```", line)) {
        inside <- FALSE
        next
      }
      if (inside && !startsWith(line, "#>")) out <- c(out, line)
    }
    out <- trimws(out, which = "right")
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
    missing_code <- setdiff(code_lines(orig), echoed_lines(rmd))
    msg_code <- paste0(basename(orig), " has ", length(missing_code),
                       " code line(s) that no echoed block of ", basename(rmd),
                       " contains, so a chunk was edited without re-knitting. ",
                       "First: ",
                       paste(utils::head(missing_code, 2L), collapse = " | "))
    expect_equal(length(missing_code), 0L, info = msg_code)
    checked <- checked + 1L
  }
  expect_gt(checked, 0L)
})

# Captured console output is prose too. Five articles print a model comparison,
# and the paragraph `compare_models()` appends to it is a methodological claim:
# the shipped articles carried a superseded version that read the standard
# error of a difference as a threshold for the difference itself. This pins the
# current wording in every article that prints a comparison, in the knitted
# source and the rendered page, so the articles cannot lag the function again.
# The expected lines are what `.model_comparison_interpretation()` prints; they
# are written out here rather than taken from the function so the check holds
# on a tree where the articles have been updated ahead of it.

test_that("every article that prints a model comparison shows the current reading", {
  vdir <- testthat::test_path("..", "..", "vignettes")
  skip_if_not(dir.exists(vdir),
              "run from a source checkout, not an installed package")
  rmds <- list.files(vdir, pattern = "[.]Rmd$", full.names = TRUE)
  skip_if(length(rmds) == 0L, "no knitted articles")
  expected <- c("elpd_diff is the difference in expected log pointwise predictive",
                "density vs the best model, and se_diff is its standard error: the",
                "uncertainty about that difference, not evidence for it. Read the two",
                "together. A difference small relative to se_diff is not distinguished",
                "from zero by this comparison, whatever se_diff itself is.",
                "Treat any ratio as a heuristic, not a decision rule, and check the",
                "PSIS diagnostics and whether the difference matters for the",
                "prediction you care about.")
  stale <- "is the conventional threshold"
  checked <- 0L
  for (rmd in rmds) {
    txt <- readLines(rmd, warn = FALSE)
    html <- sub("[.]Rmd$", ".html", rmd)
    html_txt <- if (file.exists(html)) {
      paste(readLines(html, warn = FALSE), collapse = "\n")
    } else {
      ""
    }
    expect_false(any(grepl(stale, txt, fixed = TRUE)),
                 info = paste0(basename(rmd), " still prints the superseded ",
                               "model-comparison rule"))
    expect_false(grepl(stale, html_txt, fixed = TRUE),
                 info = paste0(basename(html), " still prints the superseded ",
                               "model-comparison rule"))
    if (!any(grepl("Model Comparison (", txt, fixed = TRUE))) next
    for (line in expected) {
      expect_true(any(grepl(line, txt, fixed = TRUE)),
                  info = paste0(basename(rmd), " is missing: ", line))
      expect_true(grepl(line, html_txt, fixed = TRUE),
                  info = paste0(basename(html), " is missing: ", line))
    }
    checked <- checked + 1L
  }
  expect_gt(checked, 0L)
})
