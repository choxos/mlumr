# The subgroup-identification article reports numbers from a committed results
# file. Nothing tied the two together: regenerating the results and forgetting
# to re-knit shipped an article whose numbers no longer came from any file in
# the repository, and a green build said nothing about it because knitting is
# not part of the build.
#
# The article records the MD5 of the results file it was built from. These
# assertions compare that recorded value against the file on disk, in both the
# knitted source and the rendered HTML, so the two cannot drift apart silently.

.artifact_path <- function(...) testthat::test_path("..", "..", ...)

test_that("the subgroup article was built from the committed results file", {
  csv <- .artifact_path("data-raw", "subgroup_identification_results.csv")
  rmd <- .artifact_path("vignettes", "subgroup-identification.Rmd")
  html <- .artifact_path("vignettes", "subgroup-identification.html")
  # `data-raw` is build-ignored, so an installed package has neither the results
  # nor the article source; there is nothing to compare there. The results file
  # is what says which of the two this is. Once it exists, a MISSING article is
  # the failure this test is for, not a reason to skip: a skip here is exactly
  # how the check disappears while the build stays green.
  skip_if_not(file.exists(csv),
              "run from a source checkout, not an installed package")
  expect_true(file.exists(rmd),
              info = "the knitted article is missing beside its results")
  expect_true(file.exists(html),
              info = "the rendered article is missing beside its results")
  if (!file.exists(rmd) || !file.exists(html)) return(invisible(NULL))

  md5 <- unname(tools::md5sum(csv))
  expect_match(md5, "^[0-9a-f]{32}$")

  for (artifact in c(rmd, html)) {
    txt <- paste(readLines(artifact, warn = FALSE), collapse = "\n")
    # The digest must appear literally. Its absence means either the article
    # predates this provenance line or it was knitted from a different results
    # file; both are the drift this test exists to catch.
    expect_true(
      grepl(md5, txt, fixed = TRUE),
      info = paste0(basename(artifact), " does not record the MD5 of the ",
                    "committed results file (", md5, "). Re-knit the article ",
                    "with vignettes/precompile.R after regenerating the ",
                    "results.")
    )
  }
})

test_that("the results manifest matches the results it describes", {
  csv <- .artifact_path("data-raw", "subgroup_identification_results.csv")
  manifest <- .artifact_path("data-raw", "subgroup_identification_manifest.dcf")
  skip_if_not(file.exists(csv),
              "run from a source checkout, not an installed package")
  # The manifest is written by the same script, in the same step, as the results
  # it describes. Results present with no manifest means that step did not
  # finish, which is a broken artifact set rather than an absent one.
  expect_true(file.exists(manifest),
              info = paste("the results manifest is missing; the study writes",
                           "it beside the CSV, so the run did not complete"))
  if (!file.exists(manifest)) return(invisible(NULL))

  fields <- read.dcf(manifest)
  expect_true(all(c("Script-MD5", "Results-MD5", "Rows", "mlumr", "R") %in%
                    colnames(fields)))

  expect_identical(unname(fields[1, "Results-MD5"]),
                   unname(tools::md5sum(csv)))
  # The row count is the cheap structural check the digest cannot give: a
  # truncated file has a different digest but nothing says WHAT changed.
  expect_identical(as.integer(fields[1, "Rows"]),
                   nrow(utils::read.csv(csv)))

  # The other half of the provenance link, and the half nothing checked. The
  # article's own digest test hashes the CSV, and the two assertions above
  # hash the CSV, so editing the study script without rerunning it left every
  # gate green while the manifest went on naming a script that no longer
  # exists in the tree. A reader could then no longer tell whether the
  # difference was a comment or a different data-generating process. This is
  # what makes "these results came from this script" a statement about the
  # files in front of you.
  script <- .artifact_path("data-raw", "simulate_subgroup_identification.R")
  expect_true(file.exists(script),
              info = "the manifest describes a study script that is not here")
  if (file.exists(script)) {
    expect_identical(
      unname(fields[1, "Script-MD5"]), unname(tools::md5sum(script)),
      info = paste0("the committed study script is not the one that produced ",
                    "the committed results. Rerun the study, which rewrites ",
                    "both the results and the manifest, or keep the script as ",
                    "it ran until the next run. The manifest is the run's own ",
                    "record and must not be edited to agree with a script ",
                    "that came after it.")
    )
  }
})
