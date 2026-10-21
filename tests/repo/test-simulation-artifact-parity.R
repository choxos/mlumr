# Two articles report numbers from committed results files: the
# subgroup-identification article and the survival-optimization article. Nothing
# tied an article to its results: regenerating the results and forgetting to
# re-knit shipped an article whose numbers no longer came from any file in the
# repository, and a green build said nothing about it because knitting is not
# part of the build.
#
# Each article records the MD5 of every results file it was built from. These
# assertions compare those recorded values against the files on disk, in both
# the knitted source and the rendered HTML, so the two cannot drift apart
# silently.

.artifact_path <- function(...) testthat::test_path("..", "..", ...)

# One entry per study. `results` maps the manifest's field name for each
# results file to the file; the manifest records `<field>-MD5` and the row
# count under `rows`.
.studies <- list(
  list(article = "subgroup-identification",
       script = "simulate_subgroup_identification.R",
       manifest = "subgroup_identification_manifest.dcf",
       results = c(Results = "subgroup_identification_results.csv"),
       rows = c(Results = "Rows")),
  list(article = "survival-optimization",
       script = "simulate_survival_optimization.R",
       manifest = "survival_optimization_manifest.dcf",
       results = c(`Results-Fixed` = "survival_optimization_fixed.csv",
                   `Results-Fits` = "survival_optimization_fits.csv"),
       rows = c(`Results-Fixed` = "Results-Fixed-Rows",
                `Results-Fits` = "Results-Fits-Rows"))
)

for (study in .studies) {
  test_that(paste("the", study$article, "article was built from the committed",
                  "results files"), {
    csvs <- vapply(study$results, function(f) .artifact_path("data-raw", f),
                   character(1))
    rmd <- .artifact_path("vignettes", paste0(study$article, ".Rmd"))
    html <- .artifact_path("vignettes", paste0(study$article, ".html"))
    # `data-raw` is build-ignored, so an installed package has neither the
    # results nor the article source; there is nothing to compare there. The
    # results files are what say which of the two this is. Once they exist, a
    # missing article is the failure this test is for, not a reason to skip: a
    # skip here is exactly how the check disappears while the build stays
    # green.
    skip_if_not(all(file.exists(csvs)),
                "run from a source checkout, not an installed package")
    expect_true(file.exists(rmd),
                info = "the knitted article is missing beside its results")
    expect_true(file.exists(html),
                info = "the rendered article is missing beside its results")
    if (!file.exists(rmd) || !file.exists(html)) return(invisible(NULL))

    for (csv in csvs) {
      md5 <- unname(tools::md5sum(csv))
      expect_match(md5, "^[0-9a-f]{32}$")
      for (artifact in c(rmd, html)) {
        txt <- paste(readLines(artifact, warn = FALSE), collapse = "\n")
        # The digest must appear literally. Its absence means either the
        # article predates this provenance line or it was knitted from a
        # different results file; both are the drift this test exists to
        # catch.
        expect_true(
          grepl(md5, txt, fixed = TRUE),
          info = paste0(basename(artifact), " does not record the MD5 of the ",
                        "committed results file ", basename(csv), " (", md5,
                        "). Re-knit the article with vignettes/precompile.R ",
                        "after regenerating the results.")
        )
      }
    }
  })

  test_that(paste("the", study$article, "manifest matches the results it",
                  "describes"), {
    csvs <- vapply(study$results, function(f) .artifact_path("data-raw", f),
                   character(1))
    manifest <- .artifact_path("data-raw", study$manifest)
    skip_if_not(all(file.exists(csvs)),
                "run from a source checkout, not an installed package")
    # The manifest is written by the same script, in the same step, as the
    # results it describes. Results present with no manifest means that step
    # did not finish, which is a broken artifact set rather than an absent one.
    expect_true(file.exists(manifest),
                info = paste("the results manifest is missing; the study",
                             "writes it beside the CSV, so the run did not",
                             "complete"))
    if (!file.exists(manifest)) return(invisible(NULL))

    fields <- read.dcf(manifest)
    expect_true(all(c("Script-MD5", paste0(names(csvs), "-MD5"), study$rows,
                      "mlumr", "R") %in% colnames(fields)))

    for (field in names(csvs)) {
      expect_identical(unname(fields[1, paste0(field, "-MD5")]),
                       unname(tools::md5sum(csvs[[field]])), info = field)
      # The row count is the cheap structural check the digest cannot give: a
      # truncated file has a different digest but nothing says what changed.
      expect_identical(as.integer(fields[1, study$rows[[field]]]),
                       nrow(utils::read.csv(csvs[[field]])), info = field)
    }

    # The other half of the provenance link. The article's own digest test
    # hashes the results, and the assertions above hash the results, so
    # editing the study script without rerunning it left every gate green
    # while the manifest went on naming a script that no longer exists in the
    # tree. A reader could then no longer tell whether the difference was a
    # comment or a different data-generating process. This is what makes
    # "these results came from this script" a statement about the files in
    # front of you.
    script <- .artifact_path("data-raw", study$script)
    expect_true(file.exists(script),
                info = "the manifest describes a study script that is not here")
    if (file.exists(script)) {
      expect_identical(
        unname(fields[1, "Script-MD5"]), unname(tools::md5sum(script)),
        info = paste0("the committed study script is not the one that ",
                      "produced the committed results. Rerun the study, which ",
                      "rewrites both the results and the manifest, or keep the ",
                      "script as it ran until the next run. The manifest is ",
                      "the run's own record and must not be edited to agree ",
                      "with a script that came after it.")
      )
    }
  })
}
