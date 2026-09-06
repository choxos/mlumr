#!/usr/bin/env Rscript
# Precompile mlumr's Stan-fitting vignettes (the multinma pattern). Run locally
# from the package root:
#
#   Rscript vignettes/precompile.R
#
# For each "<stem>.Rmd.orig" this:
#   1. knits it to a static "<stem>.Rmd", fitting the Stan models ONCE, here;
#   2. renders a self-contained "<stem>.html" (output: rmarkdown::html_vignette);
#   3. writes "<stem>.html.asis" so R CMD build / CRAN register and serve the
#      pre-rendered HTML through the R.rsp::asis engine and never run Stan.
#
# Re-run whenever a "<stem>.Rmd.orig" changes. This script, the *.Rmd.orig
# sources, the knitted *.Rmd intermediates, and figure/ are all
# build-ignored (see .Rbuildignore); only *.html + *.html.asis ship to CRAN.
#
# Requires (Suggests): multinma (real data), ggplot2 (figures), knitr, rmarkdown.

# Every package the vignette chunks reach for. Checked up front so a missing
# Suggests fails here rather than forty minutes into a Stan fit.
for (pkg in c("knitr", "rmarkdown", "multinma", "ggplot2",
              "ggsurvfit", "flexsurv", "survival", "bayesplot", "loo",
              # The engine is set to cmdstanr below, so a missing cmdstanr is
              # exactly the failure this loop exists to catch early.
              "cmdstanr")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("precompile.R needs the '", pkg, "' package installed.", call. = FALSE)
  }
}

stems <- c(
  "binary-outcomes",
  "continuous-outcomes",
  "count-outcomes",
  "survival-outcomes",
  "fitting-and-diagnostics",
  "choosing-a-method",
  "subgroup-identification"
)

# Operate inside vignettes/ so figure paths and the ../inst/REFERENCES.bib
# bibliography path resolve exactly as they will for pkgdown.
if (basename(getwd()) != "vignettes") setwd("vignettes")

# Fit with CmdStan. The shipped HTML is produced this way, so the sampler
# messages and timings in it are CmdStan's; rstan is markedly slower here and
# would make the survival vignette impractical to rebuild.
options(mlumr.stan_engine = "cmdstanr")

precompile_one <- function(stem) {
  orig <- paste0(stem, ".Rmd.orig")
  rmd  <- paste0(stem, ".Rmd")
  message("\n=== precompiling ", orig, " ===")
  # Give every article its own figure directory. Chunk names are not unique
  # across these vignettes (23 of them collide: `forest`, `prior-post`,
  # `predict-plot`, `posterior-areas`, and more), and knitr derives the image
  # file name from the chunk name alone. With one shared `figure/` directory
  # they all wrote to the same paths: `figure/forest-1.png` was claimed by five
  # articles and `figure/prior-post-1.png` by five. Whichever article ran last
  # owned the bytes, and any self-contained HTML rendered against that state
  # embedded another outcome's plot.
  #
  # This is not hypothetical. Every one of the four images in the shipped
  # continuous-outcomes.html belonged to a different article: a survival RMST
  # forest in months, a count rate-ratio posterior, a psoriasis response
  # prediction, and a survival prior-posterior overlay. The tables and prose
  # around them were correct, which is what made it survive review.
  knitr::opts_chunk$set(fig.path = file.path("figure", stem, ""))
  on.exit(knitr::opts_chunk$set(fig.path = "figure/"), add = TRUE)
  knitr::knit(orig, output = rmd)            # runs the chunks -> fits Stan here
  rmarkdown::render(rmd, quiet = TRUE)       # output format taken from the YAML
  title <- rmarkdown::yaml_front_matter(orig)$title
  writeLines(
    c(sprintf("%%\\VignetteIndexEntry{%s}", title),
      "%\\VignetteEngine{R.rsp::asis}",
      "%\\VignetteEncoding{UTF-8}"),
    paste0(stem, ".html.asis")
  )
  message("    -> ", rmd, ", ", stem, ".html, ", stem, ".html.asis")
}

for (s in stems) precompile_one(s)
message("\nAll vignettes precompiled.")
