# Writes each vignette's R code as a script for the Playground's Examples menu.
# The code is the vignette's own, extracted with knitr::purl() from the vignette
# sources at the commit the bundled mlumr binary was built from, with two
# changes that make it a plain R script:
#   * knitr::kable(x, ...) becomes x, so tables print as ordinary R output;
#   * the vignette's knitr setup chunk (hidden, include = FALSE) is left out;
#     other hidden chunks stay, since the code after them needs them.
# A data file the code reads from the repository (../data-raw/...) is copied
# into the site and listed in index.json, and the Playground places it where
# that relative path points from R's working directory (~).
# Chunks the vignette shows without running stay commented out, as purl()
# writes them. Two vignette settings that purl() alone would miss are honored:
# a global eval = FALSE in the setup chunk (so only the chunks marked
# eval = TRUE run, as in the vignette), and purl = FALSE on a chunk the
# vignette shows (kept, since the reader sees and runs it).
#
# Usage: Rscript scripts/vignette-code.R <mlumr.txt> <out dir> <cache dir>

args <- commandArgs(trailingOnly = TRUE)
kv <- strsplit(readLines(args[1]), " ")
info <- setNames(vapply(kv, function(x) paste(x[-1], collapse = " "), ""), vapply(kv, `[`, "", 1))
commit <- info[["commit"]]
short <- substr(commit, 1, 7)
out_dir <- args[2]
cache <- file.path(args[3], commit)
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "vignettes"), recursive = TRUE, showWarnings = FALSE)

# introduction.Rmd is left out: its code is a schematic with `...` placeholders.
stems <- c("binary-outcomes", "continuous-outcomes", "count-outcomes", "survival-outcomes",
           "data-preparation", "fitting-and-diagnostics", "choosing-a-method",
           "subgroup-identification")

fetch_source <- function(stem) {
  # Precompiled vignettes keep their source in <stem>.Rmd.orig.
  for (name in paste0(stem, c(".Rmd.orig", ".Rmd"))) {
    dest <- file.path(cache, name)
    if (!file.exists(dest)) {
      url <- sprintf("https://raw.githubusercontent.com/choxos/mlumr/%s/vignettes/%s", commit, name)
      ok <- tryCatch(utils::download.file(url, dest, quiet = TRUE) == 0,
                     error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok) unlink(dest)
    }
    if (file.exists(dest)) return(dest)
  }
  stop("No source found for vignette ", stem, " at ", commit)
}

drop_setup_chunks <- function(lines) {
  header <- grepl("^## ----", lines)
  keep <- rep(TRUE, length(lines))
  for (start in which(header & grepl("include *= *FALSE", lines))) {
    later <- which(header & seq_along(lines) > start)
    end <- if (length(later)) later[1] - 1 else length(lines)
    if (any(grepl("opts_chunk\\$set", lines[start:end]))) keep[start:end] <- FALSE
  }
  lines[keep]
}

# Replaces each knitr::kable(x, ...) call with x, located through R's parser so
# multi-line calls and nested brackets are handled exactly.
unwrap_kable <- function(lines) {
  repeat {
    pd <- utils::getParseData(parse(text = lines, keep.source = TRUE))
    fn <- which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "kable")
    fn <- fn[vapply(fn, function(i) {
      sib <- pd[pd$parent == pd$parent[i], ]
      any(sib$token == "SYMBOL_PACKAGE" & sib$text == "knitr")
    }, TRUE)]
    if (!length(fn)) return(lines)
    # The last call in the file first, so earlier positions stay valid.
    i <- fn[which.max(pd$line1[fn] * 1e4 + pd$col1[fn])]
    fun_expr <- pd$parent[i]
    call_id <- pd$parent[pd$id == fun_expr]
    call <- pd[pd$id == call_id, ]
    kids <- pd[pd$parent == call_id, ]
    kids <- kids[order(kids$line1, kids$col1), ]
    arg <- kids[kids$token == "expr" & kids$id != fun_expr, ][1, ]
    first <- substring(lines[arg$line1:arg$line2], 1)
    first[length(first)] <- substr(first[length(first)], 1, arg$col2)
    first[1] <- substring(first[1], arg$col1)
    before <- substr(lines[call$line1], 1, call$col1 - 1)
    after <- substring(lines[call$line2], call$col2 + 1)
    first[1] <- paste0(before, first[1])
    first[length(first)] <- paste0(first[length(first)], after)
    lines <- c(lines[seq_len(call$line1 - 1)], first, lines[-seq_len(call$line2)])
  }
}

files <- list()
data_files <- list()
for (stem in stems) {
  src <- fetch_source(stem)
  title <- gsub('^title: *|"', "", grep("^title:", readLines(src, n = 20), value = TRUE)[1])
  rmd <- readLines(src)
  chunk <- grepl("^```\\{r", rmd)
  rmd[chunk] <- sub(",\\s*purl\\s*=\\s*FALSE", "", rmd[chunk])
  setup <- rmd[seq_len(min(grep("opts_chunk\\$set\\(", rmd), length(rmd)) + 15)]
  global_eval_off <- any(grepl("^\\s*eval\\s*=\\s*FALSE", setup))
  input <- tempfile(fileext = ".Rmd")
  writeLines(rmd, input)
  tmp <- tempfile(fileext = ".R")
  if (global_eval_off) knitr::opts_chunk$set(eval = FALSE)
  knitr::purl(input, output = tmp, documentation = 1, quiet = TRUE)
  knitr::opts_chunk$restore()
  code <- unwrap_kable(drop_setup_chunks(readLines(tmp)))
  while (length(code) && !nzchar(code[1])) code <- code[-1]
  header <- c(
    sprintf('# Code from vignette("%s"): %s', stem, title),
    sprintf("# mlumr GitHub main at %s, extracted with knitr::purl(). Tables print as", short),
    "# plain R output and the vignette's knitr setup chunk is left out; otherwise",
    "# the code is the vignette's, unchanged. Chunks the vignette shows but does",
    "# not run are commented out.")
  used <- unique(regmatches(code, regexpr("\\.\\./data-raw/[A-Za-z0-9_.-]+", code)))
  for (rel in used) {
    file <- sub("^\\.\\./", "", rel)
    dest <- file.path(cache, basename(file))
    if (!file.exists(dest)) {
      utils::download.file(sprintf("https://raw.githubusercontent.com/choxos/mlumr/%s/%s", commit, file),
                           dest, quiet = TRUE, mode = "wb")
    }
    dir.create(file.path(out_dir, "data-raw"), showWarnings = FALSE)
    file.copy(dest, file.path(out_dir, file), overwrite = TRUE)
    data_files[[length(data_files) + 1]] <- list(path = file, vfs = paste0("/home/", file))
    header <- c(header,
      sprintf("# %s: the data file from the", rel),
      "# mlumr repository. The Playground puts it where that path points from R's",
      "# working directory (~), so the code runs unchanged.")
  }
  header <- c(header, "")
  writeLines(c(header, code), file.path(out_dir, "vignettes", paste0(stem, ".R")))
  # Every script must still parse.
  invisible(parse(file.path(out_dir, "vignettes", paste0(stem, ".R"))))
  files[[length(files) + 1]] <- list(path = paste0("vignettes/", stem, ".R"), title = title)
}
quick <- list(
  list(path = "quick-start/psoriasis.R", title = "Binary outcome in a minute (psoriasis)"),
  list(path = "quick-start/ndmm-survival.R", title = "Survival outcome, shorter run (NDMM)"))
index <- list(commit = commit,
              groups = list(list(name = "Vignettes", files = files),
                            list(name = "Quick start", files = quick)),
              data = data_files,
              open = list("vignettes/binary-outcomes.R"))
writeLines(jsonlite::toJSON(index, auto_unbox = TRUE, pretty = TRUE), file.path(out_dir, "index.json"))
cat(sprintf("examples: %d vignette scripts from %s\n", length(files), short))
