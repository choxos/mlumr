#!/usr/bin/env bash
# Assembles mlumr Playground into site/. Everything the page needs at run time
# is copied in here, so the served site works offline once this has run:
#   site/webr/      webR 0.6.0 (R 4.6.0) from its pinned release
#   site/repo/      the WebAssembly R packages (mlumr, its dependencies, and
#                   the packages the vignettes use)
#   site/stan/      mlumr's Stan models compiled to WebAssembly (models/,
#                   from build-models.sh)
#   site/examples/  every vignette's R code, extracted at the mlumr commit in
#                   site/repo, plus two quick-start scripts
#   site/*.js       the Playground, bundled with CodeMirror and TinyStan
#   site/fonts/     IBM Plex Sans and Mono
#   site/katex/     KaTeX, so vignette formulas render in the Viewer offline
# Needs: bash, curl, shasum, tar, node and npm, and R with knitr and jsonlite
# (to resolve and index the package repository and extract the vignette code).
set -euo pipefail
cd "$(dirname "$0")"

WEBR_VERSION=0.6.0
WEBR_SHA256=04c56d16c3ad29265a2c4ffcb800ee0b72ab6dd64dd678c9c8fec81a832cb8cc
R_MINOR=4.6
mkdir -p .cache .build

# 1. webR
tarball=".cache/webr-${WEBR_VERSION}.tar.gz"
if [ ! -f "$tarball" ]; then
  curl -fsSL -o "$tarball" \
    "https://github.com/r-wasm/webr/releases/download/v${WEBR_VERSION}/webr-${WEBR_VERSION}.tar.gz"
fi
echo "${WEBR_SHA256}  ${tarball}" | shasum -a 256 -c - >/dev/null

rm -rf site
mkdir -p site/webr
tar -xzf "$tarball" -C site/webr --strip-components 1
# Only the webR runtime is used; its demo app and source maps are not.
rm -f site/webr/index.html site/webr/repl.* site/webr/*.map
rm -rf site/webr/assets

# 2. JavaScript dependencies: package.json and package-lock.json pin the whole
# tree; npm ci installs exactly that into .build/, again whenever the lock changes.
cp package.json package-lock.json .build/
lock=$(shasum -a 256 package-lock.json | cut -d' ' -f1)
if [ "$(cat .build/node_modules/.lock-sha256 2>/dev/null)" != "$lock" ]; then
  (cd .build && npm ci --no-audit --no-fund --loglevel=error)
  echo "$lock" > .build/node_modules/.lock-sha256
fi

# 3. the Playground itself
esbuild=.build/node_modules/.bin/esbuild
export NODE_PATH="$PWD/.build/node_modules"
"$esbuild" src/app.js --bundle --format=esm --target=es2022 --minify \
  --outfile=site/app.js --log-level=warning
"$esbuild" src/stan-worker.js --bundle --format=esm --target=es2022 --minify \
  --outfile=site/stan-worker.js --log-level=warning
"$esbuild" scripts/statement-test.mjs --bundle --format=esm --platform=node \
  --outfile=.build/statement-test.mjs --log-level=warning
node .build/statement-test.mjs
node scripts/view-test.mjs
# coi-sw.js: GitHub Pages cannot send COOP/COEP headers; this service worker
# adds them, which makes the page cross-origin isolated after one reload.
cp src/index.html src/styles.css src/ide.R src/coi-sw.js shim/fortran-commons.so site/
cp -R src/assets site/assets
node scripts/katex-inline.mjs site/katex
mkdir -p site/examples/quick-start site/fonts
cp src/examples/*.R site/examples/quick-start/
for w in 400 500 600 700; do
  cp ".build/node_modules/@fontsource/ibm-plex-sans/files/ibm-plex-sans-latin-${w}-normal.woff2" site/fonts/
done
for w in 400 500; do
  cp ".build/node_modules/@fontsource/ibm-plex-mono/files/ibm-plex-mono-latin-${w}-normal.woff2" site/fonts/
done

# 4. Stan models (compiled by build-models.sh)
if [ ! -f models/manifest.json ]; then
  echo "models/ is missing: run build-models.sh first" >&2
  exit 1
fi
cp -R models site/stan

# 5. R package repository for webR. repo.r-wasm.org is built for this webR
#    release, so it supplies every package it has. The Stan stack comes from
#    r-universe: mlumr and multinma exist only there, and their compiled models
#    must load against the rstan, StanHeaders, RcppParallel and Rcpp they were
#    built with. (Some r-universe builds of Fortran packages, mvtnorm for one,
#    do not load in webR 0.6.0, which is why the rest come from repo.r-wasm.org.)
#    Every version requirement in Depends and Imports must hold for the
#    versions picked, every archive must match the checksum its repository
#    publishes, and site/repo/lock.tsv records what was published. Versions
#    are not frozen: the Playground follows mlumr's GitHub main and these
#    repositories, and the lock says exactly what each deploy carried.
Rscript --vanilla - "$R_MINOR" <<'EOF'
r_minor <- commandArgs(trailingOnly = TRUE)[1]
contrib <- function(repo) sprintf("%s/bin/emscripten/contrib/%s", repo, r_minor)
sums <- c("SHA256", "MD5sum")
universe <- available.packages(contriburl = contrib(c("https://choxos.r-universe.dev",
                                                      "https://cran.r-universe.dev")),
                               fields = sums, filters = "duplicates")
webr <- available.packages(contriburl = contrib("https://repo.r-wasm.org"), fields = sums,
                           filters = "duplicates")
stan_stack <- c("mlumr", "multinma", "rstan", "StanHeaders", "RcppParallel", "Rcpp",
                "RcppEigen", "BH", "rstantools")
pick <- function(p) if (p %in% stan_stack || !p %in% rownames(webr)) universe[p, ] else webr[p, ]
# The Playground's core set, then what the vignettes' code reaches for.
wanted <- c("mlumr", "posterior", "jsonlite", "codetools", "detectseparation",
            "knitr", "ggsurvfit", "flexsurv", "bayesplot", "loo", "multinma")
base <- rownames(installed.packages(priority = "base"))
pkgs <- character()
needs <- list()
todo <- wanted
while (length(todo)) {
  p <- todo[1]
  todo <- todo[-1]
  if (p %in% c(pkgs, base)) next
  if (!p %in% c(rownames(universe), rownames(webr))) stop("No WebAssembly build of ", p)
  pkgs <- c(pkgs, p)
  row <- pick(p)
  fields <- row[c("Depends", "Imports")]
  deps <- trimws(unlist(strsplit(paste(fields[!is.na(fields)], collapse = ","), ",")))
  deps <- deps[nzchar(deps)]
  m <- regmatches(deps, regexec("^([A-Za-z0-9.]+)\\s*(?:\\(\\s*([<>=!]+)\\s*([^)]+?)\\s*\\))?$", deps))
  for (x in m) if (length(x) && nzchar(x[3])) needs[[length(needs) + 1]] <- c(by = p, pkg = x[2], op = x[3], ver = x[4])
  deps <- vapply(m, function(x) if (length(x)) x[2] else "", "")
  todo <- c(todo, setdiff(deps[nzchar(deps) & deps != "R"], c(pkgs, base)))
}
ap <- do.call(rbind, lapply(pkgs, pick))
rownames(ap) <- pkgs
# webR 0.6.0 is R 4.6.0; its base packages come with it, at R's version.
have <- function(p) if (p == "R" || p %in% base) paste0(r_minor, ".0") else if (p %in% pkgs) ap[p, "Version"] else NA
unmet <- Filter(function(n) {
  v <- have(n[["pkg"]])
  !is.na(v) && !do.call(n[["op"]], list(package_version(v), package_version(n[["ver"]])))
}, needs)
if (length(unmet)) {
  stop("Version requirements not met by the packages picked:\n",
       paste(vapply(unmet, function(n) sprintf("  %s needs %s %s %s, has %s", n[["by"]], n[["pkg"]], n[["op"]],
                                                n[["ver"]], have(n[["pkg"]])), ""), collapse = "\n"))
}
# TRUE or FALSE against the repository's checksum, NA if it publishes none.
matches <- function(path, p) {
  if (!is.na(ap[p, "SHA256"])) return(identical(unname(tools::sha256sum(path)), ap[p, "SHA256"]))
  if (!is.na(ap[p, "MD5sum"])) return(identical(unname(tools::md5sum(path)), ap[p, "MD5sum"]))
  NA
}
out <- file.path("site/repo/bin/emscripten/contrib", r_minor)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
for (p in pkgs) {
  f <- paste0(p, "_", ap[p, "Version"], ".tgz")
  # One cache folder per repository: two repositories can hold different
  # builds under the same file name.
  # A cached archive is used only while it still matches the repository's
  # checksum (a same-version rebuild upstream replaces it).
  cache <- file.path(".cache/repo", sub("/.*", "", sub("^https?://", "", ap[p, "Repository"])))
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(file.path(cache, f)) || isFALSE(matches(file.path(cache, f), p))) {
    download.file(file.path(ap[p, "Repository"], f), file.path(cache, f), quiet = TRUE, mode = "wb")
    if (isFALSE(matches(file.path(cache, f), p))) {
      unlink(file.path(cache, f))
      stop(f, " from ", ap[p, "Repository"], " does not match the checksum its repository publishes")
    }
  }
  file.copy(file.path(cache, f), file.path(out, f), overwrite = TRUE)
}
# (r-universe gives each binary's own URL as its Repository; the lock keeps the folder.)
lock <- data.frame(package = pkgs, version = ap[pkgs, "Version"],
                   repository = sub("/[^/?]+[.]tgz[?].*$", "", ap[pkgs, "Repository"]),
                   sha256 = unname(tools::sha256sum(file.path(out, paste0(pkgs, "_", ap[pkgs, "Version"], ".tgz")))),
                   checked_against = ifelse(!is.na(ap[pkgs, "SHA256"]), "SHA256",
                                            ifelse(!is.na(ap[pkgs, "MD5sum"]), "MD5sum", "none")))
write.table(lock, "site/repo/lock.tsv", sep = "\t", quote = FALSE, row.names = FALSE)
tools::write_PACKAGES(out, type = "mac.binary")
writeLines(pkgs, "site/repo/packages.txt")
utils::untar(file.path(out, paste0("mlumr_", ap["mlumr", "Version"], ".tgz")),
                     files = "mlumr/DESCRIPTION", exdir = tempdir())
d <- read.dcf(file.path(tempdir(), "mlumr", "DESCRIPTION"))
# The Stan programs this mlumr carries, for the check against the models below.
unlink(".build/mlumr-stan", recursive = TRUE)
utils::untar(file.path(out, paste0("mlumr_", ap["mlumr", "Version"], ".tgz")), files = "mlumr/stan",
             exdir = ".build/mlumr-stan")
writeLines(c(paste("version", d[1, "Version"]), paste("commit", d[1, "RemoteSha"]),
             paste("built", d[1, "Built"])), "site/repo/mlumr.txt")
cat(sprintf("repo: %d packages, %.1f MB, %d version requirements met, %d archives checked (%d with no checksum)\n",
            length(pkgs), sum(file.size(list.files(out, "[.]tgz$", full.names = TRUE))) / 2^20, length(needs),
            sum(lock$checked_against != "none"), sum(lock$checked_against == "none")))
EOF

# The WebAssembly models must have been compiled from exactly the Stan
# programs of the mlumr just packaged (mlumr comes from r-universe's build of
# GitHub main, which moves); otherwise new R code would hand old models data
# they do not declare. Stops and names the models to rebuild.
node scripts/stan-hash.mjs check models/manifest.json .build/mlumr-stan/mlumr/stan models

# 6. Example scripts: each vignette's code, from the vignette sources at the
#    very commit the mlumr binary above was built from (scripts/vignette-code.R).
Rscript --vanilla scripts/vignette-code.R site/repo/mlumr.txt site/examples .cache/vignettes

du -sh site
