# mlumr Playground

An R IDE in your browser for the [mlumr](https://github.com/choxos/mlumr)
package, laid out like RStudio. R 4.6.0 runs in the page through
[webR](https://webr.r-wasm.org/) 0.6.0, mlumr is the build of GitHub main, and
mlumr's ten Stan models are compiled to WebAssembly and sample in one browser
worker per chain. Nothing is installed, and all computation happens in the
browser: no server runs your code or sees your data. The page does download
webR, the R packages and the models, and R code you run can reach the network
as it can on a desktop; the Viewer pane loads nothing from the network.

This branch is app-only: the R package lives on `main`, and this branch deploys
the Playground to `https://choxos.github.io/mlumr/app/`.

## What is in it

* **Source** (top left): tabbed R editor (CodeMirror) with highlighting, line
  numbers, a column 80 guide, find and replace, comment toggling, unsaved
  markers, Source on Save and section navigation. Ctrl+Enter (Cmd+Enter) runs
  the selection or the whole statement at the cursor, even when it spans
  several lines; Ctrl+Shift+Enter sources the file. Data frames open in a
  viewer tab.
* **Console** (top right): R's banner and prompt, colored messages, command
  history on the arrow keys, Esc or Ctrl+C to interrupt (it also stops a Stan
  fit), Ctrl+L to clear. Background Jobs shows each Stan fit, one progress bar
  per chain.
* **Environment** (bottom left): objects grouped into Data, Values and
  Functions with class, size and a short value; lists and fits expand level by
  level; data frames open in the viewer; List and Grid views, search, memory
  use, clear workspace, Import Dataset. History tab.
* **Files, Plots, Packages, Help, Viewer** (bottom right): plot history with
  PNG and SVG export, zoom and redraw at the pane size; installed packages;
  mlumr's help pages and vignettes (formulas rendered with KaTeX); a viewer
  for HTML output.
* **Examples** menu: the R code of every mlumr vignette with runnable code
  (binary, continuous, count and survival outcomes, data preparation, fitting
  and diagnostics, choosing a method, subgroup identification), plus two
  short quick-start scripts. The vignette scripts are extracted from the
  vignette sources at the same commit as the bundled mlumr binary; the code is
  the vignette's own, except that tables print as plain R output (no
  `knitr::kable()`) and the vignettes' knitr setup chunk is left out. Chunks
  the vignette shows without running stay commented out. The subgroup
  vignette's simulation results file ships with the site. `introduction` is
  not included: its code is a schematic with `...` placeholders.
* Light and dark themes, panes that minimize, maximize and resize, and open
  files (with unsaved edits) that survive a reload. "Reset examples" in the
  project menu restores the original example scripts.

## How mlumr and Stan run

* Packages come from this site's own repository (`site/repo/`, built by
  `build.sh`): mlumr and multinma from r-universe (which builds mlumr from
  GitHub main) with the rstan, StanHeaders, RcppParallel and Rcpp builds they
  were compiled against, and every other package from repo.r-wasm.org, whose
  builds match this webR release, except deSolve and muhaz, which only
  r-universe has. Those two need `shim/fortran-commons.so` (see DESIGN.md);
  with it, flexsurv, and so mlumr's survival STC, works. A script's packages,
  and the optional packages mlumr uses, are installed before it runs; the core
  set is installed when the page starts.
* `mlumr()` itself is unchanged. The Playground adds a `tinystan` engine at run
  time and makes it the default: R hands the Stan data to the page through
  shared memory and waits; the page samples with TinyStan in one Web Worker per
  chain and returns the draws; mlumr then computes the summaries, diagnostics
  and effects with its own code. `mlumr_engine("rstan")` switches to rstan
  inside the R worker (slower, one chain after another).
* The shared memory needs a cross-origin isolated page. `serve.py` sends the
  headers; on GitHub Pages, `coi-sw.js` (a small service worker) adds them and
  the page reloads once on the first visit.
* The models (`models/`) are compiled from the Stan sources committed at the
  mlumr commit by `build-models.sh` (emscripten 6.0.9, oneTBB 2021.13.0,
  TinyStan `e78bb62`, Stan and stanc 2.40.0, the recipe of Stan Playground's
  compile server). `models/manifest.json` records the source and binary
  hashes. `verify/` checks each binary against native CmdStan built from the
  same sources (14 cases covering all ten models; largest relative difference
  9.0e-14; `verify/results-recompiled.txt`).

## Build and serve locally

Requirements: `bash`, `curl`, `shasum`, `tar`, Node.js 18 or later with npm,
and R with the knitr and jsonlite packages. Python 3 to serve.

```sh
bash build.sh           # assembles site/ (about 200 MB)
python3 serve.py 8737   # http://127.0.0.1:8737/
```

`build.sh` downloads webR 0.6.0 (pinned by SHA-256), the WebAssembly R
packages and the vignette sources, and bundles the site; it keeps its
downloads in `.cache/` and `.build/`, so rebuilding is fast. The served site
needs no network. Rebuilding the models (`bash build-models.sh`) is needed only
when mlumr's Stan code changes; it installs emscripten, oneTBB and TinyStan
(about 2.3 GB) under `~/.cache/mlumr-wasm-build` or `$WASM_WORK`, takes about
25 minutes, and should be followed by the checks in `verify/`.

## Deployment

`.github/workflows/deploy-app.yaml` runs `build.sh` on every push to this
branch and deploys `site/` to `gh-pages/app/`, beside the pkgdown site (see the
comments in the workflow for how the two deployments coexist). The workflow
has not run yet in this form; the first push is its first test.

## Timings

Measured in headless Chromium on a shared 8-core machine with a load average
of 100 to 420 (other jobs were running), so a normal laptop is faster.

| Step | Time |
|---|---|
| First visit to ready (webR, the core packages, mlumr, the Stan bridge) | 30 to 45 s |
| Quick-start binary script (4 chains of 2000 iterations) | about a minute; sampling 2.5 s per chain |
| Quick-start survival script (Weibull, 32 integration points, 4 chains of 300 + 300) | 6 to 7 minutes |
| `continuous-outcomes` | 46 s |
| `count-outcomes` | 42 s |
| `data-preparation` | 22 s |
| `subgroup-identification` | 21 s |
| `choosing-a-method` | 2 minutes |
| `fitting-and-diagnostics` | 3 minutes |
| `binary-outcomes`, ML-UMR part | 2.5 minutes |
| `binary-outcomes`, anchored check (two multinma fits) | first fit under 25 minutes; both about an hour (estimate) |
| `survival-outcomes`, data, plots and benchmarks up to the first fit | 52 s |
| `survival-outcomes`, each ML-UMR fit (4 chains of 2000 iterations) | hours at this load |

The scripts keep the vignettes' settings. Every script was run here: six of
them to the end exactly as shipped, and `binary-outcomes` and
`survival-outcomes` with their slowest fits shortened for the test. Two parts
are slow in a browser:

* The anchored checks call multinma, which samples with rstan inside the R
  worker, one chain after another. In `binary-outcomes` the first fit ran to
  the end in under 25 minutes and its estimate matched the vignette's (log
  odds ratio 0.153 against 0.157); the two M-spline fits in `survival-outcomes` did not finish a 2-chain,
  100-iteration test in an hour, so at the vignette's 4 chains of 2000
  iterations they are not practical. The code after them was checked with
  shortened fits.
* The survival fits integrate the hazard over 64 points for 639 patients.
  With 4 chains of 200 iterations each ML-UMR fit took 12 to 14 minutes here
  (one Weibull chain, with a short warmup, took much longer), and the results
  already agreed with the vignette's table to about 0.03 on the log hazard
  ratio scale. For a quicker look, the quick-start survival script runs the
  same model with fewer points and iterations.

## Limits

* Tested in Chromium. The Stan bridge needs a growable `SharedArrayBuffer` and
  module workers.
* Survival fits at the vignette's settings are slow (see Timings). Code that
  calls multinma (the anchored checks in the binary and survival vignettes)
  runs rstan inside the R worker, chains one after another, which is much
  slower still.
* The R worker can use at most 2 GB of memory (webR's limit).
* `engine = "cmdstanr"` cannot work in a browser.
* Files written by R live in the session's memory; files open in the editor
  persist across reloads.
* "Restart R" reloads the page.
