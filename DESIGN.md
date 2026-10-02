# mlumr Playground: design notes

## What it is

An R IDE in the browser, laid out like RStudio: R 4.6.0 through webR 0.6.0,
the mlumr package from GitHub main, and mlumr's ten Stan models compiled to
WebAssembly. It replaces the earlier mlumr Playground on this branch and keeps
its name and address.

## Layout

RStudio's four panes, in RStudio's places:

| Pane | Place | Contents |
|---|---|---|
| Source | top left | File tabs with type badges, close buttons and unsaved markers (the name turns red and gains an asterisk). Toolbar: back and forward through tabs, save, Source on Save, find and replace, insert `<-`; on the right Run, previous and next section, and Source with a menu (with or without echo). CodeMirror 6 editor: R highlighting, line numbers, active line, bracket matching, a guide at column 80. Run (Ctrl+Enter) sends the selection, or else the whole statement under the cursor, however many lines it spans, and moves to the next one, as RStudio does. Status line: line:column, (Top Level), file type. Data frames open here as viewer tabs. |
| Console | top right | Tabs Console, Terminal (explains that there is no system shell in a browser) and Background Jobs (each Stan fit, one progress bar per chain). Header with the R version and `~/`, the R startup banner, a colored prompt, history on the arrow keys, Esc or Ctrl+C to interrupt, Ctrl+L to clear. A Stop button appears while R is busy. |
| Environment | bottom left | Tabs Environment and History. Toolbar: Import Dataset (CSV), the R worker's memory, clear workspace (broom), List and Grid views, refresh. A scope line (R, Global Environment) and a search box. Objects grouped into Data, Values and Functions, each with class, a short value or dimensions, and size; lists and fits expand level by level; data frames and matrices open in the data viewer. "Environment is empty" when it is. Refreshed after every top-level command. |
| Output | bottom right | Tabs Files, Plots, Packages, Help, Viewer. Plots keeps a history (back and forward), Zoom, Export as PNG or SVG, delete one, clear all, and redraws at the pane size. Help renders mlumr's Rd pages as HTML, follows links between topics, lists the vignettes, and catches `?topic` from the console. Vignettes open in the Viewer with their formulas typeset by KaTeX. Packages lists installed packages, attaches and detaches them, and installs more from the WebAssembly repositories. |

Every pane has minimize and maximize controls. The splitter between the
columns and the splitter between the rows drag; the two bottom panes always
share one height, as in RStudio, and the sizes are remembered. The top bar has
new, open, save and save all, the Examples menu, "Go to file/function" (open
files, functions in the workspace, mlumr help topics), the keyboard shortcut
sheet, the theme switch, and on the far right a project-style chip with mlumr
and the commit it runs (its menu resets the examples, clears the console or
workspace, restarts R, and switches the Stan sampler). A status bar at the
bottom shows R's state, the mlumr version and commit, the sampler, a running
fit's overall progress, and how long the current or last command took.

## What comes from the earlier Playground and the lesson

* Tokens: the same palette, value for value where it exists (`--accent`
  #1a6d73 light and #5fb8c1 dark, `--accent-strong`, the `--surface` steps,
  `--ink` to `--faint`, `--ok`, `--warn`, `--review`, the series colors). The
  dark theme is built from the lesson's dark tokens, with the page background
  one step darker for RStudio's look; the editor and console sit on the
  lesson's code background (#0b1417).
* Type: IBM Plex Sans for the interface and IBM Plex Mono for code and output,
  self hosted from `@fontsource`.
* Components: the pill theme switch (track and thumb), pill badges, rounded
  panels with hairline borders, the accent underline on the active tab, the
  logo with the wordmark (`mlumr` bold, `Playground` in the accent color).
* Structure, controls and shortcuts come from RStudio, as the maintainer asked.

## Decisions

* **Own interface on webR's JavaScript API**, not webR's demo app: the console
  is driven by `webR.read()` and `writeConsole()`, one line per prompt so
  output and echo interleave as in a real console; panes query R with
  `evalR` only while R waits at the prompt. Output is drawn in batches, so a
  long listing does not stall the page.
* **CodeMirror 6** rather than Monaco: about a tenth of the size, and its legacy
  R mode is enough. Colors are CSS variables, so the theme switch recolors the
  editor without rebuilding it. Run finds the statement under the cursor by
  scanning brackets, strings, comments and trailing operators;
  `scripts/statement-test.mjs` checks it on every build.
* **The examples are the vignettes' own code.** `scripts/vignette-code.R` purls
  each vignette's source at the commit of the bundled mlumr binary. It keeps
  the code as written, with three exceptions that make a vignette a plain
  script: `knitr::kable(x)` becomes `x`, the knitr setup chunk is left out,
  and chunks the vignette shows without running stay commented out (this
  honors a global `eval = FALSE`, which `purl()` alone does not see). A data
  file the code reads from the repository is shipped beside the scripts and
  placed where its relative path points.
* **Everything vendored at build time**: webR, the R packages (a local
  repository under `site/repo/`), the Stan models, KaTeX, the fonts and the
  bundled JavaScript. Once built, the site needs no network.
* **Packages from two sources.** repo.r-wasm.org is built for this webR
  release, so it supplies what it has. mlumr, multinma and the Stan stack they
  link against come from r-universe. Two r-universe Fortran builds, deSolve and
  muhaz (both needed by flexsurv, which mlumr's survival STC uses), import
  their COMMON blocks instead of defining them, which webR 0.6.0's loader
  cannot resolve; `shim/fortran-commons.so`, a 327-byte side module loaded at
  startup, defines that storage. With it, deSolve, muhaz and flexsurv agree
  with native R to 12 digits.
* **Packages a script needs install before it runs.** Run and Source scan the
  code (and any file it sources) for `library()`, `pkg::` and `package =`,
  plus the optional packages mlumr loads itself, and install what is missing
  from the site's repository first. Installing from inside a running script
  cannot resume some calls, so the script would otherwise stop.
* **Stan in browser workers.** `mlumr()` itself is unchanged: it validates the
  data, builds the Stan data and, after sampling, computes every summary and
  effect with the package's own code. Only the sampling step is replaced, by a
  `tinystan` engine added at run time: the R worker posts the Stan data and a
  shared buffer to the page and waits; the page runs one TinyStan worker per
  chain, shows each chain's progress, and writes the draws back. This needs a
  cross-origin isolated page: `serve.py` sends the headers, and on GitHub
  Pages `coi-sw.js` adds them to documents and worker scripts only. Without
  isolation the Playground falls back to rstan inside the R worker (slower,
  chains one after another).
* **The models are compiled from main, not reused.** `build-models.sh`
  compiles all ten Stan programs, as committed, with TinyStan and emscripten
  (Stan 2.40.0), and `verify/` checks each binary against native CmdStan.
* **Files persist in the browser.** Open files, including unsaved edits, live
  in IndexedDB; a reload restores them with their unsaved markers. "Reset
  examples" in the project menu restores the original scripts.
* **Restart R reloads the page.** webR has no in-place restart that keeps the
  page state, and the files and layout persist anyway.
