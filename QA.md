# Lesson execution record

This records the checks run on the lesson in this branch. The first version was published at https://choxos.github.io/mlumr/lesson/ on September 13, 2026. The newest revision is described first; the September 12 to 13 record follows it unchanged.

## Revision of September 15, 2026

### What this revision repairs

- **Fit finalization.** A fit fixes its model, code text, code revision, prepared data, R session and an active-fit token before anything awaits. Its view and run record are built off-state and committed together only if the fit is still the cell's active fit and was not cancelled; the catch path also writes nothing once a newer fit owns the cell. A fit cancelled while hashing commits nothing. The run record now carries the build id, the mlumr and Tangible commits, the compiled model's SHA-256 and stanc version, the TinyStan and webR versions, the R code as it ran, and a statement of what the record contains; a panel above the download button says the same.
- **Chapter menu name.** At narrow and short sizes the menu label leaves the screen but stays in the accessibility tree, so the disclosure keeps the name Chapters.
- **Chart text.** Ticks are 13 SVG units, labels 13.5 and annotations 14 on a 600 unit drawing; the chart column takes two thirds of the lesson width, becomes its own container, and the lesson drops to one column below 800 px of width. Readability target: tick text renders at 11 CSS pixels or more, measured as font size times the SVG screen scale, at every tested size.
- **Captions.** The lesson measures the caption bar and reserves its real height, so a two or three line cue no longer covers the bottom of the lesson or the notes.
- **Selection.** Prose, notes and code examples can be selected and copied; sliders, buttons, the stepper and the charts stay unselectable. Every native code example has a Copy button.
- **Held explanations.** Opening an explanation, answering a question or copying code holds the chapter like touching a control does, so the narration's next cue does not replace an opened panel.
- **Play this chapter, Reset controls, transcript.** Play this chapter seeks the narration to the chapter on screen and plays it. The reset button is named Reset controls and says that the narration and the code are untouched. `transcript.html` gives the spoken text by chapter, linked from the header and from the bottom of the chapter menu.
- **Publication gate.** `.github/workflows/deploy-lesson.yaml` runs the unit tests, the manifest check, the model check and the browser checks on every pull request into `lesson` and before every deployment (pull request #91).
- **Text and assessment.** The continuous-outcome narration now divides the standard deviation of individual outcomes, not of an average, by the square root of n, keeps a reported standard error as it is, and says that an adjusted mean standardized to another population is a different quantity from the row's own mean; the families panel, the checklist and the package notes say the same. The LOO case and its narration compare models by the paired difference of pointwise scores with its standard error and name the held-out unit, with a second illustration chart for the paired difference. The browser cell states its fixed settings and that the native companion uses simulated comparator rows, 512 points and four chains; the report chart in the last chapter is labeled as the native companion's fits; plain code cells say the chart controls do not edit them; the continuous, count and survival routes are labeled previews. The knowledge check has ten questions (anchor status, percentage points, target weights, SD and adjusted mean, overlapping tables added), and the last chapter carries a capstone with deliverables and a rubric marked as unvalidated. Optional panels add three routes with prerequisites and outcomes plus four numeric cards and a glossary (chapter 1), a stop or go map (chapter 2), the joint likelihood with an equation-to-function map (chapter 3), and a table of what the posterior interval includes and leaves out, with credible against confidence intervals (chapter 12 and the fit output). `sources.html` names the two things on the pages that are fits.

- **Native sensitivity loop.** `workflow.R --sensitivity` refits at 2048 integration points, sweeps the comparator slope prior with the index prior fixed, and evaluates a shifted and an extrapolating target, re-extracting the prespecified target effect from every refit; `--record` writes the results as JSON with the package version, the checkout's commit and clean or dirty state, the engine and the script's SHA-256, and `dist-manifest.mjs verify` refuses a build whose record names another commit than the pin, a tree with local changes, compiled code older than its sources, or a script SHA-256 other than the committed `workflow.R`; the script itself loads the checkout with `compile = NA`, which rebuilds an older DLL. The script folds draws by the fit's own chain labels for the MCSE and ESS and stops on a fit that lost a chain. `scenes/native-record.json` is that record, and the report chart in the last chapter and a panel in the priors chapter read it, so no fitted number is typed into the lesson by hand. `package-notes.md` records the run and how to read it.

### Executed checks

```sh
bash lesson.sh test
bash lesson.sh dist
node browser-qa.mjs http://127.0.0.1:4174/
Rscript workflow.R --source=<mlumr checkout at 4cfd366> --fit --sensitivity --record=scenes/native-record.json --engine=cmdstanr
```

- `browser-qa.mjs --no-runtime` in Chrome on the build that carries the native record: 0 page errors, 0 console errors, 0 failed requests; the sensitivity panel and the report chart render from the record; every other check above passed again. Rerun in Playwright's bundled Chromium, the browser the check job uses (`QA_BROWSER_CHANNEL= node browser-qa.mjs http://127.0.0.1:4174/ --no-runtime`, an empty channel; before this branch was rebased onto the check job's launch line, the same run used a temporary copy of the script with `chromium.launch()` and no channel), on the final build: 0 errors, and the new checks passed: the sensitivity panel names the record's commit and clean tree, shows all fourteen rows and says that the transport rows evaluate the base fits in other targets; the six function names under the workflow steps, now on two alternating rows, do not touch (the old single row overlapped by 3 to 8 pixels between set_agd() and add_integration() and between summary() and marginal_effects()); the Transcript link is visible in the header above 640 px and inside the open chapter menu at all fifteen sizes (a header link at 320 px overflowed the page by 54 px, so below 640 px the menu row is the route); and the Copy button must reach the Copied state, which it did with the clipboard equal to the code.
- `workflow.R --fit --sensitivity --record`: exit code 0 with R 4.6.0, cmdstanr 0.9.0, CmdStan 2.39.0 and mlumr 0.1.0.9000 at `4cfd366`; twelve fits, all with 0 divergences and a largest R-hat of at most 1.007; the base fits reproduce the September 12 record exactly; the record's script SHA-256 equals that of the committed `workflow.R`. Run six times: the second run, after the script learned to record the checkout commit, and the third, after the MCSE fold switched to the fit's own chain labels and the script began refusing a fit that lost a chain, both wrote the same numbers to the last digit; the fourth, after each refit received its own seed (2026 plus the scenario number) so that the MCSE of a difference between rows combines two independent errors, kept the base fits and the transport rows to the last digit and changed the integration and prior rows within their Monte Carlo error (the integration shift is now 0.6 and 0.2 MCSEs of the difference; the prior span 0.0028); the fifth, after the script began loading the checkout with `compile = NA` and recording the DLL's hash and currency, reproduced the fourth to the last digit; the sixth, after `--record` began requiring `digest` and `jsonlite` before any fit runs, reproduced it again; the record carries `commit 4cfd366..., dirty false` and chain and draw counts read from the fits (4 and 4000). A tampered record (another commit, dirty tree) makes `dist-manifest.mjs verify` fail with both messages, a record left over from the previous script fails its script SHA-256 check, a record without the fits fails the completeness check, a record without the DLL currency flag or with it false fails that check, a record whose fields have the wrong shape fails the shape check (witnessed with an empty target interval, an empty sensitivity row, a null bound and a non-numeric truth), `--record` without `--fit` stops before loading anything, and the committed record passes. The script's own refusal of a stale DLL was not exercised: making the pinned checkout's sources newer than its DLL would trigger a rebuild of the Stan models. The refusal was exercised on a fake fit with three of four chains and on one without chain labels; a complete fit passes unchanged. The table and its reading are in `package-notes.md`.

- `lesson.sh test`: strict TypeScript passed for the scene and the worker; **74 unit tests** passed in 6 files. The fourth new code cell test, added last, fails against the controller before it: a manifest replaced while the chains ran was named in the record (`expected 'build-id-2' to be 'build-id-1'`); the controller now requests the manifests when the fit starts, ends the finalization at once on cancel instead of after the slowest promise, and bounds each manifest request to 15 seconds. The three earlier new code cell tests fail against the previous controller: the record held the older fit's run number (2 instead of 4) after a cancelled fit finished hashing late, the record had no code text, and a fit cancelled while hashing still published its result.
- `lesson.sh dist`: `check: no errors`; the model manifest matched; 29 sources and 92 built files recorded, and `dist-manifest.mjs verify dist` passed, including the new check that the build id is the hash of the recorded sources. The narration was resynthesized for the changed sentences and lasts **1690.60 seconds** (28 minutes 11 seconds); no human listened to it.
- `browser-qa.mjs` in Chrome ran twice. With the R and Stan runtimes, on the build before the text changes: **0 page errors, 0 console errors and 0 failed requests**, every check of the earlier record passed again, including both browser Stan fits with the tables below the earlier record and Stan data equal to the native adapter check's within 3.3e-14 relative. With `--no-runtime` on the final build with the text changes: 0 errors, ten questions answered wrong and right, fifteen sizes, and the checks below passed again; the runtime code did not change between the two builds. New checks:
  - **Held explanation.** With the narration paused at the start of the diagnostics chapter and no exploration active, the first explanation was opened; the chapter held, the narration was moved past the cue that changes the case, and the explanation stayed open with its case unchanged. Return to narration then showed the narrated case and ended the exploration.
  - **Play this chapter.** From the survival chapter opened while another chapter was narrated, Play this chapter moved the narration to 1112.6 seconds, the chapter's first cue, started playback, and ended the exploration.
  - **Selection and copy.** A pointer drag selected the step's code, a triple click selected a paragraph, and the Copy button wrote the code to the clipboard, which read back equal to the shown code including line breaks.
  - **Run record.** Each downloaded record named the served build id, the pinned mlumr commit and the compiled model's SHA-256, its code hash equaled the SHA-256 of the code text it carries, and the panel describing the record's contents was present before download.
  - **Sizes.** Fifteen sizes, with the survival chapter, the longest caption (225 characters) shown with captions on, and the chapter menu read closed and open through Chromium's accessibility tree, which returned the name Chapters at every size. None scrolls sideways, header controls do not overlap, and every slider is at least 44 pixels tall. The caption bar's top was never more than 0.4 px above the lesson's bottom edge (subpixel rounding), and the notes board ended above it.

| Viewport | Tick text (px) | Tick box (px) | Caption lines |
| --- | ---: | ---: | ---: |
| 1024 × 768 | 14.6 | 19.0 | 4 |
| 1100 × 768 | 15.8 | 20.0 | 3 |
| 1152 × 800 | 16.2 | 21.0 | 3 |
| 1280 × 800 | 11.7 | 15.0 | 3 |
| 1366 × 768 | 12.6 | 16.0 | 3 |
| 1440 × 900 | 13.4 | 18.0 | 3 |
| 1600 × 900 | 15.6 | 20.0 | 3 |
| 667 × 375 | 11.3 | 15.0 | 3 |
| 844 × 390 | 12.3 | 16.0 | 3 |
| 896 × 414 | 13.2 | 17.0 | 2 |
| 320 × 640 | 11.3 | 15.0 | 6 |
| 360 × 740 | 11.3 | 15.0 | 5 |
| 390 × 844 | 11.3 | 15.0 | 5 |
| 412 × 915 | 11.3 | 15.0 | 5 |
| 720 × 450 | 11.3 | 15.0 | 3 |

The tick text column is the metric the assertion uses (font size times SVG scale, threshold 11 px); the box column is the rendered bounding height the earlier record reported. Neither is a universal accessibility minimum; 11 px is this lesson's target. Sizes from 1024 to 1152 wide are single column, 1280 and up two columns, phones scroll the chart inside its card at 520 px. Fonts were IBM Plex from Google Fonts.

### Not done in this revision

- No human listened to the narration. The sentences changed in this revision were resynthesized and checked only by decoding; the rest is unchanged since the previous revision.
- No screen reader session and no real browser zoom were run; the accessibility tree was read through CDP and sizes were emulated, with 720 × 450 standing in for 1440 × 900 at 200% zoom.
- Firefox and Safari were not run.
- The WebAssembly models were not rebuilt, and log-density parity between the browser and native models was not measured; the check remains the Stan data equality and posterior agreement recorded below.

## Record of September 12 to 13, 2026

The first version was published on September 13; the repairs described below were checked the same day.

## Provenance

- **Package.** mlumr development version 0.1.0.9000 at `main` commit `4cfd3660f56e22668ae357bde3df4b30cacb23a5`. Every function, argument and default the lesson names was checked against that commit's NAMESPACE, help pages and R source. The survival effect wording follows the pinned `marginal_effects.Rd`.
- **Tangible.** Compiler and player at `6a07bbcd5dbc548aa809b253aed503fc0a8c3251`, unchanged. Narration is local Supertonic 3 speech.
- **Browser runtimes.** webR 0.6.0 from webr.r-wasm.org, with randtoolbox, jsonlite and detectseparation from the webR package repository. TinyStan 0.3.3 with the `mlumr_binary_spfa` and `mlumr_binary_relaxed` WebAssembly models from the `webapp` branch, compiled with stanc 2.39.0 from mlumr commit `370002f`. `runtime/models/manifest.json` records their SHA-256 and the SHA-256 of each Stan program with its includes expanded; the build confirmed that the programs at `4cfd366` match.

## What this revision repairs

- **Code cells.** A cell owns its listeners and is disposed before the next chapter mounts, so one Run runs only the visible code. A fit fixes its model and code revision before anything awaits. Fit is enabled only for the code revision whose Run succeeded. All R work shares one queue. Drafts, output and results are kept per chapter, and leaving a chapter cancels a fit and says so. R cannot be interrupted, so cancelling a fit while R prepares the data restarts R, and a restart refuses every R call queued in the old session. Fit also requires that the R session which ran the code is still the current one, so a restart from any chapter disables it. A shown fit is marked stale when the code or the Run that created its dat changes, even if the code is the same. Editing, running or fitting keeps the chapter open while the narration continues.
- **Browser fit.** The Fit button runs the public `mlumr()`, with its checks and warnings; only its hand-off to a Stan backend is replaced. It samples a copy of the `dat` that the cell's latest error-free Run created, so a Run that creates no `dat`, or another cell that overwrites it, cannot change what is fitted. Worker replies are validated, all chain workers belong to one job that ends them together, and loading or sampling that stalls times out. Each fit reports divergences, tree depth hits, rank-normalized split R-hat, bulk and tail ESS and MCSE across every model quantity, and can be downloaded as a run record. Benchmarks keep their warnings.
- **Charts.** No value is moved to the edge of a chart. Axes widen to hold every value, and a chart with a fixed frame clips visibly and prints the exact values. Screen readers get the active values as text.
- **Text.** Conditional and marginal effects are named as such, the identification quiz is limited to the identity link, the RMST question states what must be held fixed, the SD to SE rule is qualified, Normal notation uses variances, the survival API text follows the pinned help page, and an assumptions box states what an unanchored comparison needs. The prior cell compares both priors in one Run.
- **Screens and contrast.** Light muted text, reference lines and the review color were darkened to pass WCAG contrast. On phones the notes board is hidden, the chapter list stays on screen, the header fits at 320 pixels, charts keep readable labels and scroll inside their cards, and portrait orientation is allowed.
- **Build and publishing.** The build refuses a Tangible checkout with local changes and rebuilds its packages from source, refuses Stan programs or a TinyStan version that do not match the models, hashes every source when it starts and every built file when it ends, and the deploy workflow refuses a `dist/` that does not match its commit.

## Executed checks

All commands ran from this branch with `TANGIBLE_DIR` pointing to the pinned Tangible checkout.

```sh
bash lesson.sh test
bash lesson.sh build
MLUMR_NATIVE=<mlumr checkout at 4cfd366 with a built DLL> bash lesson.sh adapter
node browser-qa.mjs http://127.0.0.1:4202/
```

- `lesson.sh test`: strict TypeScript passed for the scene and, with the WebWorker library, for the Stan worker. **70 unit tests** passed in 6 files:
  - **Mathematics.** The teaching mathematics, including the survival case with a population hazard ratio of 2.39839 and the log-link counterexample.
  - **Charts.** Chart coordinates across a survival control grid.
  - **Code cells.** The code cell lifecycle, with the R and Stan runtimes replaced by test doubles, including a Run that succeeds without creating `dat`, a fit cancelled while R prepares the data, a Fit after R restarts from another chapter, a shown fit whose dat a new Run replaced, a Run that finishes after its chapter was left and opened again, and R work queued or still running when R restarts.
  - **Worker protocol.** Empty, truncated, reordered and non-finite results, non-finite sampler diagnostics, and failed, cancelled and stalled workers.
  - **Contrast.** WCAG contrast for both themes.
  - **Diagnostics.** R-hat, ESS and MCSE against 17 reference cases generated with the posterior R package, version 1.7.1, which the fixture records and the tests check.

  The chart, code cell and runner tests fail when run against the modules they replaced, and the seven newest tests, for the restart, cancel, stale result and sampler diagnostic paths, fail against the modules of the commit before each was added.
- `lesson.sh build`: `check: no errors`; the model manifest matched; the compiled narration lasts **1646.10 seconds** (27 minutes 26 seconds). After the build, adding a character to `script.md` made `node dist-manifest.mjs verify build/site` fail, and removing it made the check pass again.
- `lesson.sh adapter`: all checks passed. It ran with every installed copy of mlumr hidden, as in the browser, and compared the browser's files with the package loaded from the pinned checkout:
  - **Stan data.** The Stan data for both models equal the package's own `mlumr()` data.
  - **Refusals and warnings.** The browser and the package give the same outcome and the same message or warnings when `dat` is missing, when integration points are missing, and for a relaxed fit with one aggregate row.
  - **A typed `mlumr()` call** explains that the browser cannot run the Stan backend.
  - **A browser-only failure.** Hiding the installed package exposed a failure that native runs could not see: `default_prior_sigma()` asks for the installed package's version. It is fixed, and the check fails without the fix. The fix keeps its library in R's temporary directory, so running the check leaves no file in the built site, and `node dist-manifest.mjs verify build/site` still passes afterward.
- `browser-qa.mjs` in Chrome finished with **0 page errors, 0 console errors and 0 failed requests**. The only requests it treated as expected are media requests the browser drops when the narration seeks, and the request a download link becomes. It covered the following:
  - **Narration.** The 1646.10 second narration plays unmuted, and all 13 chapters can be browsed without pausing or seeking the voice. There are no automatic pauses: the voice crossed all 12 chapter boundaries while another chapter was open, and it plays to the end and replays.
  - **Exploration.** Touching a control keeps a chapter open when the narration moves on, and Return to narration works by mouse, keyboard and touch. Reset restores the chapter's starting values without changing playback.
  - **Controls.** Every slider was moved to both ends, and the check fails if a slider does not change what the chapter shows. Every select option was chosen, and the analysis steps work by arrows, by clicking the chart and by Reset.
  - **Content.** All 7 diagnostic explanations open, all 5 questions give wrong-answer and right-answer feedback, and every chapter shows a chart. In the survival chapter at month 11.5, a target share of 0.99 and a marker effect of 2.5, the population hazard ratio is drawn above the HR = 1 line, and the text says A's survival curve stays above B's.
  - **Themes.** Both themes render, and the choice is saved.
  - **R in the browser.** Three of the six R cells ran. The integration cell printed `0.2704838`, and one Run of the prior cell printed both priors. After three return visits to the workflow chapter, one Run echoed its code once. `naive()` and `stc()` printed through the package's own methods, and a code draft survived a chapter change. Another cell then overwrote `dat` before both fits, and the fitted Stan data still matched the native check. After the fits, a Run of `x <- 1` said it created no `dat` and left Fit disabled. Cancelling a fit while R prepared the data restarted R in Chrome, and a Run in the new session made Fit available again.
  - **Stan in the browser.** Both models were fitted; the tables and checks are below. Each run record downloaded, with the right model and 1000 draws. The Stan data it contains have the same names and shapes as the native adapter check's, and every value agrees within a relative difference of 3.3e-14. The difference comes from floating point in the integration points.
  - **Screens and files.** Nine sizes were checked: 1024 × 768, 667 × 375, 844 × 390, 896 × 414, 320 × 640, 360 × 740, 390 × 844, 412 × 915, and 720 × 450, a 1440 × 900 window at 200% zoom. None scrolls sideways, chart labels are at least 13 pixels tall, header controls do not overlap, and every slider is at least 44 pixels tall. `workflow.R`, `sources.html`, `captions.vtt` and `tracks.json` are served.

Both fits used 2 chains of 500 warmup and 500 kept iterations, seed 2026, adapt_delta 0.95 and maximum tree depth 15, on the workflow cell's data.

| Shared slopes | Mean | 2.5% | 97.5% | MCSE | R-hat | Bulk ESS | Tail ESS |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `lor_comparator` | -0.517 | -0.810 | -0.235 | 0.0050 | 1.000 | 852 | 543 |
| `rd_comparator` | -0.126 | -0.195 | -0.058 | 0.0012 | 1.000 | 852 | 555 |
| `lor_index` | -0.533 | -0.833 | -0.242 | 0.0051 | 1.000 | 851 | 555 |
| `rd_index` | -0.118 | -0.182 | -0.055 | 0.0011 | 1.000 | 853 | 576 |

No divergences and no tree depth hits in 1000 draws. Across all 327 model quantities, the largest R-hat was 1.010, the smallest bulk ESS 827 and the smallest tail ESS 527.

| Separate slopes | Mean | 2.5% | 97.5% | MCSE | R-hat | Bulk ESS | Tail ESS |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| `lor_comparator` | -0.503 | -0.804 | -0.209 | 0.0042 | 1.006 | 1341 | 627 |
| `rd_comparator` | -0.123 | -0.194 | -0.052 | 0.0010 | 1.005 | 1347 | 627 |
| `lor_index` | -0.564 | -0.868 | -0.267 | 0.0047 | 1.001 | 1137 | 639 |
| `rd_index` | -0.125 | -0.192 | -0.060 | 0.0010 | 1.002 | 1126 | 663 |

No divergences and no tree depth hits in 1000 draws. Across all 331 model quantities, the largest R-hat was 1.008, the smallest bulk ESS 1090 and the smallest tail ESS 555. On the same data, the browser's `stc()` gave a log odds ratio of -0.489 in trial B's population (95% CI -0.798 to -0.179), and `naive()` gave -0.985 (95% CI -1.288 to -0.682), a comparison of different populations.

## Companion script

`Rscript workflow.R --source=<checkout at 4cfd366> --fit --engine=cmdstanr` finished with exit code 0 on September 12. Both fits kept 4000 draws, with no divergences and no treedepth hits. The largest R-hat was 1.003 for shared slopes and 1.006 for separate slopes. The details are in [package-notes.md](package-notes.md).

## Limits of this record

This is technical verification. Not done:
- **Narration.** No human listened to the regenerated narration or checked caption alignment, and Supertonic estimates word timing inside each sentence.
- **Accessibility.** No screen reader session was run, and 400% zoom was not tested.
- **Native survival calls.** The survival `marginal_effects()` routes were not executed natively.
- **Models.** The WebAssembly models were not rebuilt from source, and log-density parity was not measured.
- **webR restart.** R was restarted in Chrome only through Cancel during preparation; the Stop and restart R button itself was not clicked.
- **Scope of the fits.** The browser fit covers binary outcomes only, with a teaching budget of draws. RStan fitting, prior refits, repeated simulation and clinical validation were not run.
