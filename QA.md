# Lesson execution record

September 12 to 13, 2026. This records the checks run on the lesson in this branch. The first version was published at https://choxos.github.io/mlumr/lesson/ on September 13; the repairs described below were checked the same day.

## Provenance

- **Package.** mlumr development version 0.1.0.9000 at `main` commit `4cfd3660f56e22668ae357bde3df4b30cacb23a5`. Every function, argument and default the lesson names was checked against that commit's NAMESPACE, help pages and R source. The survival effect wording follows the pinned `marginal_effects.Rd`.
- **Tangible.** Compiler and player at `6a07bbcd5dbc548aa809b253aed503fc0a8c3251`, unchanged. Narration is local Supertonic 3 speech.
- **Browser runtimes.** webR 0.6.0 from webr.r-wasm.org, with randtoolbox, jsonlite and detectseparation from the webR package repository. TinyStan 0.3.3 with the `mlumr_binary_spfa` and `mlumr_binary_relaxed` WebAssembly models from the `webapp` branch, compiled with stanc 2.39.0 from mlumr commit `370002f`. `runtime/models/manifest.json` records their SHA-256 and the SHA-256 of each Stan program with its includes expanded; the build confirmed that the programs at `4cfd366` match.

## What this revision repairs

- **Code cells.** A cell owns its listeners and is disposed before the next chapter mounts, so one Run runs only the visible code. A fit fixes its model and code revision before anything awaits. Fit is enabled only for the code revision whose Run succeeded. All R work shares one queue. Drafts, output and results are kept per chapter, and leaving a chapter cancels a fit and says so. Editing, running or fitting keeps the chapter open while the narration continues.
- **Browser fit.** The Fit button runs the public `mlumr()`, with its checks and warnings; only its hand-off to a Stan backend is replaced. Worker replies are validated, all chain workers belong to one job that ends them together, and loading or sampling that stalls times out. Each fit reports divergences, tree depth hits, rank-normalized split R-hat, bulk and tail ESS and MCSE across every model quantity, and can be downloaded as a run record. Benchmarks keep their warnings.
- **Charts.** No value is moved to the edge of a chart. Axes widen to hold every value, and a chart with a fixed frame clips visibly and prints the exact values. Screen readers get the active values as text.
- **Text.** Conditional and marginal effects are named as such, the identification quiz is limited to the identity link, the RMST question states what must be held fixed, the SD to SE rule is qualified, Normal notation uses variances, the survival API text follows the pinned help page, and an assumptions box states what an unanchored comparison needs. The prior cell compares both priors in one Run.
- **Screens and contrast.** Light muted text, reference lines and the review color were darkened to pass WCAG contrast. On phones the notes board is hidden, the chapter list stays on screen, the header fits at 320 pixels, charts keep readable labels and scroll inside their cards, and portrait orientation is allowed.
- **Build and publishing.** The build refuses Stan programs that do not match the models, hashes every source when it starts and every built file when it ends, and the deploy workflow refuses a `dist/` that does not match its commit.

## Executed checks

All commands ran from this branch with `TANGIBLE_DIR` pointing to the pinned Tangible checkout.

```sh
bash lesson.sh test
bash lesson.sh build
MLUMR_NATIVE=<mlumr checkout at 4cfd366 with a built DLL> bash lesson.sh adapter
node browser-qa.mjs http://127.0.0.1:4201/
```

- `lesson.sh test`: strict TypeScript passed for the scene and, with the WebWorker library, for the Stan worker. **61 unit tests** passed in 6 files:
  - **Mathematics.** The teaching mathematics, including the survival case with a population hazard ratio of 2.39839 and the log-link counterexample.
  - **Charts.** Chart coordinates across a survival control grid.
  - **Code cells.** The code cell lifecycle, with the R and Stan runtimes replaced by test doubles.
  - **Worker protocol.** Empty, truncated, reordered and non-finite results, and failed, cancelled and stalled workers.
  - **Contrast.** WCAG contrast for both themes.
  - **Diagnostics.** R-hat, ESS and MCSE against 17 reference cases generated with the posterior R package.

  The chart, code cell and runner tests fail when run against the modules they replaced.
- `lesson.sh build`: `check: no errors`; the model manifest matched; the compiled narration lasts **1647.14 seconds** (27 minutes 27 seconds). After the build, adding a character to `script.md` made `node dist-manifest.mjs verify build/site` fail, and removing it made the check pass again.
- `lesson.sh adapter`: all checks passed. It ran with every installed copy of mlumr hidden, as in the browser, and compared the browser's files with the package loaded from the pinned checkout:
  - **Stan data.** The Stan data for both models equal the package's own `mlumr()` data.
  - **Refusals and warnings.** The browser and the package give the same outcome and the same message or warnings when `dat` is missing, when integration points are missing, and for a relaxed fit with one aggregate row.
  - **A typed `mlumr()` call** explains that the browser cannot run the Stan backend.
  - **A browser-only failure.** Hiding the installed package exposed a failure that native runs could not see: `default_prior_sigma()` asks for the installed package's version. It is fixed, and the check fails without the fix.
- `browser-qa.mjs` in Chrome finished with **0 page errors, 0 console errors and 0 failed requests**. The only requests it treated as expected are media requests the browser drops when the narration seeks, and the request a download link becomes. It covered the following:
  - **Narration.** The 1647.15 second narration plays unmuted, and all 13 chapters can be browsed without pausing or seeking the voice. There are no automatic pauses: the voice crossed all 12 chapter boundaries while another chapter was open, and it plays to the end and replays.
  - **Exploration.** Touching a control keeps a chapter open when the narration moves on, and Return to narration works by mouse, keyboard and touch. Reset restores the chapter's starting values without changing playback.
  - **Controls.** Every slider was moved to both ends, and the check fails if a slider does not change what the chapter shows. Every select option was chosen, and the analysis steps work by arrows, by clicking the chart and by Reset.
  - **Content.** All 7 diagnostic explanations open, all 5 questions give wrong-answer and right-answer feedback, and every chapter shows a chart. In the survival chapter at month 11.5, a target share of 0.99 and a marker effect of 2.5, the population hazard ratio is drawn above the HR = 1 line, and the text says A's survival curve stays above B's.
  - **Themes.** Both themes render, and the choice is saved.
  - **R in the browser.** Three of the six R cells ran. The integration cell printed `0.2704838`, and one Run of the prior cell printed both priors. After three return visits to the workflow chapter, one Run echoed its code once. `naive()` and `stc()` printed through the package's own methods, and a code draft survived a chapter change.
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
- **webR restart.** Stop and restart R was not exercised in a real browser.
- **Scope of the fits.** The browser fit covers binary outcomes only, with a teaching budget of draws. RStan fitting, prior refits, repeated simulation and clinical validation were not run.
