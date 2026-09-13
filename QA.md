# Lesson execution record

September 12 to 13, 2026. This records the checks run on the lesson in this branch before it was published at https://choxos.github.io/mlumr/lesson/.

## Provenance

- **Package.** mlumr development version 0.1.0.9000 at `main` commit `4cfd3660f56e22668ae357bde3df4b30cacb23a5`. Every function, argument and default the lesson names was checked against that commit's NAMESPACE, help pages and R source in two independent passes. One problem found by the first pass was fixed: the draft taught `check_diagnostics()`, which is internal, so `workflow.R --fit` failed on an installed package. The lesson and the script now use `summary(fit)`.
- **Tangible.** Compiler and player at `6a07bbcd5dbc548aa809b253aed503fc0a8c3251`, unchanged. Narration is local Supertonic 3 speech.
- **Browser runtimes.** webR 0.6.0 from webr.r-wasm.org, with randtoolbox, jsonlite and detectseparation from the webR package repository. TinyStan 0.3.3 with the `mlumr_binary_spfa` and `mlumr_binary_relaxed` WebAssembly models from the `webapp` branch. The binomial Stan programs on `main` have not changed since those models were compiled.

## What changed from the first draft

- **Design.** A new light and dark theme built on the mlumr web app's tokens (IBM Plex, teal accent), with a remembered theme switch, restyled Tangible player controls, captions, notes board and loading screen. Chart series colors were checked for colorblind separation and contrast in both themes: treatment A is `#006c98` (light) and `#3398c2` (dark); treatment B is `#c8741d` (light) and `#cf7b26` (dark).
- **Charts.** Every one of the 13 chapters has at least one SVG chart that shows the mechanism being taught. The four chapters that had only text, code or a quiz now have an analysis-steps diagram, an outcome-type curve, a picture for each diagnostic problem, and a forest plot of the companion script's real fits.
- **Runnable code.** Five chapters have R cells that run in the browser. The Run mlumr chapter loads mlumr's own R code and fits its binomial Stan model on the page.
- **Narration.** The script was rewritten in plain language, keeping every scene cue. A second review against the package added plain definitions of covariate, link scale, prior, posterior draw, chain and identification. It also corrected the statements about survival data in the subgroup check, the tied event time refusals, centering, and the survival data function.

## Executed checks

All commands ran from this branch with `TANGIBLE_DIR` pointing to the pinned Tangible checkout.

```sh
bash lesson.sh test
bash lesson.sh build
node browser-qa.mjs http://127.0.0.1:7860
```

- `lesson.sh test`: strict TypeScript passed, and all 6 math tests passed. They cover response averaging and non-collapsibility, quadrature convergence, Bernoulli dependence, coefficient versus target identification, survival hazards and RMST, and split R-hat.
- `lesson.sh build`: `check: no errors`. The compiled narration lasts **1581.07 seconds** (26 minutes 21 seconds). Both `audio.m4a` and `audio.webm` decode fully with FFmpeg.
- `browser-qa.mjs` in Chrome finished with **0 browser errors and 0 failed requests**. It covered the following:
  - **Narration.** Narration plays unmuted. All 13 chapters can be browsed without pausing or seeking the voice. Seeking the timeline shows the narrated chapter. The lesson has no automatic pauses: narration stops only when the learner presses pause, and it crossed all 12 chapter boundaries without stopping while another chapter was open. Playback runs to the end and replays.
  - **Exploration.** Exploring one chapter is kept separate from what the narration changes, and "Return to narration" works by mouse, keyboard and touch.
  - **Controls.** Every slider was moved to both ends and changes the chart. Every select option was chosen. In the Run mlumr chapter, the six analysis steps move with the previous and next arrows and by clicking a step in the chart, and Reset returns to step 1. Reset keeps the playback state. The survival slider keeps the voice playing when moved by mouse, keyboard or tablet touch.
  - **Content.** All 7 diagnostic explanations open, all 5 questions give wrong-answer and right-answer feedback, and every chapter shows at least one chart.
  - **Themes.** Both themes render, the switch changes the color scheme, and the choice is saved.
  - **R in the browser.** The integration R cell ran in webR and printed `0.2704838`, matching the chart. The mlumr cell ran `set_ipd()`, `set_agd()`, `combine_data()`, `add_integration()`, `check_identification()`, `naive()` and `stc()` without errors, and printed through the package's own methods.
  - **Stan in the browser.** The browser fit of the shared-slopes model returned the table below.
  - **Screens and files.** There is no horizontal overflow at 1024 × 768, 667 × 375, 844 × 390 and 896 × 414, and every slider is at least 44 pixels tall. `workflow.R`, `sources.html`, `captions.vtt` and `tracks.json` are all served.

| Browser fit, shared slopes (2 chains, 500 + 500) | Mean | 2.5% | 97.5% | Split R-hat |
| --- | ---: | ---: | ---: | ---: |
| `lor_comparator` | -0.521 | -0.811 | -0.240 | 1.003 |
| `rd_comparator` | -0.127 | -0.196 | -0.059 | 1.003 |
| `lor_index` | -0.537 | -0.831 | -0.250 | 1.003 |
| `rd_index` | -0.119 | -0.183 | -0.054 | 1.003 |

On the same data, `stc()` in the browser gave a log odds ratio of -0.4888 in trial B's population (95% CI -0.7984 to -0.1792), which agrees with `lor_comparator`. The relaxed model also fitted in the browser, with split R-hat at most 1.002. Before building the browser cell, the same pipeline was checked outside the browser. There, the Stan JSON that webR builds matched what `mlumr()` passes to its engine, and the TinyStan posterior matched CmdStan and a real `mlumr()` fit within Monte Carlo error.

## Companion script

`Rscript workflow.R --source=<checkout at 4cfd366> --fit --engine=cmdstanr` finished with exit code 0. Both fits kept 4000 draws, with no divergences and no treedepth hits. The largest R-hat was 1.003 for shared slopes and 1.006 for separate slopes. The details are in [package-notes.md](package-notes.md).

## Limits of this record

This is technical verification, not a human listening review. Supertonic estimates word timing inside each sentence. The browser fit covers binary outcomes only, with a teaching budget of draws. RStan fitting, survival fits, prior refits, repeated simulation and clinical validation were not run.
