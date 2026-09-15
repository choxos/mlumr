# mlumr lesson

A narrated, interactive lesson on multilevel unanchored meta-regression (ML-UMR) with the [mlumr](https://github.com/choxos/mlumr) R package. It is published at **https://choxos.github.io/mlumr/lesson/**.

This branch holds only the lesson, the same way the `webapp` branch holds only the web app. The R package lives on `main`.

The lesson is for researchers who know basic regression and are new to population-adjusted indirect comparisons. It has 13 narrated chapters. Every chapter has an interactive chart, and several have R code cells that run in the browser. The chapter on running mlumr loads the package's own R code with webR and fits its binomial Stan models with TinyStan, all on the page. There are also five knowledge checks, captions, light and dark themes, and a companion R script.

## Layout

| Path | What it is |
| --- | --- |
| `script.md` | The narration, with the Tangible cues that drive the scene. |
| `scenes/` | The scene: `scene.ts` (views and state), `content.ts` (text, steps, questions, code cells), `charts.ts` (SVG charts), `style.ts` (light and dark themes), `navigation.ts` (chapters and theme switch), `codecell.ts` and `runner.ts` (webR and TinyStan), `diagnostics.ts` (R-hat, ESS and MCSE as in the posterior R package), `math.ts` (the teaching models), and `*.test.ts` unit tests. |
| `r/` | R files for the in-browser mlumr cell: `load.R` loads the package sources and stands in for the Stan backend step, `lesson-helpers.R` prepares a fit through the public `mlumr()`, and `check-adapter.R` checks that natively. |
| `runtime/` | The TinyStan worker and the precompiled binomial mlumr models (`mlumr_binary_spfa`, `mlumr_binary_relaxed`), copied from the `webapp` branch, with `models/manifest.json` binding each binary to its Stan program. |
| `workflow.R`, `package-notes.md` | The companion script and its API notes and execution record. |
| `sources.html` | Sources, scope and what runs in the browser. |
| `lesson.sh`, `finish-site.mjs`, `verify-models.mjs`, `dist-manifest.mjs` | Build wrapper, post-build fixes, the model check and the build manifest. |
| `browser-qa.mjs`, `QA.md` | Browser checks and their record. |
| `dist/` | The built site that GitHub Actions publishes. |

## Build

The build needs Node 22.6 or newer, pnpm, npm, Git and FFmpeg. The first run downloads the pinned [Tangible](https://github.com/scienceetonnante/tangible) checkout and its local Supertonic voice (about 123 MB). Later runs reuse both, and only changed sentences are spoken again.

```sh
bash lesson.sh test      # strict TypeScript for the scene and the worker, and the unit tests
bash lesson.sh build     # narration, captions and the site in build/site
bash lesson.sh serve     # serve build/site locally
bash lesson.sh dist      # build, then copy the site to dist/ for publishing
bash lesson.sh adapter   # after a build: check the browser mlumr adapter natively with R
```

`TANGIBLE_DIR` can point to an existing Tangible checkout at revision `6a07bbcd5dbc548aa809b253aed503fc0a8c3251`. `build` and `dist` refuse a checkout with local changes to its tracked files or packages, and rebuild its compiled packages from source first. The build copies mlumr's R sources from a local checkout of the package, set with `MLUMR_DIR` (default `../mlumr`) at the commit `MLUMR_REF` (default `4cfd3660f56e22668ae357bde3df4b30cacb23a5`). Use a static HTTP server; opening `index.html` from disk does not work.

The build checks `runtime/models/manifest.json` against the Stan programs at `MLUMR_REF` and stops if they differ, and it writes `build-manifest.json` with the SHA-256 of every source and built file. `lesson.sh adapter` needs R with mlumr's dependencies; set `MLUMR_NATIVE` to an mlumr checkout at `MLUMR_REF` with a built DLL to compare the browser's Stan data, refusals and warnings with the package itself.

`finish-site.mjs` finishes the site after Tangible builds it. It applies the saved theme before the page paints and restyles the loading screen, lists the R files that the browser writes into webR, and writes `transcript.html`, the spoken text of `script.md` by chapter with each chapter's start time.

## Publish

Run `bash lesson.sh dist`, commit `dist/`, and push this branch. `.github/workflows/deploy-lesson.yaml` first checks `dist/build-manifest.json` against the commit, and refuses a `dist/` built from other sources, then copies `dist/` into the `lesson/` folder of `gh-pages`. The docs deployment on `main` excludes `lesson/` from its cleanup, so it never deletes the lesson.

## Check

Serve the built lesson, then run the browser checks with Chrome:

```sh
TANGIBLE_DIR=/path/to/tangible node browser-qa.mjs http://127.0.0.1:4174
```

The script plays the narration, visits every chapter, moves every control, answers every question, opens an explanation while its chapter is narrated and crosses the next cue, plays a chapter from its Explore view, selects and copies a code example, switches themes, runs the R cells and browser Stan fits of both models, and checks laptop, desktop, tablet, landscape phone, portrait phone and 200% zoom sizes. At every size it measures the rendered chart tick text (at least 11 CSS pixels), reads the chapter menu's accessible name from Chromium's accessibility tree, and shows the longest caption to confirm it stays below the lesson. After `lesson.sh adapter`, it also checks that the browser's Stan data equal the native ones, and it checks that a downloaded run record names the build, the package commit and the compiled model. It writes screenshots and a JSON record to `qa-artifacts/`. Add `--no-runtime` to skip the webR and Stan checks when offline.

`.github/workflows/deploy-lesson.yaml` runs the same checks on every pull request into `lesson` and before every deployment: `lesson.sh test`, `dist-manifest.mjs verify dist`, `verify-models.mjs` against the Stan programs of the pinned mlumr commit, and `browser-qa.mjs --no-runtime` on the committed `dist/` in Playwright's Chromium (`QA_BROWSER_CHANNEL=` chooses it instead of installed Chrome). The screenshots and JSON record are kept as the `browser-qa` workflow artifact. The webR and Stan runtime checks and `lesson.sh adapter` still run locally.

## Chapters

| Chapter | What you do | What you see |
| --- | --- | --- |
| Two trials, no common arm | Change who each trial enrolled | The crude difference and the same-population difference answer different questions. |
| What adjustment has to assume | Add a hidden difference to trial B | A trial difference can look like a treatment difference. |
| Shared or separate slopes | Change trial B's slope | Shared slopes keep the conditional odds ratio the same at every marker value. |
| Average the predictions | Spread patients out, change the number of points | The risk of the average patient is not the average risk. |
| Rebuild the population | Change how two markers go together | Identical summaries can hide different risks. |
| Choose what to estimate | Change the target population | Conditional and population odds ratios differ even with shared slopes. |
| What subgroup rows can tell you | Move the subgroup rows and the target | Identified, precise and pinned-down targets are different things. |
| Run mlumr in your browser | Step through the analysis and run it | The real package code and Stan model on made-up data. |
| Pick the outcome model | Switch outcome types | Each type needs different summaries and reports different effects. |
| Survival after averaging | Change time, mix and risk difference | Population hazard ratios change over time; RMST needs a horizon. |
| When the prior matters | Tighten the prior, move the target | A narrow interval can come from assumptions instead of data. |
| Read a fit before trusting it | Open seven problems | Sampling, integration, identification and transport are separate checks. |
| Report it well | Answer ten questions, read the capstone | What a defensible report contains. |

## Listening and exploring

The narration plays through the chapters on its own. Touching a control, opening an explanation, editing or running code, or opening a chapter from the menu holds that chapter on screen while the voice continues; **Return to narration** rejoins it. **Play this chapter** seeks the narration to the start of the chapter on screen and plays it. **Reset controls** restores the chapter's starting control values and touches neither the narration nor the code cell. The transcript link in the header opens the spoken text by chapter, and every native code example has a Copy button.

## Scope

The charts are small, made-up teaching models with exact calculations, and they are labeled as such. The browser fit covers binary outcomes with a small sampling budget. [sources.html](sources.html) lists the sources, the pinned versions and what the method cannot do. Some function names resemble `multinma`, but the models are not interchangeable.
