# mlumr lesson

A narrated, interactive lesson on multilevel unanchored meta-regression (ML-UMR) with the [mlumr](https://github.com/choxos/mlumr) R package. It is published at **https://choxos.github.io/mlumr/lesson/**.

This branch holds only the lesson, the same way the `webapp` branch holds only the web app. The R package lives on `main`.

The lesson is for researchers who know basic regression and are new to population-adjusted indirect comparisons. It has 13 narrated chapters. Every chapter has an interactive chart, and several have R code cells that run in the browser. The chapter on running mlumr loads the package's own R code with webR and fits its binomial Stan models with TinyStan, all on the page. There are also five knowledge checks, captions, light and dark themes, and a companion R script.

## Layout

| Path | What it is |
| --- | --- |
| `script.md` | The narration, with the Tangible cues that drive the scene. |
| `scenes/` | The scene: `scene.ts` (views and state), `content.ts` (text, steps, questions, code cells), `charts.ts` (SVG charts), `style.ts` (light and dark themes), `navigation.ts` (chapters and theme switch), `codecell.ts` and `runner.ts` (webR and TinyStan), `math.ts` and `math.test.ts` (the teaching models and their tests). |
| `r/` | R files for the in-browser mlumr cell: `load.R` loads the package sources, `lesson-helpers.R` builds the Stan JSON. |
| `runtime/` | The TinyStan worker and the precompiled binomial mlumr models (`mlumr_binary_spfa`, `mlumr_binary_relaxed`), copied from the `webapp` branch. |
| `workflow.R`, `package-notes.md` | The companion script and its API notes and execution record. |
| `sources.html` | Sources, scope and what runs in the browser. |
| `lesson.sh`, `finish-site.mjs` | Build wrapper and post-build fixes. |
| `browser-qa.mjs`, `QA.md` | Browser checks and their record. |
| `dist/` | The built site that GitHub Actions publishes. |

## Build

The build needs Node 22 or newer, pnpm, npm, Git and FFmpeg. The first run downloads the pinned [Tangible](https://github.com/scienceetonnante/tangible) checkout and its local Supertonic voice (about 123 MB). Later runs reuse both, and only changed sentences are spoken again.

```sh
bash lesson.sh test      # strict TypeScript and the math tests
bash lesson.sh build     # narration, captions and the site in build/site
bash lesson.sh serve     # serve build/site locally
bash lesson.sh dist      # build, then copy the site to dist/ for publishing
```

`TANGIBLE_DIR` can point to an existing Tangible checkout at revision `6a07bbcd5dbc548aa809b253aed503fc0a8c3251`. The build copies mlumr's R sources from a local checkout of the package, set with `MLUMR_DIR` (default `../mlumr`) at the commit `MLUMR_REF` (default `4cfd3660f56e22668ae357bde3df4b30cacb23a5`). Use a static HTTP server; opening `index.html` from disk does not work.

`finish-site.mjs` makes three fixes after Tangible builds the site. It rounds pause times to the player's 0.01 second clock, so a pause does not fire twice on resume. It applies the saved theme before the page paints and restyles the loading screen. It lists the R files that the browser writes into webR.

## Publish

Run `bash lesson.sh dist`, commit `dist/`, and push this branch. `.github/workflows/deploy-lesson.yaml` copies `dist/` into the `lesson/` folder of `gh-pages`. The docs deployment on `main` excludes `lesson/` from its cleanup, so it never deletes the lesson.

## Check

Serve the built lesson, then run the browser checks with Chrome:

```sh
TANGIBLE_DIR=/path/to/tangible node browser-qa.mjs http://127.0.0.1:4174
```

The script plays the narration, visits every chapter, moves every control, answers every question, switches themes, runs the R cells and a browser Stan fit, and checks four small screens. It writes screenshots and a JSON record to `qa-artifacts/`. Add `--no-runtime` to skip the webR and Stan checks when offline.

## Chapters

| Chapter | What you do | What you see |
| --- | --- | --- |
| Two trials, no common arm | Change who each trial enrolled | The crude difference and the same-population difference answer different questions. |
| What adjustment has to assume | Add a hidden difference to trial B | A trial difference can look like a treatment difference. |
| Shared or separate slopes | Change trial B's slope | Shared slopes keep the patient-level gap constant. |
| Average the predictions | Spread patients out, change the number of points | The risk of the average patient is not the average risk. |
| Rebuild the population | Change how two markers go together | Identical summaries can hide different risks. |
| Choose what to estimate | Change the target population | Patient and population odds ratios differ even with shared slopes. |
| What subgroup rows can tell you | Move the subgroup rows and the target | Identified, precise and pinned-down targets are different things. |
| Run mlumr in your browser | Step through the analysis and run it | The real package code and Stan model on made-up data. |
| Pick the outcome model | Switch outcome types | Each type needs different summaries and reports different effects. |
| Survival after averaging | Change time, mix and risk difference | Population hazard ratios change over time; RMST needs a horizon. |
| When the prior matters | Tighten the prior, move the target | A narrow interval can come from assumptions instead of data. |
| Read a fit before trusting it | Open seven problems | Sampling, integration, identification and transport are separate checks. |
| Report it well | Answer five questions | What a defensible report contains. |

## Scope

The charts are small, made-up teaching models with exact calculations, and they are labeled as such. The browser fit covers binary outcomes with a small sampling budget. [sources.html](sources.html) lists the sources, the pinned versions and what the method cannot do. Some function names resemble `multinma`, but the models are not interchangeable.
