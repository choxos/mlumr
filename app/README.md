# mlumr Playground

Local-only browser prototype for running `mlumr` models with TinyStan
WebAssembly. This folder is intentionally ignored through the parent repo's
`.git/info/exclude`, not the tracked `.gitignore`.

The app is browser-first. It does not call system R at runtime. The optional
local-R bridge is deliberately deferred because a static web page cannot execute
local programs without a localhost helper or desktop wrapper.

## First-Time Setup

```bash
npm install
npm run prepare:stan
```

To create WASM assets, run a Stan Playground compilation server locally:

```bash
docker run -p 8083:8080 -it ghcr.io/flatironinstitute/stan-wasm-server:latest
```

Then, in this folder:

```bash
STAN_WASM_SERVER_URL=http://localhost:8083 npm run prepare:wasm
npm run check:assets
npm run dev
```

The browser app expects model assets at:

```text
src/wasm-assets/<model_name>/main.js
src/wasm-assets/<model_name>/main.wasm
```

## Supported v1 Surface

- Families: `binomial`, `normal`, `poisson`, `survival`
- Models: `spfa`, `relaxed`, and an in-browser SPFA versus relaxed comparison run
- Runtime backend: precompiled browser WASM
- Covariate distributions for integration: normal and Bernoulli
- Survival distributions: parametric PH/AFT variants plus M-spline and piecewise exponential flexible baselines

STC/naive analyses, prior sensitivity, arbitrary Stan compilation, and local
system-R execution are out of scope for this prototype.
