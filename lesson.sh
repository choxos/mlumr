#!/usr/bin/env bash
set -euo pipefail
lesson_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
tangible_revision=6a07bbcd5dbc548aa809b253aed503fc0a8c3251
tangible_dir="${TANGIBLE_DIR:-$lesson_dir/.cache/tangible}"
# The mlumr checkout and commit whose R sources the in-browser R cell loads.
mlumr_dir="${MLUMR_DIR:-$lesson_dir/../mlumr}"
mlumr_ref="${MLUMR_REF:-4cfd3660f56e22668ae357bde3df4b30cacb23a5}"
command="${1:-serve}"
case "$command" in
  help|--help|-h)
    echo 'Usage: ./lesson.sh [setup|check|test|build|dist|serve|scene|ref]'
    echo 'build creates local spoken narration and a static site in build/site.'
    echo 'dist runs build and copies the site to dist/, which the lesson branch deploys.'
    echo 'serve opens an existing build. Requires Node >=22, pnpm, npm, Git; build also requires FFmpeg.'
    echo 'TANGIBLE_DIR can point to an existing checkout of the pinned Tangible revision.'
    echo 'MLUMR_DIR and MLUMR_REF choose the mlumr checkout and commit for the browser R code.'
    exit 0 ;;
  setup|check|test|build|dist|serve|scene|ref) ;;
  *) echo "Unknown command: $command" >&2; exit 2 ;;
esac
if [[ ! -f "$tangible_dir/package.json" ]]; then
  mkdir -p "$tangible_dir"
  git -C "$tangible_dir" init --quiet
  git -C "$tangible_dir" remote add origin https://github.com/scienceetonnante/tangible.git
  git -C "$tangible_dir" fetch --depth 1 origin "$tangible_revision"
  git -C "$tangible_dir" checkout --detach --quiet FETCH_HEAD
fi
if [[ "$(git -C "$tangible_dir" rev-parse HEAD)" != "$tangible_revision" ]]; then
  echo "TANGIBLE_DIR must use revision $tangible_revision" >&2
  exit 1
fi
if [[ ! -f "$tangible_dir/packages/cli/dist/index.js" ]]; then
  (cd "$tangible_dir" && pnpm install --frozen-lockfile && pnpm build)
fi
mkdir -p "$lesson_dir/node_modules/@tangible"
ln -sfn "$tangible_dir/packages/core" "$lesson_dir/node_modules/@tangible/core"
ln -sfn "$tangible_dir/packages/player" "$lesson_dir/node_modules/@tangible/player"
ln -sfn "$tangible_dir/node_modules/vitest" "$lesson_dir/node_modules/vitest"

build_site() {
  (cd "$tangible_dir" && pnpm lesson check --lesson "$lesson_dir")
  (cd "$tangible_dir" && pnpm lesson build --offline --bundle --lesson "$lesson_dir")
  local site="$lesson_dir/build/site"
  cp "$lesson_dir/workflow.R" "$lesson_dir/sources.html" "$site/"
  cp -R "$tangible_dir/packages/player/node_modules/katex/dist/fonts" "$site/"
  # In-browser Stan: the TinyStan worker and the precompiled binomial mlumr models.
  rm -rf "$site/stan" && mkdir -p "$site/stan"
  (cd "$lesson_dir/runtime" && npm ci --silent && ./node_modules/.bin/esbuild stan-worker.ts --bundle --format=esm --outfile="$site/stan/worker.js" --log-level=warning)
  cp -R "$lesson_dir/runtime/models/." "$site/stan/"
  # In-browser R: mlumr's R sources at a pinned commit, without the four files
  # that need rstan or cmdstanr, plus the binomial Stan programs whose data
  # blocks the JSON writer reads.
  local src
  src="$(mktemp -d)"
  git -C "$mlumr_dir" archive "$mlumr_ref" R NAMESPACE inst/stan | tar -x -C "$src"
  rm -rf "$site/r" && mkdir -p "$site/r/R" "$site/r/inst/stan"
  for file in "$src"/R/*.R; do
    case "$(basename "$file")" in
      stanmodels.R|backend_rstan.R|backend_cmdstanr.R|zzz.R) ;;
      *) cat "$file"; echo ;;
    esac
  done > "$site/r/R/mlumr.R"
  cp "$src/NAMESPACE" "$site/r/"
  cp "$src"/inst/stan/mlumr_binary_*.stan "$site/r/inst/stan/"
  cp -R "$src/inst/stan/include" "$site/r/inst/stan/"
  cp "$lesson_dir"/r/*.R "$site/r/"
  rm -rf "$src"
  node "$lesson_dir/finish-site.mjs"
  echo "Built narrated lesson at $site/index.html (mlumr R sources from $mlumr_ref)"
}

case "$command" in
  setup) echo "Tangible ready at $tangible_revision" ;;
  test)
    (cd "$tangible_dir" && pnpm exec tsc --project "$lesson_dir/tsconfig.json")
    (cd "$tangible_dir" && pnpm exec vitest run --root "$lesson_dir") ;;
  build) build_site ;;
  dist)
    build_site
    rm -rf "$lesson_dir/dist" && cp -R "$lesson_dir/build/site" "$lesson_dir/dist"
    echo "Copied the site to $lesson_dir/dist" ;;
  *) (cd "$tangible_dir" && pnpm lesson "$command" --lesson "$lesson_dir") ;;
esac
