#!/usr/bin/env bash
# Compiles mlumr's ten Stan programs to WebAssembly with TinyStan, the way
# Stan Playground's compile server does (emscripten, a wasm build of oneTBB,
# TinyStan's own make rules), from the Stan sources committed at one mlumr
# commit. Output: models/<model>/main.{js,wasm} and models/manifest.json.
#
# Usage: bash build-models.sh [mlumr commit, default main] [model ...]
# Heavy: every model compiles all of Stan Math. Run it alone, under nice.
set -euo pipefail
cd "$(dirname "$0")"
here=$(pwd)

REPO=${MLUMR_REPO:-$(git -C "$here" rev-parse --show-toplevel)}
COMMIT=$(git -C "$REPO" rev-parse "${1:-main}")
shift || true
WORK=${WASM_WORK:-$HOME/.cache/mlumr-wasm-build}
EMSDK_VERSION=6.0.9
TBB_VERSION=2021.13.0
TINYSTAN_COMMIT=e78bb624b363b751e697c0ff2501a9d5fa4b9b49
# stanc 2.40.0, the release TinyStan pins, taken from the local CmdStan install
# so the native comparison uses the very same translator.
STANC_BIN=${STANC_BIN:-$HOME/.cmdstan/cmdstan-2.40.0/bin/stanc}
MODELS=("$@")
if [ ${#MODELS[@]} -eq 0 ]; then
  MODELS=(mlumr_binary_spfa mlumr_binary_relaxed mlumr_normal_spfa mlumr_normal_relaxed
          mlumr_poisson_spfa mlumr_poisson_relaxed mlumr_survival_spfa mlumr_survival_relaxed
          mlumr_survival_mspline_spfa mlumr_survival_mspline_relaxed)
fi
mkdir -p "$WORK"

# emscripten
if [ ! -x "$WORK/emsdk/emsdk" ]; then
  git clone -q --depth 1 https://github.com/emscripten-core/emsdk.git "$WORK/emsdk"
fi
if [ ! -f "$WORK/emsdk/.activated-$EMSDK_VERSION" ]; then
  "$WORK/emsdk/emsdk" install "$EMSDK_VERSION"
  "$WORK/emsdk/emsdk" activate "$EMSDK_VERSION"
  touch "$WORK/emsdk/.activated-$EMSDK_VERSION"
fi
# shellcheck disable=SC1091
EMSDK_QUIET=1 source "$WORK/emsdk/emsdk_env.sh"

# oneTBB for wasm, no threads
if [ ! -f "$WORK/oneTBB/install/lib/libtbb.a" ]; then
  (cd "$WORK" && curl -fsSL -o tbb.tar.gz "https://github.com/uxlfoundation/oneTBB/archive/refs/tags/v$TBB_VERSION.tar.gz" &&
     rm -rf oneTBB && tar -xzf tbb.tar.gz && mv "oneTBB-$TBB_VERSION" oneTBB)
  mkdir -p "$WORK/oneTBB/build"
  (cd "$WORK/oneTBB/build" &&
     emcmake cmake .. -DCMAKE_CXX_COMPILER=em++ -DCMAKE_C_COMPILER=emcc \
       -DCMAKE_BUILD_TYPE=MinSizeRel -DTBB_STRICT=OFF \
       -DCMAKE_CXX_FLAGS="-fwasm-exceptions -Wno-unused-command-line-argument" \
       -DTBB_DISABLE_HWLOC_AUTOMATIC_SEARCH=ON -DBUILD_SHARED_LIBS=OFF \
       -DTBB_EXAMPLES=OFF -DTBB_TEST=OFF -DEMSCRIPTEN_WITHOUT_PTHREAD=true \
       -DCMAKE_INSTALL_PREFIX="$WORK/oneTBB/install/" > ../cmake.log &&
     cmake --build . -j 2 > ../build.log && cmake --install . > ../install.log)
fi

# TinyStan, pinned to the commit Stan Playground's compile server uses
if [ ! -d "$WORK/tinystan/.git" ]; then
  git clone -q https://github.com/WardBrian/tinystan.git "$WORK/tinystan"
  (cd "$WORK/tinystan" && git checkout -q "$TINYSTAN_COMMIT" &&
     git submodule update --init --recursive --depth 1)
fi
mkdir -p "$WORK/tinystan/make"
cat > "$WORK/tinystan/make/local" <<EOF
CXX_TYPE=clang
PRECOMPILED_HEADERS=true
TBB_INTERFACE_NEW=1
TBB_INC=$WORK/oneTBB/install/include/
TBB_LIB=$WORK/oneTBB/install/lib/
LDFLAGS_TBB ?= -Wl,-L,"\$(TBB_LIB)"
LDLIBS_TBB ?= -ltbb
TINYSTAN_SERIAL=true
CXXFLAGS+=-fwasm-exceptions
LDFLAGS+=-sMODULARIZE -sFAKE_DYLIBS -sEXPORT_NAME=createModule -sEXPORT_ES6 -sENVIRONMENT=web,worker -sINCOMING_MODULE_JS_API=print,printErr
LDFLAGS+=-sEXIT_RUNTIME=1 -sALLOW_MEMORY_GROWTH=1
EXPORTS=_malloc,_free,_tinystan_api_version,_tinystan_create_model,_tinystan_destroy_error,_tinystan_destroy_model,_tinystan_get_error_message,_tinystan_get_error_type,_tinystan_model_num_free_params,_tinystan_model_param_names,_tinystan_sample,_tinystan_separator_char,_tinystan_stan_version
LDFLAGS+=-sEXPORTED_FUNCTIONS=\$(EXPORTS) -sEXPORTED_RUNTIME_METHODS=stringToUTF8,getValue,UTF8ToString,lengthBytesUTF8,HEAPF64
EOF

# The Stan sources exactly as committed (not the working tree). stanc runs in
# the TinyStan folder and finds the includes by a relative path, so no path on
# the build machine ends up in the binaries' error messages.
SRC="$WORK/stan-src-$COMMIT"
if [ ! -d "$SRC" ]; then
  mkdir -p "$SRC"
  git -C "$REPO" archive "$COMMIT" inst/stan | tar -x -C "$SRC"
fi

# A selective rebuild may keep models from an earlier commit only if their
# Stan programs are unchanged at this one; check before compiling anything.
node scripts/stan-hash.mjs kept "$COMMIT" "$SRC/inst/stan" "${MODELS[@]}"

mkdir -p models
for m in "${MODELS[@]}"; do
  out="$WORK/models-$COMMIT/$m"
  mkdir -p "$out"
  cp "$SRC/inst/stan/$m.stan" "$out/main.stan"
  echo "== $m"
  start=$(date +%s)
  (cd "$WORK/tinystan" &&
     emmake make STANC="$STANC_BIN" STANCFLAGS="--include-paths=../stan-src-$COMMIT/inst/stan --filename-in-msg=$m.stan" \
       "$out/main.js" > "$out/build.log" 2>&1 &&
     emstrip "$out/main.wasm")
  echo "   $(( $(date +%s) - start )) s"
  mkdir -p "models/$m"
  cp "$out/main.js" "$out/main.wasm" "models/$m/"
done

# The dimensions of every data variable, from stanc --info: the page writes
# the Stan data with them, so a length-one vector stays an array.
INFO="$WORK/models-$COMMIT/info"
mkdir -p "$INFO"
for m in "${MODELS[@]}"; do
  (cd "$SRC/inst/stan" && "$STANC_BIN" --info --include-paths=. "$m.stan") > "$INFO/$m.json" 2>/dev/null
done

# Manifest: which commit, which sources, which binaries, per model. A model
# kept from an earlier build stays only if its Stan program is unchanged at
# this commit (scripts/stan-hash.mjs, which build.sh also uses to check them).
node scripts/stan-hash.mjs write "$COMMIT" "$SRC/inst/stan" "$INFO" "${MODELS[@]}"
