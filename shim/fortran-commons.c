/*
 * Storage for Fortran COMMON blocks that two WebAssembly package builds
 * import instead of defining: deSolve and muhaz, which exist only on
 * r-universe, built with a newer flang than webR 0.6.0's own repository.
 * webR's loader cannot resolve those imports, so the packages (and flexsurv,
 * which imports both) fail to load. The Playground loads this side module
 * globally at startup, so the imports resolve here.
 *
 * The blocks hold no initial values in the Fortran sources (initialized
 * blocks are defined inside the packages), so zeroed storage is what Fortran
 * expects. Each block gets 32 KB, well above the size of any of them (the
 * largest, ODEPACK's DLS001, is under 2 KB).
 *
 * The built side module is committed beside this file. Rebuild with
 *   emcc -O2 -sSIDE_MODULE=1 fortran-commons.c -o fortran-commons.so
 * (emscripten 6.0.9, the version build-models.sh installs).
 */
#define BLOCK(name) __attribute__((aligned(16), used)) double name[4096];

/* deSolve: ODEPACK and VODE solver state */
BLOCK(dls001_)
BLOCK(dlsa01_)
BLOCK(dlsr01_)
BLOCK(dlss01_)
BLOCK(dlpk01_)
BLOCK(dvod01_)
BLOCK(dvod02_)
BLOCK(zvod01_)
BLOCK(zvod02_)
BLOCK(linal_)
BLOCK(conra5_)

/* muhaz: pilot bandwidth settings */
BLOCK(hazpil_)
