## R CMD check results

Local `R CMD check --as-cran --no-manual mlumr_0.1.0.tar.gz` on macOS
(aarch64-apple-darwin20, R 4.5) reports:

* 0 errors
* 2 warnings (both environmental; see "Environmental notes" below)
* 1 note (expected: new submission + cmdstanr declared via
  `Additional_repositories`)

No tests fail and no examples error.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5
* planned before release: win-builder (devel and release), R-hub
  (Fedora, Windows Server, Ubuntu Linux)

## Environmental notes (macOS-only warnings)

Two warnings surface only on the submitter's macOS / Apple clang
toolchain and are not expected to appear on CRAN's build servers:

* **Stale Homebrew `ld` search path**:
  `ld: warning: search path '/opt/homebrew/Cellar/r/4.5.1/lib/R/lib' not found`.
  The submitter previously had a Homebrew-installed R; the path is now
  stale. The build still completes successfully.
* **`checkbashisms` script not installed locally**: the macOS toolchain
  does not ship the Debian `checkbashisms` helper, so R CMD check
  reports that "a complete check needs the 'checkbashisms' script."
  CRAN's Linux servers run this check normally.

## Notes to CRAN reviewers

* This is a new submission.
* The package embeds six Stan models (binomial / normal / poisson,
  each with SPFA and relaxed variants). They are compiled at install
  time via the standard rstantools pipeline; expect several minutes of
  C++ compilation.
* `cmdstanr` is listed in `Suggests` with `Additional_repositories:
  https://stan-dev.r-universe.dev`. This follows the same approach
  adopted by brms and other Stan-ecosystem packages: cmdstanr is an
  optional backend, and rstan is the default, CRAN-available engine.
  Examples, tests, and vignettes do not require cmdstanr; cmdstanr
  comparison code is skipped unless cmdstanr and CmdStan are available.
* Developer-only directories and build artifacts are excluded via
  `.Rbuildignore`; the current source tarball weighs about 380 KB.
* The installed package is approximately 6.9 MB, dominated by the six
  Stan-generated shared libraries (5.4 MB in `libs/`). This is in line
  with other Stan-backed CRAN packages (e.g. `rstanarm`, `brms`) and is
  the unavoidable consequence of shipping pre-compiled Stan models.
