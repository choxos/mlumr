## Submission

This is an update of mlumr from 0.1.0, which is on CRAN, to 0.2.0. `NEWS.md`
lists the changes. The largest are time-to-event outcomes (parametric and
M-spline baselines, fitted to reconstructed Kaplan-Meier data) and prediction
of marginal effects in arbitrary target populations.

## R CMD check results

Local `R CMD check --as-cran --no-manual mlumr_0.2.0.tar.gz` on macOS 27.0.1
(aarch64-apple-darwin23), R 4.6.0:

* 0 errors
* 1 warning, environmental: the `checkbashisms` script is not installed on
  this machine, so the check reports that "a complete check needs the
  'checkbashisms' script." CRAN's Linux servers have it.
* 0 notes. The CRAN incoming feasibility check reports, as information, that
  `cmdstanr` is in Suggests and is not in a mainstream repository. It is
  available from the declared
  `Additional_repositories: https://mc-stan.org/r-packages`.

Tests under `--as-cran`: `[ FAIL 0 | WARN 32 | SKIP 93 | PASS 2853 ]`. The 93
skips are the Stan-sampling tests, which are marked `skip_on_cran()` because
each one compiles or samples a model. They run in a separate CI job with
`NOT_CRAN=true` (below), where they pass.

## Test environments

* local macOS 27.0.1 (aarch64-apple-darwin23), R 4.6.0
* GitHub Actions, `R CMD check --no-manual --as-cran`, R release: macOS,
  Windows and Ubuntu. Status OK on all three.
* GitHub Actions, Ubuntu, R release, full test suite with the Stan tests
  enabled under CmdStan: 4705 passed, 0 failed, 0 skipped.

## Reverse dependencies

None. No CRAN package depends on, imports, suggests or links to mlumr.

## Notes to CRAN reviewers

* The package embeds ten Stan models: binomial, normal and Poisson, each with
  SPFA and relaxed variants, plus parametric and M-spline survival in both
  variants. They are compiled at install time through the standard rstantools
  pipeline, so installation takes several minutes of C++ compilation.
* The installed size is 14.9 Mb: `libs` 10.2 Mb (the compiled Stan models),
  `R` 2.1 Mb and `doc` 2.0 Mb. As with other Stan-backed packages such as
  `rstanarm` and `brms`, the shared library dominates. The source tarball is
  1.9 Mb.
* `cmdstanr` is an optional backend. rstan is the default engine and is on
  CRAN. Examples and tests do not require cmdstanr, and the cmdstanr
  comparison code is skipped unless cmdstanr and CmdStan are available.
* Seven of the nine vignettes fit Stan models, so they are pre-rendered and
  shipped through the `R.rsp::asis` engine; re-building them runs no Stan. The
  other two are knitr vignettes: `introduction` evaluates no code, and
  `data-preparation` runs data-setup code only, with no model fitting.
