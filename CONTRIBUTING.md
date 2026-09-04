# Contributing to mlumr

Thank you for considering a contribution to `mlumr`. This package
implements Bayesian multilevel unanchored meta-regression for indirect
treatment comparisons, so changes need both software-quality and
statistical-methodology review.

## Before You Start

- Use GitHub issues for bug reports, feature requests, documentation
  gaps, and methodological questions.
- For larger changes, open an issue first so the design can be discussed
  before implementation.
- Keep pull requests focused. A statistical-method change, documentation
  update, and refactor should usually be separate pull requests.

## Development Setup

Install package dependencies from the repository root:

``` r

install.packages(c("remotes", "testthat", "devtools", "roxygen2"))
remotes::install_deps(dependencies = TRUE)
```

`mlumr` uses Stan through `rstan` and optionally `cmdstanr`, so you need
a working C++ toolchain:

- macOS: Xcode Command Line Tools.
- Windows: Rtools.
- Linux: `g++` and `make`.

## Branches and Pull Requests

`main` is the only integration branch. Everything reaches it through a
pull request, including work by the maintainer, because the pull request
is what runs the checks, produces a reviewable diff, and records why a
change was made.

- **One branch per logical change, never one branch per release.** A
  release is a milestone, a `NEWS.md` heading, and a tag; it is not a
  unit of review. Branch names should describe the change
  (`survival-rmst-predictions`, `interval-censoring-validation`). A
  prefix such as `feature/` or `fix/` is optional.
- Bundling a whole version into one branch costs reviewable diffs,
  per-change CI evidence, targeted reverts and cherry-picks, useful
  `git bisect` resolution, and informative blame. Keep changes
  separable.
- Commit as often as is useful while working. Pull requests are
  squash-merged, so each commit on `main` is one complete, tested,
  revertible change.
- Write commit subjects in the imperative (“Add M-spline survival
  baselines”). Conventional Commit prefixes are not used here.
- `git commit --amend` and `git push --force-with-lease` are acceptable
  only on an unmerged branch that nobody else has pulled. Never rewrite
  `main`, and never rewrite a commit that has been submitted to CRAN.
- Keep implementation, its tests, the roxygen source, the regenerated
  documentation, and the `NEWS.md` bullet together in the same pull
  request.

Between releases `main` carries a development version (`0.1.0.9000`), so
add `NEWS.md` entries as the work lands rather than reconstructing them
later.

## Releases

Release branches use `release/vX.Y.Z` and exist only for final
preparation, after the changes they cover are already merged into
`main`. They should live for hours, not weeks, and contain only:

- the version bump in `DESCRIPTION`,
- the finalized `NEWS.md` section,
- a refreshed `cran-comments.md`,
- regenerated artifacts: roxygen documentation, `src/stanExports_*`, the
  precompiled vignettes, `CITATION.cff`, and `codemeta.json`.

The release branch is merged into `main` by pull request once the full
check matrix is green. The version tag `vX.Y.Z` and the GitHub release
are created from the merged commit only after CRAN accepts it; while a
submission is pending, that commit is immutable. If CRAN asks for
changes, increment to the next version rather than reusing the submitted
one, and note the resubmission in `cran-comments.md`.

## Code Style

- Follow the style already used in the surrounding R files.
- Prefer clear validation and explicit errors over silent coercion.
- Keep public APIs aligned with the `multinma`-inspired workflow where
  possible.
- Do not add new package dependencies unless they are necessary and
  justified.
- For generated documentation, update roxygen comments and regenerate
  the `.Rd` files together.

## Testing

Run focused tests while developing:

``` r

devtools::test()
```

For release-impacting changes, run:

``` sh
R CMD build .
R CMD check --as-cran mlumr_*.tar.gz
```

When a change touches Stan models, survival handling, posterior
summaries, or integration behavior, add or update tests that verify the
statistical output or the relevant edge case.

## Documentation

Update user-facing documentation when behavior changes:

- roxygen examples and parameter descriptions,
- README examples,
- vignettes,
- `NEWS.md`.

For methodological changes, document the statistical assumption being
changed and any implications for existing analyses.

## Pull Request Checklist

Before opening a pull request, confirm:

- Tests relevant to the change pass.
- Documentation is updated.
- `NEWS.md` has an entry for user-facing changes.
- No generated build artifacts, local caches, `.Rcheck` directories, or
  tarballs are committed.
- The pull request description explains both the software change and,
  when relevant, the statistical-methodology impact.

## Code of Conduct

All contributors are expected to follow the project [Code of
Conduct](https://choxos.github.io/mlumr/CODE_OF_CONDUCT.md).
