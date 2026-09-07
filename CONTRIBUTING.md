# Contributing to mlumr

Thank you for considering a contribution to `mlumr`. This package implements
Bayesian multilevel unanchored meta-regression for indirect treatment
comparisons, so changes need both software-quality and statistical-methodology
review.

## Before You Start

- Use GitHub issues for bug reports, feature requests, documentation gaps, and
  methodological questions.
- For larger changes, open an issue first so the design can be discussed before
  implementation.
- Keep pull requests focused. A statistical-method change, documentation update,
  and refactor should usually be separate pull requests.

## Development Setup

Install package dependencies from the repository root:

```r
install.packages(c("remotes", "testthat", "devtools", "roxygen2"))
remotes::install_deps(dependencies = TRUE)
```

`mlumr` uses Stan through `rstan` and optionally `cmdstanr`, so you need a
working C++ toolchain:

- macOS: Xcode Command Line Tools.
- Windows: Rtools.
- Linux: `g++` and `make`.

## Branches and Pull Requests

Everything reaches an integration branch through a pull request, including work
by the maintainer, because the pull request is what runs the checks, produces a
reviewable diff, and records why a change was made.

`main` holds the released state, with two exceptions: from a CRAN submission
until a version is accepted it holds the most recent commit submitted to CRAN
and nothing else merges into it, and between releases it may hold the
development version. Work for a version that has not been released yet
integrates on `pre-release/vX.Y.Z` instead, and that branch is merged into
`main` as one reviewed step when the version is ready to submit. A single
change therefore has one pull request, targeting whichever of the two is the
current integration branch; it is not opened twice.

The pre-release branch is also merged into `main` between releases, to keep the
two from drifting while a version is still being built. Such a merge bumps no
version of its own, and is neither tagged nor submitted; what it does change is
which version `main` carries, from the released one to the development version
already set on the pre-release branch, and its `DESCRIPTION` says so. It is the
merge made when the version is ready to submit, and only that one, that becomes
the CRAN commit.

- **One branch per logical change, never one change branch per release.** A
  release is a milestone, a `NEWS.md` heading, and a tag; it is not a unit of
  review. The pre-release integration branch is not what this rule is about: it
  collects changes that were each reviewed on their own branch and pull request.
  Branch names should describe the change (`survival-rmst-predictions`,
  `interval-censoring-validation`). A prefix such as `feature/` or `fix/` is
  optional.
- Bundling a whole version into one branch costs reviewable diffs, per-change
  CI evidence, targeted reverts and cherry-picks, useful `git bisect`
  resolution, and informative blame. Keep changes separable.
- Commit as often as is useful while working. Pull requests are squash-merged,
  so each commit on the integration branch is one complete, tested, revertible
  change. The one exception is the pull request that merges a pre-release
  branch into `main`; see Releases.
- Write commit subjects in the imperative ("Add M-spline survival baselines").
  Conventional Commit prefixes are not used here.
- `git commit --amend` and `git push --force-with-lease` are acceptable only on
  an unmerged branch that nobody else has pulled. Never rewrite `main`, and
  never rewrite a commit that has been submitted to CRAN.
- Keep implementation, its tests, the roxygen source, the regenerated
  documentation, and the `NEWS.md` bullet together in the same pull request.

Between releases the integration branch carries a development version
(`0.1.0.9000`), so add `NEWS.md` entries as the work lands rather than
reconstructing them later. The development version is set in the first commit
on a new pre-release branch, immediately after the release it follows, so no
development commit carries a released version number. It is not bumped as work
accumulates; the release version is set once, during release preparation.

## Releases

A version under development integrates on `pre-release/vX.Y.Z`, which collects
the pull requests for that version. `main` holds the released state until that
branch is first synchronized into it, and the development version after that.
It carries `vX.Y.Z` itself only once the branch is merged for a submission,
which is the merge described below. Final preparation happens on the
pre-release branch, and it contains, beyond the changes themselves:

- the version bump in `DESCRIPTION`,
- the finalized `NEWS.md` section,
- a refreshed `cran-comments.md`,
- regenerated artifacts: roxygen documentation, `src/stanExports_*`, the
  precompiled vignettes, `CITATION.cff`, and `codemeta.json`.

The pre-release branch is merged into `main` by pull request once the full
check matrix is green. That pull request is merged with a merge commit, not
squashed: squashing would collapse every change on the pre-release branch into
one commit on `main` and discard exactly the per-change history the branch was
kept to preserve. A merge made because the version is ready to submit is the
commit that is submitted to CRAN. A synchronizing merge between releases is
neither submitted nor tagged. The version tag `vX.Y.Z` and the GitHub release
are created from the submitted commit only after CRAN accepts it. While a
submission is pending, the submitted commit is immutable and nothing else is
merged into `main`, a synchronizing merge included, so `main` is either the
released state, the development version between releases, or the most recent
commit submitted to CRAN, never a mixture of a submitted version and later
work. If CRAN asks for changes, increment to the next version rather than
reusing the submitted one, merge the resubmission the same way, and note it in
`cran-comments.md`.

## Code Style

- Follow the style already used in the surrounding R files.
- Prefer clear validation and explicit errors over silent coercion.
- Keep public APIs aligned with the `multinma`-inspired workflow where possible.
- Do not add new package dependencies unless they are necessary and justified.
- For generated documentation, update roxygen comments and regenerate the `.Rd`
  files together.

## Testing

Run focused tests while developing:

```r
devtools::test()
```

For release-impacting changes, run:

```sh
R CMD build .
R CMD check --as-cran mlumr_*.tar.gz
```

When a change touches Stan models, survival handling, posterior summaries, or
integration behavior, add or update tests that verify the statistical output or
the relevant edge case.

## Documentation

Update user-facing documentation when behavior changes:

- roxygen examples and parameter descriptions,
- README examples,
- vignettes,
- `NEWS.md`.

For methodological changes, document the statistical assumption being changed
and any implications for existing analyses.

## Pull Request Checklist

Before opening a pull request, confirm:

- Tests relevant to the change pass.
- Documentation is updated.
- `NEWS.md` has an entry for user-facing changes.
- No generated build artifacts, local caches, `.Rcheck` directories, or tarballs
  are committed.
- The pull request description explains both the software change and, when
  relevant, the statistical-methodology impact.

## Code of Conduct

All contributors are expected to follow the project
[Code of Conduct](CODE_OF_CONDUCT.md).
