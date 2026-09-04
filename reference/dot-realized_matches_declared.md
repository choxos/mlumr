# Does the realized integration design reproduce the declared one?

Projects the realized centered profiles onto the DECLARED design's
principal directions and asks whether each one still carries its share
of the spread the declared design claimed there. Each covariate is first
scaled by the spread the declared design claims for it, so the
comparison is unit-free.

## Usage

``` r
.realized_matches_declared(
  declared,
  realized,
  ref_sd = NULL,
  factor = 0.5,
  min_spread = 0.05
)
```

## Arguments

- declared:

  Matrix of declared mean profiles (rows are AgD rows).

- realized:

  Matrix of realized integration means, or `NULL`.

- ref_sd:

  Reference SD per covariate (the IPD SDs), to put both designs on the
  scale the identification screens use. Non-finite or non-positive
  entries fall back to 1.

- factor:

  Smallest share of each declared singular value the realized design
  must still provide along that same direction.

- min_spread:

  Absolute floor, in IPD standard deviations, matching
  [`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md)
  and
  [`.profile_rank()`](https://choxos.github.io/mlumr/reference/dot-profile_rank.md).

## Value

`TRUE` when the realized design reproduces the declared one, or when
there is nothing to compare against.

## Details

This replaces a comparison of ranks. A rank drop is the extreme case of
a collapsed direction, so this test subsumes it, and it also catches the
case the rank test could not see: declared means `c(-1, 1)` and realized
means `c(-1e-10, 1e-10)` both have rank 2, yet the likelihood has almost
no leverage along that direction and the reported geometry described a
design that was not fitted. The `factor` is far above quadrature noise,
which moves a singular value by a relative `O(1 / n_int)`.

Comparing the two singular-value SPECTRA is not enough, because singular
values arrive sorted and carry no direction. Declared spread in
covariate 1 and realized spread of the same size in covariate 2 produce
identical spectra, so a spectrum test would report a match while the
likelihood sees a different covariate entirely. Projecting onto the
declared directions is what makes the comparison directional.

The comparison uses the same absolute scale as
[`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md)
and
[`.profile_rank()`](https://choxos.github.io/mlumr/reference/dot-profile_rank.md),
not only a relative one. A purely relative test disagrees with them in a
window: declared means `c(-0.08, 0.08)` against realized
`c(-0.04, 0.04)` retain exactly half their spread, so a relative test
passes and the declared profiles are reported, while the grid the
likelihood actually integrates over sits at 0.04 IPD SDs, below the
`0.05` floor those two screens use, and is the unidentified design they
exist to catch. A direction must therefore keep BOTH its share of the
declared spread and its standing above the floor.

Declared directions already below the floor are skipped: they carry no
separation the likelihood can use either way, so requiring the grid to
reproduce them would flag noise.

The projection is measured two ways, because neither alone suffices and
each covers the other's blind spot.

Per-axis LENGTHS are directional: they pair the k-th declared direction
with the realized energy on that same direction, so they catch a grid
that keeps its total spread but relocates it onto a different declared
axis. They are not a rank. A grid collapsed onto a diagonal of two
declared axes still has a long component on each of them separately, so
lengths alone accept a grid spanning one direction where the declared
design spans two; two
[`distr()`](https://choxos.github.io/mlumr/reference/distr.md) calls
keyed off the same margin do exactly that, and in a jointly defined
subgroup table it is one copy-and-paste away.

SINGULAR VALUES of the projection count the directions actually spanned,
which is what
[`.profile_rank()`](https://choxos.github.io/mlumr/reference/dot-profile_rank.md)
screens, so they close that hole. On their own they are looser than the
directional test, not stricter: they arrive sorted and carry no
direction, so the largest realized combination is judged against the
largest declared direction even when its energy sits on another, and a
grid retaining 40 percent of the dominant direction passes on surplus it
carries elsewhere.

Requiring both means a direction must keep its own share of the declared
spread AND remain a direction the grid genuinely spans.
