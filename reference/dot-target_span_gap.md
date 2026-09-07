# Distance from the target to the practically spanned part of a fitted design

[`.target_in_span()`](https://choxos.github.io/mlumr/reference/dot-target_in_span.md)
is exact, and an exact answer is the wrong yardstick for judging how
well an estimand is supported by the rows the likelihood actually
integrates over. A finite grid misses its declared mean by a few
hundredths of an SD, so realized rows that were declared identical are
never quite identical: an exact test finds that the noise spans every
direction and certifies any target, which says nothing about precision.

## Usage

``` r
.target_span_gap(profiles, target, ref_sd, min_spread = 0.05)
```

## Arguments

- profiles:

  The fitted design, rows by covariates.

- target:

  The target covariate profile.

- ref_sd:

  Reference SD per covariate; non-finite or non-positive entries fall
  back to 1.

- min_spread:

  Smallest RMS separation, in IPD SDs, that counts as a spanned
  direction.

## Value

A non-negative number, or `NA_real_` when the inputs cannot be compared.

## Details

This asks the practical question the rest of the screen asks. Directions
along which the rows move less than `min_spread` IPD SDs, the floor
[`.profile_rank()`](https://choxos.github.io/mlumr/reference/dot-profile_rank.md)
counts by, are not treated as spanned; the target's deviation from the
rows' center is projected onto the directions that remain, and the
length of the residual is returned, in IPD SDs. It is not an
identification verdict, and it is not folded into one: a target 0.09 SD
from a single row is 0.09 SD off that row's span, and the estimand
contains that much of a coefficient the row cannot pin down however
small the number is. The verdict stays with the exact test; this says
how far.
