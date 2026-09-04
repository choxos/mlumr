# Numerical rank of an aggregate design, on a scale that can be judged

[`qr()`](https://rdrr.io/r/base/qr.html) calls a column negligible
relative to the norms it is handed, so an uncentered covariate sitting
on a large offset collapses: `qr(cbind(1, 1e7 + c(0, 1, 2)))$rank` is 1,
although the design the model fits, with covariates centered by default,
is plainly rank 2. Centering and scaling first asks the question about
the design being fitted.

## Usage

``` r
.profile_rank(profiles, ref_sd, min_spread = 0.05)
```

## Arguments

- profiles:

  Aggregate subgroup mean matrix, rows by covariates.

- ref_sd:

  Reference SD per covariate (the IPD SDs), used to put the profile
  separations on a scale that can be judged. Non-finite or non-positive
  entries fall back to 1.

- min_spread:

  Smallest RMS profile separation along a direction, in IPD standard
  deviations, that counts as a direction. Defaults to the value
  [`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md)
  screens on.

## Value

Integer rank, or `0` when the design cannot be decomposed.

## Details

The scale has to come from OUTSIDE the profiles. Dividing each column by
its own root-mean-square, which this did, stretches any separation back
to unit size: aggregate means of `c(-1e-10, 1e-10)` became `c(-1, 1)`
and the design was reported full rank, so the identity-link
relaxed-model screen in
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) never
fired on a comparator the likelihood cannot separate. The IPD standard
deviations are an absolute scale and are what
[`.subgroup_geometry()`](https://choxos.github.io/mlumr/reference/dot-subgroup_geometry.md)
already uses, so the two diagnostics now agree.

[`qr()`](https://rdrr.io/r/base/qr.html) cannot supply that judgment on
its own either. LINPACK's `dqrdc2` compares each column's remaining norm
against that SAME column's original norm, so a covariate separated by
`1e-11` IPD SDs is still "independent" of the intercept and counts
toward the rank. Directions are therefore counted by singular value
against an absolute floor, the `spread` that
[`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md)
already screens on, so the two agree by construction.
