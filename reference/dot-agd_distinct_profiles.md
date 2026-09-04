# Number of distinct aggregate likelihood profiles

Mean-profile rank is the right count only where the mean profile is the
design, which is the identity link. Under any other link the integrated
response depends on a row's whole covariate distribution, so two rows
with equal means but different spreads do contribute different
constraints, and collapsing them on their means would understate the
evidence.

## Usage

``` r
.agd_distinct_profiles(data)
```

## Arguments

- data:

  An `mlumr_data` object.

## Value

Integer count of distinct integration grids, or the row count when there
are no integration points to compare.

## Details

Two rows built from an identical integration grid are a different
matter: they are the identical function of the comparator parameters
whatever the link, so the second repeats the first's likelihood term and
adds no constraint. Counting distinct grids is therefore a valid upper
bound where the raw row count is not, and it is what makes a duplicated
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) row
stop suppressing the warning for the nonlinear families too.
