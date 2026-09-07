# Did two frames keep different rows of one source?

Equal digests with unequal ranks are one source that two fits filtered
differently, which is what happens when the models use covariates that
are missing on different rows. That is a mismatch under every ordering
semantics: it is not a reordering, so allowing one does not allow it.
Every path through
[`.same_observations()`](https://choxos.github.io/mlumr/reference/dot-same_observations.md)
therefore asks this, and the paths that permit a reordering ask only
this.

## Usage

``` r
.kept_different_rows_of_one_source(kx, ky)
```

## Arguments

- kx, ky:

  Two `.source_key` columns.

## Value

`TRUE` when the keys show one source filtered two ways.

## Details

Absent keys and different digests are left alone here. They say the rows
could not be placed, which the ordered path reports as `NA` and the
paths that do not depend on order have no reason to raise.
