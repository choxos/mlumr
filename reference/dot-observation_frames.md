# The observations a fit's pointwise likelihood is over

Every comparison here is paired: `loo_compare()` differences pointwise
values column by column, and DIC ranks totals over one data set. An
equal number of columns is all `loo` itself can check, and it is not
enough: two fits of different data with the same number of rows, or of
the same data in a different row order, produce a matrix of the right
shape and a comparison that means nothing. The fit carries the data it
was built from, so the identity can be checked rather than assumed.

## Usage

``` r
.observation_frames(fit)
```

## Arguments

- fit:

  An `mlumr_fit`.

## Value

A list with elements `ipd`, `agd` and `pseudo` (the last `NULL` outside
survival), or `NULL` when the fit carries no data.

## Details

The frames returned hold, in stored row order, the internal columns that
define an observation (`.study`, `.trt`, the outcome, exposure, and for
survival the times and status; named explicitly, since only the internal
names are reserved and a covariate may itself begin with a dot) together
with the covariates the fit used. For survival comparators the aggregate
rows carry the covariate summaries and the reconstructed
pseudo-individuals carry the times and status, and the pointwise units
are the latter; both frames are kept, since a change to either changes
the likelihood.

The values define an observation, not their representation. A factor and
the character vector it codes, with or without unused levels, or an
integer count and the double that was read from a file, describe the
same observations, so each column is reduced to its plain values. Study,
treatment and arm are labels: a study numbered 1 is the same study
whether the number was stored as a factor, an integer or a double, so
those are compared as the strings that name them. Counts are accepted
within rounding tolerance and rounded before Stan sees them, and are
rounded the same way here.
