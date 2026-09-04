# Knot-spacing-aware RW1 step weights for the spline coefficients

Returns `sqrt` of the normalized knot gaps (length `n_scoef - 1`) so the
RW1 increments are scaled by interval width under unevenly spaced knots.
Reimplemented from `multinma` (GPL-3, `multinma:::rw1_prior_weights`).

## Usage

``` r
.rw1_prior_weights(spec)
```
