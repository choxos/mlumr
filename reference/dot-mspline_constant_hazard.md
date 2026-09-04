# RW1 anchor for the log-ratio spline coefficients (centers on a flat baseline)

Returns the inverse-softmax (length `n_scoef - 1`) of the M-spline
coefficient vector that produces a **constant baseline hazard**, so the
RW1 smoothing prior is centered on a flat baseline even when the knots
are unevenly spaced. Combined in Stan as
`softmax(append_row(0, lscoef_prior_mean))`, this recovers the
constant-hazard simplex exactly. Uses the knot-spacing construction of
Jackson (arXiv:2306.03957); reimplemented from `multinma` (GPL-3,
`multinma:::mspline_constant_hazard`).

## Usage

``` r
.mspline_constant_hazard(spec)
```
