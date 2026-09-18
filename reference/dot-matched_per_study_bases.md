# Per-study M-spline bases of matching dimension

One basis per stratum from that study's own observed times, which is
what keeps a stratified flexible baseline identified. The Stan models
share one simplex dimension across strata, so when tied event times
collapse quantile knots in one study only, the internal-knot count is
reduced until both studies agree; a pooled fallback would restore the
nonidentified configuration the per-study knots exist to prevent.

## Usage

``` r
.matched_per_study_bases(ipd, pseudo, n_knots, degree)
```

## Arguments

- ipd:

  The index study's individual data (`.time`, `.status`).

- pseudo:

  The comparator study's reconstructed pseudo-IPD.

- n_knots:

  Requested number of internal knots.

- degree:

  Spline degree (3 = cubic M-spline, 0 = piecewise exponential).

## Value

A list with `index` and `comparator` basis specs of equal `n_scoef`, and
`n_knots` (the realized count actually used).
