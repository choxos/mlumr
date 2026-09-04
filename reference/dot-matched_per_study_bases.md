# Per-study M-spline bases of matching dimension

Builds one basis per baseline stratum from that study's OWN observed
times, which is what keeps a stratified flexible baseline identified: a
basis function with no support over a study's observed period leaves
that study's spline scale free to trade off against its intercept, an
exact likelihood ridge (see
`tests/testthat/test-mspline-identification.R`).

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

## Details

The Stan models carry one simplex dimension shared across strata, so the
two bases must agree in size. Tied event times, which are the norm in
pseudo-IPD reconstructed from a digitized Kaplan-Meier curve, can
collapse duplicated quantile knots in one study and not the other.
Falling back to a single pooled basis when that happens would route
valid user data straight back into the nonidentified configuration the
per-study knots exist to prevent, so instead the realized internal-knot
count is reduced until BOTH studies agree, and if no workable count
exists the fit stops rather than silently returning a ridge.
