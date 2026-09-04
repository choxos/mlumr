# Choose M-spline knots for a flexible-baseline survival model

Place boundary and internal knots for the M-spline baseline hazard used
by the flexible survival models (`distribution = "mspline"` or
`"pexp"`). Knots are chosen from the pooled event/censoring times of the
index IPD and the reconstructed comparator pseudo-IPD.

## Usage

``` r
make_knots(data, n_knots = 7, type = c("quantile", "equal"))
```

## Arguments

- data:

  An `mlumr_data` object (survival family) from
  [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md).

- n_knots:

  Number of internal knots (default 7; capped at 50). Use a smaller
  value when events are scarce; the recommended rule of thumb is to keep
  the number of spline coefficients (`n_knots + degree + 1`) below half
  the number of events.

- type:

  Internal-knot placement: `"quantile"` (default, at quantiles of the
  pooled event times) or `"equal"` (evenly spaced between the
  boundaries).

## Value

A list with `internal` (internal knot locations), `boundary` (lower and
upper boundary knots) and `n_knots` (the realized number of internal
knots after dropping any that coincide with the boundaries).

## Details

The lower boundary knot is fixed at 0 (not the minimum delayed-entry
time), so the cumulative hazard is anchored at `H(0) = 0` and stays
continuous with the backward constant-hazard extrapolation;
delayed-entry times (\> 0) are evaluated on the basis. The upper
boundary knot is the maximum observed time across both data sources. The
M-spline basis is normalized so that the baseline cumulative hazard
equals 1 at the upper boundary; the hazard scale is carried by the model
intercepts.

## See also

[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) with
`distribution = "mspline"`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Build a survival network, then choose M-spline baseline knots:
dat <- combine_data(index_ipd, comparator_agd)
knots <- make_knots(dat, n_knots = 5)
knots$boundary  # lower (0) and upper boundary knots
knots$internal  # internal knot locations
} # }
```
