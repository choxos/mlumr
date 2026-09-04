# Knot placement from a single set of survival times

The computational core of
[`make_knots()`](https://choxos.github.io/mlumr/reference/make_knots.md),
factored out so the same rule can be applied either to the pooled times
(one shared baseline) or to each study's own times (`aux_by = ".study"`,
one baseline per study). Per-study boundary knots are what multinma's
default `type = "quantile"` does, and they are what keeps a stratified
baseline identified: a basis function with no support over a study's
observed period leaves that study's spline scale free to trade off
against its intercept.

## Usage

``` r
.knots_from_times(
  all_times,
  event_times,
  n_knots,
  type = c("quantile", "equal")
)
```

## Arguments

- all_times:

  All observed times for the stratum (events and censorings).

- event_times:

  Event times only; falls back to `all_times` if empty.

- n_knots:

  Number of internal knots.

- type:

  `"quantile"` (event-time quantiles) or `"equal"` (equally spaced).

## Value

A list with `internal`, `boundary`, and `n_knots`.
