# Caption naming the RMST restriction time, when the panel shows one

An RMST value is defined only together with its restriction time, so a
plot that shows one has to name it: two forests drawn to different
horizons look comparable and are not.

## Usage

``` r
.rmst_caption(x, effects)
```

## Arguments

- x:

  The `mlumr_marginal_effects` object (carries an `rmst_horizon`
  attribute for survival fits).

- effects:

  The effect labels on the panel.

## Value

A caption string, or `NULL` when no RMST measure is shown.
