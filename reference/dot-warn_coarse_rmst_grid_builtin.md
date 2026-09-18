# The same check for the fit's own populations

The `rmst_*` draws are integrated in Stan on the same grid by the same
trapezoid rule, so the standardized curve is evaluated on that grid for
the requested populations over the rows Stan averaged, and judged per
row. A deterministic subset of at most 200 draws and 60 rows keeps the
check cheap; it estimates the share rather than taking a census.

## Usage

``` r
.warn_coarse_rmst_grid_builtin(object, pops = c("index", "comparator"))
```

## Arguments

- object:

  A survival `mlumr_fit`.

- pops:

  The populations whose RMST is being returned; only their curves are
  judged.

## Value

`NULL`, invisibly; called for the warning.
