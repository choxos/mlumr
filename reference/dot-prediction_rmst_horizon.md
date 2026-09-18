# Restriction time behind an RMST prediction, if it carries one

Predictions integrated to different horizons are different estimands and
are refused rather than drawn on one axis.

## Usage

``` r
.prediction_rmst_horizon(x, df)
```

## Arguments

- x:

  The `mlumr_prediction` object.

- df:

  Its data-frame form.

## Value

A single finite restriction time, or `NULL` when none is recorded.
