# Restriction time behind an RMST prediction, if it carries one

`predict(type = "rmst")` returns a `horizon` column (and survival fits
also record an `rmst_horizon` attribute). An RMST value is only defined
together with that time, so the plotting method has to be able to
recover it. Predictions integrated to different horizons are different
estimands and are refused rather than drawn on one axis.

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
