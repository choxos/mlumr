# Target-standardized location contrast for an AFT fit

The AFT linear predictor is a log-time location, so the contrast
standardized to a target population is the difference of the ARITHMETIC
mean linear predictors over its rows,
`mean(eta_index) - mean(eta_comparator)`. Its exponential is a ratio of
geometric-mean survival times. This is the log-scale counterpart of
[`.target_loghr_origin()`](https://choxos.github.io/mlumr/reference/dot-target_loghr_origin.md),
which averages on the hazard scale and therefore uses log-sum-exp; here
the `1/M` does not cancel and is applied.

## Usage

``` r
.target_delta_eta(object, newdata)
```

## Details

With shared coefficients the covariate term drops out draw by draw, so
the result equals the built-in `delta_eta` for every target.
