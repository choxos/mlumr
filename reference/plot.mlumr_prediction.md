# Plot absolute predictions from a fitted ML-UMR model

Dispatches on the prediction `type`: time-indexed types (`"survival"`,
`"hazard"`, `"cumhaz"`, `"loghr"`) are drawn as curves with credible
bands at the coverage the result was summarized with; scalar types
(`"rmst"`, `"median"`, `"response"`) as point-intervals. The mlumr
analogue of `plot(predict(multinma_fit))`.

## Usage

``` r
# S3 method for class 'mlumr_prediction'
plot(x, ref_line = NULL, ...)
```

## Arguments

- x:

  A [`predict()`](https://rdrr.io/r/stats/predict.html) result (an
  `mlumr_prediction`).

- ref_line:

  Optional numeric null-reference line(s). Drawn as horizontal line(s)
  for curve types and vertical line(s) for scalar types, mirroring the
  `plot(predict(fit), ref_line = c(0, 1))` idiom (e.g. probability
  bounds for `type = "response"`). The log-hazard-ratio curve defaults
  to `ref_line = 0`.

- ...:

  Unused.

## Value

A `ggplot` object (compose further layers, e.g. a KM overlay, with `+`).

## See also

[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md),
[`geom_km()`](https://choxos.github.io/mlumr/reference/geom_km.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plot(predict(fit, type = "survival")) + geom_km(dat)
plot(predict(fit, type = "response"), ref_line = c(0, 1))
plot(predict(fit, type = "loghr"))
} # }
```
