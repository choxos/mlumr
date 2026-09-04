# Note that some median-survival draws never reach S = 0.5

Emitted from
[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
(survival, `type = "median"`) when a positive fraction of posterior
draws have an unreached median. Suppress with
`options(mlumr.quiet_median_not_reached = TRUE)`.

## Usage

``` r
.median_not_reached_note(max_p)
```
