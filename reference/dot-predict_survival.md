# Survival predictions (internal dispatch for [`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md))

Reads the population-standardized survival generated quantities and
returns a tidy summary by treatment, population, and (for curves) time.

## Usage

``` r
.predict_survival(object, population, type, summary, probs, times = NULL)
```
