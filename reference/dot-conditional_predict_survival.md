# Conditional survival predictions S(t \| x) at covariate profiles

Internal dispatch for
[`conditional_predict()`](https://choxos.github.io/mlumr/reference/conditional_predict.md)
on survival fits. Returns the conditional survival probability for each
treatment at each profile and fitted prediction time. Conditional hazard
and RMST are well-defined, but this helper currently returns survival
only; use
[`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
for population-standardized hazard and RMST summaries.

## Usage

``` r
.conditional_predict_survival(object, newdata, summary, probs)
```
