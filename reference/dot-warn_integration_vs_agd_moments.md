# Warn when the generated grid contradicts the declared AgD moments

A hand-written
[`distr()`](https://choxos.github.io/mlumr/reference/distr.md) that
ignores the AgD columns integrates the wrong population silently. Only
gross contradictions are flagged; suppress with
`options(mlumr.quiet_integration_moments = TRUE)`.

## Usage

``` r
.warn_integration_vs_agd_moments(X_int_array, agd_data, cov_names)
```
