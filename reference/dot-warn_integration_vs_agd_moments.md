# Warn when the generated grid contradicts the declared AgD moments

[`distr()`](https://choxos.github.io/mlumr/reference/distr.md) specifies
the *shape* of the comparator covariate distribution; the
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
mean/SD summaries describe the target population. In the standard
workflow each
[`distr()`](https://choxos.github.io/mlumr/reference/distr.md)
references the AgD columns so they agree by construction, but a
hand-written distribution (e.g. `distr(qnorm, mean = 0, sd = 1)` while
the AgD declares mean 10) integrates the wrong population silently. Flag
only gross contradictions so ordinary QMC scatter never false-warns;
suppress with `options(mlumr.quiet_integration_moments = TRUE)`.

## Usage

``` r
.warn_integration_vs_agd_moments(X_int_array, agd_data, cov_names)
```
