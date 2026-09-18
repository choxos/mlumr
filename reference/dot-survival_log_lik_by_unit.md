# Pointwise log-likelihood for LOO/WAIC, optionally grouped for survival

`"observation"` is
[`extract_log_lik()`](https://choxos.github.io/mlumr/reference/extract_log_lik.md).
For survival fits `"arm"` sums the comparator pseudo-IPD columns within
each arm and `"aggregate"` sums them all, so leaving out a unit leaves
out that arm or all external evidence. The index IPD stays per
individual.

## Usage

``` r
.survival_log_lik_by_unit(object, survival_unit = "observation")
```
