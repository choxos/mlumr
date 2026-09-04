# Spline coefficient draws for one treatment's baseline

`scoef` is a `[n_scoef, n_strata]` matrix in Stan, so the draws are
named `scoef[j,s]`. Stratum 1 is the index study and stratum `n_strata`
is the comparator, which coincide when the baseline is shared. Older
fits stored a plain vector named `scoef[j]`; those are still readable.

## Usage

``` r
.surv_scoef_draws(object, treatment = c("index", "comparator"))
```
