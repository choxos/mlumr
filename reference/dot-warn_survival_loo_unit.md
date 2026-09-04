# Warn that survival LOO/WAIC pointwise units are reconstructed pseudo-IPD

For survival fits `log_lik_agd` is per reconstructed pseudo-individual,
so LOO/WAIC operate at that level rather than per aggregate row or per
trial. Emitted once per session (suppress with
`options(mlumr.quiet_survival_loo = TRUE)`).

## Usage

``` r
.warn_survival_loo_unit(object)
```
