# Log survival S(t \| eta) in R, mirroring the Stan log_surv_scalar()

Vectorized over posterior draws (`eta`, `aux`, `aux2` are vectors). `t`
is usually a scalar time, but a vector recycled against the draws is
also supported, which is how the likelihood tests evaluate several times
at once.

## Usage

``` r
.r_log_surv(dist, t, eta, aux, aux2)
```
