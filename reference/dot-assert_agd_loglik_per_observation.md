# Refuse a pointwise log-likelihood that is not one column per observation

Every route into LOO, WAIC, and DIC treats one column of `log_lik_agd`
as one held-out data point, and the arm / aggregate routes additionally
align those columns with `stan_data$agd_arm`. Tie aggregation breaks
both assumptions in the same way: it keeps one row per distinct
likelihood key and carries the multiplicity in `stan_data$agd_count`, so
`log_lik_agd` holds one UNWEIGHTED value per UNIQUE row and `agd_arm` is
the collapsed arm map. Both objects still agree in length, so nothing
downstream errors, and the diagnostics come back quietly understating
every tied observation.

## Usage

``` r
.assert_agd_loglik_per_observation(object)
```

## Details

Fail closed instead. The multiplicities must be expanded back to the
original observation sequence (repeat unique column `k` its
`agd_count[k]` times, and expand the arm map with it) before any
diagnostic reads them.
