# Refuse a pointwise log-likelihood collapsed over tied aggregate rows

Tie aggregation (not shipped yet) would keep one `log_lik_agd` column
per distinct row with the multiplicity in `stan_data$agd_count`. LOO,
WAIC and DIC need one column per observation, so that shape is refused
here.

## Usage

``` r
.assert_agd_loglik_per_observation(object)
```
