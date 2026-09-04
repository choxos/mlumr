# Chain-aware tail ESS for rstan draws

rstan does not report tail ESS, so compute it from the post-warmup draws
with `posterior`. Returns a named numeric vector over the columns of
`draws`, all `NA_real_` when `posterior` is unavailable or the draws
cannot be laid out as equal-length chains.

## Usage

``` r
.rstan_ess_tail(draws, chain_ids)
```
