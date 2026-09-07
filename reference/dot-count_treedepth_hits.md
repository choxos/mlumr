# Count transitions that stopped at the sampler's treedepth limit

Count transitions that stopped at the sampler's treedepth limit

## Usage

``` r
.count_treedepth_hits(sp, limit)
```

## Arguments

- sp:

  Per-chain sampler parameter matrices from
  [`rstan::get_sampler_params()`](https://mc-stan.org/rstan/reference/stanfit-class.html).

- limit:

  The `max_treedepth` the sampler actually ran under.

## Value

A single count across all chains.
