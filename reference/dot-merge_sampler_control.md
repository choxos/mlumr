# Merge a caller's rstan `control` with the settings mlumr names itself

Tested by name, since `control = NULL` is present in `dots` and would
still reach
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
as a second `control`.

## Usage

``` r
.merge_sampler_control(adapt_delta, max_treedepth, dots)
```

## Value

A list with the merged `control` and `dots` with `control` removed.
