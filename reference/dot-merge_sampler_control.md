# Merge a caller's rstan `control` with the settings mlumr names itself

Tested by name rather than by value. A caller who writes
`control = NULL` leaves an element that is present and NULL, so
`is.null(dots$control)` is true while the name is still in `dots`, and
forwarding it would hand
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
two `control` arguments: the collision this merge exists to prevent. `$`
also matches partially, so an exact test on the names is the one that
means what it says.

## Usage

``` r
.merge_sampler_control(adapt_delta, max_treedepth, dots)
```

## Arguments

- adapt_delta, max_treedepth:

  The settings mlumr exposes as arguments.

- dots:

  The caller's `...`, as a list.

## Value

A list with the merged `control` and `dots` with `control` removed.
