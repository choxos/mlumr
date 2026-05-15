# Calculate WAIC for an mlumr_fit

Watanabe-Akaike Information Criterion (Watanabe 2010) based on the
pointwise log-likelihoods. WAIC is asymptotically equivalent to LOO-CV;
prefer
[`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md)
when Pareto-k is well-behaved.

## Usage

``` r
calculate_waic(object, ...)
```

## Arguments

- object:

  An `mlumr_fit` object.

- ...:

  Additional arguments passed to
  [`loo::waic()`](https://mc-stan.org/loo/reference/waic.html).

## Value

An object of class `waic` (see
[`loo::waic()`](https://mc-stan.org/loo/reference/waic.html)).

## Note

As with
[`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md),
each AgD row is treated as an independent observation. WAIC will be
optimistic for AgD rows that share a study (see the note on
[`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
waic_spfa <- calculate_waic(fit_spfa)
} # }
```
