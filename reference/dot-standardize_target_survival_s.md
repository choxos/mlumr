# Target-population standardized survival curve S-bar(t) per treatment

g-computation of the population-average survival function over an
arbitrary target population (Chandler & Ishak Eq 14): for each
treatment, `S_bar_k(t) = (1/M) sum_m S_k(t | x_m)` over the `M` rows of
`newdata`.

## Usage

``` r
.standardize_target_survival_s(
  object,
  newdata,
  times,
  ibasis,
  ibasis_cmp = NULL,
  log_scale = FALSE
)
```

## Value

A list with `index` and `comparator`, each an `[n_draws, length(times)]`
matrix of target-standardized survival probabilities, and `share`, a
list of two per-draw vectors named the same way.

## Details

The `share` element is the resolution share of
[`.decay_share()`](https://choxos.github.io/mlumr/reference/dot-decay_share.md)
computed from the decay pieces summed over the profiles, not from the
averaged curve: the average can look resolved when none of its parts is.
