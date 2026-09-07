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

The `share` element judges how well `times` resolves the curves that
were averaged, one value per draw and treatment: the decay that the
profiles lose inside their own steepest grid interval, summed over
profiles, as a fraction of the decay they lose in total. It is measured
on each profile's curve before the averaging, because the average can
look resolved when none of its parts is. Two profiles that each collapse
inside a different interval average to a curve that loses half its decay
in each, under any threshold, while the trapezoid rule is linear and
overstates the average RMST by exactly the mean of what it overstates
for the two. With one profile the share is the one
[`.rmst_max_interval_share()`](https://choxos.github.io/mlumr/reference/dot-rmst_max_interval_share.md)
computes.
