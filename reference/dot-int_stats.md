# Compute summary statistics for integration points

The SD is the population one, with the point count in the denominator:
the grid is a deterministic representation of a distribution, not a
sample from it, and its moments are compared with the distribution's. A
sample SD carried a factor of `sqrt(m / (m - 1))` that no target shares,
0.8% at 64 points, most of the 1% heuristic.

## Usage

``` r
.int_stats(X_int, cov_names, n_agd)
```
