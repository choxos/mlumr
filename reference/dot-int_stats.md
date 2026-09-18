# Compute summary statistics for integration points

The SD is the population one: the grid represents a distribution rather
than sampling it, and a sample SD carries a `sqrt(m / (m - 1))` factor
that no target shares.

## Usage

``` r
.int_stats(X_int, cov_names, n_agd)
```
