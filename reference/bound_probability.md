# Apply a boundary-only binomial continuity correction

At zero or all events, uses the pseudo-count estimate
`(r + min_count) / (n + 2 * min_count)`. Interior probabilities are
unchanged.

## Usage

``` r
bound_probability(p, n, min_count = 0.5)
```
