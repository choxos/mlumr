# Central mass of a prior, for choosing a plotting window

Returns the interval holding the prior's central 99%, truncated at
`lower` when the parameter is constrained. `NA`-free and finite: a
Cauchy has no variance but its quantiles exist, and an unrecognized
prior returns an empty range so the caller keeps the posterior window.

## Usage

``` r
.prior_quantile_range(pr, lower = -Inf)
```
