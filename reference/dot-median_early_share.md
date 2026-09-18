# Share of draws whose median falls before the first fitted prediction time

There the median is interpolated between `S(0) = 1` and the first grid
value, which can be off by a large factor. The share is computed per
draw, as in the RMST check.

## Usage

``` r
.median_early_share(surv_mat)
```
