# Share of draws whose median falls before the first fitted prediction time

There the median is interpolated between `S(0) = 1` and the first grid
value, with nothing in between to say where inside that interval the
curve crossed 0.5. The error is not small: an exponential curve with
rate 100 has median 0.0069, and a grid whose first point is 0.2 reports
0.1. Per draw, like the RMST check, so a posterior in which some draws
collapse early is not hidden by the ones that do not.

## Usage

``` r
.median_early_share(surv_mat)
```
