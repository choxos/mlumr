# Stable difference exp(log_x) - exp(log_y)

Cancellation happens before the return to the natural scale. Equal logs
return exactly `0`, two `+Inf` logs return `NaN`, arguments recycle and
`NA` propagates.

## Usage

``` r
.exp_difference_logs(log_x, log_y)
```
