# Stable difference exp(log_x) - exp(log_y)

Evaluates the difference from the logarithms so that cancellation
happens before the return to the natural scale. Equal logarithms return
exactly `0`, including `-Inf - -Inf`, where both quantities are zero.
Two `+Inf` logs are the exception: both quantities are unbounded, their
difference has no value, and `NaN` is returned rather than a null
effect. Arguments are recycled to a common length; `NA` propagates.

## Usage

``` r
.exp_difference_logs(log_x, log_y)
```
