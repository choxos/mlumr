# Convert a time column, refusing a factor

[`as.numeric()`](https://rdrr.io/r/base/numeric.html) on a factor
returns its level codes, which look like plausible times and pass every
later check.

## Usage

``` r
.reject_factor_time(x, nm)
```
