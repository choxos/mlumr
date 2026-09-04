# Largest finite value, or NA when there is none

`max(x, na.rm = TRUE)` returns `-Inf` for an all-missing vector, which
then passes every "is it small enough" threshold. Return `NA_real_`
instead so a comparison that never happened cannot be reported as
agreement.

## Usage

``` r
.max_finite(x)
```
