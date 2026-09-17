# Refuse an argument these wrappers do not have

`mean` and `sd` sit behind `...` so they cannot take part in partial
matching; this check keeps `...` from swallowing a typo.

## Usage

``` r
.reject_gamma_dots(...)
```
