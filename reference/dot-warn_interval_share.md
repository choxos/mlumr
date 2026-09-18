# Warn when enough posterior draws are integrated from a single straight line

A curve is badly resolved in a draw when more than half of its decay
falls inside one grid interval; the criterion is the fraction of such
draws on the worst curve, ignored below one in twenty.

## Usage

``` r
.warn_interval_share(shares)
```
