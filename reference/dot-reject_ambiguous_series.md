# Refuse a prediction frame whose two arms cannot be told apart

Colour and fill are keyed on `treatment`, so when both arms carry the
same label (which
[`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md)
permits, with a warning) the index and comparator series of a population
fall into ONE ggplot2 group. The line and ribbon then connect
alternating rows of two different predictions instead of drawing two
curves, which is a wrong figure rather than an ugly one. Nothing in the
frame can separate them, so refuse rather than guess.

## Usage

``` r
.reject_ambiguous_series(x)
```
