# Refuse a prediction frame whose two arms cannot be told apart

Colour and fill are keyed on `treatment`, so two arms with one label
fall into one ggplot2 group and the line joins two different
predictions.

## Usage

``` r
.reject_ambiguous_series(x)
```
