# Refuse an argument these wrappers do not have

`mean` and `sd` sit behind `...` so that they cannot take part in
partial matching. Without that, adding an `sd` formal made `s` ambiguous
between `scale` and `sd`, and `qgamma(p, shape = 2, s = 4)`, which stats
accepts as `scale`, failed with "argument 3 matches multiple formal
arguments". Only formals declared before `...` are matched partially, so
moving the pair behind it restores the abbreviation and costs only this
check, which keeps `...` from silently swallowing a typo the way stats
would not.

## Usage

``` r
.reject_gamma_dots(...)
```

## Arguments

- ...:

  Must be empty.
