# The interpretation paragraph `prior_sensitivity()` prints

Kept as a function rather than inline
[`cat()`](https://rdrr.io/r/base/cat.html) calls so the shipped vignette
can be checked against it by CALLING it. The check used to locate these
lines by matching the source text and skipped when the markers moved,
which turned the one edit the gate exists to catch into a silent pass.

## Usage

``` r
.prior_sensitivity_interpretation()
```

## Value

A character vector, one element per printed line.
