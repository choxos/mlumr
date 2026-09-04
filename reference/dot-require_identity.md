# Refuse a missing grouping identifier

[`unique()`](https://rdrr.io/r/base/unique.html) keeps `NA` and
`which(NA == NA)` selects nothing, so an arm column of `NA` passed the
single-arm check and then matched no rows: the summary came back with
`.study`, `.trt`, and every covariate mean `NA`, from data that carried
a treatment and a mean. The object was structurally valid, so nothing
downstream objected to integrating over `NA`.

## Usage

``` r
.require_identity(x, nm, as_char = TRUE)
```

## Arguments

- x:

  The column.

- nm:

  Its name, for the message.

- as_char:

  Return `as.character(x)` rather than `x`.
