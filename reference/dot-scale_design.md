# Scale a design's predictors by powers of two

Division by a power of two is exact in binary, so this changes no
distinction between rows: two rows that differ by 1e-20 in a column
still do afterward. That is what the feasibility question needs, where
centering would round a row at 1e-20 onto a row at 0 once both are
shifted by 0.5 and read a free zero row as pinned. Only the size matters
here, so the largest absolute value lands in \[1, 2).

## Usage

``` r
.scale_design(X)
```

## Arguments

- X:

  Design matrix, intercept first.

## Value

The scaled design.
