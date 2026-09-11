# Scale a design's predictors by powers of two

Division by a power of two is exact in binary, so this changes no
distinction between rows and no exact rank: two rows that differ by
1e-20 in a column still do afterward. Only the size changes, so that the
largest absolute value lands in \[1, 2) and the pivot decisions of a
numerical factorization do not depend on the predictor's units. R's
`dqrdc2` judges a column against its own original norm, so a predictor
whose whole range is `2^-60` survives beside an intercept of ones; a
rule that judged it against the largest column would drop it. Scaling a
column by `c` scales its coefficient by `1 / c`, so the rounding bound
`p * eps * |X||b|` is unchanged too.

## Usage

``` r
.scale_design(X)
```

## Arguments

- X:

  Design matrix, intercept first.

## Value

The scaled design.

## Details

The exception is a column spanning most of the double range, whose
smallest entries underflow to zero when the column is scaled to its
largest; that is why the bitwise and exact tests read the unscaled rows.
