# The near-exact screen for a log-link outcome with non-positive values

The response-scale fit on the whole outcome, started from the positive
rows' log-scale fit. The outcome is scaled by a power of two, which is
exact and only shifts the intercept under a log link. Any iterate's
residual bounds the least-squares minimum from above, so a small one
justifies the warning and a large one only withholds it.

## Usage

``` r
.screen_mixed_zero(X, y, pos)
```

## Arguments

- X:

  Design matrix as the model fits it, intercept included.

- y:

  Outcome vector with at least one positive value and at least one zero
  or negative one.

- pos:

  Logical, which rows are positive.

## Value

`TRUE` invisibly if the warning was issued.
