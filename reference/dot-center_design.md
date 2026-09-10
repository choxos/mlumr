# Center a design's predictors on their midrange and scale them to unit size

Centering adds a multiple of the intercept column, so the column space,
the fitted values and the residual are all unchanged, but the
coefficients stop carrying the offset. Without it a predictor recorded
as `x + 1e12` forces an intercept near `-1e12 * slope`, and the rounding
bound counts that cancellation as rounding until it exceeds a genuine
residual.

## Usage

``` r
.center_design(X)
```

## Arguments

- X:

  Design matrix, intercept first.

## Value

The centered and scaled design.

## Details

The center is the midrange, formed from halves so that neither the sum
nor the shift can overflow: a column holding values near both 1e308 and
-1e308 has a mean that overflows on this platform's double accumulation
and a shift from it that overflows for the far value, and either sends a
non-finite design into [`qr()`](https://rdrr.io/r/base/qr.html). Any
center serves; only the offset matters.

Scaling each centered column to a largest absolute value of one keeps
the rank and pivot decisions independent of the predictor's units
whatever the factorization's pivoting rule. R's `dqrdc2` judges a column
against its own original norm, so a predictor whose whole range is
`2^-60` survives beside an intercept of ones; a rule that judged it
against the largest column would drop it, and an outcome exactly
reproduced through it would then read as an ordinary residual against
the intercept alone. Scaling a column by `c` scales its coefficient by
`1 / c`, so the rounding bound `p * eps * |X||b|` is unchanged, and the
column space, the fitted values and every feasibility question are too.
