# Least-squares residual ratios of a linear fit

[`lm.fit()`](https://rdrr.io/r/stats/lmfit.html)'s default pivot
tolerance of `1e-7` can drop a column that is nearly but not exactly
collinear with another, measuring the residual against a design smaller
than the one that will be fitted, so it is lowered to the floor. A
linear fit of finite data always returns, so this cannot fail to give a
verdict.

## Usage

``` r
.linear_fit_ratios(X, y)
```

## Arguments

- X:

  Design matrix, intercept included.

- y:

  Outcome vector.

## Value

See
[`.fit_ratios()`](https://choxos.github.io/mlumr/reference/dot-fit_ratios.md).
