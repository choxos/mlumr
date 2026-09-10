# Response-scale residual ratio of a log-link normal fit

The likelihood under `link = "log"` is `normal(exp(theta), sigma)`, so
the residual that informs sigma is `y - exp(theta)` on the RESPONSE
scale, and that is the one to measure when asking whether it is nearly
zero. Whether it is EXACTLY zero is a different question, answered on
the log scale by
[`.check_normal_residual_variation()`](https://choxos.github.io/mlumr/reference/dot-check_normal_residual_variation.md),
so this fit is only a screen and a failure to converge costs nothing but
the screen.

## Usage

``` r
.log_link_response_ratio(X, y, start = NULL)
```

## Arguments

- X:

  Design matrix, intercept included.

- y:

  Non-negative outcome vector. Zeros are allowed when `start` is given,
  since the default starting values take `log(y)`.

- start:

  Starting coefficients, or `NULL` for
  [`glm.fit()`](https://rdrr.io/r/stats/glm.html)'s own.

## Value

The residual ratio, or `NA` when the fit could not run.

## Details

[`glm.fit()`](https://rdrr.io/r/stats/glm.html) takes its QR pivot
tolerance as `min(1e-7, epsilon / 1000)`, so the only way to stop it
discarding a nearly collinear column is through `epsilon`. At `1e-13`
the pivot tolerance is `1e-16`, matching the linear fit. Whether the
iteration then reports convergence is not required: near an exact fit
the deviance changes at rounding level from one iterate to the next, and
whether that clears the tolerance differs between platforms. The
residual of ANY iterate bounds the least-squares minimum from above, so
a small one justifies the warning and a large one only withholds it,
which is the safe direction for a screen.

The fit is a Gaussian log-link IRLS, whose deviance is a sum of squared
outcomes, so it cannot run on an outcome spanning more than about 700
log units however it is centered: the squares overflow. The caller
centers `log(y)` on its midrange, which keeps both ends finite whenever
the span fits at all. Beyond that there is no screen, and no verdict
depends on one.
