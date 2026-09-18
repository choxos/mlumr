# Log upper regularized gamma from the log of its argument, in R

Mirrors Stan's `log_gamma_surv_from_log_x()`. Below a `log_x` of -700,
where `exp(log_x)` would underflow and `pgamma(0, k)` report survival 1,
the lower tail is the exact leading series term `w^k / gamma(k + 1)` and
the return is its log complement, `log(1 - w^k / gamma(k + 1))`. Every R
site needing this quantity calls here, as every Stan site does.

## Usage

``` r
.r_log_gamma_surv_from_log_x(k, log_x)
```

## Arguments

- k:

  Shape, recycled to the length of `log_x`.

- log_x:

  Log of the incomplete-gamma argument.

## Value

Log survival, the same length as `log_x`.
