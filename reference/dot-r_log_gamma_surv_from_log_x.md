# Log upper regularized gamma from the log of its argument, in R

Mirrors Stan's `log_gamma_surv_from_log_x()`. `exp(log_x)` underflows to
zero below about -745 and `pgamma(0, k)` then reports survival 1, which
is badly wrong for a small shape: the survival depends on `w^k`, which
is `exp(k * log_x)` and stays of order one however far the log has gone.
At `k = 1e-6` and `log_x = -1013.8` the true value is 0.0010127 and the
underflowed one is exactly 1.

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

## Details

Below the threshold the leading term of the series for the lower
regularized gamma, `w^k / gamma(k + 1)`, is exact to double precision,
because the next term is smaller by a factor of `w`. A `log_x` of
negative infinity, which is time zero, still gives survival 1.

Every R site needing this quantity calls here, so the survival and the
hazard cannot disagree with each other or with Stan. They did: the
correction was at first applied only to the generalized-gamma survival,
which left the gamma survival reporting 1, the gamma hazard too small by
a factor of 1000, and the generalized-gamma hazard by 987. Stan routes
all four through one function, so R does too.
