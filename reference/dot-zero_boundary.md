# Whether zero rows can be sent to the boundary while positive rows stay fit

Under a log link a zero outcome is matched only in the limit where its
linear predictor goes to `-Inf`. The residual goes to zero along a ray
in coefficient space exactly when some direction `d` leaves every
positive row's predictor unchanged, `X_pos d = 0`, and lowers every zero
row's, `X_zero d < 0`. A rank deficit in `X_pos` is necessary for that
and not sufficient: with positive rows at `x = 0` and zeros at `x = -1`
and `x = 1` the one free direction moves the two zero rows in opposite
directions, and their means stay bounded away from zero.

## Usage

``` r
.zero_boundary(X_pos, X_zero, raw_pos = X_pos, raw_zero = X_zero)
```

## Arguments

- X_pos:

  Scaled design rows of the positive outcomes.

- X_zero:

  Scaled design rows of the zero outcomes.

- raw_pos, raw_zero:

  The same rows unscaled, for the bitwise duplicate test: scaling a
  column that spans most of the double range underflows its smallest
  entries to zero, and a zero row at 1e-200 would then read as a
  duplicate of a positive row at 0.

## Value

`"reachable"`, `"unreachable"` or `"unknown"`.

## Details

The question is a linear feasibility one. With `k` free directions it is
decided exactly for `k <= 2`: a zero row whose profile lies in the row
space of the positive rows is pinned and settles it; for one direction
the zero rows' loadings must share a sign; for two, their loadings must
lie in an open half-plane through the origin, which is a gap of more
than pi between consecutive angles. Beyond two, and for a gap within
rounding of pi, the answer is left unknown and the caller refuses
conservatively.

The rows arrive scaled but not centered: centering rounds, and a zero
row at `1e-20` shifted by `0.5` lands on the positive rows at `0` and
reads as pinned when it is free.
