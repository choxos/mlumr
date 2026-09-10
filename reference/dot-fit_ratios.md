# Residual and numerical-zero ratios, on a scale where squares cannot overflow

Everything is divided by one common magnitude before being squared, so
the ratios are unchanged while the sums stay in range.

## Usage

``` r
.fit_ratios(X, y, mu_hat, b, rank, mu = NULL)
```

## Arguments

- X:

  Design matrix.

- y:

  Outcome.

- mu_hat:

  Fitted values.

- b:

  Fitted coefficients; `NA` for columns dropped as redundant.

- rank:

  Fitted rank.

- mu:

  Fitted values for a nonlinear link, or `NULL`.

## Value

List with `ratio` (residual sum of squares over the total sum of
squares), `zero_ratio` (the rounding bound on the same scale) and
`rank`.

## Details

`zero_ratio` is what the computed residual can be when the true one is
zero. For `r = y - fl(X b)` the elementwise rounding is bounded by
`p * eps * (|X| |b|)`, which is small when the fitted coefficients are
small and large when they are not. That is an UPPER bound on rounding,
and it is used only in that direction: a residual above it is certainly
real, while a residual at or below it is undecided, not proven zero.
Coefficients dropped as redundant are `NA` and contribute nothing, so a
constant covariate needs no special handling.
