# Refuse normal IPD that its own covariates fit exactly

The normal likelihood carries no information about the residual SD once
the fit is exact. With `n` IPD rows and a design of rank `r`,
integrating out the coefficients leaves a marginal density for sigma
proportional to `sigma^(r - n) * exp(-RSS / (2 * sigma^2))`. When `RSS`
is zero that is `sigma^(r - n)` all the way down, whose integral to zero
diverges for every `n > r`, and a proper prior on sigma does not repair
it: a prior with positive density at zero leaves the divergence exactly
where it was. Proper priors on the coefficients do not either; they
scale the density by the prior at the exact solution and leave its shape
in sigma. The posterior is improper and nothing reports it. The sampler
drifts toward zero and returns whatever it reached, with
ordinary-looking diagnostics.

## Usage

``` r
.check_normal_residual_variation(data, link = "identity", center = TRUE)
```

## Arguments

- data:

  An `mlumr_data` object.

- link:

  The resolved link, `"identity"` or `"log"`.

- center:

  Whether the model will center the covariates or QR them; see
  [`.residual_variation_status()`](https://choxos.github.io/mlumr/reference/dot-residual_variation_status.md).

## Value

`TRUE` invisibly if the data were warned about.

## Details

The question is whether an exact fit exists, and
[`.residual_variation_status()`](https://choxos.github.io/mlumr/reference/dot-residual_variation_status.md)
settles it in this order, stopping at the first that decides:

- A saturated design (`n <= rank`) reproduces any outcome, and its
  posterior is proper: the marginal density for sigma is bounded at zero
  and falls as `sigma^(-n)` far out. Nothing in the data separates the
  residual SD from the coefficients there, so what is reported for sigma
  is potentially strongly sensitive to the coefficient priors. That
  warns.

- A constant outcome with `n > rank` is reproduced by the intercept
  alone. The exact fit is certain, and so is the impropriety. Refused.

- Replicate design rows carrying different outcomes prove the residual
  positive whatever the fit, since identical rows get identical fitted
  values. The posterior is proper.

- When every replicate group agrees and there are exactly `rank`
  distinct rows, the design reaches every outcome on those rows, so the
  fit is exact and the posterior improper. Refused.

- Otherwise the computed residual is compared with the rounding an exact
  fit can leave, `p * eps * |X||b|` elementwise. Above it the residual
  is certainly real and the posterior proper. At or below it nothing at
  double precision tells an exact fit from one this close, and Stan
  computes the same likelihood at the same precision, so the model is
  refused as undecidable rather than passed as proper.

A proper posterior whose residual is at most `1e-6` of the outcome's
total sum of squares is warned about: the residual SD will concentrate
near zero and the sampler has to work there. That is a screen on the
input, not a verdict on the fit; the sampler's own diagnostics say how
it went.

Under `link = "log"` an exact fit `y = exp(X b)` exists exactly when
`log(y)` lies in the column space of `X`, so existence is decided by a
linear fit of `log(y)`, which cannot overflow however wide `y` is. The
near-exact screen is then taken on the response scale, where the
likelihood measures its residual, because an outcome spanning many
orders of magnitude can have an ordinary residual in `log(y)` while the
response-scale fit reproduces every large observation and leaves almost
nothing.

A non-positive observation is a valid one under a log-link normal, which
constrains the mean and not the data, and a negative one cannot be
matched by a positive mean at all, so its residual bounds the total away
from zero and the posterior is proper. Zero is different: the mean can
approach it at the boundary where the linear predictor goes to `-Inf`.
If every row is zero, or the positive rows are fitted exactly while some
direction of the coefficients leaves them fixed and drives the zero
rows' predictors down, the likelihood grows as `sigma^(-n)` along that
ray and whether a posterior exists depends on how fast the coefficient
priors decay: a normal prior tames it, a Student-t or Cauchy prior does
not. The guard does not see the prior, so it refuses those cases. It
passes the mixed case when the positive rows leave a real residual, or
when no such direction exists, which
[`.zero_boundary()`](https://choxos.github.io/mlumr/reference/dot-zero_boundary.md)
decides exactly for up to two free directions and leaves unknown, and
therefore refused, beyond.

The test is the residual sum of squares against the outcome's own total
sum of squares, so it is invariant to the units of the outcome.

Resolution. A contrast between design rows smaller than machine epsilon
times the column's own size is below the pivot tolerance of the
factorization and is treated as absent, as it is by the centered design
the model fits by default. The structural rules see the raw rows
exactly, so replicate profiles are matched bitwise whatever their size.
