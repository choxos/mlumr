# Refuse normal IPD that its own covariates fit exactly

With an exact fit the marginal density of the residual SD behaves as
`sigma^(rank - n)` near zero and does not integrate, and the sampler
drifts toward zero with ordinary-looking diagnostics. A constant outcome
and a least-squares fit that is exact to numerical precision (residual
sum of squares at most 1e-12 of the total) are refused; a saturated
design (`n <= rank`) has a proper posterior whose residual SD is not
separated from the coefficients, so it warns. Under `link = "log"` the
question is asked on `log(y)` when every outcome is positive, and not
otherwise.

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

  The centers the model subtracts from the covariates, or a logical for
  the raw design.

## Value

`TRUE` invisibly if the data were warned about.
