# Center IPD + integration covariates about their pooled mean (all families)

Matches `center = TRUE` default. The intercept then represents the
baseline at the average covariate rather than at covariate = 0, removing
the intercept\<-\>slope collinearity that forces deep NUTS trajectories
on real-scale covariates. The likelihood is invariant because `X_ipd`
and the integration grid are shifted by the same `xbar` and the
intercept absorbs the shift. A fixed numerical intercept prior is placed
on the centered intercept, however, so centering need not leave the
posterior unchanged. `cov_center` is always stored (zeros when
`center = FALSE`) so predict()/conditional_effects() can map raw-scale
covariate values onto the (possibly centered) model scale.

## Usage

``` r
.mlumr_center_covariates(
  stan_data,
  center = TRUE,
  family = "binomial",
  agd_means = NULL
)
```
