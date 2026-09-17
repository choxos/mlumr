# Center IPD and integration covariates about their pooled mean

The intercept then sits at the average covariate, which removes the
intercept and slope collinearity that forces deep NUTS trajectories on
raw-scale covariates. The likelihood is unchanged; the intercept prior
is not. `cov_center` is stored whenever both covariate matrices are
present (zeros when `center = FALSE`) so the prediction functions can
map raw covariate values onto the model scale.

## Usage

``` r
.mlumr_center_covariates(
  stan_data,
  center = TRUE,
  family = "binomial",
  agd_means = NULL
)
```
