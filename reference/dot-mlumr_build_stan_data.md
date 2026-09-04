# Build the Stan data list for mlumr()

Build the Stan data list for mlumr()

## Usage

``` r
.mlumr_build_stan_data(
  data,
  family,
  link_info,
  prior_intercept,
  prior_beta,
  prior_beta_comparator = NULL,
  prior_sigma,
  surv_info = NULL,
  prior_aux = NULL,
  prior_smooth = NULL,
  n_knots = 7L,
  knots = NULL,
  pred_times = NULL,
  rmst_horizon = NULL,
  n_rmst_grid = 100L,
  aux_by = NULL,
  model = "spfa",
  center = TRUE,
  qr = FALSE
)
```
