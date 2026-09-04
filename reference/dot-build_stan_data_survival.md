# Augment the Stan data list with survival-specific arrays

Augment the Stan data list with survival-specific arrays

## Usage

``` r
.build_stan_data_survival(
  stan_data,
  data,
  surv_info,
  pred_times,
  n_knots,
  knots = NULL,
  rmst_horizon,
  n_rmst_grid = 100L,
  prior_aux,
  prior_smooth,
  n_strata = 1L
)
```

## Arguments

- stan_data:

  The partially built Stan data list to add to.

- data:

  The combined `mlumr_data` object holding the IPD and pseudo-IPD.

- surv_info:

  Distribution metadata from
  [`.survival_distribution_info()`](https://choxos.github.io/mlumr/reference/dot-survival_distribution_info.md).

- pred_times:

  Prediction grid, or `NULL` for the default grid.

- n_knots:

  Number of internal knots for a flexible baseline.

- knots:

  Explicit
  [`make_knots()`](https://choxos.github.io/mlumr/reference/make_knots.md)
  result, or `NULL` to derive them.

- rmst_horizon:

  RMST restriction time, or `NULL` for the default.

- n_rmst_grid:

  Number of RMST integration points.

- prior_aux:

  Prior on the parametric auxiliary shape parameters.

- prior_smooth:

  Prior on the flexible-baseline smoothing SD.

- n_strata:

  Number of baseline strata (1 or 2), from
  [`.resolve_aux_strata()`](https://choxos.github.io/mlumr/reference/dot-resolve_aux_strata.md).

## Value

`stan_data` with the survival arrays, bases and grids added.
