# RMST per draw from a target-standardized survival curve (trapezoid)

RMST per draw from a target-standardized survival curve (trapezoid)

## Usage

``` r
.rmst_from_surv_matrix(s_mat, times, share)
```

## Arguments

- s_mat:

  Draws by times survival matrix, already averaged over the target's
  profiles.

- times:

  The integration grid.

- share:

  The per-draw resolution share of the profiles behind `s_mat`, the
  `share` element
  [`.standardize_target_survival_s()`](https://choxos.github.io/mlumr/reference/dot-standardize_target_survival_s.md)
  returns. It is not derived from `s_mat` here on purpose: the average
  of the profiles can pass the check when every profile fails it.
