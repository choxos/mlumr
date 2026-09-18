# Warn when the RMST grid is too coarse for the hazard it is integrating

When most of the decay falls inside one grid interval the trapezoid rule
overstates the integral badly, and both arms alike, so a difference or
ratio can lose the whole effect: exponential rates 100 and 200 to
`tau = 10` on the default 100-node grid give an RMST ratio of 1.0001
against a true 2.0. The trigger is the share of the total decay that
lands in one interval.

## Usage

``` r
.warn_coarse_rmst_grid(share)
```

## Arguments

- share:

  Per-draw shares, one vector per curve, from
  [`.decay_share()`](https://choxos.github.io/mlumr/reference/dot-decay_share.md)
  or
  [`.standardize_target_survival_s()`](https://choxos.github.io/mlumr/reference/dot-standardize_target_survival_s.md);
  `NA` where there is no decay.

## Value

`NULL`, invisibly; called for the warning.
