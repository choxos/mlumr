# Target-standardized marginal log hazard by treatment and time

Uses the equivalent definition `E(f) / E(S)`, accumulating log density
and log survival separately so opposite infinities are never added.

## Usage

``` r
.standardize_target_survival_log_h(
  object,
  newdata,
  times,
  ibasis = NULL,
  ibasis_cmp = NULL,
  mbasis = NULL,
  mbasis_cmp = NULL
)
```
