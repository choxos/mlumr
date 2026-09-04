# Package-specific parametric survival G-computation (RMST difference)

Fits a parametric survival model to the index IPD (adjusting for
covariates), G-computes the marginal restricted mean survival time
(RMST) in the comparator population, and contrasts it with the
comparator RMST from the reconstructed pseudo-IPD. Standard errors come
from a nonparametric bootstrap. This survival extension is a package
benchmark, not the established binary/continuous/count STC procedure.
Requires the `flexsurv` package.

## Usage

``` r
.stc_survival(
  data,
  conf_level,
  z,
  distribution,
  n_boot = 200L,
  seed = NULL,
  rmst_horizon = NULL
)
```
