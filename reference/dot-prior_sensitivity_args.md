# Arguments that replay a fit under a rescaled `prior_beta`

Everything except the prior being swept has to come from the original
fit, or the sweep varies more than one factor. Split out from the refit
loop so the replay can be checked without sampling.

## Usage

``` r
.prior_sensitivity_args(
  fit,
  prior_beta_i,
  verbose,
  prior_beta_comparator_i = NULL
)
```
