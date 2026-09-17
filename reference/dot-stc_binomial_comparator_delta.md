# Comparator-population delta-method terms for binomial STC

The standardized event probability is `p = sum(w_i p_i) / sum(w_i)` over
the comparator grid; its gradient in the coefficients is analytic, so
the uncertainty does not depend on the predictors' units. The gradients
are formed from the log probabilities; see
[`.stc_binomial_gradients()`](https://choxos.github.io/mlumr/reference/dot-stc_binomial_gradients.md).

## Usage

``` r
.stc_binomial_comparator_delta(
  fit,
  newdata,
  weights,
  beta_hat,
  V,
  link_resolved,
  log_p_A,
  log_q_A,
  p_B,
  var_p_B
)
```
