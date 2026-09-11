# Comparator-population delta-method terms for binomial STC

The standardized event probability is `p = sum(w_i p_i) / sum(w_i)` over
the comparator grid, and its uncertainty comes through the delta method
from the coefficient covariance. The gradients are analytic:
`d p / d beta = sum(w_i p_i'(eta_i) X_i) / sum(w_i)`, with `p_i'` the
inverse link's derivative, and the link-scale and log-scale functionals
follow by the chain rule. A central difference in the coefficient
coordinates was used before, with a step proportional to
`max(1, |beta|)`; that step is not a property of the model. Multiply a
predictor by 1e6 and its coefficient shrinks by 1e6 while the step stays
near 6e-6, so the perturbation moved the target linear predictor by
about 6, not a local derivative at all, and a comparator probability of
0.75 on 40 subjects at the observed profile reported a standard error of
0.032 instead of the 0.068 the same data give in any other units.
Analytic gradients transform with the design, so equivalent units give
equivalent uncertainty.

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

## Details

Everything is formed on the log scale so that a tail probability outside
double precision keeps its digits: see
[`.stc_binomial_gradients()`](https://choxos.github.io/mlumr/reference/dot-stc_binomial_gradients.md).
