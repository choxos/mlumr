# Marginal effects standardized to an arbitrary target population

Internal dispatch for
[`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
when `newdata` is supplied. Transports the marginal effect to the target
covariate distribution by g-computation (Chandler & Ishak Eq 9-10).
Effect-measure conventions match the built-in populations: binomial
`LOR` is the logit-based marginal odds ratio, `RD`/`RR` are natural;
normal `MD`; poisson `RR` natural.

## Usage

``` r
.marginal_effects_target(
  object,
  newdata,
  effect,
  summary,
  probs,
  at_time = NULL
)
```
