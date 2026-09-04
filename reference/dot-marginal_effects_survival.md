# Marginal survival treatment effects (internal dispatch for marginal_effects)

Returns the marginal hazard ratio (PH) / time ratio (AFT) on the natural
scale (null 1), the RMST difference (null 0), and the natural-scale RMST
ratio (null 1), in the index and/or comparator populations, matching the
estimands of Chandler & Ishak (ML-UMR survival). The Stan `delta_*` are
log HR / log time ratios and are exponentiated here.

## Usage

``` r
.marginal_effects_survival(
  object,
  population,
  effect,
  summary,
  probs,
  at_time = NULL
)
```
