# Family metadata registry

Internal single-source-of-truth for family-specific Stan model names,
AgD weighting, prediction-variable prefixes, and supported links and
effect measures. Every R-side call site that hard-coded a per-family
branch now looks up the relevant field here. This file is deliberately
pure-R and makes no Stan calls; keeping it a data registry makes
rstantools regeneration of `R/stanmodels.R` independent.

## Details

Fields:

- `stan_prefix`:

  Prefix for the Stan model name (the full name is
  `<stan_prefix>_{spfa,relaxed}`).

- `predict_prefix`:

  Column prefix for generated-quantity variables in
  [`predict.mlumr_fit()`](https://choxos.github.io/mlumr/reference/predict.mlumr_fit.md)
  (e.g. `"p"`, `"y"`, `"rate"`).

- `link_default`:

  The default link when the user passes `link = NULL`.

- `links`:

  Vector of supported links. Should match the branches in
  [`check_link()`](https://choxos.github.io/mlumr/reference/check_link.md).

- `effect_measures`:

  Supported values of the `effect` argument in
  [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md)
  (excluding `"all"`).

- `marginal_effect_vars`:

  Generated-quantity column names for each effect measure, per
  population. Expanded in
  [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md).

- `comp_weight_field`:

  Name of the Stan-data field used to weight the comparator-population
  marginal predictions (`n_agd` for binomial, `E_agd` for poisson,
  `NULL` for normal = equal weights).
