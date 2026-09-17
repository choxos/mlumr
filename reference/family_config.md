# Family metadata registry

Family-specific Stan model names, AgD weighting, prediction-variable
prefixes, and supported links and effect measures, looked up by every
family branch in the R code.

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
  (excluding `"all"`). For `"survival"` the scalar contrast is chosen
  per fit by
  [`.surv_scalar_effect_name()`](https://choxos.github.io/mlumr/reference/dot-surv_scalar_effect_name.md);
  `"hr"` stands for it here.

- `marginal_effect_vars`:

  Generated-quantity column names for each effect measure, per
  population. Expanded in
  [`marginal_effects()`](https://choxos.github.io/mlumr/reference/marginal_effects.md).

- `comp_weight_field`:

  The Stan-data field the comparator-population marginal predictions are
  weighted by, which must match the field the family's
  `generated quantities` block uses: `n_agd` (binomial), `E_agd`
  (poisson), `agd_weight` (normal) and `NULL` for survival.
