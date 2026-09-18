# Compare fitted ML-UMR models

Compare two or more `mlumr_fit` objects by DIC (default), LOO, or WAIC.
For LOO/WAIC,
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html)
is used under the hood; the output is the standard `loo_compare` table.
For DIC the return is a data frame ordered by DIC.

## Usage

``` r
compare_models(
  ...,
  criterion = c("dic", "loo", "waic"),
  survival_unit = c("observation", "arm", "aggregate")
)
```

## Arguments

- ...:

  Two or more `mlumr_fit` objects. For DIC, `mlumr_dic` objects are also
  accepted.

- criterion:

  One of `"dic"` (default), `"loo"`, or `"waic"`. LOO and WAIC require
  the optional `loo` package.

- survival_unit:

  For survival fits compared by `"loo"`/`"waic"`, the pointwise unit
  forwarded to
  [`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md)
  /
  [`calculate_waic()`](https://choxos.github.io/mlumr/reference/calculate_waic.md):
  `"observation"` (default; per reconstructed comparator
  pseudo-individual, optimistic), `"arm"`, or `"aggregate"`. Choose
  `"arm"` or `"aggregate"` to select on whole-external-arm predictive
  fit. Ignored for non-survival families and for `criterion = "dic"`.

## Value

For `"loo"` / `"waic"`: a `compare.loo` table from
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html).
For `"dic"`: a data frame (invisibly) with columns `Model`, `DIC`, `pD`,
`Delta_DIC`.

## Details

DIC is the default for backward compatibility and because it has no
additional package dependencies. For principled Bayesian model
comparison, LOO (Vehtari, Gelman, Gabry 2017) is preferred and requires
the optional `loo` package.
