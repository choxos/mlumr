# Calculate DIC for model comparison

Computes the Deviance Information Criterion using the variance-based
effective-parameters formula from Gelman et al. (2004):
`pD = 0.5 * Var(D)`. This is more stable than the plug-in alternative
for multimodal posteriors.

## Usage

``` r
calculate_dic(object)
```

## Arguments

- object:

  An `mlumr_fit` object

## Value

A list of class `mlumr_dic` with components `DIC`, `pD`, `D_bar`

## Details

DIC is retained for backward compatibility and rough comparison. For
principled Bayesian model comparison, prefer
[`calculate_loo()`](https://choxos.github.io/mlumr/reference/calculate_loo.md)
or
[`calculate_waic()`](https://choxos.github.io/mlumr/reference/calculate_waic.md)
(Vehtari, Gelman, Gabry 2017).

## Examples

``` r
if (FALSE) { # \dontrun{
dic_spfa <- calculate_dic(fit_spfa)
dic_relaxed <- calculate_dic(fit_relaxed)
} # }
```
