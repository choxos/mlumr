# Target-population standardized response means per treatment (g-computation)

Bayesian g-computation / model-based standardization of the
per-treatment marginal mean outcome over an arbitrary target population,
reusing the validated conditional-effects machinery (`.conditional_*`).
For each posterior draw and treatment k, returns
`mu_k = (1/M) sum_m g^{-1}(alpha_k + (x_m - xbar)' beta_k)` averaged
over the `M` rows of `newdata` (the target covariate distribution).
Non-survival families only.

## Usage

``` r
.standardize_target_response(object, newdata)
```

## Arguments

- object:

  An `mlumr_fit` (binomial / normal / poisson).

- newdata:

  Data frame of target-population covariate profiles.

## Value

A list with `index` and `comparator`, each a length-`n_draws` vector of
target-standardized marginal response means.
