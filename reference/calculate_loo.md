# Calculate LOO-CV for an mlumr_fit

Computes approximate leave-one-out cross-validation (PSIS-LOO, Vehtari,
Gelman, Gabry 2017) using the pointwise log-likelihoods stored by the
Stan models. Returns a `loo` object from the `loo` package.

## Usage

``` r
calculate_loo(object, ...)
```

## Arguments

- object:

  An `mlumr_fit` object.

- ...:

  Additional arguments passed to
  [`loo::loo.array()`](https://mc-stan.org/loo/reference/loo.html).

## Value

An object of class `psis_loo` (see
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html)).

## Details

Pareto-k diagnostics: values \> 0.7 indicate observations for which the
PSIS approximation is unreliable; the printed output flags these.
Typical remedies are running more iterations, using
`moment_match = TRUE`, or (for highly influential AgD rows) refitting
without the offending observation to check sensitivity.

## Note

**AgD rows are treated as independent observations.** Each AgD row
contributes one column to the pointwise `log_lik` matrix. If two or more
AgD rows come from the same study (e.g. subgroup summaries within a
single trial) the PSIS-LOO approximation does not account for the
within-study clustering; effective sample sizes are inflated and
Pareto-k warnings are understated. For clustered AgD, corroborate with
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
or refit omitting suspect rows to check the influence on the posterior.

## Examples

``` r
if (FALSE) { # \dontrun{
loo_spfa <- calculate_loo(fit_spfa)
print(loo_spfa)
} # }
```
