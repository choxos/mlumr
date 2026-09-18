# Calculate LOO-CV for an mlumr_fit

Computes approximate leave-one-out cross-validation (PSIS-LOO, Vehtari,
Gelman, Gabry 2017) using the pointwise log-likelihoods stored by the
Stan models. Returns a `loo` object from the `loo` package.

## Usage

``` r
calculate_loo(
  object,
  survival_unit = c("observation", "arm", "aggregate"),
  ...
)
```

## Arguments

- object:

  An `mlumr_fit` object.

- survival_unit:

  For survival fits, the LOO/WAIC pointwise unit: `"observation"`
  (default; per reconstructed comparator pseudo-individual, optimistic),
  `"arm"` (group the comparator pseudo-IPD by comparator arm, so each
  external arm is one held-out unit), or `"aggregate"` (all comparator
  pseudo-IPD as a single external-evidence unit). The index IPD always
  stays per-individual. Ignored for non-survival families.

- ...:

  Further arguments passed to
  [`loo::loo()`](https://mc-stan.org/loo/reference/loo.html); `r_eff` is
  computed from the fit's chains.

## Value

An object of class `psis_loo` (see
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html)).

## Details

Pareto-k diagnostics: values \> 0.7 indicate observations for which the
PSIS approximation is unreliable; the printed output flags these.
Typical remedies are running more iterations or, for highly influential
AgD rows, refitting without the offending observation to check
sensitivity. Moment matching
([`loo::loo_moment_match()`](https://mc-stan.org/loo/reference/loo_moment_match.html))
needs the fitted model rather than a log-likelihood matrix, so
`moment_match` is refused.

## Note

**AgD rows are treated as independent observations.** Subgroup rows from
one study share no clustering term, so their effective sample sizes are
inflated and Pareto-k warnings understated; corroborate with
[`prior_sensitivity()`](https://choxos.github.io/mlumr/reference/prior_sensitivity.md)
or by refitting without suspect rows.

**Survival fits.** The comparator enters as reconstructed pseudo-IPD, so
the default pointwise unit is one pseudo-individual and the criteria are
optimistic relative to leaving out the comparator arm. Set
`survival_unit = "arm"` or `"aggregate"` to hold out whole comparator
arms or all of the external evidence instead.

## Examples

``` r
if (FALSE) { # \dontrun{
loo_spfa <- calculate_loo(fit_spfa)
print(loo_spfa)
} # }
```
