# Pointwise log-likelihood for LOO/WAIC, optionally grouped for survival

At the default `survival_unit = "observation"` this is
[`extract_log_lik()`](https://choxos.github.io/mlumr/reference/extract_log_lik.md)
(per-observation; per reconstructed pseudo-individual for survival AgD)
and emits the pseudo-IPD-level warning. For survival fits, `"arm"`
collapses the comparator pseudo-IPD log-likelihood by comparator arm
(summing within arm, exact under conditional independence given the
parameters) and `"aggregate"` collapses all comparator pseudo-IPD into
one external-evidence unit; the index IPD stays per-individual. Leaving
out a grouped unit then corresponds to leaving out that whole external
arm / the whole external evidence, which is the question grouped
LOO/WAIC answer (and is not optimistic at the pseudo-individual level).
Non-survival families ignore the option.

## Usage

``` r
.survival_log_lik_by_unit(object, survival_unit = "observation")
```
