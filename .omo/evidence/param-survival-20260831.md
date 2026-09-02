# Parametric survival numerical evidence

## Exact model translation

Invocation:

`Rscript -e 'for (f in c("inst/stan/mlumr_survival_spfa.stan", "inst/stan/mlumr_survival_relaxed.stan")) { x <- rstan::stanc(file=f,isystem="inst/stan",allow_undefined=TRUE); stopifnot(nzchar(x$cppcode)); cat(basename(f), "stanc_ok\n") }'`

Observable:

```text
mlumr_survival_spfa.stan stanc_ok
mlumr_survival_relaxed.stan stanc_ok
```

## Extreme-value driver

Scenario: expose the current `survival_functions.stan` through rstan, evaluate
the supplied Weibull PH, Weibull AFT, Gompertz, and log-logistic cases, then
exercise gamma and generalized-gamma tails, population-standardized hazard,
left censoring, and adjacent-time delayed entry.

Key observables:

```text
weib_ph          -1
weib_aft         -1
gompertz         -1
loglogistic    -800
gamma_haz         0
marginal_haz    400
left_exp       -800
left_gamma    -1600.6931471805599
left_gengamma  -800
gengamma_density -4.9207009302640979e+306
delayed -0.95728570561952742
delayed_want -0.95728570561952497
```

Binary observable: all explicit `stopifnot()` checks completed and the final
driver printed `combined_numerics_ok`. Separate family-increment comparisons
printed `increments_ok`.

## Focused package test

Invocation:

`Rscript -e 'devtools::test(filter = "survival-gengamma-tail", stop_on_failure = TRUE)'`

Observable:

```text
Duration: 84.7 s
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 65 ]
```
