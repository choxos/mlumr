# Evaluate the conditional survival curve S(t \| profile) per posterior draw

Evaluate the conditional survival curve S(t \| profile) per posterior
draw

## Usage

``` r
.surv_eval_curve(object, eta, treatment = c("index", "comparator"))
```

## Arguments

- object:

  An `mlumr_fit` (survival).

- eta:

  Numeric vector (length n_draws) of the per-draw linear predictor at
  one covariate profile for one treatment.

- treatment:

  Which arm's baseline hazard to evaluate against. With
  `aux_by = ".study"` (the default) each study has its own baseline, so
  the curve depends on which arm is being predicted.

## Value

A matrix of survival probabilities, draws (rows) by fitted prediction
times (columns).
