# Refuse a separated binomial fit

A separated binomial GLM reports convergence with finite coefficients,
because iterative reweighting stops when the deviance stops changing.
The fitted values show complete separation (every probability at 0 or
1); quasi-complete separation needs the linear program in
[`.stc_separation_status()`](https://choxos.github.io/mlumr/reference/dot-stc_separation_status.md),
which runs when detectseparation is installed.

## Usage

``` r
.stc_refuse_separation(fit)
```

## Arguments

- fit:

  A fitted `glm`.

## Value

The separation status, invisibly: a list with `status`, one of
`"not_separated"`, `"unknown"` or `"not_applicable"`, and `reason` for
the latter two. A separated fit throws instead.
