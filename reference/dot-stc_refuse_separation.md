# Refuse a separated binomial fit

A separated GLM reports convergence with finite coefficients. Complete
separation shows in the fitted values; quasi-complete separation needs
the linear program in
[`.stc_separation_status()`](https://choxos.github.io/mlumr/reference/dot-stc_separation_status.md).

## Usage

``` r
.stc_refuse_separation(fit)
```

## Value

The separation status, invisibly (`status` and `reason`). A separated
fit throws instead.
