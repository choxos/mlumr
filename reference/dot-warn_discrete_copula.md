# Warn that the copula correction does not cover nonbinary discrete margins

The Spearman and Pearson corrections handle two cases:
continuous-continuous (exact) and anything paired with a BINARY margin
(prevalence-independent heuristics). A count or ordinal margin (Poisson,
negative binomial, an ordered category) is neither. It goes through the
continuous branch, where the map is exact only for a continuous margin,
so the realized association need not match the target: attenuated beside
a continuous margin, and possibly overshooting beside a binary one,
whose heuristic inflates the latent correlation. The correction it needs
depends on its thresholds: with those fixed, the observed correlation
rises strictly with the latent one, so a feasible target has one latent
value, and the package does not implement the numerical inversion that
finds it. Say so rather than let the realized association quietly miss
the target.

## Usage

``` r
.warn_discrete_copula(dtypes, cov_names, cor_adjust)
```

## Arguments

- dtypes:

  Distribution types from
  [`get_distribution_type()`](https://choxos.github.io/mlumr/reference/get_distribution_type.md).

- cov_names:

  Covariate names, same order as `dtypes`.

- cor_adjust:

  The adjustment method in force.

## Value

`TRUE` invisibly if a warning was issued, `FALSE` otherwise.
