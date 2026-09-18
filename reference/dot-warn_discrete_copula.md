# Warn that the copula correction does not cover nonbinary discrete margins

The Spearman and Pearson corrections cover continuous margins exactly
and binary margins heuristically. A count or ordinal margin goes through
the continuous branch, so its realized association need not match the
target.

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
