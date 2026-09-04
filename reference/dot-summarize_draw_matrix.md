# Summarize a draws matrix column-wise into a tidy data frame

Applies
[`.summarize_draw_vector()`](https://choxos.github.io/mlumr/reference/dot-summarize_draw_vector.md)
across columns of `draws` and renames the quantile columns to `qNN` form
(e.g., `q2.5`, `q50`, `q97.5`).

## Usage

``` r
.summarize_draw_matrix(draws, probs)
```

## Arguments

- draws:

  A numeric matrix or data frame of posterior draws (one column per
  quantity, one row per draw).

- probs:

  Quantile probabilities.

## Value

Data frame with columns `mean`, `sd`, and one `qNN` column per element
of `probs`.
