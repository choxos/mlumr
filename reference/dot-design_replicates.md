# Replicate design rows, and whether their outcomes agree

Rows with identical covariates get identical fitted values under any
model, so two such rows with different outcomes leave a residual that no
fit can remove. That is a structural fact, not a numerical one: it
proves the residual sum of squares positive without measuring it. The
rows compared are the ones the model fits, so two raw rows that its
centering rounds together count as replicates, since the model cannot
tell them apart.

## Usage

``` r
.design_replicates(X, y)
```

## Arguments

- X:

  Design matrix as the model fits it.

- y:

  Outcome vector.

## Value

List with `n_distinct`, the number of distinct design rows, and
`consistent`, `FALSE` if some replicate group carries more than one
outcome value.
