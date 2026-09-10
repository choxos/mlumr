# Replicate design rows, and whether their outcomes agree

Rows with identical covariates get identical fitted values under any
model, so two such rows with different outcomes leave a residual that no
fit can remove. That is a structural fact, not a numerical one: it
proves the residual sum of squares positive without measuring it. Rows
are compared on their exact binary representation, since
[`paste()`](https://rdrr.io/r/base/paste.html) on doubles keeps fifteen
digits and could merge two rows that differ.

## Usage

``` r
.design_replicates(X, y)
```

## Arguments

- X:

  Raw design matrix.

- y:

  Outcome vector.

## Value

List with `n_distinct`, the number of distinct design rows, and
`consistent`, `FALSE` if some replicate group carries more than one
outcome value.
