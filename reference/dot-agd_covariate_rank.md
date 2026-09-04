# Rank of the AgD comparator covariate design

The relaxed model's `mu_comparator` and `beta_comparator` are informed
only by the AgD likelihood, which contributes one term per AgD row
evaluated at that row's integration grid. The number of comparator
parameters those terms can separate is therefore the rank of the per-row
mean covariate profiles augmented with an intercept column, NOT the
number of rows: rows that repeat the same covariate summaries add
likelihood terms but no new direction.

## Usage

``` r
.agd_covariate_rank(data)
```

## Arguments

- data:

  An `mlumr_data` object with integration points.

## Value

Integer rank, at least 1.

## Details

Uses the declared aggregate covariate means, which define the
identity-link design exactly and do not vary with integration
resolution.
