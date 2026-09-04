# Does the target covariate profile lie in the aggregate row space?

Identifying every comparator coefficient is sufficient for identifying
the index-population estimand, not necessary. Under an identity link
that estimand is `mu_c + m_ipd' beta_c`, one linear functional of the
comparator parameters, and the aggregate rows pin down every functional
in their row space. One aggregate row whose covariate means happen to
equal the IPD means identifies it exactly while separating neither the
intercept nor the slope.

## Usage

``` r
.target_in_span(profiles, target, ref_sd)
```

## Arguments

- profiles:

  Aggregate subgroup mean matrix, rows by covariates.

- target:

  The target population's covariate means.

- ref_sd:

  Reference SD per covariate, to condition the comparison.

## Value

`TRUE`, `FALSE`, or `NA` when it cannot be determined.
