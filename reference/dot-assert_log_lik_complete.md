# Refuse a pointwise log-likelihood that does not cover the fitted data

LOO, WAIC, and DIC score whatever `log_lik_ipd` and `log_lik_agd`
columns the saved draws hold, and extracting them used to stop only when
both were absent. rstan's `pars` with `include = FALSE`, passed through
the `...` of
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md), saves a
fit without either block, and a fit object edited afterwards can lose
single columns. The criteria were then computed over part of the data
the model was fitted to, with only the column count to show for it, and
two fits missing the same block were compared without complaint although
that part can rank them differently than all of it.

## Usage

``` r
.assert_log_lik_complete(object)
```

## Arguments

- object:

  An `mlumr_fit` object.

## Value

`TRUE` invisibly; stops otherwise.

## Details

The columns that belong are fixed by the Stan models: `log_lik_ipd`
holds one per index observation, `1:n_ipd`, and `log_lik_agd` one per
aggregate row, `1:n_agd_rows`, except under survival, where the
comparator enters as reconstructed pseudo-individuals and there is one
per pseudo-individual, `1:n_agd`. Outside survival `n_agd` is each row's
sample size, not a column count. Where tied rows were collapsed, the
expanded count `sum(agd_count)` is what has to be there, and
[`.assert_agd_loglik_per_observation()`](https://choxos.github.io/mlumr/reference/dot-assert_agd_loglik_per_observation.md)
has already refused the collapsed shape. The indexes are compared as a
set, so a missing column, a repeated one, and a misnumbered one that
keeps the count are all refused, and so is a column named as a pointwise
value whose index cannot be read.
