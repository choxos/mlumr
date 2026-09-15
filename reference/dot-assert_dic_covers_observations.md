# Refuse a cached DIC that does not cover the observations it carries

[`compare_models()`](https://choxos.github.io/mlumr/reference/compare_models.md)
accepts `mlumr_dic` objects beside fits and uses their numbers as they
are, so
[`.assert_log_lik_complete()`](https://choxos.github.io/mlumr/reference/dot-assert_log_lik_complete.md)
never sees the draws they were computed from.
[`calculate_dic()`](https://choxos.github.io/mlumr/reference/calculate_dic.md)
used to score whatever `log_lik_ipd` and `log_lik_agd` columns a fit
held, and an object it made then records in `n_obs` how many columns
that was, beside the frames of the observations the fit was built from.
A saved object outlives the fix, since loading it is not recomputing it,
and the check between objects that their `n_obs` agree does not see two
that are partial alike.

## Usage

``` r
.assert_dic_covers_observations(dic)
```

## Arguments

- dic:

  An `mlumr_dic` object.

## Value

`TRUE` invisibly; stops otherwise.

## Details

The pointwise units are fixed by the data the object carries: one per
index row, and one per aggregate row, or per reconstructed comparator
pseudo-individual under survival, where the `pseudo` frame is present.
An `n_obs` that differs from that count was computed over part of the
data, or over columns that were not the fit's, and is refused: the
omitted terms' covariance with the kept ones enters the variance
penalty, so the score cannot be completed from its value. A count that
agrees is consistent with a complete score rather than proof of one,
since columns misnumbered within the right count leave no trace in a
scalar. An object without its observations, from a version before they
were recorded, cannot be checked and is compared as before, with the
message that
[`.assert_same_observations()`](https://choxos.github.io/mlumr/reference/dot-assert_same_observations.md)
gives for a model carrying no data; one that carries its observations
but not `n_obs` is compared with a message of its own saying that the
count is not recorded.
