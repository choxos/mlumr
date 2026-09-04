# Print one resolved regression-coefficient prior block

`beta` and `beta_comparator` are reported the same way, so the broadcast
/ per-coefficient decision, the autoscaling footnote and the default tag
are written once. `resolved` is the per-coefficient struct stored on the
fit; `user_prior` is what the caller passed, used for the fallback on
older fits that carry no resolved struct and for the package-default
tag.

## Usage

``` r
.print_beta_prior_block(heading, resolved, user_prior, digits)
```
