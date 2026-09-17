# Number of distinct aggregate likelihood profiles

Two rows built from the same integration grid contribute the same
likelihood term whatever the link, so the second adds no constraint.
Each grid is sorted into a canonical order before comparing, because the
likelihood sees the multiset of points and not their order. Returns the
row count when there are no integration points.

## Usage

``` r
.agd_distinct_profiles(data)
```
