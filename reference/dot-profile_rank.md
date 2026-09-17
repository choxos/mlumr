# Number of directions an aggregate design spreads along, plus the intercept

Profiles are centered and divided by the IPD SDs, then the directions
whose RMS spread reaches `min_spread` IPD SDs are counted; the floor is
the value
[`check_identification()`](https://choxos.github.io/mlumr/reference/check_identification.md)
screens `spread` on, so the two agree.
[`qr()`](https://rdrr.io/r/base/qr.html) is not used because it judges
each column against its own norm: an offset of 1e7 collapses the rank
and a separation of 1e-11 still counts. A design that cannot be
decomposed returns 0.

## Usage

``` r
.profile_rank(profiles, ref_sd, min_spread = 0.05)
```
