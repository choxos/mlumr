# Numerical rank of the centered aggregate profile matrix, plus the intercept

[`.profile_rank()`](https://choxos.github.io/mlumr/reference/dot-profile_rank.md)
says how far a design moves; this says whether the directions exist at
all, with the usual `max(dim) * eps * max(d)` tolerance. Profiles at
-0.01 and 0.01 have a spread below the screen and a numerical rank of 2,
and precise aggregate outcomes can still pin the slope down there.

## Usage

``` r
.profile_numeric_rank(profiles, ref_sd)
```
