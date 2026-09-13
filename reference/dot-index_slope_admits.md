# Is one candidate slope still allowed by the index?

The conditions
[`.slope_region_pairs()`](https://choxos.github.io/mlumr/reference/dot-slope_region_pairs.md)
leaves, tested at the slopes a comparator arm's divergence could
actually use. Those are `(u[2] - u[1]) / (z_b - z_a)`, so the test is
carried out on the CROSS-MULTIPLIED form: computing the division and
comparing is a floating-point solve, and a solve certifies nothing. The
sign of `p * D - C * q` decides, corrected for the sign of `q`, and
[`.exact_sum_sign()`](https://choxos.github.io/mlumr/reference/dot-exact_sum_sign.md)
answers it exactly whenever `C`, `D`, `p` and `q` each survived their
own subtraction. Where one of them did not, the answer is undecided
rather than guessed: a false "outside" hides an improper posterior and a
false "inside" refuses a proper one.

## Usage

``` r
.index_slope_admits(region, u1, u2)
```

## Arguments

- region:

  The index's design and region ends, as
  [`.check_survival_scale_collapse()`](https://choxos.github.io/mlumr/reference/dot-check_survival_scale_collapse.md)
  reports in its `index_region` attribute.

- u1, u2:

  The arm's two lowest distinct targets. Their difference is the
  numerator every candidate slope shares, and it arrives unsubtracted
  because whether that subtraction was itself exact decides whether the
  sign below is the true one: a numerator off by a rounding is a slope
  off by a rounding, and a wrong "inside" there is a false refusal.

## Value

A function of `(z0, z)` giving, for each `z`, whether the slope
`p / (z - z0)` is strictly inside the region (`1`), on its boundary
(`0`), outside it (`-1`), or undecided (`NA`). A denominator of zero or
a non-finite one is not a candidate slope and reads as `-1`. `NULL`
where the region does not restrict the slope or was not usable, which a
caller reads as no restriction at all.
