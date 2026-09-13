# Which slopes an eventless index still allows

An index whose rows are all censored pins nothing, and that is not the
same as constraining nothing. Each row is observed to lie in a region,
and as the auxiliary goes to its boundary the row's contribution tends
to one where its linear predictor is inside that region and to zero
where it is outside, so the coefficients keeping the index alive are the
ones satisfying every row at once:

## Usage

``` r
.index_slope_admits(region, u1, u2)
```

## Arguments

- region:

  The index's censored design and region ends, as
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

## Details

`lower_i <= mu + beta' x_i <= upper_i`.

That is a nonempty region, which is why the index's own order is zero.
It is not the whole space, and under `model = "spfa"` with
`aux_by = "none"` the comparator shares `beta` AND the auxiliary, so a
divergence needs a slope this region still admits. Eliminating `mu`
between a row with a finite lower end and one with a finite upper end
(Fourier-Motzkin) leaves

`lower_i - upper_j <= beta (x_i - x_j)`,

one linear condition on the slope per such pair. Pairs at the same
covariate value say nothing about the slope; they are a conflict or not,
and
[`.censoring_bounds_aux()`](https://choxos.github.io/mlumr/reference/dot-censoring_bounds_aux.md)
has already answered that.

The candidate slopes are `(u[2] - u[1]) / (z_b - z_a)`, so the test is
carried out on the CROSS-MULTIPLIED form: computing the division and
comparing is a floating-point solve, and a solve certifies nothing. The
sign of `p * D - C * q` decides, corrected for the sign of `q`, and
[`.exact_sum_sign()`](https://choxos.github.io/mlumr/reference/dot-exact_sum_sign.md)
answers it exactly whenever `C`, `D`, `p` and `q` each survived their
own subtraction. Where one of them did not, the answer is undecided
rather than guessed: a false "outside" hides an improper posterior and a
false "inside" refuses a proper one.
