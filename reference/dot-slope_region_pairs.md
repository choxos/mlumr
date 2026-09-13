# The slope conditions an index's observation regions leave

Every index row is observed to lie in a region: a censored row in the
interval its censoring puts it in, an event row at the single point its
own time puts it at. As the auxiliary goes to its boundary a row's
contribution tends to one where its linear predictor is inside that
region and vanishes exponentially where it is outside, and an event
row's density grows where the predictor reproduces its time and vanishes
anywhere else. So the coefficients keeping the index alive are the ones
satisfying every row at once:

## Usage

``` r
.slope_region_pairs(region)
```

## Arguments

- region:

  The index's design and region ends, as
  [`.check_survival_scale_collapse()`](https://choxos.github.io/mlumr/reference/dot-check_survival_scale_collapse.md)
  reports in its `index_region` attribute.

## Value

A list of `cc`, `dd` and the errors `c_err`, `d_err` their own
subtractions left, each condition reading `cc <= beta * dd`. `NULL`
where the region leaves no condition on the slope or was not usable,
which a caller reads as no restriction at all.

## Details

`lower_i <= mu + beta' x_i <= upper_i`.

Under `model = "spfa"` with `aux_by = "none"` the comparator shares
`beta` AND the auxiliary, so any divergence of its own needs a slope
this region still allows. Eliminating `mu` between a row with a finite
lower end and one with a finite upper end (Fourier-Motzkin) leaves

`lower_i - upper_j <= beta (x_i - x_j)`,

one linear condition on the slope per such pair. Pairs at the same
covariate value say nothing about the slope; they are a conflict or not,
and
[`.censoring_bounds_aux()`](https://choxos.github.io/mlumr/reference/dot-censoring_bounds_aux.md)
has already answered that.

The conditions are returned rather than solved, because two different
questions are asked of them: whether one particular candidate slope is
allowed
([`.index_slope_admits()`](https://choxos.github.io/mlumr/reference/dot-index_slope_admits.md)),
and whether any allowed slope also satisfies one further condition
([`.slope_escape_feasible()`](https://choxos.github.io/mlumr/reference/dot-slope_escape_feasible.md)).
Both read the same representation.
