# Can a bounded slope carry a node past a one-sided censoring threshold?

The arm's event rows are all at ONE target, so they are matched at a
single node `z_m` and leave the slope free by themselves; every node
then sits at `t + beta (z_j - z_m)`. A threatening censored row is
escaped where some node's predictor reaches its threshold, which is one
more condition of the same shape the index's own region has,
`c <= beta (z_j - z_m)`, with `c` positive because a row satisfied AT
the target is not threatening in the first place.

## Usage

``` r
.escape_state(pairs, nodes, target, threshold, side)
```

## Arguments

- pairs:

  The index region's conditions, as
  [`.slope_region_pairs()`](https://choxos.github.io/mlumr/reference/dot-slope_region_pairs.md)
  returns.

- nodes:

  The arm's integration nodes, one row per node.

- target:

  The arm's single distinct event target.

- threshold:

  The binding end of the threatening rows' satisfied sets: the largest
  lower end where they are all satisfied above, the smallest upper end
  where they are all satisfied below.

- side:

  `"above"` or `"below"`.

## Value

`"inside"` where some admitted slope escapes STRICTLY, `"outside"` where
no admitted slope escapes at all, and `"boundary"` where the only
escapes sit on a boundary or the arithmetic could not settle it.

## Details

Only the widest separation matters, in each direction. A larger `|d|` is
a weaker condition when `c > 0`, so `max(z) - min(z)` and its negative
dominate every other node pair, and the quadratic scan over pairs is not
needed.
