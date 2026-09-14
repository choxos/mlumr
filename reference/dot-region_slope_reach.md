# Does the index's region reach the slope directions a grid spans?

[`.slope_region_pairs()`](https://choxos.github.io/mlumr/reference/dot-slope_region_pairs.md)
projects the region onto the slope only for ONE declared covariate,
where that projection is an interval. With more the region is a
polyhedron and the projection is not read off pairs, so the question
"does this arm's ridge lie inside it" cannot be answered. That is not
the same as the region being unrestrictive, and reading it as though it
were issued an impropriety certificate on a proper fit.

## Usage

``` r
.region_slope_reach(region, nodes)
```

## Arguments

- region:

  The index's design and region ends, as
  [`.check_survival_scale_collapse()`](https://choxos.github.io/mlumr/reference/dot-check_survival_scale_collapse.md)
  reports in its `index_region` attribute.

- nodes:

  The arm's integration nodes, one row per node.

## Value

`TRUE` where the region constrains at least one slope direction the grid
spans, `FALSE` where it constrains none of them, and `NA` where the
question could not be put. A caller reads anything but `FALSE` as a
reason to report rather than refuse.

## Details

Before reporting that, ask the one part that IS exactly answerable:
whether the region constrains any slope direction the grid can move
along at all. The region confines only the functionals in the row space
of its own design; anything orthogonal to every row is free, so a node
difference `d` whose `(0, d)` lies outside that row space leaves
`d' beta` unbounded and the region says nothing about this arm. Adding
the node-difference rows raises the rank by however many of those
directions the region does NOT already constrain, so a gain equal to
their own rank means it constrains none of them.

The row space does not see which way a row is bounded, and that matters.
Rows bounded on one side only restrict no slope however many directions
they span: raising `mu_index` clears every lower end at any slope, and
lowering it every upper one. A slope is pinned only between a finite
lower end and a finite upper one, which is also where
[`.slope_region_pairs()`](https://choxos.github.io/mlumr/reference/dot-slope_region_pairs.md)
finds its pairs.
