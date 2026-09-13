# Does any node pair form a slope the index still admits?

The three-valued reading of
[`.index_slope_admits()`](https://choxos.github.io/mlumr/reference/dot-index_slope_admits.md)
over every ordered node pair, which is every candidate slope the arm's
divergence could use. One strictly admitted pair is enough to leave the
arm's rate standing; with none, the answer turns on whether anything was
left open.

## Usage

``` r
.admitted_slope_state(nodes, admits)
```

## Arguments

- nodes:

  The arm's integration nodes, one row per node.

- admits:

  The closure
  [`.index_slope_admits()`](https://choxos.github.io/mlumr/reference/dot-index_slope_admits.md)
  returns.

## Value

`"inside"` where some pair's slope is strictly admitted, `"outside"`
where every pair is certified excluded or no pair exists, and
`"boundary"` otherwise. `"boundary"` covers both of the answers a caller
has to report rather than act on: a slope admitted only with equality,
where the index's intercept is pinned to a point, and one the arithmetic
could not settle. They are the same instruction, so they are not told
apart here.

## Details

The scan stops at the first strictly admitted pair, which is the common
case, so the quadratic cost is only paid where the answer really is that
the index admits nothing. Measured on a grid where no pair is ever
admitted, so no early exit happens: 0.02 s at 256 nodes, 0.18 s at 1024,
2.5 s at 4096.
