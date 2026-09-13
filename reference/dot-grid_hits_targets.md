# Can one affine map send every target onto a grid node?

The comparator's matching equations are solvable when some coefficient
vector sends each distinct target onto the linear predictor of some
integration node. With more distinct targets than the grid's rank that
is not automatic, and it is not impossible either: an overdetermined
system can be consistent, which is the case
[`.check_comparator_tied_events()`](https://choxos.github.io/mlumr/reference/dot-check_comparator_tied_events.md)
used to skip in silence.

## Usage

``` r
.grid_hits_targets(nodes, targets)
```

## Arguments

- nodes:

  The arm's integration nodes, one row per node.

- targets:

  The arm's event targets, on the scale the density matches.

## Value

`TRUE` where an exact map was found, `FALSE` where the enumeration
excluded every candidate, and `NA` where the question was not decided.
Every `NA` carries the reason as a `declined` attribute, because a
question that went unasked is not a question that found nothing and only
the reason says which of those happened:

- `"dimension"`, more than one covariate. A standing limit of the method
  used here, the same answer at every `n_int` and on every arm.

- `"budget"`, a grid past the enumeration budget. The same data is
  answerable at a smaller `n_int`.

- `"inexact"`, a candidate within rounding of a match whose determinant
  could not be settled exactly, because one of the four differences
  feeding it rounded. The eight-product expansion of the original
  operands would decide it and is not built here.

- `"unavailable"`, `"degenerate"`, `"nonfinite"`, no usable grid, fewer
  than two distinct nodes or targets, or non-finite inputs.

The caller reports the first two kinds of fact about a particular grid
and documents the standing limit; see
[`.check_comparator_tied_events()`](https://choxos.github.io/mlumr/reference/dot-check_comparator_tied_events.md).

## Details

Only a single covariate is decided here, where the map is a line
`target = a + b * node` and two (target, node) assignments fix it, so
enumerating node pairs against the first two targets covers every
candidate. Anything wider, or a grid large enough that the enumeration
would cost more than the fit, is left undecided rather than guessed: a
false certificate here refuses a working model.

The enumeration anchors the first target at each node in turn and runs
the second anchor and every candidate node as a vectorized pass, so it
costs `n * n * (k - 2)` elementary operations for `n` nodes and `k`
targets, not the `n * n * (n + k)` of a scalar inner loop. That
distinction is the whole reach of the check: the old cost model capped
it near 170 nodes, so an `n_int` of 256 left unexamined the arm that 8
nodes refused.

The match must be EXACT, not merely close. A best match that leaves a
positive residual is a ridge the profile abandons as soon as the
auxiliary falls below that residual, so accepting one refuses a proper
fit for a singularity it does not have. Nodes `(1, 2, 3)` against
targets `(0, 1, 2 + 1e-15)` are a near miss no affine map removes.

Consistency is therefore read off the DETERMINANT of the original data,

`(u[i] - u[1]) * (z[j2] - z[j1]) - (u[2] - u[1]) * (z[j] - z[j1])`

which is built from differences of the inputs and contains no division
and no slope. Rebuilding predictions from a fitted `(a, b)` and
comparing them to the targets fails in both directions. It is too
permissive, because the slope can be enormous and then the terms forming
a prediction dwarf the residual: nodes `(1, 1 + 2^-52, 2)` against
targets `(0, 1, 2)` give `b = 2^52`, and any tolerance scaled by those
terms accepts the target at 2 against a prediction of 1. And it is too
strict, because the round trip is inexact where the geometry is not:
`-log(2) + log(2) * 3 == log(4)` is FALSE while
`log(4) - log(2) * 2 == 0` is TRUE.

What decides is that determinant's EXACT value, which the computed one
need not be in either direction. Asking instead whether every operation
producing it was individually exact is SUFFICIENT for the computed value
to be the real one, and reading a sufficient condition as a necessary
one left an exactly consistent grid undecided: at `a = qnorm(0.75)`,
nodes `(-a, 0, a)` against targets `(0, log 2, log 4)` are carried by
`mu = log 2` and slope `log(2) / a`, and both products in the
determinant round by the same `-5.3745e-17`, so they cancel and the
computed zero is the true one. Those nodes are the symmetric quartiles
of the default Gaussian integration grid. The determinant is split into
its exact parts instead and
[`.exact_sum_is_zero()`](https://choxos.github.io/mlumr/reference/dot-exact_sum_is_zero.md)
answers for the whole expression, with no tolerance anywhere. Verified
against exact rational arithmetic on 12,000 generated grids, half of
them carrying a planted affine image: no disagreement in either
direction.
