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

`TRUE` only where an exact map was found, `FALSE` where the enumeration
excluded every candidate it examined, and `NA` where the case was not
decided: more than one covariate, a grid past the enumeration budget, or
a candidate that is close without being exact.

## Details

Only a single covariate is decided here, where the map is a line
`target = a + b * node` and two (target, node) assignments fix it, so
enumerating node pairs against the first two targets covers every
candidate. Anything wider, or a grid large enough that the enumeration
would cost more than the fit, is left undecided rather than guessed: a
false certificate here refuses a working model.

The match must be EXACT, not merely close. A best match that leaves a
positive residual is a ridge the profile abandons as soon as the
auxiliary falls below that residual, so accepting one refuses a proper
fit for a singularity it does not have. Nodes `(1, 2, 3)` against
targets `(0, 1, 2 + 1e-15)` are a near miss no affine map removes, and
they report undecided rather than a match.

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
