# Whether zero rows can be sent to the boundary while positive rows stay put

Under a log link a zero outcome is matched only in the limit where its
linear predictor goes to `-Inf`, and a zero count's likelihood keeps
rising as its rate falls. Both ask for a direction `d` of the
coefficients that leaves every positive row's predictor unchanged,
`X_pos d = 0`, and lowers the zero rows'. The normal guard needs every
zero row lowered, `X_zero d < 0`, since the residual reaches zero only
when all of them do (`strict = TRUE`). The Poisson likelihood needs only
some row lowered and none raised, `X_zero d <= 0` with `X_zero d != 0`,
since it rises along the ray as long as one rate falls
(`strict = FALSE`). A rank deficit in `X_pos` is necessary for either
and not sufficient: with positive rows at `x = 0` and zeros at `x = -1`
and `x = 1` the one free direction moves the two zero rows in opposite
directions.

## Usage

``` r
.zero_boundary(
  X_pos,
  X_zero,
  raw_pos = X_pos,
  raw_zero = X_zero,
  strict = TRUE
)
```

## Arguments

- X_pos:

  Scaled design rows of the positive outcomes.

- X_zero:

  Scaled design rows of the zero outcomes.

- raw_pos, raw_zero:

  The same rows unscaled, for the bitwise and exact tests: scaling a
  column that spans most of the double range underflows its smallest
  entries to zero.

- strict:

  Whether every zero row must be lowered (`TRUE`, the normal log-link
  boundary) or only some with none raised (`FALSE`, the Poisson
  likelihood's recession direction).

## Value

`"reachable"`, `"unreachable"` or `"unknown"`.

## Details

The question is a linear feasibility one, decided exactly for up to two
free directions. A zero row that lies EXACTLY in the row space of the
positive rows is pinned: every direction that leaves the positive
predictors fixed leaves its own fixed too. Whether it does is settled by
[`.exact_rank()`](https://choxos.github.io/mlumr/reference/dot-exact_rank.md),
never by a small computed distance, since a row at `2^-48` off that
space is free and a numerical test cannot tell it from one exactly on
it. A pinned row makes the strict boundary unreachable and simply drops
out of the weak question. The remaining rows' loadings on the free
directions are computed numerically, so a row within `1e-8` of the row
space, whose loadings are rounding, leaves the answer unknown. The
number of free directions the zero rows load on is again taken exactly;
for one direction the loadings must share a sign, for two they must lie
in an open half-plane, which is a gap of more than pi between
consecutive angles, or for the weak question a closed one with some row
off its boundary. A gap within rounding of pi is settled exactly when
the two rows bounding it point exactly opposite ways, and left unknown
otherwise. Beyond two directions the answer is unknown, and the caller
refuses conservatively.
