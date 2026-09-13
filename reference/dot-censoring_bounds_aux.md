# Whether a censored row bounds the auxiliary away from its boundary

The event rows are fitted exactly, so the auxiliary runs to its boundary
unless some censored row's survival goes to zero with it. That happens
when the row's linear predictor is strictly below the log of its
censoring time, and the condition is the same whichever boundary it is:
the log-normal survival `1 - Phi((log c - eta) / sigma)` goes to zero as
`sigma` does, and `exp(-(c e^-eta)^k)` goes to zero as `k` grows.

## Usage

``` r
.censoring_bounds_aux(
  X,
  y,
  events,
  exact_fit = FALSE,
  lower = NULL,
  upper = NULL
)
```

## Arguments

- X:

  The full centered design, intercept first.

- y:

  The fitted response for every row, on the scale the family's collapse
  happens on: `log(time)` for the location-scale families, whose `eta`
  sits at `log t`, and `time` itself for Gompertz, whose ridge is
  `log(shape) - log(expm1(shape * t))` and so needs the times rather
  than their logarithms. This function does not transform it and does
  not know the family; it only requires that `lower` and `upper` arrive
  on the same scale.

- events:

  Logical, which rows are events.

- exact_fit:

  Whether the event rows are known to be fitted exactly. The structural
  shortcut above holds only then, and the caller knows it from the
  geometry it already measured; a near-exact fit puts the duplicated row
  at the fitted value rather than at the event's time, so the shortcut
  is not taken for one.

- lower, upper:

  The ends of each non-event row's observation region, on the same scale
  as `y`, one value per non-event row, in the order those rows appear in
  `X`. `lower` may be `-Inf` and `upper` may be `Inf`, which is what an
  open end means; `lower <= upper` is required and a row that violates
  it leaves the answer `"undetermined"`. The defaults are the
  right-censored region, `y[!events]` and `Inf`, so a caller that knows
  only times gets the behavior it had before the other two censoring
  types were admitted.

## Value

`"bounded"`, `"suppresses"`, `"unbounded"`, or `"undetermined"`.
`"bounded"` means the auxiliary is held away from its boundary, which is
an exponential suppression and removes any polynomial growth elsewhere.
`"suppresses"` means it is not, but the coefficient volume that keeps
the rows' likelihood positive shrinks as the auxiliary's width to the
power of the `order` attribute, so it cancels that many powers of a
growth that shares the auxiliary. `"unbounded"` means the contribution
is a positive constant, and `"undetermined"` that none of the three was
established.

## Details

It is a question about a censored row's fitted value, not about the
coefficients. Without full column rank the exact solutions form an
affine family, but a row's fitted value is the same across the whole
family whenever its covariate vector lies in the ROW SPACE of the event
design, which is the usual estimability condition. That is strictly
weaker than identifying every coefficient, and the difference is not a
corner case: exact events and a censored row at the same covariate
profile fix that row's predictor exactly, however deficient the design
is, because the row is one of the event rows. Requiring the stronger
condition refused fits that were proper.

A row that is not estimable is not evidence in either direction, and is
consulted for neither answer: it cannot produce `"bounded"`, and it
blocks `"unbounded"`, which needs every row to be strictly inside its
region. All it can do is leave the answer `"undetermined"`.

A censored row that repeats an event row's covariate profile needs no
solve at all. Where the event fit interpolates, its predictor IS that
event's own `y`, for every exact solution and however deficient or
ill-conditioned the design is, so the question reduces to comparing two
stored data values. That is worth asking first: it is exact where the
numerical path is not, and it answers cases the numerical path refuses,
including a design whose numerical rank falls below its exact one.
