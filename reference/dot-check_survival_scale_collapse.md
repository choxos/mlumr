# Refuse a log-normal survival fit whose exact events collapse its scale

A log-normal AFT is a normal model for `log(t)` with a positive scale
`sdlog`, and its exact-event density has exactly the singularity that
[`.check_normal_residual_variation()`](https://choxos.github.io/mlumr/reference/dot-check_normal_residual_variation.md)
refuses. With `n` uncensored index rows, a design of rank `r` that
reproduces every `log(t)` exactly, and the coefficients integrated out,
the marginal density of the scale behaves as `sdlog^(r - n)` near zero.
Its integral diverges for every `n > r`, and no prior with positive
density at zero repairs it: `prior_aux` defaults to a half-normal, and
the half-t and exponential alternatives all have positive density there.
The sampler would drift toward zero and report where it stopped.

## Usage

``` r
.check_survival_scale_collapse(
  data,
  distribution,
  aux_by = ".study",
  center = TRUE
)
```

## Arguments

- data:

  An `mlumr_data` object with `family = "survival"`.

- distribution:

  The resolved survival distribution.

- aux_by:

  The auxiliary stratification, as passed to
  [`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md).

- center:

  The centers the model subtracts from the covariates, as for
  [`.check_normal_residual_variation()`](https://choxos.github.io/mlumr/reference/dot-check_normal_residual_variation.md).

## Value

`TRUE` invisibly if the data were warned about, `FALSE` otherwise,
carrying a `bounds_aux` attribute that is `TRUE` when the index rows
were shown to bound the auxiliary away from its boundary (a real
residual, or a censored row that bounds). Anything else means this
function did not establish that, which is not the same as establishing
the opposite.
[`.check_comparator_tied_events()`](https://choxos.github.io/mlumr/reference/dot-check_comparator_tied_events.md)
reads it under `aux_by = "none"`. A shared-auxiliary warning also
carries `index_exact`: `TRUE` when the index event design was shown to
reproduce its own times and so pins a shared coefficient vector, `FALSE`
only where it was shown to pin nothing, and `NA` where the question was
not settled. The three are distinct on purpose, and an index with NO
events is not automatically the second of them: having no events means
no design to fit, not that nothing bounds the auxiliary. Censored rows
alone can bound it, and when they conflict they do, so an eventless
index is answered by asking whether any linear predictor satisfies every
one of its regions at once. A certified conflict returns
`bounds_aux = TRUE`, a certified absence of one returns
`index_exact = FALSE`, and an undecided case returns neither.

## Details

Four scopes, each a deliberate limit rather than a certificate.

**The distribution.** Two groups, told apart by what the auxiliary `aux`
is rather than by the family's name. `"lognormal"` and `"gengamma"` are
refused, because for both of them `aux` *is* a scale: the log-scale
standard deviation for the one, and `sigma` for the other, which enters
the Lawless density `gengamma_lpdf(y, mu, sigma, k)` as a `-log(sigma)`
term and as the divisor of the log residual. An exact fit sends either
to zero, where the density with the coefficients integrated out behaves
as `aux^(rank - n)` and does not integrate. The generalized gamma's
*second* auxiliary is its shape, and it is not what diverges: at an
exact fit the density's dependence on it is bounded.

The Weibull, log-logistic and gamma are log-location-scale families too,
but their auxiliary is a shape, the reciprocal of a scale, so the same
exact fit sends it to `+Inf` rather than to zero. The likelihood there
grows polynomially in the shape and a half-normal or exponential prior's
tail integrates it, while a half-t's may not: propriety is a property of
the prior, not of the data, and refusing the data would refuse
well-posed default fits. A warning says so instead, and a censored row
that bounds the shape suppresses it, as it does for a scale.

**The auxiliary stratification.** Decided only when the index study
holds the scale alone, which `aux_by = ".study"` (the default) and
`NULL` both give it. Under `aux_by = "none"` the comparator rows enter
the same parameter, and sharing does not bound it by itself: a
comparator of right-censored rows whose fitted times sit above their
censoring times contributes a likelihood tending to one as the scale
goes to zero, which repairs nothing. Whether it bounds the parameter is
a question about the marginalized aggregate likelihood, which this
geometry does not see, so that case is reported rather than decided, and
warned about rather than refused.

**Censoring.** A right-censored row at `c` whose fitted `eta` is below
`log(c)` has survival going to zero faster than any power of the scale,
and it makes the posterior proper on its own. One at or above `log(c)`
has survival going to one half or one and does nothing. So censored rows
are consulted, and only when the fit on the event rows determines their
linear predictors. That is estimability, not identification: a censored
row's predictor is determined whenever its covariate vector lies in the
ROW SPACE of the event design, which is weaker than that design having
full column rank, and the difference is not a corner case, since a
censored row at a covariate profile the events already occupy is always
in it. A gap between the fitted predictor and the nearer end of the
row's region that is no larger than the rounding in computing it does
not count either, since its sign is not information. Otherwise the
question is left undecided and said to be. They are consulted BEFORE any
message is issued, including the near-exact and saturated ones: a row
that bounds the parameter makes every one of those messages untrue, not
just the refusal.

**Which scale the fit is read on.** The log-time one for the
location-scale families, whose `eta` sits at `log t`. Gompertz is the
exception: its hazard is `exp(eta + shape * t)`, so profiling a row's
density over `eta` puts the maximum at
`log(shape) - log(expm1(shape * t))`, about `log(shape) - shape * t`.
That ridge needs the event TIMES in the column space, not their
logarithms, with the intercept absorbing the `log(shape)`, so Gompertz
is read on the time scale throughout.

**Delayed entry, left and interval censoring.** All examined, through
the OBSERVATION REGION each row is known to lie in, on whichever of
those two scales the family is read on. A right-censored row at `c` runs
from `c` upwards with no upper end; a left-censored one at `u` runs up
to `u` with no lower end; an interval one runs from `l` to `u`. As the
auxiliary goes to its boundary the fitted distribution concentrates at
the fitted value, so a row's contribution tends to one when that value
is strictly inside its region and to zero when it is strictly outside,
and only the second bounds the auxiliary.

A delayed entry is not an ordinary lower end, because the contribution
is conditional on survival to it: with the fitted value BELOW the entry
the conditional law piles up just above it and the probability tends to
one, not to zero. So an entry never closes a region from below, while an
interval that opens strictly above its entry still does.

Skipping a censoring type outright was not the safe choice it looked
like: it let a row that suppresses nothing stand in for one that does,
and sent an improper posterior to the sampler in silence.

The comparator side is not examined HERE, and its rows are not safe for
being left out. They enter a likelihood marginalized over the
integration grid, which is not this geometry but a worse one: that
marginal is a finite mixture, and repeated comparator event times can
make it diverge where this index geometry is perfectly healthy.
[`.check_comparator_tied_events()`](https://choxos.github.io/mlumr/reference/dot-check_comparator_tied_events.md)
is that question. It runs whatever this function concludes, except that
a SHARED auxiliary this function bounded bounds the comparator's too,
which is what the `bounds_aux` attribute on the return value reports.
