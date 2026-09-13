# mlumr 0.1.0.9000 (development version)

## Behavior and validation changes to existing functions

* **A log-normal or generalized-gamma survival fit whose covariates
  reproduce every event time exactly is now refused too.** The exact-fit guard ran only for
  `family = "normal"`, and a log-normal AFT is a normal model for `log(t)`
  with a positive scale: the same singularity is there. With `n` uncensored
  index rows, a design of rank `r` that reaches every `log(t)`, and the
  coefficients integrated out, the density of `sdlog` behaves as
  `sdlog^(r - n)` near zero and does not integrate for any `n` above `r`.
  `prior_aux` defaults to a half-normal and every supported alternative has
  positive density at zero, so none of them repairs it, and no convergence
  diagnostic can: the sampler drifts toward zero and reports where it
  stopped. `mlumr()` now decides this before dispatch, with the same exact
  geometry the normal guard uses.

  Which families, decided by what the auxiliary *is* rather than by the
  family's name. `"lognormal"` and `"gengamma"` are refused, because for
  both of them the first auxiliary is a scale: the log-scale SD for one, and
  the Lawless `sigma` for the other, which the density divides the log
  residual by and carries a `-log(sigma)` term for. An exact fit sends
  either to zero. The generalized gamma's *second* auxiliary is its shape,
  and it is not what diverges: at an exact fit the density's dependence on
  it is bounded. The Weibull, log-logistic and gamma carry a shape as their
  only auxiliary, so the same exact fit sends it to `+Inf`, where a
  half-normal or exponential prior's tail integrates the growth and a
  half-t's need not. Propriety is then a property of the prior rather than
  of the data, and refusing the data would refuse well-posed default fits,
  so those warn instead. The Gompertz is one of them, and it is read on the
  TIME scale rather than the log-time one the others are: its hazard is
  `exp(eta + shape * t)`, so an exact fit drives the linear predictor to
  about `log(shape) - shape * t`, and that ridge needs the event times
  themselves in the column space rather than their logarithms. With the
  coefficients integrated out the marginal goes as `shape^(n - 2k)`, for the
  `k` coefficients the ridge moves, so a Cauchy on `prior_aux` and on those
  leaves a tail that does not integrate. Five events at `t = 1:5` over
  `x = 0:4` are exactly linear in `x` on the time scale and their marginal
  slope `d log M / d log shape` is 1.000, which a Cauchy `prior_aux` turns
  into `shape^-1`; three events at `t = exp(0:2)`, exactly linear on the log
  scale instead, have no such ridge and measure -577,014 per decade. Each
  censored row is placed in its observation region on the same scale. The
  Gompertz previously received no diagnosis at all.

  A saturated design, with as many uncensored rows as its design has free
  columns, is an exact fit too: it reproduces every event time and leaves no
  residual degree of freedom. It is also the one case where integrating the
  coefficients out cancels the auxiliary's growth exactly, so it is proper
  for every family but two, and it warns instead that nothing in the index
  data separates the auxiliary from the coefficients. The first exception is
  the proportional-hazards Weibull, whose cumulative hazard `t^shape e^eta`
  leaves the width in the location of order one so that nothing cancels: two
  rank-2 rows both at `t = 1` give a profile likelihood of exactly
  `shape^2 e^-2`, with the coefficients held at `eta = 0` rather than moving
  into their prior tails, so a `prior_cauchy()` auxiliary contributing
  `shape^-2` leaves a constant tail that does not integrate. That one keeps
  the prior-tail warning.

  The Gompertz is the second, for a different reason. Integrating one of its
  rows over its own linear predictor gives
  `shape * e^(shape t) / expm1(shape t)`, which tends to the SHAPE rather
  than to a constant, so a saturated design contributes `shape^n` against
  `shape^-2` for each coefficient the ridge moves: the marginal goes as
  `shape^(n - 2k)` and propriety fails once `n >= 2k + 1`, which is not a
  property of `n == rank` at all. Three events all at `t = 1` on a rank-3
  design, whose times are the intercept alone, measure a slope of 1.000, and
  six rows whose times need two of six columns measure 2.000; a Cauchy
  `prior_aux` takes off 2 and leaves `shape^-1` and `shape^0`, neither of
  which integrates. Which coefficients the ridge moves is not decidable at
  double precision, for the same reason exactness is not, so every saturated
  Gompertz takes the prior-tail warning rather than a guess at which ones
  are the proper ones. The exemption belongs to `n == rank` alone: with
  more uncensored rows than the rank the cancellation is partial and every
  shape family is warned about as before.

  The censoring check refuses a design it cannot rescale exactly. Its
  tolerance bounds the solve's error with the COLUMN-SCALED design's
  condition number, which is sound because the two are the same computation:
  Householder QR is equivariant under an exact power-of-two column scaling
  and the pivot test is per-column relative, so over 20,000 random designs
  the rescaled unscaled solve was bit-identical to the scaled one, with the
  same rank and pivots, and the predictor error never reached the tolerance
  when measured against the exact rational solution. A column whose own
  entries span more than the exponent field breaks that, and toward the
  unsafe answer: dividing `c(2^1020, 2^-100, 2^200, 2^-300)` by `2^1020`
  flushes two entries to zero, so the scaled design reads as well
  conditioned (kappa 18.8) exactly because the information is gone, where
  the design solved has kappa 5.5e307. The tolerance from 18.8 is far too
  small and the row came back `"bounded"`, which suppresses the refusal.
  Such a design is now `"undetermined"`.

* **A comparator curve whose event rows outnumber the rank of the
  integration nodes that match them is refused for a log-normal survival fit,
  and warned about for the shape families.** The comparator likelihood is not the continuously
  integrated one the model is written to mean. Each reconstructed
  pseudo-individual contributes `log_sum_exp(ll) - log(n_int)`, a finite
  equally weighted mixture over the integration grid, and every
  pseudo-individual in an arm sees the same grid, so a node reproducing a
  row's event time carries a density spike proportional to one over the
  auxiliary's width.

  What decides propriety is how many spikes stand up at once and what
  coefficient volume that costs. An allocation sends each of the `m` event
  rows to a grid node; its design `D` carries that node's covariate vector
  beside an intercept, one row per event row, and the rows stand on spikes
  together exactly when `D b = targets` is consistent. The exponent is
  `m - rank(D)`, and the rate is the LARGEST of those over the consistent
  allocations, since the marginal is their sum and the smallest rank
  dominates it. What bounds that smallest rank is the CANONICAL allocation,
  which sends every row sharing a target to one node: its design has rank at
  most the dimension the grid reaches, `rank(cbind(1, X_int))`, which is
  `1 + n_cov` for any grid that is not degenerate, and at most the number of
  distinct times. That is a claim about the canonical allocation and not
  about every one, since rows sharing a target can sit at different nodes
  wherever the coefficients are orthogonal to the difference between them,
  which two or more covariates allow; those allocations have higher rank,
  are subdominant, and change nothing.

  All `m` rows are matched on the solution set, so each stands on a spike
  that grows as the auxiliary approaches its boundary, while the set is
  pinned in only the `rank(D)` directions the equations fix and its width
  shrinks in each of those. The rate is the difference, and neither factor is
  shared across families:

  * `lognormal` and `gengamma`: height `1 / sdlog`, width `sdlog`, rate
    `m - rank(D)` as the scale goes to zero. Measured over 20 midpoint normal
    nodes with the coefficients integrated against normal priors,
    `d log M / d log sdlog` is -0.000, -1.000 and -2.000 across `1,4`,
    `1,1,4` and `1,1,4,4`.
  * `weibull-aft` and `loglogistic`: height `shape`, width `1 / shape`, rate
    `m - rank(D)` as the shape grows. `d log M / d log shape` is +0.000, +1.000
    and +2.000 on the same three, for both.
  * `gamma`: height `sqrt(shape)`, width `1 / sqrt(shape)`, so the rate is
    half, `(m - rank(D)) / 2`. For `m` rows on one time the integral is exactly
    `Gamma(m k) / m^(m k) / Gamma(k)^m`, whose slope in `log k` is
    `(m - 1) / 2`: 0.500002, 1.000003 and 1.500005 for `m` of 2, 3 and 4,
    the closed form agreeing with quadrature to 7e-12 at `k` of 10 to 1000.
    Reporting `m - rank(D)` would claim non-integrability against a half-t
    `prior_aux` with degrees of freedom in (0.5, 1) that does integrate it.
  The proportional-hazards Weibull and Gompertz are deliberately not
  examined. Their width does not shrink at all, since `t^shape e^eta` and
  `e^eta expm1(shape t) / shape` both leave a row's curvature at -1 whatever
  the shape is, so their growth is `m` regardless of `k`: measured with the
  coefficient priors out, +2.000, +2.000, +3.000 and +4.000 across `1,1`,
  `1,4`, `1,1,4` and `1,1,4,4`. A repeat is therefore not what causes it, and
  firing on repeats would attribute to ties something they do not do. What
  the growth meets is the coefficient priors, through however many
  coefficients the ridge moves and each of their tails, and settling that
  needs those counts per configuration. It is a different question and is not
  answered here.

  What makes any of these nonzero is more event rows than the matched
  design has rank, which repeated times are the usual but not the only way to
  reach; tied CENSORED times contribute a survival probability rather than a
  density spike and do not count toward it at all. The profile maximum, by contrast, grows in every one
  of those cases including the convergent ones, which is why the volume and
  not the profile is what decides this.

  A censored row in the same arm can suppress the divergence, and it has to
  threaten the ridge before that is worth asking. Every point of the solution
  set puts a MATCHED node exactly at its target, so a row whose region
  probability tends to one there suppresses nothing: its contribution is a
  mixture over the grid, which that one node holds at `1 / n_int` whatever
  the others do. Two comparator events at `t = 1` with a right-censored row
  at `t = 0.5` are that case, and the rate-1 divergence is certified rather
  than left open; the same row at `t = 2` does suppress the matched node and
  only the other nodes are left to settle. The ends are inclusive, since a
  predictor sitting exactly on a censoring time leaves that row at a half.
  That decides at the matched nodes what used to be deferred wholesale, so
  the point-mass grid measured at rate +1.000 with a right-censored row at
  `t = 0.5` is now refused rather than reported.

  A pinned ridge is isolated points too, and those ones can be looked at.
  Under `model = "spfa"` with a shared auxiliary the index's own pinned rows
  fix the slope, so each ridge point's node predictors are determined:
  matching one target at node `j` puts every node at
  `u + (z_l - z_j)' beta`, and a censored row suppresses nothing if ANY of
  those sits inside its region. Index events at `x = -1, +1` with
  `t = exp(-1), exp(1)` pin `beta = 1`, and then a comparator with events
  tied at `t = 1` and right censoring at `t = 2` has a ridge point leaving an
  unmatched node at 2, past `log 2`, so its divergence is certified rather
  than deferred. Only a single distinct target is enumerated: past that a
  point has to match the other targets too, which is an equality between
  computed quantities, and a tolerance there would certify a ridge that does
  not exist.

  For a row that does threaten, whether it suppresses turns on `rank(D)`
  against the reach. Its contribution is a mixture
  over the grid too, so it vanishes only if every node's region probability
  vanishes. Below the reach the ridge has a free direction and pushing it one
  way clears every right-censored row, the other way every left-censored one:
  measured at rate +1.000 for two events at `t = 1` with a right-censored row
  at `t = 2` on 20 nodes.

  Three cases are left undecided instead. At the reach the ridge is isolated
  points and a censored row can cover all of them, so on a point-mass grid
  that same pair collapses while the row at `t = 0.5` leaves rate +1.000, and
  deciding which takes enumerating `choose(n_int, k)` ridge points. And when
  the censored rows bound on both sides, one free direction cannot clear them
  all unless the matched node has unpinned neighbors far enough out on each
  side: with `n_int = 2` a right-censored row at `t = 2` together with a
  left-censored row at `t = 0.5` collapses whichever node is matched, while
  either alone leaves rate +1.000, and the same pair on 20 nodes stays
  divergent at +1.000. And the free direction can be one the comparator does
  not own: under `model = "spfa"` with `aux_by = "none"` that direction is
  the shared `beta`, which an exactly fitting index pins, leaving the ridge
  at isolated points whatever `rank(D)` is. Index events at `x = -1` and
  `x = +1` both at `t = 1` force `mu_index` and `beta` to zero, so every node
  sits at `mu_comparator` and a comparator right-censored row at `t = 2` is
  above all of them; tilting `beta` to lift one past `log 2` costs the index
  a residual of the same order, so the two exponentials trade rather than
  cancel. Such an arm is reported rather than refused, scale family or not.

  Which side a row needs is read from its region and its delayed entry, not
  from its status code, and only the rows that have to be ESCAPED count: one
  already satisfied at a matched node does not need the free direction and
  cannot make the arm two-sided. A right-censored row is satisfied above its time and a
  left-censored one below its bound, including below its entry, since
  conditioning on survival to the entry piles the mass just above it and that
  pile lies inside the region. An interval-censored row is two-sided only when
  it opens strictly above its entry; one that opens AT its entry has that pile
  inside it and is one-sided like a left-censored row, so such an arm keeps
  the refusal.

  Distinctness is counted on the scale the density matches, which for every
  family this examines is `log(time)`. Two distinct doubles can share a
  logarithm, and counting raw times reads one target as two, so `k` came out
  too large and the check returned silently on a curve whose spikes all
  collapse onto one predictor. The
  grid's reach likewise uses the exact rank rather than `qr()`'s default
  tolerance, under which independent but badly scaled columns read as
  deficient while the direction is still there.

  Past the grid's reach there may be nothing to refuse, and this is
  deliberately narrow about which case it is in. With `k` above
  `rank(cbind(1, X_int))` the `k` equations need not have a solution, and
  where they do not the best simultaneous match leaves a residual `d > 0`,
  and the profile collapses like `exp(-d^2 / (2 aux^2))` once the auxiliary
  falls below `d`. What happens before that looks exactly like a divergence
  and is not one: three distinct times over 20 nodes leave `d = 5.99e-4` and
  the profile peaks between `sdlog` of 1e-3 and 1e-4 before falling to
  -1.8e7 by 1e-7, while the same times over 64 nodes leave `d = 3.62e-5`,
  peak at 1e-5 instead, and collapse from 1e-6 on. A finer grid moves the
  collapse out rather than removing it, and the posterior is proper either
  way, so an ordinary reconstructed curve with many distinct times and one
  rounding tie is not refused. A slope measured over any fixed range of the
  auxiliary cannot tell the two apart, which is why the test is structural.

  A count is not that structure, though. Distinct response values are not
  independent linear constraints, and an overdetermined system can still be
  consistent, so neither an absence of repeats nor more distinct times than
  the reach establishes `d > 0`. Comparator events at `t = 1, 2, 4` on the
  nodes `1, 2, 3` that `add_integration()` really builds for a uniform
  covariate are three distinct times with no repeat, past a reach of 2, and
  are matched exactly by `b = (-log 2, log 2)`: rank 2 against 3 rows, and a
  measured slope of -1.0000 per decade of scale. With one covariate the map
  is a line that two (target, node) assignments fix, so node pairs are
  enumerated and this case is refused; wider designs are left alone, and a
  grid too large to enumerate is REPORTED rather than left silent. The
  enumeration anchors the first target at each node and runs the rest as a
  vectorized pass, so it costs `n * n * (k - 2)` rather than the
  `n * n * (n + k)` of a scalar inner loop, and its budget covers the
  ordinary resolutions; past that the same three events over the same
  declared covariate would be refused at one `n_int` and unexamined at
  another, which is what the warning names. Consistency is read off the
  determinant of the
  original data, `(u[i] - u[1]) * (z[j2] - z[j1]) - (u[2] - u[1]) *
  (z[j] - z[j1])`, which has no division and no slope in it, and only an
  exact zero certifies: a residual that is merely small is a near miss no
  affine map removes, and it reports undecided. A COMPUTED zero is not an
  exact one either, since both products round before the subtraction, so the
  zero counts only when every step that produced it was itself exact, which
  is checked with the error terms of the sums and products. Nodes
  `(0, 0.3961039261018525, 1.04621481495181)` against targets
  `(0, 0.6209825942831111, 1.6401786176669797)` compute a determinant of 0
  while the determinant of those very doubles is -3.4958e-17, and no
  permutation of them is an affine match. Silence from this check is not a certificate
  that the posterior is proper. A refusal is a certificate that it is not.

  The exponent used is `m - min(k, reach)`, which is exact for one covariate
  and a lower bound for more than one. `min(k, reach)` bounds the canonical
  allocation's rank, and a consistent allocation of lower rank gives a larger
  exponent: among
  two covariates, three collinear nodes carry three distinct targets affinely
  along that line at rank 2 rather than 3. A refusal is therefore still
  certified, since a positive lower bound is a positive rate, while a skip
  may be hiding one. Searching for a lower-rank consistent allocation among
  two or more covariates is not attempted.

  What the rate decides differs too. The scale families diverge as the scale
  goes to zero, where every supported prior has positive density, so no
  prior repairs it and the fit is refused. `weibull-aft` and `loglogistic`
  diverge as the shape grows, where the rate meets `prior_aux`'s tail, so a
  half-normal or an exponential integrates it and a half-t need not, and
  that is reported rather than refused. `gamma` is reported on a different
  pair: its ridge also displaces the comparator intercept by `-log(shape)`,
  and a normal `prior_intercept` contributes `exp(-(log shape)^2 / 200)` at
  the default width, which integrates any polynomial, so the posterior
  exists and the shape concentrates far out. It is a heavy-tailed intercept
  prior that leaves `prior_aux` to integrate the growth, which a half-t does
  for degrees of freedom of at least `(m - rank(D)) / 2`. Equality integrates
  rather than failing: the auxiliary's `shape^-(df + 1)` meets the Student-t
  intercept's `(log shape)^-(df + 1)` on the `-log(shape)` ridge, and
  `1 / (shape * (log shape)^(df + 1))` integrates for every supported
  intercept prior.

  One combination is reported rather than refused for a reason unrelated to
  censoring. Under `model = "spfa"` with `aux_by = "none"` the arms share one
  `beta` as well as one auxiliary, so reaching the comparator check means the
  index did not bound that auxiliary, which under that model means its own
  design fits exactly. That is not enough on its own: it must also CONSTRAIN
  the shared slope somewhere in the directions the arm's grid spans, since
  repeated index events at one covariate profile at one time fit exactly and
  leave `beta` wholly unconstrained, and then whatever node-specific values
  the comparator's equations pin it to lie in the index's solution set by
  construction, the sets always intersect, and the arm is refused rather than
  reported. Partial identification is not that case and stays reported: an
  index that fixes `beta1` at a value none of the comparator's pairwise
  differences reaches leaves the sets disjoint even while `beta2` is free.
  The pinned-ridge report wants the opposite of the same measurement, since
  one free direction is enough to move an integration point past a censoring
  time, so it asks for every spanned direction rather than for any. Two or
  more comparator
  targets pin it too, to values the integration points fix, and if those sets
  do not intersect then every path to the boundary leaves one side with a
  positive residual whose decay beats the other's growth. Solving that
  combined system is out of scope, so the case is warned about. A single
  distinct target is not that case: its one equation is absorbed by the free
  `mu_comparator`, so the shared slope stays free BY THE EVENTS and both
  singularities stand at once. That reasoning covers the event rows only. A
  censored comparator row cannot be escaped either once the index pins that
  slope, so a single target WITH a censored row in the arm is the
  isolated-ridge report above and only one without is still refused.
  Neither is an index that never had an
  exact design: failing to bound the auxiliary does not imply one, and an
  index of nothing but right-censored rows pins no slope at all, its
  `mu_index` rising above every censoring time so that its likelihood tends
  to one while the comparator divergence is left whole. The
  index guard now reports what it established in an `index_exact` attribute
  and the comparator check reads it. That attribute is three-valued: `FALSE`
  only where the index was shown to pin nothing, `TRUE` where its event
  design reproduces its own times, and `NA` where the question was not
  settled. `undecidable` and `unresolved` leave it `NA` rather than `FALSE`,
  since those do not establish a positive residual and the index may yet be
  exact with a solution set the comparator's slopes miss, which is the same
  proper configuration the branch reports.

  An index with no events at all is not automatically the `FALSE` of those
  three. Having no events means there is no design to fit, not that nothing
  bounds the auxiliary: censored rows alone can bound it, and conflicting
  ones do. An index carrying one row left-censored at `t = 1` and another
  right-censored at `t = 4` on the same covariate profile has no linear
  predictor satisfying both, `sup_mu L = Phi(-log(4) / (2 sdlog))^2` falls
  faster than the comparator's `sdlog^-2` grows, and the shared-scale fit is
  proper. So an eventless index is answered by asking whether any linear
  predictor satisfies every one of its observation regions at once: rows at
  one covariate profile share a predictor, so their regions must overlap, and
  a certified conflict reports the bound while a certified absence of one
  reports that the index pins nothing. Anything else is left undecided. The
  overlap is tested by exact ordering: these ends are stored observation
  times rather than the output of a solve, and a gap of any positive size
  bounds, since ends `d` apart contribute `exp(-(d / (2 sdlog))^2)` and
  `integral sdlog^-m exp(-(d / (2 sdlog))^2)` converges at zero for every
  `d > 0`. Exact equality is not a conflict, and it is not freedom either:
  the shared predictor has to sit ON that point, so the coefficients keeping
  the group's likelihood positive are a shrinking neighborhood of a
  hyperplane rather than an open region. A left-censored row at `t = 1`
  beside a right-censored row at `t = 1` on one profile peaks at `1/4` at
  every scale POINTWISE, while integrating the intercept out against
  `normal(0, a)` gives `arccos(a^2 / (a^2 + s^2)) / (2 pi)`, or
  `s / (sqrt(2) pi a)` near zero: one power of the scale, not a constant.
  Measured `d log L / d log s` is 1.000000 for one such profile, 2.000000 for
  two independent ones and 3.000000 for three. Both sides are written in
  powers of the same width, so those come off the comparator's rate directly
  and the eventless index reports an ORDER rather than a flag: two tied
  comparator events against one touching profile is `1 - 1 = 0` and stands,
  three is `2 - 1 = 1` and is still refused. The order is carried only for
  `lognormal`, where it was measured; the other families report the question
  as unsettled rather than refusing on an unmeasured exponent.

  It also only comes off a rate that is EXACT, which is a property of the
  RANK rather than of the covariate count. A consistent allocation's design
  has rank at least 1, and at least 2 whenever two targets differ, since its
  rows all carry an intercept and proportional rows there are identical rows,
  which put every row on one predictor and make every target equal. So a
  recorded rank of 1 or 2 is the smallest achievable one however many
  covariates are declared, and only from 3 can a lower-rank allocation exist.
  Taking a positive order off a rate that IS a bound can cross the refusal
  threshold from the wrong side: four
  comparator events at three distinct targets carried by three collinear
  nodes have a true rate of `4 - 2 = 2` while the recorded one is
  `4 - 3 = 1`, and netting one power off that reads as zero. A subtraction
  that LEAVES the rate at or above one is still certified, since the true net
  is at least the reported one; only one that takes it below is reported.

  The subtraction is valid only where the two sides pin INDEPENDENT
  directions, which is a property of the model. Under `relaxed` the index
  constrains `mu_index` and `beta` while the comparator constrains
  `mu_comparator` and `beta_comparator`, so the stacked system is block
  diagonal and the rank is exactly the sum. Under `spfa` the arms share
  `beta` and the blocks can overlap: two independent touching index profiles
  give order 2 while four comparator events at two matched times give rate 2,
  and if both sides pin the shared slope the stacked rank gains only one
  index direction, leaving a true rate of 1 that a full subtraction would
  report as 0. Computing the joint rank means solving the combined system
  across every allocation, which is out of scope, so a shared slope is
  reported instead of netted. Only from order TWO, though: in
  `(mu_index, mu_comparator, beta)` an index constraint is `(1, 0, x)` while
  every comparator constraint is `(0, 1, z)`, so no combination of comparator
  rows reaches a nonzero first component, a single index row is independent
  of all of them, and the ranks add whatever the shared slope does. It takes
  a second index row for the difference `(0, 0, x_1 - x_2)` to appear, which
  is a pure slope direction and can lie in the comparator's span. It also
  needs somewhere to lie: a matched design of rank 1 is one row,
  `(0, 1, z_j)`, whose only vector with a zero second component is the zero
  vector, so nothing of the form `(0, 0, v)` is in it and the ranks add
  again. Four comparator events tied at one time are rate 3 against two
  touching index profiles' 2, and that nets to 1 rather than going
  unresolved.

  Those touching rows also PIN, exactly as an exact event design does, and
  the comparator has to test against them: left and right censoring meeting
  at `t = 1` on `x = -1` and `x = 1` forces `mu_index` and `beta` to zero
  just as two events there would. Reporting that the index pinned nothing
  left the comparator treating the shared slope as free, so its censored rows
  read as escapable and a fit they exponentially suppress was refused. The
  eventless path now carries that design, and the pinned-ridge report asks
  whether the index pins the slope rather than whether its EVENT design fits
  exactly.

  Reproducing its own times is also not the same as IDENTIFYING that shared
  slope, and the report needs the second. Repeated index events at one
  covariate profile at one time are `constant`, fit exactly, and pin only
  `mu_index`: `beta` stays free, and a free `beta` is the direction the
  comparator tilts along to lift an integration point past a censoring time,
  so that arm keeps the refusal. What has to be identified is only the slope
  directions the COMPARATOR's grid spans, since the escape is a change in the
  node linear predictors and that is `(z_j - z_1)' beta`: a covariate the grid
  integrates as a point mass contributes no such direction, and leaving its
  coefficient unidentified costs the comparator nothing. The index guard
  therefore hands its event design over rather than reducing it to a verdict,
  and the comparator tests estimability of exactly the node-difference
  directions. Centering does not affect the answer, since it leaves the slope
  coefficients unchanged and shifts node differences by nothing.

  An index WITH events carries the same order, on the same distinction. A
  design shown to reproduce its own times pins rather than suppresses, so it
  reports zero and the comparator refusal stands, while `undecidable`,
  `unresolved` and `unresolved_log` did not settle whether a residual exists
  at all; a real one there contributes `exp(-RSS / (2 sdlog^2))` and removes
  the comparator's growth entirely, so those report the question as unsettled
  rather than a zero that would turn it into a refusal.

  Exactly integrating a declared Gaussian covariate is a different model
  rather than a guaranteed repair, and how far that goes is worth being
  precise about. It leaves `log T ~ N(mu, beta^2 + sdlog^2)`, and with
  `mu ~ N(0, a^2)` integrated out, `m` events tied at one time give
  `(2 pi)^(-m / 2) tau^(1 - m) / sqrt(tau^2 + m a^2)` for
  `tau^2 = beta^2 + sdlog^2`. That behaves as `r^(1 - m)` near the origin
  against the plane's own `r dr`, leaving `integral r^(2 - m) dr`: finite for
  two tied events and divergent from three on. So for two the quadrature is
  what creates the singularity and exact integration removes it, while for
  three or more the exactly integrated model is improper as well. Nothing
  here establishes the question for the other families or for other covariate
  distributions, and they are no longer told it holds for them. Two comparator events at `t = 1`
  on 64 nodes, coefficients integrated against `normal(0, 10)` and
  `normal(0, 2.5)`: the grid likelihood runs 0.0143, 0.185, 1.72, 171 and
  17103 as `sdlog` falls through 0.1, 0.001, 0.0001, 1e-6 and 1e-8, while
  the continuous one runs 0.0142, 0.0307, 0.0390, 0.0556 and 0.0721. So a
  larger `n_int` is not the repair: within the grid's reach a bigger fixed
  rule is still a finite mixture and only scales the coefficient of the same
  divergence. Neither is jittering the tied times. Where ties come from
  rounding, an interval-censored representation of what was actually observed
  is the honest model and `set_agd_surv()` accepts one. The index-side guard
  says nothing about the rest of this: the refused configuration has a
  perfectly healthy index fit.

  Under `aux_by = "none"` the index rows share the auxiliary, and an index
  fit that leaves a real residual contributes `exp(-RSS / (2 sdlog^2))`,
  which goes to zero faster than any power and removes this divergence; so
  does an index censored row that bounds. Sharing does not do it on its own,
  so the comparator check is skipped for a shared auxiliary only where the
  index guard established the bound, and not where it merely warned about an
  exact or saturated index.

  Right-censored rows are consulted before any of that is said, for the
  shape families too: one whose fitted time falls below its censoring time
  has survival going to zero faster than any power of the scale, and
  `exp(-(c e^-eta)^k)` goes to zero as `k` grows for exactly the same rows,
  so either way the posterior is proper and nothing is said, including the
  near-exact and saturated messages. One at or above its censoring time does
  nothing. A censored row's predictor counts as determined when its
  covariate vector lies in the row space of the event design, which is
  weaker than every coefficient being identified: exact events and a
  censored row at the same covariate profile fix that row's predictor
  however deficient the design is. When it is not determined the question is
  said to be undecided rather than guessed.

  Under `aux_by = "none"` the comparator rows share the auxiliary. That does
  not bound it on its own, since a comparator of right-censored rows whose
  fitted times sit above their censoring times contributes a likelihood
  tending to one at the boundary, and whether it does bound it belongs to
  the marginalized aggregate likelihood, which this geometry does not see.
  So that case warns and is not refused. A fit that is nearly rather than
  exactly exact warns that the auxiliary will concentrate against its
  boundary, as the normal guard warns about sigma; under the
  proportional-hazards Weibull and the gamma it says the auxiliary may
  rather than will, because the ridge moves the coefficients there and an
  ordinary `prior_beta` or `prior_intercept` can stop the shape before the
  residual does.
  Every censoring type is examined, through the OBSERVATION REGION each row
  is known to lie in on the log scale. A right-censored row runs from its
  own time upwards with no upper end; a left-censored one runs up to its
  own time with no lower end; an interval one runs between its two. As the
  auxiliary goes to its boundary the fitted distribution concentrates at the
  fitted value, so a row's contribution tends to one when that value is
  strictly inside its region and to zero when it is strictly outside, and
  only the second bounds the auxiliary.

  A delayed entry is not a lower end of that region. It conditions the
  observation on survival to it, and with the fitted value BELOW the entry
  the conditional law piles up just above the entry, so the probability
  tends to one rather than to zero. An interval that opens strictly above
  its entry does still bound from below, since the pile is then outside it.
  So neither delayed entry nor a left-censored row whose upper bound sits
  above the fitted time rescues an exact fit. The comparator side is not
  examined.

* **A normal fit whose covariates reproduce the outcome exactly is now
  refused.** Integrating out the coefficients leaves a marginal density for
  the residual SD proportional to `sigma^(rank - n)` near zero, which does not
  integrate for any `n` above the rank; a prior with positive density at zero
  leaves that divergence where it is, and proper coefficient priors only scale
  it. The posterior is improper, nothing reported it, and the sampler drifted
  toward zero and returned where it stopped with ordinary-looking diagnostics.
  `mlumr()` now decides before sampling whether an exact fit exists. Where the
  data settle it structurally they are believed: a constant outcome, or one
  with only as many distinct covariate profiles as the design has rank and
  agreeing replicates, is fitted exactly and refused; replicate profiles with
  different outcomes prove the residual positive, whatever a fit reports.
  Otherwise the residual sum of squares is compared with the rounding an exact
  fit can leave, `p * eps * |X||b|` elementwise, which grows with the fitted
  coefficients. That bound is used only as a bound: a residual above it is
  real, and a residual at or below it is refused as undecidable at double
  precision, not declared improper. A proper posterior whose residual is at
  most `1e-6` of the outcome's total is warned about, since the residual SD
  will concentrate near zero and the sampler has to work there; the sampler's
  own diagnostics say how it went. Under `link = "log"` existence is decided
  on `log(y)`, a linear question that cannot overflow however wide the outcome
  is, and the near-exact screen is taken on the response scale the likelihood
  uses. Zeros under `link = "log"` are the boundary case: a positive mean
  can only approach them as their linear predictor goes to `-Inf`, where the
  likelihood grows without bound as the residual SD shrinks and only the
  coefficient priors' tails decide whether a posterior exists. An outcome
  identically zero is refused, and so is one whose positive rows are fitted
  exactly while leaving a direction of the coefficients free to take the zero
  rows there; zeros beside positive rows that leave a real residual, or that
  pin every coefficient, pass, and a negative outcome anywhere settles it. A
  zero row is pinned only when it lies exactly in the span of the positive
  rows; one merely within rounding of that span is refused as undecided.
  The check judges the design the model fits, with the model's own centers
  or none, and its rank is that design's EXACT rank, computed in exact
  arithmetic: a covariate that differs from a combination of the others by
  less than rounding is still a column of the model, and an outcome can be
  reproduced exactly through it with enormous coefficients where a
  factorization at machine precision, having dropped the column, shows an
  ordinary residual. The structural rules see that with the exact rank and
  refuse it as an exact fit; otherwise such a design is refused as
  unresolved rather than passed on the reduced fit. With `center = FALSE`
  the rounding bound
  carries the cancellation of the raw predictor offsets, as the likelihood
  then does, so a residual below that rounding is refused as undecidable
  where the centered fit would only warn. A saturated design is warned
  about rather than refused: its posterior is proper, but nothing in the
  data separates the residual SD from the coefficients, so the estimate of
  sigma is potentially strongly sensitive to the coefficient priors. The
  check runs before any model is compiled or sampled.
* **Posterior summaries now carry how many draws they used.** The summaries
  behind `predict()`, `marginal_effects()`, `conditional_effects()` and
  `conditional_predict()` pass `na.rm = TRUE`, which is right: one bad draw
  should not erase an otherwise usable summary. But they removed those draws
  without a trace, so a mean taken over a third of the chain printed exactly
  like a mean taken over all of it and nothing downstream could tell the two
  apart. Every summary row now has `n_draws` and `n_draws_used` columns, so
  the accounting travels with the result through `saveRDS()`, a report, or a
  table, and a warning at the call names how many quantities were affected
  and the worst loss, once per call rather than once per column. A median
  survival the grid never reaches is not a lost draw and keeps its own
  `p_not_reached` diagnostic, with `n_draws_used` showing what the summary
  rests on. Only `NA` and `NaN` are counted, since those are what `na.rm`
  removes; an infinite draw propagates into the mean and announces itself.

* **A convergence diagnostic that cannot be computed is no longer reported as
  a good one.** An Rhat of `Inf` is a parameter whose chains did not mix at
  all, and it was filtered out before the maximum was taken, so a fit holding
  `1.001` and `Inf` reported a maximum Rhat of 1.001 and raised no warning.
  Infinite values now reach the worst-case statistic, in `check_diagnostics()`
  and in the printed fit summary alike. A genuinely missing value is counted
  and reported instead of being dropped from a statistic that calls itself the
  maximum, and the report names the parameters it could not check. It does not
  name a cause: a constant generated quantity has no Rhat, and neither does a
  parameter whose chains are each stuck at a different constant or whose draws
  are not finite, and nothing here has looked at the draws to tell those apart.
  A column that is absent, or present but not numeric, is counted the same way.
  `c(NA, NA)` is a logical vector in R, which is what a backend writes into a
  column it never filled, and reading it as zero diagnostics rather than as two
  missing ones meant the summary printed no line at all. Divergence and
  treedepth counts the backend did not supply were read as zero, which is the
  answer that says the sampler behaved; they are now reported as unknown, as is
  a count that is not a whole number or is past the integer range, both of
  which `as.integer()` had been turning into a clean zero or a silent `NA`.

* **`mlumr_forest()` takes its null from the effect rather than from the
  axis.** The reference line defaulted to `1` when `log_x = TRUE` and `0`
  otherwise, so a hazard ratio, risk ratio or RMST ratio drawn on a linear axis
  got a null line at 0, which is not a value those measures can take. When the
  frame carries an `effect` column the null is now read from it through the
  same resolver the package's own forest method uses, so both figures agree.
  Without that column the axis remains the only hint and the previous default
  applies.

* **`set_agd()` no longer rejects a valid binary covariate's standard
  deviation.** The check compared a reported SD against `sqrt(p * (1 - p))`,
  which is the POPULATION standard deviation of a Bernoulli variable. A sample
  standard deviation uses the n-1 denominator and equals
  `sqrt(n / (n - 1) * p * (1 - p))`, so it is always larger: five zeros and
  five ones report a mean of 0.5 and an SD of 0.5270, and that was refused as
  impossible. The bound is now the finite-sample maximum at `n = 2`, the
  loosest factor any sample can have, with an allowance for rounding. It does not tighten with the outcome sample size,
  because that count is not the covariate's denominator: a covariate carrying
  its own missingness was summarized over fewer rows, and fewer rows make the
  bound looser rather than tighter. Genuinely inconsistent summaries are still
  refused.

  The rounding allowance is half a unit of the coarsest decimal grid the stored
  value lands on, and it does not claim to be the precision the figure was
  reported to. `0.1`, `0.10` and `0.100000` are one double in R, so nothing can
  be scanned out of the value to say which was printed, and all three get the
  same allowance. That makes the check deliberately lenient, in the direction
  that matters for reading published tables: it will not refuse a valid summary
  for having been rounded, and it may accept a mean and SD pair that a more
  precise report would have ruled out. A pair no rounding can reconcile is
  still refused.

* **`dlogitnorm()` rejects arguments it cannot use.** `plogitnorm()` and
  `qlogitnorm()` pass `...` to `pnorm()` and `qnorm()`, so a misspelled name
  reaches those functions and errors. The density computes its own value and
  silently discarded whatever `...` collected, so `dlogitnorm(0.5, lgo = TRUE)`
  returned the natural-scale density and looked like an answer. Unused
  arguments are now named in an error.

* **`conditional_effects()` explains an `hr`/`tr` refusal without a false
  claim about the model.** The messages said that a proportional-hazards model
  has no constant time ratio and that an accelerated failure time model's
  hazard ratio varies with time. Neither is true of the exponential or the
  Weibull, which are both proportional-hazards AND accelerated failure time:
  with a baseline shape shared across arms each has a constant hazard ratio and
  a constant time ratio, related by `TR = HR^(-1/shape)` (`1/HR` for an
  exponential). The refusal now describes the parameterization the fit
  estimates, gives that conversion for the dual families, and reserves the
  time-varying explanation for the log-normal and log-logistic, where it holds.

* **`prior_sensitivity()` says what it cannot reproduce.** `mlumr()` forwards
  `...` to the sampler, so a fit could have run with a non-default `thin` or
  `init` that nothing recorded, and the refits then silently used the defaults.
  The names of those arguments are now stored with the fit, and a refit that
  cannot replay them warns and names them.

* **`naive()` no longer reports a Cox comparison that has no maximum, or one
  that never converged.** Events in both arms are necessary for the partial
  likelihood to identify the treatment coefficient and are not sufficient: the
  likelihood can be monotone, with no interior maximum, while `coxph()` stops
  on its convergence criterion and returns finite numbers anyway. Six
  uncensored subjects with the three index events all before the three
  comparator ones give a coefficient of 21.9 with a standard error of 24795,
  and `coxph()` says so in a warning that nothing read. That warning is now
  inspected and the comparison refused.

  What makes the likelihood monotone is the risk sets, not the order of the
  event times. Ordered events are enough only when censoring leaves nobody from
  the earlier arm at risk when the later arm fails: with an index subject
  failing at 1 and censored at 4, and a comparator failing at 2 and censored at
  3, every index event still precedes every comparator event and the maximum is
  a finite log hazard ratio of 0.347. Nothing refuses that fit, and the
  explanation no longer claims the ordering alone is the problem.

  A failure to converge is not an ordinary warning either. `coxph()` documents
  several termination conditions and states that its own detection of an
  infinite coefficient is not always successful, so the absence of the monotone
  warning is not a certificate that a finite maximum exists. Nonconvergence was
  reissued to the caller and the coefficient then packaged with a Wald
  interval; it is now refused. Any other `coxph()` warning is still passed
  through unchanged rather than turned into a rejection.

* **STC detects quasi-complete separation when \pkg{detectseparation} is
  installed.** The existing screen requires every fitted probability to sit at
  0 or 1, which complete separation produces and quasi-complete separation does
  not: rows on the separating hyperplane keep fitted probabilities of exactly
  0.5, so a fit with an infinite maximum likelihood estimate passed with
  `converged = TRUE` and finite coefficients. Whether a finite maximum exists
  is a linear program rather than a threshold, so the exact test lives behind a
  new **Suggests** dependency and runs when it is available. A check that
  cannot be completed is treated as unknown rather than as separated, which is
  right, and it is no longer treated as a clean bill either. The result is a
  status of `"separated"`, `"not_separated"` or `"unknown"` with a reason, and
  an unknown one now warns: the estimate is still returned, but it says that
  only the fitted-value screen ran, that the screen cannot see quasi-complete
  separation, and that the interval is therefore unverified. Previously every
  way of not knowing, an absent dependency most of all, took the same path as a
  fit that had been checked and cleared.

* **Directly observed arms get exact intervals.** The proportions and
  rates that `naive()` reports for each arm, and the observed comparator
  proportion in a binomial `stc()`, had Wald intervals around a
  boundary-corrected standard error, bounded to the parameter range. At 0
  events of 100 that interval ended at 0.0138, and enumerating every count
  put its coverage of a true probability of 0.014 at 75.5%, of 0.02 at
  86.6%: the zero-count outcome alone excludes the truth. They are now the
  exact Clopper-Pearson and Garwood intervals `binom.test()` and
  `poisson.test()` report, whose coverage is at least the nominal level for
  every true value; a comparator built from several aggregate rows is
  pooled, which is conservative for the size-weighted mean of its strata.
  The arm standard errors are unchanged and still feed the contrasts, which
  are not exact and are not claimed to be: the link-scale contrast and the
  log risk ratio remain Wald around the boundary-corrected quantities and
  the risk difference remains Wald on the natural scale around the raw
  difference of proportions. Twelve configurations at 100 observations per
  arm are pinned in the tests by enumerating every pair of counts, and the
  documentation now reports those twelve as the values at those true
  probabilities rather than as a range: they run from 0.853 to 0.9999. The
  risk difference is worst between opposite boundaries, where a true
  difference of 0.966 is covered 85.3% of the time, and the log risk ratio
  is worst with both arms near the same boundary, where 0.986 against 0.957
  is covered 92.1%. The standardized index probability
  of an STC is a model prediction and keeps its delta-method interval,
  documented as an asymptotic approximation.

* **Binomial STC uncertainty no longer depends on the units of a covariate.**
  The delta-method gradients of the standardized event probability, its
  logarithm and its link-scale value were central differences in coefficient
  space with a step proportional to `max(1, |beta|)`. That step is not a
  property of the model: multiply a predictor by 1e6 and its coefficient
  shrinks by 1e6 while the step stays near 6e-6, so the perturbation moved
  the target linear predictor by about 6, and a comparator probability of
  0.75 on 40 subjects at the observed profile reported a standard error of
  0.032 at units 1e6 and 0.019 at 1e8 instead of the 0.068 the data give.
  The gradients are now analytic, `sum(w_i p_i'(eta_i) X_i)` and its chain
  rules through the link, formed on the log scale so a grid point in either
  tail keeps its share, and they transform with the design: equivalent
  units give identical uncertainty for every binomial link.

* **`check_integration()` judges a binary margin against its distribution,
  and says which correlation pairs it measured.** The declared-target SD of a
  binary covariate was read from a supplied `_sd` column when the AgD had
  one. That column is a sample SD of the source data: two zeros and two
  ones have SD 0.577, while the Bernoulli(0.5) distribution has SD 0.5 and
  the largest sample SD any binary grid of `m` points can reach is
  `sqrt(m / (m - 1)) / 2`, so the grid read as 13% off at every resolution
  and the target verdict never left `review`. The target SD of a binary
  margin is now `sqrt(p * (1 - p))` from the declared mean, and grid SDs
  are population SDs, since a deterministic grid is not a sample. On the
  joint side, the maximum discrepancy was taken over the pairs with a finite
  realized correlation, so a variable constant on the grid dropped its pairs
  and one measured pair out of three was summarized as `close`. The
  correlation verdicts are now `"partial"` when the measured pairs pass but
  some pair with a correlation to realize could not be measured, and
  `"review"` whenever a measured pair misses the heuristic, whatever else
  is missing. A new `correlation_pairs` component counts the pairs
  expected and measured, on the doubled grid and between resolutions, and
  names the omitted ones with a reason; a margin declared with no variance
  has no correlation to realize, so its pairs are listed separately and
  kept out of the count, while a rare variable the grid never varied is a
  resolution failure.

* **A Poisson STC with no finite maximum likelihood estimate is refused.**
  The Poisson log-likelihood is `sum(y * eta - E * exp(eta))` up to a
  constant, so along a direction of the coefficients that leaves every
  positive-count row's rate fixed and lowers a zero-count row's rate, it
  increases toward a supremum it never attains: with no events at all, or
  with a subgroup that has none while another has some. The likelihood
  itself is bounded, by 1 when there are no events; what is missing is a
  finite coefficient that maximizes it. Iterative reweighting stops anyway when the
  deviance stops changing, and reported convergence, finite coefficients
  and a finite covariance from where it stopped: 80 zero counts gave an
  intercept near -27 with a standard error near 57,500, and a zero-event
  subgroup beside a positive one a slope near 21. The binomial separation
  check does not apply there and said so, which was not a certificate.
  `stc()` now refuses a Poisson outcome model with no events, and one where
  such a direction exists, deciding the latter exactly for up to two free
  directions with the same feasibility test the normal guard uses and
  refusing what it cannot decide. A returned Poisson result records on its
  `separation` component that the check ran and the maximum is finite. The
  variance floor that stood in for a zero-event index arm is gone with it,
  since the fit it patched is no longer returned.

* **M-spline basis support is judged over the period a study was at risk.** The
  check evaluated each basis column on `[0, max(time)]`. A column supported
  only where nobody is under observation multiplies no event hazard and no
  exposure increment, so its coefficient is moved by the prior alone, yet it
  counted as supported. Support is now evaluated over the merged union of the
  study's per-subject `[entry, exit]` intervals, and strictly inside them: that
  excludes the stretch before the earliest entry under delayed entry, and also
  any gap in which the risk set is empty, which a single span from first entry
  to last exit would have treated as observed. Every path that builds a basis
  passes those times, including user-supplied per-study knots and the shared
  baseline. Where entry is delayed, a message also records which stretch of the
  curve nobody was at risk over. It no longer calls that stretch prior-driven:
  a basis column straddling the entry time is one parameter governing both
  sides, so the observed part informs the unobserved part and the hazard below
  entry is extrapolated under the spline restrictions, with the prior deciding
  whatever those leave weakly determined. Absolute survival and RMST integrate
  from 0 and so depend on it; conditioning on survival to a landmark cancels
  the pre-landmark cumulative hazard, which is not the same as being free of
  the smoothing prior or of shape uncertainty.

* **A shared baseline whose studies never overlap on a spline column is
  refused.** Column support says every column carries likelihood for SOMEBODY.
  It cannot say the studies are tied to each other, and with `aux_by = "none"`
  they have to be: that model has one weight simplex and one intercept per
  study, so if the studies' observed exposure falls on disjoint sets of
  columns, mass can be moved between the sets and absorbed exactly by the
  intercepts. With a piecewise-exponential baseline on `[0, 3]` split at 1, the
  index study observed on `[0, 1]` and the comparator on `[2, 3]`, replacing
  the weight `w` by any other value in (0, 1) and shifting the two intercepts
  to match leaves every likelihood term identical while the conditional hazard
  ratio moves from 1 to 3. Every column is supported, so nothing objected.
  `mlumr()` now also requires the studies and the columns they touch to form
  one connected component, and says which studies are cut off from which. This
  is exact at degree 0, where columns have disjoint supports. Above it the
  supports overlap, so connectivity rules out this failure mode and is not a
  proof of identification.

* **Interval- and left-censored likelihoods under delayed entry are evaluated
  in whichever form the numbers survive.** The interval branch formed the
  unconditional interval probability and then subtracted `log S(entry)`. That
  is correct algebra and poor arithmetic: both terms grow without bound in the
  tail, so the subtraction cancels the significant digits and yields `NaN` once
  either underflows. Rebuilding it from increments as
  `log S(lower)/S(entry) + log[1 - S(upper)/S(lower)]` fixed that end and broke
  the other, where survival rounds to exactly 1 and every increment collapses
  to zero. A Gamma baseline with shape 10, entry at 0.025 and an event in
  (0.05, 0.1] has a conditional log probability of -38.222; built from
  increments it came back unusable, and left censoring under delayed entry took
  the same route. Each route is now used only where its rounding is well
  below the interval's mass, and neither is used at all where a closed form
  exists: the exponential, Weibull and Gompertz families take analytic
  cumulative-hazard differences in every regime, and the log-logistic has an
  exact expression of its own. For the log-normal, gamma and generalized gamma
  the CDF difference serves the lower half and the survival increment the
  upper, each behind a resolution test, and an interval neither resolves is
  integrated from the density by Simpson's rule in log time, refined until two
  estimates agree. A difference that is merely finite is not thereby accurate:
  four ULPs from 0.1 came back 16% low from one difference and 29% high from
  the other, and a midpoint density times the width, the earlier fallback,
  was 57% low on a wide interval whose mass is small because the density falls
  a hundredfold across it.

* **`compare_models()` no longer reads a standard error as a threshold, and
  refuses fits built on different observations.** The LOO/WAIC printout said
  that `se_diff > 2` is the conventional threshold for a meaningful difference.
  A large standard error is uncertainty about a difference, not evidence for
  it; the paragraph now says to read `elpd_diff` against `se_diff`, to treat
  any ratio as a heuristic rather than a decision rule, and to check the PSIS
  diagnostics. Every comparison the function makes is also paired, column by
  column, and `loo` can only check that the pointwise matrices have the same
  shape: two fits of different data with the same number of rows, or of the
  same rows in a different order, compared without complaint. The fits carry
  the data they were built from, so the columns that define an observation
  (`.study`, `.trt`, the outcome, exposure, and for survival the times and
  status, and for survival comparators both the aggregate rows with their
  covariate summaries and the reconstructed pseudo-individuals), together with
  every covariate the fits share, are now compared across the fits row for
  row and a mismatch is an error. Covariates
  only one fit uses are not compared, since models of the same outcomes with
  different covariate sets are exactly what gets compared. Because the stored
  columns can only show what both fits kept, the setup functions now also
  record for every row a key made of a digest of the whole source and the
  row's rank within a canonical ordering of it (an internal `.source_key`
  column, which is now a reserved name; nothing of the source's content is
  kept). Two fits holding the same keys in a different order were built from
  one source reordered between them, and are refused even when they share no
  covariate. Fits whose sources differ in columns the models did not use, or
  a fit from before the keys existed, cannot have their row order verified;
  the comparison then runs with a warning saying so.
  `calculate_dic()` objects carry the same frames, so a DIC comparison is
  checked too. A model whose object carries no data is reported as
  unverifiable rather than assumed to match.

* **`prior_sensitivity()` validates `probs` with the shared validator.** Its
  local copy of the check omitted the duplicate test that `.validate_probs()`
  applies everywhere else, so two equal probabilities produced two identically
  named `qNN` columns and the second silently overwrote the first: the caller
  asked for n quantiles and received fewer, with no error.

* **`prior_summary()` names the constrained prior instead of calling every
  positive-constrained prior a "half-distribution".** Two of those labels were
  wrong: an exponential is already supported on the positive half-line, so
  `<lower=0>` truncates nothing, and a normal or t with a nonzero location
  truncated at zero is a truncated normal or t, not a half-normal or half-t.

* **`prior_sensitivity()` names its quantile columns like the rest of the
  package.** They were built with `paste0("q", round(100 * probs))`, which
  labelled the default 2.5th and 97.5th percentiles `q2` and `q98`, and made
  distinct probabilities collide: `probs = c(0.024, 0.025)` produced `q2` twice
  and the second silently overwrote the first. The columns are now `q2.5`,
  `q50`, `q97.5`, matching `marginal_effects()`, so the two can be joined by
  name. Code reading `q2` or `q98` must be updated.

* **`prior_sensitivity()` no longer claims a scale sweep proves the inference is
  data-driven.** Constant summaries across the tested scales show insensitivity
  to those scales, within one prior family at one location on one model. The
  printed interpretation now says that, and points at `check_identification()`
  for the question a scale sweep cannot answer.

* **Marginal summaries are no longer clipped to finite reporting bounds.** The
  Stan models previously passed marginal probabilities through `safe_logit()`
  (clamping to `[1e-10, 1 - 1e-10]`) and ratios through `safe_divide()`
  (flooring the denominator at `1e-10`). Both helpers are gone. Event and
  non-event probabilities, marginal means, and rates are now formed on the log
  scale and the contrasts are built from those logs, so the reported quantity is
  the mathematical one rather than a finite surrogate. Two consequences: the
  likelihood is no longer biased by a clamp at extreme linear predictors, and
  ratios are no longer biased downward. The old `safe_divide()` substituted a
  denominator LARGER than the true one, so a risk ratio or rate ratio with a
  near-zero comparator was systematically understated; the log-scale contrast
  reports it. Working on the log scale also removes most of what used to trigger
  the clip in the first place, because a marginal probability is no longer
  rounded to 0 or 1 before the contrast is taken: `lor_*` is finite in cases
  where `safe_logit()` previously returned its clip boundary. What remains is
  that a ratio whose true value overflows double precision is now `Inf` rather
  than a large finite surrogate. That needs only a finite log contrast above
  `log(.Machine$double.xmax)`, about 709.78, not an infinite linear predictor.
  Read the log-scale generated quantities when it happens.
  See `?mlumr-numerical-evaluation`.

* **`predict(type = "link")` reports the marginal link, not the mean linear
  predictor.** It previously returned `E[eta]`, the average conditional linear
  predictor. It now returns `g(E[g^-1(eta)])`: the fitted link applied to the
  population-standardized response mean. Differencing two `type = "link"`
  predictions therefore gives a contrast on the fitted link scale, which
  reproduces a reported effect where the two scales coincide (a logit binomial
  fit's `lor_*`) and needs a transformation elsewhere, since
  `marginal_effects()` reports on the scale conventional for the family. The two
  definitions of `type = "link"` agree for the identity link and differ for
  logit, probit, cloglog, and log. This is a deliberate divergence from `multinma`, which keeps the two
  apart: its `predict(type = "link")` returns `E[eta]`, and the marginal
  link-scale contrast lives in `marginal_effects(mtype = "link")`. mlumr has no
  conditional population estimand to pair `E[eta]` with, since every effect it
  reports is standardized over a population, so it reports the marginal link
  under the one name rather than offering two link scales that differ silently.

* **Boundary probabilities use a continuity correction instead of a clamp.**
  `bound_probability()` previously clamped every input into
  `[min_count / n, 1 - min_count / n]`. It now leaves interior probabilities
  untouched and replaces only an observed 0 or 1 with the pseudo-count estimate
  `(r + min_count) / (n + 2 * min_count)`. For a zero-event arm with
  `min_count = 0.5` that is `0.5 / (n + 1)` rather than `0.5 / n`, so the link
  contrast, its standard error, and the risk ratio that `naive()` and `stc()`
  report for a binomial arm with no events (or no non-events) change slightly.
  Arms with events on both sides are unaffected.

* **New arguments are inserted before the sampler controls, so positional calls
  are not preserved.** `mlumr()` gains model-defining arguments ahead of
  `chains`, `iter`, and the rest. A call that passed sampler settings by
  position rather than by name therefore binds them to the wrong parameters and
  stops with a validation error naming the argument it actually received. Call
  `mlumr()` with named arguments.

* **`prior_sensitivity()` varies the prior and nothing else.** Every
  model-defining setting is taken from the original fit and replayed: family,
  link, survival distribution and its baseline controls (`n_knots`,
  `mspline_degree`, `pred_times`, `rmst_horizon`, `n_rmst_grid`, `aux_by`), the
  design-matrix controls (`center`, `qr`), the integration points, the shape and
  smoothing priors, and the engine and sampler settings. `...` may no longer
  override any of them, so movement across the sweep is attributable to the
  prior alone. For relaxed fits the comparator prior is swept alongside the
  index one, and `prior_beta_comparator_scales` pairs a chosen comparator scale
  with each index scale. Both are reported per row, in `scale` and
  `scale_comparator`, so a refit is never labeled by only half of the prior it
  was fitted under. On an SPFA fit, which has no comparator coefficient prior,
  the argument is declined with a warning and the column is dropped. Survival
  rows carry an `effect` label (`LOG_HR`, `LOG_TR`, or `DELTA_ETA`) and an
  `at_time` where one applies, from the same shared helper `marginal_effects()`
  uses.

* **`verbose = FALSE` now silences the sampler banner too.** cmdstanr writes its
  "Running MCMC with N chains / Chain k finished in ..." lines to stdout rather
  than through the condition system, so neither `refresh = 0` nor
  `suppressMessages()` suppressed them: roughly fifteen lines per fit, which
  buries the output of any loop over more than a handful of models. `mlumr()`
  now passes `verbose` through to the backend, and an explicit `show_messages`
  or `show_exceptions` in `...` still wins.

* **`prior_normal(autoscale = TRUE)` rescales the prior location as well as its
  scale.** Autoscaling states a prior on the coefficient of a covariate measured
  in standard-deviation units, so recovering it on the original scale divides
  both the location and the scale by that covariate's SD. Only the scale was
  divided before, which left a nonzero prior mean attached to the wrong
  covariate scale. The default `mean = 0` is unaffected, since `0 / sd` is `0`.

* **`check_integration()` compares correlations on one scale.** The realized
  integration-point correlation was always measured with Pearson while the
  target, when derived from the IPD, defaults to Spearman. The method that
  defined the target is now carried into the diagnostic and reported in the
  output, so the comparison can no longer warn (or reassure) purely from a
  method mismatch.

* **`check_integration()` also reports fidelity to the declared moments.** The
  existing resolution diagnostic compares two grid sizes and answers "is `n_int`
  large enough". It now additionally compares each realized grid moment against
  the mean and standard deviation declared in `set_agd()`, which answers the
  different question "does the grid represent the population it claims to". A
  `distr()` specification can be numerically well resolved and still target the
  wrong marginal.

* **`add_integration()` states where the copula correction does not apply.** The
  Spearman and Pearson maps branch on continuous versus binary margins. A
  nonbinary discrete margin, such as a count or an ordered category, has no
  branch and is mapped with the continuous-margin formula, so the realized
  association need not match the target. The calibration it needs is
  threshold-aware: with the margin's thresholds fixed, the observed
  correlation rises strictly with the latent Gaussian one, so a feasible
  target has one latent value, and what the package lacks is the numerical
  inversion that finds it, not a value to invert to. `add_integration()`
  warns when it detects such a covariate, and the documentation states the
  limitation.

* **`add_integration()` rejects a Pearson correlation with non-Gaussian
  margins.** A covariate-scale Pearson correlation is the Gaussian-copula
  correlation only when the margins are themselves Gaussian, so
  `cor_adjust = "pearson"` combined with a non-`qnorm` continuous margin and a
  nonzero off-diagonal entry now errors instead of silently treating the
  supplied matrix as a latent one. Use `cor_adjust = "spearman"`, Gaussian
  margins, or `cor_adjust = "none"` with a matrix already on the latent scale.

* **`add_integration()` warns when a supplied `distr()` distribution grossly
  contradicts the declared `set_agd()` moments.**

* **Continuous multi-row comparator estimand, and `outcome_n` is now required
  for it.** For the normal family the comparator-population standardized effect
  from several `set_agd()` rows is weighted by `outcome_n` (sample size) rather
  than by inverse variance, so splitting one comparator population into subgroup
  rows no longer changes the estimand. An inverse-variance average estimates a
  common mean efficiently but is not the comparator population's mean, which is
  the size-weighted mixture of its strata. Because there is no defensible way to
  combine several population strata without knowing how large they are,
  `set_agd()` now requires `outcome_n` when normal aggregate data have more than
  one row and errors rather than silently falling back to precision weights.
  `naive()` and `stc()` use the same weighting. Single-row AgD is unchanged and
  still does not require `outcome_n`.

* **`naive()` combines multiple aggregate rows as strata.** For binomial data
  the comparator standard error was computed as though the pooled comparator
  were a single binomial sample of size `sum(n)`. It is now the variance of the
  sample-size-weighted average of the row proportions,
  `sum(w_k^2 * p_k * (1 - p_k) / n_k)` with `w_k = n_k / sum(n)`, propagated to
  the link scale by the delta method. It reduces exactly to the previous formula
  for single-row aggregate data. The reported comparator event rate is now the
  observed proportion; the continuity correction is applied only inside the
  effect calculation.

* **`stc()` no longer reports an index-population contrast.** For the binomial,
  normal, and count families `stc()` previously returned an index-population
  effect alongside the comparator-population one, obtained by assuming the
  treatment difference is constant on the link scale. That constancy is an extra
  assumption which is not part of the STC estimand and is not testable from the
  available data, and it does not hold under effect modification, which is the
  situation population adjustment exists to handle. `stc()` is now what its
  design supports: a comparator-population estimand, labeled as such in the
  returned object. Use `mlumr()`, which standardizes both treatment models and
  reports both populations without that assumption, when the index population is
  the decision target.

* **`seed` defaults to 2026 and says so.** `seed = NULL` previously drew from
  the session RNG whenever `.Random.seed` existed. R initializes that variable
  on demand from the clock and the process id, so its presence never
  established that `set.seed()` had been called: an unseeded fit was silently
  irreproducible, and drawing from the state also advanced the caller's stream.
  `seed = NULL` now uses the documented default of 2026 and warns, and the fit
  banner marks the seed as a default.

* **A difference of two unbounded quantities is undefined, not zero.** The
  log-scale contrast used by the marginal summaries returned an exact zero when
  both logs were `+Inf`, reporting an indeterminate difference as a null
  effect. Both the R helper and its Stan counterpart now leave it undefined.
  Two `-Inf` logs still give zero, because both quantities are zero.

* **Zero exposures and zero aggregate standard errors are refused again.**
  `E_ipd` and `E_agd` in the poisson models and `se_agd` in the normal models
  are declared `<lower=1e-12>`, as they were in 0.1.0. Under a bound of zero an
  exposure of exactly zero reaches `log()` in the linear predictor, and a zero
  aggregate standard error makes the normal likelihood improper.

* **The collinearity guard reports a design that cannot be full rank.** It
  returned quietly whenever there were no more complete IPD rows than
  covariates, which is exactly the case it exists to catch. A covariate whose
  empirical standard deviation is undefined, which is what a single IPD row
  produces, is treated as having no usable scale instead of aborting
  autoscaling.

* **Tail ESS is computed rather than looked for.** `check_diagnostics()` tested
  a column that the default rstan backend never produced, so the check was
  inert on every fit the package makes. Tail ESS is now computed chain-aware
  from the post-warmup draws, and a fit that cannot supply it is reported as
  such rather than passing silently.

* **`check_integration()` no longer passes a comparison it did not make.** An
  all-missing set of differences gave `-Inf` from `max(na.rm = TRUE)`, which
  clears every threshold and printed as "close"; such a comparison now reads
  "unavailable". The declared-target standard deviation falls back to the
  Bernoulli form only for margins declared binary, rather than for any
  covariate whose mean happens to land in `[0, 1]`. A correlation matrix passed
  directly is resolved by name the way `add_integration()` resolves it, so
  reversed dimnames no longer produce a verdict about the wrong pairs. Under
  `cor_adjust = "none"` the supplied matrix is the latent Gaussian copula
  correlation, which the realized covariate-scale correlation does not
  estimate; that comparison is withheld and named instead of scored across the
  two scales.

* **The cmdstanr executable cache now notices a changed include.** The cache
  key concatenated the MD5 of the model file with the MD5 of every include and
  kept the first 32 characters. An MD5 digest is already 32 characters, so the
  key was the model file's digest alone and every include hash was discarded.
  Editing a shared include, which is where the likelihood helpers and the
  numerical guards live, produced the same key, and a previously compiled
  executable was reused. The key is now a digest over a canonical payload
  naming every source file with its own content, plus the CmdStan version,
  its installation path and the content of its `make/local`, so a changed
  include, a different CmdStan, or changed build flags invalidate it. A
  compiler upgrade with everything else unchanged does not, and does not need
  to. Anyone carrying a cache from an earlier version will get one recompile.

* **`stc()` refuses an outcome model whose fitted probabilities have all
  reached a boundary.** Its checks looked for a failure the fitting reports,
  and separation is not one: iterative
  reweighting stops when the deviance stops changing, and a separated fit has
  no maximum for it to stop at, so a binomial arm with no events returned
  convergence, finite coefficients and a finite covariance. A hundred rows with
  the outcome always zero produced a coefficient of -26.6 and a largest fitted
  probability of 3e-12, with a confidence interval to match, every number a
  property of where the iteration stopped rather than of the data. Raising
  `maxit` changes none of them. The symptom separation always leaves is now
  an error: every fitted probability against a boundary, either one, tested
  only where a boundary exists, so a genuinely rare event still fits.
  Quasi-complete separation, where rows sit on the separating hyperplane and
  keep interior fitted probabilities, is not detected; separating that from a
  strong but identified fit needs an exact test rather than the fitted
  values.

* **A caller's rstan `control` reaches the sampler.** `...` is documented as
  passing arguments to `rstan::sampling()`, but the backend supplied its own
  `control` beside the caller's, and `control` is a formal of that function, so
  argument matching failed before sampling began and the sampler's other
  settings could not be reached. The two are merged now, with the caller's
  entries winning as the more specific request.

* **`conditional_predict()` returns the quantiles it was asked for.**
  `quantile()` names each result with `format()`, which prints to the display
  precision, while every lookup in the package builds the name from the
  probability itself. The two spellings agree for a round probability and not
  otherwise: a third is `33.33333%` to R and `q33.3333333333333` here, so
  asking for it returned NA for both treatments out of entirely finite draws,
  and only the default probabilities happened to line up. The summaries now
  carry the package's own names, so no lookup can disagree with them.

* **A `cmdstanr` run that produced no draws now says so, instead of failing
  inside `checkmate` on a path under `tempdir()`.** `cmdstanr` decides which
  chains are worth reading with `is_finished() | is_queued()`, and a *queued*
  chain is one whose process never started, so it has written no CSV while
  its intended path still comes back as readable. `fit$draws()` then handed
  that path to `read_cmdstan_csv()`, and the whole fit died on
  `Assertion on 'files' failed: File does not exist`, naming a temporary file
  the caller had never heard of and giving nothing to act on. The backend now
  checks that the output it is about to read exists, and reports how many
  chains produced nothing and where CmdStan's own messages can be found. A
  chain that ran and *failed* is unaffected: `cmdstanr` drops it and the run
  continues on the chains that finished, as a partly failing multi-chain fit
  already relied on.

## Transportability to arbitrary target populations

* **`newdata` argument** on `marginal_effects()` and `predict.mlumr_fit()`
  transports treatment effects and absolute outcomes to an **arbitrary target
  population** by Bayesian g-computation (model-based standardization over a
  supplied covariate distribution), as in the ML-UMR transportability step.
  Version 0.1.0 offered only the built-in index and comparator populations.
  Supported for all families' effects and predictions. For survival, the
  collapsible RMST-based effects (`"rmstd"`, `"rmstr"`) and every absolute
  prediction transport. The marginal hazard ratio is reported for a target
  population too, but it is not a property of that population alone: hazard
  ratios are non-collapsible, and the marginal one weights the covariate
  distribution by each arm's own survival. It follows the same
  evaluation-time convention as the built-in populations, the closed-form
  `t -> 0` limit when the two studies share a baseline shape and the requested
  (or first) fitted time when they do not. An AFT fit reports its
  target-standardized location contrast,
  `exp(mean(eta_index) - mean(eta_comparator))` over the target rows: with
  shared coefficients the covariate term cancels draw by draw and the value is
  the same for every target, which is the sense in which a shared-shape time
  ratio is population-invariant, while with relaxed coefficients it does not
  cancel and the value belongs to that target.

  Standardizing to the index covariates reproduces `population = "index"`
  exactly, for every measure including the hazard ratio, which is the check
  that the transport path and the built-in path are the same calculation.

* **`marginal_effects()` emits a one-line note** when a relaxed fit is queried
  for the index population, reporting the **marginal posterior variance
  change** of `beta_comparator` for every covariate, `1 - (posterior sd /
  prior sd)^2`, positive where the posterior is narrower than the marginal
  prior and negative where it is wider. The note says in place that the
  number is descriptive, neither a fraction learned nor an identification
  test, so it does not stand as a verdict on any covariate. A single marginal
  comparator curve constrains
  `beta'X` but not the direction of `beta`, which is exactly what transporting
  to the index population needs, and an event count cannot detect that. The
  prior SD respects the prior family: Student-t scales are converted via
  `sqrt(df / (df - 2))`, and priors with no finite variance (`df <= 2`,
  including the Cauchy) report `NA` rather than a number that would misstate how
  much was learned. Suppress with
  `options(mlumr.quiet_relaxed_index = TRUE)`.

## Plotting

* **`plot()` methods** for the result objects, following multinma's convention
  that calling `plot()` on an effects or prediction object produces the
  corresponding figure:
    - `plot(marginal_effects(fit))`: forest of population-standardized effects.
    - `plot(predict(fit, type = "survival"))`: a curve with a credible band.
      The `"hazard"`, `"cumhaz"` and `"loghr"` types plot the same way, and
      `"rmst"`, `"median"` and `"response"` plot as point-intervals. Compose
      further layers, such as a Kaplan-Meier overlay, with `+`.
    - `plot(conditional_effects(fit, newdata = ...))`: effects by covariate
      profile.
* Each forest draws the null line implied by the measure it is showing, per
  facet: 0 for differences and log scales, 1 for the risk ratio, rate ratio,
  hazard ratio, time ratio, RMST ratio, and the two exponentiated survival
  contrasts. A forest showing only ratio measures is drawn on a log axis, so
  reciprocal effects sit at equal distances from the null. The interval's
  coverage is read from the quantiles the result carries rather than assumed to
  be 95%, and a time-specific marginal hazard ratio is labelled with the
  evaluation time it belongs to.
* **`geom_km()`** overlays the observed Kaplan-Meier curves (from the
  `mlumr_data` object) on a model survival plot, colored by treatment and
  honoring delayed entry. Each curve carries the population its arm was
  measured in, so on a plot faceted by population it appears only in its own
  panel.
* **`plot_prior_posterior()`** (exported; the `multinma` name) overlays the
  posterior of named parameters on the prior the fit records for each of them,
  including the `<lower=0>` truncation for the constrained ones. A parameter
  the fit carries no prior for is refused rather than drawn against another
  parameter's.
* **`mlumr_forest()`** draws a forest plot from a plain data frame of estimates
  and interval bounds, for comparisons the `plot()` methods do not cover because
  they mix estimators: putting `naive()`, `stc()`, and both ML-UMR models on one
  axis, for instance. It takes the reference line, axis label, title, and
  subtitle as arguments so the caller sets the measure's null rather than
  inheriting one. One interval far wider than the rest is clipped to a viewport
  built from the others, with an arrow on the side it runs past, so a single
  wide row does not squeeze the rest into a line. A bound that is infinite is
  clipped that way; a bound that is MISSING is not, because no interval was
  reported and drawing one from edge to edge would put an uncertainty on the
  figure that nobody estimated. Such a row shows its point estimate alone.
* `marginal_effects()`, `predict()`, and `conditional_effects()` now return
  lightweight `data.frame` subclasses so these `plot()` methods can dispatch;
  all existing data-frame behavior (indexing, `knitr::kable()`, the reporting
  engine) is unchanged.
* `ggplot2` moved from Suggests to Imports (the plot methods use it at run
  time), at `>= 3.4.0` because they use `linewidth`, which 3.3.x ignores.

## Time-to-event (survival) outcomes

* **New `prior_aux2` argument for the second generalized-gamma auxiliary
  parameter.** The two auxiliaries govern different features of the hazard and
  can need different regularization; both previously took whatever `prior_aux`
  specified. `NULL` (the default) reuses `prior_aux`, so existing fits are
  unchanged, and `prior_summary()` shows the two separately when they can
  differ, naming them as the generalized-gamma `sigma` and `k = 1 / Q^2` for the
  Lawless shape `Q` rather than as anonymous auxiliaries. Supplying it for a
  distribution with fewer than two auxiliary parameters warns and is discarded
  without being validated, rather than being silently ignored, and
  `prior_sensitivity()` refuses to vary it mid-sweep like
  every other scenario-defining argument. `prior_aux`'s documentation now also
  records that one default is reused across auxiliary parameters that do not
  share a scale: the Gompertz shape has units of 1 / time, so the same trial
  expressed in days rather than years gives a half-normal(0, 2) an entirely
  different meaning.

* **New `"survival"` outcome family for data setup.** `set_ipd()` accepts
  time-to-event data, and **`set_agd_surv()`** takes the comparator arm as
  reconstructed pseudo-IPD (event and censoring times digitized from a published
  Kaplan-Meier curve) together with its covariate moments. `combine_data()` and
  `add_integration()` carry the family through.

* **Frequentist benchmarks for survival**: `naive()` returns an unadjusted Cox
  log hazard ratio, and `stc()` performs parametric G-computation of the RMST
  difference using the `flexsurv` package (a suggested dependency). `stc()`
  takes an `rmst_horizon` argument, since its own default is the pooled maximum
  observed time while a stratified flexible `mlumr()` baseline defaults to the
  follow-up both studies observed; the two are different estimands. Note that
  `naive()` is on the **log** scale, so it is not directly comparable with
  `marginal_effects(effect = "hr")` unless exponentiated.

* **Survival `stc()` uncertainty is a nonparametric bootstrap**, not the delta
  method the other families use: the RMST is an integral of a fitted survival
  function and has no convenient closed-form variance. `n_boot` (default 200,
  `0` for a point estimate with no interval) and `seed` control it, and the seed
  is restored on exit so the caller's RNG stream is untouched. `n_boot = 1` is
  rejected, because the standard error of a single resample is undefined and was
  otherwise indistinguishable from every resample having failed.

* **`survival_unit` for LOO and WAIC.** `calculate_loo()`, `calculate_waic()`,
  and `compare_models()` gain a `survival_unit` argument controlling what one
  pointwise unit is for a survival fit. The comparator arm enters as
  reconstructed pseudo-individuals, so the default `"observation"` holds out one
  pseudo-individual at a time and is optimistic: the pseudo-IPD are a
  digitization of a single published curve, not independent observations.
  `"arm"` groups them so each external arm is one held-out unit, and
  `"aggregate"` treats all comparator pseudo-IPD as a single external-evidence
  unit. The index IPD always stay per-individual.

* **Regression coefficients are labeled by covariate name.** `summary()` on a
  fit now prints `beta[age]` rather than `beta[1]` (and `beta_index[age]` /
  `beta_comparator[age]` for relaxed fits). The underlying `variable` strings in
  `fit$summary` are unchanged, so code that indexes on `beta[1]` keeps working.

* **HTA prediction suite** from `predict()` on a survival fit:
  `type = "survival"`, `"hazard"`, `"cumhaz"`, `"rmst"` (restricted mean
  survival time), `"median"`, and `"loghr"` (the time-varying marginal log
  hazard ratio curve, null 0). `predict(type = "median")` carries a
  `p_not_reached` column reporting the posterior probability that the median is
  beyond follow-up. A `times` request is answered one row per requested time,
  in the order asked, with a `requested_time` column beside `time`: each is
  evaluated at the nearest fitted grid time, and a message names the requested
  and the used time whenever the two differ or two requests land on one grid
  point. `pred_times` sets the grid itself for exact evaluation.
  `conditional_effects()` / `conditional_predict()` give
  covariate-conditional contrasts and survival curves. Both grid-based
  quantities say when their grid is too coarse to trust, per posterior draw:
  RMST warns when more than half of the fitted survival decay lands inside a
  single interval of the `n_rmst_grid` grid (a two-node grid always does),
  judged on each target profile's own curve before the profiles are averaged,
  since profiles that collapse inside different intervals average to a curve
  that looks resolved while the trapezoid overstates every one of them; and
  the median warns when the curve is already at or below 0.5 at the first
  `pred_times` point, where it can only be interpolated from `S(0) = 1`
  across the whole first interval. Both point at a refit with a finer grid.

* **`marginal_effects()` reports natural-scale survival effects** (null 1): the
  hazard ratio (`HR`) for proportional-hazards distributions, the time ratio
  (`TR`) for AFT distributions with one shared shape and one shared coefficient
  vector, or the exponentiated linear-predictor contrast (`EXP_DELTA_ETA`) where
  neither holds. Plus the RMST difference (`RMSTD`, null 0) and RMST ratio
  (`RMSTR`).

* **A scalar hazard ratio never travels without its evaluation time.** Marginal
  hazard ratios are non-collapsible and generally time-varying, so
  `marginal_effects()` carries an `at_time` column, reported as `0` for the
  closed-form `t -> 0` limit under a shared baseline shape and as the evaluation
  time under study-specific shapes. For the whole curve use
  `predict(type = "loghr")`; the RMST-based effects are collapsible and free of
  this entirely.

* **RMST results carry their restriction time.** RMST is an integral to a
  horizon, so results computed to different horizons are different estimands and
  must not be pooled. `predict(type = "rmst")` and the `RMSTD` / `RMSTR` rows of
  `marginal_effects()` report a `horizon` column.

* **An effect that is not available is an error, not a substitution.** With an
  AFT distribution and study-specific shapes, the exponentiated contrast is not
  a time ratio, and the same applies to any relaxed AFT fit even with shared
  shapes. An explicit `effect = "hr"` / `"tr"` request stops in these cases and
  names the alternative rather than returning a differently-named quantity.

* **Parametric and flexible baselines** via the `distribution` argument to
  `mlumr()`:
    - Proportional hazards: `"exponential"`, `"weibull"` (default),
      `"gompertz"` (positive-shape, increasing-hazard parameterization).
    - Accelerated failure time: `"exponential-aft"`, `"weibull-aft"`,
      `"lognormal"`, `"loglogistic"`, `"gamma"`, `"gengamma"` (the positive-`Q`
      Lawless generalized gamma subfamily; negative-`Q` shapes are not covered).
    - Flexible baseline hazard: `"mspline"` (M-spline) and `"pexp"`
      (piecewise exponential), with a random-walk smoothing prior.

* **New priors** `default_prior_aux()` (shape/scale parameters) and
  `default_prior_smooth()` (M-spline smoothing SD), configurable via the
  `prior_aux` and `prior_smooth` arguments to `mlumr()`. **`make_knots()`**
  places M-spline knots, and the `knots` argument accepts a custom placement.

* **The baseline hazard is estimated per study by default.** `mlumr()` gains
  `aux_by`, defaulting to `".study"`: the index and comparator studies get their
  own M-spline coefficients (or their own parametric shape parameters) rather
  than sharing one shape, matching what `multinma::nma()` does. Sharing one
  baseline across both studies is `aux_by = "none"`. Two single-arm trials
  rarely share a hazard shape, and assuming they do imposes proportional hazards
  *across studies*, which no randomization supports.

  Each stratum gets its own knots over its own observed support. This is
  required for identification, not a refinement: with one pooled basis spanning
  the longer study, a shorter study can have basis functions it never observes,
  leaving a flat likelihood direction that the prior rather than the data
  resolves.

* **Censoring support** in `set_ipd()`: right, left, interval, and delayed entry
  (left truncation) via a `survival::Surv()` object. The
  `time`/`status`/`entry_time` column route covers right-censoring (status
  `0`/`1`) and optional delayed entry; supply a `Surv` object for left- or
  interval-censored data.

* `outcome` is no longer required for `family = "survival"`, which uses
  `Surv`/`time`/`status` instead. It is still required for the other families,
  and its absence is now reported as such.

## Identifying the relaxed model's comparator coefficients

* **New `check_identification()`**: how much aggregate evidence does
  `model = "relaxed"` need before its index-population estimate is data-driven
  rather than prior-driven? In the relaxed model `beta_comparator` is identified
  only by the aggregate likelihood, so the answer is fixed by the aggregate
  subgroup rows before any model is fitted.

  It answers that for a relaxed specification and declines a fitted SPFA
  object, which has no `beta_comparator` for the question to be about.

  Each aggregate row contributes one constraint and the comparator side has
  `K + 1` unknowns (the intercept counts), so `S >= K + 1` rows are necessary.
  They are not sufficient: the rows must also differ in **every** covariate
  direction. `check_identification()` measures that directly, reporting
  `cond_inv` (smallest over largest singular value of the centered, IPD-scaled
  subgroup-mean matrix) and `eff_dim`, the participation ratio of the squared
  singular-value spectrum, which says how evenly the spectral variation is
  spread across directions and is not a count of identified coefficients.
  Subgroups reported one variable at a time never spread it beyond a single
  direction however many are published.

  For a nonlinear mean model the report is labeled descriptive only: subgroup
  means do not determine the likelihood geometry there, because the
  within-row distributions also affect the integrated response.

* **New `prior_beta_comparator` argument** to `mlumr()` lets the relaxed model
  use a separate (typically tighter) prior on `beta_comparator`, which
  regularizes the index-population estimand that would otherwise extrapolate
  weakly-identified coefficients over the IPD covariate distribution. Defaults
  to `prior_beta` (so behavior matches earlier versions); ignored for
  `model = "spfa"`. Surfaced separately by `prior_summary()` and reused by
  `prior_sensitivity()`. All five relaxed Stan models take
  `prior_beta_comparator_mean` / `_sd` / `_dist` / `_df`, so the comparator
  coefficients can use a fully independent prior including a different family
  from `beta_index` (for example a heavy-tailed Student-t for regularization).

* **The weak-identifiability warning no longer counts a duplicated row as
  evidence.** Version 0.1.0 warned when `n_agd_rows < 2 * n_cov`, so repeating
  a `set_agd()` row silenced it without adding anything. What replaces the
  count depends on the link, because what a row contributes does. Under an
  identity link the mean profiles are the design, so the warning triggers on
  the rank of that design. Under any other link the integrated response depends
  on each row's whole covariate distribution, and two rows with equal means but
  different spreads do carry different constraints, so mean rank would
  understate the evidence; there the warning triggers on the number of rows
  that do not repeat another's integration grid, a bound that holds under every
  link because a repeated grid gives an identical likelihood term.

## Covariate distributions

* **`distr()` now honors arguments passed by position.** It captures its `...`
  unevaluated and evaluation walked `names(args)`, so anything supplied without
  a name was never iterated and never reached the quantile function.
  `distr(qnorm, 10, 2)` therefore integrated a STANDARD normal, silently, with
  no warning and no error: the fit ran, converged, and answered a different
  question than the one asked. Positional arguments are now matched against the
  quantile function's own formals once, at construction, so the stored
  specification says what each argument is. Arguments after `...` in the
  quantile function's signature stay name-only, which is R's own rule, and an
  argument that matches nothing is an error rather than a silent omission.
  Abbreviated names are completed the same way: `distr(qbinom, si = 5, ...)`
  evaluated with five trials, because R completes `si` at call time, but the
  margin classification read `args$size`, found nothing, and labeled a
  five-trial binomial binary. The stored name is now the full formal.

* **New moment-parameterized marginal distributions**, mirroring the ones
  `multinma` exports so a published baseline table can be used as printed:
  `qgamma()` / `pgamma()` / `dgamma()` and `qlogitnorm()` / `plogitnorm()` /
  `dlogitnorm()`. All accept a `mean` and `sd` that override the native
  parameters (`shape`/`rate` for the gamma, `mu`/`sigma` on the logit scale for
  the logit-normal), so `distr(qgamma, mean = age_mean, sd = age_sd)` works
  directly in `add_integration()` instead of requiring a hand conversion to
  shape and rate. Without `mean` and `sd` they forward to \pkg{stats}
  unchanged, so they are drop-in safe. The logit-normal is the natural marginal
  for a covariate reported as a proportion, such as percent body surface area.

  Both parameterizations validate what they are given. `mean` and `sd` must be
  supplied together (half a moment specification is an error, not a silent
  fallback to the native defaults). A gamma needs both strictly positive and
  finite: a negative SD is not a typo the conversion can absorb, since both
  `shape` and `rate` square it and `sd = -2` would otherwise return exactly the
  `sd = 2` distribution. A logit-normal mean must lie strictly inside `(0, 1)`
  and its SD must satisfy `sd^2 < mean * (1 - mean)`, the bound any variable on
  `(0, 1)` obeys. Supplying a conflicting `rate` and `scale` is refused rather
  than resolved in favor of one of them.

  The logit-normal moment reparameterization has no closed form and is solved
  numerically. The moments are integrated over the latent normal variable
  rather than over `x` on `(0, 1)`, where a concentrated margin is a narrow
  spike that adaptive quadrature steps over; the search runs on `log(sigma)` so
  the scale cannot go negative, starts from the delta-method approximation on
  the logit scale, and repeats until a restart stops improving, because
  Nelder-Mead reports convergence when its simplex collapses rather than when
  it has arrived. The recovered moments are then checked against the target,
  relative to the target rather than absolutely, instead of the optimizer's
  convergence flag being trusted on its own.

## Example data

* **The worked examples are built on datasets derived from published trials**,
  replacing the freely invented datasets used in version 0.1.0's vignettes.
  Provenance differs by example and is stated in each dataset's help page. The
  plaque psoriasis data are redistributed from `multinma`, where the individual
  patient data are themselves simulated to resemble the published trial. The
  shoulder pain and dental caries data are synthetic, generated with `synthpop`
  from openly licensed trial data.

* `psoriasis_ipd` / `psoriasis_agd` (binary), `shoulder_ipd` / `shoulder_agd`
  (continuous), `caries_ipd` / `caries_agd` (count), and `ndmm_ipd` /
  `ndmm_agd` / `ndmm_agd_covs` (survival, newly diagnosed multiple myeloma,
  also redistributed from `multinma`).
  `data-raw/prepare_multinma_subsets.R` reproduces every bundled dataset.

* `psoriasis_ipd$prevsys` is `integer` 0/1 rather than `logical`, matching
  every other binary covariate in the bundled sets. `set_ipd()` declines a
  logical covariate, so the pair previously could not be used without coercing
  it first. The values are unchanged, and no dataset shipped in 0.1.0.

* Each help page now records the covariates that look wrong but are not:
  `caries_ipd$exposure` is the poisson offset that `set_ipd()` requires,
  constant at 1 because `dmft` is a whole-mouth count with no time at risk;
  `caries_ipd$log_cfu` is bimodal, with 9 of 103 values at exactly zero;
  `psoriasis_ipd$weight` is missing for 2 of 347 rows; and the shoulder and
  caries pairs share one `study` label across their IPD and AgD halves because
  each pair is two arms of one trial, so the `combine_data()` warning about it
  is expected.

## Performance

* The binary, continuous, and count IPD likelihoods now use Stan's fused
  **GLM density functions** (`bernoulli_logit_glm`, `normal_id_glm`,
  `poisson_log_glm`) on their canonical links (logit / identity / log); these
  carry analytic gradients and are faster than the equivalent `_lpdf` forms.
  Non-canonical links (probit, cloglog, log-normal) are unchanged. They are
  statistically equivalent to the 0.1.0 implementation: results match up to
  Monte Carlo error.
* Models **center the covariates** by default: the IPD design matrix and the
  comparator integration grid are shifted to their pooled covariate mean before
  fitting. This removes an intercept-versus-slope collinearity that, on
  real-scale covariates (for example age in years), could push the NUTS sampler
  into very deep (max-treedepth) trajectories and dramatically slow fits. The
  shift is estimand-invariant for the reported effects (the intercept absorbs
  it, so all population-standardized contrasts are unchanged) and is applied
  transparently to `predict()`, `conditional_effects()`, and
  `conditional_predict()`. It is not prior-invariant: `prior_intercept` then
  applies to the intercept at the pooled covariate mean, so intercepts and
  intercept prior-versus-posterior plots are on a different scale from an
  uncentered fit. `center = FALSE` restores the raw-scale parameterization, and
  `qr = TRUE` offers a QR-rotated design as an alternative conditioning fix.

## Documentation

* **The `shoulder` and `caries` examples no longer call a reference estimate
  a known truth.** Both are synthesized from a single randomized trial and then
  split into a single-arm IPD source and a single-arm aggregate source, and the
  documentation said this made them examples "whose true answer is still
  known". What is available is a randomized reference comparison, obtained by
  fitting the two arms together on the full data. That is an estimate carrying
  sampling error, not an evaluated population causal truth, since neither
  dataset comes from a declared generating model with a computable estimand.

* **The relaxed model's identification claim is corrected.**
  `prior_beta_comparator` said the comparator-population effect "is identified
  directly by the AgD". The AgD likelihood informs the comparator-population
  outcome, but identifying `beta_comparator` or a treatment contrast depends on
  the number and geometry of the independent aggregate summaries, the link, the
  covariate distribution, the outcome precision, and the prior. The
  documentation says so, and points at `check_identification()` and
  `prior_sensitivity()`.

* **`set_agd_surv()` states that reconstruction uncertainty is not propagated.**
  Pseudo-individual records enter the likelihood as observed data, so a survival
  posterior conditions on one digitization of one published curve and carries
  none of the uncertainty in producing it: intervals are narrower than the
  evidence supports, most visibly for flexible baselines, late-tail RMST and
  medians, and weakly identified relaxed comparator coefficients. The help page
  now says this and describes refitting across plausible reconstructions as the
  way to see how much it matters.

* Reorganized the vignettes into nine outcome-focused guides: `introduction`,
  `data-preparation`, `binary-outcomes`, `continuous-outcomes`,
  `count-outcomes`, `survival-outcomes`, `subgroup-identification`,
  `fitting-and-diagnostics`, and `choosing-a-method`.
* The modeling vignettes use the bundled example data and show complete
  output: posterior summary and effect tables, forest plots, survival and
  Kaplan-Meier curves, and posterior diagnostic plots.
* To keep checks fast, those vignettes are precompiled, following multinma's
  pattern: the Stan models are fitted once locally through
  `vignettes/precompile.R` and the rendered HTML ships through the `R.rsp`
  engine, so no Stan model runs at build or check time. The prebuilt `*.html`
  and their `*.html.asis` registration stubs are tracked and included in the
  build.
* The binary-outcomes vignette covers quasi-complete separation, handled with
  a heavy-tailed coefficient prior (`prior_student_t()` / `prior_cauchy()`),
  following Gelman et al. (2008).
* Covariate marginals follow multinma's own examples
  (`example_plaque_psoriasis.Rmd`, `example_ndmm.Rmd`): gamma for skewed
  continuous covariates, logit-normal for proportions, Bernoulli for binary,
  with age in years and weight in kilograms. Each vignette fits both the
  prognostic-only model and the one with treatment interactions, and the
  anchored cross-check does the same with `multinma`.
* The vignettes are self-contained. Each opens with a visible `library(mlumr)`
  and `options(mc.cores = ...)` chunk and otherwise uses exported functions, so
  every line producing a displayed result is printed in the vignette itself and
  the visible chunks are meant to run in a new session. The identification
  vignette additionally calls one internal diagnostic helper. The survival
  vignette leads with the M-spline baseline, matching multinma's NDMM example,
  and adds a relaxed-model effect-modification section.
* `?set_agd` now states what an aggregate Poisson row assumes about exposure.
  The likelihood multiplies a row's total exposure by the rate averaged over
  the supplied covariate distribution. That reproduces the sum of each
  person's exposure times their own rate exactly when the distribution is
  weighted by exposure, and, with person-level moments, only when exposure
  carries no information about the covariate-specific rate within the row.
  The whole covariate distribution the rate is averaged over therefore has
  to describe person-time rather than people: the marginal shape each
  `distr()` assumes around the moments, and with two or more covariates the
  correlation `add_integration()` combines them with, whose default is
  estimated from the index sample. Exposure can change a covariate's skewness
  or how the covariates go together without moving any of their moments. The
  two populations coincide whenever mean exposure does not vary with the
  covariates, and published subgroup tables usually report the person-level
  reading without saying which one they support.
* `?set_agd_surv` now states which population the covariate moments must
  describe when the comparator has delayed entry. The model conditions on
  survival to entry inside the covariate integral and averages afterwards, so
  the distribution integrated over, its shape and dependence as well as its
  moments, must be that of the population observed at entry rather than a
  baseline, pre-selection population; surviving to entry selects on the very
  covariates being integrated out. Varying entry times need one assumption
  more, since the model carries a single covariate distribution per arm while
  the population observed at each entry time can differ, both because each
  entry time selects a different subgroup and because who enters when can be
  related to the covariates. A test now compares the
  model's aggregate delayed-entry likelihood, draw by draw, against direct
  integration under that definition over a two-component covariate mixture,
  and against the other order of conditioning and averaging, which gives a
  different number.

## Dependencies

* Added `splines2` and `survival` to Imports, and `flexsurv` to Suggests; the
  survival STC G-computation uses `flexsurv` when it is available.
* Moved `ggplot2` from Suggests to Imports. The `plot()` methods,
  `mlumr_forest()`, `geom_km()`, and `plot_prior_posterior()` build ggplot
  objects at run time rather than only in examples.
* Added `multinma` (the anchored cross-check in the vignettes), `ggsurvfit`
  (the observed Kaplan-Meier displays), and `R.rsp` (the precompiled-vignette
  engine) to Suggests, and `R.rsp` to `VignetteBuilder`.

* **`Additional_repositories` is pinned, and the optional cmdstanr backend has
  a Windows limitation because of it.** That field points at
  `https://mc-stan.org/r-packages`, which the Stan project describes as
  deprecated. The maintained repository is not usable there: adding it to the
  resolution chain makes `rstan` resolve to a development snapshot while
  `StanHeaders` still resolves to the released CRAN build, and that pair fails
  to compile. Pinning a repository whose versions can never outrank CRAN keeps
  the dependency graph deterministic.

  The cost is that the pinned repository serves cmdstanr 0.8.0, which predates
  current Rtools and cannot build CmdStan on Windows with R 4.6. **On Windows,
  install cmdstanr from the maintained repository instead**:
  `install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))`.
  `mlumr_engine("cmdstanr")` offers that route interactively and names which
  repository it is using, and when a cmdstanr older than 0.9.0 is already
  installed on Windows, its own toolchain check reports the missing Rtools,
  and R itself can compile C++, so the Rtools it does not recognize is there,
  it says to upgrade from there, and to restart R afterwards since the loaded
  cmdstanr stays in use until then, before trying to build CmdStan, rather
  than offering an installation that fails. A fit that selects cmdstanr through
  the `engine` argument or the option in a profile, neither of which passes
  through `mlumr_engine()`, meets the same check and stops with the same
  advice instead of reaching compilation and failing there. The default
  `rstan` backend is unaffected on every platform; cmdstanr is optional
  throughout.

  A scheduled job re-checks both halves of this: that the pinned repository
  still serves cmdstanr, and whether `rstan` and `StanHeaders` have started
  resolving from one source again, which is the condition that would let the
  pin be reverted. It fails after a review date so the workaround cannot
  outlive its reason unnoticed.

## Package logo

* **New hex-sticker logo** using a broken-anchor motif, for the unanchored
  comparison. Built with `hexSticker` and the Ubuntu font.

# mlumr 0.1.0

Initial CRAN release.

## Core models

* **ML-UMR models**: Bayesian multilevel unanchored meta-regression with
  two model variants:
    - SPFA (Shared Prognostic Factor Assumption): shared covariate effects
      across treatments
    - Relaxed SPFA: treatment-specific covariate coefficients allowing
      effect modification estimation

* **Three outcome families**: binary (binomial), continuous (normal), and
  count (Poisson) outcomes, each with appropriate link functions:
    - Binomial: logit (default), probit, cloglog
    - Normal: identity (default), log
    - Poisson: log

* **Dual Stan backend**: rstan (default, CRAN-compatible) with optional
  cmdstanr support. Switch engines with `mlumr_engine("cmdstanr")`, which
  guides installation of cmdstanr and CmdStan if needed. Per-call override
  via the `engine` argument in `mlumr()`.

* **Simulated Treatment Comparison (STC)**: Frequentist outcome regression
  via parametric G-computation with delta-method standard errors. Supports
  prediction at covariate means or marginalization over full covariate
  distributions using integration points.

* **Naive unadjusted estimate**: Benchmark comparison of crude outcome
  summaries with delta-method confidence intervals.

## Data preparation

* `set_ipd()`, `set_agd()`, and `combine_data()` provide a
  unified interface for preparing IPD and AgD for all three methods.
* `set_ipd()` rejects covariate names that collide with reserved
  internal columns (`.outcome`, `.study`, `.trt`, `.exposure`) so user
  values cannot be silently overwritten by the standardized frame.
* `set_agd()` applies the same check to its covariate mean/SD columns
  (`.n`, `.r`, `.y`, `.se`, `.study`, `.trt`, `.E`) and additionally
  rejects `cov_means` entries that collapse to duplicate names after
  stripping the `_mean` / `_prop` suffix (e.g. `c("age_mean", "age")`).
* `add_integration()` generates Sobol-sequence quasi-Monte Carlo
  integration points with a Gaussian copula to account for covariate
  correlations, enabling accurate marginalization over the AgD covariate
  distribution.
* `mlumr()`, `add_integration()`, `check_integration()`, and
  `prior_sensitivity()` include `verbose` controls so scripts and
  tests can suppress package-level progress output while retaining warnings.
* The public API mirrors the function names used by the related
  `multinma` package for the data-setup, integration, and
  effect-summary workflow (`set_ipd()`, `set_agd()`,
  `add_integration()`, `unnest_integration()`, `distr()`,
  `marginal_effects()`, `qbern()`/`pbern()`/`dbern()`). Users
  familiar with ML-NMR can transfer their muscle memory directly to
  ML-UMR. When both packages are attached in the same R session R
  issues masking warnings on the shared names; disambiguate with
  `mlumr::function()` / `multinma::function()`.

## Prior system

* Prior constructors `prior_normal(mean, sd)`, `prior_student_t(df, mean, sd)`,
  `prior_cauchy(mean, sd)` (alias for `prior_student_t(df = 1, ...)`), and
  `prior_exponential(rate)`. All six Stan models branch on the prior
  family at runtime, so any of these can be supplied to `prior_intercept`,
  `prior_beta`, or (normal family only) `prior_sigma`.
* `prior_beta` accepts either a single prior (broadcast to all covariates)
  or a list of per-coefficient priors. Per-coefficient priors must share
  the same family and df (Stan branches on a single dist code).
* `prior_normal()`, `prior_student_t()`, and `prior_cauchy()` carry an
  `autoscale` argument. When passed as `prior_beta` with
  `autoscale = TRUE`, each coefficient's prior scale is divided by the
  empirical SD of its covariate (Gelman et al., 2008).
  `autoscale = FALSE` by default.
* Default priors follow the Stan community's prior-choice recommendations
  (Vehtari et al., 2025):
    - `prior_intercept`: `prior_normal(0, 10)`
    - `prior_beta`: `prior_normal(0, 2.5)` (weakly informative; Gelman et al.,
      2008)
    - `prior_sigma`: `prior_normal(0, 2.5)` (half-normal via the `<lower=0>`
      constraint in Stan) for the normal family.
* `default_prior_intercept()`, `default_prior_beta()`, and
  `default_prior_sigma()` accessors expose the package defaults. Values
  are tagged with `$default = TRUE` and the package `$version` so
  `prior_summary()` can report whether each prior is a default and which
  mlumr version produced it.
* `prior_summary()` S3 generic + `prior_summary.mlumr_fit()` method for
  human-readable introspection of every prior used in a fit, including
  post-autoscale per-coefficient scales.
* `prior_sensitivity()` refits a model across a grid of `prior_beta`
  scales and returns a posterior-summary table; the workflow recommended
  by Vehtari et al.'s prior-choice wiki for judging data- vs prior-driven
  inference.

## Inference helpers

* `predict.mlumr_fit()` returns population-specific predicted outcomes.
* `marginal_effects()` returns posterior treatment-effect summaries.
* `conditional_effects()` returns covariate-conditional treatment
  effects.
* `conditional_predict()` returns predictions at specific covariate
  values.
* `predict.mlumr_fit()` and `conditional_effects()` document the
  Jensen's-inequality gap on non-identity links: response-scale summaries
  are `E[g^{-1}(eta)]`, not `g^{-1}(E[eta])`.

## Model comparison

* `calculate_dic()` for DIC-based comparison (no extra dependencies).
* `calculate_loo()` and `calculate_waic()` using the optional `loo`
  package for PSIS-LOO and WAIC (Vehtari, Gelman, Gabry, 2017). `loo`
  is in `Suggests`, not `Imports`.
* `compare_models()` accepts `criterion = c("dic", "loo", "waic")`,
  defaulting to `"dic"`.
* All six Stan models produce pointwise log-likelihood vectors
  (`log_lik_ipd`, `log_lik_agd`); the standard contract for
  `loo::loo()` / `loo::waic()`.

## Sampling

* Regression coefficients `beta` (and `beta_comparator` in relaxed
  models) are sampled via an affine (non-centered) reparameterization:
  `z_beta ~ std_* (0, 1)`, `beta = prior_beta_mean + prior_beta_sd .* z_beta`.
  This decouples HMC adaptation from the prior scale and typically
  improves mixing when the prior scale is mis-matched with the
  posterior scale.

## Diagnostics

* Automatic MCMC diagnostic checks (divergences, Rhat, ESS, treedepth)
  via `check_diagnostics()`.
* `check_integration()` provides a `check_joint` argument that
  compares pairwise correlation matrices at the current vs doubled `n_int`
  (and against the user-supplied correlation target when available).

## Stan internals

* Stan prior hyperparameter declarations are shared across all six
  models via `#include include/priors_hyperparameters.stan` and
  `include/priors_sigma_hyperparameters.stan`. Prior log-density
  dispatchers live in `include/priors_functions.stan`.
* Binary-link numerical helpers are shared by the binary SPFA and relaxed
  models via `include/binary_functions.stan`.
* `E_ipd` in the two Poisson Stan models carries `<lower=0>` so
  off-API consumers who assemble `stan_data` manually get a Stan
  validation error rather than `log(0) = -Inf` on a non-positive
  exposure.
* Internal reference page `?mlumr-numerical-guards` documents
  `safe_logit`, `safe_divide`, and the `<lower=0>` Stan guards.

## Documentation

* A package startup message reports the installed mlumr version and GitHub
  repository when the package is attached.
* `?mlumr-package` provides a full overview of the typical workflow
  (data preparation -> integration -> fit -> diagnostics -> inference) and
  points at the alternative methods (`stc()`, `naive()`).
* `@seealso` cross-links across `predict.mlumr_fit()`,
  `marginal_effects()`, `conditional_effects()`,
  `conditional_predict()`, `prior_summary()`, and
  `prior_sensitivity()`.
* Six vignettes covering data preparation, ML-UMR models, STC and
  naive benchmarks, method comparison, and a complete worked example.
  Vignettes run compact examples during package checks; intentionally failing
  demonstrations and longer production-style fits remain non-executed.

## Testing

* Test coverage spans data setup, integration, link functions, priors,
  prior summaries, prior sensitivity, engine selection, diagnostics,
  prediction, conditional effects, ML-UMR validation, fitted-model behavior,
  STC, naive benchmarks, utility functions, and LOO/WAIC/DIC model
  comparison.
* The test suite includes reserved-name guards, duplicate covariate-name
  checks, standardized-frame shape checks, pointwise log-likelihood
  extraction, integration diagnostics, posterior-summary validation, and
  family-specific behavior for binary, normal, and Poisson outcomes.
