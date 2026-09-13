# The comparator likelihood is a finite equally weighted mixture over the
# integration grid, `log_sum_exp(ll) - log(n_int)`, not the continuously
# integrated likelihood the model is written to mean. Every pseudo-individual
# in an arm sees the same grid, so `m` event rows falling on `k` distinct times
# can be matched at once by `k` nodes: `k` equations in the comparator's
# coefficients, solvable only up to the dimension the grid reaches. The `m`
# spikes then carry `aux^-m` against a coefficient volume of `aux^r`, where
# `r` is the RANK of the matched allocation's design, so the marginal behaves
# as `aux^(r - m)`. Measured over 20 midpoint normal nodes with the
# coefficients integrated against normal priors: 0.000 for two events at
# different times, 1.000 for three at two distinct times, 2.000 for four at
# two, for `lognormal` and `weibull-aft` alike.
#
# Three things follow that earlier versions of this guard got wrong. The power
# is not the largest tie, so (1, 1, 4, 4) is 2 rather than 1. Past the grid's
# reach the equations may have no solution, in which case the best match
# leaves a residual `d > 0` and the profile collapses once the auxiliary falls
# below it, so an ordinary reconstructed curve with many distinct times and
# one repeat is proper and must not be refused. But `d > 0` does not follow
# from the counts: distinct response values are not independent linear
# constraints, and an overdetermined system can be consistent, so the rank of
# the matched design is what the exponent counts and neither `m <= k` nor
# `k > reach` is a certificate of anything.

# `agd_surv` carries left- or interval-censored comparator rows, which the
# column route does not accept: those need a survival::Surv() object.
.comp_stub <- function(agd_time, agd_status,
                       ipd_time = exp(c(-0.4, 0.3, 0.1, 0.7)),
                       ipd_x = c(-0.5, -0.5, 0.5, 0.5),
                       int_distr = NULL, n_int = 32, agd_surv = NULL,
                       agd_entry = NULL) {
  ip <- set_ipd(
    data.frame(trt = "A", time = ipd_time, status = rep(1L, length(ipd_time)),
               x = ipd_x),
    treatment = "trt", covariates = "x", family = "survival",
    time = "time", status = "status"
  )
  ag <- if (is.null(agd_surv)) {
    set_agd_surv(
      data.frame(trt = "B", time = agd_time, status = agd_status,
                 x_mean = 0, x_sd = 0.5),
      treatment = "trt", time = "time", status = "status",
      cov_means = "x_mean", cov_sds = "x_sd", cov_types = "continuous"
    )
  } else {
    set_agd_surv(
      data.frame(trt = "B", x_mean = 0, x_sd = 0.5,
                 ent = agd_entry %||% rep(0, nrow(agd_surv))),
      treatment = "trt", Surv = agd_surv, entry_time = "ent",
      cov_means = "x_mean", cov_sds = "x_sd", cov_types = "continuous"
    )
  }
  d <- combine_data(ip, ag)
  suppressWarnings(
    if (is.null(int_distr)) {
      add_integration(d, n_int = n_int, verbose = FALSE,
                      x = distr(stats::qnorm, mean = x_mean, sd = x_sd))
    } else {
      add_integration(d, n_int = n_int, verbose = FALSE, x = int_distr)
    }
  )
}

check <- function(d, distribution = "lognormal", aux_by = ".study", ...) {
  mlumr:::.check_comparator_tied_events(d, distribution, aux_by = aux_by, ...)
}
msg <- function(...) tryCatch(check(...), error = conditionMessage)
msg_w <- function(d, dist) {
  tryCatch(check(d, distribution = dist), warning = conditionMessage)
}

test_that("the refused power is m - n_distinct, not the largest tie", {
  # Three rows at two distinct times: one repeat, power 1.
  expect_match(msg(.comp_stub(c(1, 1, 4), c(1L, 1L, 1L))),
               "3 event rows at 2 distinct log-times")
  expect_match(msg(.comp_stub(c(1, 1, 4), c(1L, 1L, 1L))),
               "diverges at rate 1")
  # Written as the scale family's own boundary, in one over the auxiliary.
  expect_match(msg(.comp_stub(c(1, 1, 4), c(1L, 1L, 1L))),
               "`\\(1 / sdlog\\)\\^1`")
  # Four rows at the SAME two distinct times: two repeats, power 2. The
  # largest tie is still 2, so a rule keyed on it reports 1 here and is
  # wrong: the two times can be matched at two nodes and all four spikes
  # stand on a ridge pinned in only two directions.
  expect_match(msg(.comp_stub(c(1, 1, 4, 4), c(1L, 1L, 1L, 1L))),
               "4 event rows at 2 distinct log-times")
  expect_match(msg(.comp_stub(c(1, 1, 4, 4), c(1L, 1L, 1L, 1L))),
               "diverges at rate 2")
  # Three at one time is also power 2, by the same arithmetic.
  expect_match(msg(.comp_stub(c(1, 1, 1, 4), c(1L, 1L, 1L, 0L))),
               "diverges at rate 2")
  # And four at one time is power 3.
  expect_match(msg(.comp_stub(c(1, 1, 1, 1), c(1L, 1L, 1L, 1L))),
               "diverges at rate 3")
  expect_match(msg(.comp_stub(c(1, 1, 1, 1), c(1L, 1L, 1L, 1L))),
               "4 event rows at 1 distinct log-time")
})

test_that("distinct times past the grid's reach are not refused", {
  # One covariate, so the comparator reaches 1 + 1 = 2 independent linear
  # predictors. Three distinct event times are three equations with no
  # solution: the best simultaneous match leaves a residual and the profile
  # collapses once the auxiliary falls below it. A repeat inside such a curve
  # is not an improper posterior and refusing it blocks valid fits.
  d <- .comp_stub(c(1, 1, 4, 7), c(1L, 1L, 1L, 1L))
  expect_silent(check(d))
  expect_false(check(d))
  expect_silent(check(d, distribution = "weibull-aft"))
  d5 <- .comp_stub(c(1, 1, 4, 7, 9), rep(1L, 5))
  expect_silent(check(d5))
  # Right at the reach it IS refused: two distinct times, two equations, a
  # solution exists.
  expect_error(check(.comp_stub(c(1, 1, 4), c(1L, 1L, 1L))), "improper")
})

test_that("a real reconstructed curve with a rounding tie is not refused", {
  # The Morgan2012 comparator arm bundled with the package: 263 event times,
  # every one distinct. Duplicate one, which is what rounding to a reported
  # curve's resolution produces, and the largest tie is 2 while the distinct
  # count is 262. A rule keyed on the tie refuses this; the geometry does not,
  # because 262 equations have no solution in 2 coefficients.
  skip_if_not(requireNamespace("mlumr", quietly = TRUE))
  utils::data("ndmm_agd", package = "mlumr", envir = environment())
  ag <- ndmm_agd[ndmm_agd$status == 1L, , drop = FALSE]
  tie <- c(ag$eventtime, ag$eventtime[1L])
  expect_gt(length(unique(tie)), 2L)
  expect_equal(max(table(tie)), 2L)
  d <- .comp_stub(tie, rep(1L, length(tie)))
  expect_silent(check(d))
  expect_false(check(d))
  # The same curve with every time distinct is silent too, which is the
  # baseline the tie has to be read against.
  expect_silent(check(.comp_stub(ag$eventtime, rep(1L, nrow(ag)))))
})

test_that("a degenerate integration grid shrinks the reach", {
  # A covariate integrated as a point mass leaves the grid one reachable
  # linear predictor, the intercept's. Two distinct times are then two
  # equations with no solution and nothing to refuse, while a single repeated
  # time is one equation and still diverges.
  flat <- distr(stats::qunif, min = 1, max = 1)
  expect_silent(check(.comp_stub(c(1, 1, 4), c(1L, 1L, 1L), int_distr = flat)))
  expect_match(msg(.comp_stub(c(1, 1, 1), c(1L, 1L, 1L), int_distr = flat)),
               "grid reaches 1 independent linear predictor")
  expect_error(check(.comp_stub(c(1, 1, 1), c(1L, 1L, 1L), int_distr = flat)),
               "improper")
})

test_that("distinctness is counted on the scale the density matches", {
  # Every covered family but Gompertz matches the linear predictor to
  # `log(time)`. Two distinct doubles can share a logarithm, and counting raw
  # times there reads one target as two, so `k` comes out too large and the
  # guard returns silently on a curve whose spikes all collapse onto one
  # predictor.
  t1 <- 1e300
  t2 <- t1 * (1 + 2^-52)
  expect_true(t1 != t2)
  expect_identical(log(t1), log(t2))
  d <- .comp_stub(c(t1, t2, 4), c(1L, 1L, 1L))
  expect_error(check(d), "improper")
  expect_match(msg(d), "2 distinct log-times")
  # Every family this examines matches log(time); Gompertz, which reads the
  # time scale instead, is not one of them.
})

test_that("the grid's reach uses the exact rank, not a tolerance", {
  # Columns that are independent but badly scaled read as rank-deficient at
  # `qr()`'s default tolerance. The independent direction is there whatever it
  # costs to reach and the coefficient prior is positive where the ridge sits,
  # so nothing about the scaling removes the singularity.
  scaled <- distr(stats::qunif, min = 1e7, max = 1e7 + 2)
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L), int_distr = scaled)
  nodes <- matrix(d$integration_points[1L, , ],
                  nrow = dim(d$integration_points)[2L])
  expect_equal(qr(cbind(1, nodes))$rank, 1L)
  expect_equal(mlumr:::.exact_rank(cbind(1, nodes))$rank, 2L)
  expect_error(check(d), "improper")
  expect_match(msg(d), "grid reaches 2 independent linear predictors")
})

test_that("a censored row is only conclusive where the ridge is a set", {
  # A censored row's own contribution is a mixture over the grid too, so it
  # vanishes only if EVERY node's region probability vanishes.
  flat <- distr(stats::qunif, min = 1, max = 1)
  # `k == reach`: the ridge is isolated points and a censored row can cover
  # them all. Measured on a point-mass grid, two events at t = 1 with a
  # right-censored row at t = 2 collapse instead of diverging, while the same
  # row at t = 0.5 leaves rate +1.000. Deciding which takes enumerating every
  # ridge point, so the arm is reported rather than refused, scale family or
  # not.
  cen <- .comp_stub(c(1, 1, 2), c(1L, 1L, 0L), int_distr = flat)
  w <- expect_warning(check(cen), "neither refused nor passed as proper")
  expect_match(conditionMessage(w), "ridge is isolated")
  expect_false(grepl("bound on both sides", conditionMessage(w), fixed = TRUE))
  expect_true(suppressWarnings(check(cen)))
  # With no censored row there is nothing to suppress it and the refusal is
  # certain.
  expect_error(check(.comp_stub(c(1, 1), c(1L, 1L), int_distr = flat)),
               "improper")
  # `k < reach` leaves a free direction, and a censored row on ONE side
  # cannot suppress an unbounded ridge: moving along it sends some node past
  # any censoring time, which holds that row's mixture at 1 / n_int. Measured
  # at rate +1.000 with the maximum at slope 0.80, past the 0.24 where a node
  # clears log 2.
  expect_error(check(.comp_stub(c(1, 1, 2), c(1L, 1L, 0L))), "improper")
  # All LEFT-censored is the same argument with the sign flipped.
  left <- survival::Surv(c(1, 1, 0.5), c(1, 1, 0), type = "left")
  expect_error(check(.comp_stub(NULL, NULL, agd_surv = left)), "improper")
  # Censoring on BOTH sides is not escapable by pushing one direction, and
  # whether some matched point has unpinned neighbors far enough on each side
  # is a property of the grid: with `n_int = 2` a right-censored row at 2 and
  # a left-censored row at 0.5 collapse whichever point is matched, while
  # either alone leaves rate +1.000, and the same pair on 20 points stays
  # divergent at +1.000. Not settled here, so not refused.
  both <- .comp_stub(NULL, NULL, agd_surv =
    survival::Surv(time = c(1, 1, 2, NA), time2 = c(1, 1, Inf, 0.5),
                   type = "interval2"))
  w2 <- expect_warning(check(both), "neither refused nor passed as proper")
  expect_match(conditionMessage(w2), "bound on both sides")
  expect_match(conditionMessage(w2), "unpinned neighbors on both sides")
  expect_false(grepl("ridge is isolated", conditionMessage(w2), fixed = TRUE))
  # An interval-censored row is two-sided when it opens ABOVE its entry.
  iv <- .comp_stub(NULL, NULL, agd_surv =
    survival::Surv(time = c(1, 1, 1.5), time2 = c(1, 1, 2),
                   type = "interval2"))
  expect_warning(check(iv), "neither refused nor passed as proper")
  # Opening AT its entry makes it one-sided instead: conditioning on survival
  # to the entry piles the mass just above it, and that pile is inside the
  # interval, so a node pushed below clears the row as a left-censored one
  # does. Reading sidedness off the status code alone let this reach the
  # sampler improper.
  aligned <- .comp_stub(NULL, NULL, agd_entry = c(0, 0, 1.5),
    agd_surv = survival::Surv(time = c(1, 1, 1.5), time2 = c(1, 1, 2),
                              event = c(1, 1, 3), type = "interval"))
  expect_equal(aligned$agd$pseudo_ipd$.status, c(1L, 1L, 3L))
  expect_equal(aligned$agd$pseudo_ipd$.start_time[3L],
               aligned$agd$pseudo_ipd$.delay_time[3L])
  expect_error(check(aligned), "improper")
})

test_that("a refusal reports the geometry it is refusing", {
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L))
  e <- msg(d)
  expect_match(e, "improper")
  expect_match(e, "2 equations in the comparator's coefficients")
  expect_match(e, "grid reaches 2 independent linear predictors")
  # The rank of the matched design, which is what the exponent counts.
  expect_match(e, "a matching design of rank 2 exists")
  expect_match(e, "3 event rows against a matched design of rank 2")
  # And it no longer says an absence of repeats is an absence of a ridge.
  expect_false(grepl("all distinct pin the coefficients", e, fixed = TRUE))
  expect_match(e, "not on its own an absence of a ridge")
  expect_match(e, "larger `n_int` is still a finite mixture")
  # The exact-integration counterpart is scoped to what was proved: proper
  # for two tied events, improper from three, and only for this family.
  expect_match(e, "two tied events converge, three or more do not")
  expect_false(grepl("IS proper for these same data", e, fixed = TRUE))
  # The index side really is healthy: this is not the index collapse in
  # disguise, which is the whole point of the finding.
  expect_false(mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                      center = FALSE))
  # The generalized gamma's first auxiliary is a scale too.
  expect_error(check(d, distribution = "gengamma"), "improper")
})

test_that("the rate is measured per family, not shared", {
  # Reading one family's exponent off another is how a wrong rate reaches a
  # message. The height and width of a matched spike differ by family.
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L))
  # Height `shape`, width `1 / shape`: rate `m - rank(D)`. Measured +0.000,
  # +2.000 across `1,4`, `1,1,4`, `1,1,4,4`, for both of these.
  for (dist in c("weibull-aft", "loglogistic")) {
    w <- expect_warning(check(d, distribution = dist), "tail of `prior_aux`")
    expect_match(conditionMessage(w), "diverges at rate 1", fixed = TRUE)
    expect_match(conditionMessage(w), "`shape^1`", fixed = TRUE)
    expect_match(conditionMessage(w), "as one over the shape", fixed = TRUE)
  }
  # Gamma's spike is `sqrt(shape)` high and `1 / sqrt(shape)` wide, so the
  # rate is HALF. For `m` rows on one time the integral is exactly
  # `Gamma(m k) / m^(m k) / Gamma(k)^m`, whose slope in `log k` is
  # `(m - 1) / 2`: 0.500002, 1.000003, 1.500005 for m of 2, 3, 4. Reporting 1
  # here would claim non-integrability against a half-t `prior_aux` with
  # degrees of freedom in (0.5, 1) that does integrate it.
  g <- expect_warning(check(d, distribution = "gamma"),
                      "depends on `prior_intercept` as well as")
  expect_match(conditionMessage(g), "diverges at rate 0.5", fixed = TRUE)
  expect_match(conditionMessage(g), "`shape^0.5`", fixed = TRUE)
  expect_match(conditionMessage(g), "square root of the shape", fixed = TRUE)
  # Two repeats double it, back to a whole number.
  g2 <- expect_warning(check(.comp_stub(c(1, 1, 4, 4), rep(1L, 4)),
                             distribution = "gamma"), "prior_aux")
  expect_match(conditionMessage(g2), "diverges at rate 1.", fixed = TRUE)
  # Gamma's ridge also displaces the intercept by -log(shape), so
  # `prior_aux` is not the whole story: a normal intercept prior contributes
  # exp(-(log shape)^2 / 200) and integrates any polynomial. That factor is
  # 0.95 nats at a shape of 1e6, which is why a measured slope over any
  # reachable range still looks like undamped growth.
  expect_match(conditionMessage(g), "`prior_intercept` as well as",
               fixed = TRUE)
  expect_match(conditionMessage(g), "shifts the comparator intercept",
               fixed = TRUE)
  # Equality integrates rather than failing: a Student-t intercept prior read
  # on the -log(shape) ridge adds (log shape)^-(df + 1) on top of the
  # auxiliary's shape^-(df + 1), and 1 / (shape * (log shape)^(df + 1)) is
  # integrable for every supported intercept prior.
  expect_match(conditionMessage(g), "degrees of freedom of at least 0.5",
               fixed = TRUE)
  expect_match(conditionMessage(g), "Equality integrates", fixed = TRUE)
  # The other two leave the coefficients where they were, so `prior_aux`
  # alone does decide there.
  expect_false(grepl("prior_intercept",
                     msg_w(d, "weibull-aft"), fixed = TRUE))
})

test_that("the moving-ridge families are not examined here", {
  # The PH Weibull and Gompertz widths do not shrink with the auxiliary at
  # all, so their growth is the ROW count whatever the distinct-time count
  # is, and a repeat is not what causes it. Measured with the coefficient
  # priors out: +2.000, +2.000, +3.000 and +4.000 across `1,1`, `1,4`,
  # `1,1,4` and `1,1,4,4`, which is `m` and independent of `k`. What that
  # growth meets is the coefficient priors, through however many coefficients
  # the ridge moves and each of their tails, which is a different question
  # from this one. Reporting it here would attribute to ties something they
  # do not do, so these two are left out until that question is answered.
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L))
  expect_silent(check(d, distribution = "weibull"))
  expect_false(check(d, distribution = "weibull"))
  expect_silent(check(d, distribution = "gompertz"))
  expect_false(check(d, distribution = "gompertz"))
  # Including where every time is distinct, which is the configuration that
  # shows the growth has nothing to do with repeats.
  expect_silent(check(.comp_stub(c(1, 4), c(1L, 1L)), distribution = "weibull"))
})

test_that("only repeated EVENT times in one arm are the problem", {
  # As many distinct times as events is as many equations as spikes, and the
  # volume cancels them: power zero, which integrates.
  expect_silent(check(.comp_stub(c(0.5, 1.5, 2.5, 4), c(1L, 1L, 0L, 1L))))
  expect_false(check(.comp_stub(c(0.5, 1.5, 2.5, 4), c(1L, 1L, 0L, 1L))))
  # Tied CENSORED times contribute a survival probability, not a density
  # spike, so they are not this.
  expect_silent(check(.comp_stub(c(2.5, 2.5, 1, 4), c(0L, 0L, 1L, 1L))))
  # One event alone cannot ride a ridge: its own equation pins the
  # coefficients and the marginal is flat in the auxiliary.
  expect_silent(check(.comp_stub(c(1, 2.5, 3, 4), c(1L, 0L, 0L, 0L))))
  # A distribution with no auxiliary to collapse is not examined.
  expect_silent(check(.comp_stub(c(1, 1, 4), c(1L, 1L, 1L)),
                      distribution = "exponential"))
})

test_that("a shared auxiliary is skipped only where the index bounds it", {
  # Under `aux_by = "none"` the index rows carry the same parameter. A
  # positive index residual contributes exp(-RSS / (2 sdlog^2)), which goes
  # to zero faster than any power of the scale and removes this divergence.
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L))
  bounded <- mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                    aux_by = "none",
                                                    center = FALSE)
  expect_true(attr(bounded, "bounds_aux"))
  expect_silent(check(d, aux_by = "none", index_bounds_aux = TRUE))
  expect_false(check(d, aux_by = "none", index_bounds_aux = TRUE))
  # A saturated index supplies no decaying residual. The index guard only
  # WARNS there and continues, so skipping the comparator whenever the
  # auxiliary is shared sent this improper fit to the sampler in silence.
  sat <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L),
                    ipd_time = exp(c(-0.4, 0.3)), ipd_x = c(-0.5, 0.5))
  # `expect_warning()` hands back the CONDITION, so the return value has to
  # be captured separately or an attribute assertion reads the wrong object
  # and passes on a NULL.
  expect_warning(
    mlumr:::.check_survival_scale_collapse(sat, "lognormal", aux_by = "none",
                                           center = FALSE),
    "share that parameter"
  )
  idx <- suppressWarnings(
    mlumr:::.check_survival_scale_collapse(sat, "lognormal", aux_by = "none",
                                           center = FALSE)
  )
  expect_true(idx)
  expect_false(isTRUE(attr(idx, "bounds_aux")))
  expect_error(check(sat, aux_by = "none", index_bounds_aux = FALSE),
               "improper")
  # The SPFA model shares one `beta`, so under a shared auxiliary an exact
  # index pins the slope to its own solution set while two or more comparator
  # targets pin it to node-specific values. If those do not intersect, one
  # side always carries a positive residual whose decay beats the other's
  # growth, so the combined posterior is proper. That system is not solved
  # here, so the case is reported rather than refused.
  w <- expect_warning(
    check(d, aux_by = "none", index_bounds_aux = FALSE, model = "spfa",
          index_exact = TRUE),
    "neither refused nor passed as proper"
  )
  expect_match(conditionMessage(w), "share one `beta`", fixed = TRUE)
  expect_match(conditionMessage(w), "2 comparator equations", fixed = TRUE)
  # A single distinct target constrains nothing beyond `mu_comparator`, so
  # `beta` stays free, the comparator ridge contains whatever the index needs
  # and both singularities stand at once. Still refused.
  one <- .comp_stub(c(1, 1), c(1L, 1L))
  expect_error(check(one, aux_by = "none", index_bounds_aux = FALSE,
                     model = "spfa", index_exact = TRUE), "improper")
  # Nor does an index that never had an exact design pin anything. Failing to
  # bound the auxiliary does not imply one: the index guard returns before
  # reaching its geometry when the index has no events, and an index of
  # nothing but right-censored rows lets `mu_index` rise above every
  # censoring time, so its likelihood tends to one as the scale falls and the
  # comparator divergence is left whole.
  expect_error(check(d, aux_by = "none", index_bounds_aux = FALSE,
                     model = "spfa", index_exact = FALSE), "improper")
  # But an UNSETTLED index is not that: `undecidable`, `unresolved` and
  # `unresolved_log` do not establish a positive residual, so the index may
  # be exact with a solution set the comparator's node-specific slopes miss,
  # which is the same proper configuration the branch reports. Refusing there
  # would state a certainty the data do not carry.
  expect_warning(check(d, aux_by = "none", index_bounds_aux = FALSE,
                       model = "spfa", index_exact = NA),
                 "neither refused nor passed as proper")
  # And that is what the index guard reports for an eventless index: it
  # returns early, so neither attribute is set.
  eventless <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L), ipd_time = c(2, 3, 4, 5))
  eventless$ipd$data$.status <- 0L
  idx0 <- mlumr:::.check_survival_scale_collapse(eventless, "lognormal",
                                                 aux_by = "none",
                                                 center = FALSE)
  # FALSE, not merely unset: an eventless index is a determination.
  expect_identical(attr(idx0, "index_exact"), FALSE)
  expect_false(isTRUE(attr(idx0, "bounds_aux")))
  # The relaxed model gives the comparator its own coefficients, so the
  # question does not arise.
  expect_error(check(d, aux_by = "none", index_bounds_aux = FALSE,
                     model = "relaxed", index_exact = TRUE), "improper")
  # Nor does it arise once the comparator has its own auxiliary.
  expect_error(check(d, aux_by = ".study", model = "spfa",
                     index_exact = TRUE), "improper")
  # The attribute is set where the index guard does reach an exact design.
  expect_true(attr(idx, "index_exact"))
  # `.study` and NULL are the same stratification and both are examined,
  # whatever the index did.
  expect_error(check(d, aux_by = NULL, index_bounds_aux = TRUE), "improper")
  expect_error(check(d, aux_by = ".study", index_bounds_aux = TRUE),
               "improper")
})

# ---- the counts are not certificates -------------------------------------

# Distinct event times, no repeat anywhere, and more of them than the grid's
# rank: both of the counts the guard used to test say "nothing to see", and
# both are wrong. The times 1, 2, 4 are carried onto the nodes 1, 2, 3 by
# `b = (-log 2, log 2)` exactly, a design of rank 2 against 3 rows, and the
# marginal slope measures -1.0000 per decade of `sdlog`.
.uniform_stub <- function(times, n_int = 8) {
  ip <- set_ipd(
    data.frame(trt = "A", time = c(0.8, 1.2, 2, 2.5), status = rep(1L, 4),
               x = c(0, 0, 1, 1)),
    treatment = "trt", covariates = "x", family = "survival",
    time = "time", status = "status"
  )
  ag <- set_agd_surv(
    data.frame(trt = "B", time = times, status = rep(1L, length(times)),
               x_mean = 2, x_sd = 4 / sqrt(12)),
    treatment = "trt", time = "time", status = "status",
    cov_means = "x_mean", cov_sds = "x_sd", cov_types = "continuous"
  )
  suppressWarnings(add_integration(combine_data(ip, ag), n_int = n_int,
                                   verbose = FALSE,
                                   x = distr(stats::qunif, min = 0, max = 4)))
}

test_that("an overdetermined system that is consistent is still refused", {
  d <- .uniform_stub(c(1, 2, 4))
  # The grid really does contain the three nodes, from the ordinary public
  # `add_integration()` call rather than a hand-built one.
  grid <- as.numeric(d$integration_points[1, , 1])
  expect_true(all(c(1, 2, 3) %in% grid))
  expect_equal(as.vector(cbind(1, c(1, 2, 3)) %*% c(-log(2), log(2))),
               log(c(1, 2, 4)))
  # Three distinct targets, so `m <= k` and `k > reach` both held and both
  # used to skip the arm in silence.
  e <- msg(d)
  expect_match(e, "3 event rows at 3 distinct log-times")
  expect_match(e, "an overdetermined system can still be consistent")
  expect_match(e, "a matching design of rank 2 exists")
  expect_match(e, "diverges at rate 1")
  # Repeats on top of it raise `m` and not the rank.
  expect_match(msg(.uniform_stub(c(1, 1, 2, 4))), "diverges at rate 2")
})

test_that("a target the grid cannot carry is still passed in silence", {
  # The same grid and the same count of distinct times, but `log(3)` is not
  # on any line through the others' nodes, so the best match leaves a real
  # residual and there is no ridge to refuse.
  expect_false(check(.uniform_stub(c(1, 2, 3))))
  expect_false(check(.uniform_stub(c(1, 2, 3, 5))))
  # And the certificate declines to answer rather than guessing where it
  # cannot enumerate: more than one covariate is not decided here.
  expect_true(is.na(mlumr:::.grid_hits_targets(cbind(1:3, 1:3), log(c(1, 2)))))
  expect_true(is.na(mlumr:::.grid_hits_targets(NULL, log(c(1, 2)))))
})

test_that("only an exact determinant certifies a match", {
  g <- mlumr:::.grid_hits_targets
  m <- function(v) matrix(v, ncol = 1L)
  # Consistency is read off the determinant of the original data, not off
  # predictions rebuilt from a fitted slope. The round trip is not exact even
  # where the geometry is: `log(4) - log(2) * 2` is zero while
  # `-log(2) + log(2) * 3 == log(4)` is FALSE.
  expect_true(log(4) - log(2) * 2 == 0)
  expect_false(-log(2) + log(2) * 3 == log(4))
  expect_true(g(m(c(1, 2, 3)), log(c(1, 2, 4))))
  # A residual that is merely small is a near miss, and no affine map removes
  # it. Certifying it would refuse a proper fit, so it reports undecided.
  expect_true(is.na(g(m(c(1, 2, 3)), c(0, 1, 2 + 1e-15))))
  # Two nodes a hair apart make the reconstructed slope enormous, so any
  # tolerance scaled by the terms of a prediction swallows a gross miss: on
  # `(1, 1 + 2^-52, 2)` the target at 2 was accepted against a prediction of
  # 1. The determinant is built from differences of the inputs and has no
  # slope in it, so it is not fooled.
  expect_false(isTRUE(g(m(c(1, 1 + 2^-52, 2)), c(0, 1, 2))))
  expect_false(isTRUE(g(m(c(0, 2^-60, 1)), c(0, 1, 2))))
  # Magnitude alone does not prevent a certificate when the geometry is exact.
  expect_true(g(m(c(1, 2, 3) * 2^40), c(0, 1, 2)))
  # A target no line carries is a plain no.
  expect_false(g(m(c(1, 2, 3)), log(c(1, 2, 3))))
  # A COMPUTED zero is not an exact zero. Both products are rounded before
  # the subtraction, so a genuinely nonzero determinant can cancel to 0.
  # These nodes and targets compute 0 while the determinant of those very
  # doubles is -3.4958e-17, and no permutation of them is an affine match.
  zr <- c(0, 0.3961039261018525, 1.04621481495181)
  ur <- c(0, 0.6209825942831111, 1.6401786176669797)
  expect_equal((ur[3] - ur[1]) * (zr[2] - zr[1]) -
                 (ur[2] - ur[1]) * (zr[3] - zr[1]), 0)
  expect_false(isTRUE(g(m(zr), ur)))
})

test_that("the exact-arithmetic probes report what they promise", {
  ts <- mlumr:::.two_sum_err
  tp <- mlumr:::.two_prod_err
  # `a + b == s + e` and `a * b == p + e`, exactly. An error of zero is the
  # claim that the operation was exact, and that is all the guard asks.
  # 2^-52 is the last bit of 1, so that sum is exact and 2^-60 is not.
  expect_true(ts(1, 2^-52, 1 + 2^-52) == 0)
  expect_true(ts(1, 2^-60, 1 + 2^-60) != 0)
  expect_equal(ts(1, 2^-60, 1 + 2^-60), 2^-60)
  expect_true(tp(2, 3, 6) == 0)                     # small integers
  expect_true(tp(log(2), 2, log(2) * 2) == 0)       # scaling by a power of 2
  # `(1 + 2^-52)^2` is `1 + 2^-51 + 2^-104`, whose last term falls off the
  # end, so the product is inexact by exactly that term.
  a <- 1 + 2^-52
  expect_true(tp(a, a, a * a) != 0)
  expect_equal(tp(a, a, a * a), 2^-104)
  expect_equal(a * a, 1 + 2^-51)
  # The product transformation fails quietly at the bottom of the range: a
  # product that underflows takes its half-products with it, so the error
  # comes back zero while `p` is not `a * b`. That must not read as exact.
  expect_equal(6.661338147750939e-16 * 1e-310, 0)
  expect_true(is.nan(tp(6.661338147750939e-16, 1e-310, 0)))
  # A zero operand gives a genuinely exact zero and is not affected.
  expect_true(tp(0, 1e-310, 0) == 0)
  expect_true(tp(1e-310, 0, 0) == 0)
})

test_that("any positive same-profile gap is a conflict", {
  b <- mlumr:::.censoring_bounds_aux
  X <- cbind(1, c(0, 0))
  ev <- c(FALSE, FALSE)
  f <- function(lo, up) b(X, c(0, 0), ev, lower = lo, upper = up)
  # These ends are stored observation times, not the output of a solve, so
  # there is no rounding to discount. A gap of any size bounds: with ends `d`
  # apart the pair contributes `exp(-(d / (2 s))^2)`, and
  # `integral s^-m exp(-(d / (2 s))^2)` converges at zero for every `d > 0`.
  # Discarding a rounding-sized gap refused proper fits.
  expect_identical(f(c(-Inf, 1e-15), c(0, Inf)), "bounded")
  expect_identical(f(c(-Inf, log(4)), c(0, Inf)), "bounded")
  # The decay only shows once `s` falls below `d`, which is why no slope
  # measured above that range sees it.
  d <- 1e-15
  at <- function(s) 2 * stats::pnorm(-d / (2 * s), log.p = TRUE) + 2 * log(1 / s)
  expect_gt(at(1e-15), 0)      # still growing
  expect_lt(at(1e-17), -2000)  # collapsed
  # Exact equality is not a conflict, and it is not freedom either. The
  # shared predictor has to sit ON that point, so the coefficients keeping
  # the group positive are a shrinking neighborhood of a hyperplane, and the
  # volume they cost is what the auxiliary sees: a half at every scale
  # pointwise, one power of the scale once the intercept is integrated out.
  touching <- f(c(-Inf, 0), c(0, Inf))
  expect_identical(as.character(touching), "suppresses")
  expect_identical(attr(touching, "order"), 1L)
  # A region with interior is the case that really contributes a constant.
  expect_identical(f(c(-Inf, 0), c(log(4), Inf)), "unbounded")
  # The order is the measured one. With `mu ~ N(0, a^2)` the integrated
  # index likelihood is `arccos(a^2 / (a^2 + s^2)) / (2 pi)`, which is
  # `s / (sqrt(2) pi a)` near zero.
  a <- 10
  lbar <- function(s) acos(a^2 / (a^2 + s^2)) / (2 * pi)
  slope <- (log(lbar(1e-4)) - log(lbar(1e-3))) / (log(1e-4) - log(1e-3))
  expect_equal(slope, 1, tolerance = 1e-6)
  expect_equal(lbar(1e-3),
               stats::integrate(function(mu) {
                 stats::pnorm(-mu / 1e-3) * stats::pnorm(mu / 1e-3) *
                   stats::dnorm(mu, 0, a)
               }, -Inf, Inf, rel.tol = 1e-12)$value,
               tolerance = 1e-6)
  # Two independent touching profiles pin two directions, so the order is
  # the rank of those rows rather than a flag.
  X2 <- cbind(1, c(0, 0, 1, 1))
  two <- b(X2, rep(0, 4L), rep(FALSE, 4L),
           lower = c(-Inf, 0, -Inf, 0), upper = c(0, Inf, 0, Inf))
  expect_identical(as.character(two), "suppresses")
  expect_identical(attr(two, "order"), 2L)
})

test_that("a determinant that is not finite answers instead of aborting", {
  g <- mlumr:::.grid_hits_targets
  # Finite nodes and finite targets can still overflow their products. Both
  # determinant terms go to infinity here, so the determinant is NaN and the
  # tolerance is Inf, and `abs(NaN) <= Inf` is NA. Comparing on that aborted
  # the fit rather than returning the undecided answer the budget and the
  # wider designs already return.
  expect_true(is.na(abs(NaN) <= Inf))
  expect_no_error(g(matrix(c(0, 5e307, 1e308), ncol = 1L), c(-700, 0, 700)))
  expect_false(isTRUE(g(matrix(c(0, 5e307, 1e308), ncol = 1L),
                        c(-700, 0, 700))))
})

test_that("an underflowed determinant does not certify a match", {
  g <- mlumr:::.grid_hits_targets
  # Both determinant products underflow to zero here, so a determinant test
  # that trusted a computed zero would certify a ridge that does not exist.
  expect_false(isTRUE(g(matrix(c(0, 1e-310, 2e-310), ncol = 1L),
                        c(0, 2^-52, 3 * 2^-52))))
})

test_that("the enumeration cutoff declines instead of overflowing", {
  g <- mlumr:::.grid_hits_targets
  # `n` and `length(u)` are integers, so `n * n * (n + length(u))` overflows
  # at the grid sizes this cutoff exists to decline: 2048 gives about 8.6e9,
  # which becomes NA, and `if (NA)` aborted the fit with "missing value where
  # TRUE/FALSE needed" rather than returning the undecided answer.
  expect_true(is.na(suppressWarnings(2048L * 2048L * 2051L)))
  # A decline is LABELED, because it is the only one of the three ways this
  # answers NA that makes the same data answerable at one `n_int` and
  # unexamined at another. The cost is `n * n * (k - 2)`, so a wide grid
  # against many targets is what reaches the cutoff.
  wide <- g(matrix(seq_len(512), ncol = 1L), as.numeric(seq_len(162)))
  expect_true(is.na(wide))
  expect_identical(attr(wide, "declined"), "budget")
  # A grid inside the budget is still decided, and the budget now covers the
  # ordinary resolutions: the cost model used to charge `n * n * (n + k)`,
  # which capped this near 170 nodes and left an `n_int` of 256 unexamined
  # on the arm that 8 nodes refused.
  expect_true(g(matrix(c(1, 2, 3), ncol = 1L), log(c(1, 2, 4))))
  big <- g(matrix(c(seq_len(3), 3 + seq_len(253) / 4), ncol = 1L),
           log(c(1, 2, 4)))
  expect_true(big)
  expect_null(attr(big, "declined"))
  # The other two ways of answering NA are not declines: more than one
  # covariate is the same answer at every grid size, and a close-but-inexact
  # candidate means the enumeration RAN and certified nothing.
  expect_null(attr(g(matrix(1:6, ncol = 2L), log(c(1, 2, 4))), "declined"))
  near <- g(matrix(c(1, 2, 3), ncol = 1L), c(0, 1, 2 + 1e-15))
  expect_true(is.na(near))
  expect_null(attr(near, "declined"))
})

test_that("the refusal survives the public mlumr() call", {
  d <- .uniform_stub(c(1, 2, 4))
  fit <- function() {
    suppressWarnings(mlumr(d, model = "relaxed", distribution = "lognormal",
                           center = FALSE, qr = FALSE, engine = "rstan",
                           seed = 2026, verbose = FALSE, refresh = 0))
  }
  expect_error(fit(), "improper")
})

# ---- an index with no events is not automatically a verdict ----------------

.eventless_stub <- function(sv, x = c(0, 0)) {
  ip <- suppressWarnings(set_ipd(
    data.frame(trt = "A", x = x),
    treatment = "trt", covariates = "x", family = "survival", Surv = sv
  ))
  ag <- set_agd_surv(
    data.frame(trt = "B", time = c(1, 1), status = c(1L, 1L),
               x_mean = 0, x_sd = 1),
    treatment = "trt", time = "time", status = "status",
    cov_means = "x_mean", cov_sds = "x_sd", cov_types = "continuous"
  )
  suppressWarnings(add_integration(combine_data(ip, ag), n_int = 8,
                                   verbose = FALSE,
                                   x = distr(stats::qnorm, mean = x_mean,
                                             sd = x_sd)))
}

test_that("conflicting censored rows bound a shared auxiliary with no events", {
  # One subject's event is known to fall at or before 1, another's after 4, at
  # the same covariate profile. No linear predictor satisfies both, so
  # `sup_mu L = Phi(-log(4) / (2 sdlog))^2`, which beats the comparator's
  # `sdlog^-2`: the joint posterior is proper and must not be refused.
  d <- .eventless_stub(survival::Surv(time = c(NA, 4), time2 = c(1, Inf),
                                      type = "interval2"))
  expect_equal(as.integer(d$ipd$data$.status), c(2L, 0L))
  idx <- mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                aux_by = "none", center = FALSE)
  expect_true(isTRUE(attr(idx, "bounds_aux")))
  # Which is what stops the comparator guard, so the fit goes through.
  expect_false(check(d, aux_by = "none", model = "relaxed",
                     index_bounds_aux = TRUE))
})

test_that("an eventless index that pins nothing is still reported", {
  # All right-censored: raising the intercept clears every one of them at
  # once, the index likelihood tends to one at the boundary, and the
  # comparator divergence is left whole.
  for (x in list(c(0, 0), c(0, 1))) {
    d <- .eventless_stub(survival::Surv(time = c(2, 4), event = c(0, 0)), x)
    idx <- mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                  aux_by = "none",
                                                  center = FALSE)
    expect_null(attr(idx, "bounds_aux"))
    expect_false(attr(idx, "index_exact"))
  }
  # Two-sided but compatible is the same answer: a predictor between the two
  # ends satisfies both rows.
  d <- .eventless_stub(survival::Surv(time = c(NA, 1), time2 = c(4, Inf),
                                      type = "interval2"))
  idx <- mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                aux_by = "none", center = FALSE)
  expect_false(attr(idx, "index_exact"))
})

test_that("an ordinary integration resolution does not erase the verdict", {
  # The same three comparator events over the same declared covariate. The
  # enumeration used to be charged `n * n * (n + k)`, the cost of a scalar
  # inner loop, which capped it near 170 nodes: at `n_int = 8` this arm was
  # refused and at 256 it ran to the sampler with nothing in the result
  # saying the question had gone unasked. The cost is `n * n * (k - 2)`.
  for (n_int in c(8L, 64L, 256L)) {
    d <- .uniform_stub(c(1, 2, 4), n_int = n_int)
    grid <- as.numeric(d$integration_points[1, , 1])
    expect_true(all(c(1, 2, 3) %in% grid))
    expect_length(unique(grid), n_int)
    expect_match(msg(d), "a matching design of rank 2 exists")
    expect_match(msg(d), "diverges at rate 1")
  }
})

test_that("a declined enumeration is reported, not passed in silence", {
  # Past the budget the question really is unasked, and that is a third
  # state: not a proved incompatibility and not a certificate of propriety.
  # 512 nodes against 162 distinct targets is `512 * 512 * 160`, over the
  # cutoff, and the helper declines before enumerating anything.
  wide <- mlumr:::.grid_hits_targets(matrix(seq_len(512), ncol = 1L),
                                     as.numeric(seq_len(162)))
  expect_identical(attr(wide, "declined"), "budget")
  # Which the caller turns into a warning rather than the silent `next` that
  # made the same data answerable at one grid size and not at another.
  d <- .comp_stub(as.numeric(seq_len(162)), rep(1L, 162), n_int = 512)
  w <- tryCatch(check(d), warning = conditionMessage)
  expect_match(w, "left unexamined")
  expect_match(w, "enumeration budget")
  expect_match(w, "not a certificate")
  # And it says so without claiming the posterior is improper.
  expect_no_match(w, "is therefore improper")
  # An arm inside the budget still answers silently.
  expect_silent(check(.comp_stub(c(1, 1, 4, 7), rep(1L, 4))))
})

test_that("a touching eventless index cancels one power of the comparator", {
  # A left-censored row at `t = 1` beside a right-censored row at `t = 1` on
  # one covariate profile. Pointwise the pair peaks at `1/4` at every scale,
  # which is why this used to read as "contributes nothing"; integrating the
  # intercept out against `normal(0, a)` gives
  # `arccos(a^2 / (a^2 + s^2)) / (2 pi)`, which is `s / (sqrt(2) pi a)` near
  # zero. One power of the scale, against the comparator's one over it.
  d <- .eventless_stub(survival::Surv(time = c(NA, 1), time2 = c(1, Inf),
                                      type = "interval2"))
  expect_equal(as.integer(d$ipd$data$.status), c(2L, 0L))
  idx <- mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                aux_by = "none",
                                                center = FALSE)
  expect_null(attr(idx, "bounds_aux"))
  expect_identical(attr(idx, "aux_order"), 1)
  # Two tied comparator events are rate 1, so the net is 0 and the fit
  # stands. This is the refusal the joint order removes.
  expect_silent(check(d, aux_by = "none", model = "relaxed",
                      index_aux_order = 1))
  expect_false(check(d, aux_by = "none", model = "relaxed",
                     index_aux_order = 1))
  # Reading the index as a flag instead of an order is what refused it.
  expect_match(msg(d, aux_by = "none", model = "relaxed"),
               "is therefore improper")
  # And the cancellation is one power, not an exemption: a third tied
  # comparator event leaves `2 - 1 = 1` and is still refused.
  three <- .eventless_stub(survival::Surv(time = c(NA, 1), time2 = c(1, Inf),
                                          type = "interval2"))
  three$agd$pseudo_ipd <- three$agd$pseudo_ipd[c(1, 2, 2), , drop = FALSE]
  expect_match(msg(three, aux_by = "none", model = "relaxed",
                   index_aux_order = 1), "diverges at rate 1")
  # Under `aux_by = ".study"` the comparator has its own auxiliary and the
  # index order is not consulted at all.
  expect_match(msg(d, aux_by = ".study", index_aux_order = 1),
               "is therefore improper")
})

test_that("an index order that was not settled is reported, not netted", {
  # The order was derived and measured for the normal on the log scale. The
  # other families' rates are written in powers of the same width, so the
  # same subtraction should hold, but it has not been measured for them and
  # an unmeasured exponent is not a certificate. They report instead.
  d <- .eventless_stub(survival::Surv(time = c(NA, 1), time2 = c(1, Inf),
                                      type = "interval2"))
  idx <- mlumr:::.check_survival_scale_collapse(d, "gengamma",
                                                aux_by = "none",
                                                center = FALSE)
  expect_true(is.na(attr(idx, "aux_order")))
  w <- tryCatch(check(d, distribution = "gengamma", aux_by = "none",
                      model = "relaxed", index_aux_order = NA_real_),
                warning = conditionMessage)
  expect_match(w, "was not settled")
  expect_no_match(w, "is therefore improper")
})

test_that("a shared slope pinned by the index blocks the censoring escape", {
  # SPFA shares one `beta` between the arms. Index events at `x = -1` and
  # `x = +1` both at `t = 1` force `mu_index` and `beta` to zero, so every
  # integration point sits at `mu_comparator` and a comparator right-censored
  # row at `t = 2` is above all of them. The escape along the ridge that a
  # comparator read alone would have is not available: that direction is the
  # slope the index pins.
  pinned <- .comp_stub(c(1, 1, 2), c(1L, 1L, 0L),
                       ipd_time = c(1, 1), ipd_x = c(-1, 1), n_int = 8)
  w <- tryCatch(check(pinned, aux_by = "none", model = "spfa",
                      index_exact = TRUE,
                      index_design = cbind(1, c(-1, 1))),
                warning = conditionMessage)
  expect_match(w, "the shared `beta`")
  expect_match(w, "neither refused nor passed as proper")
  expect_no_match(w, "is therefore improper")
  # One distinct comparator time, so the rank is 1 and the old gate
  # (`rank_d > 1`) left this to the refusal. The censored row is not an
  # event row and does not count toward `m`.
  expect_match(w, "2 event rows at 1 distinct log-time")
  # Remove the censored row and the escape question is moot: nothing
  # suppresses the ridge and the refusal is right.
  bare <- .comp_stub(c(1, 1), c(1L, 1L), ipd_time = c(1, 1),
                     ipd_x = c(-1, 1), n_int = 8)
  expect_match(msg(bare, aux_by = "none", model = "spfa", index_exact = TRUE,
                   index_design = cbind(1, c(-1, 1))),
               "is therefore improper")
  # And it is specific to the shared slope: under `relaxed` the comparator
  # carries its own `beta_comparator`, which the index does not pin.
  expect_match(msg(pinned, aux_by = "none", model = "relaxed"),
               "is therefore improper")
})

test_that("a shared slope is not netted against the index's own order", {
  # The subtraction assumes the two sides pin INDEPENDENT directions, which
  # is a property of the model. Under `relaxed` the index constrains
  # `mu_index` and `beta` while the comparator constrains `mu_comparator`
  # and `beta_comparator`, so the stacked system is block diagonal and the
  # rank is exactly the sum. Under `spfa` they share `beta` and can overlap.
  #
  # Two independent touching index profiles give order 2, and four
  # comparator events at two matched times give rate 2. If both sides pin
  # the shared slope the stacked rank gains only one index direction, so the
  # true rate is 1 and the fit is improper, while a full subtraction reports
  # 0 and admits it.
  ip <- suppressWarnings(set_ipd(
    data.frame(trt = "A", x = c(0, 0, 1, 1)),
    treatment = "trt", covariates = "x", family = "survival",
    Surv = survival::Surv(time = c(NA, 1, NA, 1), time2 = c(1, Inf, 1, Inf),
                          type = "interval2")
  ))
  ag <- set_agd_surv(
    data.frame(trt = "B", time = c(1, 1, 4, 4), status = rep(1L, 4),
               x_mean = 0, x_sd = 1),
    treatment = "trt", time = "time", status = "status",
    cov_means = "x_mean", cov_sds = "x_sd", cov_types = "continuous"
  )
  d <- suppressWarnings(add_integration(combine_data(ip, ag), n_int = 8,
                                        verbose = FALSE,
                                        x = distr(stats::qnorm, mean = x_mean,
                                                  sd = x_sd)))
  idx <- mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                aux_by = "none",
                                                center = FALSE)
  expect_identical(attr(idx, "aux_order"), 2)
  # `relaxed` gives the comparator its own slope, so the blocks are disjoint
  # and `2 - 2 = 0` is exact.
  expect_false(check(d, aux_by = "none", model = "relaxed",
                     index_aux_order = 2))
  # `spfa` shares it, so this reports instead of admitting it in silence.
  w <- tryCatch(check(d, aux_by = "none", model = "spfa",
                      index_aux_order = 2), warning = conditionMessage)
  expect_match(w, "share one `beta`")
  expect_match(w, "do not simply add")
  expect_no_match(w, "is therefore improper")
  # A zero order is nothing to overlap, so a shared slope changes nothing.
  bare <- .comp_stub(c(1, 1, 4, 4), rep(1L, 4))
  expect_match(msg(bare, aux_by = "none", model = "spfa",
                   index_exact = FALSE, index_aux_order = 0),
               "is therefore improper")
})

test_that("an index residual this check could not resolve is not a zero", {
  # `unresolved`, `unresolved_log` and `undecidable` did not establish
  # whether the index leaves a residual. A real one contributes
  # `exp(-RSS / (2 sdlog^2))` and removes the comparator's growth entirely,
  # so reading them as "contributes nothing" turns an open question into a
  # refusal. Only a design shown to reproduce its own times is a zero.
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L))
  expect_match(msg(d, aux_by = "none", model = "relaxed",
                   index_aux_order = 0), "is therefore improper")
  w <- tryCatch(check(d, aux_by = "none", model = "relaxed",
                      index_aux_order = NA_real_), warning = conditionMessage)
  expect_match(w, "could not resolve")
  expect_no_match(w, "is therefore improper")
  # The index guard now says which of the two it established. An exact index
  # design pins rather than suppresses, so it reports a zero.
  exact <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L),
                      ipd_time = c(1, 2), ipd_x = c(0, 1))
  idx <- suppressWarnings(
    mlumr:::.check_survival_scale_collapse(exact, "lognormal",
                                           aux_by = "none", center = FALSE)
  )
  expect_true(attr(idx, "index_exact"))
  expect_identical(attr(idx, "aux_order"), 0)
})

test_that("an exact index fit that leaves the slope free does not pin it", {
  # Reproducing its own times is not identifying the shared slope, and the
  # comparator needs the second. Repeated index events at ONE covariate
  # profile at one time fit exactly and pin only `mu_index`; `beta` stays
  # free, and a free `beta` is exactly the direction the comparator tilts
  # along to lift an integration point past a censoring time.
  free <- suppressWarnings(.comp_stub(c(1, 1, 2), c(1L, 1L, 0L),
                                      ipd_time = c(1, 1), ipd_x = c(0, 0),
                                      n_int = 8))
  idx <- suppressWarnings(
    mlumr:::.check_survival_scale_collapse(free, "lognormal", aux_by = "none",
                                           center = FALSE)
  )
  expect_true(attr(idx, "index_exact"))
  expect_equal(unname(attr(idx, "index_design")), cbind(1, c(0, 0)))
  # So the escape is available and the refusal is right.
  expect_match(msg(free, aux_by = "none", model = "spfa", index_exact = TRUE,
                   index_design = cbind(1, c(0, 0))),
               "is therefore improper")
  # The same data with index covariates that DO identify the slope reports.
  pins <- .comp_stub(c(1, 1, 2), c(1L, 1L, 0L),
                     ipd_time = c(1, 1), ipd_x = c(-1, 1), n_int = 8)
  idx2 <- suppressWarnings(
    mlumr:::.check_survival_scale_collapse(pins, "lognormal", aux_by = "none",
                                           center = FALSE)
  )
  expect_equal(unname(attr(idx2, "index_design")), cbind(1, c(-1, 1)))
  w <- tryCatch(check(pins, aux_by = "none", model = "spfa",
                      index_exact = TRUE,
                      index_design = cbind(1, c(-1, 1))),
                warning = conditionMessage)
  expect_match(w, "the shared `beta`")
  # A design with fewer rows than coefficients cannot pin them either, even
  # when it is saturated.
  short <- suppressWarnings(.comp_stub(c(1, 1, 2), c(1L, 1L, 0L),
                                       ipd_time = 1, ipd_x = 0, n_int = 8))
  idx3 <- suppressWarnings(
    mlumr:::.check_survival_scale_collapse(short, "lognormal", aux_by = "none",
                                           center = FALSE)
  )
  expect_equal(unname(attr(idx3, "index_design")), cbind(1, 0))
  expect_match(msg(short, aux_by = "none", model = "spfa", index_exact = TRUE,
                   index_design = cbind(1, 0)), "is therefore improper")
  # But identification is tested on the directions the GRID spans, not on
  # every column. A covariate integrated as a point mass contributes no
  # escape direction, so leaving its coefficient unidentified costs the
  # comparator nothing and requiring full column rank refused that fit.
  ip <- suppressWarnings(set_ipd(
    data.frame(trt = "A", time = c(1, 1), status = c(1L, 1L),
               x1 = c(-1, 1), x2 = c(0, 0)),
    treatment = "trt", covariates = c("x1", "x2"), family = "survival",
    time = "time", status = "status"
  ))
  ag <- set_agd_surv(
    data.frame(trt = "B", time = c(1, 1, 2), status = c(1L, 1L, 0L),
               x1_mean = 0, x1_sd = 0.5, x2_mean = 0, x2_sd = 0),
    treatment = "trt", time = "time", status = "status",
    cov_means = c("x1_mean", "x2_mean"), cov_sds = c("x1_sd", "x2_sd"),
    cov_types = c("continuous", "continuous")
  )
  flat <- suppressWarnings(add_integration(
    combine_data(ip, ag), n_int = 8, verbose = FALSE, cor = diag(2),
    x1 = distr(stats::qnorm, mean = x1_mean, sd = x1_sd),
    x2 = distr(stats::qunif, min = 0, max = 0)
  ))
  # The grid really is flat in x2, so no node moves along that direction.
  expect_true(all(flat$integration_points[1, , 2] == 0))
  design <- cbind(1, c(-1, 1), c(0, 0))
  # Full column rank is 3 and this design has rank 2, so the old test said
  # it pinned nothing, while the only direction the grid moves along is x1
  # and the index does pin that.
  expect_identical(mlumr:::.exact_rank(design)$rank, 2L)
  w2 <- tryCatch(check(flat, aux_by = "none", model = "spfa",
                       index_exact = TRUE, index_design = design),
                 warning = conditionMessage)
  expect_match(w2, "the shared `beta`")
  expect_no_match(w2, "is therefore improper")
})

test_that("two-sided censoring still reports when the slope is not pinned", {
  # `spfa_pinned` and `two_sided` are different reasons for the same
  # undecided answer, and gating the second on the first would refuse an arm
  # that the reach argument alone already leaves open. An index that fits
  # exactly without identifying the slope is not pinned, and two-sided
  # comparator censoring is still two-sided.
  sv <- survival::Surv(time = c(1, 1, NA, 2), time2 = c(1, 1, 0.5, Inf),
                       type = "interval2")
  two <- suppressWarnings(.comp_stub(NULL, NULL, ipd_time = c(1, 1),
                                     ipd_x = c(0, 0), n_int = 2,
                                     agd_surv = sv))
  w <- tryCatch(check(two, aux_by = "none", model = "spfa",
                      index_exact = TRUE,
                      index_design = cbind(1, c(0, 0))),
                warning = conditionMessage)
  expect_match(w, "they bound on both sides")
  expect_no_match(w, "is therefore improper")
})
