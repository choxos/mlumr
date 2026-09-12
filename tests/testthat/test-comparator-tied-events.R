# The comparator likelihood is a finite equally weighted mixture over the
# integration grid, `log_sum_exp(ll) - log(n_int)`, not the continuously
# integrated likelihood the model is written to mean. Every pseudo-individual
# in an arm sees the same grid, so `m` event rows falling on `k` distinct times
# can be matched at once by `k` nodes: `k` equations in the comparator's
# coefficients, solvable only up to the dimension the grid reaches. The `m`
# spikes then carry `aux^-m` against a coefficient volume of `aux^k`, so the
# marginal behaves as `aux^(k - m)`. Measured over 20 midpoint normal nodes
# with the coefficients integrated against their default priors: 0.000 for two
# events at different times, 1.000 for three at two distinct times, 2.000 for
# four at two, for `lognormal` and `weibull-aft` alike.
#
# Two things follow that the first version of this guard got wrong. The power
# is `m - k` and not the largest tie, so (1, 1, 4, 4) is 2 rather than 1. And
# past the grid's reach the `k` equations have no solution at all: the best
# match leaves a residual `d > 0` and the profile collapses once the auxiliary
# falls below it, so an ordinary reconstructed curve with many distinct times
# and one repeat is proper and must not be refused.

# `agd_surv` carries left- or interval-censored comparator rows, which the
# column route does not accept: those need a survival::Surv() object.
.comp_stub <- function(agd_time, agd_status,
                       ipd_time = exp(c(-0.4, 0.3, 0.1, 0.7)),
                       ipd_x = c(-0.5, -0.5, 0.5, 0.5),
                       int_distr = NULL, n_int = 32, agd_surv = NULL) {
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
      data.frame(trt = "B", x_mean = 0, x_sd = 0.5)[rep(1L, nrow(agd_surv)), ],
      treatment = "trt", Surv = agd_surv,
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
  # An interval-censored row is two-sided by itself.
  iv <- .comp_stub(NULL, NULL, agd_surv =
    survival::Surv(time = c(1, 1, 1.5), time2 = c(1, 1, 2),
                   type = "interval2"))
  expect_warning(check(iv), "neither refused nor passed as proper")
})

test_that("a refusal reports the geometry it is refusing", {
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L))
  e <- msg(d)
  expect_match(e, "improper")
  expect_match(e, "matched at once by 2 integration points")
  expect_match(e, "2 equations in the comparator's coefficients")
  expect_match(e, "grid reaches 2 independent linear predictors")
  expect_match(e, "all distinct pin the coefficients")
  expect_match(e, "larger `n_int` is still a finite mixture")
  expect_match(e, "restriction on the quadrature")
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
  # Height `shape`, width `1 / shape`: rate `m - k`. Measured +0.000, +1.000,
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
  expect_match(conditionMessage(g), "degrees of freedom above 0.5",
               fixed = TRUE)
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
  idx <- expect_warning(
    mlumr:::.check_survival_scale_collapse(sat, "lognormal", aux_by = "none",
                                           center = FALSE),
    "share that parameter"
  )
  expect_false(isTRUE(attr(idx, "bounds_aux")))
  expect_error(check(sat, aux_by = "none", index_bounds_aux = FALSE),
               "improper")
  # `.study` and NULL are the same stratification and both are examined,
  # whatever the index did.
  expect_error(check(d, aux_by = NULL, index_bounds_aux = TRUE), "improper")
  expect_error(check(d, aux_by = ".study", index_bounds_aux = TRUE),
               "improper")
})
