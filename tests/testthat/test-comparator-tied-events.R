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

.comp_stub <- function(agd_time, agd_status,
                       ipd_time = exp(c(-0.4, 0.3, 0.1, 0.7)),
                       ipd_x = c(-0.5, -0.5, 0.5, 0.5),
                       int_distr = NULL, n_int = 32) {
  ip <- set_ipd(
    data.frame(trt = "A", time = ipd_time, status = rep(1L, length(ipd_time)),
               x = ipd_x),
    treatment = "trt", covariates = "x", family = "survival",
    time = "time", status = "status"
  )
  ag <- set_agd_surv(
    data.frame(trt = "B", time = agd_time, status = agd_status,
               x_mean = 0, x_sd = 0.5),
    treatment = "trt", time = "time", status = "status",
    cov_means = "x_mean", cov_sds = "x_sd", cov_types = "continuous"
  )
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

test_that("the refused power is m - n_distinct, not the largest tie", {
  # Three rows at two distinct times: one repeat, power 1.
  expect_match(msg(.comp_stub(c(1, 1, 4), c(1L, 1L, 1L))),
               "3 event rows at 2 distinct times")
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
               "4 event rows at 2 distinct times")
  expect_match(msg(.comp_stub(c(1, 1, 4, 4), c(1L, 1L, 1L, 1L))),
               "diverges at rate 2")
  # Three at one time is also power 2, by the same arithmetic.
  expect_match(msg(.comp_stub(c(1, 1, 1, 4), c(1L, 1L, 1L, 0L))),
               "diverges at rate 2")
  # And four at one time is power 3.
  expect_match(msg(.comp_stub(c(1, 1, 1, 1), c(1L, 1L, 1L, 1L))),
               "diverges at rate 3")
  expect_match(msg(.comp_stub(c(1, 1, 1, 1), c(1L, 1L, 1L, 1L))),
               "4 event rows at 1 distinct time")
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

test_that("a refusal reports the geometry it is refusing", {
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L))
  e <- msg(d)
  expect_match(e, "improper")
  expect_match(e, "matched at once by 2 integration points")
  expect_match(e, "2 equations in the comparator's coefficients")
  expect_match(e, "grid reaches 2 independent linear predictors")
  expect_match(e, "all distinct leave rate zero")
  expect_match(e, "larger `n_int` is still a finite mixture")
  expect_match(e, "restriction on the quadrature")
  # The index side really is healthy: this is not the index collapse in
  # disguise, which is the whole point of the finding.
  expect_false(mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                      center = FALSE))
  # The generalized gamma's first auxiliary is a scale too.
  expect_error(check(d, distribution = "gengamma"), "improper")
})

test_that("tied comparator events warn a shape family", {
  # The divergence is at infinity for these, where the prior tail decides,
  # so the fit is reported rather than refused. Measured slope for
  # `weibull-aft` at three rows on two distinct times is the same 1.000.
  d <- .comp_stub(c(1, 1, 4), c(1L, 1L, 1L))
  for (dist in c("weibull", "weibull-aft", "loglogistic", "gamma",
                 "gompertz")) {
    w <- expect_warning(check(d, distribution = dist),
                        "tail of `prior_aux`")
    expect_match(conditionMessage(w), "3 event rows at 2 distinct times")
    # A shape family runs to infinity, so its exponent is on the auxiliary
    # itself rather than on one over it. Same rate, opposite boundary.
    expect_match(conditionMessage(w), "diverges at rate 1")
    expect_match(conditionMessage(w), "`shape\\^1`")
    expect_true(suppressWarnings(check(d, distribution = dist)))
  }
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
