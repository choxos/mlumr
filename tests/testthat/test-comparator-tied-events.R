# The comparator likelihood is a finite equally weighted mixture over the
# integration grid, `log_sum_exp(ll) - log(n_int)`, not the continuously
# integrated likelihood the model is written to mean. Every pseudo-individual
# in an arm sees the same grid, so tied event times can all select the same
# node, and the condition that the node reproduce their common time is ONE
# equation in the comparator's coefficients. The ridge is then a tube whose
# width falls with the auxiliary, and the marginal behaves as the auxiliary
# to the power (ties - 1): measured 1.000 for two tied events and 2.000 for
# three, over 64 midpoint normal nodes, for `lognormal` and `weibull-aft`
# alike. The index geometry is untouched by this and says nothing about it.

.comp_stub <- function(agd_time, agd_status, ipd_time = exp(c(-0.4, 0.3, 0.1, 0.7))) {
  ip <- set_ipd(
    data.frame(trt = "A", time = ipd_time, status = rep(1L, length(ipd_time)),
               x = c(-0.5, -0.5, 0.5, 0.5)),
    treatment = "trt", covariates = "x", family = "survival",
    time = "time", status = "status"
  )
  ag <- set_agd_surv(
    data.frame(trt = "B", time = agd_time, status = agd_status,
               x_mean = 0, x_sd = 0.5),
    treatment = "trt", time = "time", status = "status",
    cov_means = "x_mean", cov_sds = "x_sd", cov_types = "continuous"
  )
  suppressWarnings(add_integration(
    combine_data(ip, ag), n_int = 32,
    x = distr(stats::qnorm, mean = x_mean, sd = x_sd), verbose = FALSE
  ))
}

check <- function(d, distribution = "lognormal", aux_by = ".study") {
  mlumr:::.check_comparator_tied_events(d, distribution, aux_by = aux_by)
}

test_that("tied comparator events refuse a scale family", {
  # Two events at t = 1. The index rows have positive residual variation, so
  # the index guard passes them and would never look here.
  d <- .comp_stub(c(1, 1, 2.5, 4), c(1L, 1L, 0L, 1L))
  expect_error(check(d), "improper")
  expect_error(check(d), "2 events at one time")
  expect_error(check(d), "ONE equation")
  # The index side really is healthy: this is not the index collapse in
  # disguise, which is the whole point of the finding.
  expect_false(mlumr:::.check_survival_scale_collapse(d, "lognormal",
                                                      center = FALSE))
  # The message says the power the marginal follows, and that a bigger grid
  # is not the repair.
  e <- tryCatch(check(d), error = function(e) conditionMessage(e))
  expect_match(e, "power 1")
  expect_match(e, "larger `n_int` is still a finite mixture")
  expect_match(e, "restriction on the quadrature")
  # Three tied events raise the power to two.
  d3 <- .comp_stub(c(1, 1, 1, 4), c(1L, 1L, 1L, 1L))
  e3 <- tryCatch(check(d3), error = function(e) conditionMessage(e))
  expect_match(e3, "3 events at one time")
  expect_match(e3, "power 2")
  # The generalized gamma's first auxiliary is a scale too.
  expect_error(check(d, distribution = "gengamma"), "improper")
})

test_that("tied comparator events warn a shape family", {
  # The divergence is at infinity for these, where the prior tail decides,
  # so the fit is reported rather than refused. Measured slope for
  # `weibull-aft` with two tied events is the same 1.000.
  d <- .comp_stub(c(1, 1, 2.5, 4), c(1L, 1L, 0L, 1L))
  for (dist in c("weibull", "weibull-aft", "loglogistic", "gamma",
                 "gompertz")) {
    w <- expect_warning(check(d, distribution = dist),
                        "tail of `prior_aux`")
    expect_match(conditionMessage(w), "2 events at one time")
    expect_true(check(d, distribution = dist) |> suppressWarnings())
  }
})

test_that("only tied EVENT times in one arm are the problem", {
  # Distinct event times are as many equations as events and pin the
  # coefficients to a point, whose volume cancels the spikes.
  expect_silent(check(.comp_stub(c(0.5, 1.5, 2.5, 4), c(1L, 1L, 0L, 1L))))
  expect_false(check(.comp_stub(c(0.5, 1.5, 2.5, 4), c(1L, 1L, 0L, 1L))))
  # Tied CENSORED times contribute a survival probability, not a density
  # spike, so they are not this.
  expect_silent(check(.comp_stub(c(2.5, 2.5, 1, 4), c(0L, 0L, 1L, 1L))))
  # One event alone cannot ride a tube: its own equation pins the
  # coefficients and the marginal is flat in the auxiliary.
  expect_silent(check(.comp_stub(c(1, 2.5, 3, 4), c(1L, 0L, 0L, 0L))))
  # A distribution with no auxiliary to collapse is not examined.
  expect_silent(check(.comp_stub(c(1, 1, 2.5, 4), c(1L, 1L, 0L, 1L)),
                      distribution = "exponential"))
})

test_that("a shared auxiliary is not the comparator's to refuse", {
  # Under `aux_by = "none"` the index rows carry the same parameter, and a
  # positive index residual contributes exp(-RSS / (2 sdlog^2)), which goes
  # to zero faster than any power of the scale. An index fit that is itself
  # exact is the other guard's to refuse.
  d <- .comp_stub(c(1, 1, 2.5, 4), c(1L, 1L, 0L, 1L))
  expect_silent(check(d, aux_by = "none"))
  expect_false(check(d, aux_by = "none"))
  # `.study` and NULL are the same stratification and both are examined.
  expect_error(check(d, aux_by = NULL), "improper")
})
