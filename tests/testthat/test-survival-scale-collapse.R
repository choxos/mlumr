# A log-normal AFT is a normal model for log(t) with a positive scale, so IPD
# whose covariates reproduce every event time exactly makes the same improper
# posterior the normal guard refuses: with the coefficients integrated out the
# density of `sdlog` behaves as sdlog^(rank - n) near zero and does not
# integrate. `prior_aux` defaults to a half-normal, and every supported
# alternative has positive density at zero, so no prior repairs it.

.surv_stub <- function(time, status = rep(1L, length(time)),
                       x = c(-0.5, -0.5, 0.5, 0.5), entry = NULL) {
  source <- data.frame(trt = "A", time = time, status = status, x = x)
  if (!is.null(entry)) source$entry <- entry
  ip <- set_ipd(source, treatment = "trt", covariates = "x",
                family = "survival", time = "time", status = "status",
                entry_time = if (is.null(entry)) NULL else "entry")
  ag <- set_agd_surv(
    data.frame(trt = "B", time = c(0.5, 1.5, 2.5, 4),
               status = c(1L, 1L, 0L, 1L), x_mean = 0, x_sd = 0.5),
    treatment = "trt", time = "time", status = "status",
    cov_means = "x_mean", cov_sds = "x_sd", cov_types = "continuous"
  )
  suppressWarnings(add_integration(
    combine_data(ip, ag), n_int = 32,
    x = distr(stats::qnorm, mean = x_mean, sd = x_sd), verbose = FALSE
  ))
}

check <- function(d, distribution = "lognormal", aux_by = ".study",
                  center = FALSE) {
  mlumr:::.check_survival_scale_collapse(d, distribution, aux_by = aux_by,
                                         center = center)
}

test_that("uncensored event times the covariates fit exactly are refused", {
  # Four exact events at t = 1 against a design of rank 2. Every log time is
  # zero and the intercept alone reaches it.
  expect_error(check(.surv_stub(rep(1, 4))), "improper")
  expect_error(check(.surv_stub(rep(1, 4))), "no censored row")
  # Distinct times still reproduced exactly: two profiles, two values.
  expect_error(check(.surv_stub(c(1, 1, exp(2), exp(2)))), "improper")
})

test_that("event times with residual variation pass silently", {
  set.seed(2026)
  d <- .surv_stub(exp(c(-0.4, 0.3, 0.1, 0.7)))
  expect_silent(check(d))
  expect_false(check(d))
})

test_that("a shared auxiliary is not the index study's to refuse", {
  # Under aux_by = "none" the comparator rows enter the same parameter, so
  # the index geometry alone does not settle propriety and the fit is not
  # refused on it. Sharing does not establish propriety either, which is the
  # subject of its own test below.
  expect_warning(check(.surv_stub(rep(1, 4)), aux_by = "none"))
  expect_error(suppressWarnings(check(.surv_stub(rep(1, 4)), aux_by = "none")),
               NA)
  # `.study` and NULL are the same stratification and both are examined.
  expect_error(check(.surv_stub(rep(1, 4)), aux_by = NULL), "improper")
})

test_that("a shape auxiliary is warned about, not refused", {
  # The Weibull, log-logistic and gamma carry a shape, the reciprocal of a
  # scale, so the same exact fit sends it to +Inf. A half-normal or
  # exponential prior integrates that and a half-t need not, which makes
  # propriety a property of the prior rather than of the data.
  for (dist in c("weibull", "weibull-aft", "loglogistic", "gamma")) {
    expect_warning(out <- check(.surv_stub(rep(1, 4)), distribution = dist),
                   "depends on the tail of `prior_aux`")
    expect_true(out)
  }
  # The generalized gamma is NOT one of them. Its first auxiliary is the
  # Lawless `sigma`, which the density divides the log residual by and
  # carries a `-log(sigma)` term for: a scale, collapsing to zero on an
  # exact fit exactly as `sdlog` does, with the shape sitting in the second
  # auxiliary where the density's dependence on it is bounded. So it is
  # refused, and the refusal names its own parameter.
  expect_error(check(.surv_stub(rep(1, 4)), distribution = "gengamma"),
               "generalized-gamma `sigma`")
  expect_error(check(.surv_stub(rep(1, 4)), distribution = "gengamma"),
               "sigma\\^\\(rank - n\\)")
  expect_error(check(.surv_stub(rep(1, 4)), distribution = "lognormal"),
               "sdlog\\^\\(rank - n\\)")
  # A distribution with no auxiliary at all has nothing to collapse.
  expect_false(check(.surv_stub(rep(1, 4)), distribution = "exponential"))
  expect_false(check(.surv_stub(rep(1, 4)), distribution = "mspline"))
})

test_that("a censored row that fails before it was censored bounds the scale", {
  # Three exact events on a rank-2 design plus a censored row whose fitted
  # time is below its censoring time: its survival goes to zero faster than
  # any power of the scale, and the posterior is proper.
  d <- .surv_stub(c(1, 1, 1, exp(3)), status = c(1L, 1L, 1L, 0L),
                  x = c(-0.5, -0.5, 0.5, 0.5))
  expect_false(check(d))
  # And one censored above its fitted time does nothing: survival goes to one
  # and the divergence is exactly where it was.
  d2 <- .surv_stub(c(1, 1, 1, exp(-3)), status = c(1L, 1L, 1L, 0L),
                   x = c(-0.5, -0.5, 0.5, 0.5))
  expect_error(check(d2), "improper")
  expect_error(check(d2), "no censored row is predicted to fail")
})

test_that("censored rows the event fit does not determine are refused as undecided", {
  # One event profile, so the event design has rank 1 against two columns and
  # the exact solutions are a line: the censored row's predictor moves along
  # it, and whether it falls below its censoring time is not a fact about
  # the data.
  d <- .surv_stub(c(1, 1, 1, exp(-3)), status = c(1L, 1L, 1L, 0L),
                  x = c(0, 0, 0, 1))
  expect_error(check(d), "could not be decided")
  expect_error(check(d), "not determined by that fit")
})

test_that("a censored row bounds when the fit lands outside its region", {
  # Every censoring type is the same question about the OBSERVATION REGION
  # the row lies in: as the scale goes to zero the fitted distribution
  # concentrates at the fitted value, so the row's contribution tends to one
  # when that value is strictly inside its region and to zero when it is
  # strictly outside. Only the second bounds the scale.
  #
  # Three events at t = 1 fix the fitted time at 1 for both profiles.
  # Measured on three repeated events over an intercept-only design,
  # `d log M / d log(1/sdlog)` with a left-censored row at upper bound 2 is
  # 2.000, identical to the same data with the row removed; at upper bound
  # 0.5 the marginal collapses to -1.8e11 instead.
  left_at <- function(u) {
    d <- .surv_stub(rep(1, 4))
    d$ipd$data$.status[4L] <- 2L
    d$ipd$data$.time[4L] <- u
    d
  }
  # Upper bound above the fitted time: the probability tends to one, the row
  # suppresses nothing, and the collapse is the one it was hiding.
  expect_error(check(left_at(2)), "improper")
  # Below it: the probability tends to zero and the fit is proper.
  expect_false(check(left_at(0.5)))

  interval_at <- function(lo, hi) {
    d <- .surv_stub(rep(1, 4))
    d$ipd$data$.status[4L] <- 3L
    d$ipd$data$.start_time[4L] <- lo
    d$ipd$data$.time[4L] <- hi
    d
  }
  # An interval holding the fitted time bounds nothing; one that misses it
  # on either side bounds.
  expect_error(check(interval_at(0.5, 2)), "improper")
  expect_false(check(interval_at(2, 3)))
  expect_false(check(interval_at(0.2, 0.5)))

  # An interval that does not open before it closes has no region at all.
  bad <- interval_at(2, 3)
  bad$ipd$data$.start_time[4L] <- 3
  expect_false(check(bad))
})

test_that("a delayed entry never closes a region from below", {
  # It conditions the probability on survival to it rather than bounding
  # the row, and the difference shows exactly where the fitted value sits
  # BELOW the entry: the conditional law piles up just above the entry, so
  # the probability of the region tends to ONE, not to zero. Three events at
  # t = 1 with a left-censored row entering at 1.5 and closing at 2: the
  # row's own contribution is 1.000000 at every scale from 0.1 down, and the
  # marginal slope is 2.000, identical to the same data with the row
  # removed. An earlier version of this test asserted the opposite and
  # pinned the bug.
  d <- .surv_stub(rep(1, 4))
  d$ipd$data$.status[4L] <- 2L
  d$ipd$data$.time[4L] <- 2
  expect_error(check(d), "improper")
  d$ipd$data$.delay_time[4L] <- 1.5
  expect_error(check(d), "improper")
  # An interval that opens STRICTLY above its entry does still bound from
  # below, since the pile at the entry is then outside it.
  iv <- .surv_stub(rep(1, 4))
  iv$ipd$data$.status[4L] <- 3L
  iv$ipd$data$.delay_time[4L] <- 1.2
  iv$ipd$data$.start_time[4L] <- 1.5
  iv$ipd$data$.time[4L] <- 2
  expect_false(check(iv))
  # And an interval opening exactly AT its entry does not: the pile is
  # inside it.
  iv$ipd$data$.start_time[4L] <- 1.2
  expect_error(check(iv), "improper")
  # A right-censored row is not tightened by its entry: its region is
  # [log c, Inf) whatever the entry is, since the entry is below c.
  r <- .surv_stub(c(1, 1, 1, exp(-3)), status = c(1L, 1L, 1L, 0L),
                  x = c(-0.5, -0.5, 0.5, 0.5))
  expect_error(check(r), "improper")
  r$ipd$data$.delay_time[4L] <- exp(-4)
  expect_error(check(r), "improper")
})

test_that("delayed entry does not rescue an exact fit", {
  # It used to skip the question entirely, which admitted the collapse in
  # silence. Each row contributes `f(t) / S(entry)` with the entry time
  # strictly below its own row's time, so as the scale goes to zero the
  # fitted distribution concentrates at the fitted time, `S(entry)` tends to
  # one, and every term is the undelayed one. Measured on three exact events
  # over a rank-2 design, `d log M / d log(1/sdlog)` is 1.000 with no delayed
  # entry, 1.000 with entry at half the event time and 1.000 with entry at
  # 99% of it, and from `sdlog = 1e-3` down the three marginals agree to
  # every printed digit.
  expect_error(check(.surv_stub(rep(1, 4), entry = rep(0.1, 4))), "improper")
  expect_error(check(.surv_stub(rep(1, 4), entry = rep(0.99, 4))), "improper")
  # A shape family is warned about under delayed entry for the same reason.
  expect_warning(
    check(.surv_stub(rep(1, 4), entry = rep(0.1, 4)), distribution = "weibull"),
    "depends on the tail of `prior_aux`"
  )
  # And a censored row that bounds the scale still bounds it: the entry
  # survival tends to one there too, so the row is exactly the undelayed
  # one and the fit is proper.
  expect_false(check(.surv_stub(c(1, 1, 1, exp(3)), status = c(1L, 1L, 1L, 0L),
                                x = c(-0.5, -0.5, 0.5, 0.5),
                                entry = rep(0.1, 4))))
  # An entry at or above its own row's time is the one case the argument
  # does not cover, and it is left alone rather than guessed at.
  d <- .surv_stub(rep(1, 4))
  d$ipd$data$.delay_time <- c(0.1, 0.1, 0.1, 1)
  expect_false(check(d))
})

test_that("as many event rows as free columns warns about the prior instead", {
  # Two events on a rank-2 design and nothing else: the posterior is proper,
  # and nothing in the index data separates the scale from the coefficients.
  #
  # No censored row here, deliberately. This fixture used to carry two, at
  # exp(2) and exp(3) against fitted times of 1 and exp(1), so both were
  # predicted to fail long before they were censored and both bounded the
  # scale. The warning below was false for that data, and the censored rows
  # were not being consulted before it was issued. The bounded version is
  # asserted to be silent in its own test.
  d <- .surv_stub(c(1, exp(1)), status = c(1L, 1L), x = c(-0.5, 0.5))
  expect_warning(check(d), "as many as the free columns")
  # Both coefficient priors, not just one: the exact-fit coefficient vector
  # includes the intercept, and the two defaults are different widths.
  expect_warning(check(d), "sensitive to `prior_intercept` and `prior_beta`")
  expect_warning(check(d), "normal\\(0, 10\\)` against")
})

test_that("mlumr() refuses the log-normal collapse before it reaches the engine", {
  # No Stan model is compiled and no backend is chosen. The spy fails loudly,
  # so a refusal that came after dispatch would show as the spy's error.
  local_mocked_bindings(
    .mlumr_fit_backend = function(...) stop("engine reached"),
    .package = "mlumr"
  )
  expect_error(
    suppressWarnings(mlumr(.surv_stub(rep(1, 4)), model = "relaxed",
                           distribution = "lognormal", aux_by = ".study",
                           center = FALSE, qr = FALSE, seed = 2026,
                           verbose = FALSE)),
    "improper"
  )
  # Under the model's own centering the design still reaches every log time,
  # so the verdict does not depend on passing center = FALSE.
  expect_error(
    suppressWarnings(mlumr(.surv_stub(rep(1, 4)), model = "relaxed",
                           distribution = "lognormal", seed = 2026,
                           verbose = FALSE)),
    "improper"
  )
  # Ordinary event times reach the spy, which is what proves the spy is live.
  set.seed(2026)
  expect_error(
    suppressWarnings(mlumr(.surv_stub(exp(rnorm(4))), model = "relaxed",
                           distribution = "lognormal", seed = 2026,
                           verbose = FALSE)),
    "engine reached"
  )
})

test_that("a censored row bounds a shape family too, and suppresses its warning", {
  # exp(-(c e^-eta)^k) goes to zero as k grows for exactly the rows whose
  # fitted time falls below their censoring time, so the shape families are
  # consulted before they are warned about rather than after.
  bounded <- .surv_stub(c(1, 1, 1, exp(3)), status = c(1L, 1L, 1L, 0L))
  unbounded <- .surv_stub(c(1, 1, 1, exp(-3)), status = c(1L, 1L, 1L, 0L))
  for (dist in c("weibull", "loglogistic", "gamma")) {
    expect_silent(check(bounded, distribution = dist))
    expect_false(check(bounded, distribution = dist))
    expect_warning(check(unbounded, distribution = dist),
                   "depends on the tail of `prior_aux`")
  }
  # And an undetermined censored predictor is said to be undetermined rather
  # than assumed either way.
  undetermined <- .surv_stub(c(1, 1, 1, exp(-3)), status = c(1L, 1L, 1L, 0L),
                             x = c(0, 0, 0, 1))
  expect_warning(check(undetermined, distribution = "weibull"),
                 "could not be told")
  # A censored row bounds the generalized gamma's scale the same way, and
  # suppresses the refusal rather than the warning.
  expect_false(check(bounded, distribution = "gengamma"))
  expect_error(check(unbounded, distribution = "gengamma"),
               "generalized-gamma `sigma`")
})

test_that("a shape warning does not claim an exact fit it could not establish", {
  # `unresolved`, `unresolved_log` and `undecidable` all mean the question
  # could not be ANSWERED at double precision. They used to fall into the
  # same message as a resolved exact fit, which told the user their
  # covariates reproduce every event time exactly and that the result hangs
  # on `prior_aux`, about a design that is merely near-collinear or rounded.
  # A near-collinear design: the second column is the first plus 1e-13, so
  # the exact rank is 3 and a factorization at machine precision resolves 2.
  x <- c(-1, 0, 1, 2)
  d <- .surv_stub(exp(c(-1, 0, 1, 2)), x = x)
  d$ipd$data$x2 <- x + 1e-13
  d$covariates <- c("x", "x2")
  for (dist in c("weibull", "loglogistic", "gamma")) {
    w <- expect_warning(check(d, distribution = dist))
    expect_match(conditionMessage(w), "could not be told at double precision")
    expect_false(grepl("fit every event time exactly on the log scale, so",
                       conditionMessage(w)))
  }
  # The scale families are refused on the same design, and say why.
  for (dist in c("lognormal", "gengamma")) {
    expect_error(check(d, distribution = dist),
                 "could not be decided")
  }
})

test_that("a nearly exact fit warns about the boundary the sampler works at", {
  # The residual is real and the posterior proper, and the auxiliary still
  # concentrates against its boundary. The normal guard warns here too.
  d <- .surv_stub(exp(c(-1, -1, 1, 1) + c(0, 1e-9, 0, 1e-9)))
  expect_warning(check(d), "very nearly fit the event times exactly")
  expect_warning(check(d), "`sdlog` will concentrate")
  # The PH Weibull says "may", not "will": its ridge scales the linear
  # predictor with the shape, so ordinary normal coefficient priors can stop
  # it before this residual does, and asserting concentration would be a
  # sampler diagnosis the data do not support on their own.
  w <- expect_warning(check(d, distribution = "weibull"))
  expect_match(conditionMessage(w), "the Weibull shape may concentrate")
  expect_match(conditionMessage(w), "conditional on `prior_beta`")
  # The gamma names the intercept prior, for the same reason by a different
  # route, and the AFT families keep the unconditional "will".
  w <- expect_warning(check(d, distribution = "gamma"))
  expect_match(conditionMessage(w), "may concentrate")
  expect_match(conditionMessage(w), "conditional on `prior_intercept`")
  for (dist in c("weibull-aft", "loglogistic", "lognormal")) {
    w <- expect_warning(check(d, distribution = dist))
    expect_match(conditionMessage(w), "will concentrate")
    expect_false(grepl("conditional on", conditionMessage(w)), label = dist)
  }
})

test_that("a bounding censored row is consulted before every boundary message", {
  # `near_exact` and `saturated` used to warn before the censored rows were
  # looked at, so a row that holds the auxiliary away from its boundary got
  # a warning saying it would concentrate there, or that nothing in the
  # index data separates it from the coefficients. Both are untrue once such
  # a row is present, and the documented behavior says censoring suppresses
  # the message.
  #
  # Nearly exact events plus a censored row fitted to fail well before it
  # was censored.
  near <- .surv_stub(exp(c(-1, -1, 1, 1, 3) + c(0, 1e-9, 0, 1e-9, 0)),
                     status = c(1L, 1L, 1L, 1L, 0L),
                     x = c(-0.5, -0.5, 0.5, 0.5, -0.5))
  expect_silent(check(near))
  expect_false(check(near))
  # Saturated events (two rows, rank two) plus the same kind of censored row.
  sat <- .surv_stub(c(exp(-1), exp(1), exp(3)), status = c(1L, 1L, 0L),
                    x = c(-0.5, 0.5, -0.5))
  expect_silent(check(sat))
  expect_false(check(sat))
  # Without that row both still say what they said.
  expect_warning(check(.surv_stub(exp(c(-1, -1, 1, 1) + c(0, 1e-9, 0, 1e-9)))),
                 "concentrate against its boundary")
  expect_warning(check(.surv_stub(c(exp(-1), exp(1)), status = c(1L, 1L),
                                  x = c(-0.5, 0.5))),
                 "as many as the free columns")
})

test_that("a shared auxiliary is reported, not assumed to be bounded", {
  # `aux_by = "none"` returned silently, on the reading that the comparator
  # rows sharing the parameter bound it. They need not: a comparator of
  # right-censored rows whose fitted times sit above their censoring times
  # contributes a likelihood tending to one at the boundary. That question
  # belongs to the marginalized aggregate likelihood, which this check does
  # not see, so it warns rather than either refusing or staying silent.
  d <- .surv_stub(rep(1, 4))
  expect_warning(out <- check(d, aux_by = "none"),
                 "sharing does not bound it on its own")
  expect_true(out)
  for (dist in c("lognormal", "gengamma", "weibull", "gamma")) {
    expect_warning(check(d, distribution = dist, aux_by = "none"),
                   "does not examine")
  }
  # And it is a warning in place of the refusal, not beside it: the same
  # data under the default stratification is refused.
  expect_error(check(d), "improper")
  # A censored row that bounds the parameter still silences it.
  bounded <- .surv_stub(c(1, 1, 1, exp(3)), status = c(1L, 1L, 1L, 0L))
  expect_false(check(bounded, aux_by = "none"))
  # A near-exact fit is proper whatever the comparator does, so it does not
  # get the shared-auxiliary message. It does still get its own: sharing
  # multiplies the index's near-boundary likelihood by whatever the
  # comparator contributes there, and when that is a nonzero limit, which is
  # the configuration this check cannot analyze, the concentration is
  # unchanged. Silence would hide the sampler diagnostic in exactly the case
  # that motivates the branch.
  near <- .surv_stub(exp(c(-1, -1, 1, 1) + c(0, 1e-9, 0, 1e-9)))
  w <- expect_warning(check(near, aux_by = "none"))
  expect_match(conditionMessage(w), "concentrate against its boundary")
  expect_false(grepl("sharing does not bound", conditionMessage(w)))
})

test_that("a censored predictor fixed by a deficient design still bounds", {
  # `.censoring_bounds_aux()` used to give up as soon as the event design was
  # rank-deficient, on the stronger condition that every coefficient be
  # identified. A censored row's predictor only needs to lie in the ROW
  # SPACE of the event design, and the clearest case of that is a censored
  # row at a covariate profile the events already occupy: its predictor is
  # then fixed however deficient the design is.
  #
  # Two exact events at x = -0.5 leave the slope aliased, so the design has
  # rank 1 out of 2 columns. The censored row sits at the same x, fitted to
  # fail at log-time 0 against a censoring time of 3, so it bounds the
  # parameter and the posterior is proper. This was REFUSED before.
  # The constant covariate makes set_ipd() warn on its own account, which
  # is a different check and not this one's business.
  d <- suppressWarnings(.surv_stub(c(1, 1, exp(3)), status = c(1L, 1L, 0L),
                                   x = c(-0.5, -0.5, -0.5)))
  expect_silent(check(d))
  expect_false(check(d))
  expect_false(check(d, distribution = "gengamma"))
  expect_silent(check(d, distribution = "weibull"))
  # A censored row OUTSIDE that row space is still undetermined, and said to
  # be, rather than assumed either way: x = 0.5 is not in the span of the
  # single event profile.
  out <- .surv_stub(c(1, 1, exp(3)), status = c(1L, 1L, 0L),
                    x = c(-0.5, -0.5, 0.5))
  expect_error(check(out), "could not be decided")
  expect_warning(check(out, distribution = "weibull"), "could not be told")
  # And one inside the row space whose censoring time is BELOW its fitted
  # time bounds nothing, so it is refused on the merits, not as undecided.
  above <- suppressWarnings(
    .surv_stub(c(exp(2), exp(2), exp(1)), status = c(1L, 1L, 0L),
               x = c(-0.5, -0.5, -0.5)))
  expect_error(check(above), "improper")
})

test_that("a numerical rank below the exact one answers nothing", {
  # `lm.fit()` picks its own rank at a numerical tolerance and can drop a
  # column `.exact_rank()` keeps. The estimability test runs on the exact
  # rank, so the two would be answering about different models: fitted
  # values from the reduced one, rows judged against the full one. Where the
  # exact fit depends on the dropped direction that is not a rounding
  # difference, and the error runs the wrong way, toward a silent pass.
  #
  # Events at x = (-1, 0, 1, 2) with a second column x + 1e-13, and event
  # times equal to that second column. The exact rank is 3 of 3 columns, so
  # every row is estimable, but lm.fit() resolves 2 and returns NA for the
  # third coefficient.
  x <- c(-1, 0, 1, 2)
  Xe <- cbind(1, x, x + 1e-13)
  ye <- x + 1e-13
  expect_identical(mlumr:::.exact_rank(Xe)$rank, 3L)
  expect_identical(stats::lm.fit(Xe, ye)$rank, 2L)
  # The exact solution is (0, 0, 1) and the reduced one is (1e-13, 1, 0).
  # They agree on every event row and not on this censored one: its true
  # predictor is 10 and its reduced predictor is 1e-13, so against a
  # censoring time of 5 the reduced fit says the row fails first and bounds
  # the parameter, and the true fit says it does not.
  X <- rbind(Xe, c(1, 0, 10))
  y <- c(ye, 5)
  events <- c(rep(TRUE, 4L), FALSE)
  expect_identical(mlumr:::.censoring_bounds_aux(X, y, events), "undetermined")
  # The two answers it must still give.
  ev3 <- c(TRUE, TRUE, FALSE)
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, rep(-0.5, 3L)), c(0, 0, 3), ev3),
    "bounded"
  )
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, c(-0.5, -0.5, 0.5)), c(0, 0, 3),
                                  ev3),
    "undetermined"
  )
})

test_that("a censored row repeating an event profile bounds without a solve", {
  # A censored row at one of the event rows' covariate profiles has its
  # predictor pinned to that event's fitted value, and where the event fit
  # interpolates, that value IS the event's log time. No solve enters it, so
  # the answer survives a design the numerical path refuses to fit: exact
  # rank 3, numerical rank 2, which the guard above sends to
  # "undetermined". The fit it was refusing is proper, and a log-normal was
  # being rejected for it.
  x <- c(-1, 0, 1, 1)
  Xe <- cbind(1, x, x + 1e-13)
  ye <- c(0.5, 1, 1.5, 1.5)
  expect_identical(mlumr:::.exact_rank(Xe)$rank, 3L)
  expect_identical(stats::lm.fit(Xe, ye)$rank, 2L)
  X <- rbind(Xe, c(1, 0, 1e-13))
  events <- c(rep(TRUE, 4L), FALSE)
  y <- c(ye, log(20))
  expect_identical(mlumr:::.censoring_bounds_aux(X, y, events), "undetermined")
  expect_identical(
    mlumr:::.censoring_bounds_aux(X, y, events, exact_fit = TRUE),
    "bounded"
  )
  # The shortcut only ever adds a bound. A twin censored before its event
  # says nothing, and one censored exactly at it sits on the boundary, where
  # survival tends to 1/2 and bounds nothing.
  expect_identical(
    mlumr:::.censoring_bounds_aux(X, c(ye, 0.2), events, exact_fit = TRUE),
    "undetermined"
  )
  expect_identical(
    mlumr:::.censoring_bounds_aux(X, c(ye, 1), events, exact_fit = TRUE),
    "undetermined"
  )
  # A profile 1e-13 away is a different profile, not a twin: pinning it to
  # an event time it does not share is the error the exact keys avoid, and a
  # 15-digit character conversion would commit it.
  expect_identical(
    mlumr:::.censoring_bounds_aux(rbind(Xe, c(1, 1e-13, 2e-13)), y, events,
                                  exact_fit = TRUE),
    "undetermined"
  )
  # A row that is an extrapolation rather than a repeat is still refused by
  # the numerical rank guard, shortcut or not.
  expect_identical(
    mlumr:::.censoring_bounds_aux(rbind(Xe, c(1, 0, 10)), c(ye, 5), events,
                                  exact_fit = TRUE),
    "undetermined"
  )
  # And the boundary sweep the tolerance exists for is unchanged by it: a
  # twin at its event's own time is never read as a bound.
  set.seed(2026)
  verdicts <- vapply(seq_len(400L), function(i) {
    xs <- round(stats::rnorm(4L), 3L)
    b <- stats::rnorm(2L)
    yes <- b[1L] + b[2L] * xs
    xc <- xs[sample.int(4L, 1L)]
    mlumr:::.censoring_bounds_aux(
      rbind(cbind(1, xs), c(1, xc)), c(yes, b[1L] + b[2L] * xc),
      c(rep(TRUE, 4L), FALSE), exact_fit = TRUE
    )
  }, character(1L))
  expect_identical(sum(verdicts == "bounded"), 0L)
})

test_that("mlumr() no longer refuses a log-normal a repeated profile bounds", {
  # The same geometry through the caller: the log-normal above was refused
  # as undecided, and it is proper. Two covariates 1e-13 apart are what puts
  # the numerical rank below the exact one, so this needs its own stub.
  x <- c(-1, 0, 1, 1, 0)
  d <- local({
    source <- data.frame(trt = "A", time = c(exp(c(0.5, 1, 1.5, 1.5)), 20),
                         status = c(1L, 1L, 1L, 1L, 0L), x = x, z = x + 1e-13)
    ip <- set_ipd(source, treatment = "trt", covariates = c("x", "z"),
                  family = "survival", time = "time", status = "status")
    ag <- set_agd_surv(
      data.frame(trt = "B", time = c(0.5, 1.5, 2.5, 4),
                 status = c(1L, 1L, 0L, 1L), x_mean = 0, x_sd = 0.5,
                 z_mean = 0, z_sd = 0.5),
      treatment = "trt", time = "time", status = "status",
      cov_means = c("x_mean", "z_mean"), cov_sds = c("x_sd", "z_sd"),
      cov_types = c("continuous", "continuous")
    )
    suppressWarnings(add_integration(
      combine_data(ip, ag), n_int = 32,
      x = distr(stats::qnorm, mean = x_mean, sd = x_sd),
      z = distr(stats::qnorm, mean = z_mean, sd = z_sd), verbose = FALSE
    ))
  })
  expect_silent(check(d))
  expect_false(check(d))
})

test_that("the exact-fit shortcut is only offered to an exact fit", {
  # `.check_survival_scale_collapse()` consults the bound for every geometry
  # that is not `positive`, and only three of those interpolate: `exact` is
  # decided structurally, `saturated` has full row rank, and `constant` is
  # reproduced by the intercept alone. A `near_exact` fit puts the twin at
  # the fitted value instead of at the event's time, so it must not get the
  # shortcut, and the argument the caller passes is what withholds it.
  x <- c(-1, 0, 1, 1)
  Xe <- cbind(1, x, x + 1e-13)
  ye <- c(0.5, 1, 1.5, 1.5)
  expect_identical(
    mlumr:::.residual_variation_status(Xe, ye, "identity")$status,
    "exact"
  )
  near <- ye + c(0, 0, 0, 1e-9)
  # Which non-interpolating status this lands in is a LAPACK question, not a
  # contract: the same design reads `near_exact` where the factorization
  # resolves the third column and `unresolved` where it does not (Windows
  # does not). The contract is only that it is not one of the three the
  # caller offers the shortcut to.
  expect_false(
    mlumr:::.residual_variation_status(Xe, near, "identity")$status %in%
      c("exact", "constant", "saturated")
  )
  expect_identical(
    mlumr:::.censoring_bounds_aux(rbind(Xe, c(1, 0, 1e-13)), c(near, log(20)),
                                  c(rep(TRUE, 4L), FALSE)),
    "undetermined"
  )
})

test_that("a rounding-sized gap at the censoring boundary decides nothing", {
  # A censored row duplicating an event's covariate profile AND its time
  # sits exactly on the fitted boundary: its survival tends to 1/2 and it
  # bounds nothing. But `eta` comes from `lm.fit()`, so the computed gap is
  # a rounding-sized number whose sign is not information. Over 4000 random
  # boundary rows of this shape, a third came out strictly below and were
  # read as bounding, which admits an improper fit in silence.
  set.seed(2026)
  verdicts <- vapply(seq_len(400L), function(i) {
    x <- round(stats::rnorm(4L), 3L)
    b <- stats::rnorm(2L)
    ye <- b[1L] + b[2L] * x
    xc <- x[sample.int(4L, 1L)]
    mlumr:::.censoring_bounds_aux(
      rbind(cbind(1, x), c(1, xc)), c(ye, b[1L] + b[2L] * xc),
      c(rep(TRUE, 4L), FALSE)
    )
  }, character(1L))
  # Never "bounded": the gap is rounding, not evidence.
  expect_identical(sum(verdicts == "bounded"), 0L)
  expect_true(all(verdicts %in% c("undetermined", "unbounded")))
  # A gap that is real is still read as real, at a size far below anything
  # the fit could have invented.
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, rep(-0.5, 3L)), c(0, 0, 1e-6),
                                  c(TRUE, TRUE, FALSE)),
    "bounded"
  )
  # And a censored row above its fitted time still bounds nothing.
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, rep(-0.5, 3L)), c(0, 0, -1e-6),
                                  c(TRUE, TRUE, FALSE)),
    "unbounded"
  )
})

test_that("the solve-error tolerance is a norm bound, not a coordinatewise one", {
  # A coefficient's error is set by the norm of the WHOLE solution, not by
  # its own size, so `|Xc| |beta|` quietly assumes every coordinate carries
  # at most `cond * eps` of RELATIVE error. A small coefficient against a
  # large censored covariate breaks that: the term that dominates the
  # predictor's error is the one the product understates.
  #
  # Events at t = 1000:1004 on (1, t, t^2) keep FULL numerical rank at a
  # condition number of 6e11, so the rank guard above does not catch them.
  # Every value here is a small integer or a power of two, so the design,
  # the event times and the censored row's own boundary are exact in double
  # and the whole discrepancy is the least-squares solve.
  tt <- 1000:1004
  Xe <- cbind(1, tt, tt^2)
  ev <- c(rep(TRUE, 5L), FALSE)
  gaps <- numeric(0L)
  coordinatewise <- numeric(0L)
  for (q in c(38L, 40L, 44L)) {
    b <- c(3, 2, 2^-q)
    ye <- b[1L] + b[2L] * tt + b[3L] * tt^2
    fit <- stats::lm.fit(Xe, ye)
    # If this ever fails the case is vacuous rather than wrong: the rank
    # guard would answer before the tolerance is reached.
    expect_identical(fit$rank, 3L)
    xc <- c(1, 0, -2^q)
    # The censored row sits EXACTLY on its own fitted boundary, where
    # survival tends to 1/2 and nothing is bounded.
    eta_true <- b[1L] - b[3L] * 2^q
    beta_hat <- fit$coefficients
    gaps <- c(gaps, eta_true - sum(xc * beta_hat))
    used <- fit$qr$pivot[seq_len(fit$rank)]
    cond <- kappa(Xe[, used, drop = FALSE], exact = FALSE)
    coordinatewise <- c(
      coordinatewise,
      max(8, nrow(Xe)) * .Machine$double.eps * max(1, cond) *
        max(sum(abs(xc) * abs(beta_hat)), abs(eta_true), 1)
    )
    expect_identical(
      mlumr:::.censoring_bounds_aux(rbind(Xe, xc), c(ye, eta_true), ev),
      "undetermined"
    )
  }
  # The defect stated so that it survives a change of LAPACK: at a boundary
  # row the solve error exceeds what the coordinatewise tolerance allows, so
  # that tolerance reads rounding as signal. Its SIZE and its SIGN belong to
  # the platform (macOS gives +2.87 at q = 44; Windows a smaller value, and
  # the opposite sign at q = 38), and pinning either broke the Windows
  # build. What holds everywhere is that the bound is beaten.
  expect_true(any(abs(gaps) > coordinatewise))
  # Widening it must not cost the answers it exists to give.
  ev3 <- c(TRUE, TRUE, FALSE)
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, rep(-0.5, 3L)), c(0, 0, 1e-6), ev3),
    "bounded"
  )
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, rep(-0.5, 3L)), c(0, 0, -1e-6), ev3),
    "unbounded"
  )
  expect_identical(
    mlumr:::.censoring_bounds_aux(
      rbind(cbind(1, c(-1, 0, 1, 2)), cbind(1, 0.5)),
      c(0.5, 1, 1.5, 2, log(50)), c(rep(TRUE, 4L), FALSE)
    ),
    "bounded"
  )
})

test_that("the propriety verdict does not depend on the predictor's units", {
  # `kappa()` on unscaled columns counts a units choice as ill-conditioning.
  # Symmetric event profiles at x = -1 and 1 fitted by eta = 1 + x, with a
  # censored row at the midpoint whose censoring time sits half a log unit
  # above its fitted value: the row bounds, at every scale. Multiplying the
  # covariate by 2^50 took the unscaled condition number to 1.1e15 while the
  # scaled design's is exactly 1, and the tolerance that came out of it
  # swallowed the real gap and returned "undetermined", refusing a
  # log-normal the row makes proper.
  #
  # The scales are powers of two, so the data are bit-identical across them
  # and only the units differ.
  for (p in c(0L, 10L, 20L, 30L, 40L, 50L)) {
    sc <- 2^p
    expect_identical(
      mlumr:::.censoring_bounds_aux(
        rbind(cbind(1, c(-1, 1) * sc), c(1, 0)), c(0, 2, 1.5),
        c(TRUE, TRUE, FALSE)
      ),
      "bounded",
      label = paste("covariate scaled by 2^", p)
    )
  }
})

test_that("gompertz is diagnosed rather than passed in silence", {
  # It was in neither family list, so the guard returned before saying
  # anything at all. Its hazard is `exp(eta + a t)`, so an exact fit at a
  # common event time drives the linear predictor to
  # `log(a) - log(expm1(a))`, about -a. With the coefficient integrated out
  # against a Cauchy `prior_intercept` on an intercept-only design,
  # `d log M / d log a` at a = 1e6 is -1.000 with one event row, 0.000 with
  # two and 1.000 with three: the marginal goes as `shape^(n - 2k)`, for `k`
  # the coefficients the ridge moves, which is one here. A Cauchy
  # `prior_aux` contributes a^-2, so three rows leave a^-1 and no posterior.
  expect_warning(check(.surv_stub(rep(1, 4)), distribution = "gompertz"),
                 "depends on the tail of `prior_aux`")
  w <- expect_warning(check(.surv_stub(rep(1, 4)), distribution = "gompertz"))
  # Both coefficient priors are named, since the ridge moves them rather
  # than leaving them where they were. `n - rank - 1` was the exponent
  # measured on an intercept-only design, where `rank` and `k` coincide; it
  # is wrong wherever they do not, and five rows exactly linear in one
  # centered covariate measure 1.000 against its 2.
  expect_match(conditionMessage(w),
               "`prior_intercept` and `prior_beta` therefore bear")
  expect_match(conditionMessage(w), "shape\\^\\(n - 2k\\)")
  expect_match(conditionMessage(w), "log\\(expm1\\(shape \\* t\\)\\)")
  # A censored row that bounds the shape still suppresses the warning.
  expect_false(check(.surv_stub(c(1, 1, 1, exp(3)), status = c(1L, 1L, 1L, 0L),
                                x = c(-0.5, -0.5, 0.5, 0.5)),
                     distribution = "gompertz"))
  # The auxiliary has a name of its own rather than the Weibull fallback.
  w2 <- expect_warning(check(.surv_stub(rep(1, 4)), distribution = "gompertz",
                             aux_by = "none"))
  expect_match(conditionMessage(w2), "the Gompertz shape")
})

test_that("gompertz is read on the time scale, not the log one", {
  # The ridge is `eta = log(shape) - log(expm1(shape * t))`, about
  # `log(shape) - shape * t`, so it needs the event TIMES in the column
  # space and not their logarithms. Reading it on the log scale was wrong in
  # both directions, and these are the two directions.
  #
  # Five events at t = 1:5 over x = 0:4 are exactly linear in x on the time
  # scale and leave a residual of 0.085 of the total on the log scale. The
  # marginal slope `d log M / d log shape` is 1.000, so a Cauchy `prior_aux`
  # leaves `shape^-1` and no posterior. On the log scale the status is
  # `positive` and this returned silent.
  raw_exact <- .surv_stub(1:5, rep(1L, 5), x = 0:4)
  expect_warning(check(raw_exact, distribution = "gompertz"),
                 "on the time scale")
  # A location-scale family reads the same rows on the log scale and finds
  # real residual variation there, so it stays silent. The scale is the only
  # thing separating the two verdicts.
  expect_silent(check(raw_exact))
  expect_false(check(raw_exact))

  # The other direction. Three events at t = exp(0:2) fit exactly on the log
  # scale and not on the time scale, where the marginal FALLS by 577,014 per
  # decade of shape. The log-scale reading warned about a collapse Gompertz
  # does not have.
  log_exact <- .surv_stub(exp(0:2), rep(1L, 3), x = 0:2)
  expect_silent(check(log_exact, distribution = "gompertz"))
  expect_false(check(log_exact, distribution = "gompertz"))
  expect_error(check(log_exact), "could not be decided")
})

test_that("a censored gompertz row is placed on the time scale too", {
  # Same rows, two scales, opposite verdicts. Events at t = 1 and t = 4 over
  # x = 0 and 1 are fitted as `1 + 3x` on the time scale and as `4^x` on the
  # log scale, so at x = 0.5 the fitted time is 2.5 on one and 2 on the
  # other. A row censored at 2.2 therefore sits ABOVE the Gompertz fitted
  # time, where `S(c)` tends to one and bounds nothing, and BELOW the
  # log-normal one, where it tends to zero and bounds the scale.
  d <- .surv_stub(c(1, 4, 2.2), status = c(1L, 1L, 0L), x = c(0, 1, 0.5))
  expect_warning(check(d, distribution = "gompertz"), "on the time scale")
  expect_silent(check(d))
  expect_false(check(d))
})

test_that("a saturated gompertz design is not asserted proper", {
  # `n == rank` cancels the auxiliary's growth for the location-scale
  # families and does not for this one. Integrating a row's density over its
  # own `eta` gives `shape * e^(shape t) / expm1(shape t)`, which tends to
  # the SHAPE rather than to a constant, so a saturated design contributes
  # `shape^n` against `shape^-2` for each coefficient the ridge moves: the
  # marginal goes as `shape^(n - 2k)` and propriety fails once
  # `n >= 2k + 1`. Measured at `n == rank`: 1.000 for three events all at
  # t = 1 on a rank-3 design, where the times are the intercept alone and
  # `k` is one, and 2.000 for six rows whose times need two of six columns.
  # A Cauchy `prior_aux` takes off 2, leaving `shape^-1` and `shape^0`,
  # neither of which integrates.
  #
  # `k` is not decidable at double precision, which is the same reason the
  # rest of this guard does not guess at exactness, so every saturated
  # Gompertz takes the prior-tail warning rather than a guess at which ones
  # are the proper ones.
  sat <- .surv_stub(c(1, exp(1)), status = c(1L, 1L), x = c(-0.5, 0.5))
  w <- expect_warning(check(sat, distribution = "gompertz"))
  expect_match(conditionMessage(w), "as many as the free columns")
  expect_match(conditionMessage(w), "tail of `prior_aux`")
  expect_false(grepl("The posterior is proper", conditionMessage(w)))
  # The location-scale families keep the exemption, on the same measurement
  # that granted it: 0.000 for `weibull-aft` over eight decades of shape.
  w2 <- expect_warning(check(sat, distribution = "weibull-aft"))
  expect_match(conditionMessage(w2), "The posterior is proper")
  expect_false(grepl("tail of `prior_aux`", conditionMessage(w2)))
})

test_that("the shape warning names the prior that actually settles it", {
  # The exact-fit ridge does not move the same way for all four. Under the
  # AFT parameterizations an exact fit pins eta at log(t) and the ridge
  # leaves the coefficients where they are, so `prior_aux` carries it alone.
  # It does not under the other two, and pointing only at `prior_aux` there
  # sends the reader to the wrong sensitivity analysis.
  d <- .surv_stub(rep(1, 4))
  # `t e^-eta` is Gamma(shape, 1), mode at shape - 1, so the intercept falls
  # like -log(shape).
  w <- expect_warning(check(d, distribution = "gamma"))
  expect_match(conditionMessage(w), "prior_intercept")
  expect_match(conditionMessage(w), "-log\\(shape\\)")
  # Proportional-hazards Weibull: cumulative hazard t^shape exp(eta), so the
  # whole linear predictor scales with the shape.
  w <- expect_warning(check(d, distribution = "weibull"))
  expect_match(conditionMessage(w), "prior_beta")
  expect_match(conditionMessage(w), "proportional-hazards")
  # The AFT forms hold eta fixed, so they say `prior_aux` and stop.
  for (dist in c("weibull-aft", "loglogistic")) {
    w <- expect_warning(check(d, distribution = dist))
    expect_match(conditionMessage(w), "prior_aux")
    expect_false(grepl("prior_intercept|prior_beta", conditionMessage(w)),
                 label = dist)
  }
})

test_that("a saturated shape fit is promised propriety only where it holds", {
  # Saturated IS an exact fit, but `n == rank` is the one case where
  # integrating the coefficients out cancels the auxiliary's growth exactly.
  # Measured as `d log M / d log shape` from 10 to 1e6 on this design, with
  # the coefficients integrated against their default priors: 0.000 for
  # `weibull-aft`, 0.002 for `loglogistic` and negative throughout for
  # `gamma`. Telling those three that propriety turns on the `prior_aux`
  # tail was a false diagnosis.
  #
  # The proportional-hazards Weibull is the exception, and this is the
  # concrete case: two rank-2 rows both at t = 1. The profile likelihood is
  # exactly `k^2 e^-2`, verified by optimizing over eta at k = 1, 10, 100,
  # 1000, where `L / k^2` is 0.135335 throughout, and the marginal slope is
  # exactly 2.000 over eight decades. The coefficients stay at eta = 0
  # rather than moving into their prior tails, and `prior_cauchy()` is a
  # supported `prior_aux` contributing only `k^-2`, so the tail is constant
  # and does not integrate.
  d <- .surv_stub(c(1, 1), status = c(1L, 1L), x = c(-0.5, 0.5))
  w <- expect_warning(check(d, distribution = "weibull"))
  expect_match(conditionMessage(w), "tail of `prior_aux`")
  expect_false(grepl("The posterior is proper", conditionMessage(w)))
  # It still says what the geometry is, which is why it is exact.
  expect_match(conditionMessage(w), "as many as the free columns")
  for (dist in c("weibull-aft", "loglogistic", "gamma")) {
    w <- expect_warning(check(d, distribution = dist))
    expect_match(conditionMessage(w), "The posterior is proper")
    # The exponent named is the shape's GROWTH, not a scale density's.
    expect_match(conditionMessage(w), "exponent `n - rank`")
    expect_match(conditionMessage(w),
                 "sensitive to `prior_intercept` and `prior_beta`")
    expect_false(grepl("tail of `prior_aux`", conditionMessage(w)),
                 label = dist)
  }
  # The scale families keep the propriety claim, and it is right for them:
  # the exponent is `rank - n`, which is zero when the design is saturated.
  for (dist in c("lognormal", "gengamma")) {
    w <- expect_warning(check(d, distribution = dist))
    expect_match(conditionMessage(w), "The posterior is proper")
    expect_match(conditionMessage(w), "rank - n")
    expect_match(conditionMessage(w), "prior_beta")
  }
  # The exemption belongs to `n == rank` alone. With more uncensored rows
  # than the rank the cancellation is partial and the growth returns: the
  # same measurement on three rows over a rank-2 design gives 1.000 for
  # `weibull-aft` and `loglogistic` and 0.487 for `gamma`, which are
  # `n - rank` and `(n - rank) / 2`. Every shape family is warned about
  # there, as before.
  d4 <- .surv_stub(rep(1, 4))
  for (dist in c("weibull", "weibull-aft", "loglogistic", "gamma")) {
    expect_warning(check(d4, distribution = dist),
                   "depends on the tail of `prior_aux`")
  }
})

test_that("the censoring tolerance is scaled by cancellation, not by the result", {
  # The rounding in `Xc %*% beta` is governed by the size of the terms that
  # went into it, `|Xc| |beta|`, not by the size of what came out. An
  # ill-conditioned design with FULL numerical rank, which the rank guard
  # therefore lets through, reaches an exact fit through large cancelling
  # coefficients, and a predictor near zero then carries an absolute error
  # many orders above its own magnitude. A tolerance built from `abs(eta)`
  # was beaten by 48 of 1808 such rows, each a rounding artifact read as a
  # bound and an improper fit dispatched in silence. This is the bound
  # `.fit_ratios()` already uses.
  # One such row, stated exactly rather than sampled, so this test fails
  # against the old tolerance rather than only usually failing: the gap is
  # 2.84e-14 and the result-magnitude tolerance was 2.63e-14.
  d <- 0.012490285295965886
  x <- 1 + d * c(0, 1, 2, 3, 4)
  Xe <- cbind(1, x, x^2, x^3)
  b <- c(-155.79859154632462, 2.658432850546038,
         7.178998046692592, 131.15666646614784)
  ye <- as.vector(Xe %*% b)
  # Full numerical rank, so the rank guard above does not catch it.
  expect_identical(mlumr:::.exact_rank(Xe)$rank, 4L)
  expect_identical(stats::lm.fit(Xe, ye)$rank, 4L)
  expect_identical(
    mlumr:::.censoring_bounds_aux(rbind(Xe, Xe[1L, , drop = FALSE]),
                                  c(ye, ye[1L]),
                                  c(rep(TRUE, 5L), FALSE)),
    "undetermined"
  )
  # And a sweep, to show it is not one hand-picked design.
  set.seed(2026)
  v <- character(0)
  for (i in seq_len(300L)) {
    dd <- 10^stats::runif(1L, -3, -1.5)
    xx <- 1 + dd * c(0, 1, 2, 3, 4)
    X2 <- cbind(1, xx, xx^2, xx^3)
    if (mlumr:::.exact_rank(X2)$rank != 4L) next
    if (stats::lm.fit(X2, stats::rnorm(5L))$rank != 4L) next
    bb <- stats::rnorm(4L, sd = 100)
    y2 <- as.vector(X2 %*% bb)
    j <- sample.int(5L, 1L)
    v <- c(v, mlumr:::.censoring_bounds_aux(
      rbind(X2, X2[j, , drop = FALSE]), c(y2, y2[j]),
      c(rep(TRUE, 5L), FALSE)))
  }
  expect_gt(length(v), 100L)
  expect_identical(sum(v == "bounded"), 0L)
  # Still conservative in one direction only: a real gap is read as real.
  ev <- c(TRUE, TRUE, FALSE)
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, rep(-0.5, 3L)), c(0, 0, 1e-6), ev),
    "bounded"
  )
})

test_that("the censoring tolerance carries the least-squares solve error too", {
  # The dot-product bound covers the rounding in `Xc %*% beta`. It does not
  # cover the error already in `beta` from the QR solve, which the
  # conditioning of the design amplifies, and which a censored row that is an
  # EXTRAPOLATION of the event rows amplifies again. A censored row that
  # merely duplicates an event row does not show this: the fitted value there
  # is accurate to the backward error, which is why the earlier sweep of
  # duplicated rows came back clean at the same condition numbers.
  #
  # One such row stated exactly. The censored row is `w %*% Xe` with its own
  # exact fitted value `w %*% ye`, so the true gap is zero and it bounds
  # nothing. The condition number is 3.5e7.
  #
  # The constants are hex float literals because this failure lives in the
  # last bits: `%.17g` does NOT round-trip these doubles, and a decimal
  # transcription of this case returns "undetermined" under the OLD code as
  # well, which would make the test pass against the bug it is for.
  d <- as.numeric("0x1.5ef48d4faa0bbp-8")
  x <- 1 + d * c(0, 1, 2, 3, 4)
  Xe <- cbind(1, x, x^2, x^3)
  b <- as.numeric(c("-0x1.116557c36b40cp+7", "0x1.c6eadcf37f451p+5",
                    "0x1.fb7e1d0fbd995p+7", "-0x1.aaee84524599bp+5"))
  w <- as.numeric(c("0x1.3fa11f6ca81f4p+5", "0x1.04166d83129cp+5",
                    "-0x1.28553b6e229p+6", "-0x1.73e3f43d6b0c1p+4",
                    "0x1.87c33fbc666dap+4"))
  ye <- as.vector(Xe %*% b)
  expect_identical(stats::lm.fit(Xe, ye)$rank, 4L)
  expect_gt(kappa(Xe, exact = TRUE), 1e7)
  expect_identical(
    mlumr:::.censoring_bounds_aux(rbind(Xe, as.vector(w %*% Xe)),
                                  c(ye, sum(w * ye)),
                                  c(rep(TRUE, 5L), FALSE)),
    "undetermined"
  )
  # The conditioning is measured on the columns the fit USED, not on the
  # whole design: a rank-deficient one is singular, and its condition number
  # would be infinite and swallow every answer, including the
  # deficient-but-estimable rows this helper exists to give.
  ev <- c(TRUE, TRUE, FALSE)
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, rep(-0.5, 3L)), c(0, 0, 3), ev),
    "bounded"
  )
  expect_identical(
    mlumr:::.censoring_bounds_aux(cbind(1, rep(-0.5, 3L)), c(0, 0, 1e-6), ev),
    "bounded"
  )
})
