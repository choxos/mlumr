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

test_that("delayed entry and left or interval censoring are not examined", {
  # Their contributions are conditional probabilities whose limits are a
  # separate argument. Silence, not a guess in either direction.
  expect_false(check(.surv_stub(rep(1, 4), entry = c(0.1, 0.1, 0.1, 0.1))))
  d <- .surv_stub(rep(1, 4))
  d$ipd$data$.status <- c(1L, 1L, 2L, 1L)
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
  expect_warning(check(d), "sensitive to `prior_beta`")
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
  expect_warning(check(d, distribution = "weibull"),
                 "the Weibull shape will concentrate")
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
  # A near-exact fit is proper whatever the comparator does, so it is left
  # alone rather than warned about a second time.
  expect_silent(check(.surv_stub(exp(c(-1, -1, 1, 1) + c(0, 1e-9, 0, 1e-9))),
                      aux_by = "none"))
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
