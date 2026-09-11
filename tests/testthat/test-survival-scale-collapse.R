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

test_that("a saturated shape fit is not promised a proper posterior", {
  # Saturated IS an exact fit: a design with as many free columns as event
  # rows reproduces every one of them. So a shape family diverges here for
  # the same reason it does under `exact`, and the propriety the scale
  # families get is not something to claim for it.
  #
  # The concrete case: two rank-2 PH Weibull event rows both at t = 1. The
  # profile likelihood is exactly `k^2 e^-2`, verified by optimizing over
  # eta at k = 1, 10, 100, 1000, where `L / k^2` is 0.135335 throughout: the
  # coefficients stay at eta = 0 rather than moving into their prior tails.
  # `prior_cauchy()` is a supported `prior_aux` and contributes only `k^-2`,
  # so the tail is constant and does not integrate.
  d <- .surv_stub(c(1, 1), status = c(1L, 1L), x = c(-0.5, 0.5))
  for (dist in c("weibull", "weibull-aft", "loglogistic", "gamma")) {
    w <- expect_warning(check(d, distribution = dist))
    expect_match(conditionMessage(w), "tail of `prior_aux`")
    expect_false(grepl("The posterior is proper", conditionMessage(w)),
                 label = dist)
    # It still says what the geometry is, which is why it is exact.
    expect_match(conditionMessage(w), "as many as the free columns")
  }
  # The scale families keep the propriety claim, and it is right for them:
  # the exponent is `rank - n`, which is zero when the design is saturated.
  for (dist in c("lognormal", "gengamma")) {
    w <- expect_warning(check(d, distribution = dist))
    expect_match(conditionMessage(w), "The posterior is proper")
    expect_match(conditionMessage(w), "rank - n")
    expect_match(conditionMessage(w), "prior_beta")
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
