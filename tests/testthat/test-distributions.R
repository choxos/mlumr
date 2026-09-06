# Covariate-distribution helpers: qgamma/pgamma/dgamma and the logit-normal
# trio accept `mean`/`sd` and otherwise forward to stats:: unchanged.

test_that("gamma helpers reparameterize from mean and sd", {
  # shape = (mean/sd)^2, rate = mean/sd^2
  m <- 10; s <- 2
  shape <- (m / s)^2; rate <- m / s^2
  expect_equal(qgamma(0.5, mean = m, sd = s), stats::qgamma(0.5, shape = shape, rate = rate))
  expect_equal(pgamma(9, mean = m, sd = s), stats::pgamma(9, shape = shape, rate = rate))
  expect_equal(dgamma(9, mean = m, sd = s), stats::dgamma(9, shape = shape, rate = rate))
  # the reparameterization really does reproduce the requested moments
  x <- stats::rgamma(2e5, shape = shape, rate = rate)
  expect_equal(mean(x), m, tolerance = 0.02)
  expect_equal(stats::sd(x), s, tolerance = 0.05)
})

test_that("gamma helpers forward to stats when mean/sd are absent", {
  expect_identical(qgamma(0.5, shape = 2, rate = 1), stats::qgamma(0.5, shape = 2, rate = 1))
  expect_identical(pgamma(1.5, shape = 2, rate = 1), stats::pgamma(1.5, shape = 2, rate = 1))
  expect_identical(dgamma(1.5, shape = 2, rate = 1), stats::dgamma(1.5, shape = 2, rate = 1))
})

test_that("logit-normal helpers solve for mu/sigma from mean and sd", {
  q <- qlogitnorm(0.5, mean = 0.3, sd = 0.1)
  expect_true(is.finite(q) && q > 0 && q < 1)
  # the median of a logit-normal is plogis(mu), and the solved mean is on target
  pars <- mlumr:::.pars_logitnorm(0.3, 0.1)
  dens <- function(x) dlogitnorm(x, mu = pars$mu, sigma = pars$sigma)
  m <- stats::integrate(function(x) x * dens(x), 0, 1)$value
  expect_equal(m, 0.3, tolerance = 1e-3)
})

test_that("logit-normal helpers forward to their own parameterization", {
  expect_equal(qlogitnorm(0.5, mu = 0, sigma = 1), 0.5, tolerance = 1e-8)
  expect_equal(plogitnorm(0.5, mu = 0, sigma = 1), 0.5, tolerance = 1e-8)
  expect_true(dlogitnorm(0.5, mu = 0, sigma = 1) > 0)
})

test_that("invalid logit-normal moments raise the intended message", {
  # These guards previously called abort()/warn(), which exist nowhere in the
  # package, so every one of them failed with `could not find function "abort"`
  # instead of its message. R CMD check caught it as "no visible global function
  # definition"; keep it caught here.
  expect_error(qlogitnorm(0.5, mean = 50, sd = 10), "strictly inside \\(0, 1\\)")
  expect_error(qlogitnorm(0.5, mean = -0.2, sd = 0.1), "strictly inside \\(0, 1\\)")
  expect_error(mlumr:::.pars_logitnorm(c(0.2, 0.3), c(0.1, 0.1, 0.1)),
               "same length")
  # and nothing in the package reaches for a non-existent error helper
  for (nm in c("abort", "warn", "inform")) {
    expect_false(exists(nm, envir = asNamespace("mlumr"), inherits = FALSE),
                 info = nm)
  }
})

# ---- logit-normal moment parameterization ---------------------------------

test_that("logit-normal moment matching recovers the requested moments", {
  pars <- mlumr:::.pars_logitnorm(0.34, 0.19)
  mom <- mlumr:::.ln_moments(pars$mu, pars$sigma)
  expect_equal(unname(mom[["mean"]]), 0.34, tolerance = 1e-4)
  expect_equal(unname(mom[["sd"]]), 0.19, tolerance = 1e-4)
  # And the quantile function inverts the same solution.
  expect_equal(qlogitnorm(0.5, mean = 0.34, sd = 0.19),
               stats::plogis(pars$mu), tolerance = 1e-8)
})

test_that("half a moment specification is rejected, not silently ignored", {
  # `mu` / `sigma` carry defaults, so falling through to them turned
  # `distr(qlogitnorm, mean = bsa_mean)` into a standard logit-normal that has
  # neither the requested mean nor any relation to the data.
  expect_error(qlogitnorm(0.5, mean = 0.34), "needs both")
  expect_error(qlogitnorm(0.5, sd = 0.19), "needs both")
  expect_error(plogitnorm(0.5, mean = 0.34), "needs both")
  expect_error(dlogitnorm(0.5, sd = 0.19), "needs both")
  # The native parameterization still works untouched.
  expect_equal(qlogitnorm(0.5, mu = 0, sigma = 1), 0.5)
})

test_that("infeasible logit-normal moments are rejected before optimizing", {
  # A variable on (0, 1) has variance below mean * (1 - mean); the bound is
  # attained only by a two-point distribution on the boundaries.
  expect_error(qlogitnorm(0.5, mean = 0.5, sd = 0.6), "impossible")
  expect_error(qlogitnorm(0.5, mean = 0.02, sd = 0.2), "impossible")
  expect_error(qlogitnorm(0.5, mean = 0, sd = 0.1), "strictly inside")
  expect_error(qlogitnorm(0.5, mean = 1, sd = 0.1), "strictly inside")
  expect_error(qlogitnorm(0.5, mean = 0.3, sd = 0), "strictly positive")
  expect_error(qlogitnorm(0.5, mean = 0.3, sd = -0.1), "strictly positive")
  expect_error(qlogitnorm(0.5, mean = NA_real_, sd = 0.1), "finite")
})

test_that("logit-normal density and CDF are correct on and outside the support", {
  mu <- 0.2
  sigma <- 0.8

  # The Jacobian form dnorm(qlogis(x)) / (x (1 - x)) is 0/0 at x = 0 and x = 1,
  # and -Inf - (-Inf) on the log scale; both are NaN in floating point although
  # the density is zero there. Outside [0, 1] qlogis() is NaN with a warning.
  edges <- c(-0.5, 0, 1, 1.5)
  expect_equal(dlogitnorm(edges, mu = mu, sigma = sigma), rep(0, 4))
  expect_equal(dlogitnorm(edges, mu = mu, sigma = sigma, log = TRUE),
               rep(-Inf, 4))
  expect_silent(dlogitnorm(edges, mu = mu, sigma = sigma))

  # The distribution function is defined everywhere: 0 below, 1 above.
  expect_equal(plogitnorm(c(-0.5, 0), mu = mu, sigma = sigma), c(0, 0))
  expect_equal(plogitnorm(c(1, 1.5), mu = mu, sigma = sigma), c(1, 1))
  expect_silent(plogitnorm(edges, mu = mu, sigma = sigma))
  # `...` semantics survive the out-of-support handling.
  expect_equal(plogitnorm(edges, mu = mu, sigma = sigma, lower.tail = FALSE),
               c(1, 1, 0, 0))
  expect_equal(plogitnorm(edges, mu = mu, sigma = sigma, log.p = TRUE),
               c(-Inf, -Inf, 0, 0))

  # Missing values propagate rather than being swallowed by the boundary fill.
  expect_true(is.na(dlogitnorm(NA_real_, mu = mu, sigma = sigma)))
  expect_true(is.na(plogitnorm(NA_real_, mu = mu, sigma = sigma)))

  # The interior is unchanged: still the Jacobian form, and still a density.
  x <- seq(0.001, 0.999, length.out = 25)
  expect_equal(dlogitnorm(x, mu = mu, sigma = sigma),
               stats::dnorm(stats::qlogis(x), mu, sigma) / (x * (1 - x)))
  expect_equal(
    stats::integrate(function(z) dlogitnorm(z, mu = mu, sigma = sigma),
                     0, 1)$value,
    1, tolerance = 1e-6)
})

# ---- the moment parameterization validates what it is given ---------------

test_that("gamma moments are validated and converted without overflow", {
  # Both conversions square the SD, so a negative one returned exactly the
  # positive distribution.
  expect_error(qgamma(0.5, mean = 10, sd = -2), "strictly positive")
  expect_error(qgamma(0.5, mean = -10, sd = 2), "strictly positive")
  expect_error(qgamma(0.5, mean = 10, sd = NA_real_), "finite")
  expect_error(dgamma(1, mean = Inf, sd = 2), "finite")

  # `mean / sd^2` overflowed as soon as sd^2 did, at sd = 1.4e154, even where
  # the rate itself is representable.
  expect_equal(qgamma(0.5, mean = 1e200, sd = 1e200),
               stats::qgamma(0.5, shape = 1, rate = 1e-200))
  expect_equal(pgamma(1e200, mean = 1e200, sd = 1e200),
               stats::pgamma(1e200, shape = 1, rate = 1e-200))

  # Half a moment specification is a mistake: this used to return the shape-2
  # distribution and ignore the mean.
  expect_error(dgamma(1, shape = 2, mean = 5), "needs both")
  expect_error(qgamma(0.5, mean = 5), "needs both")
  expect_error(qgamma(0.5, shape = 2, sd = 1), "needs both")

  # The `scale = 1 / rate` default made both look supplied, and forwarding only
  # `scale` resolved the conflict silently in its favor.
  expect_error(qgamma(0.5, shape = 2, rate = 2, scale = 1), "not both")
  expect_identical(qgamma(0.5, shape = 2, rate = 2),
                   stats::qgamma(0.5, shape = 2, rate = 2))
  expect_identical(qgamma(0.5, shape = 2, scale = 0.5),
                   stats::qgamma(0.5, shape = 2, scale = 0.5))
})


# ---- the density lines its arguments up ------------------------------------

test_that("the logit-normal density aligns x with its parameters", {
  # Subsetting x to the open interval while leaving mu and sigma at full length
  # paired the surviving points with the wrong parameters.
  got <- dlogitnorm(c(0, 0.5), mu = c(0, 10), sigma = 1)
  expect_equal(got, c(0, stats::dnorm(0, 10, 1) / 0.25))
  expect_equal(dlogitnorm(c(0.5, 0.5), mu = c(0, 10), sigma = c(1, 2)),
               stats::dnorm(c(0, 0), c(0, 10), c(1, 2)) / 0.25)
  # A parameter that cannot be used must never read as a density of zero. It is
  # now an error rather than an NA, matching the moment parameterization, which
  # has always rejected a non-finite `mean` / `sd`: one unusable parameter used
  # to give three different answers depending on which of d/p/q saw it.
  expect_error(dlogitnorm(0, mu = NA_real_), "must not be missing")
  expect_error(dlogitnorm(0.5, mu = 0, sigma = NA_real_), "must not be missing")
  expect_equal(dlogitnorm(numeric(0), mu = 0, sigma = 1), numeric(0))
})

test_that("the logit-normal density takes `log` positionally, as dnorm does", {
  # It sat behind `...`, so a fourth positional argument was collected and
  # dropped and the natural-scale density came back instead.
  expect_equal(dlogitnorm(0.5, 0, 1, TRUE), dlogitnorm(0.5, 0, 1, log = TRUE))
  expect_equal(dlogitnorm(0.5, 0, 1, TRUE),
               base::log(dlogitnorm(0.5, 0, 1)))
})


# ---- moment conversion is exact, not merely convergent ---------------------

test_that("logit-normal moments are integrated accurately for a tight margin", {
  # The moments used to be integrated over x on (0, 1), where a concentrated
  # margin is a narrow spike that adaptive quadrature steps over. Because the
  # objective and its acceptance check both used that rule, the solver
  # certified a distribution whose SD it had never matched: for this pair it
  # reported 0.001100 where the truth is 0.001559, 42% low, and
  # add_integration() then drew points from the wrong distribution.
  pars <- mlumr:::.lnopt(0.01, 0.0011)
  expect_false(anyNA(pars))
  # An independent check that owes nothing to the package's own quadrature.
  u <- (seq_len(2e6) - 0.5) / 2e6
  x <- stats::plogis(pars[["mu"]] + pars[["sigma"]] * stats::qnorm(u))
  expect_equal(mean(x), 0.01, tolerance = 1e-5)
  expect_equal(sqrt(mean((x - mean(x))^2)), 0.0011, tolerance = 1e-4)
})

test_that("the moment solver reaches every feasible target it is given", {
  # A single Nelder-Mead pass reports convergence when its simplex collapses,
  # which on this objective happens short of the target; restarting until a
  # pass stops improving fixes it. Sweep the feasible region rather than one
  # convenient point.
  for (m in c(0.001, 0.01, 0.1, 0.5, 0.9, 0.99)) {
    for (f in c(0.05, 0.3, 0.7, 0.9)) {
      s <- f * sqrt(m * (1 - m))
      pars <- expect_silent(mlumr:::.lnopt(m, s))
      mom <- mlumr:::.ln_moments(pars[["mu"]], pars[["sigma"]])
      expect_equal(unname(mom[["mean"]]), m, tolerance = 1e-6,
                   info = sprintf("mean = %g, sd = %g", m, s))
      expect_equal(unname(mom[["sd"]]), s, tolerance = 1e-6,
                   info = sprintf("mean = %g, sd = %g", m, s))
    }
  }
})

test_that("the acceptance check is relative to the target moments", {
  # It was absolute at 1e-3, which is larger than either of these targets, so
  # a solution with mean 0.00147 and SD 0.00139 passed in silence.
  pars <- mlumr:::.lnopt(5e-4, 4e-4)
  expect_false(anyNA(pars))
  mom <- mlumr:::.ln_moments(pars[["mu"]], pars[["sigma"]])
  expect_equal(unname(mom[["mean"]]), 5e-4, tolerance = 1e-6)
  expect_equal(unname(mom[["sd"]]), 4e-4, tolerance = 1e-6)
})


test_that("the gamma wrappers still accept an abbreviated `scale`", {
  # Adding an `sd` formal alongside `scale` made `s` ambiguous, so a call
  # stats:: accepts stopped with "argument 3 matches multiple formal
  # arguments". Only formals before `...` are matched partially.
  expect_equal(qgamma(0.5, shape = 2, s = 4), stats::qgamma(0.5, shape = 2, s = 4))
  expect_equal(pgamma(1, shape = 2, s = 4), stats::pgamma(1, shape = 2, s = 4))
  expect_equal(dgamma(1, shape = 2, s = 4), stats::dgamma(1, shape = 2, s = 4))
  expect_equal(qgamma(0.5, shape = 2, r = 2), stats::qgamma(0.5, shape = 2, r = 2))
  # `...` exists only to hold the pair back from partial matching; it must not
  # become a place where a typo goes quiet.
  expect_error(qgamma(0.5, shape = 2, nope = 1), "unused argument")
  expect_error(qgamma(0.5, shape = 2, m = 5), "full names")
  # And the moment parameterization is unaffected by the move.
  expect_equal(qgamma(0.5, mean = 10, sd = 2),
               stats::qgamma(0.5, shape = 25, rate = 2.5))
})

test_that("an infeasible logit-normal pair is named, not indexed out of bounds", {
  # The feasibility test recycles a scalar `sd` against a vector `mean`, then
  # reported the pair with the index that test produced.
  expect_error(qlogitnorm(0.5, mean = c(0.5, 0.01), sd = 0.3), "impossible")
  expect_error(qlogitnorm(0.5, mean = 0.01, sd = c(0.005, 0.3)), "impossible")
  expect_error(qlogitnorm(0.5, mean = c(0.01, 0.5), sd = 0.3), "impossible")
  # Feasible recycling still works, in both directions.
  expect_length(qlogitnorm(0.5, mean = c(0.2, 0.3), sd = 0.05), 2L)
  expect_length(qlogitnorm(0.5, mean = 0.3, sd = c(0.05, 0.1)), 2L)
})

# ---- margin classification -------------------------------------------------

test_that("qlogitnorm is classified as a continuous margin", {
  # The fallback classifier evaluates the quantile function on 99 points; a
  # concentrated logit-normal returns 1 at every one of them to machine
  # precision, so it was labeled binary and handed the binary copula
  # correlation adjustment.
  expect_equal(unname(get_distribution_type(x = distr(qlogitnorm, mu = 20,
                                                      sigma = 0.01))),
               "continuous")
  expect_equal(unname(get_distribution_type(x = distr(qlogitnorm, mu = 0,
                                                      sigma = 1))),
               "continuous")
})
