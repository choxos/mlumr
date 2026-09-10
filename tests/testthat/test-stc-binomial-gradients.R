# The binomial STC delta method. Its gradients are analytic, so equivalent
# units of a predictor give equivalent uncertainty; the central difference
# they replace stepped by max(1, |beta|) * eps^(1/3) in coefficient space,
# which is not a property of the model: a predictor in units 1e6 larger has
# a coefficient 1e6 smaller and the same step moved the target linear
# predictor by 6.

make_logistic_units <- function(multiplier, link = "logit") {
  # Both outcomes occur at both profiles, so there is no separation:
  # 200/400 events at x = 0 and 30/40 at x = 1. The model is saturated on
  # the two profiles, so the fitted probability at x = 1 is 30/40 under
  # every link, and its delta-method SE is sqrt(0.75 * 0.25 / 40).
  ip <- set_ipd(
    data.frame(trt = "A",
               y = c(rep(0L, 200), rep(1L, 200), rep(0L, 10), rep(1L, 30)),
               x = multiplier * c(rep(0, 400), rep(1, 40))),
    "trt", outcome = "y", covariates = "x"
  )
  # The target is the observed high profile: a zero-SD normal margin puts
  # every integration point at x = multiplier.
  ag <- set_agd(data.frame(trt = "B", n = 10000L, r = 5000L,
                           x_mean = multiplier, x_sd = 0),
                "trt", outcome_n = "n", outcome_r = "r",
                cov_means = "x_mean", cov_sds = "x_sd",
                cov_types = "continuous")
  d <- suppressWarnings(add_integration(
    combine_data(ip, ag), n_int = 64,
    x = distr(stats::qnorm, mean = x_mean, sd = x_sd), verbose = FALSE
  ))
  suppressWarnings(stc(d, link = link))
}

test_that("the standardized probability's SE does not depend on the units of x", {
  exact <- sqrt(0.75 * 0.25 / 40)
  for (link in c("logit", "probit", "cloglog")) {
    rows <- lapply(c(1, 1e3, 1e6, 1e8), function(m) {
      s <- make_logistic_units(m, link)
      c(p = s$p_hat_index, se = s$p_hat_index_se, link_se = s$se,
        log_rr_se = s$log_rr_se, rd_se = s$rd_se)
    })
    rows <- do.call(rbind, rows)
    # The GLM's own convergence tolerance is what separates these from the
    # exact values; the invariance below is at rounding.
    expect_equal(rows[, "p"], rep(0.75, 4), tolerance = 1e-6)
    # Before: 0.0685, 0.0685, 0.0317, 0.0187 across the four multipliers.
    expect_equal(rows[, "se"], rep(exact, 4), tolerance = 1e-4)
    for (col in c("p", "se", "link_se", "log_rr_se", "rd_se")) {
      expect_lt(max(abs(rows[, col] / rows[1, col] - 1)), 1e-8)
    }
  }
})

test_that("the analytic gradients agree with central differences where those resolve", {
  set.seed(2026)
  X <- cbind(1, rnorm(50), rbinom(50, 1, 0.4))
  w <- runif(50)
  central <- function(fn, beta) {
    h <- 1e-5 * pmax(1, abs(beta))
    vapply(seq_along(beta), function(j) {
      u <- l <- beta
      u[j] <- u[j] + h[j]
      l[j] <- l[j] - h[j]
      (fn(u) - fn(l)) / (2 * h[j])
    }, numeric(1))
  }
  for (link in c("logit", "probit", "cloglog")) {
    log_means <- function(b) {
      lp <- mlumr:::.binary_log_probs(as.vector(X %*% b), link)
      c(event = mlumr:::.weighted_log_mean_exp(lp$event, w),
        nonevent = mlumr:::.weighted_log_mean_exp(lp$nonevent, w))
    }
    fns <- list(
      log_mean = function(b) log_means(b)[["event"]],
      log_nonevent_mean = function(b) log_means(b)[["nonevent"]],
      mean = function(b) exp(log_means(b)[["event"]]),
      link = function(b) {
        lm <- log_means(b)
        mlumr:::.binary_link_from_logs(lm[["event"]], lm[["nonevent"]], link)
      }
    )
    for (beta in list(c(0.2, 0.5, -0.3), c(-4, 1, 0.5), c(3, -1, 0.5))) {
      g <- mlumr:::.stc_binomial_gradients(X, as.vector(X %*% beta), w, link)
      for (name in names(fns)) {
        reference <- central(fns[[name]], beta)
        scale <- max(abs(reference))
        expect_lt(max(abs(g[[name]] - reference)) / scale, 1e-6,
                  label = paste(link, name, beta[1]))
      }
    }
  }
})

test_that("a heterogeneous target keeps its link-scale SE across units", {
  # Two target profiles at x = -1 and x = 1 with weights 1/4 and 3/4, an
  # exact logistic MLE of (0, log 3) and a coefficient covariance of
  # diag(1/15, 1/15). The link-scale SE is 0.23094011; a step rule in
  # coefficient space gave 0.2117 at units 1e6 and 0.2066 at 1e8.
  b <- c(0, log(3))
  V <- diag(c(1 / 15, 1 / 15))
  Xt <- cbind(1, c(-1, 1))
  w <- c(0.25, 0.75)
  mu <- plogis(as.vector(Xt %*% b))
  p <- sum(w * mu)
  exact_gp <- colSums(w * mu * (1 - mu) * Xt)
  exact_link <- exact_gp / (p * (1 - p))
  exact_se <- sqrt(as.numeric(t(exact_link) %*% V %*% exact_link))
  expect_equal(exact_se, 0.23094011, tolerance = 1e-7)
  for (c in c(1, 1e3, 1e6, 1e8)) {
    D <- diag(c(1, c))
    Di <- diag(c(1, 1 / c))
    X <- Xt %*% D
    beta <- as.vector(Di %*% b)
    vcov <- Di %*% V %*% Di
    g <- mlumr:::.stc_binomial_gradients(X, as.vector(X %*% beta), w, "logit")
    expect_equal(sqrt(as.numeric(t(g$link) %*% vcov %*% g$link)), exact_se,
                 tolerance = 1e-12)
    expect_equal(sqrt(as.numeric(t(g$mean) %*% vcov %*% g$mean)),
                 sqrt(as.numeric(t(exact_gp) %*% V %*% exact_gp)),
                 tolerance = 1e-12)
  }
})

test_that("the gradients stay finite in tails the probabilities cannot represent", {
  # Event probabilities near 1e-18 and 1 - 1e-18: the log-scale forms keep
  # each point's share where the probabilities themselves would round.
  set.seed(2026)
  X <- cbind(1, rnorm(20))
  for (link in c("logit", "probit", "cloglog")) {
    for (beta0 in c(-40, 40)) {
      g <- mlumr:::.stc_binomial_gradients(X, as.vector(X %*% c(beta0, 0.5)),
                                           rep(1, 20), link)
      expect_true(all(is.finite(unlist(g))), label = paste(link, beta0))
    }
  }
  # Logit and probit at eta = -800 and 800, beyond any representable
  # probability: the log probabilities are still finite and so are the
  # gradients, and the share of a point at -800 beside one at 0 is 0.
  for (link in c("logit", "probit")) {
    X <- cbind(1, c(0, 1))
    g <- mlumr:::.stc_binomial_gradients(X, c(0, -800), c(1, 1), link)
    expect_true(all(is.finite(unlist(g))), label = link)
    expect_equal(g$log_mean, c(g$log_mean[1], 0), tolerance = 1e-12)
  }
  # Cloglog at eta = 800: exp(eta) overflows, the point's non-event
  # probability is 0 and its share of the non-event mean is 0. The product
  # 0 * -Inf used to be NaN and poisoned every SE while the estimate stayed
  # finite. The point at eta = 0 carries the whole non-event mean, so the
  # gradient of log q-bar is -X_1 and the link gradient is that over
  # log q-bar = log(1/2) - 1.
  X <- cbind(1, c(0, 1))
  g <- mlumr:::.stc_binomial_gradients(X, c(0, 800), c(1, 1), "cloglog")
  expect_true(all(is.finite(unlist(g))))
  expect_equal(g$log_nonevent_mean, c(-1, 0), tolerance = 1e-12)
  expect_equal(g$link, c(-1, 0) / (log(0.5) - 1), tolerance = 1e-12)
  # A zero weight is no share, not NaN.
  g0 <- mlumr:::.stc_binomial_gradients(X, c(800, 0), c(0, 1), "cloglog")
  expect_true(all(is.finite(unlist(g0))))
  expect_equal(g0$log_nonevent_mean, c(-1, -1), tolerance = 1e-12)
})
