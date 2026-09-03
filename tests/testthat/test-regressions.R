# Regression tests for the methodology and code fixes made during v0.2.0
# hardening. These pin the pure-R guards, relabelings, and estimand contracts so
# a future change cannot silently undo them (most need no Stan sampling).

test_that("mismatched Surv length is rejected for IPD (C1)", {
  df <- data.frame(trt = "A", age = c(0.1, -0.2, 0.3, 0.0),
                   male = c(0L, 1L, 0L, 1L))
  sv_short <- survival::Surv(c(5, 6), c(1, 0))          # 2 rows vs 4 data rows
  expect_error(
    set_ipd(df, "trt", covariates = c("age", "male"), family = "survival",
            Surv = sv_short),
    "match exactly"
  )
})

test_that("mismatched Surv length is rejected for AgD survival (C1)", {
  agd <- data.frame(trt = "B", age_mean = 0.2, age_sd = 1, male_prop = 0.4)
  agd <- agd[rep(1L, 4L), ]                             # 4 pseudo-individual rows
  sv_short <- survival::Surv(c(5, 6), c(1, 0))          # 2 rows
  expect_error(
    set_agd_surv(agd, "trt", Surv = sv_short,
                 cov_means = c("age_mean", "male_prop"),
                 cov_sds = c("age_sd", NA),
                 cov_types = c("continuous", "binary")),
    "match exactly"
  )
})

test_that("entry_time is combined with a right-censored Surv, not discarded (H1)", {
  df <- data.frame(trt = "A", age = c(0.1, -0.2, 0.3, 0.0),
                   male = c(0L, 1L, 0L, 1L), entry = c(1, 2, 3, 4))
  sv <- survival::Surv(c(5, 6, 7, 8), c(1, 1, 1, 1))
  obj <- set_ipd(df, "trt", covariates = c("age", "male"), family = "survival",
                 Surv = sv, entry_time = "entry")
  expect_equal(obj$data$.delay_time, c(1, 2, 3, 4))
})

test_that("counting-type Surv plus entry_time is rejected as double-specified (H1)", {
  df <- data.frame(trt = "A", age = c(0.1, -0.2, 0.3, 0.0),
                   male = c(0L, 1L, 0L, 1L), entry = c(1, 2, 3, 4))
  sv_counting <- survival::Surv(c(1, 2, 3, 4), c(5, 6, 7, 8), c(1, 1, 1, 1))
  expect_error(
    set_ipd(df, "trt", covariates = c("age", "male"), family = "survival",
            Surv = sv_counting, entry_time = "entry"),
    "already encoded"
  )
})

test_that("combine_data rejects a shared treatment label (H4)", {
  ipd <- set_ipd(data.frame(trt = "A", outcome = c(1, 0, 1, 1), x1 = c(0.1, 0.2, -0.1, 0)),
                 "trt", "outcome", "x1")
  agd <- set_agd(data.frame(trt = "A", n_total = 100, n_events = 40, x1_mean = 0.3),
                 "trt", outcome_n = "n_total", outcome_r = "n_events",
                 cov_means = "x1_mean")
  expect_error(combine_data(ipd, agd), "distinct treatments")
})

test_that("an all-NA treatment column is rejected (M10)", {
  agd_na <- data.frame(trt = NA_character_, n_total = 100, n_events = 40,
                       x1_mean = 0.3)
  expect_error(
    set_agd(agd_na, "trt", outcome_n = "n_total", outcome_r = "n_events",
            cov_means = "x1_mean"),
    "missing"
  )
})

test_that("integer controls reject values above .Machine$integer.max (M10)", {
  expect_error(mlumr:::.validate_mlumr_integer(2^31, "seed", 0L),
               "and <=")
  expect_silent(mlumr:::.validate_mlumr_integer(2026L, "seed", 0L))
})

test_that("make_knots caps n_knots to a sane range (M10)", {
  skip_if_not_installed("survival")
  dat <- sim_survival_data(n_ipd = 40, n_agd = 40, n_int = 8)
  expect_error(make_knots(dat, n_knots = 100), "0, 50")
})

test_that("the naive normal benchmark requires >= 2 IPD observations (M13)", {
  ipd <- suppressWarnings(
    set_ipd(data.frame(trt = "A", outcome = 5, x1 = 0.1),
            "trt", "outcome", "x1", family = "normal")
  )
  agd <- set_agd(data.frame(trt = "B", y_mean = 3, y_se = 0.5, x1_mean = 0.2),
                 "trt", family = "normal", outcome_mean = "y_mean",
                 outcome_se = "y_se", cov_means = "x1_mean")
  dat <- combine_data(ipd, agd)
  expect_error(naive(dat), "two IPD")
})

test_that("prior_sensitivity validates its grid, probs, and dots (M9)", {
  stub <- structure(list(priors = list(beta = prior_normal(0, 1)),
                         model = "spfa"),
                    class = "mlumr_fit")
  expect_error(prior_sensitivity(stub, prior_beta_scales = numeric(0)),
               "one or more positive")
  expect_error(prior_sensitivity(stub, probs = c(-0.1, 0.5)), "\\[0, 1\\]")
  # Use a protected arg that reaches `...` (prior_beta* partial-match the
  # `_scales` formals and raise R's own ambiguity error instead).
  expect_error(prior_sensitivity(stub, prior_intercept = prior_normal(0, 5)),
               "cannot override")
})

test_that("model-comparison labels are disambiguated (L1)", {
  nm <- mlumr:::.comparison_names(list(1, 2), c("SPFA", "SPFA"))
  expect_false(nm[[1]] == nm[[2]])
})

test_that("survival STC labels a cumulative-hazard ratio, not a hazard ratio (H8)", {
  skip_if_not_installed("flexsurv")
  dat <- sim_survival_data(n_ipd = 80, n_agd = 80, n_int = 8)
  res <- suppressWarnings(stc(dat, distribution = "weibull", n_boot = 0))
  expect_true(all(c("log_chr", "chr") %in% names(res)))
  expect_false(any(c("loghr", "hr") %in% names(res)))
})

test_that("the gengamma log-survival helper uses the stable upper-tail form", {
  # log1m(gamma_p(a, x)) loses all precision in the upper tail, where gamma_p
  # rounds to 1; log(gamma_q(a, x)) is the stable form. v0.2.0 ships ONE such
  # helper, log_surv_scalar(). The precomputed-eta twin log_surv_pre() is
  # deferred to v0.2.1 (inst/future/v0.2.1/), and the dual-path check that both
  # twins use the stable form travels with it.
  f <- system.file("stan", "include", "survival_functions.stan", package = "mlumr")
  if (!nzchar(f)) {
    f <- testthat::test_path("..", "..", "inst", "stan", "include",
                             "survival_functions.stan")
  }
  skip_if_not(file.exists(f))
  code <- grep("^\\s*//", readLines(f, warn = FALSE), value = TRUE, invert = TRUE)
  expect_false(any(grepl("log1m(gamma_p", code, fixed = TRUE)))
  expect_gte(sum(grepl("log(gamma_q", code, fixed = TRUE)), 1L)
})


# --------------------------------------------------------------------------
# v0.2.0 pre-release verification pass (July 2026).
# --------------------------------------------------------------------------

test_that("the normal comparator weight field matches what Stan weights by (A1)", {
  # The normal Stan models weight the comparator-population marginal by
  # `agd_weight`; the R link-scale path in predict() must use the same field, or
  # type = "link" and type = "response" describe different target populations.
  expect_identical(get_family_config("normal")$comp_weight_field, "agd_weight")
  expect_identical(get_family_config("binomial")$comp_weight_field, "n_agd")
  expect_identical(get_family_config("poisson")$comp_weight_field, "E_agd")

  weighted <- vapply(
    c("mlumr_normal_spfa", "mlumr_normal_relaxed"),
    function(model) {
      path <- system.file("stan", paste0(model, ".stan"), package = "mlumr")
      if (!nzchar(path)) path <- file.path("..", "..", "inst", "stan",
                                           paste0(model, ".stan"))
      skip_if_not(file.exists(path), "Stan source not available")
      any(grepl("agd_weight[k]", readLines(path, warn = FALSE), fixed = TRUE))
    },
    logical(1)
  )
  expect_true(all(weighted))
})

test_that(".compute_marginal_link uses stable normal generated quantities", {
  fit <- structure(
    list(
      family = "normal", model = "spfa", link = "log",
      draws = data.frame(
        y_index_comparator = c(Inf, exp(2)),
        link_y_index_comparator = c(800, 2),
        check.names = FALSE
      )
    ),
    class = "mlumr_fit"
  )
  linked <- mlumr:::.compute_marginal_link(fit, "y_index_comparator")
  expect_equal(linked$y_index_comparator, c(800, 2))
})

test_that(".compute_marginal_link reads older finite normal-log fits", {
  fit <- structure(
    list(
      family = "normal", model = "spfa", link = "log",
      draws = data.frame(y_index_comparator = exp(c(0, 2)))
    ),
    class = "mlumr_fit"
  )
  linked <- mlumr:::.compute_marginal_link(fit, "y_index_comparator")
  expect_equal(linked$y_index_comparator, c(0, 2))
})

test_that("the effect-measures table tolerates NULL interval bounds (B1)", {
  # exp(NULL) raises "non-numeric argument to mathematical function"; the table
  # builder must normalize bounds before exponentiating them.
  x <- list(family = "survival", link = "log",
            rmst_diff = 1.2, se = NA_real_, ci_lower = NULL, ci_upper = NULL,
            log_chr = -0.3, log_chr_se = NA_real_,
            log_chr_lower = NULL, log_chr_upper = NULL)
  tab <- mlumr:::.effect_measures_df(x)
  expect_s3_class(tab, "data.frame")
  expect_true(all(is.na(tab$CI_lower)))
  expect_equal(tab$Estimate[tab$Measure == "RMST difference"], 1.2)

  y <- list(family = "binomial", link = "logit", estimate = 0.4, se = 0.1,
            ci_lower = NULL, ci_upper = NULL)
  tab2 <- mlumr:::.effect_measures_df(y)
  expect_true("Odds ratio" %in% tab2$Measure)
  expect_true(all(is.na(tab2$CI_lower)))
})

test_that("the effect-measures table retains infinite exact ratios", {
  tab <- mlumr:::.effect_measures_df(list(
    family = "binomial", link = "logit",
    estimate = 1000, se = 1, ci_lower = 998, ci_upper = 1002,
    rd = 1, rd_se = 0, rd_lower = 1, rd_upper = 1,
    log_rr = 1000, log_rr_lower = 998, log_rr_upper = 1002
  ))
  expect_identical(tab$Estimate[tab$Measure == "Odds ratio"], Inf)
  expect_identical(tab$Estimate[tab$Measure == "Risk ratio"], Inf)
})

test_that("poisson benchmarks report a rate difference (B3)", {
  set.seed(2026)
  n <- 150
  x <- stats::rnorm(n)
  ipd <- data.frame(trt = "A", y = stats::rpois(n, exp(0.2 + 0.4 * x)),
                    x = x, e = 1)
  i <- set_ipd(ipd, treatment = "trt", outcome = "y", covariates = "x",
               family = "poisson", exposure = "e")
  a <- set_agd(data.frame(trt = "B", n = 150, count = 180, E = 150,
                          x_mean = 0.3, x_sd = 1),
               treatment = "trt", family = "poisson", outcome_n = "n",
               outcome_r = "count", outcome_E = "E",
               cov_means = "x_mean", cov_sds = "x_sd",
               cov_types = "continuous")
  dat <- add_integration(combine_data(i, a), n_int = 32,
                         x = distr(qnorm, mean = x_mean, sd = x_sd),
                         verbose = FALSE)

  for (res in list(naive(dat), stc(dat))) {
    expect_true(is.finite(res$rd))
    expect_true(is.finite(res$rd_se))
    expect_lt(res$rd_lower, res$rd_upper)
    tab <- mlumr:::.effect_measures_df(res)
    expect_true("Rate difference" %in% tab$Measure)
  }
})

test_that("the relaxed identifiability warning counts distinct profiles, not rows (B2)", {
  make_data <- function(x_means, z_means) {
    set.seed(2026)
    n <- 120
    x <- stats::rnorm(n)
    ipd <- data.frame(trt = "A", y = x + stats::rnorm(n),
                      x = x, z = stats::rnorm(n))
    i <- set_ipd(ipd, treatment = "trt", outcome = "y",
                 covariates = c("x", "z"), family = "normal")
    k <- length(x_means)
    a <- set_agd(data.frame(trt = "B", n = rep(60, k),
                            y_mean = rep(0, k), y_se = rep(0.2, k),
                            x_mean = x_means, x_sd = 1,
                            z_mean = z_means, z_sd = 1),
                 treatment = "trt", family = "normal", outcome_n = "n",
                 outcome_mean = "y_mean", outcome_se = "y_se",
                 cov_means = c("x_mean", "z_mean"),
                 cov_sds = c("x_sd", "z_sd"),
                 cov_types = c("continuous", "continuous"))
    add_integration(combine_data(i, a), n_int = 32,
                    x = distr(qnorm, mean = x_mean, sd = x_sd),
                    z = distr(qnorm, mean = z_mean, sd = z_sd),
                    verbose = FALSE)
  }

  # 4 rows, all the same covariate profile: rank 1, so beta_comparator is not
  # identified even though the row count (4) passes the old n_agd_rows >=
  # 2 * n_cov check.
  dup <- make_data(rep(0.5, 4), rep(-0.2, 4))
  expect_equal(mlumr:::.agd_covariate_rank(dup), 1L)
  expect_equal(nrow(dup$agd$data), 4L)

  # 4 rows varying in one covariate only: rank 2, still short of n_cov + 1 = 3.
  partial <- make_data(c(-1, -0.2, 0.4, 1.1), rep(-0.2, 4))
  expect_equal(mlumr:::.agd_covariate_rank(partial), 2L)

  # 4 genuinely distinct profiles: rank 3 = n_cov + 1, enough to identify.
  distinct <- make_data(c(-1, -0.2, 0.4, 1.1), c(0.1, 0.9, -0.5, 0.3))
  expect_equal(mlumr:::.agd_covariate_rank(distinct), 3L)

  skip_on_cran()
  skip_if_not_installed("rstan")
  # A deliberately tiny fit emits many unrelated ESS/Rhat warnings, so collect
  # everything and assert on the one that matters rather than letting hundreds
  # of diagnostic warnings surface as test noise.
  seen <- character()
  withCallingHandlers(
    mlumr(dup, model = "relaxed", chains = 1, iter = 60, warmup = 30,
          seed = 2026, refresh = 0, verbose = FALSE),
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("aggregate design rank 1", seen)))
  expect_true(any(grepl("independent profiles", seen)))
  expect_true(any(grepl("subgroup rows", seen)))
})

test_that("an incomplete chain set is recorded and reported, not hidden (C4)", {
  # Both Stan backends drop a chain that terminates abnormally and return a fit
  # built from the survivors. Everything downstream is then computed from fewer
  # chains than requested, so the counts must reach the diagnostics and be
  # surfaced by check_diagnostics() and summary().
  fit <- structure(
    list(
      family = "binomial", model = "spfa", link = "logit",
      draws = data.frame(mu_index = c(0, 0.1), mu_comparator = c(0, 0.1)),
      summary = data.frame(variable = "mu_index", Rhat = 1.0, n_eff = 1000,
                           ess_tail = 1000),
      diagnostics = list(n_divergent = 0, n_max_treedepth = 0,
                         n_chains_requested = 4L, n_chains_returned = 1L),
      sampling_args = list(adapt_delta = 0.95, max_treedepth = 15)
    ),
    class = "mlumr_fit"
  )
  expect_warning(check_diagnostics(fit), "Only 1 of 4 requested chain")

  # A complete run says nothing about chains.
  fit$diagnostics$n_chains_returned <- 4L
  expect_silent(check_diagnostics(fit))

  # The counting helper agrees with the chain labels, and tolerates NULL labels
  # (backends that cannot report them) by assuming the run was complete.
  expect_equal(mlumr:::.n_chains_returned(c(1, 1, 2, 2, 3), 4L), 3L)
  expect_equal(mlumr:::.n_chains_returned(NULL, 4L), 4L)
  expect_equal(mlumr:::.n_chains_returned(integer(0), 2L), 2L)
})


test_that("a difference of two unbounded quantities is not reported as zero", {
  # +Inf - +Inf is indeterminate. The equality branch used to fire first and
  # return an exact null effect. Two -Inf logs are a different case: both
  # quantities are zero, so their difference really is zero.
  expect_true(is.nan(mlumr:::.exp_difference_logs(Inf, Inf)))
  expect_equal(mlumr:::.exp_difference_logs(-Inf, -Inf), 0)
  expect_equal(mlumr:::.exp_difference_logs(1, 1), 0)
  expect_equal(mlumr:::.exp_difference_logs(Inf, 0), Inf)
  expect_equal(mlumr:::.exp_difference_logs(0, Inf), -Inf)
  # Recycled, so the guard has to be elementwise.
  out <- mlumr:::.exp_difference_logs(c(Inf, -Inf, 2), Inf)
  expect_true(is.nan(out[1]))
  expect_equal(out[2], -Inf)
})

test_that("the Stan difference helper guards the same case", {
  src <- readLines(
    system.file("stan", "include", "numerical_functions.stan", package = "mlumr")
  )
  body <- paste(src, collapse = "\n")
  expect_match(body, "not_a_number\\(\\)", fixed = FALSE)
})

test_that("an unseeded fit uses the documented default rather than RNG state", {
  # `.Random.seed` exists after ANY use of the RNG, not only after set.seed(),
  # because R initializes it on demand from the clock and the process id.
  # Reading it made an unseeded fit silently irreproducible.
  set.seed(2026)
  invisible(stats::runif(1))
  before <- .Random.seed
  expect_warning(res <- mlumr:::.resolve_mlumr_seed(NULL), "default seed 2026")
  expect_equal(res$value, 2026L)
  expect_equal(res$source, "default")
  # Resolving the seed must not advance the caller's RNG stream.
  expect_identical(.Random.seed, before)

  expect_silent(user <- mlumr:::.resolve_mlumr_seed(7))
  expect_equal(user$value, 7L)
  expect_equal(user$source, "user")
})

test_that("a rank-deficient covariate design is reported, not skipped", {
  # The guard returned quietly whenever there were no more rows than
  # covariates, which is precisely when the design cannot be full rank.
  df <- data.frame(x1 = c(0, 1), x2 = c(0, 2))
  expect_warning(
    mlumr:::.warn_collinear_ipd_covariates(df, c("x1", "x2")),
    "rank deficient"
  )
  # With enough rows the ordinary condition-number path still applies.
  set.seed(2026)
  wide <- data.frame(x1 = stats::rnorm(50), x2 = stats::rnorm(50))
  expect_silent(mlumr:::.warn_collinear_ipd_covariates(wide, c("x1", "x2")))
})

test_that("permuted integration grids count as one aggregate profile", {
  # The likelihood averages over a row's integration points, so it sees the
  # multiset of tuples and not their order. Comparing the grids as stored made
  # one duplicated row look like two independent constraints.
  pts <- matrix(c(0.1, 0.4, 0.9, 1.2, 2.2, 3.3), ncol = 2)
  x_int <- array(0, dim = c(2L, 3L, 2L))
  x_int[1, , ] <- pts
  x_int[2, , ] <- pts[c(3L, 1L, 2L), ]
  data <- list(agd = list(data = data.frame(trt = c("B", "B"))),
               integration_points = x_int)
  expect_equal(mlumr:::.agd_distinct_profiles(data), 1L)

  x_int[2, , ] <- pts + 5
  data$integration_points <- x_int
  expect_equal(mlumr:::.agd_distinct_profiles(data), 2L)
})

test_that("a collapsed realized design is preferred over the declared one", {
  # Equal ranks are not equal designs: declared means c(-1, 1) and realized
  # means c(-1e-10, 1e-10) both have rank 2, but the likelihood has almost no
  # leverage along that direction.
  declared <- matrix(c(-1, 1), ncol = 1)
  expect_false(
    mlumr:::.realized_matches_declared(declared, matrix(c(-1e-10, 1e-10), ncol = 1))
  )
  expect_true(
    mlumr:::.realized_matches_declared(declared, matrix(c(-1.01, 0.99), ncol = 1))
  )
  expect_true(mlumr:::.realized_matches_declared(declared, NULL))
})

test_that("check_identification refuses a fit that has no comparator block", {
  spfa <- structure(list(model = "spfa", data = structure(list(), class = "mlumr_data")),
                    class = c("mlumr_fit", "list"))
  expect_error(check_identification(spfa), "no\n?\\s*comparator-only coefficients")
})

test_that("a comparator prior sweep is declined, not fatal, on an SPFA fit", {
  spfa <- structure(
    list(family = "binomial", model = "spfa",
         priors = list(beta = prior_normal(0, 2.5))),
    class = c("mlumr_fit", "list")
  )
  # The value is invalid, but the argument does not apply to this model at all,
  # so the answer is "ignored", not an error about its contents.
  expect_warning(
    try(prior_sensitivity(spfa, prior_beta_comparator_scales = -1,
                          verbose = FALSE), silent = TRUE),
    "ignored for the spfa model"
  )
})

test_that("a comparison with no finite entries is not reported as agreement", {
  # max(x, na.rm = TRUE) returns -Inf for an all-NA vector, which passed every
  # threshold and certified agreement computed from nothing.
  expect_true(is.na(mlumr:::.max_finite(c(NA_real_, NaN, Inf))))
  expect_equal(mlumr:::.max_finite(c(NA_real_, 0.3, 0.1)), 0.3)
  expect_equal(mlumr:::.moment_verdict(NA_real_, 0.05, "close"), "unavailable")
  expect_equal(mlumr:::.moment_verdict(0.01, 0.05, "close"), "close")
  expect_equal(mlumr:::.moment_verdict(0.5, 0.05, "close"), "review")
})

test_that("autoscaling survives a covariate whose SD is undefined", {
  # A single IPD row makes stats::sd() NA rather than 0. `any(NA)` then aborted
  # with "missing value where TRUE/FALSE needed" instead of falling back.
  expect_warning(
    fields <- stan_prior_fields_beta(
      prior_normal(0, 2.5, autoscale = TRUE), n_cov = 2L,
      sd_x = c(NA_real_, 2), covariate_names = c("age", "weight")
    ),
    "no usable empirical SD"
  )
  expect_true(all(is.finite(fields$sd)))
  expect_equal(fields$sd[[1]], 2.5)     # unscaled fallback
  expect_equal(fields$sd[[2]], 2.5 / 2) # autoscaled as usual
})

test_that("custom knots make n_knots irrelevant rather than invalid", {
  # The resolved basis is validated on its own terms, so rejecting an unused
  # n_knots turned a valid custom specification into an error.
  expect_silent(
    mlumr:::.validate_survival_controls(
      pred_times = NULL, rmst_horizon = NULL, mspline_degree = 0L,
      n_knots = 0L, distribution = "pexp",
      knots = list(internal = c(2, 5), boundary = c(0, 10))
    )
  )
  expect_error(
    mlumr:::.validate_survival_controls(
      pred_times = NULL, rmst_horizon = NULL, mspline_degree = 0L,
      n_knots = 0L, distribution = "pexp"
    ),
    "cannot be used with"
  )
})

test_that("exposure and standard-error bounds stay strictly positive in Stan", {
  # Released v0.1.0 declared these <lower=1e-12>. Relaxing them to <lower=0>
  # admits an exposure of exactly zero (log(0) in the linear predictor) and a
  # zero aggregate standard error (an improper normal likelihood).
  for (f in c("mlumr_poisson_spfa", "mlumr_poisson_relaxed")) {
    src <- readLines(system.file("stan", paste0(f, ".stan"), package = "mlumr"))
    expect_true(any(grepl("vector<lower=1e-12>[n_ipd] E_ipd;", src, fixed = TRUE)))
    expect_true(any(grepl("real<lower=1e-12> E_agd;", src, fixed = TRUE)))
  }
  for (f in c("mlumr_normal_spfa", "mlumr_normal_relaxed")) {
    src <- readLines(system.file("stan", paste0(f, ".stan"), package = "mlumr"))
    expect_true(any(grepl("real<lower=1e-12> se_agd;", src, fixed = TRUE)))
  }
})

test_that("rstan draws carry a chain-aware tail ESS", {
  skip_if_not_installed("posterior")
  set.seed(2026)
  draws <- data.frame(a = stats::rnorm(400), b = stats::rnorm(400))
  chain_ids <- rep(1:4, each = 100)
  out <- mlumr:::.rstan_ess_tail(draws, chain_ids)
  expect_named(out, c("a", "b"))
  expect_true(all(is.finite(out)))
  # Unequal chain lengths cannot be reshaped, so the diagnostic is reported as
  # unavailable rather than computed from a wrong layout.
  expect_true(all(is.na(mlumr:::.rstan_ess_tail(draws, c(rep(1L, 150), rep(2L, 250))))))
})
