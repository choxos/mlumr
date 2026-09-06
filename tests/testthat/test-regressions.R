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
  # `prior_sensitivity()` now defers to the shared `.validate_probs()` rather
  # than carrying its own copy of the check, so the message is the package-wide
  # one. Duplicates are the case the local copy missed: they produced two
  # identically named `qNN` columns and only the last survived.
  expect_error(prior_sensitivity(stub, probs = c(-0.1, 0.5)),
               "unique finite numeric values between 0 and 1")
  expect_error(prior_sensitivity(stub, probs = c(0.5, 0.5)),
               "unique finite numeric values between 0 and 1")
  # Distinct doubles are not enough. These two differ, and both are written
  # `q3`, so the loop below assigned that column twice and the first quantile
  # asked for was gone. The check is on the names the summary will use.
  collide <- c((0.1 + 0.2) / 10, 0.3 / 10)
  expect_false(collide[[1]] == collide[[2]])
  expect_identical(.quantile_names(collide), c("q3", "q3"))
  expect_error(prior_sensitivity(stub, probs = collide),
               "share a summary column: q3")
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
  f <- stan_source_path("include", "survival_functions.stan")
  expect_true(file.exists(f))
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
      path <- stan_source_path(paste0(model, ".stan"))
      expect_true(file.exists(path))
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

  # The counting helper agrees with the chain labels. Labels it cannot read are
  # reported as unknown rather than as a complete run: see the dedicated test
  # below.
  expect_equal(mlumr:::.n_chains_returned(c(1, 1, 2, 2, 3), 4L), 3L)

  # An unknown layout is announced instead of passing as complete.
  fit$diagnostics$n_chains_returned <- NA_integer_
  expect_warning(check_diagnostics(fit), "could not be labeled by chain")
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
  # `system.file()` returns "" when the package is loaded from source without
  # its inst/ tree shimmed, which would make `readLines()` abort rather than
  # report. Fall back to the source tree, and require the file either way: a
  # skip here would hide a packaging defect.
  path <- stan_source_path("include", "numerical_functions.stan")
  expect_true(file.exists(path))
  body <- paste(readLines(path), collapse = "\n")
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
    src <- readLines(stan_source_path(paste0(f, ".stan")))
    expect_true(any(grepl("vector<lower=1e-12>[n_ipd] E_ipd;", src, fixed = TRUE)))
    expect_true(any(grepl("real<lower=1e-12> E_agd;", src, fixed = TRUE)))
  }
  for (f in c("mlumr_normal_spfa", "mlumr_normal_relaxed")) {
    src <- readLines(stan_source_path(paste0(f, ".stan")))
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


# --- PR #24 review follow-ups ------------------------------------------------

.stc_boundary_fixture <- function(agd_df) {
  set.seed(2026)
  n <- 200
  x1 <- rbinom(n, 1, 0.4)
  outcome <- rbinom(n, 1, plogis(-0.5 + 1.0 * x1))
  ipd <- set_ipd(data.frame(trt = "A", outcome = outcome, x1 = x1),
                 "trt", "outcome", "x1")
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total", outcome_r = "n_events",
                 cov_means = "x1_mean")
  dat <- add_integration(combine_data(ipd, agd), n_int = 32,
                         x1 = distr(qbern, prob = x1_mean))
  stc(dat)
}

test_that("a zero-event comparator arm keeps its uncertainty in binomial STC", {
  res <- .stc_boundary_fixture(
    data.frame(trt = "B", n_total = 100, n_events = 0, x1_mean = 0.3)
  )
  # With the raw binomial variance p(1 - p)/n, a 0/100 arm has variance exactly
  # 0: the interval collapsed to a point and the risk-difference SE ignored the
  # comparator entirely, although 0/100 alone is consistent with p up to
  # roughly 0.03. `.naive_binomial()` already corrected this.
  expect_gt(res$p_comparator_se, 0)
  expect_gt(res$p_comparator_upper, res$p_comparator_lower)
  expect_gt(res$rd_se, 0)
})

test_that("binomial STC standard errors do not depend on comparator tabulation", {
  pooled <- .stc_boundary_fixture(
    data.frame(trt = "B", n_total = 100, n_events = 0, x1_mean = 0.3)
  )
  # The same arm described as two equal strata. The boundary correction takes
  # its `n` from the pooled total, so both descriptions must agree; with each
  # row's own `n`, 0/100 corrected to 0.5/101 and 0/50 twice to 0.5/51.
  split <- .stc_boundary_fixture(
    data.frame(trt = "B", n_total = c(50, 50), n_events = c(0, 0),
               x1_mean = c(0.3, 0.3))
  )
  expect_equal(pooled$p_comparator_se, split$p_comparator_se)
  expect_equal(pooled$rd_se, split$rd_se)
  expect_equal(pooled$se, split$se)
})

test_that("the match test uses the same floor as the identification screens", {
  f <- .realized_matches_declared
  # A purely relative test disagreed with `.profile_rank()` and the `spread`
  # screen in a window of its own: declared means of c(-0.08, 0.08) against
  # realized c(-0.04, 0.04) keep exactly half their spread, so the declared
  # profiles were reported as reproduced, while the grid the likelihood
  # integrates over sits at 0.04 IPD SDs, below the 0.05 floor, and ranks 1
  # where the declared design ranks 2.
  declared <- matrix(c(-0.08, 0.08), ncol = 1)
  realized <- matrix(c(-0.04, 0.04), ncol = 1)
  expect_equal(.profile_rank(declared, 1), 2L)
  expect_equal(.profile_rank(realized, 1), 1L)
  expect_false(f(declared, realized, 1))
  # Half the spread is still a match when both sides clear the floor.
  expect_true(f(matrix(c(-1, 1), ncol = 1), matrix(c(-0.6, 0.6), ncol = 1), 1))
  # A declared direction already below the floor carries nothing the likelihood
  # can use, so the grid is not required to reproduce it.
  expect_true(f(matrix(c(-0.01, 0.01), ncol = 1),
                matrix(c(-0.001, 0.001), ncol = 1), 1))
  # A grid that cannot be compared at all is announced, not reported as a match
  # that was checked.
  expect_warning(f(matrix(c(-1, 1), ncol = 1),
                   cbind(c(-1, 1), c(0, 1)), 1),
                 "could not be compared")
  expect_warning(f(matrix(c(-1, 1), ncol = 1),
                   matrix(c(NaN, 1), ncol = 1), 1),
                 "could not be compared")
})


test_that("realized integration geometry is compared direction by direction", {
  f <- .realized_matches_declared
  # Same spectrum, different covariate. A sorted singular-value comparison
  # cannot tell these apart and reported a match, so `check_identification()`
  # described spread in covariate 1 while the likelihood only saw covariate 2.
  expect_false(f(cbind(c(-1, 1), c(0, 0)), cbind(c(0, 0), c(-1, 1))))
  # The case the rank test could not see stays caught.
  expect_false(f(cbind(c(-1, 1)), cbind(c(-1e-10, 1e-10))))
  # A genuinely two-dimensional design with one direction collapsed.
  declared <- cbind(c(-1, 1, 0), c(0, 0, 2))
  expect_false(f(declared, cbind(c(-0.3, 0.3, 0), c(0, 0, 2))))
  expect_false(f(declared, cbind(c(-1, 1, 0), c(0, 0, 0.6))))
  expect_true(f(declared, declared))
  # Quadrature noise moves a singular value by a relative O(1 / n_int).
  expect_true(f(cbind(c(-1, 1), c(2, 4)),
                cbind(c(-1.001, 1.002), c(2.001, 3.999))))
})


test_that("a realized grid that collapses onto a diagonal is caught", {
  f <- .realized_matches_declared
  rank <- .profile_rank
  # Per-axis lengths are not a rank. A grid that collapses onto a diagonal of
  # two declared axes keeps a long component on each axis separately, so a
  # columnwise comparison passed it while it spanned one direction where the
  # declared design spanned two. `.profile_rank()` counted the drop; this
  # function did not, and the two screens contradicted each other on the same
  # design.
  declared <- cbind(c(-1, 1, 0), c(0, 0, 2))
  diagonal <- cbind(c(-0.5, 0.5, 1), c(-0.5, 0.5, 1))
  expect_equal(rank(declared, c(1, 1)), 3L)
  expect_equal(rank(diagonal, c(1, 1)), 2L)
  expect_false(f(declared, diagonal, c(1, 1)))

  # The realistic form: a 2 x 2 subgroup table whose two `distr()` calls were
  # both keyed off the same margin, which is one copy-and-paste away.
  table_2x2 <- cbind(c(0, 0, 1, 1), c(0, 1, 0, 1))
  both_sex <- cbind(table_2x2[, 1], table_2x2[, 1])
  expect_equal(rank(table_2x2, c(0.5, 0.5)), 3L)
  expect_equal(rank(both_sex, c(0.5, 0.5)), 2L)
  expect_false(f(table_2x2, both_sex, c(0.5, 0.5)))

  # A faithful grid still passes, and so does one shrunk within tolerance, so
  # the stricter test did not simply start rejecting everything.
  expect_true(f(declared, declared, c(1, 1)))
  expect_true(f(declared, declared * 0.8, c(1, 1)))
  expect_false(f(declared, declared * 0.4, c(1, 1)))

  # Both collapses again with no reference scale. Without `ref_sd` the
  # absolute floor is skipped and the share test is the only guard, so this is
  # the path where the singular-value half of that test is load bearing. Every
  # case above supplies `ref_sd`, under which the floor catches these anyway,
  # and dropping the singular values from the share test went unnoticed.
  expect_false(f(declared, diagonal, NULL))
  expect_false(f(table_2x2, both_sex, NULL))
  expect_true(f(declared, declared, NULL))
})


test_that("a sorted singular-value pairing alone is too lenient", {
  f <- .realized_matches_declared
  # Singular values arrive sorted and carry no direction, so on their own they
  # judge the largest realized combination against the largest declared
  # direction even when that energy sits somewhere else. A grid keeping 40
  # percent of the dominant direction then passes on the surplus it carries on
  # the second, which the per-axis lengths reject. Both tests are applied.
  declared <- cbind(c(-2, -2, 2, 2), c(-1, 1, -1, 1))
  short_on_first <- cbind(0.4 * declared[, 1], declared[, 2])
  expect_false(f(declared, short_on_first, c(1, 1)))

  # The same hole as a swap of two counted directions: the spectrum is
  # identical, the directions are exchanged.
  swapped_declared <- cbind(c(-2, 2, 0), c(0, 0, 0.5))
  swapped <- cbind(c(0, 0, 0.5), c(-2, 2, 0))
  expect_false(f(swapped_declared, swapped, c(1, 1)))

  # And the directional test alone stays too lenient for the diagonal case, so
  # neither check is redundant.
  diag_declared <- cbind(c(-1, 1, 0), c(0, 0, 2))
  expect_false(f(diag_declared, cbind(c(-0.5, 0.5, 1), c(-0.5, 0.5, 1)), c(1, 1)))
  expect_true(f(diag_declared, diag_declared, c(1, 1)))
})


test_that("the grid comparison never rejects a design the rank screen accepts", {
  f <- .realized_matches_declared

  # The boundary case, constructed rather than hoped for. A 2 x 1 design has
  # spread `a` in IPD SDs, so a declared 0.052 sits just above the 0.05 floor
  # and a realized 0.048 just below it. The share is 0.92, far above `factor`,
  # so this is the floor acting alone: the grid is faithful in proportion and
  # still not a direction the likelihood can use.
  declared <- matrix(c(-0.052, 0.052), ncol = 1)
  realized <- matrix(c(-0.048, 0.048), ncol = 1)
  expect_false(f(declared, realized, 1))
  # And the rank screen says the same thing about the same pair, which is the
  # property that matters: one direction declared, none realized.
  expect_equal(.profile_rank(declared, 1), 2L)
  expect_equal(.profile_rank(realized, 1), 1L)

  # State it as an invariant over a sweep as well. Given a faithful grid
  # (quadrature-scale noise), any rejection must be one where `.profile_rank()`
  # independently counts fewer directions in the realized grid. A rejection
  # without a rank drop would be this function inventing a mismatch the rest of
  # the package does not see.
  # A sweep whose designs never approach the floor asserts nothing about the
  # floor. Drawn from an arbitrary scale, every grid matched and `dropped` was
  # never consulted, so `matched || dropped` was `expect_true(TRUE)` 300 times
  # and stayed green with the rank comparison replaced by `FALSE`. Put the
  # generator ON the boundary instead: a two-row design has spread exactly
  # `half / ref_sd`, so drawing that target either side of 0.05 and perturbing
  # it makes the noise cross the floor. Assert the EQUIVALENCE, and require
  # both outcomes to occur so the sweep cannot go quiet again.
  set.seed(2026)
  accepted <- 0L
  rejected <- 0L
  for (trial in seq_len(400)) {
    ref_sd <- stats::runif(1, 0.2, 3)
    half <- stats::runif(1, 0.045, 0.055) * ref_sd
    dec <- matrix(c(-half, half), ncol = 1)
    faithful <- dec + matrix(stats::rnorm(2, sd = 0.03 * half), 2, 1)
    matched <- isTRUE(f(dec, faithful, ref_sd))
    dropped <- .profile_rank(faithful, ref_sd) < .profile_rank(dec, ref_sd)
    expect_equal(matched, !dropped)
    if (matched) accepted <- accepted + 1L else rejected <- rejected + 1L
    # A grid IDENTICAL to the declared one always reproduces it. This is the
    # strongest form of the property and holds with no tolerance argument.
    expect_true(f(dec, dec, ref_sd))
  }
  expect_gt(rejected, 0L)
  expect_gt(accepted, 0L)
})


test_that("one tolerance governs every spread comparison", {
  f <- .realized_matches_declared

  # Slack on a single comparison moves the disagreement instead of removing
  # it. With a tolerant floor here and a bare `>=` in `.profile_rank()`, a
  # realized spread inside the slack window cleared the floor while the rank
  # screen counted the direction as lost. Both now use `.at_least()`, so the
  # two answers track each other across the window.
  declared <- matrix(c(-0.052, 0.052), ncol = 1)
  for (edge in c(0.0499999999, 0.0499999997, 0.0499999994, 0.048)) {
    realized <- matrix(c(-edge, edge), ncol = 1)
    dropped <- .profile_rank(realized, 1) < .profile_rank(declared, 1)
    expect_equal(isTRUE(f(declared, realized, 1)), !dropped)
  }

  # The share test needs the same tolerance, for the same reason: the two
  # realized measurements are separate routes to one quantity, so requiring
  # both against a bare threshold rejects a grid sitting exactly on `factor`.
  dec <- matrix(c(-1.3792740098849141, -1.110798484273599, -1.9041066897235663,
                  -2.9223694564651317, 2.1287187527194424,
                  0.66839307325321073), ncol = 1)
  center <- mean(dec)
  expect_true(f(dec, center + 0.5 * (dec - center), 1))
  expect_false(f(dec, center + 0.49 * (dec - center), 1))

  # The tolerance is relative, so it means the same thing at any spread.
  for (scale_by in c(1e-3, 1, 1e3)) {
    scaled <- matrix(c(-0.052, 0.052) * scale_by, ncol = 1)
    expect_true(f(scaled, scaled, scale_by))
    expect_false(f(scaled, scaled * 0.5, scale_by))
  }

})


test_that("the WEAK flag uses the same floor as the rank screen", {
  # The `flagged` screen is a fifth route to this threshold and has to move
  # with the others. Left on a bare `<`, a spread inside the slack window
  # counted as a direction for the rank screen, so `mlumr()` stayed silent,
  # while `check_identification()` called the same design WEAK.
  #
  # Driven through `check_identification()` rather than through the helper,
  # because it is the `flagged` field that carries the contradiction: an
  # assertion on `.at_least()` itself passes whatever `flagged` does.
  at_spread <- function(target) {
    set.seed(2026)
    n <- 200L
    x <- stats::rnorm(n)
    # For a two-row design the reported spread is exactly `half / sd(x)`, so
    # solving for `half` lands it on a chosen side of the floor.
    half <- target * stats::sd(x)
    ipd <- data.frame(trt = "A", y = x + stats::rnorm(n), x = x)
    i <- set_ipd(ipd, treatment = "trt", outcome = "y",
                 covariates = "x", family = "normal")
    a <- set_agd(data.frame(trt = "B", n = c(60, 60),
                            y_mean = c(0, 0), y_se = c(0.2, 0.2),
                            x_mean = c(-half, half), x_sd = c(1, 1)),
                 treatment = "trt", family = "normal", outcome_n = "n",
                 outcome_mean = "y_mean", outcome_se = "y_se",
                 cov_means = "x_mean", cov_sds = "x_sd",
                 cov_types = "continuous")
    joined <- combine_data(i, a)
    dat <- add_integration(joined, n_int = 32,
                           x = distr(qnorm, mean = x_mean, sd = x_sd),
                           verbose = FALSE)
    suppressMessages(check_identification(dat, verbose = FALSE))
  }

  # Inside the tolerance window: the rank screen counts the direction, so the
  # flag must not contradict it.
  inside <- at_spread(0.05 * (1 - 5e-9))
  expect_equal(inside$diagnostic_scope, "identity")
  expect_lt(inside$spread, 0.05)
  expect_false(inside$flagged)

  # Genuinely below the floor is still flagged, so the tolerance did not
  # simply disable the screen.
  below <- at_spread(0.05 * (1 - 1e-6))
  expect_lt(below$spread, 0.05)
  expect_true(below$flagged)

  # And a design comfortably clear of the floor stays unflagged.
  clear <- at_spread(0.2)
  expect_false(clear$flagged)
})


test_that("a threshold that is not a number is rejected, not silently NA", {
  # `NA` and `NaN` are not thresholds. Comparing against one returns NA rather
  # than a verdict, and the NA surfaced far from its cause: `.profile_rank()`
  # answered `NA_integer_`, and `.realized_matches_declared()` died at
  # `if (!any(counts))` with "missing value where TRUE/FALSE needed".
  declared <- cbind(c(-1, 1))
  for (bad in list(NA, NA_real_, NaN)) {
    expect_error(.profile_rank(declared, 1, min_spread = bad),
                 "Spread threshold values must not be")
    matches <- function() {
      .realized_matches_declared(declared, declared, 1, min_spread = bad)
    }
    expect_error(matches(), "Spread threshold values must not be")
  }

  # The message names no argument, because this helper is called with three
  # different thresholds and naming one would misreport the other two. It also
  # avoids the singular: a threshold may be a VECTOR, the shape
  # `factor * declared_spread` has, so "must be a number" would state a scalar
  # constraint the function does not impose. A partly-NA vector is rejected on
  # the same footing as a scalar one, and a wholly finite vector is accepted.
  expect_error(.at_least(c(1, 2), c(0.5, NA)), "Spread threshold values")
  expect_equal(.at_least(c(1, 2), c(0.5, 1.5)), c(TRUE, TRUE))

  # `factor` is the other way a threshold goes NA: `share_of` is
  # `factor * declared_spread`, and `declared_spread` is finite by the time it
  # is used, so an NA there came from `factor`. Blaming `min_spread` would send
  # the reader to the argument that is still correct.
  bad_factor <- function() {
    .realized_matches_declared(declared, declared, 1, factor = NA)
  }
  # Take the condition FROM `expect_error()`, which cannot hand one back unless
  # something was raised. Reaching for the message with
  # `tryCatch(bad_factor(), error = conditionMessage)` instead is vacuous in
  # exactly the case this guards: with the check removed the call returns a
  # value rather than raising, `tryCatch` passes that value straight through,
  # and `grepl` on `NA` or `TRUE` is FALSE, so the assertion holds while the
  # thing it tests has gone.
  err <- expect_error(bad_factor(), "Spread threshold values must not be")
  expect_false(grepl("min_spread", conditionMessage(err), fixed = TRUE))

  # An infinite threshold IS coherent: nothing clears `Inf`, everything clears
  # `-Inf`. Those keep working, on a bare comparison, since there is no slack
  # to compute from an infinity.
  expect_false(.at_least(1, Inf))
  expect_true(.at_least(1, -Inf))
  expect_true(.realized_matches_declared(declared, declared, 1,
                                         min_spread = Inf))
  expect_equal(.profile_rank(declared, 1, min_spread = Inf), 1L)
})


test_that("a declared spread sitting exactly on the floor still matches itself", {
  # `counts` decides which declared directions clear `min_spread` from one
  # decomposition; the realized side recomputes that same quantity by two
  # other routes, column norms of the projection and a second decomposition.
  # The routes agree only to within a few ULPs, so a declared spread landing
  # exactly on 0.05 was counted by the first and fell a ULP short of the
  # second, and a grid identical to the declared one was reported as failing
  # to reproduce it.
  set.seed(10)
  n <- 8L
  p <- 3L
  basis <- qr.Q(qr(cbind(1, matrix(stats::rnorm(n * (n - 1L)), n, n - 1L))))
  u <- basis[, -1L][, seq_len(p)]
  v <- qr.Q(qr(matrix(stats::rnorm(p * p), p, p)))[, seq_len(p)]
  declared <- u %*% diag(c(1, 0.05, 0.05) * sqrt(n), p) %*% t(v)

  # Two of the three spreads straddle the floor by less than a ULP.
  centered <- sweep(scale(declared, center = TRUE, scale = FALSE),
                    2L, c(1, 1, 1), "/")
  spreads <- svd(centered)$d / sqrt(n)
  expect_true(any(abs(spreads - 0.05) < 1e-15))

  expect_true(.realized_matches_declared(declared, declared, c(1, 1, 1)))
})


test_that("the two identification screens agree on an undecomposable design", {
  # `.profile_rank()` fails closed on a mean matrix that carries NA, which is
  # the legacy integration-mean case its own comment names. `.subgroup_geometry()`
  # is handed the SAME matrix by `check_identification()` and used to abort
  # inside LAPACK with "infinite or missing values in 'x'", so one screen
  # reported an unidentified design and the other stopped the call.
  M <- cbind(c(0, 1, NA), c(1, 0, 1))
  expect_equal(.profile_rank(M, c(1, 1)), 0L)
  geom <- .subgroup_geometry(M, c(1, 1))
  expect_equal(geom$spread, 0)
  expect_equal(geom$cond_inv, 0)
  expect_equal(geom$eff_dim, 0)

  # A finite design is untouched.
  ok <- .subgroup_geometry(cbind(c(0, 1, 2), c(1, 0, 1)), c(1, 1))
  expect_gt(ok$spread, 0)
  expect_gt(ok$cond_inv, 0)
})

test_that("an unlabeled chain layout is reported as unknown, not as success", {
  # `NULL` chain ids mean the backend could not divide the draws into chains,
  # which is the abnormal layout the diagnostic exists to notice. Returning the
  # requested count asserted that every chain came back.
  expect_true(is.na(.n_chains_returned(NULL, 4L)))
  expect_true(is.na(.n_chains_returned(integer(0), 4L)))
  expect_equal(.n_chains_returned(rep(1:3, each = 10L), 4L), 3L)

  # Tail ESS from unlabeled draws would describe a layout that is not the
  # fit's, so it is withheld.
  draws <- data.frame(a = rnorm(20), b = rnorm(20))
  expect_true(all(is.na(.rstan_ess_tail(draws, NULL))))
  expect_true(all(is.na(.rstan_ess_tail(draws, rep(1L, 19L)))))
})

test_that("R rejects the exposures and standard errors Stan rejects", {
  # The Stan data blocks declare `<lower=1e-12>`; below that the fit died at
  # initialization with a message naming a Stan variable, not the column.
  ipd_df <- data.frame(trt = "A", y = c(1L, 2L), e = c(1, 1e-13), x = c(0, 1))
  expect_error(
    set_ipd(ipd_df, "trt", "y", "x", exposure = "e", family = "poisson"),
    "at least 1e-12"
  )
  expect_silent(
    set_ipd(data.frame(trt = "A", y = c(1L, 2L), e = c(1, 1), x = c(0, 1)),
            "trt", "y", "x", exposure = "e", family = "poisson")
  )
  # The bound is inclusive. A validator written with `>` would pass every
  # rejection case above and still refuse a legal boundary value.
  expect_silent(
    set_ipd(data.frame(trt = "A", y = c(1L, 2L), e = c(1e-12, 1), x = c(0, 1)),
            "trt", "y", "x", exposure = "e", family = "poisson")
  )

  agd_norm <- data.frame(trt = "B", m = 1, s = 1e-13, x_mean = 0.5, x_sd = 1)
  expect_error(
    set_agd(agd_norm, "trt", family = "normal",
            outcome_mean = "m", outcome_se = "s",
            cov_means = "x_mean", cov_sds = "x_sd"),
    "at least 1e-12"
  )

  agd_pois <- data.frame(trt = "B", r = 5, E = 1e-13, x_mean = 0.5, x_sd = 1)
  expect_error(
    set_agd(agd_pois, "trt", family = "poisson",
            outcome_r = "r", outcome_E = "E",
            cov_means = "x_mean", cov_sds = "x_sd"),
    "at least 1e-12"
  )

  agd_norm$s <- 1e-12
  expect_silent(
    set_agd(agd_norm, "trt", family = "normal",
            outcome_mean = "m", outcome_se = "s",
            cov_means = "x_mean", cov_sds = "x_sd")
  )
  agd_pois$E <- 1e-12
  expect_silent(
    set_agd(agd_pois, "trt", family = "poisson",
            outcome_r = "r", outcome_E = "E",
            cov_means = "x_mean", cov_sds = "x_sd")
  )
})


test_that("an ignored autoscale on prior_intercept warns exactly once", {
  # The warning and the family/link block were duplicated inside `mlumr()`, and
  # the second copy carried pre-`prior_beta_comparator` text, so a user setting
  # autoscale on prior_intercept got two warnings that disagreed about which
  # priors autoscaling reaches. `check_link()` runs immediately after the
  # warning, so an invalid link stops the call there without sampling.
  set.seed(2026)
  n <- 60
  ipd <- set_ipd(
    data.frame(trt = "A", y = rbinom(n, 1, 0.5), x = rbinom(n, 1, 0.4)),
    "trt", "y", "x"
  )
  agd <- set_agd(
    data.frame(trt = "B", n_total = 100, n_events = 40, x_mean = 0.3),
    "trt", outcome_n = "n_total", outcome_r = "n_events",
    cov_means = "x_mean"
  )
  dat <- suppressWarnings(add_integration(
    combine_data(ipd, agd), n_int = 8, x = distr(qbern, prob = x_mean)
  ))

  seen <- character(0)
  expect_error(
    withCallingHandlers(
      mlumr(dat, link = "nonsense", seed = 2026,
            prior_intercept = prior_normal(0, 10, autoscale = TRUE)),
      warning = function(cond) {
        seen <<- c(seen, conditionMessage(cond))
        invokeRestart("muffleWarning")
      }
    ),
    "is not valid for family"
  )
  autoscale <- seen[grepl("autoscaling is only applied to", seen, fixed = TRUE)]
  expect_length(autoscale, 1L)
  expect_match(autoscale, "prior_beta_comparator", fixed = TRUE)
})


test_that("a withheld target-correlation comparison has the documented verdict", {
  # `cor_adjust = "none"` declares a latent Gaussian-copula matrix, which the
  # realized covariate-scale correlation does not estimate, so the comparison
  # is withheld. Leaving the field unset made a deliberate abstention
  # indistinguishable from a missing field.
  set.seed(2026)
  n <- 80
  ipd_df <- data.frame(trt = "A", y = rbinom(n, 1, 0.5),
                       x1 = rnorm(n), x2 = rnorm(n))
  agd_df <- data.frame(trt = "B", n_total = 100, n_events = 40,
                       x1_mean = 0.1, x1_sd = 1, x2_mean = -0.1, x2_sd = 1)
  ipd <- set_ipd(ipd_df, "trt", "y", c("x1", "x2"))
  agd <- set_agd(agd_df, "trt", outcome_n = "n_total", outcome_r = "n_events",
                 cov_means = c("x1_mean", "x2_mean"),
                 cov_sds = c("x1_sd", "x2_sd"))
  cor_mat <- matrix(c(1, 0.3, 0.3, 1), 2, 2,
                    dimnames = list(c("x1", "x2"), c("x1", "x2")))
  dat <- suppressWarnings(add_integration(
    combine_data(ipd, agd), n_int = 32, cor = cor_mat, cor_adjust = "none",
    x1 = distr(qnorm, mean = x1_mean, sd = x1_sd),
    x2 = distr(qnorm, mean = x2_mean, sd = x2_sd)
  ))
  # check_integration() re-integrates, so it needs the distributions too.
  out <- suppressMessages(suppressWarnings(check_integration(
    dat, check_joint = TRUE, verbose = FALSE, cor = cor_mat,
    cor_adjust = "none",
    x1 = distr(qnorm, mean = x1_mean, sd = x1_sd),
    x2 = distr(qnorm, mean = x2_mean, sd = x2_sd)
  )))
  expect_identical(out$verdict$target_correlation, "unavailable")
})


test_that("the comparator prior section is printed once", {
  # `prior_summary()` carried both the shared `.print_beta_prior_block()` call
  # and the hand-rolled block it replaced, guarded on the same condition, so
  # every relaxed fit printed the comparator section twice.
  fit <- structure(
    list(
      model = "relaxed",
      priors = list(
        intercept = prior_normal(0, 10),
        beta = prior_normal(0, 1),
        beta_resolved = list(
          mean = c(0, 0), sd = c(1, 1), dist = 0L, df = 1,
          autoscale = c(FALSE, FALSE), sd_x = c(1, 1),
          covariate_names = c("x1", "x2"), user_specified = TRUE
        ),
        beta_comparator = prior_normal(0, 2),
        beta_comparator_resolved = list(
          mean = c(0, 0), sd = c(2, 2), dist = 0L, df = 1,
          autoscale = c(FALSE, FALSE), sd_x = c(1, 1),
          covariate_names = c("x1", "x2"), user_specified = TRUE
        )
      )
    ),
    class = "mlumr_fit"
  )
  printed <- capture.output(prior_summary(fit))
  expect_length(grep("Comparator regression coefficients", printed), 1L)
})


test_that("a partly-missing tail ESS column is reported, not silently dropped", {
  # `posterior::ess_tail()` returns NA for a chain layout it cannot use, for
  # any non-finite draw, and for a parameter that is constant across chains.
  # Keeping only the finite entries checked the survivors and read as clean.
  base_fit <- function(ess) {
    structure(
      list(
        summary = data.frame(variable = paste0("v", seq_along(ess)),
                             Rhat = 1, n_eff = 1000, ess_tail = ess),
        diagnostics = list(n_divergent = 0, n_max_treedepth = 0,
                           n_chains_requested = 4L, n_chains_returned = 4L),
        sampling_args = list(adapt_delta = 0.95, max_treedepth = 15)
      ),
      class = "mlumr_fit"
    )
  }
  expect_message(check_diagnostics(base_fit(c(1000, NA, 2000))),
                 "unavailable for 1 of 3")
  # All missing: name the outcome, not a cause the backend did not record.
  expect_message(check_diagnostics(base_fit(c(NA_real_, NA_real_))),
                 "Tail ESS is unavailable for this fit")
  expect_silent(check_diagnostics(base_fit(c(1000, 2000))))
  # A low value among the available ones still warns.
  expect_warning(
    suppressMessages(check_diagnostics(base_fit(c(100, NA)))),
    "tail-ESS values < 400"
  )
})
