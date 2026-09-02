test_that("set_ipd builds a survival object from time/status columns", {
  set.seed(2026)
  df <- data.frame(
    trt = "A",
    time = rexp(40, 0.1),
    status = rbinom(40, 1, 0.7),
    age = rnorm(40, 60, 10),
    male = rbinom(40, 1, 0.5)
  )
  ipd <- set_ipd(df, treatment = "trt", covariates = c("age", "male"),
                 family = "survival", time = "time", status = "status")
  expect_s3_class(ipd, "mlumr_ipd")
  expect_equal(ipd$family, "survival")
  expect_equal(ipd$n, 40)
  expect_equal(ipd$n_events, sum(df$status))
  expect_true(all(c(".time", ".start_time", ".delay_time", ".status") %in%
                    names(ipd$data)))
})

test_that("set_ipd survival accepts a Surv object", {
  set.seed(2026)
  df <- data.frame(
    trt = "A",
    t = rexp(30, 0.1),
    d = rbinom(30, 1, 0.6),
    x = rnorm(30)
  )
  s <- survival::Surv(df$t, df$d)
  ipd <- set_ipd(df, treatment = "trt", covariates = "x",
                 family = "survival", Surv = s)
  expect_equal(ipd$n_events, sum(df$d))
  expect_equal(ipd$data$.time, df$t)
})

test_that("set_ipd survival requires time/status and rejects outcome misuse", {
  df <- data.frame(trt = "A", time = c(1, 2, 3), status = c(1, 0, 1),
                   x = rnorm(3))
  expect_error(
    set_ipd(df, treatment = "trt", covariates = "x", family = "survival"),
    "Surv|time.*status"
  )
  # non-survival families still require outcome
  expect_error(
    set_ipd(df, treatment = "trt", covariates = "x", family = "normal"),
    "outcome.*required"
  )
})

test_that(".get_surv_data maps censoring types to status codes", {
  # right censoring
  sr <- survival::Surv(c(5, 8), c(1, 0))
  rr <- mlumr:::.get_surv_data(NULL, Surv = sr)
  expect_equal(rr$.status, c(1L, 0L))
  expect_equal(rr$.time, c(5, 8))

  # delayed entry (counting)
  sc <- survival::Surv(c(1, 2), c(6, 9), c(1, 0))
  rc <- mlumr:::.get_surv_data(NULL, Surv = sc)
  expect_equal(rc$.delay_time, c(1, 2))
  expect_equal(rc$.time, c(6, 9))

  # interval censoring
  si <- survival::Surv(c(2, 4), c(5, 7), type = "interval2")
  ri <- mlumr:::.get_surv_data(NULL, Surv = si)
  expect_equal(ri$.status, c(3L, 3L))
  expect_true(all(ri$.start_time < ri$.time))
})

test_that(".get_surv_data handles left-censoring and the 0/1 column route", {
  # left-censoring -> status code 2
  sl_obj <- survival::Surv(c(4, 8), c(1, 0), type = "left")
  sl <- mlumr:::.get_surv_data(NULL, Surv = sl_obj)
  expect_equal(sl$.status, c(1L, 2L))

  # column route: 0/1 status, optional delayed entry
  df <- data.frame(t = c(2, 5, 9), d = c(1, 0, 1), e = c(0, 1, 0))
  r <- mlumr:::.get_surv_data(df, time = "t", status = "d")
  expect_equal(r$.status, c(1L, 0L, 1L))
  expect_equal(r$.time, c(2, 5, 9))
  expect_true(all(r$.start_time == 0) && all(r$.delay_time == 0))
  re <- mlumr:::.get_surv_data(df, time = "t", status = "d", entry_time = "e")
  expect_equal(re$.delay_time, c(0, 1, 0))
  # logical status is accepted and coerced
  rl <- mlumr:::.get_surv_data(data.frame(t = c(3, 6), d = c(TRUE, FALSE)),
                               time = "t", status = "d")
  expect_equal(rl$.status, c(1L, 0L))
})

test_that(".get_surv_data column route rejects non-0/1 status codes", {
  expect_error(
    mlumr:::.get_surv_data(data.frame(t = c(1, 2, 3), d = c(0, 1, 2)),
                           time = "t", status = "d"),
    "0/1|Surv"
  )
})

test_that(".validate_survival_times catches invalid inputs", {
  expect_error(
    mlumr:::.validate_survival_times(c(1, -1), c(0, 0), c(0, 0), c(1L, 1L), "IPD"),
    "strictly positive"
  )
  expect_error(
    mlumr:::.validate_survival_times(c(1, 2), c(0, 0), c(0, 0), c(1L, 5L), "IPD"),
    "status"
  )
  expect_error(
    mlumr:::.validate_survival_times(c(1, 2), c(0, 0), c(2, 0), c(1L, 1L), "IPD"),
    "earlier than"
  )
})

test_that("survival IPD drops rows with missing values (with a warning)", {
  # Missingness across treatment, time, status, and a covariate; three complete
  # rows (with varying age) survive, so only the missing-row warning is raised.
  df <- data.frame(
    trt = c("A", "A", "A", "A", NA, "A", "A"),
    time = c(2, 5, 7, NA, 8, 9, 3),
    status = c(1, 0, 1, 1, 1, 0, NA),
    age = c(60, 55, 62, 64, 50, NA, 70)
  )
  expect_warning(
    ipd <- set_ipd(df, treatment = "trt", covariates = "age",
                   family = "survival", time = "time", status = "status"),
    "missing values will be excluded"
  )
  expect_equal(ipd$n, 3L)
})

test_that("set_agd_surv rejects multi-arm comparator evidence", {
  set.seed(2026)
  agd <- data.frame(
    arm = rep(c("X", "Y"), each = 10),
    trt = "B",
    time = rexp(20, 0.1),
    status = rbinom(20, 1, 0.6),
    age_mean = 60
  )
  expect_error(
    set_agd_surv(agd, treatment = "trt", arm = "arm", time = "time",
                 status = "status", cov_means = "age_mean",
                 cov_types = "continuous", cov_sds = "age_mean"),
    "Multi-arm|single comparator arm"
  )
})

test_that("set_agd_surv builds pseudo-IPD and per-arm covariate summary", {
  set.seed(2026)
  agd <- data.frame(
    trt = "B",
    time = rexp(50, 0.12),
    status = rbinom(50, 1, 0.6),
    age_mean = 62, age_sd = 9,
    male_prop = 0.45
  )
  obj <- set_agd_surv(agd, treatment = "trt", time = "time", status = "status",
                      cov_means = c("age_mean", "male_prop"),
                      cov_sds = c("age_sd", NA),
                      cov_types = c("continuous", "binary"))
  expect_s3_class(obj, "mlumr_agd_surv")
  expect_s3_class(obj, "mlumr_agd")
  expect_equal(obj$family, "survival")
  expect_equal(obj$n_arms, 1)
  expect_equal(obj$n_pseudo, 50)
  expect_equal(obj$covariates, c("age", "male"))
  # arm summary (stored as $data so add_integration consumes it directly)
  expect_equal(nrow(obj$data), 1)
  expect_true(all(c("age_mean", "age_sd", "male_mean") %in% names(obj$data)))
})

test_that("set_agd_surv rejects covariates that vary within an arm", {
  agd <- data.frame(
    trt = "B", time = c(1, 2, 3), status = c(1, 1, 0),
    age_mean = c(60, 61, 62)
  )
  expect_error(
    set_agd_surv(agd, treatment = "trt", time = "time", status = "status",
                 cov_means = "age_mean", cov_types = "continuous",
                 cov_sds = NA),
    "constant within arm"
  )
})

test_that("combine_data and add_integration work for survival", {
  set.seed(2026)
  ipd <- data.frame(trt = "A", time = rexp(40, 0.1), status = rbinom(40, 1, 0.7),
                    age = rnorm(40, 60, 10), male = rbinom(40, 1, 0.5))
  ipd_obj <- set_ipd(ipd, treatment = "trt", covariates = c("age", "male"),
                     family = "survival", time = "time", status = "status")
  agd <- data.frame(trt = "B", time = rexp(60, 0.12), status = rbinom(60, 1, 0.6),
                    age_mean = 62, age_sd = 9, male_prop = 0.45)
  agd_obj <- set_agd_surv(agd, treatment = "trt", time = "time", status = "status",
                          cov_means = c("age_mean", "male_prop"),
                          cov_sds = c("age_sd", NA),
                          cov_types = c("continuous", "binary"))
  dat <- combine_data(ipd_obj, agd_obj)
  expect_s3_class(dat, "mlumr_data")
  expect_equal(dat$family, "survival")

  dat <- suppressWarnings(add_integration(
    dat, n_int = 16,
    age = distr(qnorm, mean = age_mean, sd = age_sd),
    male = distr(qbern, prob = male_mean),
    verbose = FALSE
  ))
  expect_true(dat$has_integration)
  # array is n_arms x n_int x n_cov
  expect_equal(dim(dat$integration_points), c(1L, 16L, 2L))
  expect_output(print(dat), "Time-to-event")
})

test_that("combine_data rejects family mismatch with survival", {
  set.seed(2026)
  ipd <- data.frame(trt = "A", outcome = rbinom(20, 1, 0.5), age = rnorm(20))
  ipd_bin <- set_ipd(ipd, treatment = "trt", outcome = "outcome",
                     covariates = "age")
  agd <- data.frame(trt = "B", time = rexp(20, 0.1), status = rbinom(20, 1, 0.6),
                    age_mean = 0)
  agd_surv <- set_agd_surv(agd, treatment = "trt", time = "time",
                           status = "status", cov_means = "age_mean",
                           cov_types = "continuous", cov_sds = "age_mean")
  expect_error(combine_data(ipd_bin, agd_surv), "Family mismatch")
})

test_that("survival setup rejects reserved covariate names", {
  df <- data.frame(trt = "A", time = c(1, 2), status = c(1, 0), .time = c(0, 0))
  expect_error(
    set_ipd(df, treatment = "trt", covariates = ".time",
            family = "survival", time = "time", status = "status"),
    "reserved"
  )
})

test_that(".validate_survival_times handles delayed-entry censoring correctly", {
  vst <- mlumr:::.validate_survival_times
  expect_true(vst(time = c(5, 6), start_time = c(0, 0), delay_time = c(0, 0),
                  status = c(0L, 1L), label = "IPD"))
  # Left-censored (status 2) with delayed entry is supported (treated as
  # interval-censored on (delay, time]); it must not error.
  expect_true(vst(time = c(5, 6), start_time = c(0, 0), delay_time = c(0, 2),
                  status = c(1L, 2L), label = "IPD"))
  # An interval lower bound that precedes the entry time is rejected.
  expect_error(
    vst(time = 10, start_time = 2, delay_time = 4, status = 3L, label = "IPD"),
    "interval-censored"
  )
})

# ---- identifiers that as.numeric() or NA would quietly rewrite ------------

test_that("a factor status is refused rather than read as level codes", {
  # as.numeric() on a factor returns level codes. For the single level "0"
  # those codes are all 1, so the 0/1 check saw 1s, passed them, and stored
  # every censored record as an event: an arm in which nobody had the event
  # became an arm in which everybody did.
  mk <- function(s) {
    d <- data.frame(trt = "A", x = c(1, 2, 3), t = c(5, 6, 7), s = s)
    set_ipd(d, "trt", covariates = "x", family = "survival",
            time = "t", status = "s")
  }
  expect_error(mk(factor(c("0", "0", "0"))), "level codes")
  # Two levels were already caught, by a route that happened to work.
  expect_error(mk(factor(c("0", "1", "0"))), "level codes")
  # Numeric and logical statuses are untouched.
  expect_equal(mk(c(0, 0, 0))$n_events, 0L)
  expect_equal(mk(c(TRUE, FALSE, TRUE))$n_events, 2L)
})

test_that("a factor entry time is refused on the Surv route too", {
  # The column route guarded its times; the Surv route coerced entry_time
  # directly, so factor(c("0", "10")) became delayed entry at 1 and 2 and the
  # likelihood was conditioned on the wrong risk sets. Those values are
  # positive and below the event times, so nothing later objected.
  mk <- function(entry) {
    d <- data.frame(trt = "A", x = c(1, 2), entry = entry)
    set_ipd(d, "trt", covariates = "x", family = "survival",
            Surv = survival::Surv(c(5, 15), c(1, 1)), entry_time = "entry")
  }
  expect_error(mk(factor(c("0", "10"))), "level codes")
  expect_equal(mk(c(0, 10))$data$.delay_time, c(0, 10))
})

test_that("n_events counts every failure, not only the exactly observed ones", {
  # Status codes are 0 right-censored, 1 exact, 2 left-censored, 3 interval-
  # censored. Only 0 is a non-event: for 2 and 3 the failure is known to have
  # happened and only its time is not. Counting status 1 alone reported
  # n_events = 0 for a fully interval-censored arm.
  df <- data.frame(trt = "A", x = c(1, 2))
  mk <- function(sv) {
    set_ipd(df, "trt", covariates = "x", family = "survival", Surv = sv)
  }
  interval <- survival::Surv(c(2, 4), c(5, 7), type = "interval2")
  expect_equal(mk(interval)$n_events, 2L)
  # Right-censored rows are still not events.
  expect_equal(mk(survival::Surv(c(5, 6), c(1, 0)))$n_events, 1L)
  # And the same holds for the reconstructed comparator arm.
  ad <- data.frame(trt = "B", x_mean = c(0.5, 0.5))
  agd <- set_agd_surv(ad, "trt", Surv = interval, cov_means = "x_mean")
  expect_equal(agd$n_events, 2L)
})

# ---- an arm has to be identifiable before it can be grouped ---------------

test_that("a missing arm identifier is refused, not silently matched", {
  # unique() keeps NA and which(NA == NA) selects nothing, so an all-NA arm
  # column passed the single-arm check and then matched no rows: the summary
  # came back .study NA, .trt NA, age_mean NA, from data carrying trt "B" and
  # a mean of 60. That object is structurally valid, so the model would have
  # gone on to integrate over NA covariate means.
  mk <- function(d, ...) {
    set_agd_surv(d, "trt", time = "t", status = "s", cov_means = "age_mean",
                 cov_types = "continuous", ...)
  }
  d <- data.frame(trt = "B", t = c(4, 6), s = c(1, 0), age_mean = 60, a = NA)
  expect_error(mk(d, arm = "a"), "must not contain missing values")
  d2 <- d
  d2$a <- "control"
  d2$st <- c("S1", NA)
  expect_error(mk(d2, study = "st", arm = "a"), "must not contain missing")
})

test_that("one arm label cannot cover two studies", {
  # An arm is one reconstructed curve from one study. Keying only on the label
  # merged rows that shared it: study c("S1", "S2") with arm
  # c("control", "control") passed the single-arm check, was labeled "S1", and
  # kept both studies in the pseudo-IPD.
  mk <- function(d) {
    set_agd_surv(d, "trt", time = "t", status = "s", cov_means = "age_mean",
                 cov_types = "continuous", study = "st", arm = "a")
  }
  d <- data.frame(trt = "B", t = c(4, 6), s = c(1, 0), age_mean = 60,
                  st = c("S1", "S2"), a = c("control", "control"))
  expect_error(mk(d), "spans more than one study")
  # Two treatments were already refused, by the whole-frame check.
  d2 <- d
  d2$st <- "S1"
  d2$trt <- c("B", "C")
  expect_error(mk(d2), "single treatment")
  # One study, one treatment, one arm still works.
  d3 <- d
  d3$st <- "S1"
  got <- mk(d3)
  expect_equal(got$n_arms, 1L)
  expect_equal(got$data$.study, "S1")
})

test_that("survival data prints as survival data", {
  # Survival was given a family label and then fell through to the Poisson
  # branches, which read fields it does not have. sprintf("%d", NULL) is
  # character(0), so the IPD event line printed nothing; sum(NULL) is 0, so the
  # comparator reported "Total exposure = 0.0" for a quantity a reconstructed
  # curve does not have. The existing test matched only "Time-to-event", which
  # the broken output also satisfied.
  ipd_df <- data.frame(trt = "A", x = c(1, 2, 3), t = c(5, 6, 7),
                       s = c(1, 1, 0))
  ipd <- set_ipd(ipd_df, "trt", covariates = "x", family = "survival",
                 time = "t", status = "s")
  agd_df <- data.frame(trt = "B", t = c(4, 6, 8, 9), s = c(1, 1, 1, 0),
                       x_mean = 0.5)
  agd <- set_agd_surv(agd_df, "trt", time = "t", status = "s",
                      cov_means = "x_mean")
  out <- paste(capture.output(print(combine_data(ipd, agd))), collapse = "\n")

  expect_match(out, "Time-to-event")
  expect_match(out, "Events = 2 \\(66\\.7%\\), censored = 1")
  expect_match(out, "Reconstructed pseudo-IPD: 4 row")
  expect_match(out, "Events = 3 \\(75\\.0%\\), censored = 1")
  expect_false(grepl("exposure", out, ignore.case = TRUE))
})

test_that(".validate_survival_controls rejects bad grids and spline controls", {
  vsc <- mlumr:::.validate_survival_controls
  # valid inputs pass silently
  expect_true(vsc(c(1, 2, 3), 5, 3L, 7L))
  expect_true(vsc(NULL, NULL, NULL, 5L))
  # pred_times must be finite positive
  expect_error(vsc(c(-1, 2), NULL, NULL, 5L), "pred_times")
  expect_error(vsc(c(1, Inf), NULL, NULL, 5L), "pred_times")
  # rmst_horizon must be a single finite positive number
  expect_error(vsc(NULL, c(1, 2), NULL, 5L), "rmst_horizon")
  expect_error(vsc(NULL, -3, NULL, 5L), "rmst_horizon")
  # mspline_degree must be a non-negative integer
  expect_error(vsc(NULL, NULL, 2.5, 5L), "mspline_degree")
  # n_knots must be a non-negative integer
  expect_error(vsc(NULL, NULL, NULL, -1), "n_knots")
})
