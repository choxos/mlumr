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
  skip_if_not_installed("survival")
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
  skip_if_not_installed("survival")
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
  skip_if_not_installed("survival")
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

test_that(".validate_survival_controls rejects bad grids / spline controls", {
  skip_if_not(exists(".validate_survival_controls", asNamespace("mlumr")),
             "survival model controls arrive with the model fitting change")
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

test_that(".validate_survival_prediction_times rejects non-finite/non-positive", {
  skip_if_not(exists(".validate_survival_prediction_times", asNamespace("mlumr")),
             "survival model controls arrive with the model fitting change")
  vt <- mlumr:::.validate_survival_prediction_times
  expect_equal(vt(c(1, 2, 3)), c(1, 2, 3))
  expect_error(vt(NA), "finite")
  expect_error(vt(c(1, Inf)), "finite")
  expect_error(vt(-1), "positive|finite")
  expect_error(vt(0), "positive|finite")
  expect_error(vt("a"), "finite|positive")
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

# ---- a status column that as.numeric() would misread ----------------------

test_that("a factor status is refused rather than read as level codes", {
  # as.numeric() on a factor returns level codes. For the single level "0"
  # those codes are all 1, so the 0/1 check saw 1s, passed them, and stored
  # every censored record as an event: an arm in which nobody had the event
  # became an arm in which everybody did.
  d <- data.frame(trt = "A", x = c(1, 2, 3), t = c(5, 6, 7),
                  s = factor(c("0", "0", "0")))
  expect_error(
    set_ipd(d, "trt", covariates = "x", family = "survival",
            time = "t", status = "s"),
    "level codes")
  # Two levels were already caught, by a route that happened to work.
  d2 <- d
  d2$s <- factor(c("0", "1", "0"))
  expect_error(
    set_ipd(d2, "trt", covariates = "x", family = "survival",
            time = "t", status = "s"),
    "level codes")
  # Numeric and logical statuses are untouched.
  d3 <- d
  d3$s <- c(0, 0, 0)
  expect_equal(set_ipd(d3, "trt", covariates = "x", family = "survival",
                       time = "t", status = "s")$n_events, 0L)
  d4 <- d
  d4$s <- c(TRUE, FALSE, TRUE)
  expect_equal(set_ipd(d4, "trt", covariates = "x", family = "survival",
                       time = "t", status = "s")$n_events, 2L)
})

test_that("a factor entry time is refused on the Surv route too", {
  # The column route guarded its times; the Surv route coerced entry_time
  # directly, so factor(c("0", "10")) became delayed entry at 1 and 2 and the
  # likelihood was conditioned on the wrong risk sets. The values are positive
  # and smaller than the event times, so nothing later objected.
  df <- data.frame(trt = "A", x = c(1, 2), entry = factor(c("0", "10")))
  sv <- survival::Surv(c(5, 15), c(1, 1))
  expect_error(
    set_ipd(df, "trt", covariates = "x", family = "survival",
            Surv = sv, entry_time = "entry"),
    "level codes")
  # A numeric entry column is carried through unchanged.
  df$entry <- c(0, 10)
  got <- set_ipd(df, "trt", covariates = "x", family = "survival",
                 Surv = sv, entry_time = "entry")
  expect_equal(got$data$.delay_time, c(0, 10))
})

test_that("n_events counts every failure, not only the exactly observed ones", {
  # Status codes are 0 right-censored, 1 exact, 2 left-censored, 3 interval-
  # censored. Only 0 is a non-event: for 2 and 3 the failure is known to have
  # happened and only its time is not. Counting status 1 alone reported
  # n_events = 0 for a fully interval-censored arm.
  df <- data.frame(trt = "A", x = c(1, 2))
  iv <- survival::Surv(c(2, 4), c(5, 7), type = "interval2")
  expect_equal(set_ipd(df, "trt", covariates = "x", family = "survival",
                       Surv = iv)$n_events, 2L)
  # Right-censored rows are still not events.
  rc <- survival::Surv(c(5, 6), c(1, 0))
  expect_equal(set_ipd(df, "trt", covariates = "x", family = "survival",
                       Surv = rc)$n_events, 1L)
  # And the same holds for the reconstructed comparator arm.
  ad <- data.frame(trt = "B", x_mean = 0.5)[rep(1, 2), ]
  agd <- set_agd_surv(ad, "trt", Surv = iv, cov_means = "x_mean")
  expect_equal(agd$n_events, 2L)
})

# ---- an arm has to be identifiable before it can be grouped ---------------

test_that("a missing arm identifier is refused, not silently matched", {
  # unique() keeps NA and which(NA == NA) selects nothing, so an all-NA arm
  # column passed the single-arm check and then matched no rows: the summary
  # came back .study NA, .trt NA, age_mean NA, from data carrying trt "B" and
  # a mean of 60. The object was structurally valid, so the model would have
  # integrated over NA covariate means.
  d <- data.frame(trt = "B", t = c(4, 6), s = c(1, 0), age_mean = 60, a = NA)
  expect_error(
    set_agd_surv(d, "trt", time = "t", status = "s", cov_means = "age_mean",
                 cov_types = "continuous", arm = "a"),
    "must not contain missing values")
  # A missing study or treatment is refused on the same grounds.
  d2 <- d
  d2$a <- "control"
  d2$st <- c("S1", NA)
  expect_error(
    set_agd_surv(d2, "trt", time = "t", status = "s", cov_means = "age_mean",
                 cov_types = "continuous", study = "st", arm = "a"),
    "must not contain missing values")
})

test_that("one arm label cannot cover two studies or two treatments", {
  # An arm is one reconstructed curve, from one study, on one treatment.
  # Keying only on the label merged rows that shared it: study c("S1", "S2")
  # with arm c("control", "control") passed the single-arm check, was labeled
  # "S1", and kept both studies in the pseudo-IPD.
  d <- data.frame(trt = "B", t = c(4, 6), s = c(1, 0), age_mean = 60,
                  st = c("S1", "S2"), a = c("control", "control"))
  expect_error(
    set_agd_surv(d, "trt", time = "t", status = "s", cov_means = "age_mean",
                 cov_types = "continuous", study = "st", arm = "a"),
    "spans more than one study")
  # Two treatments were already refused, by the whole-frame check.
  d2 <- d
  d2$st <- "S1"
  d2$trt <- c("B", "C")
  expect_error(
    set_agd_surv(d2, "trt", time = "t", status = "s", cov_means = "age_mean",
                 cov_types = "continuous", study = "st", arm = "a"),
    "single treatment")
  # One study, one treatment, one arm still works.
  d3 <- d
  d3$st <- "S1"
  got <- set_agd_surv(d3, "trt", time = "t", status = "s",
                      cov_means = "age_mean", cov_types = "continuous",
                      study = "st", arm = "a")
  expect_equal(got$n_arms, 1L)
  expect_equal(got$data$.study, "S1")
})
