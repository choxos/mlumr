# Fixture builders shared by build-armfast-cases.R and the benchmark workload
# builder. Sourcing this file replaces mlumr's sampler with a capture, so every
# mlumr() call in the sourcing script stops just before Stan would run and
# hands back the complete Stan data list.
#
# Not a test file: source it from a script that defines `script` as its own
# path (see build-armfast-cases.R).

# Always the source tree this script sits in (two levels up), never an
# installed copy, so the fixtures are built by the code under review.
pkgload::load_all(normalizePath(file.path(dirname(script), "..", "..")),
                  quiet = TRUE)
suppressPackageStartupMessages(library(survival))

# Replace the sampler with a capture. mlumr() runs every guard and builds the
# complete Stan data list before this function is reached.
ns <- asNamespace("mlumr")
capture_backend <- function(engine, model_name, stan_data, ...) {
  stop(structure(class = c("armfast_capture", "condition"),
                 list(message = "captured", call = NULL,
                      stan_data = stan_data, model_name = model_name)))
}
unlockBinding(".mlumr_fit_backend", ns)
assign(".mlumr_fit_backend", capture_backend, envir = ns)
lockBinding(".mlumr_fit_backend", ns)

capture_stan_data <- function(dat, ...) {
  res <- tryCatch(
    suppressWarnings(mlumr(dat, ..., chains = 1, iter = 2, warmup = 1,
                           refresh = 0, verbose = FALSE, seed = 2026)),
    armfast_capture = function(cond) cond)
  if (!inherits(res, "armfast_capture")) {
    stop("mlumr() returned without reaching the sampler.")
  }
  res
}

# Simulate one arm from a Weibull proportional-hazards model on up to three
# covariates: a continuous one, a binary one, and a second continuous one.
sim_arm <- function(n, mu, shift, n_cov, shape) {
  beta <- c(0.3, -0.2, 0.15)[seq_len(n_cov)]
  age <- stats::rnorm(n, shift, 1)
  male <- stats::rbinom(n, 1, 0.5)
  score <- stats::rnorm(n, -shift, 1.2)
  X <- cbind(age = age, male = male, score = score)[, seq_len(n_cov), drop = FALSE]
  eta <- mu + drop(X %*% beta)
  t_event <- (-log(stats::runif(n)) / exp(eta))^(1 / shape)
  cens <- stats::rexp(n, 0.08)
  data.frame(time = pmin(t_event, cens), status = as.integer(t_event <= cens), X)
}

# Turn simulated right-censored rows into the requested censoring pattern.
# "mixed" yields events, right-, left-, and interval-censored rows through an
# interval2 Surv object; `entry` adds a delayed-entry time on 40% of rows,
# below every bound the row carries.
outcome_columns <- function(d, censoring, entry) {
  n <- nrow(d)
  t <- d$time
  out <- list(Surv = NULL, time = "time", status = "status", entry_time = NULL)
  lower <- t
  if (censoring == "mixed") {
    kind <- sample(c("event", "right", "left", "interval"), n, replace = TRUE,
                   prob = c(0.55, 0.2, 0.1, 0.15))
    lo <- t
    hi <- t
    hi[kind == "right"] <- NA
    lo[kind == "left"] <- NA
    lo[kind == "interval"] <- 0.8 * t[kind == "interval"]
    hi[kind == "interval"] <- 1.25 * t[kind == "interval"]
    out$Surv <- survival::Surv(lo, hi, type = "interval2")
    out$time <- NULL
    out$status <- NULL
    lower <- ifelse(is.na(lo), hi, lo)
  }
  if (entry) {
    d$entry <- ifelse(stats::runif(n) < 0.4, 0.4 * lower, 0)
    out$entry_time <- "entry"
  }
  list(data = d, args = out)
}

make_data <- function(seed = 2026, n_ipd = 60, n_agd = 80, n_cov = 2,
                      censoring = c("right", "mixed"), entry = FALSE,
                      n_int = 16, shape = 1.2, loghr = -0.4) {
  censoring <- match.arg(censoring)
  set.seed(seed)
  covs <- c("age", "male", "score")[seq_len(n_cov)]

  idx <- sim_arm(n_ipd, mu = 0, shift = 0, n_cov = n_cov, shape = shape)
  idx$trt <- "A"
  o <- outcome_columns(idx, censoring, entry)
  ipd_obj <- do.call(set_ipd, c(
    list(data = o$data, treatment = "trt", covariates = covs,
         family = "survival"), o$args))

  cmp <- sim_arm(n_agd, mu = -loghr, shift = 0.2, n_cov = n_cov, shape = shape)
  agd <- data.frame(trt = "B", time = cmp$time, status = cmp$status,
                    age_mean = mean(cmp$age), age_sd = stats::sd(cmp$age))
  if (n_cov >= 2) agd$male_prop <- mean(cmp$male)
  if (n_cov >= 3) {
    agd$score_mean <- mean(cmp$score)
    agd$score_sd <- stats::sd(cmp$score)
  }
  o <- outcome_columns(agd, censoring, entry)
  agd_obj <- do.call(set_agd_surv, c(
    list(data = o$data, treatment = "trt",
         cov_means = c("age_mean", "male_prop", "score_mean")[seq_len(n_cov)],
         cov_sds = c("age_sd", NA, "score_sd")[seq_len(n_cov)],
         cov_types = c("continuous", "binary", "continuous")[seq_len(n_cov)]),
    o$args))

  dat <- combine_data(ipd_obj, agd_obj)
  ds <- list(age = distr(qnorm, mean = age_mean, sd = age_sd),
             male = distr(qbern, prob = male_mean),
             score = distr(qnorm, mean = score_mean, sd = score_sd))[covs]
  suppressWarnings(do.call(add_integration, c(
    list(data = dat, n_int = n_int, verbose = FALSE), ds)))
}

# Defensive multi-arm fixture: the arm designs must differ, otherwise a
# lookup that always reads arm 1 would be invisible. Only qr = 0 data are
# turned into multi-arm fixtures, so the comparator covariate block of Xq_int
# is the centered design in X_int and the two can be perturbed together. The
# block is located by matching columns, because the relaxed design carries
# separate index and comparator coefficient columns.
make_multi_arm <- function(sd, n_arms = 3L) {
  stopifnot(sd$n_agd_rows == 1L, sd$qr == 0L,
            length(dim(sd$X_int)) == 3L, length(dim(sd$Xq_int)) == 3L)
  X <- sd$X_int
  Xq <- sd$Xq_int
  n_int <- dim(X)[2L]
  n_cov <- dim(X)[3L]
  nB <- dim(Xq)[3L]
  cov_cols <- vapply(seq_len(n_cov), function(k) {
    hit <- which(vapply(seq_len(nB), function(j) {
      isTRUE(all.equal(Xq[1, , j], X[1, , k], check.attributes = FALSE))
    }, logical(1)))
    if (length(hit) != 1L) {
      stop("Covariate ", k, " does not map to exactly one design column.")
    }
    hit
  }, integer(1))
  stopifnot(all(cov_cols > 2L))
  X2 <- array(0, c(n_arms, n_int, n_cov))
  Xq2 <- array(0, c(n_arms, n_int, nB))
  for (a in seq_len(n_arms)) {
    f <- 1 + 0.25 * (a - 1)
    s <- 0.3 * (a - 1)
    X2[a, , ] <- X[1, , ] * f + s
    Xq2[a, , ] <- Xq[1, , ]
    Xq2[a, , cov_cols] <- Xq[1, , cov_cols] * f + s
  }
  sd$X_int <- X2
  sd$Xq_int <- Xq2
  sd$n_agd_rows <- n_arms
  # Interleaved and unequal: arm 1 gets half the rows, arm 3 one sixth.
  sd$agd_arm <- as.integer(rep_len(c(1L, 2L, 3L, 1L, 2L, 1L), sd$n_agd))
  stopifnot(length(unique(sd$agd_arm)) == n_arms)
  sd
}
