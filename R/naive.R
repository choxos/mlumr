#' Naive unadjusted indirect comparison
#'
#' Compute an unadjusted (naive) indirect treatment comparison by comparing
#' crude outcomes from the IPD and AgD without any covariate adjustment.
#' Returns treatment effect on the link scale plus absolute predictions
#' (event probabilities, risk difference, risk ratio) for both populations.
#'
#' For binomial outcomes, event-probability intervals use Wald standard errors
#' and are bounded to `[0, 1]`. For Poisson outcomes, the log-rate contrast
#' uses a 0.5 continuity correction when an observed event count is zero.
#'
#' @param data An `mlumr_data` object from [combine_data()]
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal/poisson: ignored (identity/log always used).
#'   If `NULL`, uses the canonical default.
#' @param conf_level Confidence level for the interval (default 0.95)
#'
#' @return An object of class `mlumr_naive`
#' @export
#'
#' @examples
#' \dontrun{
#' result <- naive(dat)
#' print(result)
#' }
naive <- function(data, link = NULL, conf_level = 0.95) {

  .validate_mlumr_data_object(data)

  family <- data$family %||% "binomial"
  ipd <- data$ipd$data
  agd <- data$agd$data
  z <- .z_from_conf_level(conf_level)

  out <- switch(
    family,
    binomial = .naive_binomial(data, ipd, agd, link, conf_level, z),
    normal = .naive_normal(data, ipd, agd, conf_level, z),
    poisson = .naive_poisson(data, ipd, agd, conf_level, z),
    survival = .naive_survival(data, conf_level, z),
    stop("Unsupported outcome family.", call. = FALSE)
  )

  class(out) <- c("mlumr_naive", "list")
  out
}


#' Naive comparison for binomial outcomes
#' @keywords internal
.naive_binomial <- function(data, ipd, agd, link, conf_level, z) {
  link_info <- check_link("binomial", link)
  link_resolved <- link_info$link

  n_index <- nrow(ipd)
  n_comparator <- sum(agd$.n)
  # The reported rate is the observed proportion. The continuity correction is
  # applied only inside the effect calculation, so a zero-event arm is still
  # reported as zero events rather than as its corrected surrogate.
  p_index <- mean(ipd$.outcome)
  p_comparator <- sum(agd$.r) / n_comparator
  p_index_effect <- bound_probability(p_index, n_index)
  p_comparator_effect <- bound_probability(p_comparator, n_comparator)

  estimate <- link_fun(p_index_effect, link_resolved) -
    link_fun(p_comparator_effect, link_resolved)

  # Same boundary correction as the comparator below: with the raw proportion,
  # p(1 - p) is 0 for an all-events or no-events IPD arm, so the index arm would
  # contribute no uncertainty and its interval would collapse to a point.
  p_index_se <- sqrt(p_index_effect * (1 - p_index_effect) / n_index)
  # Several aggregate rows are strata of one comparator population, not one
  # binomial sample of size sum(n). The variance of their size-weighted average
  # proportion is sum(w_k^2 p_k (1 - p_k) / n_k), propagated to the link scale
  # by the delta method. Both reduce to the previous single-sample formulas when
  # there is one row.
  row_p <- agd$.r / agd$.n
  row_w <- .normalize_weights(agd$.n)
  # Keep the row-level variance, which is what the declared size-weighted
  # stratified mean actually has, but take the boundary correction from the
  # POOLED n. bound_probability() moves a boundary row to
  # min_count / (n + 2 min_count); with each row's own n that made the answer
  # depend on how one aggregate arm had been tabulated, since 0/100 corrects to
  # 0.5/101 while 0/50 + 0/50 corrects to 0.5/51 twice. Correcting against the
  # total leaves interior rows untouched, so heterogeneous strata keep their own
  # p_k(1 - p_k), and equivalent splits of one arm now agree exactly.
  #
  # Collapsing to a single pooled binomial instead would be wrong here: two
  # equal strata at 0.1 and 0.9 have variance 0.09 / N, while the pooled form
  # gives 0.25 / N and inflates every interval it feeds.
  row_p_effect <- bound_probability(row_p, n_comparator)
  var_p_comparator_effect <- sum(
    row_w^2 * row_p_effect * (1 - row_p_effect) / agd$.n
  )
  var_link_index <- binomial_link_variance(
    p_index_effect, n_index, link_resolved
  )
  var_link_comparator <- link_derivative_response(
    p_comparator_effect, link_resolved
  )^2 * var_p_comparator_effect
  se <- sqrt(var_link_index + var_link_comparator)
  # Use the boundary-corrected variance on the absolute scale too. With raw
  # `row_p`, a zero-event or all-event arm has p(1 - p) = 0 and contributes no
  # uncertainty at all: 0/100 gave p_comparator_se = 0, a degenerate [0, 0]
  # interval, and a risk difference whose SE ignored the comparator entirely,
  # although 0/100 alone is consistent with p up to roughly 0.03. The link-scale
  # effect and log_rr_se already use the corrected variance; these did not.
  p_comparator_se <- sqrt(var_p_comparator_effect)
  p_index_ci <- .bounded_wald_interval(p_index, p_index_se, z,
                                       lower = 0, upper = 1)
  p_comparator_ci <- .bounded_wald_interval(p_comparator, p_comparator_se, z,
                                            lower = 0, upper = 1)
  rd <- p_index - p_comparator
  rd_se <- sqrt(p_index_se^2 + p_comparator_se^2)
  log_rr <- log(p_index_effect) - log(p_comparator_effect)
  log_rr_se <- sqrt(
    (1 - p_index_effect) / (n_index * p_index_effect) +
      var_p_comparator_effect / p_comparator_effect^2
  )

  list(
    estimate = estimate,
    link_effect = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "binomial",
    link = link_resolved,
    p_index = p_index,
    p_index_se = p_index_se,
    p_index_lower = p_index_ci$lower,
    p_index_upper = p_index_ci$upper,
    p_comparator = p_comparator,
    p_comparator_se = p_comparator_se,
    p_comparator_lower = p_comparator_ci$lower,
    p_comparator_upper = p_comparator_ci$upper,
    rd = rd,
    rd_se = rd_se,
    rd_lower = rd - z * rd_se,
    rd_upper = rd + z * rd_se,
    log_rr = log_rr,
    log_rr_se = log_rr_se,
    log_rr_lower = log_rr - z * log_rr_se,
    log_rr_upper = log_rr + z * log_rr_se,
    n_index = n_index,
    n_comparator = n_comparator,
    data = data
  )
}


#' Naive comparison for normal outcomes
#' @keywords internal
.naive_normal <- function(data, ipd, agd, conf_level, z) {
  mean_index <- mean(ipd$.outcome)
  n_index <- nrow(ipd)
  var_index <- var(ipd$.outcome) / n_index

  # Sample-size weights, not inverse-variance weights. An inverse-variance
  # average estimates a common mean efficiently, but the estimand here is the
  # comparator population's mean, which is the size-weighted mixture of its
  # strata. This matches what the Stan models and stc() target. Both reduce to
  # the same value for single-row aggregate data.
  row_w <- if (!is.null(agd$.n)) .normalize_weights(agd$.n) else
    rep(1 / nrow(agd), nrow(agd))
  mean_comparator <- sum(row_w * agd$.y)
  var_comparator <- sum(row_w^2 * agd$.se^2)

  estimate <- mean_index - mean_comparator
  se <- sqrt(var_index + var_comparator)
  mean_index_se <- sqrt(var_index)
  mean_comparator_se <- sqrt(var_comparator)

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "normal",
    mean_index = mean_index,
    mean_index_se = mean_index_se,
    mean_index_lower = mean_index - z * mean_index_se,
    mean_index_upper = mean_index + z * mean_index_se,
    mean_comparator = mean_comparator,
    mean_comparator_se = mean_comparator_se,
    mean_comparator_lower = mean_comparator - z * mean_comparator_se,
    mean_comparator_upper = mean_comparator + z * mean_comparator_se,
    n_index = n_index,
    data = data
  )
}


#' Naive comparison for Poisson outcomes
#' @keywords internal
.naive_poisson <- function(data, ipd, agd, conf_level, z) {
  n_index <- nrow(ipd)
  events_index <- sum(ipd$.outcome)
  exposure_index <- sum(ipd$.exposure)
  rate_index <- events_index / exposure_index

  events_comparator <- sum(agd$.r)
  exposure_comparator <- sum(agd$.E)
  rate_comparator <- events_comparator / exposure_comparator

  events_index_adjusted <- max(events_index, 0.5)
  events_comparator_adjusted <- max(events_comparator, 0.5)
  log_rate_index <- log(events_index_adjusted / exposure_index)
  log_rate_comparator <- log(events_comparator_adjusted / exposure_comparator)

  estimate <- log_rate_index - log_rate_comparator
  se <- sqrt(1 / events_index_adjusted + 1 / events_comparator_adjusted)
  # Use the continuity-corrected counts on the absolute scale too. With the raw
  # rate, a zero-event arm has variance rate / exposure = 0 and contributes no
  # uncertainty at all, so its interval collapses to a point and the rate
  # difference below ignores that arm entirely, although 0 events alone is
  # consistent with a clearly positive rate. The log-rate contrast already used
  # the corrected counts; these did not.
  rate_index_se <- sqrt(events_index_adjusted) / exposure_index
  rate_comparator_se <- sqrt(events_comparator_adjusted) / exposure_comparator
  # Rate difference on the natural per-unit-exposure scale, the additive
  # counterpart of the rate ratio. The two arms are independent, so the variance
  # of the difference is the sum of the two rate variances already computed.
  rd <- rate_index - rate_comparator
  rd_se <- sqrt(rate_index_se^2 + rate_comparator_se^2)
  # Bound the absolute-scale interval at 0 around the REPORTED rate, the way the
  # binomial arm does. The previous interval was a log-scale Wald around the
  # continuity-corrected rate 0.5 / exposure, so for a zero-event arm it did not
  # contain the rate it was printed beside: rate 0 with interval
  # [0.0004, 0.101]. Rates are non-negative, so the point sits on the bound.
  rate_index_ci <- .bounded_wald_interval(rate_index, rate_index_se, z,
                                          lower = 0)
  rate_comparator_ci <- .bounded_wald_interval(rate_comparator,
                                               rate_comparator_se, z, lower = 0)

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "poisson",
    rd = rd,
    rd_se = rd_se,
    rd_lower = rd - z * rd_se,
    rd_upper = rd + z * rd_se,
    rate_index = rate_index,
    rate_index_se = rate_index_se,
    rate_index_lower = rate_index_ci$lower,
    rate_index_upper = rate_index_ci$upper,
    rate_comparator = rate_comparator,
    rate_comparator_se = rate_comparator_se,
    rate_comparator_lower = rate_comparator_ci$lower,
    rate_comparator_upper = rate_comparator_ci$upper,
    n_index = n_index,
    events_index = events_index,
    exposure_index = exposure_index,
    events_comparator = events_comparator,
    exposure_comparator = exposure_comparator,
    data = data
  )
}


#' Naive comparison for survival outcomes
#'
#' Unadjusted Cox proportional-hazards log hazard ratio comparing the index IPD
#' against the reconstructed comparator pseudo-IPD, plus Kaplan-Meier median
#' survival per arm. Because this benchmark is a right-censored Cox model, left-
#' and interval-censored records (internal status 2/3) are rejected rather than
#' collapsed to right-censoring.
#' @keywords internal
.naive_survival <- function(data, conf_level, z) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for the naive survival comparison.",
         call. = FALSE)
  }
  ipd <- data$ipd$data
  pseudo <- data$agd$pseudo_ipd

  # The naive benchmark is a right-censored Cox model. Internal `.status` encodes
  # 0 = right-censored, 1 = event, 2 = left-censored, 3 = interval-censored. The
  # Bayesian model (`mlumr()`) handles 2/3, but collapsing them to right-censored
  # non-events (status != 1) would misrepresent the data (a left-censored record
  # is known to have failed by its time; an interval-censored record failed within
  # an interval). Reject them here, matching `stc()` and `geom_km()`, rather than
  # silently produce an invalid Cox estimate.
  if (any(c(ipd$.status, pseudo$.status) %in% c(2L, 3L))) {
    stop("`naive()` fits a right-censored Cox benchmark and does not support ",
         "left- or interval-censored survival data (internal status 2 or 3). ",
         "These are supported by the Bayesian model `mlumr()`, but the naive ",
         "comparison would have to collapse them to right-censored non-events, ",
         "which misrepresents the data. Restrict the naive comparison to ",
         "right-censored / event data (status 0/1, optional delayed entry).",
         call. = FALSE)
  }

  pooled <- data.frame(
    time = c(ipd$.time, pseudo$.time),
    entry = c(ipd$.delay_time, pseudo$.delay_time),
    event = as.integer(c(ipd$.status, pseudo$.status) == 1L),
    arm = factor(c(rep("index", nrow(ipd)), rep("comparator", nrow(pseudo))),
                 levels = c("comparator", "index")),
    stringsAsFactors = FALSE
  )
  has_delay <- any(pooled$entry > 0)
  surv_obj <- if (has_delay) {
    survival::Surv(pooled$entry, pooled$time, pooled$event)
  } else {
    survival::Surv(pooled$time, pooled$event)
  }

  cox <- survival::coxph(surv_obj ~ arm, data = pooled)
  estimate <- unname(stats::coef(cox)[1])
  se <- sqrt(diag(stats::vcov(cox))[1])

  km <- survival::survfit(surv_obj ~ arm, data = pooled)
  km_tab <- summary(km)$table
  med <- if (is.matrix(km_tab)) km_tab[, "median"] else km_tab["median"]
  med_comparator <- unname(med[grep("comparator", names(med))][1])
  med_index <- unname(med[grep("index", names(med))][1])

  list(
    estimate = estimate,
    log_hr = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "survival",
    median_index = med_index,
    median_comparator = med_comparator,
    n_index = nrow(ipd),
    n_comparator = nrow(pseudo),
    events_index = sum(ipd$.status == 1L),
    events_comparator = sum(pseudo$.status == 1L),
    data = data
  )
}
