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
  p_index <- bound_probability(mean(ipd$.outcome), n_index)
  p_comparator <- bound_probability(sum(agd$.r) / n_comparator, n_comparator)

  estimate <- link_fun(p_index, link_resolved) -
    link_fun(p_comparator, link_resolved)
  se <- sqrt(
    binomial_link_variance(p_index, n_index, link_resolved) +
      binomial_link_variance(p_comparator, n_comparator, link_resolved)
  )

  p_index_se <- sqrt(p_index * (1 - p_index) / n_index)
  p_comparator_se <- sqrt(p_comparator * (1 - p_comparator) / n_comparator)
  p_index_ci <- .bounded_wald_interval(p_index, p_index_se, z,
                                       lower = 0, upper = 1)
  p_comparator_ci <- .bounded_wald_interval(p_comparator, p_comparator_se, z,
                                            lower = 0, upper = 1)
  rd <- p_index - p_comparator
  rd_se <- sqrt(p_index_se^2 + p_comparator_se^2)
  log_rr <- log(p_index) - log(p_comparator)
  log_rr_se <- sqrt(
    (1 - p_index) / (n_index * p_index) +
      (1 - p_comparator) / (n_comparator * p_comparator)
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

  iv_weights <- 1 / (agd$.se^2)
  mean_comparator <- weighted.mean(agd$.y, iv_weights)
  var_comparator <- 1 / sum(iv_weights)

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
  rate_index_se <- sqrt(rate_index / exposure_index)
  rate_comparator_se <- sqrt(rate_comparator / exposure_comparator)
  log_rate_index_se <- sqrt(1 / events_index_adjusted)
  log_rate_comparator_se <- sqrt(1 / events_comparator_adjusted)

  list(
    estimate = estimate,
    se = se,
    ci_lower = estimate - z * se,
    ci_upper = estimate + z * se,
    conf_level = conf_level,
    family = "poisson",
    rate_index = rate_index,
    rate_index_se = rate_index_se,
    rate_index_lower = exp(log_rate_index - z * log_rate_index_se),
    rate_index_upper = exp(log_rate_index + z * log_rate_index_se),
    rate_comparator = rate_comparator,
    rate_comparator_se = rate_comparator_se,
    rate_comparator_lower = exp(log_rate_comparator - z * log_rate_comparator_se),
    rate_comparator_upper = exp(log_rate_comparator + z * log_rate_comparator_se),
    n_index = n_index,
    events_index = events_index,
    exposure_index = exposure_index,
    events_comparator = events_comparator,
    exposure_comparator = exposure_comparator,
    data = data
  )
}
