#' Naive unadjusted indirect comparison
#'
#' Compute an unadjusted (naive) indirect treatment comparison by comparing
#' crude outcomes from the IPD and AgD without any covariate adjustment.
#' The index outcome remains marginal over the index-study population and the
#' comparator outcome remains marginal over the comparator population. The
#' contrast therefore has no single standardized target population. It returns
#' the link-scale contrast plus the two observed marginal outcomes and available
#' natural-scale contrasts.
#'
#' The two arms are observed directly, so their intervals are exact: the
#' Clopper-Pearson interval for a binomial proportion and the Garwood
#' interval for a Poisson rate, the ones `binom.test()` and `poisson.test()`
#' report, whose coverage is at least the nominal level for every true value
#' (they are conservative rather than shortest). A comparator built from
#' several aggregate rows is pooled for its interval: independent Poisson
#' counts add, so the pooled rate interval is exact for the exposure-weighted
#' mean, and the pooled binomial interval is conservative for the
#' size-weighted mean of its strata. The arm standard errors are
#' still reported, and they and the contrasts use the boundary pseudo-count
#' `(r + 0.5) / (n + 1)` when an arm has zero or all events, or 0.5 events
#' when a Poisson count is zero; the reported crude proportions and rates
#' are unchanged. The link-scale contrast and the log risk ratio get Wald
#' intervals around those corrected quantities; the risk difference gets one
#' on the natural scale, centered on the raw difference of proportions with
#' the corrected standard errors. All three are approximate, and their
#' coverage is not bounded by the nominal level.
#'
#' Twelve configurations are pinned in the package's tests, each by
#' enumerating every pair of counts at 100 observations per arm at a nominal
#' 95%. Their recorded coverage runs from 0.853 to 0.9999. Those are the
#' values at those true probabilities and they bound nothing else: the
#' twelve do not cover other probabilities, other sample sizes, other links,
#' stratified AgD or a transported [stc()] contrast. Coverage is worst near
#' opposite boundaries, where a true risk difference of 0.966 (0.986 against
#' 0.020) is covered 85.3% of the time and a true log risk ratio at 0.986
#' against 0.957 is covered 92.1%. The naive comparison is a crude
#' benchmark, and a contrast between arms at opposite boundaries is not one
#' of the things it does well.
#'
#' Scale note: `$estimate` (and the binomial `$log_rr`) is on the link / log
#' scale, where the null is 0. To compare against the natural-scale risk ratio
#' or rate ratio from [marginal_effects()] (where the null is 1), exponentiate
#' it (e.g. `exp(result$estimate)`).
#'
#' @param data An `mlumr_data` object from [combine_data()]
#' @param link Link function. For binomial: `"logit"` (default), `"probit"`,
#'   or `"cloglog"`. For normal/poisson: ignored (identity/log always used).
#'   For survival: ignored (an unadjusted Cox proportional-hazards log hazard
#'   ratio is returned). The naive Cox benchmark accepts only right-censored /
#'   event data (optionally with delayed entry); left- or interval-censored data
#'   (which the Bayesian [mlumr()] model supports) are rejected. If `NULL`, uses
#'   the canonical default.
#' @param conf_level Confidence level for the interval (default 0.95)
#'
#' @section Normal-family weighting:
#' Across multiple AgD rows the normal-family comparator mean here is
#' population weighted using `outcome_n`, matching the Bayesian ML-UMR
#' comparator-population estimand. `outcome_n` is required when there is more
#' than one row; a single row has weight one. The comparator-mean variance
#' combines independent, mutually exclusive strata as `sum(w^2 * se^2)` using
#' normalized population weights. The same weighting applies to [stc()].
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
  # The arms are observed directly, so their intervals are exact. A bounded
  # Wald interval around a boundary-corrected SE ended at 0.0138 for 0 of
  # 100 and covered a true probability of 0.014 only 75.5% of the time. The
  # size-weighted mean of the comparator strata is the pooled proportion,
  # and the exact interval on the pooled count is conservative for it.
  p_index_ci <- .clopper_pearson_interval(sum(ipd$.outcome), n_index,
                                          conf_level)
  p_comparator_ci <- .clopper_pearson_interval(sum(agd$.r), n_comparator,
                                               conf_level)
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
  if (n_index < 2L) {
    stop("The naive normal benchmark needs at least two IPD observations to ",
         "estimate the index-mean variance; ", n_index, " supplied.",
         call. = FALSE)
  }
  var_index <- var(ipd$.outcome) / n_index

  if (nrow(agd) > 1L && is.null(agd$.n)) {
    stop("`outcome_n` is required for multiple normal AgD rows.", call. = FALSE)
  }
  agd_weights <- agd$.n %||% 1
  w_norm <- .normalize_weights(agd_weights)
  mean_comparator <- sum(w_norm * agd$.y)
  var_comparator <- sum(w_norm^2 * agd$.se^2)

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
  # The arms are observed directly, so their intervals are exact (Garwood).
  # A bounded Wald interval around the corrected SE ended at 0.0139 for 0
  # events over an exposure of 100 and covered a true rate of 0.02 about
  # 86% of the time; the exact interval's lower bound is 0 at 0 events, so
  # the rate it is printed beside is inside it.
  rate_index_ci <- .garwood_interval(events_index, exposure_index, conf_level)
  rate_comparator_ci <- .garwood_interval(events_comparator,
                                          exposure_comparator, conf_level)

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

  # The partial likelihood needs events in both arms to identify the treatment
  # coefficient. With none at all, or with every event in one arm, coxph()
  # returns NA or a coefficient running off to infinity and warns rather than
  # failing, and the result was packaged as an ordinary hazard ratio with a
  # confidence interval. Refuse instead: the comparison is not estimable, and a
  # number that looks like a log hazard ratio is worse than no number.
  events_by_arm <- tapply(pooled$event, pooled$arm, sum)
  events_by_arm[is.na(events_by_arm)] <- 0L
  if (sum(pooled$event) == 0L || any(events_by_arm == 0L)) {
    stop("The naive Cox comparison needs at least one event in each arm: ",
         sprintf("observed %d in the comparator arm and %d in the index arm. ",
                 as.integer(events_by_arm[["comparator"]]),
                 as.integer(events_by_arm[["index"]])),
         "With an event-free arm the treatment coefficient is not identified ",
         "by the partial likelihood.", call. = FALSE)
  }

  # Events in both arms is necessary for an interior maximum and not
  # sufficient. The partial likelihood can be monotone in the treatment
  # coefficient, in which case it has no maximum but coxph() stops on its
  # convergence criterion and returns FINITE numbers: six uncensored subjects
  # with the three index events all before the three comparator ones give a
  # coefficient of 21.9 with a standard error of 24795, which clears the check
  # below and was packaged as an ordinary hazard ratio with an interval of
  # roughly [-48576, 48620].
  #
  # What makes it monotone is the RISK SETS, not the order of the event times.
  # Ordered events are enough only when nobody from the earlier arm is still at
  # risk when the later arm fails, which is why the six-subject example above
  # is uncensored. Add censoring and the same ordering is perfectly estimable:
  # A failing at 1 and censored at 4, B failing at 2 and censored at 3 puts
  # every A event before every B event, yet an A subject is still at risk at
  # B's event, the partial likelihood is r/(2r + 2) * 1/(r + 2), and its
  # maximum is at r = sqrt(2), a log hazard ratio of 0.347. So the arrangement
  # is not what is tested here: coxph() reports the monotone case in a warning,
  # which nothing read, and that warning is what is read.
  cox_warnings <- character(0)
  cox <- withCallingHandlers(
    survival::coxph(surv_obj ~ arm, data = pooled),
    warning = function(w) {
      cox_warnings <<- c(cox_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  monotone <- grepl("may be infinite", cox_warnings, fixed = TRUE)
  # Failing to converge is not an ordinary warning either. coxph() documents
  # several termination conditions and says its detection of an infinite
  # coefficient is not always successful, so the absence of the message above
  # is not a certificate that a finite maximum exists. Reissuing "Ran out of
  # iterations and did not converge" and then returning the coefficient and a
  # Wald interval built from it presents the state the iteration stopped in as
  # an estimate.
  unconverged <- !monotone &
    (grepl("did not converge", cox_warnings, fixed = TRUE) |
       grepl("Ran out of iterations", cox_warnings, fixed = TRUE))
  # Anything coxph() reported that is neither is still the caller's to see.
  for (w in cox_warnings[!monotone & !unconverged]) warning(w, call. = FALSE)

  estimate <- unname(stats::coef(cox)[1])
  se <- sqrt(diag(stats::vcov(cox))[1])
  if (any(monotone)) {
    stop("The naive Cox comparison has no interior maximum: the partial ",
         "likelihood is monotone in the treatment coefficient, which happens ",
         "when no risk set ever compares the two arms in both directions. ",
         "Event times ordered by arm are the usual way to reach that, and ",
         "only when censoring leaves nobody from the earlier arm at risk when ",
         "the later one fails. coxph() stopped on its convergence criterion ",
         "and returned a ",
         "coefficient of ", format(estimate, digits = 4), " with a standard ",
         "error of ", format(se, digits = 4), "; both describe where the ",
         "iteration stopped rather than the data, and the interval built from ",
         "them spans essentially the whole real line. Events in both arms are ",
         "necessary for this comparison and are not sufficient. Use mlumr(), ",
         "whose prior makes the posterior proper.", call. = FALSE)
  }
  if (any(unconverged)) {
    stop("The naive Cox comparison did not converge: coxph() reported ",
         paste(sQuote(cox_warnings[unconverged]), collapse = "; "),
         ". It returned a coefficient of ", format(estimate, digits = 4),
         " with a standard error of ", format(se, digits = 4),
         ", but those describe the state the iteration stopped in, so the ",
         "Wald interval built from them does not have its nominal coverage. ",
         "coxph() also documents that its own detection of an infinite ",
         "coefficient is not always successful, so a fit that stops this way ",
         "is not evidence that a finite maximum exists. Use mlumr(), whose ",
         "prior makes the posterior proper.", call. = FALSE)
  }
  if (!is.finite(estimate) || !is.finite(se) || se <= 0) {
    stop("The naive Cox comparison did not produce an estimable treatment ",
         "effect (coefficient ", format(estimate), ", standard error ",
         format(se), "). This usually means the arms are separated in time, ",
         "so the partial likelihood has no interior maximum.", call. = FALSE)
  }

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
