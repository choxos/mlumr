#' Parse survival outcome columns into mlumr's internal contract
#'
#' Maps either a [survival::Surv()] object or character column names to the
#' internal columns `.time`, `.start_time`, `.delay_time`, `.status` with status
#' codes `0` = right-censored, `1` = event, `2` = left-censored,
#' `3` = interval-censored.
#'
#' @param data Source data frame (used for the character-column route).
#' @param Surv An optional `survival::Surv()` object.
#' @param time,status,entry_time Character column names (character route). Only
#'   right-censoring (status `0`/`1`) and optional delayed entry are supported
#'   via this route; use a `Surv` object for left/interval censoring.
#' @return A data frame with `.time`, `.start_time`, `.delay_time`, `.status`.
#' @keywords internal
.get_surv_data <- function(data, Surv = NULL, time = NULL, status = NULL,
                           entry_time = NULL) {
  if (!is.null(Surv)) {
    if (!inherits(Surv, "Surv")) {
      stop("`Surv` must be a survival::Surv() object", call. = FALSE)
    }
    sm <- unclass(Surv)
    type <- attr(Surv, "type")
    n <- nrow(sm)
    # The Surv object and `data` are combined column-wise downstream (cbind for
    # IPD, data.frame() for AgD pseudo-IPD). Base R would recycle a shorter Surv
    # across the data rows, silently fabricating and misassigning patient
    # outcomes. Require an exact row match so recycling can never happen. (A
    # NULL `data` means the parser was called without a frame to recycle
    # against, e.g. in isolation, so there is nothing to check.)
    if (!is.null(data) && n != nrow(data)) {
      msg <- paste0(
        "`Surv` object has %d row(s) but `data` has %d row(s). They must match ",
        "exactly; a mismatched `Surv` would recycle survival times and statuses ",
        "across patients."
      )
      stop(sprintf(msg, n, nrow(data)), call. = FALSE)
    }
    out <- data.frame(
      .time = rep(NA_real_, n), .start_time = rep(0, n),
      .delay_time = rep(0, n), .status = rep(NA_integer_, n)
    )
    if (type == "right") {
      out$.time <- as.numeric(sm[, "time"])
      out$.status <- ifelse(sm[, "status"] == 1, 1L, 0L)
    } else if (type == "counting") {
      out$.delay_time <- as.numeric(sm[, "start"])
      out$.time <- as.numeric(sm[, "stop"])
      out$.status <- ifelse(sm[, "status"] == 1, 1L, 0L)
    } else if (type == "left") {
      out$.time <- as.numeric(sm[, "time"])
      out$.status <- ifelse(sm[, "status"] == 1, 1L, 2L)
    } else if (type %in% c("interval", "interval2")) {
      st <- as.integer(sm[, "status"])
      t1 <- as.numeric(sm[, "time1"])
      t2 <- as.numeric(sm[, "time2"])
      out$.status <- st
      out$.time <- ifelse(st == 3L, t2, t1)
      out$.start_time <- ifelse(st == 3L, t1, 0)
    } else {
      stop(sprintf("Unsupported Surv type: '%s'", type), call. = FALSE)
    }
    # Delayed entry supplied alongside a Surv object. The counting-type Surv
    # already carries the entry time in its `start` column, so a separate
    # `entry_time` would be a contradictory double specification; reject it.
    # For right/left/interval Surv, combine `entry_time` into `.delay_time`
    # rather than silently discarding it (the previous behavior).
    if (!is.null(entry_time)) {
      if (type == "counting") {
        stop("Delayed entry is already encoded in the counting-type Surv() ",
             "`start` column; do not also pass `entry_time`.", call. = FALSE)
      }
      if (!entry_time %in% names(data)) {
        stop(sprintf("`entry_time` column '%s' not found in `data`", entry_time),
             call. = FALSE)
      }
      out$.delay_time <- as.numeric(data[[entry_time]])
    }
    return(out)
  }

  if (is.null(time) || is.null(status)) {
    stop("Provide either a `Surv` object or both `time` and `status` columns",
         call. = FALSE)
  }
  # as.numeric() on a factor returns its internal level codes, not the printed
  # numbers, and plausible-looking positive times would pass every later check.
  # Status factors are caught by the 0/1 validation below; times are not.
  .reject_factor_time <- function(x, nm) {
    if (is.factor(x)) {
      stop("`", nm, "` is a factor; as.numeric() would use its level codes ",
           "rather than the times shown. Convert explicitly, e.g. ",
           "as.numeric(as.character(", nm, ")).", call. = FALSE)
    }
    as.numeric(x)
  }
  time_vals <- .reject_factor_time(data[[time]], time)
  status_raw <- data[[status]]
  status_num <- if (is.logical(status_raw)) as.integer(status_raw) else as.numeric(status_raw)
  # The column route only supports right-censoring (0 = censored, 1 = event).
  # Reject anything else so 2/3 censoring codes or factor coercions are not
  # silently swept to right-censored; direct users to Surv() for left/interval.
  non_na <- status_num[!is.na(status_num)]
  if (length(non_na) > 0L && !all(non_na %in% c(0, 1))) {
    stop("`status` must be 0/1 (or logical) for the column route. For left- or ",
         "interval-censoring or delayed entry, pass a survival::Surv() object.",
         call. = FALSE)
  }
  delay <- if (!is.null(entry_time)) {
    .reject_factor_time(data[[entry_time]], entry_time)
  } else {
    rep(0, length(time_vals))
  }
  data.frame(
    .time = time_vals,
    .start_time = rep(0, length(time_vals)),
    .delay_time = delay,
    .status = ifelse(status_num == 1, 1L, 0L),
    stringsAsFactors = FALSE
  )
}


#' Validate parsed survival times and status codes
#' @keywords internal
.validate_survival_times <- function(time, start_time, delay_time, status, label) {
  if (any(is.na(time)) || any(is.na(status))) {
    stop(sprintf("%s survival times/status must not contain NA", label),
         call. = FALSE)
  }
  if (any(!is.finite(time)) || any(time <= 0)) {
    stop(sprintf("%s survival times must be finite and strictly positive", label),
         call. = FALSE)
  }
  if (!all(status %in% c(0L, 1L, 2L, 3L))) {
    stop(sprintf("%s status must be coded 0 (right), 1 (event), 2 (left), 3 (interval)",
                 label), call. = FALSE)
  }
  if (any(!is.finite(delay_time)) || any(delay_time < 0)) {
    stop(sprintf("%s delayed-entry times must be finite and non-negative", label),
         call. = FALSE)
  }
  if (any(delay_time >= time)) {
    stop(sprintf("%s delayed-entry times must be earlier than event/censoring times",
                 label), call. = FALSE)
  }
  interval <- status == 3L
  if (any(interval) && any(start_time[interval] >= time[interval])) {
    stop(sprintf("%s interval lower bounds must be earlier than upper bounds",
                 label), call. = FALSE)
  }
  # Left-censored (status 2) with delayed entry is treated as interval-censored on
  # (delay, time]: the Stan likelihood uses the S(delay) - S(time) numerator (then
  # conditions on T > delay), so the combination is fit correctly and needs no
  # recoding or rejection.
  #
  # Interval-censored (status 3) with delayed entry: the interval lower bound must
  # not precede the entry time (a subject known alive at `delay` cannot have an
  # event interval opening before it). Enforce start_time >= delay_time so the
  # Stan log_diff_exp(S(start), S(time)) conditions on the correct lower bound.
  if (any(interval & start_time < delay_time)) {
    msg <- paste0(
      "%s has interval-censored observations (status 3) whose interval lower ",
      "bound precedes the delayed-entry time; the lower bound must be at or ",
      "after the entry time."
    )
    stop(sprintf(msg, label), call. = FALSE)
  }
  invisible(TRUE)
}


#' Set up survival IPD (internal; dispatched from [set_ipd()])
#' @keywords internal
.set_ipd_survival <- function(data, treatment, covariates, study,
                              Surv, time, status, entry_time) {
  .validate_non_empty_data(data, "IPD")
  .validate_required_covariates(covariates, "covariates")

  surv_cols <- if (is.null(Surv)) c(time, status, entry_time) else character(0)
  required_cols <- c(treatment, covariates, surv_cols, study)
  .check_required_columns(data, required_cols)
  .validate_reserved_internal_names(
    c(covariates, treatment, study, time, status, entry_time),
    c(".study", ".trt", ".time", ".start_time", ".delay_time", ".status"),
    "Column name(s)"
  )
  .validate_ipd_covariates(data, covariates)

  surv_df <- .get_surv_data(data, Surv = Surv, time = time, status = status,
                            entry_time = entry_time)

  ipd_data <- data.frame(
    .study = if (!is.null(study)) data[[study]] else "IPD_Study",
    .trt = data[[treatment]],
    stringsAsFactors = FALSE
  )
  ipd_data <- cbind(ipd_data, surv_df)
  for (cov in covariates) ipd_data[[cov]] <- data[[cov]]

  # Drop incomplete rows on all setup columns (treatment, study, the four
  # survival columns, and covariates), mirroring the non-survival path's
  # .drop_missing_rows() so missing treatment / entry rows are dropped with a
  # warning rather than erroring downstream.
  keep <- stats::complete.cases(ipd_data[, c(".study", ".trt", ".time",
                                             ".start_time", ".delay_time",
                                             ".status", covariates)])
  if (!all(keep)) {
    warning(sprintf("%d rows with missing values will be excluded", sum(!keep)),
            call. = FALSE)
    ipd_data <- ipd_data[keep, , drop = FALSE]
  }
  .validate_complete_rows_remain(ipd_data, "IPD")
  .validate_survival_times(ipd_data$.time, ipd_data$.start_time,
                           ipd_data$.delay_time, ipd_data$.status, "IPD")
  .validate_single_treatment(ipd_data, ".trt", "IPD")
  .warn_constant_ipd_covariates(ipd_data, covariates)

  out <- list(
    data = ipd_data,
    n = nrow(ipd_data),
    treatment = unique(ipd_data$.trt),
    covariates = covariates,
    family = "survival",
    type = "ipd",
    n_events = sum(ipd_data$.status == 1L)
  )
  class(out) <- c("mlumr_ipd", "list")
  out
}


#' Set up aggregate survival data (reconstructed pseudo-IPD)
#'
#' Prepare comparator aggregate survival data for an unanchored indirect
#' comparison. The comparator arm is supplied as **reconstructed pseudo-IPD**
#' (event/censoring times digitized from a published Kaplan-Meier curve, e.g.
#' via the Guyot algorithm) together with summary covariate moments
#' (means/SDs). The Stan model integrates the comparator likelihood over the
#' covariate distribution implied by those moments.
#'
#' @param data Data frame of reconstructed pseudo-IPD (one row per
#'   pseudo-individual).
#' @param treatment Column name for the (single) comparator treatment.
#' @param Surv Optional [survival::Surv()] object describing the outcome. Use
#'   this for left/interval censoring or delayed entry.
#' @param time,status,entry_time Character column names as an alternative to
#'   `Surv` (right-censoring with status `0`/`1`, plus optional delayed entry).
#' @param cov_means Character vector of covariate mean/proportion column names
#'   (constant within each arm). Suffixes `_mean`/`_prop` are stripped to match
#'   the IPD covariate names.
#' @param cov_sds Character vector of covariate SD column names (`NA` for binary
#'   covariates). `NULL` treats all covariates as binary.
#' @param cov_types Character vector of `"continuous"`/`"binary"` per covariate.
#'   If `NULL`, inferred from the presence of an SD column.
#' @param study Optional study identifier column.
#' @param arm Optional arm identifier column. Only a single comparator arm is
#'   supported; if supplied, it must have one unique value.
#'   Multi-arm reconstructed survival comparators are rejected until a
#'   weighting estimand is implemented. Defaults to a single arm.
#'
#' @return An object of class `mlumr_agd_surv` (also inheriting `mlumr_agd`).
#' @seealso [set_agd()] for non-survival aggregate data;
#'   [multinma::set_agd_surv()] for the ML-NMR equivalent.
#' @export
#'
#' @examples
#' \dontrun{
#' agd <- set_agd_surv(
#'   data = comparator_km,
#'   treatment = "trt",
#'   time = "time", status = "status",
#'   cov_means = c("age_mean", "male_prop"),
#'   cov_sds = c("age_sd", NA),
#'   cov_types = c("continuous", "binary")
#' )
#' }
set_agd_surv <- function(data, treatment, Surv = NULL,
                         time = NULL, status = NULL, entry_time = NULL,
                         cov_means, cov_sds = NULL, cov_types = NULL,
                         study = NULL, arm = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  .validate_non_empty_data(data, "AgD survival")
  .validate_required_covariates(cov_means, "cov_means")

  spec <- .agd_covariate_spec(cov_means, cov_sds, cov_types)
  cov_sds <- spec$cov_sds
  cov_types <- spec$cov_types
  cov_names <- spec$cov_names

  surv_cols <- if (is.null(Surv)) c(time, status, entry_time) else character(0)
  required_cols <- c(treatment, cov_means, cov_sds[!is.na(cov_sds)],
                     surv_cols, study, arm)
  .check_required_columns(data, required_cols)
  .validate_single_treatment(data, treatment, "AgD survival")
  .validate_reserved_internal_names(
    c(cov_means, cov_sds[!is.na(cov_sds)], treatment, study, arm,
      time, status, entry_time),
    c(".study", ".trt", ".arm", ".time", ".start_time", ".delay_time", ".status"),
    "Column name(s)"
  )
  .validate_agd_covariate_names(cov_means)
  .validate_agd_covariates(data, cov_means, cov_sds)
  .validate_agd_cov_types(cov_types)
  .validate_agd_binary_covariates(data, cov_means, cov_sds, cov_types)

  surv_df <- .get_surv_data(data, Surv = Surv, time = time, status = status,
                            entry_time = entry_time)
  .validate_survival_times(surv_df$.time, surv_df$.start_time,
                           surv_df$.delay_time, surv_df$.status, "AgD")

  arm_vec <- if (!is.null(arm)) {
    as.character(data[[arm]])
  } else if (!is.null(study)) {
    as.character(data[[study]])
  } else {
    rep("AgD_Arm", nrow(data))
  }
  study_vec <- if (!is.null(study)) as.character(data[[study]]) else arm_vec
  trt_vec <- data[[treatment]]

  pseudo_ipd <- data.frame(
    .study = study_vec, .trt = trt_vec, .arm = arm_vec,
    .time = surv_df$.time, .start_time = surv_df$.start_time,
    .delay_time = surv_df$.delay_time, .status = surv_df$.status,
    stringsAsFactors = FALSE
  )

  arms <- unique(arm_vec)
  # Only a single comparator arm is supported. With more than one arm the
  # comparator-population predictions/marginal effects would be an undefined
  # equal-arm mixture (the generated quantities average integration points
  # equally across arms), so reject it rather than return a silent mixture.
  if (length(arms) > 1L) {
    stop("Multi-arm reconstructed survival comparators are not yet supported. ",
         "Supply a single comparator arm (one reconstructed Kaplan-Meier curve).",
         call. = FALSE)
  }
  arm_summary <- .build_arm_summary(data, arms, arm_vec, study_vec, trt_vec,
                                    cov_means, cov_sds, cov_names)
  cov_info <- .agd_cov_info(cov_names, cov_sds, cov_types)

  out <- list(
    data = arm_summary,
    pseudo_ipd = pseudo_ipd,
    treatment = unique(trt_vec),
    covariates = cov_names,
    cov_info = cov_info,
    family = "survival",
    type = "agd",
    n_arms = length(arms),
    n_pseudo = nrow(pseudo_ipd),
    n_events = sum(pseudo_ipd$.status == 1L)
  )
  class(out) <- c("mlumr_agd_surv", "mlumr_agd", "list")
  out
}


#' Build the per-arm covariate-summary table for survival AgD
#' @keywords internal
.build_arm_summary <- function(data, arms, arm_vec, study_vec, trt_vec,
                               cov_means, cov_sds, cov_names) {
  rows <- lapply(arms, function(a) {
    idx <- which(arm_vec == a)
    row <- data.frame(
      .study = study_vec[idx[1]], .trt = trt_vec[idx[1]], .arm = a,
      stringsAsFactors = FALSE
    )
    for (i in seq_along(cov_means)) {
      mean_vals <- data[[cov_means[[i]]]][idx]
      if (length(unique(mean_vals)) > 1L) {
        stop(sprintf("Covariate '%s' must be constant within arm '%s'",
                     cov_means[[i]], a), call. = FALSE)
      }
      row[[paste0(cov_names[[i]], "_mean")]] <- mean_vals[1]
      if (!is.na(cov_sds[[i]])) {
        sd_vals <- data[[cov_sds[[i]]]][idx]
        if (length(unique(sd_vals)) > 1L) {
          stop(sprintf("Covariate SD '%s' must be constant within arm '%s'",
                       cov_sds[[i]], a), call. = FALSE)
        }
        row[[paste0(cov_names[[i]], "_sd")]] <- sd_vals[1]
      }
    }
    row
  })
  do.call(rbind, rows)
}


#' Resolve survival distribution metadata
#'
#' @param distribution One of the survival distribution strings (default
#'   `"weibull"`).
#' @return A list with the distribution name, `kind`
#'   (`"parametric"`/`"flexible"`), integer `dist_code` (1-9 for parametric,
#'   `NA` for flexible), `mspline_degree`, `is_ph` (proportional hazards flag),
#'   `n_aux` (number of shape parameters), and the Stan model `stan_prefix`.
#' @keywords internal
.survival_distribution_info <- function(distribution = NULL) {
  distribution <- distribution %||% "weibull"
  valid <- c("exponential", "weibull", "gompertz", "exponential-aft",
             "weibull-aft", "lognormal", "loglogistic", "gamma", "gengamma",
             "mspline", "pexp")
  if (!is.character(distribution) || length(distribution) != 1L ||
        !(distribution %in% valid)) {
    stop(sprintf("`distribution` must be one of: %s",
                 paste(valid, collapse = ", ")), call. = FALSE)
  }
  flexible <- distribution %in% c("mspline", "pexp")
  dist_code <- switch(distribution,
    exponential = 1L, weibull = 2L, gompertz = 3L,
    "exponential-aft" = 4L, "weibull-aft" = 5L, lognormal = 6L,
    loglogistic = 7L, gamma = 8L, gengamma = 9L,
    NA_integer_
  )
  n_aux <- switch(distribution,
    exponential = 0L, "exponential-aft" = 0L, gengamma = 2L,
    mspline = 0L, pexp = 0L, 1L
  )
  list(
    distribution = distribution,
    kind = if (flexible) "flexible" else "parametric",
    dist_code = dist_code,
    mspline_degree = switch(distribution, mspline = 3L, pexp = 0L, NA_integer_),
    is_ph = distribution %in% c("exponential", "weibull", "gompertz",
                                "mspline", "pexp"),
    n_aux = n_aux,
    stan_prefix = if (flexible) "mlumr_survival_mspline" else "mlumr_survival"
  )
}


#' @method print mlumr_agd_surv
#' @export
print.mlumr_agd_surv <- function(x, ...) {
  cat("Aggregate survival data (reconstructed pseudo-IPD)\n")
  cat("==================================================\n")
  cat("Comparator treatment:", x$treatment, "\n")
  cat(sprintf("  Arms = %d | pseudo-individuals = %d | events = %d\n",
              x$n_arms, x$n_pseudo, x$n_events))
  cat("  Covariates:", paste(x$covariates, collapse = ", "), "\n")
  invisible(x)
}


#' Does the baseline shape actually differ between strata?
#'
#' `n_strata > 1` alone is not the question. `aux_by = ".study"` allocates one
#' auxiliary column per study, but a distribution with no shape parameter has
#' nothing to allocate: for the exponential (PH) and exponential-AFT the hazard
#' is `exp(eta)` and the baseline is carried entirely by the study intercept,
#' which is study-specific in every unanchored fit. Stratifying an exponential
#' therefore changes nothing, and the closed-form contrasts stay exact.
#'
#' This mirrors the Stan gate exactly. `mlumr_survival_{spfa,relaxed}.stan`
#' swaps `delta_*` for the time-varying `loghr_*[1]` under
#' `n_strata > 1 && nonexp && dist <= 3`, where `nonexp` excludes precisely the
#' two exponential codes; the flexible models use `n_strata > 1` unconditionally
#' because their per-stratum spline simplex IS the baseline. Reading the gate
#' from one helper is what stops the R layer attaching a prediction time to a
#' number Stan computed as the `t -> 0` limit.
#'
#' @param object An `mlumr_fit` (survival family).
#' @return `TRUE` when the two studies genuinely have different baseline
#'   shapes, `FALSE` otherwise.
#' @keywords internal
.aux_shapes_differ <- function(object) {
  n_strata <- object$stan_data$n_strata %||% 1L
  if (n_strata <= 1L) return(FALSE)
  info <- object$surv_info
  if (is.null(info)) return(FALSE)
  identical(info$kind, "flexible") || (info$n_aux %||% 0L) > 0L
}


#' Label and evaluation time for the scalar survival treatment effect
#'
#' `delta_*` means three different things depending on the fit, and every
#' user-facing surface that reports it must say which. Deriving that in one
#' place is the point: [marginal_effects()] and [prior_sensitivity()] previously
#' decided independently, which is exactly how one of them came to print a
#' generic "log hazard ratio / log time ratio" for a quantity that was neither.
#'
#' * **Proportional hazards.** A marginal log hazard ratio. Marginal hazard
#'   ratios are non-collapsible, so it always carries a time: the `t -> 0` limit
#'   when the baseline shapes are shared, otherwise the first prediction time.
#' * **AFT, shared shapes, SPFA.** A genuine log time ratio. Both arms share
#'   coefficients, so the covariate term cancels from
#'   `mean(eta_index) - mean(eta_comparator)` and the contrast is constant in
#'   both time and covariates.
#' * **AFT otherwise.** Not a time ratio. If the shapes differ there is no
#'   constant acceleration factor at all. If the model is `relaxed`, the
#'   coefficients differ by treatment, so the covariate term does NOT cancel and
#'   the contrast is the average of covariate-specific log time ratios over the
#'   population (a geometric mean once exponentiated), not one population-level
#'   acceleration factor. Either way the honest name is the location contrast.
#'
#' @param object An `mlumr_fit` (survival family).
#' @param log_scale `TRUE` for the log-scale name (as stored in `delta_*`),
#'   `FALSE` for the natural-scale name [marginal_effects()] reports.
#' @return A list with `label` and `at_time` (`NA` when the measure has no
#'   evaluation time).
#' @keywords internal
.surv_scalar_label <- function(object, log_scale = FALSE) {
  is_ph <- isTRUE(object$surv_info$is_ph)
  differs <- .aux_shapes_differ(object)
  relaxed <- identical(object$model %||% "spfa", "relaxed")
  if (is_ph) {
    list(label = if (log_scale) "LOG_HR" else "HR",
         at_time = if (differs) object$pred_times[1] else 0)
  } else if (differs || relaxed) {
    list(label = if (log_scale) "DELTA_ETA" else "EXP_DELTA_ETA",
         at_time = NA_real_)
  } else {
    list(label = if (log_scale) "LOG_TR" else "TR", at_time = NA_real_)
  }
}
