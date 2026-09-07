#' Convert a time column, refusing a factor
#'
#' `as.numeric()` on a factor returns its internal level codes, not the printed
#' numbers, and the small positive integers that produces look like plausible
#' times: they pass every later check. The column route guarded its own times
#' this way; the `Surv` route coerced `entry_time` directly, so a factor entry
#' column became left-truncation times of 1, 2, 3 and the likelihood was
#' conditioned on the wrong risk sets.
#'
#' @param x The column.
#' @param nm Its name, for the message.
#' @return A numeric vector.
#' @keywords internal
.reject_factor_time <- function(x, nm) {
  if (is.factor(x)) {
    stop("`", nm, "` is a factor; as.numeric() would use its level codes ",
         "rather than the times shown. Convert explicitly, e.g. ",
         "as.numeric(as.character(", nm, ")).", call. = FALSE)
  }
  as.numeric(x)
}


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
    # survival::is.Surv() rather than inherits(). The package declares survival
    # in Imports, and asking its own predicate is what makes that declaration
    # true: with only an inherits() check nothing in the package ever reached
    # into the namespace, which R CMD check reports as an unused import.
    if (!survival::is.Surv(Surv)) {
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
      out$.delay_time <- .reject_factor_time(data[[entry_time]], entry_time)
    }
    return(out)
  }

  if (is.null(time) || is.null(status)) {
    stop("Provide either a `Surv` object or both `time` and `status` columns",
         call. = FALSE)
  }
  time_vals <- .reject_factor_time(data[[time]], time)
  status_raw <- data[[status]]
  # A factor status must be refused outright, not left to the 0/1 check below.
  # as.numeric() returns level codes, and for a factor with the single level
  # "0" those codes are all 1: the check sees 1s, passes them, and every
  # censored record is stored as an event. An arm in which nobody had the event
  # became an arm in which everybody did, with no warning anywhere.
  if (is.factor(status_raw)) {
    stop("`", status, "` is a factor; as.numeric() would use its level codes ",
         "rather than the values shown, and a single-level factor would map ",
         "every row to 1. Convert explicitly, e.g. as.numeric(as.character(",
         status, ")).", call. = FALSE)
  }
  status_num <- if (is.logical(status_raw)) {
    as.integer(status_raw)
  } else {
    as.numeric(status_raw)
  }
  # The column route only supports right-censoring (0 = censored, 1 = event).
  # Reject anything else so 2/3 censoring codes are not silently swept to
  # right-censored; direct users to Surv() for left/interval.
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
    c(".study", ".trt", ".time", ".start_time", ".delay_time", ".status",
      ".source_key"),
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
  ipd_data$.source_key <- .source_row_keys(data)

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
    # Statuses 2 and 3 are left- and interval-censored failures: the event is
    # known to have occurred, only its time is not. Counting status 1 alone
    # reported n_events = 0 for a fully interval-censored arm.
    n_events = sum(ipd_data$.status != 0L)
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
#' @section Reconstruction uncertainty is not propagated:
#' The pseudo-individual records enter the likelihood as if they were observed
#' data. The posterior therefore carries outcome-model and parameter
#' uncertainty *conditional on this one reconstruction*, and none of the
#' uncertainty in the reconstruction itself: reading points off the published
#' figure, rounding in the numbers at risk, the choice of reconstruction
#' algorithm, and the fact that many pseudo-IPD sets are compatible with the
#' same published curve. Credible intervals from a survival fit are for that
#' reason narrower than the evidence supports, most visibly for flexible
#' baselines, late-tail RMST and medians, and weakly identified relaxed
#' comparator coefficients.
#'
#' There is no automatic correction for this. Treat the reconstruction as an
#' analysis choice and vary it: digitize the curve more than once, or perturb
#' the digitized points and the numbers at risk within their reading error,
#' refit on each resulting pseudo-IPD set, and report the spread across refits
#' alongside the within-fit interval. If that spread is comparable to the
#' credible interval, the interval is describing the reconstruction as much as
#' the data.
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
#' @details
#' **Which population the covariate moments must describe under delayed
#' entry.** For individual data the covariates are known, so dividing each
#' person's contribution by their own survival to entry is unambiguous. Here
#' the covariates are not observed: they are integrated out against the
#' distribution [add_integration()] builds, and the order of the two
#' operations matters. This model conditions first and averages second, so each
#' integration point contributes its own delayed-entry likelihood and the row
#' likelihood is their average. That is the right quantity when the
#' distribution integrated over describes the population **as observed at
#' entry**, meaning those who survived to their entry time and are therefore
#' in the risk set. The whole distribution has to, not only `cov_means` and
#' `cov_sds`: the delayed-entry contribution is nonlinear in the covariates,
#' so the marginal shape each `distr()` assumes and the dependence its `cor`
#' imposes change the average as surely as the moments do, and two populations
#' can share every moment while differing in either.
#'
#' It is not the right quantity when the moments describe a baseline,
#' pre-selection population, because surviving to entry is itself selective:
#' whichever covariate values carry lower hazard are over-represented at entry
#' relative to baseline. Averaging first and conditioning second is the form
#' that matches baseline moments, and the two differ in general, by more when
#' entry times are late or the covariate effects are strong.
#'
#' Varying entry times need one assumption more, because then there is no
#' single population observed at entry: each pseudo-individual should be
#' integrated against the covariate distribution among those observed at ITS
#' entry time, the people who entered then and had survived to it. That
#' distribution can differ from one entry time to the next for two reasons:
#' later entrants are a more selected group than earlier ones, and who enters
#' when may itself be related to the covariates, since cohorts enrolled at
#' different times can differ even when survival does not depend on the
#' covariates at all. The model has one covariate distribution per arm and
#' reuses it for every pseudo-individual. Pooled moments over everyone
#' enrolled describe no single risk set: they mix the populations observed at
#' every entry time, so in general they do not meet the contract above even
#' though everyone in them was observed at entry, and using them can give the
#' wrong likelihood. They are right only under a common entry time, or when
#' the covariate distribution among those observed at entry is the same at
#' every entry time. That holds, for instance, when survival to entry selects
#' the same way at every entry time and entry time is unrelated to the
#' covariates; it is the sameness that matters, not how it comes about.
#' Neither condition is checkable from the summaries supplied.
#'
#' Published summaries are ordinarily reported for the enrolled population.
#' Under a common entry time that is the population observed at entry, so they
#' are the right moments; under varying entry times they are the right moments
#' only when the covariate distribution among those observed at entry is the
#' same at every entry time, as above. State which population they came from,
#' and treat a delayed-entry comparator whose moments are known to be
#' pre-selection as a misspecification that no diagnostic here can detect.
#' Delayed entry in the individual arm is unaffected by any of this.
#'
#' @return An object of class `mlumr_agd_surv` (also inheriting `mlumr_agd`).
#'   Its `$pseudo_ipd` carries a `.source_key` column as [set_ipd()] describes:
#'   a digest of the whole of `data` with the row's rank within a canonical
#'   ordering of it, holding nothing of the content, so that
#'   [compare_models()] can recognize one source reordered between two fits.
#'   The internal names, `.source_key` among them, cannot be used as column
#'   names in `data`.
#' @seealso [set_agd()] for non-survival aggregate data.
#'   `multinma::set_agd_surv()` is the ML-NMR equivalent; the name is given as
#'   code rather than as a link because multinma is not a dependency here, and
#'   an anchored link into a package the check environment need not have is
#'   reported as an unresolved cross-reference.
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
    c(".study", ".trt", ".arm", ".time", ".start_time", ".delay_time", ".status",
      ".source_key"),
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
    .require_identity(data[[arm]], arm)
  } else if (!is.null(study)) {
    .require_identity(data[[study]], study)
  } else {
    rep("AgD_Arm", nrow(data))
  }
  study_vec <- if (!is.null(study)) {
    .require_identity(data[[study]], study)
  } else {
    arm_vec
  }
  trt_vec <- .require_identity(data[[treatment]], treatment, as_char = FALSE)

  pseudo_ipd <- data.frame(
    .study = study_vec, .trt = trt_vec, .arm = arm_vec,
    .time = surv_df$.time, .start_time = surv_df$.start_time,
    .delay_time = surv_df$.delay_time, .status = surv_df$.status,
    .source_key = .source_row_keys(data),
    stringsAsFactors = FALSE
  )

  arms <- unique(arm_vec)
  # An arm is one reconstructed curve from one study. Keying only on the arm
  # label merged rows that shared it across studies: study c("S1", "S2") with
  # arm c("control", "control") passed the single-arm check below, was
  # summarized as "S1", and kept both studies in the pseudo-IPD, so two
  # reconstructed curves became one comparator without a word. A single
  # treatment is already required across the whole frame above.
  .require_single_identity(study_vec, arm_vec, "study")
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
    n_events = sum(pseudo_ipd$.status != 0L)
  )
  class(out) <- c("mlumr_agd_surv", "mlumr_agd", "list")
  out
}


#' Refuse a missing grouping identifier
#'
#' `unique()` keeps `NA` and `which(NA == NA)` selects nothing, so an arm
#' column of `NA` passed the single-arm check and then matched no rows: the
#' summary came back with `.study`, `.trt`, and every covariate mean `NA`,
#' from data that carried a treatment and a mean. The object was structurally
#' valid, so nothing downstream objected to integrating over `NA`.
#'
#' @param x The column.
#' @param nm Its name, for the message.
#' @param as_char Return `as.character(x)` rather than `x`.
#' @keywords internal
.require_identity <- function(x, nm, as_char = TRUE) {
  if (anyNA(x)) {
    stop("`", nm, "` must not contain missing values: it identifies which ",
         "rows belong together, and a missing identifier matches no rows, ",
         "leaving an arm summary of NA.", call. = FALSE)
  }
  if (as_char) as.character(x) else x
}


#' Refuse an arm that spans more than one study
#'
#' @param values The identifier to check within each arm.
#' @param arm_vec Arm labels.
#' @param label Name of the identifier, for the message.
#' @keywords internal
.require_single_identity <- function(values, arm_vec, label) {
  for (a in unique(arm_vec)) {
    vals <- unique(values[arm_vec == a])
    if (length(vals) > 1L) {
      stop(sprintf(paste0("Arm '%s' spans more than one %s (%s). An arm is one ",
                          "reconstructed curve from one study on one ",
                          "treatment; give each its own `arm` label."),
                   a, label, paste(sQuote(vals), collapse = ", ")),
           call. = FALSE)
    }
  }
  invisible(TRUE)
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

#' The one scalar `effect` name a survival fit can legitimately supply
#'
#' Turns the natural-scale label into the `effect` selector that names it. Every
#' fit has exactly one: a proportional-hazards fit supplies a hazard ratio, a
#' shared-shape SPFA AFT fit supplies a time ratio, and anything else supplies
#' only the exponentiated location contrast. Keeping this derivation in one
#' place is what stops the selector and the label from disagreeing.
#' @param label The `label` from [.surv_scalar_label()] (natural scale).
#' @return One of `"hr"`, `"tr"`, `"exp_delta_eta"`.
#' @keywords internal
.surv_scalar_effect_name <- function(label) {
  switch(label, HR = "hr", TR = "tr", EXP_DELTA_ETA = "exp_delta_eta",
         stop("Unrecognized survival scalar label: ", label, call. = FALSE))
}

#' Message for an `effect` this survival fit cannot supply
#'
#' Says which estimand the fit does have and why the requested one does not
#' exist for it, rather than only listing the accepted strings: asking for an
#' HR from an AFT fit is a modeling misunderstanding, and "must be one of" does
#' not correct it.
#' @param effect The requested selector.
#' @param label,scalar_effect The fit's natural-scale label and its selector.
#' @param stratified `TRUE` when the baseline shapes differ by study.
#' @param valid_effects The accepted selectors for this fit.
#' @return A character message for [stop()].
#' @keywords internal
.surv_effect_scale_error <- function(effect, label, scalar_effect, stratified,
                                     valid_effects) {
  wrong_scalar <- effect %in% c("hr", "tr", "exp_delta_eta")
  if (!wrong_scalar) {
    return(sprintf("For survival family, `effect` must be one of: %s",
                   paste(valid_effects, collapse = ", ")))
  }
  # Asking for the location contrast on a fit that has a real HR or TR is the
  # mirror image of the other errors, and what the caller needs to hear is when
  # `exp_delta_eta` does apply, not a restatement of what this fit is.
  if (identical(effect, "exp_delta_eta")) {
    return(paste0("`effect = \"exp_delta_eta\"` applies to an AFT distribution ",
                  "whose shapes differ by study (`aux_by = \".study\"`) or to ",
                  "any relaxed AFT fit, whose treatment-specific coefficients ",
                  "leave the covariate term in the contrast. This fit reports ",
                  label, "; request `effect = \"", scalar_effect, "\"`."))
  }
  why <- switch(
    label,
    HR = paste0("this fit uses a proportional-hazards distribution, whose ",
                "scalar contrast is a marginal hazard ratio, not a time ratio ",
                "or a bare location contrast"),
    TR = paste0("this is a shared-shape SPFA accelerated-failure-time fit. Its ",
                "scalar contrast is a time ratio: the covariate term cancels ",
                "from mean(eta_index) - mean(eta_comparator), and the hazard ",
                "ratio is not constant in time, so there is no scalar HR"),
    EXP_DELTA_ETA = if (stratified) {
      paste0("each study has its own AFT shape (`aux_by = \".study\"`), so there ",
             "is no constant acceleration factor at all (the Weibull quantile ",
             "ratio picks up [-log S]^(1/a_i - 1/a_c), the log-normal ",
             "exp(z_p (sigma_i - sigma_c)), and so on)")
    } else {
      paste0("this is a relaxed fit, so the two treatments have different ",
             "coefficients and the covariate term does not cancel from ",
             "mean(eta_index) - mean(eta_comparator). The time ratio varies by ",
             "covariate profile, and exp(delta) is the geometric mean of those ",
             "profile-specific ratios, not one population acceleration factor")
    }
  )
  paste0("`effect = \"", effect, "\"` is not available for this fit: ", why,
         ". Request `effect = \"", scalar_effect, "\"` for the scalar contrast ",
         "this fit does supply, the collapsible `effect = \"rmstd\"` / ",
         "\"rmstr\", or conditional_effects() for profile-specific effects.")
}
