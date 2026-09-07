#' Set up individual patient data (IPD)
#'
#' Prepare IPD from the index treatment for an unanchored indirect comparison.
#'
#' @param data Data frame containing IPD
#' @param treatment Column name for treatment variable
#' @param outcome Column name for outcome variable. For `family = "binomial"`,
#'   must be binary (0/1). For `family = "normal"`, any numeric. For
#'   `family = "poisson"`, non-negative integer counts. Not used (leave `NULL`)
#'   for `family = "survival"`, which uses `Surv`/`time`/`status` instead.
#' @param covariates Character vector of covariate column names
#' @param family Outcome family: `"binomial"`, `"normal"`, `"poisson"`, or
#'   `"survival"` (time-to-event)
#' @param exposure Column name for exposure/time-at-risk (required when
#'   `family = "poisson"`)
#' @param study Column name for study identifier (optional)
#' @param Surv For `family = "survival"`, an optional [survival::Surv()] object
#'   describing the outcome (use for left/interval censoring or delayed entry).
#' @param time,status,entry_time For `family = "survival"`, character column
#'   names as an alternative to `Surv` (right-censoring with status `0`/`1`,
#'   plus optional delayed entry).
#'
#' @return An object of class `mlumr_ipd`. Its `$data` holds the treatment,
#'   study, outcome and covariate columns under internal names, and one more,
#'   `.source_key`: a digest of the whole of `data` together with the row's
#'   rank within a canonical ordering of it, which lets [compare_models()]
#'   tell fits of one source apart from fits of that source reordered, whatever
#'   covariates they use. Nothing of `data`'s content is kept in it. The
#'   internal names, `.source_key` among them, cannot be used as column names
#'   in `data`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Binary outcome
#' ipd <- set_ipd(
#'   data = trial_a,
#'   treatment = "trt",
#'   outcome = "response",
#'   covariates = c("age", "sex")
#' )
#'
#' # Continuous outcome
#' ipd <- set_ipd(
#'   data = trial_a,
#'   treatment = "trt",
#'   outcome = "score",
#'   covariates = c("age", "sex"),
#'   family = "normal"
#' )
#'
#' # Count outcome with exposure
#' ipd <- set_ipd(
#'   data = trial_a,
#'   treatment = "trt",
#'   outcome = "events",
#'   covariates = c("age", "sex"),
#'   family = "poisson",
#'   exposure = "person_years"
#' )
#' }
set_ipd <- function(data, treatment, outcome = NULL, covariates,
                    family = c("binomial", "normal", "poisson", "survival"),
                    exposure = NULL, study = NULL,
                    Surv = NULL, time = NULL, status = NULL, entry_time = NULL) {

  family <- match.arg(family)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }

  if (family == "survival") {
    return(.set_ipd_survival(data, treatment, covariates, study,
                             Surv, time, status, entry_time))
  }

  if (is.null(outcome)) {
    stop("`outcome` is required for binomial, normal, and poisson families",
         call. = FALSE)
  }
  .validate_non_empty_data(data, "IPD")
  .validate_required_covariates(covariates, "covariates")

  required_cols <- .ipd_required_columns(treatment, outcome, covariates,
                                         exposure, study)

  .check_required_columns(data, required_cols)
  .validate_ipd_outcome(data, outcome, family, exposure)
  .validate_reserved_internal_names(
    c(covariates, treatment, outcome, exposure, study),
    c(".study", ".trt", ".outcome", ".exposure", ".source_key"),
    "Column name(s)"
  )
  .validate_ipd_covariates(data, covariates)
  .validate_ipd_finite_columns(data, outcome, exposure, family)

  # The key describes the SOURCE, so it is taken before the model's own
  # complete-case filter. Two models with different covariates drop different
  # incomplete rows, and a key taken afterwards makes one source look like
  # two: the digests differ, the ranks are never compared, and two fits that
  # kept different patients are reported as merely unverifiable. Taken here
  # and carried through the drop, the digest is the source's and the ranks
  # name its rows, so keeping different patients is a mismatch.
  complete <- stats::complete.cases(data[, required_cols])
  source_keys <- .source_row_keys(data)[complete]
  data <- .drop_missing_rows(data, required_cols, complete)
  .validate_complete_rows_remain(data, "IPD")
  .warn_constant_ipd_covariates(data, covariates)
  .warn_collinear_ipd_covariates(data, covariates)
  .validate_single_treatment(data, treatment, "IPD")
  ipd_data <- .standardize_ipd_data(data, treatment, outcome, covariates,
                                    family, exposure, study, source_keys)

  out <- c(
    list(
      data = ipd_data,
      n = nrow(ipd_data),
      treatment = unique(ipd_data$.trt),
      covariates = covariates,
      family = family,
      type = "ipd"
    ),
    .ipd_outcome_summary(ipd_data, family)
  )

  class(out) <- c("mlumr_ipd", "list")
  out
}

#' Required source columns for IPD setup
#' @keywords internal
.ipd_required_columns <- function(treatment, outcome, covariates,
                                  exposure = NULL, study = NULL) {
  cols <- c(treatment, outcome, covariates)
  if (!is.null(exposure)) cols <- c(cols, exposure)
  if (!is.null(study)) cols <- c(cols, study)
  cols
}

#' Validate that required columns are present
#' @keywords internal
.check_required_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Missing columns in data: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate that input data contain at least one row
#' @keywords internal
.validate_non_empty_data <- function(data, label) {
  if (nrow(data) == 0L) {
    stop(sprintf("%s data must contain at least one row", label),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate covariate argument shape before column lookup
#' @keywords internal
.validate_required_covariates <- function(covariates, arg_name) {
  if (!is.character(covariates) || length(covariates) == 0L) {
    stop(sprintf("`%s` must be a non-empty character vector", arg_name),
         call. = FALSE)
  }
  if (any(is.na(covariates)) || any(covariates == "")) {
    stop(sprintf("`%s` must not contain missing or empty names", arg_name),
         call. = FALSE)
  }
  dup_covariates <- unique(covariates[duplicated(covariates)])
  if (length(dup_covariates) > 0L) {
    stop(sprintf("Duplicate covariates: %s",
                 paste(dup_covariates, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate IPD outcome and exposure columns
#' @keywords internal
.validate_ipd_outcome <- function(data, outcome, family, exposure = NULL) {
  if (family == "binomial") {
    outcome_vals <- data[[outcome]]
    if (!is.numeric(outcome_vals) && !is.logical(outcome_vals)) {
      stop("`outcome` must be numeric or logical 0/1 for binomial family",
           call. = FALSE)
    }
    invalid <- !is.na(outcome_vals) & !(outcome_vals %in% c(0, 1))
    if (any(invalid)) {
      stop("`outcome` must be binary (0/1) for binomial family", call. = FALSE)
    }
  } else if (family == "normal") {
    if (!is.numeric(data[[outcome]])) {
      stop("`outcome` must be numeric for normal family", call. = FALSE)
    }
  } else {
    if (is.null(exposure)) {
      stop("`exposure` is required for poisson family", call. = FALSE)
    }
    outcome_vals <- data[[outcome]]
    if (!.is_whole_number_count(outcome_vals, allow_missing = TRUE)) {
      stop("`outcome` must be non-negative integer counts for poisson family",
           call. = FALSE)
    }
    exposure_vals <- data[[exposure]]
    # The Stan models declare the exposures `<lower=1e-12>`, so anything
    # smaller is rejected at initialization with a message that names a Stan
    # variable rather than the column it came from. Reject it here instead.
    if (!is.numeric(exposure_vals) ||
          any(exposure_vals < .mlumr_min_positive, na.rm = TRUE)) {
      stop("`exposure` must be positive numeric values of at least 1e-12",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate a data source contains only one treatment
#' @keywords internal
.validate_single_treatment <- function(data, treatment, label) {
  raw <- data[[treatment]]
  # Reject missing labels: an all-NA column collapses to a single unique value
  # and would otherwise pass as "one treatment", fitting an unlabeled arm.
  if (anyNA(raw)) {
    stop(sprintf("%s treatment column '%s' contains missing (NA) values.",
                 label, treatment), call. = FALSE)
  }
  trt_vals <- unique(raw)
  if (length(trt_vals) < 1L) {
    stop(sprintf("%s treatment column '%s' has no values.", label, treatment),
         call. = FALSE)
  }
  if (length(trt_vals) > 1L) {
    stop(sprintf("%s should contain a single treatment. Found: %s",
                 label, paste(trt_vals, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Reject user names that would overwrite standardized internal columns
#' @keywords internal
.validate_reserved_internal_names <- function(user_names, reserved, label) {
  reserved_hit <- intersect(user_names, reserved)
  if (length(reserved_hit) > 0L) {
    msg <- sprintf(
      "%s collide with reserved internal columns: %s. Please rename and retry.",
      label,
      paste(reserved_hit, collapse = ", ")
    )
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate IPD covariates
#' @keywords internal
.validate_ipd_covariates <- function(data, covariates) {
  for (cov in covariates) {
    if (!is.numeric(data[[cov]])) {
      stop(sprintf("Covariate '%s' must be numeric", cov), call. = FALSE)
    }
    vals <- data[[cov]][!is.na(data[[cov]])]
    if (any(!is.finite(vals))) {
      stop(sprintf("Covariate '%s' contains non-finite values (Inf, -Inf, or NaN)", cov),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate finite IPD outcome and exposure values
#' @keywords internal
.validate_ipd_finite_columns <- function(data, outcome, exposure, family) {
  outcome_nona <- data[[outcome]][!is.na(data[[outcome]])]
  if (any(!is.finite(outcome_nona))) {
    stop("`outcome` contains non-finite values", call. = FALSE)
  }
  if (family == "poisson") {
    exposure_nona <- data[[exposure]][!is.na(data[[exposure]])]
    if (any(!is.finite(exposure_nona))) {
      stop("`exposure` contains non-finite values", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Drop rows with missing setup inputs
#' @keywords internal
.drop_missing_rows <- function(data, required_cols,
                               complete = stats::complete.cases(
                                 data[, required_cols]
                               )) {
  if (!all(complete)) {
    n_missing <- sum(!complete)
    warning(sprintf("%d rows with missing values will be excluded", n_missing),
            call. = FALSE)
    data <- data[complete, ]
  }
  data
}

#' Validate that complete-case filtering left rows to analyze
#' @keywords internal
.validate_complete_rows_remain <- function(data, label) {
  if (nrow(data) == 0L) {
    stop(sprintf("No complete %s rows remain after excluding missing values",
                 label), call. = FALSE)
  }
  invisible(TRUE)
}

#' Warn when IPD covariates have no empirical variation
#' @keywords internal
.warn_constant_ipd_covariates <- function(data, covariates) {
  constant <- vapply(covariates, function(cov) {
    length(unique(data[[cov]])) <= 1L
  }, logical(1))

  if (any(constant)) {
    warning(
      paste(
        "IPD covariate(s) have zero empirical variation after missing-row",
        "filtering:",
        paste(covariates[constant], collapse = ", "),
        "Regression coefficients for constant covariates are not identified",
        "from the IPD; consider removing them or using informative priors."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Warn when IPD covariates are (near-)linearly dependent. Constant covariates are
# handled separately by .warn_constant_ipd_covariates(); this catches collinearity
# among two or more varying covariates, which the constant check cannot see. The
# threshold is deliberately conservative (condition number of the covariate
# correlation matrix > 1000, i.e. |pairwise correlation| above ~0.998 for a pair),
# so it fires only on near-redundant covariates that genuinely weaken
# identification, not on merely-correlated ones that Stan samples without trouble.
.warn_collinear_ipd_covariates <- function(data, covariates) {
  if (length(covariates) < 2L) {
    return(invisible(TRUE))
  }
  x <- data[stats::complete.cases(data[, covariates, drop = FALSE]), covariates,
            drop = FALSE]
  sds <- vapply(x, function(col) stats::sd(as.numeric(col)), numeric(1))
  keep <- is.finite(sds) & sds > 0
  x <- x[, keep, drop = FALSE]
  if (ncol(x) < 2L) {
    return(invisible(TRUE))
  }
  if (nrow(x) < ncol(x) + 1L) {
    # Fewer complete rows than intercept plus covariates. The design is rank
    # deficient, so the correlation matrix cannot tell the covariates apart and
    # the condition number below would be meaningless. Returning quietly here
    # hid the very case the check exists to catch.
    warning(
      paste0(
        "Only ", nrow(x), " complete IPD row(s) for ", ncol(x),
        " varying covariate(s): ", paste(colnames(x), collapse = ", "),
        ". The covariate design is rank deficient, so these coefficients are",
        " not separately identified from the IPD; consider dropping covariates",
        " or using informative priors."
      ),
      call. = FALSE
    )
    return(invisible(TRUE))
  }
  cor_mat <- suppressWarnings(stats::cor(data.matrix(x)))
  if (anyNA(cor_mat)) {
    return(invisible(TRUE))
  }
  ev <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values
  min_ev <- min(ev)
  condition_number <- if (min_ev > 0) max(ev) / min_ev else Inf
  if (condition_number > 1000) {
    warning(
      paste(
        "IPD covariates are highly collinear (condition number",
        format(round(condition_number), big.mark = ","),
        "of the covariate correlation matrix):",
        paste(colnames(x), collapse = ", "),
        ". Near-linearly-dependent covariates are only weakly identified and can",
        "cause slow sampling or divergences; consider dropping or combining",
        "redundant covariates."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Standardize IPD to mlumr's internal column contract
#' @keywords internal
.standardize_ipd_data <- function(data, treatment, outcome, covariates,
                                  family, exposure = NULL, study = NULL,
                                  source_keys = .source_row_keys(data)) {
  ipd_data <- data.frame(
    .study = if (!is.null(study)) data[[study]] else "IPD_Study",
    .trt = data[[treatment]],
    stringsAsFactors = FALSE
  )

  if (family == "normal") {
    ipd_data$.outcome <- as.numeric(data[[outcome]])
  } else {
    ipd_data$.outcome <- .as_count_integer(data[[outcome]])
  }
  if (family == "poisson") {
    ipd_data$.exposure <- as.numeric(data[[exposure]])
  }

  for (cov in covariates) {
    ipd_data[[cov]] <- data[[cov]]
  }
  ipd_data$.source_key <- source_keys
  ipd_data
}

#' One key per row of a source data frame, without keeping its content
#'
#' The stored data keep only the columns a model needs. Two fits of one source
#' with different covariate sets therefore cannot show, from their stored
#' columns alone, that the source was reordered between them when the moved
#' rows agree on everything both fits kept; [compare_models()] needs to know,
#' because its pointwise comparison pairs rows by position.
#'
#' The key is two things, neither of which carries any of the source's content.
#' It is a fingerprint, not a concealment: an unkeyed digest can be recomputed,
#' so a party holding a candidate source can confirm that it is the one, and a
#' small enough space of candidates can be enumerated. What the fit does not
#' hold is the values themselves. The first is a
#' fingerprint of the whole source: its schema and then its rows, columns in
#' name order so that column order does not matter, rows sorted in byte order
#' so that row order does not matter either, and the digest of that is taken.
#' The second is the row's rank in that same sorted order, with identical rows
#' sharing one rank. A fit built from the same source carries the same
#' fingerprint and the same set of ranks, whatever order its rows came in; a
#' source that changed in any column carries a different fingerprint, and then
#' the ranks are never compared. Unused columns such as identifiers or free
#' text go into the digest and are not kept: the fit object holds thirty-two
#' hex digits and an integer per row.
#'
#' The schema is in the digest because the names are what put the columns in
#' order, and a fingerprint that used them without recording them could be
#' matched by a source they order differently. Rename a column across the sort
#' boundary and every value moves to another position; a second source whose
#' values happen to line up that way then produces the same rows, the same
#' digest and the same ranks, and two fits with disjoint covariates and
#' repeated treatments and outcomes would have their order certified on the
#' strength of it. Each value is written with its own length as well, so that
#' the separator between columns cannot be forged by a value that contains it.
#' @keywords internal
.source_row_keys <- function(data) {
  # One string per row for every kind of column: a matrix column (a `Surv`
  # object held in a data frame is one) and a list column would otherwise
  # not give one value per row.
  # A value that contains a separator, or one whose flattening collides with
  # another shape ("a,b" beside `c("a", "b")` in one list column), would make
  # two different rows one string and cost the comparison a distinction it is
  # there to make. Every part is written with its own length, at both levels,
  # so no string is a rearrangement of another.
  # `as.character()` on a double prints 15 significant digits, so two values
  # that differ below that collapse to one string, share a rank, and let a
  # swap of those rows pass as the same order. `%.17g` round-trips every
  # finite double exactly.
  render <- function(x) {
    if (is.double(x)) {
      out <- sprintf("%.17g", x)
      out[!is.finite(x)] <- as.character(x[!is.finite(x)])
      return(out)
    }
    as.character(x)
  }
  tag <- function(x) {
    x <- enc2utf8(render(x))
    x[is.na(x)] <- "\002NA"
    paste0(nchar(x, type = "bytes"), ":", x)
  }
  join <- function(parts) paste(tag(parts), collapse = ",")
  # The ordinary case, an atomic column, is returned as it is so that `tag()`
  # renders it. Flattening it with `as.character()` here reached `render()`
  # with a string and the `%.17g` path above never ran on the columns that
  # almost always carry the doubles: `as.character()` prints 15 significant
  # digits, so 2000 doubles one ulp apart collapse to 45 strings, share ranks,
  # and let a swap of those rows pass as the same order.
  flat <- function(x) {
    if (!is.null(dim(x))) return(apply(x, 1L, join))
    if (is.list(x)) return(vapply(x, function(e) join(render(e)), character(1)))
    x
  }
  # Byte order here too, for the same reason the rows are sorted that way
  # below. The default collation for a character vector follows LC_COLLATE, so
  # the same source read under two locales put its columns in two orders, and
  # column order decides the rendered row, the digest and the ranks. Two fits
  # of one source made under different locales then carried different digests,
  # which reads as two sources: the ranks are never compared, a reordering goes
  # unseen, and one source filtered two ways is no longer a mismatch.
  # Ordered on the names converted to UTF-8, not on the names themselves:
  # radix ordering accepts UTF-8, Latin-1 and bytes and errors on anything
  # else, so a column name in an unknown 8-bit encoding would stop the fit
  # here, where the locale-dependent default had sorted it. The values go
  # through `enc2utf8()` in `tag()` for the same reason. The originals are what
  # index `data`, so the conversion decides the order and nothing else.
  # The conversion is relied on, not guarded. A fallback here would only move
  # the failure three lines down: `tag()` puts these same names through
  # `enc2utf8()` to build the schema, as it does every value and every
  # character column, so a name it cannot convert stops the key either way and
  # names are not a special case. It converted every input tried, invalid
  # sequences included.
  nms <- names(data)
  nms <- nms[order(enc2utf8(nms), method = "radix")]
  cols <- lapply(nms, function(nm) tag(flat(data[[nm]])))
  rows <- if (length(cols)) {
    do.call(paste, c(cols, sep = "\036"))
  } else {
    rep("", nrow(data))
  }
  # Byte order, so that the same source gives the same ranks in every locale.
  canon <- unique(sort(rows, method = "radix"))
  # The schema first: the names that decided the order, and the type of each
  # column, so that a source with the same values under different names or a
  # different storage mode is a different source.
  types <- vapply(data[nms], function(x) paste(class(x), collapse = ","),
                  character(1))
  schema <- paste(tag(paste0(nms, "\035", types)), collapse = "\036")
  tmp <- tempfile("mlumr-source-")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(schema, canon), tmp, useBytes = TRUE)
  paste0(unname(tools::md5sum(tmp)), ":", match(rows, canon))
}

#' Summarize standardized IPD outcomes
#' @keywords internal
.ipd_outcome_summary <- function(ipd_data, family) {
  switch(family,
    binomial = list(n_events = sum(ipd_data$.outcome)),
    normal = list(mean_outcome = mean(ipd_data$.outcome),
                  sd_outcome = sd(ipd_data$.outcome)),
    poisson = list(total_events = sum(ipd_data$.outcome),
                   total_exposure = sum(ipd_data$.exposure))
  )
}


#' Set up aggregate data (AgD)
#'
#' Prepare AgD from the comparator treatment for an unanchored indirect
#' comparison.
#'
#' @param data Data frame containing AgD summary statistics
#' @param treatment Column name for treatment variable
#' @param family Outcome family: `"binomial"`, `"normal"`, or `"poisson"`.
#'   Time-to-event comparator data go to [set_agd_surv()] instead, which takes
#'   reconstructed pseudo-IPD rather than a scalar outcome summary.
#' @param outcome_n Column name for sample size. Required for binomial. For
#'   normal, required when there is more than one aggregate row, because the
#'   comparator-population estimand is the size-weighted mixture of those rows
#'   and they cannot be combined without knowing how large each is; optional for
#'   a single row, where the weighting is irrelevant.
#' @param outcome_r Column name for number of events (required for binomial
#'   and poisson)
#' @param outcome_mean Column name for mean outcome (required for normal)
#' @param outcome_se Column name for standard error of outcome (required for
#'   normal)
#' @param outcome_E Column name for total exposure (required for poisson)
#' @param cov_means Character vector of column names for covariate means/proportions
#' @param cov_sds Character vector of column names for covariate SDs
#'   (`NA` for binary covariates)
#' @param cov_types Character vector specifying `"continuous"` or `"binary"` for
#'   each covariate. If `NULL`, inferred from presence of SD.
#' @param study Column name for study identifier (optional)
#'
#' @details
#' **Rows must partition the aggregate sample, not overlap it.** Every row
#' contributes its own factor to the aggregate likelihood, which multiplies
#' them as if they came from disjoint sets of patients. That is correct when the
#' rows are one arm, or a set of mutually exclusive, jointly defined subgroup
#' cells (for example the four cells of sex crossed with prior therapy). It is
#' wrong when a publication reports several *overlapping* subgroup tables over
#' the same participants, as when age bands, sex, and disease severity are each
#' tabulated separately. Supplying those together counts every patient once per
#' table, and the posterior becomes correspondingly overconfident: the intervals
#' shrink because the model believes it has seen several independent studies.
#' Nothing in the data identifies the overlap, so `set_agd()` cannot detect this
#' and does not try. Choose one partition of the comparator sample and use only
#' its rows. See `vignette("subgroup-identification", "mlumr")` for how many
#' such rows the relaxed model needs.
#'
#' **Scale assumptions for `family = "normal"`.** The AgD likelihood is
#' `y_agd ~ normal(E[exp(eta)], se_agd)` under `link = "log"` and
#' `y_agd ~ normal(E[eta], se_agd)` under `link = "identity"`. In both
#' cases `outcome_mean` and `outcome_se` must be on the **arithmetic
#' (original, untransformed) scale**.
#'
#' A geometric mean is ALREADY on the original measurement scale, and it is a
#' different quantity from the arithmetic mean: exponentiating a log-scale mean
#' returns the geometric mean, not the arithmetic one. For a lognormal outcome
#' with log-scale mean 0 and log-scale SD 1 the geometric mean is 1 while the
#' arithmetic mean is `exp(0.5) = 1.65`, so substituting one for the other is a
#' 39% error in the reported level. No amount of standard-error propagation
#' repairs that: the delta method rescales uncertainty about a transformation,
#' it does not convert one estimand into another.
#'
#' So do not "back-transform and apply the delta method". Ask for, or compute
#' from the individual data, the arithmetic mean and its standard error. If you
#' have only log-scale summaries and are willing to assume the outcome is
#' lognormal, the arithmetic mean is `exp(m + s^2 / 2)` where `m` is the
#' log-scale mean and `s` is the log-scale **SD** of the outcome, not the
#' standard error of `m`; propagate uncertainty in `m` and `s` jointly through
#' that expression. A change score on a transformed scale generally cannot be
#' reversed from a published mean alone at all. Passing log-scale or geometric
#' summaries silently misspecifies the likelihood and biases the posterior.
#'
#' **Scale assumptions for `family = "poisson"`.** `outcome_r` is the
#' total count in each AgD row and `outcome_E` is the total
#' person-time (or other exposure). The Stan likelihood uses
#' `log(E_agd)` as an offset, so rates are modeled on the log scale
#' regardless of how `outcome_r` is tabulated.
#'
#' **What the aggregate Poisson row assumes about exposure.** The likelihood
#' for a row is the total exposure multiplied by the rate averaged over the
#' covariate distribution you supply, while the quantity it stands in for is
#' the sum over people of each person's own exposure times that person's rate.
#' The two agree exactly when the supplied distribution is the one **weighted
#' by exposure**, whatever the dependence between exposure and the covariates:
#' two people with rates 1 and 3 and exposures 9 and 1 contribute
#' `9 * 1 + 1 * 3 = 12` expected events, and `10 * (0.9 * 1 + 0.1 * 3)` is 12 as
#' well. With the person-level distribution instead, the same row gives a total
#' exposure of 10 times the mean rate of 2, which is 20. Person-level moments
#' reproduce the sum only when exposure carries no information about the
#' covariate-specific rate within the row.
#'
#' The assumption is therefore about person-time, not about people, and it
#' is about the whole distribution the rate is averaged over, not only the
#' moments: the marginal shape that each `distr()` in [add_integration()]
#' assumes around `cov_means` and `cov_sds`, and with two or more covariates
#' the correlation it combines them with, whose default is estimated from the
#' index sample. All of it has to describe the covariates weighted by
#' exposure. Exposure can leave every mean and standard deviation where it
#' was and still change a covariate's skewness or tails, or how the
#' covariates go together when it varies with their combination rather than
#' with each on its own, and the averaged rate moves with any of these.
#' Person-level summaries stand in for exposure-weighted ones when exposure
#' carries no information about the covariate-specific rate within the row.
#' Mean exposure that does not vary with the covariates is a sufficient
#' condition that does not depend on the model, because it makes the two
#' distributions coincide, shape and dependence included, and equal
#' individual exposure is its simplest case.
#' Published subgroup tables almost always report person-level moments, and
#' those do not identify the person-time distribution. Where follow-up varies
#' with a prognostic covariate, prefer rows defined so that exposure is close
#' to constant inside each one, and say which reading the reported moments
#' support. Weighting across rows does not repair a dependence inside a row,
#' and nothing in the supplied summaries reveals it, so this is not checked.
#'
#' **Scale assumptions for `family = "binomial"`.** `outcome_r` /
#' `outcome_n` are counts of events and trials. The log-odds (or
#' probit / cloglog under alternative links) are formed from
#' `outcome_r / outcome_n`, so no scale conversion is required.
#'
#' @return An object of class `mlumr_agd`. As for [set_ipd()], its `$data`
#'   carries a `.source_key` column, a digest of the whole of `data` with the
#'   row's rank within a canonical ordering of it, holding nothing of the
#'   content; it lets [compare_models()] recognize one source reordered
#'   between two fits. The internal names, `.source_key` among them, cannot be
#'   used as column names in `data`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Binary outcome
#' agd <- set_agd(
#'   data = trial_b,
#'   treatment = "trt",
#'   outcome_n = "n_total",
#'   outcome_r = "n_events",
#'   cov_means = c("age_mean", "sex_prop"),
#'   cov_sds = c("age_sd", NA),
#'   cov_types = c("continuous", "binary")
#' )
#'
#' # Continuous outcome
#' agd <- set_agd(
#'   data = trial_b,
#'   treatment = "trt",
#'   family = "normal",
#'   outcome_mean = "mean_score",
#'   outcome_se = "se_score",
#'   outcome_n = "n_total",
#'   cov_means = c("age_mean", "sex_prop")
#' )
#'
#' # Count outcome
#' agd <- set_agd(
#'   data = trial_b,
#'   treatment = "trt",
#'   family = "poisson",
#'   outcome_r = "n_events",
#'   outcome_E = "person_years",
#'   cov_means = c("age_mean", "sex_prop")
#' )
#' }
set_agd <- function(data, treatment,
                    family = c("binomial", "normal", "poisson"),
                    outcome_n = NULL, outcome_r = NULL,
                    outcome_mean = NULL, outcome_se = NULL,
                    outcome_E = NULL,
                    cov_means, cov_sds = NULL, cov_types = NULL,
                    study = NULL) {

  family <- match.arg(family)

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  .validate_non_empty_data(data, "AgD")
  .validate_required_covariates(cov_means, "cov_means")

  .validate_agd_outcome_args(family, outcome_n, outcome_r, outcome_mean,
                             outcome_se, outcome_E)
  spec <- .agd_covariate_spec(cov_means, cov_sds, cov_types)
  cov_sds <- spec$cov_sds
  cov_types <- spec$cov_types
  cov_names <- spec$cov_names

  required_cols <- .agd_required_columns(
    treatment = treatment,
    cov_means = cov_means,
    cov_sds = cov_sds,
    outcome_n = outcome_n,
    outcome_r = outcome_r,
    outcome_mean = outcome_mean,
    outcome_se = outcome_se,
    outcome_E = outcome_E,
    study = study
  )

  .check_required_columns(data, required_cols)
  .validate_single_treatment(data, treatment, "AgD")
  .validate_reserved_internal_names(
    c(cov_means, cov_sds[!is.na(cov_sds)], treatment, study,
      outcome_n, outcome_r, outcome_mean, outcome_se, outcome_E),
    c(".study", ".trt", ".n", ".r", ".y", ".se", ".E", ".source_key"),
    "Column name(s)"
  )
  .validate_agd_covariate_names(cov_means)
  .validate_agd_outcomes(data, family, outcome_n, outcome_r, outcome_mean,
                         outcome_se, outcome_E)
  .validate_agd_covariates(data, cov_means, cov_sds)
  .validate_agd_cov_types(cov_types)
  .validate_agd_binary_covariates(data, cov_means, cov_sds, cov_types)

  agd_data <- .standardize_agd_data(
    data = data,
    treatment = treatment,
    family = family,
    outcome_n = outcome_n,
    outcome_r = outcome_r,
    outcome_mean = outcome_mean,
    outcome_se = outcome_se,
    outcome_E = outcome_E,
    cov_means = cov_means,
    cov_sds = cov_sds,
    cov_names = cov_names,
    study = study
  )
  cov_info <- .agd_cov_info(cov_names, cov_sds, cov_types)

  out <- c(
    list(
      data = agd_data,
      treatment = unique(agd_data$.trt),
      covariates = cov_names,
      cov_info = cov_info,
      family = family,
      type = "agd"
    ),
    .agd_outcome_summary(data, family, outcome_n, outcome_r,
                         outcome_mean, outcome_se, outcome_E)
  )

  class(out) <- c("mlumr_agd", "list")
  out
}

#' Validate AgD outcome column arguments
#' @keywords internal
.validate_agd_outcome_args <- function(family, outcome_n, outcome_r,
                                       outcome_mean, outcome_se, outcome_E) {
  if (family == "binomial" && (is.null(outcome_n) || is.null(outcome_r))) {
    stop("`outcome_n` and `outcome_r` are required for binomial family",
         call. = FALSE)
  }
  if (family == "normal" && (is.null(outcome_mean) || is.null(outcome_se))) {
    stop("`outcome_mean` and `outcome_se` are required for normal family",
         call. = FALSE)
  }
  if (family == "poisson" && (is.null(outcome_r) || is.null(outcome_E))) {
    stop("`outcome_r` and `outcome_E` are required for poisson family",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Normalize AgD covariate specification
#' @keywords internal
.agd_covariate_spec <- function(cov_means, cov_sds = NULL, cov_types = NULL) {
  n_cov <- length(cov_means)

  if (is.null(cov_sds)) {
    cov_sds <- rep(NA_character_, n_cov)
  } else if (length(cov_sds) != n_cov) {
    stop("`cov_sds` must have same length as `cov_means` or be NULL",
         call. = FALSE)
  }

  if (is.null(cov_types)) {
    cov_types <- ifelse(is.na(cov_sds), "binary", "continuous")
  } else if (length(cov_types) != n_cov) {
    stop("`cov_types` must have same length as `cov_means` or be NULL",
         call. = FALSE)
  }

  list(
    cov_sds = cov_sds,
    cov_types = cov_types,
    cov_names = .strip_agd_cov_suffix(cov_means)
  )
}

#' AgD source columns required for setup
#' @keywords internal
.agd_required_columns <- function(treatment, cov_means, cov_sds,
                                  outcome_n = NULL, outcome_r = NULL,
                                  outcome_mean = NULL, outcome_se = NULL,
                                  outcome_E = NULL, study = NULL) {
  cols <- c(treatment, cov_means)
  for (col in c(outcome_n, outcome_r, outcome_mean, outcome_se, outcome_E)) {
    if (!is.null(col)) cols <- c(cols, col)
  }
  sd_cols <- cov_sds[!is.na(cov_sds)]
  if (length(sd_cols) > 0L) cols <- c(cols, sd_cols)
  if (!is.null(study)) cols <- c(cols, study)
  cols
}

#' Strip AgD covariate suffixes
#' @keywords internal
.strip_agd_cov_suffix <- function(cov_means) {
  sub("_prop$", "", sub("_mean$", "", cov_means))
}

#' Validate AgD covariate names after suffix stripping
#' @keywords internal
.validate_agd_covariate_names <- function(cov_means) {
  stripped <- .strip_agd_cov_suffix(cov_means)
  dup_stripped <- unique(stripped[duplicated(stripped)])
  if (length(dup_stripped) > 0L) {
    msg <- sprintf(
      paste0(
        "Covariate-name collision after stripping _mean/_prop suffix: %s. ",
        "Pass distinct `cov_means` entries."
      ),
      paste(dup_stripped, collapse = ", ")
    )
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate AgD outcome summaries
#' @keywords internal
.validate_agd_outcomes <- function(data, family, outcome_n, outcome_r,
                                   outcome_mean, outcome_se, outcome_E) {
  if (family == "binomial") {
    .validate_agd_binomial_outcomes(data[[outcome_r]], data[[outcome_n]])
  } else if (family == "normal") {
    .validate_agd_normal_outcomes(data[[outcome_mean]], data[[outcome_se]])
    if (nrow(data) > 1L && is.null(outcome_n)) {
      stop("`outcome_n` is required for multiple normal aggregate rows so ",
           "they can be combined using population weights.", call. = FALSE)
    }
    if (!is.null(outcome_n)) {
      .validate_agd_sample_size(data[[outcome_n]])
    }
  } else {
    .validate_agd_poisson_outcomes(data[[outcome_r]], data[[outcome_E]])
  }
  invisible(TRUE)
}

#' Validate binomial AgD outcomes
#' @keywords internal
.validate_agd_binomial_outcomes <- function(r_vals, n_vals) {
  if (any(is.na(r_vals)) || any(is.na(n_vals))) {
    stop("`outcome_r` and `outcome_n` must not contain NA values", call. = FALSE)
  }
  if (!is.numeric(r_vals) || !is.numeric(n_vals)) {
    stop("`outcome_r` and `outcome_n` must be numeric", call. = FALSE)
  }
  if (any(!is.finite(r_vals)) || any(!is.finite(n_vals))) {
    stop("`outcome_r` and `outcome_n` must be finite", call. = FALSE)
  }
  if (!.is_whole_number_count(r_vals)) {
    stop("`outcome_r` must be integer counts", call. = FALSE)
  }
  if (!.is_whole_number_count(n_vals)) {
    stop("`outcome_n` must be integer sample sizes", call. = FALSE)
  }
  if (any(r_vals < 0)) {
    stop("`outcome_r` must be non-negative", call. = FALSE)
  }
  if (any(n_vals <= 0)) {
    stop("`outcome_n` must be positive", call. = FALSE)
  }
  if (any(r_vals > n_vals)) {
    stop("`outcome_r` must not exceed `outcome_n`", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate optional AgD sample sizes
#' @keywords internal
.validate_agd_sample_size <- function(n_vals) {
  if (any(is.na(n_vals))) {
    stop("`outcome_n` must not contain NA values", call. = FALSE)
  }
  if (!is.numeric(n_vals)) {
    stop("`outcome_n` must be numeric", call. = FALSE)
  }
  if (any(!is.finite(n_vals))) {
    stop("`outcome_n` must be finite", call. = FALSE)
  }
  if (!.is_whole_number_count(n_vals)) {
    stop("`outcome_n` must be integer sample sizes", call. = FALSE)
  }
  if (any(n_vals <= 0)) {
    stop("`outcome_n` must be positive", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate normal AgD outcomes
#' @keywords internal
.validate_agd_normal_outcomes <- function(y_vals, se_vals) {
  if (any(is.na(y_vals)) || any(is.na(se_vals))) {
    stop("`outcome_mean` and `outcome_se` must not contain NA values",
         call. = FALSE)
  }
  if (!is.numeric(y_vals) || !is.numeric(se_vals)) {
    stop("`outcome_mean` and `outcome_se` must be numeric", call. = FALSE)
  }
  if (any(!is.finite(y_vals)) || any(!is.finite(se_vals))) {
    stop("`outcome_mean` and `outcome_se` must be finite", call. = FALSE)
  }
  # Matches the `<lower=1e-12>` bound the normal Stan models declare.
  if (any(se_vals < .mlumr_min_positive)) {
    stop("`outcome_se` must be positive and at least 1e-12", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate Poisson AgD outcomes
#' @keywords internal
.validate_agd_poisson_outcomes <- function(r_vals, E_vals) {
  if (any(is.na(r_vals)) || any(is.na(E_vals))) {
    stop("`outcome_r` and `outcome_E` must not contain NA values", call. = FALSE)
  }
  if (!is.numeric(r_vals) || !is.numeric(E_vals)) {
    stop("`outcome_r` and `outcome_E` must be numeric", call. = FALSE)
  }
  if (any(!is.finite(r_vals)) || any(!is.finite(E_vals))) {
    stop("`outcome_r` and `outcome_E` must be finite", call. = FALSE)
  }
  if (!.is_whole_number_count(r_vals)) {
    stop("`outcome_r` must be integer counts", call. = FALSE)
  }
  if (any(r_vals < 0)) {
    stop("`outcome_r` must be non-negative", call. = FALSE)
  }
  # Matches the `<lower=1e-12>` bound the poisson Stan models declare.
  if (any(E_vals < .mlumr_min_positive)) {
    stop("`outcome_E` must be positive and at least 1e-12", call. = FALSE)
  }
  invisible(TRUE)
}

#' Check whether values are valid non-negative whole-number counts
#' @keywords internal
.is_whole_number_count <- function(x, allow_missing = FALSE) {
  if (!is.numeric(x)) {
    return(FALSE)
  }
  vals <- x
  if (allow_missing) {
    vals <- vals[!is.na(vals)]
  } else if (any(is.na(vals))) {
    return(FALSE)
  }
  if (length(vals) == 0L) {
    return(TRUE)
  }
  all(is.finite(vals) &
        vals >= 0 &
        vals <= .Machine$integer.max &
        abs(vals - round(vals)) <= sqrt(.Machine$double.eps))
}

#' Validate AgD covariate summaries
#' @keywords internal
.validate_agd_covariates <- function(data, cov_means, cov_sds) {
  for (cm in cov_means) {
    if (!is.numeric(data[[cm]])) {
      stop(sprintf("Covariate mean column '%s' must be numeric", cm),
           call. = FALSE)
    }
    if (any(!is.finite(data[[cm]]))) {
      stop(sprintf("Covariate mean column '%s' contains non-finite values", cm),
           call. = FALSE)
    }
  }

  for (cs in cov_sds[!is.na(cov_sds)]) {
    if (!is.numeric(data[[cs]])) {
      stop(sprintf("Covariate SD column '%s' must be numeric", cs),
           call. = FALSE)
    }
    if (any(!is.finite(data[[cs]]))) {
      stop(sprintf("Covariate SD column '%s' contains non-finite values", cs),
           call. = FALSE)
    }
    if (any(data[[cs]] < 0)) {
      stop(sprintf("Covariate SD column '%s' must be non-negative", cs),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate AgD covariate type labels
#' @keywords internal
.validate_agd_cov_types <- function(cov_types) {
  valid_cov_types <- c("binary", "continuous")
  bad_types <- setdiff(unique(cov_types), valid_cov_types)
  if (length(bad_types) > 0L) {
    stop(sprintf("Invalid cov_types: %s. Must be 'binary' or 'continuous'.",
                 paste(sQuote(bad_types), collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate binary AgD covariate summaries
#' @keywords internal
.validate_agd_binary_covariates <- function(data, cov_means, cov_sds, cov_types) {
  for (i in seq_along(cov_types)) {
    if (cov_types[[i]] != "binary") next

    mean_vals <- data[[cov_means[[i]]]]
    cov_label <- .strip_agd_cov_suffix(cov_means[[i]])

    if (any(mean_vals < 0 | mean_vals > 1)) {
      msg <- sprintf(
        "Binary covariate '%s': mean/proportion must be in [0, 1], got %s",
        cov_label,
        paste(mean_vals[mean_vals < 0 | mean_vals > 1], collapse = ", ")
      )
      stop(msg, call. = FALSE)
    }

    if (!is.na(cov_sds[[i]])) {
      sd_vals <- data[[cov_sds[[i]]]]
      max_sd <- sqrt(mean_vals * (1 - mean_vals))
      impossible <- sd_vals > max_sd + 1e-10
      if (any(impossible)) {
        msg <- sprintf(
          paste0(
            "Binary covariate '%s': SD exceeds Bernoulli maximum sqrt(p*(1-p)). ",
            "Got SD=%s for p=%s (max SD=%s)"
          ),
          cov_label,
          paste(round(sd_vals[impossible], 4), collapse = ", "),
          paste(round(mean_vals[impossible], 4), collapse = ", "),
          paste(round(max_sd[impossible], 4), collapse = ", ")
        )
        stop(msg, call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

#' Standardize AgD to mlumr's internal column contract
#' @keywords internal
.standardize_agd_data <- function(data, treatment, family, outcome_n,
                                  outcome_r, outcome_mean, outcome_se,
                                  outcome_E, cov_means, cov_sds,
                                  cov_names, study = NULL) {
  agd_data <- data.frame(
    .study = if (!is.null(study)) data[[study]] else "AgD_Study",
    .trt = data[[treatment]],
    stringsAsFactors = FALSE
  )

  if (family == "binomial") {
    agd_data$.n <- data[[outcome_n]]
    agd_data$.r <- data[[outcome_r]]
  } else if (family == "normal") {
    agd_data$.y <- data[[outcome_mean]]
    agd_data$.se <- data[[outcome_se]]
    if (!is.null(outcome_n)) agd_data$.n <- data[[outcome_n]]
  } else {
    agd_data$.r <- data[[outcome_r]]
    agd_data$.E <- data[[outcome_E]]
  }

  for (i in seq_along(cov_means)) {
    cov_name <- cov_names[[i]]
    agd_data[[paste0(cov_name, "_mean")]] <- data[[cov_means[[i]]]]
    if (!is.na(cov_sds[[i]])) {
      agd_data[[paste0(cov_name, "_sd")]] <- data[[cov_sds[[i]]]]
    }
  }
  agd_data$.source_key <- .source_row_keys(data)
  agd_data
}

#' Build AgD covariate metadata
#' @keywords internal
.agd_cov_info <- function(cov_names, cov_sds, cov_types) {
  cov_info <- list()
  for (i in seq_along(cov_names)) {
    cov_name <- cov_names[[i]]
    cov_info[[cov_name]] <- list(
      mean_col = paste0(cov_name, "_mean"),
      sd_col = if (!is.na(cov_sds[[i]])) paste0(cov_name, "_sd") else NA,
      type = cov_types[[i]]
    )
  }
  cov_info
}

#' Summarize AgD outcomes
#' @keywords internal
.agd_outcome_summary <- function(data, family, outcome_n, outcome_r,
                                 outcome_mean, outcome_se, outcome_E) {
  switch(family,
    binomial = list(n = data[[outcome_n]], n_events = data[[outcome_r]]),
    normal = list(y = data[[outcome_mean]], se = data[[outcome_se]],
                  n = if (!is.null(outcome_n)) data[[outcome_n]] else NULL),
    poisson = list(n_events = data[[outcome_r]],
                   total_exposure = data[[outcome_E]])
  )
}


#' Combine IPD and AgD for unanchored comparison
#'
#' @param ipd An `mlumr_ipd` object from [set_ipd()]
#' @param agd An `mlumr_agd` object from [set_agd()]
#'
#' @return An object of class `mlumr_data`
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- combine_data(ipd, agd)
#' }
combine_data <- function(ipd, agd) {

  if (!inherits(ipd, "mlumr_ipd")) {
    stop("`ipd` must be created with set_ipd()", call. = FALSE)
  }
  if (!inherits(agd, "mlumr_agd")) {
    stop("`agd` must be created with set_agd()", call. = FALSE)
  }

  if (ipd$family != agd$family) {
    stop(sprintf("Family mismatch: IPD uses '%s' but AgD uses '%s'",
                 ipd$family, agd$family), call. = FALSE)
  }

  if (!identical(sort(ipd$covariates), sort(agd$covariates))) {
    stop("Covariates must match between IPD and AgD", call. = FALSE)
  }

  shared_trt <- intersect(ipd$treatment, agd$treatment)
  if (length(shared_trt) > 0) {
    # An unanchored comparison contrasts two *different* treatments across two
    # single-arm sources. A shared label makes the two model intercepts describe
    # the same treatment, so the reported "effect" is a study/population baseline
    # difference reported as a treatment effect. This cannot be made valid by a
    # warning; reject it. (When `study` is not supplied the two sides still carry
    # distinct treatment labels, so this only fires on genuine overlap.)
    msg <- paste0(
      "IPD and AgD share treatment label(s): %s. An unanchored comparison ",
      "requires two distinct treatments (one per source); a shared label would ",
      "estimate a treatment effect from baseline differences between the ",
      "studies. Relabel the arms, or use an anchored method for shared-comparator ",
      "evidence."
    )
    stop(sprintf(msg, paste(shared_trt, collapse = ", ")), call. = FALSE)
  }

  # In an unanchored comparison IPD and AgD come from different studies; a shared
  # study label is unusual and likely a data-entry error. (When `study` is not
  # supplied the two sides default to distinct labels, so this only fires on
  # explicit overlap.)
  shared_studies <- intersect(unique(ipd$data$.study), unique(agd$data$.study))
  if (length(shared_studies) > 0) {
    msg <- paste0(
      "IPD and AgD share study label(s): %s. An unanchored comparison normally ",
      "draws IPD and AgD from different studies; check the `study` columns."
    )
    warning(sprintf(msg, paste(shared_studies, collapse = ", ")), call. = FALSE)
  }

  out <- list(
    ipd = ipd,
    agd = agd,
    family = ipd$family,
    covariates = ipd$covariates,
    n_covariates = length(ipd$covariates),
    treatments = c(ipd$treatment, agd$treatment),
    index_treatment = ipd$treatment,
    comparator_treatment = agd$treatment,
    has_integration = FALSE
  )

  class(out) <- c("mlumr_data", "list")
  out
}


#' @method print mlumr_data
#' @export
print.mlumr_data <- function(x, ...) {
  family <- x$family %||% "binomial"
  family_label <- switch(family,
    binomial = "Binary",
    normal   = "Continuous",
    poisson  = "Count",
    survival = "Time-to-event"
  )

  cat(sprintf("Unanchored Comparison Data (%s)\n", family_label))
  cat("====================================\n\n")
  cat("Index treatment (IPD):", x$index_treatment, "\n")
  cat("  N =", x$ipd$n, "\n")

  if (family == "binomial") {
    cat("  Events =", x$ipd$n_events,
        sprintf("(%.1f%%)", 100 * x$ipd$n_events / x$ipd$n), "\n\n")
  } else if (family == "normal") {
    cat(sprintf("  Mean outcome = %.3f (SD = %.3f)\n\n",
                x$ipd$mean_outcome, x$ipd$sd_outcome))
  } else if (family == "survival") {
    # Survival was given a family label and then left to fall through to the
    # Poisson branch, which reads fields it does not have. x$ipd$total_events
    # is NULL, and sprintf("%d", NULL) is character(0), so the event line
    # printed nothing at all.
    cat(sprintf("  Events = %d (%.1f%%), censored = %d\n\n",
                x$ipd$n_events, 100 * x$ipd$n_events / x$ipd$n,
                x$ipd$n - x$ipd$n_events))
  } else {
    cat(sprintf("  Total events = %d, Total exposure = %.1f\n\n",
                x$ipd$total_events, x$ipd$total_exposure))
  }

  cat("Comparator treatment (AgD):", x$comparator_treatment, "\n")
  if (family == "binomial") {
    cat("  N =", x$agd$n, "\n")
    cat("  Events =", x$agd$n_events,
        sprintf("(%.1f%%)", 100 * x$agd$n_events / x$agd$n), "\n\n")
  } else if (family == "normal") {
    if (!is.null(x$agd$n)) cat("  N =", x$agd$n, "\n")
    cat(sprintf("  Mean outcome = %s, SE = %s\n\n",
                paste(round(x$agd$y, 3), collapse = ", "),
                paste(round(x$agd$se, 3), collapse = ", ")))
  } else if (family == "survival") {
    # And here sum(NULL) is 0, so the comparator arm reported a fabricated
    # "Total exposure = 0.0" that no reconstructed curve ever had.
    cat(sprintf("  Reconstructed pseudo-IPD: %d row(s)\n", x$agd$n_pseudo))
    cat(sprintf("  Events = %d (%.1f%%), censored = %d\n\n",
                x$agd$n_events, 100 * x$agd$n_events / x$agd$n_pseudo,
                x$agd$n_pseudo - x$agd$n_events))
  } else {
    cat(sprintf("  Total events = %d, Total exposure = %.1f\n\n",
                sum(x$agd$n_events), sum(x$agd$total_exposure)))
  }

  cat("Covariates (", x$n_covariates, "):",
      paste(x$covariates, collapse = ", "), "\n")
  if (x$has_integration) {
    cat("Integration points:", x$n_int, "(QMC with Gaussian copula)\n")
  } else {
    cat("Integration points: not yet added (use add_integration())\n")
  }
  invisible(x)
}
