#' Refuse a pointwise log-likelihood that is not one column per observation
#'
#' Every route into LOO, WAIC, and DIC treats one column of `log_lik_agd` as one
#' held-out data point, and the arm / aggregate routes additionally align those
#' columns with `stan_data$agd_arm`. Tie aggregation breaks both assumptions in
#' the same way: it keeps one row per distinct likelihood key and carries the
#' multiplicity in `stan_data$agd_count`, so `log_lik_agd` holds one UNWEIGHTED
#' value per UNIQUE row and `agd_arm` is the collapsed arm map. Both objects
#' still agree in length, so nothing downstream errors, and the diagnostics come
#' back quietly understating every tied observation.
#'
#' Fail closed instead. The multiplicities must be expanded back to the original
#' observation sequence (repeat unique column `k` its `agd_count[k]` times, and
#' expand the arm map with it) before any diagnostic reads them.
#'
#' No shipped code path sets `agd_count`, so this guard is inert today: it is a
#' precondition on the pointwise likelihood that tie aggregation would violate,
#' placed before that feature rather than after the first wrong LOO value.
#' Deleting it as unused would remove the check at exactly the moment the
#' feature that needs it arrives.
#' @keywords internal
.assert_agd_loglik_per_observation <- function(object) {
  cnt <- object$stan_data$agd_count
  if (is.null(cnt) || !length(cnt)) return(invisible(TRUE))
  # A multiplicity is a count of observations, so validate it before drawing any
  # conclusion from `sum(cnt)`. Non-integer counts passed the expanded-shape test
  # below while `.agd_center_weights()` truncated them with `as.integer()`, so
  # `agd_count = c(1.5, 1.5)` with three likelihood columns was accepted here and
  # produced a two-element arm map there. `all(cnt <= 1)` also used to return
  # early, which waved through fractional and zero counts.
  if (!is.numeric(cnt) || anyNA(cnt) || !all(is.finite(cnt)) || any(cnt < 1) ||
        any(cnt != trunc(cnt))) {
    stop("`stan_data$agd_count` must hold finite whole-number multiplicities of ",
         "at least one, since each is a count of observations that one retained ",
         "AgD row stands for.", call. = FALSE)
  }
  if (all(cnt == 1)) return(invisible(TRUE))
  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) return(invisible(TRUE))
  n_cols <- length(.ordered_log_lik_columns(draws, "agd"))
  # Only ONE column count means the multiplicities were expanded: one column per
  # observation, `sum(cnt)`. Treating every count that merely differs from
  # `length(cnt)` as expanded passed any other shape too, which is the state
  # this guard exists to catch rather than wave through.
  if (n_cols == sum(cnt)) return(invisible(TRUE))
  if (n_cols != length(cnt)) {
    stop("`log_lik_agd` has ", n_cols, " column(s), which is neither one per ",
         "retained AgD row (", length(cnt), ") nor one per observation (",
         sum(cnt), "). LOO, WAIC, and DIC cannot align the pointwise ",
         "likelihood with the arm map in that state.", call. = FALSE)
  }
  stop("This fit collapsed tied aggregate rows (`stan_data$agd_count` has ",
       "multiplicities above one), so `log_lik_agd` holds one value per ",
       "UNIQUE row rather than one per observation. LOO, WAIC, and DIC would ",
       "count each tied observation once instead of ", max(cnt), " times, and ",
       "arm grouping would use the collapsed arm map. Expand the pointwise ",
       "log-likelihood and the arm map back to the original observations ",
       "before requesting diagnostics.", call. = FALSE)
}


#' Extract the full pointwise log-likelihood matrix from an mlumr_fit
#'
#' Combines the IPD and AgD per-observation log-likelihood draws into a
#' single matrix with one row per posterior draw and one column per
#' observation. The result is suitable for direct use with
#' [loo::loo()] / [loo::waic()].
#'
#' @param object An `mlumr_fit` object.
#' @return A numeric matrix of dimension `n_draws x (n_ipd + n_agd)`. IPD
#'   columns come first, then the AgD columns. The AgD pointwise unit is whatever
#'   the family's Stan model emits as `log_lik_agd`: for binomial / normal /
#'   poisson this is **one column per aggregate row**; for **survival** it is
#'   **one column per reconstructed pseudo-individual** (not per aggregate row),
#'   so survival LOO/WAIC operate at the pseudo-individual level. See the notes
#'   on [calculate_loo()] / [calculate_waic()].
#' @keywords internal
extract_log_lik <- function(object) {
  .validate_mlumr_fit_object(object)
  .assert_agd_loglik_per_observation(object)

  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) {
    stop("`object$draws` must contain named posterior draw columns.",
         call. = FALSE)
  }

  ipd_cols <- .ordered_log_lik_columns(draws, "ipd")
  agd_cols <- .ordered_log_lik_columns(draws, "agd")
  if (length(ipd_cols) == 0L && length(agd_cols) == 0L) {
    stop(
      "Pointwise log-likelihood columns not found in draws. ",
      "This package requires Stan models that generate `log_lik_ipd` and ",
      "`log_lik_agd` as vectors (per-observation). If you have a fit from ",
      "an older version of mlumr; refit with the current version.",
      call. = FALSE
    )
  }

  selected <- draws[, c(ipd_cols, agd_cols), drop = FALSE]
  numeric_cols <- vapply(selected, is.numeric, logical(1))
  if (!all(numeric_cols)) {
    stop("Pointwise log-likelihood columns must be numeric.", call. = FALSE)
  }

  log_lik <- as.matrix(selected)
  .validate_log_lik_matrix(log_lik)
  log_lik
}


#' The observations a fit's pointwise likelihood is over
#'
#' Every comparison here is paired: `loo_compare()` differences pointwise
#' values column by column, and DIC ranks totals over one data set. An equal
#' number of columns is all `loo` itself can check, and it is not enough:
#' two fits of different data with the same number of rows, or of the same
#' data in a different row order, produce a matrix of the right shape and a
#' comparison that means nothing. The fit carries the data it was built from,
#' so the identity can be checked rather than assumed.
#'
#' The frames returned hold, in stored row order, the internal columns that
#' define an observation (`.study`, `.trt`, the outcome, exposure, and for
#' survival the times and status; named explicitly, since only the internal
#' names are reserved and a covariate may itself begin with a dot) together
#' with the covariates the fit used. For survival comparators the aggregate
#' rows carry the covariate summaries and the reconstructed pseudo-individuals
#' carry the times and status, and the pointwise units are the latter; both
#' frames are kept, since a change to either changes the likelihood.
#'
#' The values define an observation, not their representation. A factor and
#' the character vector it codes, with or without unused levels, or an integer
#' count and the double that was read from a file, describe the same
#' observations, so each column is reduced to its plain values. Study,
#' treatment and arm are labels: a study numbered 1 is the same study whether
#' the number was stored as a factor, an integer or a double, so those are
#' compared as the strings that name them. Counts are accepted within
#' rounding tolerance and rounded before Stan sees them, and are rounded the
#' same way here.
#'
#' @param fit An `mlumr_fit`.
#' @return A list with elements `ipd`, `agd` and `pseudo` (the last `NULL`
#'   outside survival), or `NULL` when the fit carries no data.
#' @keywords internal
.observation_frames <- function(fit) {
  data <- fit$data
  if (is.null(data) || is.null(data$ipd$data)) return(NULL)
  plain <- function(df) {
    if (is.null(df)) return(NULL)
    df <- as.data.frame(df)
    rownames(df) <- NULL
    df[] <- lapply(df, function(x) {
      if (is.factor(x)) as.character(x) else if (is.numeric(x)) as.numeric(x) else as.vector(x)
    })
    for (nm in intersect(c(".study", ".trt", ".arm"), names(df))) {
      df[[nm]] <- as.character(df[[nm]])
    }
    for (nm in intersect(c(".n", ".r"), names(df))) {
      df[[nm]] <- as.numeric(.as_count_integer(df[[nm]]))
    }
    df
  }
  list(ipd = plain(data$ipd$data), agd = plain(data$agd$data),
       pseudo = plain(data$agd$pseudo_ipd))
}

#' The internal columns that define an observation, across the families
#' @keywords internal
.observation_columns <- c(".study", ".trt", ".arm", ".outcome", ".exposure",
                          ".n", ".r", ".y", ".se", ".E",
                          ".time", ".start_time", ".delay_time", ".status")

#' Were two fits built on the same observations, row for row?
#'
#' The observation columns must agree exactly, and so must every covariate
#' column the two fits share. Covariates only one of them uses are left out on
#' purpose: models of the same outcomes with different covariate sets are
#' exactly what gets compared. Two rows that differ only in such a covariate
#' are exchangeable for the model that does not use it, since its pointwise
#' likelihood is the same for both, so either pairing gives that model the
#' same comparison. A row swap that changes a shared covariate is a different
#' pairing for both models and is a mismatch.
#'
#' The stored columns can only show what both fits kept. When the fits share
#' no covariate, a swap of two rows that agree on the outcome columns moves
#' both models' pointwise likelihoods and leaves nothing in those columns to
#' see. The setup functions therefore record, for every row, a key made of a
#' fingerprint of the whole source and the row's rank within it
#' ([.source_row_keys()]). Two fits holding the same set of keys in a
#' different order were built from one source reordered between them, and
#' that is a mismatch whatever the columns say. Different sets mean the source
#' itself changed between the fits, in columns the models did not use; the
#' keys then cannot say whether the rows are in the same order, and neither
#' can the columns, so the answer is `NA`: not a mismatch, and not verified
#' either. A frame with one row cannot be reordered and needs no key.
#' @param a,b Results of [.observation_frames()].
#' @return `TRUE`, `FALSE`, or `NA` when the observations agree but the row
#'   order could not be verified.
#' @keywords internal
.same_observations <- function(a, b, pseudo_grouped = FALSE,
                               unordered = FALSE) {
  same_frame <- function(x, y, grouped = FALSE) {
    if (is.null(x) && is.null(y)) return(TRUE)
    if (is.null(x) || is.null(y)) return(FALSE)
    shared <- setdiff(intersect(names(x), names(y)), ".source_key")
    obs <- intersect(.observation_columns, union(names(x), names(y)))
    if (!all(obs %in% shared)) return(FALSE)
    kx <- x$.source_key
    ky <- y$.source_key
    if (unordered || grouped) {
      # These two allow a reordering, so they cannot ask the keys about order.
      # They still have to ask whether the rows are the same rows: a criterion
      # that does not depend on the order still depends on the observations.
      rows <- .same_grouped_rows(x, y, shared, group = grouped && !unordered)
      if (!isTRUE(rows)) return(rows)
      if (nrow(x) < 2L) return(TRUE)
      return(!.kept_different_rows_of_one_source(kx, ky))
    }
    if (!identical(x[shared], y[shared])) return(FALSE)
    if (nrow(x) < 2L) return(TRUE)
    if (is.null(kx) || is.null(ky)) return(NA)
    if (identical(kx, ky)) return(TRUE)
    if (identical(.sorted_keys(kx), .sorted_keys(ky))) return(FALSE)
    # A key is a source digest and a rank within that source. Equal digests
    # with different ranks are one source that two fits filtered differently,
    # which is what happens when the models use covariates that are missing on
    # different rows: the complete-case drop keeps different patients, the row
    # counts can still match, and the shared columns can still agree. That
    # pairs one patient's likelihood with another's, so it is a mismatch and
    # not something the keys are unable to see. Only a different source leaves
    # the order genuinely unverifiable.
    if (.kept_different_rows_of_one_source(kx, ky)) return(FALSE)
    NA
  }
  same_frame(a$ipd, b$ipd) && same_frame(a$agd, b$agd) &&
    same_frame(a$pseudo, b$pseudo, grouped = pseudo_grouped)
}

#' The source digests a set of row keys was built from
#'
#' A key is `<digest>:<rank>`; the digest names the source and the rank names
#' the row within it. Sorted and deduplicated so that two frames drawing on
#' the same sources compare equal whatever order their rows arrived in.
#' @param keys A `.source_key` column.
#' @return The distinct digests, sorted.
#' @keywords internal
.source_digests <- function(keys) {
  .sorted_keys(unique(sub(":.*$", "", keys)))
}

#' Sort key strings in byte order
#'
#' The keys are compared by sorting both sides and testing the results for
#' equality, so the order has to be a total order on the strings themselves.
#' The default collation follows `LC_COLLATE`, which can rank two distinct
#' strings as equal, and two equal multisets then sort into two different
#' vectors and read as a mismatch. Byte order is total and the same everywhere,
#' which is what [.source_row_keys()] builds the keys in.
#' @param keys A character vector of keys or digests.
#' @return The same values in byte order.
#' @keywords internal
.sorted_keys <- function(keys) {
  sort(keys, method = "radix")
}

#' Did two frames keep different rows of one source?
#'
#' Equal digests with unequal ranks are one source that two fits filtered
#' differently, which is what happens when the models use covariates that are
#' missing on different rows. That is a mismatch under every ordering
#' semantics: it is not a reordering, so allowing one does not allow it. Every
#' path through [.same_observations()] therefore asks this, and the paths that
#' permit a reordering ask only this.
#'
#' Absent keys and different digests are left alone here. They say the rows
#' could not be placed, which the ordered path reports as `NA` and the paths
#' that do not depend on order have no reason to raise.
#' @param kx,ky Two `.source_key` columns.
#' @return `TRUE` when the keys show one source filtered two ways.
#' @keywords internal
.kept_different_rows_of_one_source <- function(kx, ky) {
  !is.null(kx) && !is.null(ky) &&
    !identical(.sorted_keys(kx), .sorted_keys(ky)) &&
    identical(.source_digests(kx), .source_digests(ky))
}

#' Do two frames hold the same rows within each arm, in any order?
#'
#' The grouped survival units sum their pointwise columns inside a group
#' before anything is compared, so the order of the rows inside a group is
#' not part of the comparison and requiring it rejects work that is valid.
#' What still has to hold is that each group is made of the same rows: the
#' groups themselves must match, and within each one the rows must agree as a
#' multiset, so a row moved from one arm to another, or replaced, is still a
#' different comparator.
#'
#' Each group's rows are put in one canonical order and then compared as the
#' values they are. Rendering them as text would have been simpler and wrong:
#' `format()` prints to the display precision, so two survival times that
#' differ below `getOption("digits")` would render alike and two different
#' comparators would be approved. Ordering is exact on doubles, so nothing is
#' rounded on the way in, and rows that tie on every shared column are
#' interchangeable by construction. The source keys take no part in the order,
#' which is the very thing being allowed; [.same_observations()] still asks
#' them, after this, whether the two frames kept different rows of one source.
#' @param group Split the rows by `.arm` before comparing, which is what a
#'   per-arm unit needs. `FALSE` compares the whole frame as one multiset,
#'   for a criterion whose value does not depend on the order at all.
#' @keywords internal
.same_grouped_rows <- function(x, y, shared, group = TRUE) {
  if (nrow(x) != nrow(y)) return(FALSE)
  if (!nrow(x)) return(TRUE)
  # A non-empty label on purpose: `split()` keeps `""` as a name and
  # `list[[""]]` matches nothing, so every group would have come back empty
  # and every comparison would have passed.
  arm <- function(df) {
    if (group && ".arm" %in% names(df)) {
      paste0("arm:", as.character(df$.arm))
    } else {
      rep("all", nrow(df))
    }
  }
  gx <- split(seq_len(nrow(x)), arm(x))
  gy <- split(seq_len(nrow(y)), arm(y))
  if (!identical(sort(names(gx)), sort(names(gy)))) return(FALSE)
  # Byte order, so the canonical order does not depend on the locale.
  canonical <- function(df, idx) {
    sub <- df[idx, shared, drop = FALSE]
    ord <- do.call(order, c(unname(as.list(sub)), list(method = "radix")))
    sub <- sub[ord, , drop = FALSE]
    rownames(sub) <- NULL
    sub
  }
  for (g in names(gx)) {
    if (length(gx[[g]]) != length(gy[[g]])) return(FALSE)
    if (!identical(canonical(x, gx[[g]]), canonical(y, gy[[g]]))) return(FALSE)
  }
  TRUE
}

#' Refuse to compare fits that were not built on the same observations
#' @keywords internal
.assert_same_observations <- function(models, survival_unit = "observation",
                                      unordered = FALSE) {
  # The grouped survival units sum a group's pointwise columns before the
  # comparison, so the comparator's rows have to be the same rows in the same
  # groups but not in the same order. The index rows stay positional under
  # every unit.
  pseudo_grouped <- !identical(survival_unit, "observation")
  frames <- lapply(models, function(m) {
    if (inherits(m, "mlumr_fit")) {
      .observation_frames(m)
    } else if (inherits(m, "mlumr_dic")) {
      m$observations
    } else {
      NULL
    }
  })
  known <- Filter(Negate(is.null), frames)
  # Every pair, not each against the first: the relation is not transitive,
  # since two fits may share a covariate that a third does not carry.
  unverified <- FALSE
  for (i in seq_along(known)) {
    for (j in seq_len(i - 1L)) {
      same <- .same_observations(known[[j]], known[[i]],
                                 pseudo_grouped, unordered)
      if (isFALSE(same)) {
        stop("The compared fits were not built on the same observations in ",
             "the same order: their stored data differ in the columns that ",
             "define an observation or in a covariate they share, or hold the ",
             "rows of one source in a different order, or kept different rows ",
             "of one source. Pointwise criteria are ",
             "paired, column by column, so a comparison across different data, ",
             "or the same data in a different order, is not a comparison of ",
             "the models. Refit on one common data set.", call. = FALSE)
      }
      if (is.na(same)) unverified <- TRUE
    }
  }
  if (unverified) {
    warning("The compared fits agree on every observation column and every ",
            "covariate they share, but whether their rows are in the same ",
            "order could not be verified: they were built from sources that ",
            "differ in columns the models did not use, or one of them predates ",
            "the row keys. Pointwise criteria pair rows by position, so the ",
            "comparison assumes the order is the same. Build both fits from ",
            "one data frame to have it checked.", call. = FALSE)
  }
  unknown <- length(frames) - length(known)
  if (unknown > 0L) {
    message("Could not verify that the compared models share the same ",
            "observations: ", unknown, " of them carr",
            if (unknown == 1L) "ies" else "y",
            " no data to check. The comparison assumes they do.")
  }
  invisible(length(known))
}


#' Ordered pointwise log-likelihood columns for one data source
#' @keywords internal
.ordered_log_lik_columns <- function(draws, source) {
  pattern <- sprintf("^log_lik_%s\\[[0-9]+\\]$", source)
  cols <- grep(pattern, colnames(draws), value = TRUE)
  if (length(cols) == 0L) {
    return(character())
  }
  cols[order(.log_lik_column_index(cols, source))]
}


#' Extract integer indexes from Stan vector column names
#' @keywords internal
.log_lik_column_index <- function(cols, source) {
  pattern <- sprintf("^log_lik_%s\\[|\\]$", source)
  as.integer(gsub(pattern, "", cols))
}


#' Validate a pointwise log-likelihood matrix
#' @keywords internal
.validate_log_lik_matrix <- function(log_lik) {
  if (!is.matrix(log_lik) || !is.numeric(log_lik)) {
    stop("Pointwise log-likelihood must be a numeric matrix.", call. = FALSE)
  }
  if (nrow(log_lik) == 0L || ncol(log_lik) == 0L) {
    stop("Pointwise log-likelihood matrix must be non-empty.", call. = FALSE)
  }
  if (anyNA(log_lik) || any(!is.finite(log_lik))) {
    stop("Pointwise log-likelihood values must be finite.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Calculate DIC for model comparison
#'
#' Computes the Deviance Information Criterion using the variance-based
#' effective-parameters formula from Gelman et al. (2004): `pD = 0.5 * Var(D)`.
#' This is more stable than the plug-in alternative for multimodal posteriors.
#'
#' DIC is retained for backward compatibility and rough comparison. For
#' principled Bayesian model comparison, prefer [calculate_loo()] or
#' [calculate_waic()] (Vehtari, Gelman, Gabry 2017).
#'
#' @param object An `mlumr_fit` object
#'
#' @return A list of class `mlumr_dic` with components `DIC`, `pD`, `D_bar`,
#'   `n_obs`, `model`, and `observations`, the fit's observation frames as
#'   kept for [compare_models()], so a DIC object can still be checked against
#'   the fits it is compared with.
#' @export
#'
#' @examples
#' \dontrun{
#' dic_spfa <- calculate_dic(fit_spfa)
#' dic_relaxed <- calculate_dic(fit_relaxed)
#' }
calculate_dic <- function(object) {

  log_lik <- extract_log_lik(object)
  if (nrow(log_lik) < 2L) {
    stop("DIC requires at least two posterior draws.", call. = FALSE)
  }
  log_lik_total <- rowSums(log_lik)
  D <- -2 * log_lik_total
  D_bar <- mean(D)
  # Effective number of parameters (Gelman et al. 2004, variance-based):
  # pD = 0.5 * Var(D). More stable than the plug-in alternative (D_bar - D_hat)
  # when the posterior is multimodal.
  pD <- 0.5 * var(D)
  DIC <- D_bar + pD

  out <- list(
    DIC = DIC,
    pD = pD,
    D_bar = D_bar,
    n_obs = ncol(log_lik),
    model = .mlumr_model_label(object),
    observations = .observation_frames(object)
  )

  class(out) <- "mlumr_dic"
  out
}


#' @method print mlumr_dic
#' @export
print.mlumr_dic <- function(x, ...) {
  cat("DIC for ML-UMR Model\n")
  cat("====================\n\n")
  cat("Model:", x$model, "\n")
  cat("DIC:", round(x$DIC, 2), "\n")
  cat("pD:", round(x$pD, 2), "\n")
  cat("D_bar:", round(x$D_bar, 2), "\n")
  invisible(x)
}


#' Calculate LOO-CV for an mlumr_fit
#'
#' Computes approximate leave-one-out cross-validation (PSIS-LOO, Vehtari,
#' Gelman, Gabry 2017) using the pointwise log-likelihoods stored by the
#' Stan models. Returns a `loo` object from the `loo` package.
#'
#' Pareto-k diagnostics: values > 0.7 indicate observations for which the
#' PSIS approximation is unreliable; the printed output flags these.
#' Typical remedies are running more iterations, using `moment_match = TRUE`,
#' or (for highly influential AgD rows) refitting without the offending
#' observation to check sensitivity.
#'
#' @note
#' **AgD rows are treated as independent observations.** Each AgD row
#' contributes one column to the pointwise `log_lik` matrix. If two or
#' more AgD rows come from the same study (e.g. subgroup summaries
#' within a single trial) the PSIS-LOO approximation does not account
#' for the within-study clustering; effective sample sizes are
#' inflated and Pareto-k warnings are understated. For clustered AgD,
#' corroborate with [prior_sensitivity()] or refit omitting suspect
#' rows to check the influence on the posterior.
#'
#' **Survival fits.** The comparator AgD enters as reconstructed pseudo-IPD, so
#' each AgD pointwise unit is a single reconstructed pseudo-individual, not an
#' aggregate row or the comparator trial. Survival LOO/WAIC therefore measure
#' pseudo-individual-level predictive fit and are optimistic relative to
#' leaving out the comparator arm/trial; treat them as a rough check, not a
#' decisive model-selection criterion. Set `survival_unit = "arm"` or
#' `"aggregate"` to instead hold out whole comparator arms / the external
#' evidence as single units.
#'
#' @param object An `mlumr_fit` object.
#' @param survival_unit For survival fits, the LOO/WAIC pointwise unit:
#'   `"observation"` (default; per reconstructed comparator pseudo-individual,
#'   optimistic), `"arm"` (group the comparator pseudo-IPD by comparator arm, so
#'   each external arm is one held-out unit), or `"aggregate"` (all comparator
#'   pseudo-IPD as a single external-evidence unit). The index IPD always stays
#'   per-individual. Ignored for non-survival families.
#' @param ... Additional arguments passed to [loo::loo()] (the `log_lik` matrix
#'   dispatches to `loo::loo.matrix()`).
#'
#' @return An object of class `psis_loo` (see [loo::loo()]).
#' @export
#' @examples
#' \dontrun{
#' loo_spfa <- calculate_loo(fit_spfa)
#' print(loo_spfa)
#' }
calculate_loo <- function(object,
                          survival_unit = c("observation", "arm", "aggregate"),
                          ...) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for calculate_loo(). ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  survival_unit <- match.arg(survival_unit)
  log_lik <- .survival_log_lik_by_unit(object, survival_unit)
  r_eff <- .relative_eff_from_log_lik(log_lik, .chain_id(object))
  loo::loo(log_lik, r_eff = r_eff, ...)
}


#' Warn that survival LOO/WAIC pointwise units are reconstructed pseudo-IPD
#'
#' For survival fits `log_lik_agd` is per reconstructed pseudo-individual, so
#' LOO/WAIC operate at that level rather than per aggregate row or per trial.
#' Emitted once per session (suppress with
#' `options(mlumr.quiet_survival_loo = TRUE)`).
#' @keywords internal
.warn_survival_loo_unit <- function(object) {
  if (!identical(object$family, "survival")) return(invisible())
  if (isTRUE(getOption("mlumr.quiet_survival_loo", FALSE))) return(invisible())
  if (isTRUE(getOption("mlumr.survival_loo_warned", FALSE))) return(invisible())
  warning(
    "Survival LOO/WAIC: the comparator AgD enters as reconstructed pseudo-IPD, ",
    "so each AgD pointwise unit is one pseudo-individual (not an aggregate row ",
    "or the comparator trial). These criteria measure pseudo-individual-level ",
    "fit and are optimistic relative to leaving out the comparator arm; use ",
    "them as a rough check, not a decisive model-selection criterion. Suppress ",
    "with `options(mlumr.quiet_survival_loo = TRUE)`.",
    call. = FALSE
  )
  options(mlumr.survival_loo_warned = TRUE)
  invisible()
}


#' Pointwise log-likelihood for LOO/WAIC, optionally grouped for survival
#'
#' At the default `survival_unit = "observation"` this is `extract_log_lik()`
#' (per-observation; per reconstructed pseudo-individual for survival AgD) and
#' emits the pseudo-IPD-level warning. For survival fits, `"arm"` collapses the
#' comparator pseudo-IPD log-likelihood by comparator arm (summing within arm,
#' exact under conditional independence given the parameters) and `"aggregate"`
#' collapses all comparator pseudo-IPD into one external-evidence unit; the index
#' IPD stays per-individual. Leaving out a grouped unit then corresponds to
#' leaving out that whole external arm / the whole external evidence, which is
#' the question grouped LOO/WAIC answer (and is not optimistic at the
#' pseudo-individual level). Non-survival families ignore the option.
#' @keywords internal
.survival_log_lik_by_unit <- function(object, survival_unit = "observation") {
  .assert_agd_loglik_per_observation(object)
  if (!identical(object$family, "survival") ||
        identical(survival_unit, "observation")) {
    .warn_survival_loo_unit(object)
    return(extract_log_lik(object))
  }

  draws <- object$draws
  if (is.null(draws) || is.null(colnames(draws))) {
    stop("`object$draws` must contain named posterior draw columns.",
         call. = FALSE)
  }
  ipd_cols <- .ordered_log_lik_columns(draws, "ipd")
  agd_cols <- .ordered_log_lik_columns(draws, "agd")
  if (length(agd_cols) == 0L) {
    stop("Grouped survival LOO/WAIC needs AgD pointwise log-likelihood ",
         "columns (`log_lik_agd`).", call. = FALSE)
  }
  agd_mat <- as.matrix(draws[, agd_cols, drop = FALSE])

  groups <- if (identical(survival_unit, "aggregate")) {
    rep(1L, ncol(agd_mat))
  } else {
    g <- object$stan_data$agd_arm
    if (is.null(g) || length(g) != ncol(agd_mat)) {
      stop("Cannot group survival AgD by arm: the per-pseudo-individual arm ",
           "map (`stan_data$agd_arm`) is missing or the wrong length.",
           call. = FALSE)
    }
    as.integer(g)
  }

  # Sum each group's pseudo-individual log-likelihoods into one column: under
  # conditional independence given the parameters this is the log predictive
  # density of the whole arm / external-evidence unit.
  grouped <- vapply(sort(unique(groups)), function(gg) {
    rowSums(agd_mat[, groups == gg, drop = FALSE])
  }, numeric(nrow(agd_mat)))
  grouped <- matrix(grouped, nrow = nrow(agd_mat))

  ipd_mat <- if (length(ipd_cols) > 0L) {
    as.matrix(draws[, ipd_cols, drop = FALSE])
  } else {
    NULL
  }
  out <- if (is.null(ipd_mat)) grouped else cbind(ipd_mat, grouped)
  .validate_log_lik_matrix(out)
  out
}


#' Calculate WAIC for an mlumr_fit
#'
#' Watanabe-Akaike Information Criterion (Watanabe 2010) based on the
#' pointwise log-likelihoods. WAIC is asymptotically equivalent to
#' LOO-CV; prefer [calculate_loo()] when Pareto-k is well-behaved.
#'
#' @note
#' As with [calculate_loo()], each AgD row is treated as an
#' independent observation. WAIC will be optimistic for AgD rows
#' that share a study (see the note on `calculate_loo()`). For
#' **survival** fits the AgD pointwise unit is a reconstructed
#' pseudo-individual, so WAIC is at the pseudo-individual level
#' (see the survival note on [calculate_loo()]).
#'
#' @param object An `mlumr_fit` object.
#' @param survival_unit For survival fits, the WAIC pointwise unit:
#'   `"observation"` (default), `"arm"`, or `"aggregate"` (see [calculate_loo()]
#'   for details). Ignored for non-survival families.
#' @param ... Additional arguments passed to [loo::waic()].
#'
#' @return An object of class `waic` (see [loo::waic()]).
#' @export
#' @examples
#' \dontrun{
#' waic_spfa <- calculate_waic(fit_spfa)
#' }
calculate_waic <- function(object,
                           survival_unit = c("observation", "arm", "aggregate"),
                           ...) {
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for calculate_waic(). ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  survival_unit <- match.arg(survival_unit)
  log_lik <- .survival_log_lik_by_unit(object, survival_unit)
  loo::waic(log_lik, ...)
}


#' Compare fitted ML-UMR models
#'
#' Compare two or more `mlumr_fit` objects by DIC (default), LOO, or WAIC.
#' For LOO/WAIC, [loo::loo_compare()] is used under the hood; the output
#' is the standard `loo_compare` table. For DIC the return is a data
#' frame ordered by DIC.
#'
#' DIC is the default for backward compatibility and because it has no
#' additional package dependencies. For principled Bayesian model
#' comparison, LOO (Vehtari, Gelman, Gabry 2017) is preferred and
#' requires the optional `loo` package.
#'
#' @param ... Two or more `mlumr_fit` objects. For DIC, `mlumr_dic`
#'   objects are also accepted.
#' @param criterion One of `"dic"` (default), `"loo"`, or `"waic"`.
#'   LOO and WAIC require the optional `loo` package.
#' @param survival_unit For survival fits compared by `"loo"`/`"waic"`, the
#'   pointwise unit forwarded to [calculate_loo()] / [calculate_waic()]:
#'   `"observation"` (default; per reconstructed comparator pseudo-individual,
#'   optimistic), `"arm"`, or `"aggregate"`. Choose `"arm"` or `"aggregate"` to
#'   select on whole-external-arm predictive fit. Ignored for non-survival
#'   families and for `criterion = "dic"`.
#'
#' @return For `"loo"` / `"waic"`: a `compare.loo` table from
#'   [loo::loo_compare()]. For `"dic"`: a data frame (invisibly) with
#'   columns `Model`, `DIC`, `pD`, `Delta_DIC`.
#' @export
compare_models <- function(..., criterion = c("dic", "loo", "waic"),
                           survival_unit = c("observation", "arm", "aggregate")) {

  criterion <- .validate_diagnostic_choice(criterion, c("dic", "loo", "waic"),
                                           "criterion")
  survival_unit <- match.arg(survival_unit)
  models <- list(...)
  .validate_model_count(models)
  # DIC sums each model's pointwise log-likelihood over observations before
  # taking its mean and variance, so its value does not change under any
  # permutation of them and there is nothing paired to line up. Only the LOO
  # and WAIC standard error of the difference is formed column by column, so
  # only it needs the rows in one order.
  .assert_same_observations(
    models,
    if (criterion == "dic") "observation" else survival_unit,
    unordered = identical(criterion, "dic")
  )

  if (criterion == "dic") {
    dics <- lapply(models, function(m) {
      if (inherits(m, "mlumr_fit")) {
        calculate_dic(m)
      } else if (inherits(m, "mlumr_dic")) {
        m
      } else {
        stop("For criterion = 'dic', arguments must be mlumr_fit or mlumr_dic objects",
             call. = FALSE)
      }
    })

    model_names <- vapply(dics, function(d) d$model, character(1))
    dic_vals <- vapply(dics, function(d) d$DIC, numeric(1))
    pD_vals <- vapply(dics, function(d) d$pD, numeric(1))
    # DIC is only comparable across fits of the same observation set. Unlike the
    # LOO/WAIC path (where loo::loo_compare checks pointwise compatibility),
    # nothing here would otherwise catch ranking models built from different
    # numbers of observations.
    n_obs_vals <- vapply(dics, function(d) d$n_obs %||% NA_integer_, integer(1))
    if (length(unique(stats::na.omit(n_obs_vals))) > 1L) {
      msg <- paste0(
        "Comparing DIC across fits with different observation counts (%s). DIC ",
        "is only comparable on a common data set; this ranking is not meaningful."
      )
      warning(sprintf(msg, paste(n_obs_vals, collapse = ", ")), call. = FALSE)
    }
    model_names <- .comparison_names(models, model_names)

    out <- data.frame(
      Model = model_names,
      DIC = round(dic_vals, 2),
      pD = round(pD_vals, 2),
      Delta_DIC = round(dic_vals - min(dic_vals), 2)
    )

    out <- out[order(out$DIC), ]
    rownames(out) <- NULL

    cat("\nModel Comparison (DIC)\n")
    cat("======================\n\n")
    print(out, row.names = FALSE)
    cat("\nLower DIC = better fit. Delta_DIC > 5 is a rough heuristic for\n")
    cat("meaningful difference, not a formally calibrated threshold.\n")
    cat("DIC should not be the sole basis for model selection.\n")

    return(invisible(out))
  }

  # LOO / WAIC path
  if (!requireNamespace("loo", quietly = TRUE)) {
    stop("The 'loo' package is required for LOO/WAIC comparison. ",
         "Install with install.packages('loo').", call. = FALSE)
  }
  if (!all(vapply(models, inherits, logical(1), what = "mlumr_fit"))) {
    stop("For criterion = 'loo' or 'waic', all arguments must be mlumr_fit objects",
         call. = FALSE)
  }

  calc_fn <- if (criterion == "loo") calculate_loo else calculate_waic
  # Forward the survival LOO/WAIC pointwise unit so survival model selection is
  # not silently hardwired to the optimistic per-pseudo-individual default.
  # Ignored by the calculators for non-survival families.
  ic_list <- lapply(models, function(m) calc_fn(m, survival_unit = survival_unit))
  names(ic_list) <- .comparison_names(
    models,
    vapply(models, .mlumr_model_label, character(1))
  )

  cat(sprintf("\nModel Comparison (%s)\n", toupper(criterion)))
  cat(strrep("=", 22), "\n\n", sep = "")
  cmp <- loo::loo_compare(ic_list)
  print(cmp)
  cat(.model_comparison_interpretation(), sep = "\n")
  cat("\n")
  invisible(cmp)
}


# Helper: recover chain ids for each draw.
# Prefer the real per-draw chain labels stored by the backend (`object$chain_ids`,
# cmdstanr's actual `.chain` column or rstan's chain-major layout). Only when
# those are unavailable do we reconstruct from row ordering: both backends store
# draws in chain-major order (post-warmup iterations per chain contiguous). If
# even the chain count is unavailable we fall back to all-one (a single chain,
# which inflates r_eff but does not produce incorrect elpd).
.chain_id <- function(object) {
  n_draws <- nrow(object$draws)
  if (!is.numeric(n_draws) || length(n_draws) != 1L || n_draws < 1L) {
    return(integer())
  }
  # Authoritative path: real chain labels captured at fit time.
  stored <- object$chain_ids
  if (!is.null(stored) && is.numeric(stored) && length(stored) == n_draws &&
        all(is.finite(stored))) {
    return(as.integer(stored))
  }
  chains <- object$sampling_args$chains %||% 1L
  valid_chains <- is.numeric(chains) &&
    length(chains) == 1L &&
    is.finite(chains) &&
    chains >= 1 &&
    chains == as.integer(chains)
  if (!valid_chains) {
    return(rep(1L, n_draws))
  }
  chains <- as.integer(chains)
  iter_per_chain <- n_draws %/% chains
  if (iter_per_chain * chains != n_draws) return(rep(1L, n_draws))
  rep(seq_len(chains), each = iter_per_chain)
}


#' Compute stable relative effective sample sizes from log-likelihoods
#' @keywords internal
.relative_eff_from_log_lik <- function(log_lik, chain_id) {
  .validate_log_lik_matrix(log_lik)
  col_max <- apply(log_lik, 2L, max)
  stabilized <- sweep(log_lik, 2L, col_max, "-")
  loo::relative_eff(exp(stabilized), chain_id = chain_id)
}


#' Validate a diagnostics/model-comparison choice
#' @keywords internal
.validate_diagnostic_choice <- function(x, choices, name) {
  if (identical(x, choices)) {
    return(choices[[1L]])
  }

  valid <- is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    nzchar(x) &&
    x %in% choices

  if (!valid) {
    stop(sprintf("`%s` must be one of: %s.",
                 name, paste(sprintf("'%s'", choices), collapse = ", ")),
         call. = FALSE)
  }

  x
}


#' Validate that model comparison has at least two candidates
#' @keywords internal
.validate_model_count <- function(models) {
  if (length(models) < 2L) {
    stop("compare_models() requires at least two models.", call. = FALSE)
  }
  invisible(TRUE)
}


#' Human-readable model label for an mlumr fit or DIC object
#' @keywords internal
.mlumr_model_label <- function(object) {
  if (inherits(object, "mlumr_dic") && !is.null(object$model)) {
    return(as.character(object$model)[[1L]])
  }

  model <- object$model %||% NA_character_
  if (!is.character(model) || length(model) != 1L || is.na(model)) {
    return("ML-UMR")
  }

  switch(model,
    spfa = "SPFA",
    relaxed = "Relaxed SPFA",
    model
  )
}


#' Use user-supplied comparison names when available
#' @keywords internal
.comparison_names <- function(models, fallback) {
  user_names <- names(models)
  out <- fallback
  if (!is.null(user_names)) {
    use_user_name <- nzchar(user_names)
    out[use_user_name] <- user_names[use_user_name]
  }
  # Two unnamed fits of the same model type would otherwise both be labeled e.g.
  # "SPFA", making the comparison rows unattributable. Disambiguate duplicates.
  make.unique(out, sep = " #")
}


#' Check MCMC diagnostics and warn if issues found
#' @param fit An `mlumr_fit` object
#' @keywords internal
check_diagnostics <- function(fit) {

  .validate_mlumr_fit_object(fit)
  diag <- fit$diagnostics %||% list()
  sampling_args <- fit$sampling_args %||% list()

  # A chain that terminates abnormally is dropped by both backends, which then
  # assemble the fit from the survivors. Everything downstream (posterior
  # summaries, Rhat, effect estimates) is then computed from fewer chains than
  # were requested, with no other signal that it happened. Report it first,
  # because it invalidates the convergence checks below rather than adding to
  # them.
  n_req <- .diagnostic_count(diag$n_chains_requested)
  n_got <- .diagnostic_count(diag$n_chains_returned)
  # `.diagnostic_count()` maps an unusable value to 0, so an unknown layout and
  # a known-good one both leave `n_got` outside the comparison below. Separate
  # them: the backend reports `NA` when it could not label the draws by chain,
  # and that is a finding, not a pass.
  if (n_req > 0 && !isTRUE(n_got > 0) &&
        (is.null(diag$n_chains_returned) ||
           any(is.na(diag$n_chains_returned)))) {
    warning(paste0(
      "The draws could not be labeled by chain, so it is not known whether ",
      "all ", n_req, " requested chain(s) returned. Rhat and the effective ",
      "sample sizes below are computed from the draws as stored. Refit, or ",
      "inspect the backend fit object, before reporting these results."
    ), call. = FALSE)
  }
  if (n_req > 0 && n_got > 0 && n_got < n_req) {
    warning(sprintf(
      paste0("Only %d of %d requested chain(s) returned; the remaining chain(s) ",
             "terminated abnormally. The posterior, Rhat, and every effect ",
             "estimate below are based on the surviving chain(s) only, and Rhat ",
             "is not meaningful from a single chain. Do not report these results ",
             "without refitting successfully."),
      n_got, n_req
    ), call. = FALSE)
  }

  n_divergent <- .diagnostic_count(diag$n_divergent)
  if (n_divergent > 0) {
    warning(sprintf(
      "%d divergent transitions detected. Consider increasing adapt_delta (currently %s).",
      n_divergent, .diagnostic_value(sampling_args$adapt_delta)
    ), call. = FALSE)
  }

  n_max_treedepth <- .diagnostic_count(diag$n_max_treedepth)
  if (n_max_treedepth > 0) {
    warning(sprintf(
      "%d iterations hit max treedepth. Consider increasing max_treedepth (currently %s).",
      n_max_treedepth, .diagnostic_value(sampling_args$max_treedepth)
    ), call. = FALSE)
  }

  rhat_vals <- .finite_numeric_values(fit$summary$Rhat)
  if (length(rhat_vals) > 0L) {
    max_rhat <- max(rhat_vals)
    if (max_rhat > 1.05) {
      warning(sprintf(
        "Some Rhat values > 1.05 (max = %.3f). Chains have likely not converged.",
        max_rhat
      ), call. = FALSE)
    } else if (max_rhat > 1.01) {
      warning(sprintf(
        "Some Rhat values > 1.01 (max = %.3f). Chains may not have fully converged.",
        max_rhat
      ), call. = FALSE)
    }
  }

  ess_vals <- .finite_numeric_values(fit$summary$n_eff)
  if (length(ess_vals) > 0L) {
    min_ess <- min(ess_vals)
    if (min_ess < 400) {
      warning(sprintf(
        "Some ESS values < 400 (min = %.1f). Consider running more iterations.",
        min_ess
      ), call. = FALSE)
    }
  }

  # Tail ESS (Vehtari et al. 2021): reliable tail quantiles (the q2.5/q97.5
  # reported by predict()/marginal_effects()) need ESS-tail >= 400 too, which the
  # bulk n_eff above does not capture. Both backends supply the column when the
  # `posterior` package is installed; rstan's classic summary does not report it,
  # so the rstan backend computes it from the draws. Report an absent or
  # all-missing column instead of passing silently, which is indistinguishable
  # from a clean check.
  #
  # `posterior::ess_tail()` returns NA for several distinct reasons: a chain
  # layout it cannot use, any non-finite draw, and a parameter that is constant
  # across every chain. The backend does not record which, so name the outcome
  # and not a cause, and count the parameters that lack a value instead of
  # dropping them: a partly-missing column would otherwise be checked on its
  # finite entries alone and read as clean.
  tail_all <- if ("ess_tail" %in% names(fit$summary)) {
    fit$summary$ess_tail
  } else {
    numeric(0)
  }
  tail_vals <- .finite_numeric_values(tail_all)
  n_missing <- length(tail_all) - length(tail_vals)
  if (length(tail_vals) == 0L) {
    hint <- if (!requireNamespace("posterior", quietly = TRUE)) {
      " Install the 'posterior' package to enable this check."
    } else {
      ""
    }
    message("Tail ESS is unavailable for this fit, so tail quantiles were not ",
            "checked.", hint)
  } else {
    if (n_missing > 0L) {
      message(sprintf(
        paste0("Tail ESS is unavailable for %d of %d parameter(s), which were ",
               "not checked; the remaining %d were."),
        n_missing, length(tail_all), length(tail_vals)
      ))
    }
    if (min(tail_vals) < 400) {
      msg <- paste0(
        "Some tail-ESS values < 400 (min = %.1f). Tail quantiles ",
        "(e.g. 2.5%%/97.5%%) may be unreliable; run more iterations."
      )
      warning(sprintf(msg, min(tail_vals)), call. = FALSE)
    }
  }

  invisible(NULL)
}


#' Read a non-negative scalar diagnostic count
#' @keywords internal
.diagnostic_count <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0) {
    return(0)
  }
  as.integer(x)
}


#' Format a diagnostic setting for warning messages
#' @keywords internal
.diagnostic_value <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return("unknown")
  }
  as.character(x)
}


#' Return finite numeric values from a summary column
#' @keywords internal
.finite_numeric_values <- function(x) {
  if (!is.numeric(x)) {
    return(numeric())
  }
  x[is.finite(x)]
}


#' The interpretation paragraph the LOO/WAIC comparison prints
#'
#' Kept callable so a test can assert what users actually see rather than
#' matching source text, which is brittle and, worse, matches comments about
#' the wording as readily as the wording itself.
#'
#' The standard error of a difference measures UNCERTAINTY about that
#' difference, not support for it. An earlier version of this paragraph
#' presented a large `se_diff` as the threshold for a meaningful difference,
#' under which an `elpd_diff` of 0.1 alongside an `se_diff` of 3 would have
#' qualified as persuasive. It is the difference read against its own
#' uncertainty that carries information, and even that is a heuristic.
#'
#' @return A character vector, one element per printed line.
#' @keywords internal
.model_comparison_interpretation <- function() {
  c("",
    "elpd_diff is the difference in expected log pointwise predictive",
    "density vs the best model, and se_diff is its standard error: the",
    "uncertainty about that difference, not evidence for it. Read the two",
    "together. A difference small relative to se_diff is not distinguished",
    "from zero by this comparison, whatever se_diff itself is.",
    "Treat any ratio as a heuristic, not a decision rule, and check the",
    "PSIS diagnostics and whether the difference matters for the",
    "prediction you care about.")
}
