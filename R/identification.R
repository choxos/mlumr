#' Can the aggregate data identify the comparator coefficients?
#'
#' In the relaxed model the comparator coefficients `beta_comparator` are
#' informed only by the aggregate likelihood. This function describes how the
#' aggregate subgroup mean profiles span the covariate space.
#'
#' For a normal identity-link model, the subgroup means are the exact design
#' matrix for the aggregate mean and the reported screen is an identification
#' diagnostic. For nonlinear mean models, the integrated response also depends
#' on each row's full covariate distribution. The mean-profile spectrum is then
#' descriptive only: `flagged` is `TRUE` when there are fewer than `K + 1`
#' scalar outcome summaries and `NA` otherwise. Reconstructed survival curves
#' are refused because they contribute repeated event and censoring likelihood
#' terms rather than one scalar outcome summary per row.
#'
#' With `K` covariates there are `K + 1` comparator parameters (the intercept
#' and `K` coefficients), so at least `K + 1` scalar aggregate outcome summaries
#' are necessary. For the identity-link mean, the rows must also differ in every
#' covariate direction.
#'
#' The function summarizes the spread of the subgroup mean vectors. They are
#' centered, divided by each covariate's SD in the IPD (so a covariate measured
#' in large units cannot dominate a proportion by units alone), and decomposed.
#' `cond_inv`, the ratio of the smallest to the largest singular value, goes to
#' 0 as the rows collapse onto a lower-dimensional set. Centering costs one
#' dimension, which is the same `K + 1` fact seen from the other side.
#'
#' The identity-link screen uses `cond_inv < 0.2` and `spread < 0.05` as package
#' heuristics, not validated universal cutoffs. `cond_inv` compares directions
#' with one another, so it cannot see whether any of them carries information:
#' with a single covariate there is one singular value and the ratio is 1 for
#' every nonzero separation, including subgroup means that differ by 1e-12.
#' `spread` supplies the absolute scale it lacks, as the RMS distance of the
#' profiles from their center along the dominant direction, in IPD standard
#' deviations. Neither sees subgroup sample sizes or outcome precision, and a
#' result above both cutoffs does not establish identification. Confirm
#' conclusions with [prior_sensitivity()].
#'
#' This diagnostic concerns `model = "relaxed"` only. Under SPFA both treatments
#' share one coefficient vector, which the IPD identifies, so a single aggregate
#' row is not a problem.
#'
#' @param x An `mlumr_data` object or a fitted `mlumr_fit`.
#' @param verbose Print a readable report (default `TRUE`).
#' @param link Planned link for an unfitted data object. Defaults to the
#'   family default. A fitted object always uses its stored link.
#'
#' @return Invisibly, a list with `n_rows` (aggregate rows), `n_distinct`
#'   (those that do not repeat another's integration grid), `n_cov`,
#'   `n_rows_needed` (`n_cov + 1`), `cond_inv`, `eff_dim` (the participation
#'   ratio of the squared singular-value spectrum, which summarizes how evenly
#'   the spectral variation is spread across directions; it is not a count of
#'   identified coefficients), `spread`, `singular_values`, `means` (the scaled, centered
#'   subgroup mean matrix), `diagnostic_scope`, and `flagged`: `TRUE` for too
#'   few rows, otherwise the identity-link screen result, or `NA` when nonlinear
#'   mean-profile geometry is descriptive only.
#'
#' @seealso [mlumr()] for `model = "relaxed"`; [check_integration()] for the
#'   numerical-integration diagnostic; [prior_sensitivity()] to confirm how far
#'   an estimate moves with the prior.
#' @export
#' @examples
#' \dontrun{
#' dat <- add_integration(combine_data(ipd, agd), n_int = 64, ...)
#' check_identification(dat)
#' }
check_identification <- function(x, verbose = TRUE, link = NULL) {
  is_fit <- inherits(x, "mlumr_fit")
  if (is_fit && !is.null(link)) {
    stop("`link` is determined by the fitted object and cannot be overridden.",
         call. = FALSE)
  }
  data <- if (inherits(x, "mlumr_fit")) x$data else x
  if (!inherits(data, "mlumr_data")) {
    stop("`x` must be an mlumr_data object (from combine_data()) or an ",
         "mlumr_fit.", call. = FALSE)
  }
  # The row geometry below counts scalar constraints, one per aggregate summary.
  # A reconstructed survival curve does not fit that premise: it contributes a
  # likelihood term at every reconstructed event and censoring time, so a single
  # comparator arm can identify more than one function of (mu_c, beta_c) for
  # some models and covariate distributions and fewer than K + 1 for others.
  # Applying the mean-summary count there would report a valid design as
  # unidentified, or the reverse, so refuse rather than mislead.
  family <- data$family %||% "binomial"
  if (identical(family, "survival")) {
    stop("check_identification() implements a subgroup-mean geometry ",
         "diagnostic for the binomial, normal, and poisson families, and is ",
         "not valid for reconstructed survival curves: their repeated event ",
         "and censoring times can identify model-dependent combinations of ",
         "the comparator parameters, so a row count neither bounds nor ",
         "certifies identification. For a relaxed survival fit, inspect the ",
         "coefficient posterior and run prior_sensitivity() instead.",
         call. = FALSE)
  }
  covs <- data$covariates
  n_cov <- length(covs)

  means <- .agd_mean_profiles(data)

  ref_sd <- apply(as.matrix(data$ipd$data[, covs, drop = FALSE]), 2, stats::sd)
  geom <- .subgroup_geometry(means, ref_sd)

  out <- c(list(n_rows = nrow(means), n_cov = n_cov,
                n_rows_needed = n_cov + 1L,
                n_distinct = .agd_distinct_profiles(data)), geom)
  resolved_link <- if (is_fit) x$link else check_link(family, link)$link
  out$diagnostic_scope <- if (family == "normal" && resolved_link == "identity") {
    "identity"
  } else {
    "descriptive"
  }
  out$flagged <- if (out$n_distinct < out$n_rows_needed) {
    TRUE
  } else if (out$diagnostic_scope == "identity") {
    out$cond_inv < 0.2 || out$spread < 0.05
  } else {
    NA
  }

  if (verbose) .print_identification(out, covs)
  invisible(out)
}


# Declared AgD mean profiles define the identity-link aggregate design exactly.
# Realized integration means are retained only for legacy objects without the
# public `<covariate>_mean` columns.
.agd_mean_profiles <- function(data) {
  covs <- data$covariates
  mean_cols <- paste0(covs, "_mean")
  agd <- data$agd$data
  if (all(mean_cols %in% names(agd))) {
    means <- as.matrix(agd[, mean_cols, drop = FALSE])
    storage.mode(means) <- "double"
    if (all(is.finite(means))) {
      colnames(means) <- covs
      return(means)
    }
  }
  x_int <- data$integration_points
  if (is.null(x_int)) {
    stop("Aggregate covariate means are unavailable. set_agd() normalizes every ",
         "`cov_means` column to `<covariate>_mean`, so this object predates that ",
         "or was built by hand; run add_integration() and the realized ",
         "integration means will be used instead.", call. = FALSE)
  }
  means <- apply(x_int, c(1L, 3L), mean)
  means <- matrix(means, nrow = dim(x_int)[1L], ncol = dim(x_int)[3L])
  colnames(means) <- covs
  means
}


#' Singular-value geometry of a set of subgroup mean vectors
#'
#' @param means Matrix of aggregate subgroup covariate means, rows by covariates.
#' @param ref_sd Reference SD per covariate (the IPD SDs), used to put the
#'   columns on a common scale. Scaling by the spread of the MEANS instead would
#'   rescale a covariate whose subgroup means barely move up to the same footing
#'   as one that swings from 0 to 1, hiding the very collapse being measured.
#' @return A list with `cond_inv`, `eff_dim`, `spread`, `singular_values`,
#'   `means`.
#' @keywords internal
.subgroup_geometry <- function(means, ref_sd) {
  M <- as.matrix(means)
  k <- ncol(M)
  # Center and scale before the early return as well, so the returned `means`
  # is the scaled, centered matrix the documentation describes whatever the
  # row count. A single row previously came back on its raw scale.
  M <- scale(M, center = TRUE, scale = FALSE)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2, ref_sd, "/")
  degenerate <- list(cond_inv = 0, eff_dim = 0, spread = 0,
                     singular_values = rep(0, k), means = M)
  if (nrow(M) < 2L) return(degenerate)
  d <- svd(M)$d
  d <- d[is.finite(d)]
  if (!length(d) || max(d) <= 0) return(degenerate)
  if (length(d) < k) d <- c(d, rep(0, k - length(d)))
  # Normalize before the fourth powers. The participation ratio is scale-free
  # by construction, but `sum(d^2)^2` and `sum(d^4)` are not: a covariate whose
  # singular values reach 1e200 overflows both and returns NaN for an entirely
  # ordinary spectrum.
  dn <- d / max(d)
  list(cond_inv = min(d) / max(d),
       eff_dim = sum(dn^2)^2 / sum(dn^4),
       # `cond_inv` compares directions with each other and so cannot see
       # whether ANY of them carries information: with a single covariate there
       # is one singular value, and the ratio is 1 for every nonzero
       # separation, including means that differ by 1e-12. `spread` is the RMS
       # distance of the profiles from their center along the dominant
       # direction, in IPD standard deviations, which is an absolute scale.
       spread = max(d) / sqrt(nrow(M)),
       singular_values = d,
       means = M)
}


#' Numerical rank of an aggregate design, on a scale that can be judged
#'
#' `qr()` calls a column negligible relative to the norms it is handed, so an
#' uncentered covariate sitting on a large offset collapses:
#' `qr(cbind(1, 1e7 + c(0, 1, 2)))$rank` is 1, although the design the model
#' fits, with covariates centered by default, is plainly rank 2. Centering and
#' scaling first asks the question about the design being fitted.
#'
#' @param profiles Aggregate subgroup mean matrix, rows by covariates.
#' @return Integer rank, or `0` when the design cannot be decomposed.
#' @keywords internal
.profile_rank <- function(profiles) {
  M <- scale(as.matrix(profiles), center = TRUE, scale = FALSE)
  sds <- apply(M, 2L, function(z) sqrt(mean(z^2)))
  sds[!is.finite(sds) | sds <= 0] <- 1
  M <- sweep(M, 2L, sds, "/")
  rank <- tryCatch(qr(cbind(1, M))$rank, error = function(e) NA_integer_)
  # Fail closed. A design qr() cannot decompose, a legacy integration-mean
  # matrix carrying NA for instance, used to fall back to the aggregate row
  # count, which is exactly the quantity this rank replaced: a padded table
  # then looked full rank and suppressed the warning it should have raised.
  if (!is.finite(rank)) return(0L)
  max(as.integer(rank), 1L)
}


#' Number of distinct aggregate likelihood profiles
#'
#' Mean-profile rank is the right count only where the mean profile is the
#' design, which is the identity link. Under any other link the integrated
#' response depends on a row's whole covariate distribution, so two rows with
#' equal means but different spreads do contribute different constraints, and
#' collapsing them on their means would understate the evidence.
#'
#' Two rows built from an identical integration grid are a different matter:
#' they are the identical function of the comparator parameters whatever the
#' link, so the second repeats the first's likelihood term and adds no
#' constraint. Counting distinct grids is therefore a valid upper bound where
#' the raw row count is not, and it is what makes a duplicated `set_agd()` row
#' stop suppressing the warning for the nonlinear families too.
#'
#' @param data An `mlumr_data` object.
#' @return Integer count of distinct integration grids, or the row count when
#'   there are no integration points to compare.
#' @keywords internal
.agd_distinct_profiles <- function(data) {
  x_int <- data$integration_points
  n_rows <- nrow(data$agd$data)
  if (is.null(x_int) || length(dim(x_int)) != 3L) return(n_rows)
  n <- dim(x_int)[[1L]]
  if (n < 2L) return(n)
  keep <- rep(TRUE, n)
  for (i in seq_len(n - 1L)) {
    if (!keep[i]) next
    for (j in seq(i + 1L, n)) {
      if (!keep[j]) next
      if (identical(x_int[i, , , drop = TRUE], x_int[j, , , drop = TRUE])) {
        keep[j] <- FALSE
      }
    }
  }
  sum(keep)
}


#' Print the identification report
#' @keywords internal
.print_identification <- function(x, covs) {
  cat("\nComparator identification (relaxed model)\n")
  cat("=========================================\n\n")
  cat(sprintf("Aggregate rows:      %d (%d distinct)\n", x$n_rows,
              x$n_distinct))
  cat(sprintf("Covariates:          %d (%s)\n", x$n_cov,
              paste(covs, collapse = ", ")))
  cat(sprintf("Rows needed (K + 1): %d\n", x$n_rows_needed))
  cat(sprintf("Spectral dimension:  %.2f of %d (eff_dim)\n",
              x$eff_dim, x$n_cov))
  cat(sprintf("Balance (cond_inv):  %.4f\n", x$cond_inv))
  cat(sprintf("Spread (IPD SDs):    %.4g\n\n", x$spread))

  if (x$n_distinct < x$n_rows_needed) {
    cat("WEAK: too few distinct aggregate rows. With ", x$n_cov,
        " covariates the comparator side has ", x$n_rows_needed,
        " unknowns (the intercept and one coefficient each), and ",
        x$n_distinct, " distinct row(s) supply ", x$n_distinct,
        " constraint(s). At least ", x$n_rows_needed - x$n_distinct,
        " more jointly-defined subgroup row(s) are needed before the count is ",
        "even sufficient.\n", sep = "")
    if (x$n_distinct < x$n_rows) {
      cat("Of the ", x$n_rows, " aggregate rows, ", x$n_rows - x$n_distinct,
          " repeat an integration grid already present. A repeated row adds a ",
          "likelihood term identical to one already there, so it adds no ",
          "constraint whatever the link.\n", sep = "")
    }
  } else if (x$diagnostic_scope == "descriptive") {
    cat("DESCRIPTIVE ONLY: for a nonlinear mean model, subgroup means do not ",
        "determine the likelihood geometry because within-row distributions ",
        "also affect the integrated response. The reported spectrum describes ",
        "mean-profile spread but cannot flag or clear identification. Confirm ",
        "with prior_sensitivity().\n", sep = "")
  } else if (x$cond_inv < 0.2) {
    cat("WEAK: enough rows, but they do not vary in every direction. The ",
        "subgroup means are close to lying on a lower-dimensional set. ",
        "This is what happens when subgroups are reported one variable at a ",
        "time, or when cross-tabulated categorical cells all share nearly the ",
        "same mean on a continuous covariate.\n", sep = "")
  } else if (x$spread < 0.05) {
    cat("WEAK: the rows vary in every direction, but hardly at all. The ",
        "subgroup means sit within ", sprintf("%.3g", x$spread), " IPD ",
        "standard deviations of their own center, so every slope rests on a ",
        "lever that short. `cond_inv` cannot see this, because it compares ",
        "the directions with one another rather than with the covariate ",
        "scale.\n", sep = "")
  } else {
    cat("NOT FLAGGED: the row count, the balance of the subgroup means, and ",
        "their spread are all above the exploratory screening values. This ",
        "does not establish identification: neither measure can see subgroup ",
        "sizes, outcome precision, or link curvature; confirm with the ",
        "coefficient posterior and prior_sensitivity().\n", sep = "")
  }

  if (isTRUE(x$flagged)) {
    cat("\nThe index-population relaxed estimand averages `beta_comparator` ",
        "over the IPD covariates, so it depends on exactly the directions that ",
        "are weak here. Do not substitute the comparator population for the ",
        "decision target: report it as a sensitivity estimand, obtain subgroup ",
        "rows jointly defined across covariates, or use `model = \"spfa\"` with ",
        "its shared-coefficient assumption stated. Confirm with ",
        "prior_sensitivity().\n", sep = "")
  }
  invisible(x)
}
