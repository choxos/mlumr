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
#' The identity-link screen uses `cond_inv < 0.2` as a package heuristic, not a
#' validated universal cutoff. It ignores subgroup sample sizes and outcome
#' precision, and a result above the cutoff does not establish identification.
#' Confirm conclusions with [prior_sensitivity()].
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
#' @return Invisibly, a list with `n_rows` (aggregate rows), `n_cov`,
#'   `n_rows_needed` (`n_cov + 1`), `cond_inv`, `eff_dim` (the participation
#'   ratio of the squared singular-value spectrum, which summarizes how evenly
#'   the spectral variation is spread across directions; it is not a count of
#'   identified coefficients), `singular_values`, `means` (the scaled, centered
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
                n_rows_needed = n_cov + 1L), geom)
  resolved_link <- if (is_fit) x$link else check_link(family, link)$link
  out$diagnostic_scope <- if (family == "normal" && resolved_link == "identity") {
    "identity"
  } else {
    "descriptive"
  }
  out$flagged <- if (out$n_rows < out$n_rows_needed) {
    TRUE
  } else if (out$diagnostic_scope == "identity") {
    out$cond_inv < 0.2
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
#' @return A list with `cond_inv`, `eff_dim`, `singular_values`, `means`.
#' @keywords internal
.subgroup_geometry <- function(means, ref_sd) {
  M <- as.matrix(means)
  k <- ncol(M)
  if (nrow(M) < 2L) {
    return(list(cond_inv = 0, eff_dim = 0,
                singular_values = rep(0, k), means = M))
  }
  M <- scale(M, center = TRUE, scale = FALSE)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2, ref_sd, "/")
  d <- svd(M)$d
  d <- d[is.finite(d)]
  if (!length(d) || max(d) <= 0) {
    return(list(cond_inv = 0, eff_dim = 0,
                singular_values = rep(0, k), means = M))
  }
  if (length(d) < k) d <- c(d, rep(0, k - length(d)))
  list(cond_inv = min(d) / max(d),
       eff_dim = sum(d^2)^2 / sum(d^4),
       singular_values = d,
       means = M)
}


#' Print the identification report
#' @keywords internal
.print_identification <- function(x, covs) {
  cat("\nComparator identification (relaxed model)\n")
  cat("=========================================\n\n")
  cat(sprintf("Aggregate rows:      %d\n", x$n_rows))
  cat(sprintf("Covariates:          %d (%s)\n", x$n_cov,
              paste(covs, collapse = ", ")))
  cat(sprintf("Rows needed (K + 1): %d\n", x$n_rows_needed))
  cat(sprintf("Spectral dimension:  %.2f of %d (eff_dim)\n",
              x$eff_dim, x$n_cov))
  cat(sprintf("Spread (cond_inv):   %.4f\n\n", x$cond_inv))

  if (x$n_rows < x$n_rows_needed) {
    cat("WEAK: too few aggregate rows. With ", x$n_cov, " covariates the ",
        "comparator side has ", x$n_rows_needed, " unknowns (the intercept ",
        "and one coefficient each), and ", x$n_rows, " row(s) supply ",
        x$n_rows, " constraint(s). At least ", x$n_rows_needed - x$n_rows,
        " more jointly-defined subgroup row(s) are needed before the count is ",
        "even sufficient.\n", sep = "")
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
  } else {
    cat("NOT FLAGGED: the row count and the spread of the subgroup means are ",
        "both above the exploratory screening values. This does not establish ",
        "identification. `cond_inv` is a relative measure, so it cannot see ",
        "subgroup sizes, outcome precision, or link curvature; confirm with ",
        "the coefficient posterior and prior_sensitivity().\n", sep = "")
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
