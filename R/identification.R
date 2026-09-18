#' Can the aggregate data identify the comparator coefficients?
#'
#' In the relaxed model the comparator coefficients `beta_comparator` are
#' informed only by the aggregate rows. With `K` covariates there are `K + 1`
#' comparator parameters, so at least `K + 1` distinct aggregate rows are
#' needed, and under an identity link the rows must also differ in every
#' covariate direction.
#'
#' The subgroup mean profiles are centered, divided by the IPD covariate SDs
#' and decomposed. `cond_inv` is the ratio of the smallest to the largest
#' singular value and goes to 0 as the rows collapse onto a lower-dimensional
#' set. `eff_dim` is the participation ratio of the squared singular values,
#' the number of directions the rows effectively spread along, from 1 to
#' `K`; it is 0 when the rows do not vary or cannot be decomposed. `spread`
#' is the RMS distance of the rows from their center along the
#' dominant direction, in IPD SDs; it supplies the absolute scale `cond_inv`
#' lacks. For a normal identity-link model the subgroup means are the
#' aggregate design and the screen flags `cond_inv < 0.2` or `spread < 0.05`,
#' which are package heuristics. For other links the integrated response also
#' depends on each row's covariate distribution, so the geometry is
#' descriptive only and `flagged` is `NA` unless there are too few rows.
#' Reconstructed survival curves are refused, since a curve is not one scalar
#' summary per row. Neither measure sees subgroup sizes or outcome precision,
#' so confirm any verdict with the coefficient posterior and
#' [prior_sensitivity()]. The subgroup-identification vignette works through
#' the cases.
#'
#' @param x An `mlumr_data` object or a fitted relaxed `mlumr_fit`.
#' @param verbose Print a readable report (default `TRUE`).
#' @param link Planned link for an unfitted data object. Defaults to the
#'   family default. A fitted object always uses its stored link.
#'
#' @return Invisibly, a list with `n_rows`, `n_distinct` (rows that do not
#'   repeat another's integration grid), `n_cov`, `n_rows_needed` (`K + 1`),
#'   `cond_inv`, `eff_dim`, `spread`, `singular_values`, `means` (the scaled,
#'   centered subgroup mean matrix), `diagnostic_scope` (`"identity"` or
#'   `"descriptive"`) and `flagged`.
#'
#' @seealso [mlumr()] for `model = "relaxed"`; [prior_sensitivity()].
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
  if (is_fit && !identical(x$model, "relaxed")) {
    stop("check_identification() diagnoses the comparator coefficients of a ",
         "relaxed fit. This fit used model = \"", x$model, "\", which shares ",
         "one coefficient vector across treatments and so has no ",
         "comparator-only coefficients. Pass the mlumr_data object to see ",
         "the aggregate design geometry on its own.", call. = FALSE)
  }
  data <- if (is_fit) x$data else x
  if (!inherits(data, "mlumr_data")) {
    stop("`x` must be an mlumr_data object (from combine_data()) or an ",
         "mlumr_fit.", call. = FALSE)
  }
  family <- data$family %||% "binomial"
  if (identical(family, "survival")) {
    stop("check_identification() is a subgroup-mean geometry diagnostic for ",
         "the binomial, normal and poisson families and is not valid for ",
         "reconstructed survival curves, whose repeated event and censoring ",
         "times identify model-dependent combinations of the comparator ",
         "parameters. For a relaxed survival fit, inspect the coefficient ",
         "posterior and run prior_sensitivity() instead.", call. = FALSE)
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
    out$cond_inv < 0.2 || !.at_least(out$spread, 0.05)
  } else {
    NA
  }

  if (verbose) .print_identification(out, covs)
  invisible(out)
}


# The declared `<covariate>_mean` columns define the aggregate design and do
# not move with the integration resolution, so they are preferred. The
# realized integration means are the fallback for objects without those
# columns and the check that each `distr()` reads its own row.
.agd_mean_profiles <- function(data) {
  covs <- data$covariates
  mean_cols <- paste0(covs, "_mean")
  agd <- data$agd$data
  realized <- .agd_realized_profiles(data, covs)
  if (all(mean_cols %in% names(agd))) {
    means <- as.matrix(agd[, mean_cols, drop = FALSE])
    storage.mode(means) <- "double"
    if (all(is.finite(means))) {
      colnames(means) <- covs
      ipd_cov <- data$ipd$data[, covs, drop = FALSE]
      ref_sd <- apply(as.matrix(ipd_cov), 2L, stats::sd)
      if (!.realized_matches_declared(means, realized, ref_sd)) {
        warning("The integration distributions do not reproduce the declared ",
                "aggregate covariate means, so the realized integration ",
                "means, which are what the likelihood sees, are reported. ",
                "Check that each `distr()` reads its row's summaries.",
                call. = FALSE)
        return(realized)
      }
      return(means)
    }
  }
  if (is.null(realized)) {
    stop("Aggregate covariate means are unavailable: this object has no ",
         "`<covariate>_mean` columns and no integration points. Run ",
         "add_integration().", call. = FALSE)
  }
  realized
}


#' Mean covariate profile realized by each row's integration points
#' @noRd
.agd_realized_profiles <- function(data, covs) {
  x_int <- data$integration_points
  if (is.null(x_int)) return(NULL)
  means <- apply(x_int, c(1L, 3L), mean)
  means <- matrix(means, nrow = dim(x_int)[1L], ncol = dim(x_int)[3L])
  colnames(means) <- covs
  means
}


#' Singular-value geometry of the subgroup mean profiles
#'
#' Rows are centered and divided by the IPD SDs, so a covariate measured in
#' large units cannot dominate by units alone. Returns `cond_inv` (smallest
#' over largest singular value), `eff_dim` (participation ratio of the squared
#' singular values, the number of directions effectively spanned), `spread`
#' (RMS distance of the rows from their center along the dominant direction,
#' in IPD SDs), `singular_values` and the scaled `means`. A design whose rows
#' do not vary or cannot be decomposed reports zero geometry, `eff_dim`
#' included, as `.profile_rank()` does.
#' @noRd
.subgroup_geometry <- function(means, ref_sd) {
  M <- scale(as.matrix(means), center = TRUE, scale = FALSE)
  k <- ncol(M)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2, ref_sd, "/")
  degenerate <- list(cond_inv = 0, eff_dim = 0, spread = 0,
                     singular_values = rep(0, k), means = M)
  if (nrow(M) < 2L || !all(is.finite(M))) return(degenerate)
  d <- tryCatch(svd(M)$d, error = function(e) NULL)
  if (is.null(d)) return(degenerate)
  d <- d[is.finite(d)]
  if (!length(d) || max(d) <= 0) return(degenerate)
  if (length(d) < k) d <- c(d, rep(0, k - length(d)))
  # The participation ratio is scale-free, but its fourth powers overflow for
  # huge singular values, so normalize first.
  dn <- d / max(d)
  list(cond_inv = min(d) / max(d),
       eff_dim = sum(dn^2)^2 / sum(dn^4),
       spread = max(d) / sqrt(nrow(M)),
       singular_values = d,
       means = M)
}


# Relative slack shared by every comparison of a spread against a threshold in
# this file, so screens that measure one design through different
# decompositions cannot disagree by a few ULPs.
.spread_tol <- 1e-8

.at_least <- function(x, threshold) {
  if (!all(is.finite(threshold))) return(x >= threshold)
  x >= threshold - abs(threshold) * .spread_tol
}


#' Number of directions an aggregate design spreads along, plus the intercept
#'
#' Profiles are centered and divided by the IPD SDs, then the directions whose
#' RMS spread reaches `min_spread` IPD SDs are counted; the floor is the value
#' [check_identification()] screens `spread` on, so the two agree. `qr()` is
#' not used because it judges each column against its own norm: an offset of
#' 1e7 collapses the rank and a separation of 1e-11 still counts. A design
#' that cannot be decomposed returns 0.
#' @noRd
.profile_rank <- function(profiles, ref_sd, min_spread = 0.05) {
  M <- scale(as.matrix(profiles), center = TRUE, scale = FALSE)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2L, ref_sd, "/")
  if (!all(is.finite(M))) return(0L)
  d <- tryCatch(svd(M)$d, error = function(e) NULL)
  if (is.null(d) || !length(d) || any(!is.finite(d))) return(0L)
  as.integer(sum(.at_least(d / sqrt(nrow(M)), min_spread))) + 1L
}


#' Numerical rank of the centered aggregate profile matrix, plus the intercept
#'
#' `.profile_rank()` says how far a design moves; this says whether the
#' directions exist at all, with the usual `max(dim) * eps * max(d)`
#' tolerance. Profiles at -0.01 and 0.01 have a spread below the screen and a
#' numerical rank of 2, and precise aggregate outcomes can still pin the slope
#' down there.
#' @noRd
.profile_numeric_rank <- function(profiles, ref_sd) {
  M <- scale(as.matrix(profiles), center = TRUE, scale = FALSE)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2L, ref_sd, "/")
  if (!all(is.finite(M))) return(0L)
  d <- tryCatch(svd(M)$d, error = function(e) NULL)
  if (is.null(d) || !length(d) || any(!is.finite(d))) return(0L)
  tol <- max(dim(M)) * .Machine$double.eps * max(d)
  as.integer(sum(d > tol)) + 1L
}


#' Number of distinct aggregate likelihood profiles
#'
#' Two rows built from the same integration grid contribute the same
#' likelihood term whatever the link, so the second adds no constraint. Each
#' grid is sorted into a canonical order before comparing, because the
#' likelihood sees the multiset of points and not their order. Returns the row
#' count when there are no integration points.
#' @noRd
.agd_distinct_profiles <- function(data) {
  x_int <- data$integration_points
  n_rows <- nrow(data$agd$data)
  if (is.null(x_int) || length(dim(x_int)) != 3L) return(n_rows)
  n <- dim(x_int)[[1L]]
  if (n < 2L) return(n)
  keys <- vapply(seq_len(n), function(i) {
    grid <- x_int[i, , , drop = FALSE]
    dim(grid) <- dim(x_int)[2:3]
    ord <- do.call(order, as.data.frame(grid))
    paste(sprintf("%.17g", grid[ord, , drop = FALSE]), collapse = "\r")
  }, character(1))
  length(unique(keys))
}


#' Print the identification report
#' @noRd
.print_identification <- function(x, covs) {
  cat("\nComparator identification (relaxed model)\n\n")
  cat(sprintf("Aggregate rows:      %d (%d distinct)\n", x$n_rows,
              x$n_distinct))
  cat(sprintf("Covariates:          %d (%s)\n", x$n_cov,
              paste(covs, collapse = ", ")))
  cat(sprintf("Rows needed (K + 1): %d\n", x$n_rows_needed))
  cat(sprintf("Spectral dimension:  %.2f of %d (eff_dim)\n", x$eff_dim, x$n_cov))
  cat(sprintf("Balance (cond_inv):  %.4f\n", x$cond_inv))
  cat(sprintf("Spread (IPD SDs):    %.4g\n\n", x$spread))
  verdict <- if (x$n_distinct < x$n_rows_needed) {
    sprintf(paste("WEAK: %d distinct aggregate row(s) cannot separate the %d",
                  "comparator parameters; supply jointly defined subgroup",
                  "rows or use model = \"spfa\"."),
            x$n_distinct, x$n_rows_needed)
  } else if (x$diagnostic_scope == "descriptive") {
    paste("DESCRIPTIVE ONLY: under a nonlinear link the subgroup means do not",
          "determine the likelihood geometry, so the spread reported above",
          "neither flags nor clears identification.")
  } else if (x$cond_inv < 0.2) {
    paste("WEAK: the subgroup means lie close to a lower-dimensional set, as",
          "when subgroups are reported one variable at a time, so some",
          "comparator coefficients are barely separated.")
  } else if (!.at_least(x$spread, 0.05)) {
    sprintf(paste("WEAK: the subgroup means sit within %.3g IPD SD of their",
                  "center, so every comparator slope rests on a short lever."),
            x$spread)
  } else {
    paste("NOT FLAGGED: the row count, balance and spread are above the",
          "screening values. This does not establish identification.")
  }
  cat(strwrap(paste(verdict, "Confirm with the coefficient posterior and",
                    "prior_sensitivity()."), width = 78), sep = "\n")
  invisible(x)
}


#' Does the realized integration design reproduce the declared one?
#'
#' Compares each row's realized integration means with its declared
#' `<covariate>_mean` values, in reference SDs. A finite grid misses its
#' declared mean by a few hundredths of an SD, while a `distr()` that ignores
#' its row misses by the whole distance to whatever it was given, so a quarter
#' of an SD separates the two. `TRUE` when there is nothing to compare, or
#' when the two cannot be compared (with a warning).
#'
#' @param declared Matrix of declared mean profiles, rows by covariates.
#' @param realized Matrix of realized integration means, or `NULL`.
#' @param ref_sd Reference SD per covariate; `NULL` falls back to each
#'   declared column's range.
#' @param max_location_gap Largest per-row distance, in reference SDs, that
#'   still counts as a match.
#' @noRd
.realized_matches_declared <- function(declared, realized, ref_sd = NULL,
                                       max_location_gap = 0.25) {
  if (is.null(realized)) return(TRUE)
  if (!identical(dim(declared), dim(realized))) {
    warning("The realized integration means could not be compared with the ",
            "declared aggregate means because the two have different shapes; ",
            "the declared columns are reported unchecked.", call. = FALSE)
    return(TRUE)
  }
  scale_by <- if (is.null(ref_sd)) {
    apply(declared, 2L, function(col) {
      s <- diff(range(col))
      if (!is.finite(s) || s <= 0) s <- max(abs(col))
      if (!is.finite(s) || s <= 0) s <- 1
      s
    })
  } else {
    s <- as.numeric(ref_sd)
    s[!is.finite(s) | s <= 0] <- 1
    s
  }
  gap <- sweep(as.matrix(realized) - as.matrix(declared), 2L, scale_by, "/")
  if (!all(is.finite(gap))) {
    warning("The realized integration means could not be compared with the ",
            "declared aggregate means because one of them is not finite; ",
            "the declared columns are reported unchecked.", call. = FALSE)
    return(TRUE)
  }
  all(sqrt(rowSums(gap^2)) <= max_location_gap)
}
