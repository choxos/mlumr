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
#' Identifying every comparator coefficient is sufficient for identifying the
#' index-population estimand, not necessary. Under an identity link that
#' estimand is `mu_c + m_ipd' beta_c`, one linear functional of the comparator
#' parameters, and the aggregate rows pin down every functional in their row
#' space. A single row whose covariate means equal the IPD means identifies it
#' exactly while separating neither the intercept nor the slope.
#' `target_in_span` reports that case, so a coefficient verdict is not read as
#' one about the estimand.
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
#'   subgroup mean matrix), `diagnostic_scope`, `target_in_span`,
#'   and `flagged`: `TRUE` for too
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
  # The report is headed "relaxed model" and diagnoses `beta_comparator`. An
  # SPFA fit shares one coefficient vector across treatments, so it has no
  # comparator-only coefficients and the geometry below says nothing about it.
  if (is_fit && !identical(x$model, "relaxed")) {
    stop("check_identification() diagnoses the comparator coefficients of a ",
         "relaxed fit. This fit used model = \"", x$model, "\", which shares ",
         "one coefficient vector across treatments and so has no ",
         "comparator-only coefficients to identify. Pass the mlumr_data ",
         "object if you want the aggregate design geometry on its own.",
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
  out$target_in_span <- if (out$diagnostic_scope == "identity") {
    .target_in_span(means, colMeans(as.matrix(data$ipd$data[, covs,
                                                            drop = FALSE])),
                    ref_sd)
  } else {
    NA
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
# public `<covariate>_mean` columns, and as a cross-check: the likelihood sees
# the integration points, not the declared columns, and nothing forces a
# hand-written `distr()` to reproduce them.
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
      # Declared means are preferred because they do not move with the
      # integration resolution. They are only worth preferring while they
      # describe the design being fitted: a `distr()` that ignores the columns
      # it was meant to read (a fixed `distr(qnorm, mean = 0, sd = 1)` on every
      # row) leaves declared profiles that span directions the likelihood does
      # not have.
      ipd_cov <- data$ipd$data[, covs, drop = FALSE]
      ref_sd <- apply(as.matrix(ipd_cov), 2L, stats::sd)
      if (!.realized_matches_declared(means, realized, ref_sd)) {
        warning("The integration distributions do not reproduce the declared ",
                "aggregate covariate means: the realized profiles offer much ",
                "less spread than the `<covariate>_mean` columns claim. ",
                "Reporting the realized geometry, which is what the likelihood ",
                "sees. Check that each `distr()` reads its row's summaries.",
                call. = FALSE)
        return(realized)
      }
      return(means)
    }
  }
  if (is.null(realized)) {
    stop("Aggregate covariate means are unavailable. set_agd() normalizes every ",
         "`cov_means` column to `<covariate>_mean`, so this object predates that ",
         "or was built by hand; run add_integration() and the realized ",
         "integration means will be used instead.", call. = FALSE)
  }
  realized
}


#' Mean covariate profile actually realized by each row's integration points
#' @keywords internal
.agd_realized_profiles <- function(data, covs) {
  x_int <- data$integration_points
  if (is.null(x_int)) return(NULL)
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
  # Fail closed on a design that cannot be decomposed, exactly as
  # `.profile_rank()` does. Both are handed the same matrix, and a legacy
  # integration-mean matrix carrying NA reaches them both, so a hard LAPACK
  # error here ("infinite or missing values in 'x'") against a quiet zero
  # there would be the two screens disagreeing about one design. Zero spread
  # is the conservative reading: no direction the likelihood can use.
  if (!all(is.finite(M))) return(degenerate)
  d <- tryCatch(svd(M)$d, error = function(e) NULL)
  if (is.null(d)) return(degenerate)
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


#' Does the target covariate profile lie in the aggregate row space?
#'
#' Identifying every comparator coefficient is sufficient for identifying the
#' index-population estimand, not necessary. Under an identity link that
#' estimand is `mu_c + m_ipd' beta_c`, one linear functional of the comparator
#' parameters, and the aggregate rows pin down every functional in their row
#' space. One aggregate row whose covariate means happen to equal the IPD means
#' identifies it exactly while separating neither the intercept nor the slope.
#'
#' @param profiles Aggregate subgroup mean matrix, rows by covariates.
#' @param target The target population's covariate means.
#' @param ref_sd Reference SD per covariate, to condition the comparison.
#' @return `TRUE`, `FALSE`, or `NA` when it cannot be determined.
#' @keywords internal
.target_in_span <- function(profiles, target, ref_sd) {
  A <- as.matrix(profiles)
  b <- as.numeric(target)
  if (!nrow(A) || length(b) != ncol(A)) return(NA)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  A <- cbind(1, sweep(A, 2, ref_sd, "/"))
  b <- c(1, b / ref_sd)
  if (any(!is.finite(A)) || any(!is.finite(b))) return(NA)
  # b is a linear combination of the rows of A exactly when t(A) w = b is
  # solvable. Take the least-squares solution and read the residual: a
  # rank-deficient system gives NA coefficients, which contribute nothing.
  qr_at <- qr(t(A))
  w <- qr.coef(qr_at, b)
  w[is.na(w)] <- 0
  resid <- b - drop(t(A) %*% w)
  max(abs(resid)) <= 1e-8 * max(1, max(abs(b)))
}


#' Numerical rank of an aggregate design, on a scale that can be judged
#'
#' `qr()` calls a column negligible relative to the norms it is handed, so an
#' uncentered covariate sitting on a large offset collapses:
#' `qr(cbind(1, 1e7 + c(0, 1, 2)))$rank` is 1, although the design the model
#' fits, with covariates centered by default, is plainly rank 2. Centering and
#' scaling first asks the question about the design being fitted.
#'
#' The scale has to come from OUTSIDE the profiles. Dividing each column by its
#' own root-mean-square, which this did, stretches any separation back to unit
#' size: aggregate means of `c(-1e-10, 1e-10)` became `c(-1, 1)` and the design
#' was reported full rank, so the identity-link relaxed-model screen in
#' [mlumr()] never fired on a comparator the likelihood cannot separate. The
#' IPD standard deviations are an absolute scale and are what
#' `.subgroup_geometry()` already uses, so the two diagnostics now agree.
#'
#' `qr()` cannot supply that judgment on its own either. LINPACK's `dqrdc2`
#' compares each column's remaining norm against that SAME column's original
#' norm, so a covariate separated by `1e-11` IPD SDs is still "independent" of
#' the intercept and counts toward the rank. Directions are therefore counted
#' by singular value against an absolute floor, the `spread` that
#' [check_identification()] already screens on, so the two agree by
#' construction.
#'
#' @param profiles Aggregate subgroup mean matrix, rows by covariates.
#' @param ref_sd Reference SD per covariate (the IPD SDs), used to put the
#'   profile separations on a scale that can be judged. Non-finite or
#'   non-positive entries fall back to 1.
#' @param min_spread Smallest RMS profile separation along a direction, in IPD
#'   standard deviations, that counts as a direction. Defaults to the value
#'   [check_identification()] screens on.
#' @return Integer rank, or `0` when the design cannot be decomposed.
#' @keywords internal
.profile_rank <- function(profiles, ref_sd, min_spread = 0.05) {
  M <- scale(as.matrix(profiles), center = TRUE, scale = FALSE)
  ref_sd <- as.numeric(ref_sd)
  ref_sd[!is.finite(ref_sd) | ref_sd <= 0] <- 1
  M <- sweep(M, 2L, ref_sd, "/")
  # Fail closed. A design that cannot be decomposed, a legacy integration-mean
  # matrix carrying NA for instance, used to fall back to the aggregate row
  # count, which is exactly the quantity this rank replaced: a padded table
  # then looked full rank and suppressed the warning it should have raised.
  if (!all(is.finite(M))) return(0L)
  d <- tryCatch(svd(M)$d, error = function(e) NULL)
  if (is.null(d) || !length(d) || any(!is.finite(d))) return(0L)
  # `d / sqrt(nrow)` is the RMS distance of the profiles from their center
  # along a direction, in IPD SDs: the same quantity `.subgroup_geometry()`
  # reports as `spread`.
  n_directions <- sum(d / sqrt(nrow(M)) >= min_spread)
  # Centering removed the mean, so the intercept is always one more direction.
  as.integer(n_directions) + 1L
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
  # The likelihood integrates over a row's points, so it sees the multiset of
  # tuples and not their order. Sort each row's tuples into a canonical order
  # before comparing; comparing the grids as stored counted two orderings of one
  # grid as two constraints when they carry one. `%.17g` round-trips a double
  # exactly, so equal grids always produce equal keys.
  keys <- vapply(seq_len(n), function(i) {
    grid <- x_int[i, , , drop = FALSE]
    dim(grid) <- dim(x_int)[2:3]
    ord <- do.call(order, as.data.frame(grid))
    paste(sprintf("%.17g", grid[ord, , drop = FALSE]), collapse = "\r")
  }, character(1))
  length(unique(keys))
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

  if (isTRUE(x$flagged) && isTRUE(x$target_in_span)) {
    cat("\nThe index-population estimand is nonetheless identified. Under an ",
        "identity link it is one linear functional of the comparator ",
        "parameters, and the target covariate profile lies in the row space ",
        "of the aggregate design, so the rows pin it down even where they do ",
        "not separate the coefficients individually. Statements about ",
        "individual coefficients remain prior-driven.\n", sep = "")
  } else if (isTRUE(x$flagged)) {
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


#' Does the realized integration design reproduce the declared one?
#'
#' Projects the realized centered profiles onto the DECLARED design's principal
#' directions and asks whether each one still carries its share of the spread
#' the declared design claimed there. Each covariate is first scaled by the
#' spread the declared design claims for it, so the comparison is unit-free.
#'
#' This replaces a comparison of ranks. A rank drop is the extreme case of a
#' collapsed direction, so this test subsumes it, and it also catches the case
#' the rank test could not see: declared means `c(-1, 1)` and realized means
#' `c(-1e-10, 1e-10)` both have rank 2, yet the likelihood has almost no
#' leverage along that direction and the reported geometry described a design
#' that was not fitted. The `factor` is far above quadrature noise, which moves
#' a singular value by a relative `O(1 / n_int)`.
#'
#' Comparing the two singular-value SPECTRA is not enough, because singular
#' values arrive sorted and carry no direction. Declared spread in covariate 1
#' and realized spread of the same size in covariate 2 produce identical
#' spectra, so a spectrum test would report a match while the likelihood sees a
#' different covariate entirely. Projecting onto the declared directions is
#' what makes the comparison directional.
#'
#' The comparison uses the same absolute scale as [check_identification()] and
#' `.profile_rank()`, not only a relative one. A purely relative test disagrees
#' with them in a window: declared means `c(-0.08, 0.08)` against realized
#' `c(-0.04, 0.04)` retain exactly half their spread, so a relative test passes
#' and the declared profiles are reported, while the grid the likelihood
#' actually integrates over sits at 0.04 IPD SDs, below the `0.05` floor those
#' two screens use, and is the unidentified design they exist to catch. A
#' direction must therefore keep BOTH its share of the declared spread and its
#' standing above the floor.
#'
#' Declared directions already below the floor are skipped: they carry no
#' separation the likelihood can use either way, so requiring the grid to
#' reproduce them would flag noise.
#'
#' The projection is measured two ways, because neither alone suffices and each
#' covers the other's blind spot.
#'
#' Per-axis LENGTHS are directional: they pair the k-th declared direction with
#' the realized energy on that same direction, so they catch a grid that keeps
#' its total spread but relocates it onto a different declared axis. They are
#' not a rank. A grid collapsed onto a diagonal of two declared axes still has
#' a long component on each of them separately, so lengths alone accept a grid
#' spanning one direction where the declared design spans two; two `distr()`
#' calls keyed off the same margin do exactly that, and in a jointly defined
#' subgroup table it is one copy-and-paste away.
#'
#' SINGULAR VALUES of the projection count the directions actually spanned,
#' which is what `.profile_rank()` screens, so they close that hole. On their
#' own they are looser than the directional test, not stricter: they arrive
#' sorted and carry no direction, so the largest realized combination is judged
#' against the largest declared direction even when its energy sits on another,
#' and a grid retaining 40 percent of the dominant direction passes on surplus
#' it carries elsewhere.
#'
#' Requiring both means a direction must keep its own share of the declared
#' spread AND remain a direction the grid genuinely spans.
#'
#' @param declared Matrix of declared mean profiles (rows are AgD rows).
#' @param realized Matrix of realized integration means, or `NULL`.
#' @param ref_sd Reference SD per covariate (the IPD SDs), to put both designs
#'   on the scale the identification screens use. Non-finite or non-positive
#'   entries fall back to 1.
#' @param factor Smallest share of each declared singular value the realized
#'   design must still provide along that same direction.
#' @param min_spread Absolute floor, in IPD standard deviations, matching
#'   [check_identification()] and `.profile_rank()`.
#' @return `TRUE` when the realized design reproduces the declared one, or when
#'   there is nothing to compare against.
#' @keywords internal
.realized_matches_declared <- function(declared, realized, ref_sd = NULL,
                                       factor = 0.5, min_spread = 0.05) {
  if (is.null(realized)) {
    return(TRUE)
  }
  if (!identical(dim(declared), dim(realized))) {
    # Not a match and not a mismatch: the grid cannot be compared at all. Say
    # so rather than reporting the declared geometry as if it had been checked.
    warning("The realized integration means could not be compared with the ",
            "declared aggregate means, because the two have different shapes. ",
            "The reported geometry describes the declared columns, which have ",
            "not been checked against the design being fitted.", call. = FALSE)
    return(TRUE)
  }
  scale_by <- if (is.null(ref_sd)) {
    # No external scale supplied: fall back to the declared column spreads, so
    # the relative half of the test still means something. The absolute floor
    # is skipped in that case, since there is no scale to judge it on.
    apply(declared, 2L, function(col) {
      s <- diff(range(col))
      if (!is.finite(s) || s <= 0) s <- max(abs(col))
      if (!is.finite(s) || s <= 0) s <- 1
      s
    })
  } else {
    sd_vals <- as.numeric(ref_sd)
    sd_vals[!is.finite(sd_vals) | sd_vals <= 0] <- 1
    sd_vals
  }
  d <- sweep(scale(declared, center = TRUE, scale = FALSE), 2L, scale_by, "/")
  r <- sweep(scale(realized, center = TRUE, scale = FALSE), 2L, scale_by, "/")
  if (!all(is.finite(d)) || !all(is.finite(r))) {
    warning("The realized integration means could not be compared with the ",
            "declared aggregate means, because one of them is not finite. ",
            "The reported geometry describes the declared columns, which have ",
            "not been checked against the design being fitted.", call. = FALSE)
    return(TRUE)
  }
  decomposition <- tryCatch(svd(d), error = function(e) NULL)
  if (is.null(decomposition)) {
    warning("The realized integration means could not be compared with the ",
            "declared aggregate means, because the declared design could not ",
            "be decomposed. The reported geometry describes the declared ",
            "columns, which have not been checked against the design being ",
            "fitted.", call. = FALSE)
    return(TRUE)
  }
  # Length of each design along the declared principal directions. Dividing by
  # sqrt(nrow) gives the RMS profile separation `.subgroup_geometry()` reports
  # as `spread`, which is what the floor is stated in.
  rows <- sqrt(nrow(d))
  declared_spread <- decomposition$d / rows
  counts <- declared_spread >= min_spread
  if (!any(counts)) {
    return(TRUE)
  }
  # Project the realized grid onto the declared directions that count, and
  # measure that projection TWO ways. Neither alone is enough, and each catches
  # what the other misses.
  #
  # Per-axis lengths are directional: they pair the k-th declared direction
  # with the realized energy on THAT direction, so they catch a grid that keeps
  # its total spread but moves it onto a different declared axis. They are not
  # a rank: a grid collapsed onto a diagonal of two declared axes still has a
  # long component on each of them separately, so this test alone passes a grid
  # spanning one direction where the declared design spans two. Two `distr()`
  # calls keyed off the same margin do exactly that.
  #
  # Singular values count the directions actually spanned, which is the
  # quantity `.profile_rank()` screens, so they close that hole. But they
  # arrive sorted and carry no direction, so this test alone is LOOSER than the
  # directional one: the largest realized combination gets judged against the
  # largest declared direction even when its energy sits on another, and a grid
  # keeping 40 percent of the dominant direction passes on the surplus it
  # carries elsewhere.
  #
  # Requiring both means a direction must keep its own share AND still be a
  # direction the grid genuinely spans.
  projected <- r %*% decomposition$v[, counts, drop = FALSE]
  realized_d <- tryCatch(svd(projected)$d, error = function(e) NULL)
  if (is.null(realized_d) || any(!is.finite(realized_d))) {
    return(FALSE)
  }
  declared_spread <- declared_spread[counts]
  # Lengths along each declared direction, in the same RMS units as `spread`.
  realized_axis <- sqrt(colSums(projected^2)) / rows
  # Singular values of the same projection, sorted descending against the
  # declared spreads, which `svd()` already returns sorted.
  realized_sv <- realized_d[seq_along(declared_spread)] / rows
  realized_sv[!is.finite(realized_sv)] <- 0
  keeps_share <- realized_axis >= factor * declared_spread &
    realized_sv >= factor * declared_spread
  clears_floor <- if (is.null(ref_sd)) {
    TRUE
  } else {
    realized_axis >= min_spread & realized_sv >= min_spread
  }
  all(keeps_share) && all(clears_floor)
}
