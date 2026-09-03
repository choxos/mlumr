#' Add numerical integration points
#'
#' Generate quasi-Monte Carlo integration points using Sobol sequences and a
#' Gaussian copula to account for correlations between covariates in the AgD.
#'
#' @param data An `mlumr_data` object from [combine_data()]
#' @param n_int Number of integration points (default 64; use powers of 2).
#'   More points can improve quasi-Monte Carlo integration of the AgD likelihood
#'   and comparator-population estimands. Increase it when
#'   [check_integration()] shows numerical sensitivity. Wider posterior
#'   intervals alone indicate neither an inadequate grid nor a need for more
#'   points. Larger values cost more sampling time.
#' @param cor Correlation matrix for covariates, on the covariate scale. If
#'   `NULL` (the default) it is estimated from the IPD; see the
#'   correlation-transport note in Details.
#' @param cor_adjust Adjustment method: `"spearman"`, `"pearson"`, or `"none"`
#' @param verbose Logical; if `FALSE`, suppresses progress messages.
#' @param ... Distribution specifications for each covariate using [distr()]
#'
#' @return An `mlumr_data` object with integration points added
#' @export
#'
#' @details
#' **The correlation structure is assumed to transport.** Published aggregate
#' data report marginal covariate summaries (means, SDs, proportions) but never
#' the joint distribution, so comparator within-row dependence cannot be
#' estimated from the AgD. With `cor = NULL` this function estimates one matrix
#' from the **index (IPD)** population and applies it as a common within-row
#' copula to every comparator subgroup. It is not generally the pooled
#' comparator correlation because between-subgroup means also contribute to
#' pooled covariance. That assumption is untestable from the data
#' at hand and is inherited from ML-NMR (Phillippo et al. 2020); it is
#' additional to the shared-prognostic-factor and no-unmeasured-effect-modifier
#' assumptions of the unanchored comparison itself, and it should be stated in
#' any submission that uses these results.
#'
#' The levers are: supply `cor` directly when an external source (a registry, a
#' similar trial, a publication reporting a correlation matrix) gives a better
#' estimate for the comparator population; vary it to check sensitivity; and use
#' [check_integration()] to confirm the realized integration points reproduce
#' the AgD moments and pairwise correlations you intended. Only the marginal
#' moments are pinned by `set_agd()`; the dependence structure is your choice.
#'
#' `cor_adjust` controls how the covariate-scale correlation is mapped onto the
#' Gaussian copula. The Spearman map is exact for continuous monotone margins;
#' the Pearson map is accepted only for Gaussian continuous margins. The
#' binary-binary and continuous-binary corrections are prevalence-independent
#' heuristics, while the true
#' latent-Gaussian correlation for a discrete margin depends on its thresholds.
#' Treat the realized association as close to, not equal to, the target, and
#' check it with [check_integration()], passing the same `cor` matrix so the
#' realized-versus-target deviation is reported. `"none"` passes an explicitly
#' supplied latent Gaussian-copula matrix through unchanged and can be used
#' with any margins.
#'
#' Both corrections branch on continuous versus **binary**, and there is no
#' branch for a nonbinary discrete margin (a count such as Poisson or negative
#' binomial, or an ordered category). Such a covariate is mapped as if it were
#' continuous, which understates the attenuation its discreteness causes; and
#' because many latent Gaussian correlations induce the same observed ranks,
#' there is no unique value to map to in the first place. `add_integration()`
#' warns when it detects one. A finite Sobol grid approximates both marginal
#' moments and dependence. Verify with
#' [check_integration()], which reports the realized correlation and names the
#' scale (`cor_method`) it was measured on.
#'
#' @examples
#' \dontrun{
#' dat <- add_integration(
#'   dat,
#'   n_int = 64,
#'   x1 = distr(qnorm, mean = x1_mean, sd = x1_sd),
#'   x2 = distr(qbern, prob = x2_mean)
#' )
#' }
add_integration <- function(data, n_int = 64, cor = NULL,
                            cor_adjust = NULL, verbose = TRUE, ...) {
  ds <- list(...)
  .validate_integration_args(data, n_int, cor_adjust, verbose, ds)
  .validate_integration_distributions(ds, data)

  # Stan receives integration arrays without dimnames, so the third array
  # dimension must use the same covariate order as the IPD design matrix.
  ds <- ds[data$covariates]
  cov_names <- data$covariates
  n_cov <- length(ds)
  n_int <- as.integer(n_int)

  .warn_integration_size(n_int, n_cov)

  cor_info <- .resolve_integration_cor(
    data = data,
    cov_names = cov_names,
    n_cov = n_cov,
    cor = cor,
    cor_adjust = cor_adjust,
    ds = ds,
    verbose = verbose
  )

  u_cor <- .generate_copula_uniforms(n_int, n_cov, cor_info$copula_cor)
  integration_points <- .transform_integration_points(
    u_cor = u_cor,
    data = data,
    ds = ds,
    cov_names = cov_names,
    n_int = n_int,
    n_cov = n_cov
  )
  .warn_integration_vs_agd_moments(integration_points, data$agd$data, cov_names)

  data <- .attach_integration_points(
    data = data,
    X_int_array = integration_points,
    n_int = n_int,
    cor = cor_info$cor,
    copula_cor = cor_info$copula_cor,
    cor_adjust = cor_info$cor_adjust
  )

  mlumr_message(sprintf("Added %d integration points for AgD", n_int),
                verbose = verbose)
  data
}


#' Validate top-level integration arguments
#' @keywords internal
.validate_integration_args <- function(data, n_int, cor_adjust, verbose, ds) {
  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data()", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  valid_cor_adjust <- c("spearman", "pearson", "none")
  invalid_cor_adjust <- !is.null(cor_adjust) &&
    (!is.character(cor_adjust) ||
       length(cor_adjust) != 1L ||
       is.na(cor_adjust) ||
       !cor_adjust %in% valid_cor_adjust)
  if (invalid_cor_adjust) {
    stop(sprintf("`cor_adjust` must be one of: %s",
                 paste(valid_cor_adjust, collapse = ", ")), call. = FALSE)
  }
  invalid_n_int <- !is.numeric(n_int) || length(n_int) != 1 ||
    !is.finite(n_int) || n_int <= 0 || n_int != floor(n_int)
  if (invalid_n_int) {
    stop("`n_int` should be a positive integer", call. = FALSE)
  }
  if (length(ds) == 0) {
    stop("No covariate distributions specified. Use distr() to specify distributions.",
         call. = FALSE)
  }
  invalid_ds <- any(vapply(ds, function(x) !inherits(x, "mlumr_distr"), logical(1))) ||
    is.null(names(ds)) || any(!nzchar(names(ds)))
  if (invalid_ds) {
    stop("Covariate distributions should be specified as named arguments using distr()",
         call. = FALSE)
  }
  if (anyDuplicated(names(ds)) > 0L) {
    duplicates <- unique(names(ds)[duplicated(names(ds))])
    stop(sprintf("Duplicate distribution specifications for: %s",
                 paste(duplicates, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate integration distributions against the combined data
#' @keywords internal
.validate_integration_distributions <- function(ds, data) {
  cov_names <- names(ds)
  if (anyDuplicated(cov_names) > 0L) {
    duplicates <- unique(cov_names[duplicated(cov_names)])
    stop(sprintf("Duplicate distribution specifications for: %s",
                 paste(duplicates, collapse = ", ")), call. = FALSE)
  }

  if (!all(cov_names %in% data$covariates)) {
    missing <- setdiff(cov_names, data$covariates)
    stop(sprintf("Unknown covariates: %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  if (length(cov_names) != length(data$covariates)) {
    missing <- setdiff(data$covariates, cov_names)
    stop(sprintf("Missing distribution specifications for: %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}


#' Warn when the generated grid contradicts the declared AgD moments
#'
#' `distr()` specifies the *shape* of the comparator covariate distribution; the
#' `set_agd()` mean/SD summaries describe the target population. In the standard
#' workflow each `distr()` references the AgD columns so they agree by
#' construction, but a hand-written distribution (e.g. `distr(qnorm, mean = 0,
#' sd = 1)` while the AgD declares mean 10) integrates the wrong population
#' silently. Flag only gross contradictions so ordinary QMC scatter never
#' false-warns; suppress with `options(mlumr.quiet_integration_moments = TRUE)`.
#' @keywords internal
.warn_integration_vs_agd_moments <- function(X_int_array, agd_data, cov_names) {
  if (isTRUE(getOption("mlumr.quiet_integration_moments", FALSE))) {
    return(invisible())
  }
  if (is.null(agd_data) || is.null(dim(X_int_array))) return(invisible())
  n_agd_rows <- dim(X_int_array)[1]
  issues <- character(0)
  for (j in seq_len(n_agd_rows)) {
    for (i in seq_along(cov_names)) {
      cov <- cov_names[[i]]
      mean_col <- paste0(cov, "_mean")
      if (!mean_col %in% names(agd_data)) next
      declared_mean <- suppressWarnings(as.numeric(agd_data[[mean_col]][j]))
      if (!is.finite(declared_mean)) next
      sd_col <- paste0(cov, "_sd")
      declared_sd <- if (sd_col %in% names(agd_data)) {
        suppressWarnings(as.numeric(agd_data[[sd_col]][j]))
      } else {
        NA_real_
      }
      grid_mean <- mean(X_int_array[j, , i])
      has_sd <- is.finite(declared_sd) && declared_sd > 0
      grid_sd <- if (has_sd) stats::sd(X_int_array[j, , i]) else NA_real_
      # Scale for a "gross contradiction": prefer the declared SD; fall back to
      # a fraction of |declared mean| for SD-less (binary) covariates.
      scale <- if (has_sd) declared_sd else max(0.1 * abs(declared_mean), 1e-6)
      mean_off <- abs(grid_mean - declared_mean) > scale &&
        abs(grid_mean - declared_mean) > 0.25 * abs(declared_mean)
      sd_off <- has_sd && abs(grid_sd - declared_sd) > 0.5 * declared_sd
      if (isTRUE(mean_off) || isTRUE(sd_off)) {
        sd_dec <- if (has_sd) sprintf("%.3g", declared_sd) else "NA"
        sd_grid <- if (has_sd) sprintf("%.3g", grid_sd) else "NA"
        fmt <- "%s (AgD row %d): declared mean=%.3g, sd=%s; grid mean=%.3g, sd=%s"
        one <- sprintf(fmt, cov, j, declared_mean, sd_dec, grid_mean, sd_grid)
        issues <- c(issues, one)
      }
    }
  }
  if (length(issues)) {
    msg <- paste0(
      "The integration grid contradicts the declared AgD moments for:\n  %s\n",
      "The distr() distribution(s) do not reproduce the set_agd() summaries, so ",
      "the comparator population being integrated is not the one the aggregate ",
      "data describe. Check that each distr() references the AgD mean/SD columns. ",
      "Suppress with options(mlumr.quiet_integration_moments = TRUE)."
    )
    warning(sprintf(msg, paste(issues, collapse = "\n  ")), call. = FALSE)
  }
  invisible()
}


#' Warn when integration resolution is low for the covariate dimension
#' @keywords internal
.warn_integration_size <- function(n_int, n_cov) {
  min_recommended <- 2^(n_cov + 4)
  if (n_int < min_recommended) {
    warning(sprintf(
      paste0("n_int = %d may be insufficient for %d covariate(s). ",
             "Package heuristic: %d (= 2^(n_cov+4)). ",
             "Consider increasing n_int or using check_integration() to assess accuracy."),
      n_int, n_cov, min_recommended
    ), call. = FALSE)
  }
  invisible(TRUE)
}


#' Resolve raw and copula correlation matrices for integration
#' @keywords internal
.resolve_integration_cor <- function(data, cov_names, n_cov, cor, cor_adjust,
                                     ds, verbose) {
  if (n_cov == 1L) {
    list(cor = matrix(1), copula_cor = matrix(1), cor_adjust = "none")
  } else {
    if (is.null(cor)) {
      mlumr_message("Computing correlation matrix from IPD...", verbose = verbose)
      if (is.null(cor_adjust)) cor_adjust <- "spearman"
      cor_method <- if (cor_adjust == "none") "pearson" else cor_adjust
      cor <- suppressWarnings(stats::cor(
        data$ipd$data[, cov_names],
        method = cor_method,
        use = "complete.obs"
      ))
      .validate_computed_integration_cor(cor)
    } else {
      cor <- .validate_integration_cor(cor, n_cov, cov_names = cov_names)
      if (is.null(cor_adjust)) cor_adjust <- "pearson"
    }

    dtypes <- do.call(
      get_distribution_type,
      c(ds, list(data = utils::head(data$agd$data)))
    )
    if (identical(cor_adjust, "pearson")) {
      non_gaussian <- dtypes == "continuous" &
        vapply(ds, function(d) !identical(d$qfun_name, "qnorm"), logical(1))
      off_diagonal <- cor
      diag(off_diagonal) <- 0
      affected <- non_gaussian &
        apply(abs(off_diagonal) > sqrt(.Machine$double.eps), 1, any)
      if (any(affected)) {
        stop("`cor_adjust = \"pearson\"` cannot be used with non-Gaussian ",
             "continuous margins: a covariate-scale Pearson correlation is ",
             "not the Gaussian-copula correlation for ",
             paste(cov_names[affected], collapse = ", "), ". Supply a ",
             "Spearman correlation matrix with `cor_adjust = \"spearman\"`, ",
             "use Gaussian margins for an observed Pearson matrix, or supply ",
             "a latent Gaussian-copula matrix with `cor_adjust = \"none\"`.",
             call. = FALSE)
      }
    }
    .warn_discrete_copula(dtypes, cov_names, cor_adjust)
    copula_cor <- .adjust_integration_cor(cor, cor_adjust, dtypes)
    copula_cor <- .ensure_positive_definite_cor(copula_cor, n_cov)

    list(cor = cor, copula_cor = copula_cor, cor_adjust = cor_adjust)
  }
}


#' Warn that the copula correction does not cover nonbinary discrete margins
#'
#' The Spearman and Pearson corrections handle two cases: continuous-continuous
#' (exact) and anything paired with a BINARY margin (prevalence-independent
#' heuristics). A count or ordinal margin (Poisson, negative binomial, an
#' ordered category) is neither. It goes through the continuous branch, where
#' the map is exact only for a continuous margin, and its own discreteness both
#' attenuates the realized correlation and makes the latent Gaussian correlation
#' non-unique, because many latent correlations produce the same set of observed
#' ranks. There is no single correction to apply, so say so rather than let the
#' realized association quietly miss the target.
#'
#' @param dtypes Distribution types from [get_distribution_type()].
#' @param cov_names Covariate names, same order as `dtypes`.
#' @param cor_adjust The adjustment method in force.
#' @return `TRUE` invisibly if a warning was issued, `FALSE` otherwise.
#' @keywords internal
.warn_discrete_copula <- function(dtypes, cov_names, cor_adjust) {
  if (identical(cor_adjust, "none")) return(invisible(FALSE))
  hit <- which(dtypes == "discrete")
  if (!length(hit)) return(invisible(FALSE))
  nms <- if (length(cov_names) == length(dtypes)) cov_names[hit] else hit
  msg <- paste0(
    "Covariate(s) %s have a nonbinary discrete marginal (a count or ordered ",
    "category). The `cor_adjust = \"%s\"` copula correction covers ",
    "continuous margins exactly and binary margins heuristically, but has no ",
    "branch for these: they are mapped as if continuous, so the realized ",
    "pairwise association will fall short of the target and the latent ",
    "correlation is not unique. Check the realized values with ",
    "check_integration(), passing the same `cor`, and treat the target as ",
    "approximate."
  )
  warning(sprintf(msg, paste(nms, collapse = ", "), cor_adjust), call. = FALSE)
  invisible(TRUE)
}


#' Validate a user-supplied integration correlation matrix
#' @keywords internal
.validate_integration_cor <- function(cor, n_cov, cov_names = NULL) {
  cor <- as.matrix(cor)
  if (!is.numeric(cor) || nrow(cor) != n_cov || ncol(cor) != n_cov) {
    stop("Correlation matrix dimensions don't match number of covariates",
         call. = FALSE)
  }
  if (!is.null(cov_names)) {
    row_names <- rownames(cor)
    col_names <- colnames(cor)
    has_dimnames <- !is.null(row_names) || !is.null(col_names)
    if (has_dimnames) {
      dimnames_match <- !is.null(row_names) &&
        !is.null(col_names) &&
        setequal(row_names, cov_names) &&
        setequal(col_names, cov_names)
      if (!dimnames_match) {
        stop("`cor` dimnames must match covariate names when provided",
             call. = FALSE)
      }
      cor <- cor[cov_names, cov_names, drop = FALSE]
    }
  }
  valid <- all(is.finite(cor)) &&
    isSymmetric(cor) &&
    max(abs(diag(cor) - 1)) <= sqrt(.Machine$double.eps) &&
    all(eigen(cor, symmetric = TRUE)$values > 0)

  if (!valid) {
    stop("`cor` must be a valid correlation matrix", call. = FALSE)
  }
  cor
}


#' Validate an IPD-derived correlation matrix before copula adjustment
#' @keywords internal
.validate_computed_integration_cor <- function(cor) {
  if (any(!is.finite(cor))) {
    stop("Computed IPD correlation matrix contains non-finite values. ",
         "Check for constant or missing covariates, or provide `cor` manually.",
         call. = FALSE)
  }
  if (!isSymmetric(cor) ||
        max(abs(diag(cor) - 1)) > sqrt(.Machine$double.eps)) {
    stop("Computed IPD correlation matrix is invalid. Provide `cor` manually.",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Adjust correlations to Gaussian-copula scale
#' @keywords internal
.adjust_integration_cor <- function(cor, cor_adjust, dtypes) {
  if (cor_adjust == "spearman") {
    cor_adjust_spearman(cor, types = dtypes)
  } else if (cor_adjust == "pearson") {
    cor_adjust_pearson(cor, types = dtypes)
  } else {
    cor
  }
}


#' Ensure adjusted integration correlation is positive definite
#' @keywords internal
.ensure_positive_definite_cor <- function(copula_cor, n_cov) {
  eigen_tol <- .Machine$double.eps * max(dim(copula_cor)) * 100
  if (all(eigen(copula_cor, symmetric = TRUE)$values > eigen_tol)) {
    return(copula_cor)
  }
  warning("Adjusted correlation matrix not positive definite; applying nearPD correction.",
          call. = FALSE)
  result <- if (requireNamespace("Matrix", quietly = TRUE)) {
    as.matrix(Matrix::nearPD(copula_cor, corr = TRUE)$mat)
  } else {
    # Fallback (Matrix is normally available via copula's dependencies):
    # eigenvalue-flooring projection to the nearest positive-definite
    # correlation matrix. Clamp negative eigenvalues to a small positive
    # value, reconstruct, then rescale to a unit diagonal.
    ev <- eigen(copula_cor, symmetric = TRUE)
    vals <- pmax(ev$values, eigen_tol)
    m <- ev$vectors %*% diag(vals, nrow = length(vals)) %*% t(ev$vectors)
    d <- sqrt(diag(m))
    m <- m / tcrossprod(d)
    diag(m) <- 1
    m
  }
  # Re-check: both nearPD and the rescaled eigenvalue-flooring can leave the
  # smallest eigenvalue marginally negative (floating point), and rescaling to a
  # unit diagonal can reintroduce a tiny negative eigenvalue. Fail loudly rather
  # than hand a non-positive-definite correlation to the copula sampler.
  if (!all(eigen(result, symmetric = TRUE)$values > eigen_tol)) {
    stop("Could not produce a positive-definite integration correlation matrix ",
         "after nearPD correction. Supply a valid positive-definite `cor` or ",
         "reduce the correlation magnitudes.", call. = FALSE)
  }
  result
}


#' Generate correlated uniform quasi-Monte Carlo points
#' @keywords internal
.generate_copula_uniforms <- function(n_int, n_cov, copula_cor) {
  u <- randtoolbox::sobol(n = n_int, dim = n_cov)
  if (n_cov == 1L) {
    as.matrix(u)
  } else {
    u_cor <- tryCatch({
      cop <- copula::normalCopula(copula::P2p(copula_cor), dim = n_cov,
                                  dispstr = "un")
      copula::cCopula(u, copula = cop, inverse = TRUE)
    }, error = function(e) {
      stop(paste0(
        "Gaussian copula construction failed: ", e$message, "\n",
        "Try using `cor_adjust = \"none\"` or check covariate distributions."
      ), call. = FALSE)
    })

    as.matrix(u_cor)
  }
}


#' Transform uniform integration points to covariate scales
#' @keywords internal
.transform_integration_points <- function(u_cor, data, ds, cov_names, n_int,
                                          n_cov) {
  agd_data <- data$agd$data
  n_agd_rows <- nrow(agd_data)

  X_int_array <- array(NA_real_, dim = c(n_agd_rows, n_int, n_cov))
  dimnames(X_int_array)[[3]] <- cov_names

  for (j in 1:n_agd_rows) {
    row_data <- as.list(agd_data[j, , drop = FALSE])
    for (i in seq_along(cov_names)) {
      cov <- cov_names[i]
      u_i <- as.vector(u_cor[, i])
      X_int_array[j, , i] <- eval_distr(ds[[cov]], u_i, row_data)
    }
  }

  if (any(is.na(X_int_array) | is.infinite(X_int_array) | is.nan(X_int_array))) {
    stop("Invalid integration points generated. Check covariate distribution parameters.",
         call. = FALSE)
  }
  X_int_array
}


#' Attach generated integration points to an mlumr_data object
#' @keywords internal
.attach_integration_points <- function(data, X_int_array, n_int, cor, copula_cor,
                                       cor_adjust) {
  data$integration_points <- X_int_array
  data$n_int <- n_int
  data$int_cor <- cor
  data$int_copula_cor <- copula_cor
  data$int_cor_adjust <- cor_adjust
  data$has_integration <- TRUE
  data
}


#' Expand integration points into a long-format data frame
#'
#' @param data An `mlumr_data` object with integration points
#'
#' @return A data frame with columns for each covariate plus `.int_id` and `.agd_row`
#' @export
unnest_integration <- function(data) {

  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be an mlumr_data object", call. = FALSE)
  }
  if (!data$has_integration) {
    stop("No integration points found. Use add_integration() first.", call. = FALSE)
  }

  X_int <- data$integration_points
  dims <- dim(X_int)
  n_agd <- dims[1]
  n_int <- dims[2]
  cov_names <- dimnames(X_int)[[3]]

  out_list <- vector("list", n_agd)
  for (i in 1:n_agd) {
    mat <- X_int[i, , , drop = FALSE]
    dim(mat) <- dims[2:3]
    colnames(mat) <- cov_names
    df <- as.data.frame(mat)
    df$.int_id <- seq_len(n_int)
    df$.agd_row <- i
    out_list[[i]] <- df
  }

  do.call(rbind, out_list)
}


#' Check integration point adequacy
#'
#' Compare integration results at the current `n_int` against a doubled
#' resolution to assess numerical accuracy. Large discrepancies indicate
#' that `n_int` should be increased. Because the Sobol sequence is nested
#' (the doubled set contains the current set), this current-vs-doubled
#' difference is a convergence heuristic, not an error bound. Agreement between
#' the two grids does not establish accuracy for rare discrete margins or for a
#' final treatment-effect estimand.
#'
#' @param data An `mlumr_data` object with integration points
#' @param ... Distribution specifications (same as passed to
#'   [add_integration()])
#' @param cor Correlation matrix (same as passed to [add_integration()])
#' @param cor_adjust Adjustment method (same as passed to [add_integration()])
#' @param check_joint If `TRUE` (default), also compare pairwise correlation
#'   matrices between the current and doubled `n_int`, and the maximum
#'   per-AgD-row absolute deviation from the user-supplied `cor`. The
#'   pairwise comparison catches cases where marginals converge but joint
#'   dependence structure does not (rare in practice for QMC with sensible
#'   `cor_adjust` but worth flagging when `n_int` is small).
#'
#' @return A list with components `marginals` (the original data frame
#'   returned by previous versions) and, if `check_joint = TRUE`,
#'   `correlations`, a data frame of pairwise covariate correlations at
#'   the current and doubled `n_int` for each AgD row. Printed with a
#'   pass/warn verdict.
#'
#'   The `verdict` component reports `"stable"` / `"close"` when a comparison
#'   was made and met the heuristic, `"review"` when it did not, and
#'   `"unavailable"` when there was nothing finite to compare. A declared
#'   target the AgD does not supply, or a latent Gaussian-copula correlation
#'   (`cor_adjust = "none"`), gives `"unavailable"` rather than a pass.
#' @param verbose Logical; if `FALSE`, suppresses printed diagnostic messages.
#' @export
check_integration <- function(data, ..., cor = NULL, cor_adjust = NULL,
                              check_joint = TRUE, verbose = TRUE) {

  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be an mlumr_data object", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(check_joint) || length(check_joint) != 1L ||
        is.na(check_joint)) {
    stop("`check_joint` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!data$has_integration) {
    stop("No integration points found. Use add_integration() first.", call. = FALSE)
  }

  n_int_orig <- data$n_int
  n_int_double <- n_int_orig * 2

  # Compute stats at current resolution
  X_orig <- data$integration_points
  cov_names <- dimnames(X_orig)[[3]]
  n_agd <- dim(X_orig)[1]

  stats_orig <- .int_stats(X_orig, cov_names, n_agd)

  # Re-run at doubled resolution (temporarily)
  data_copy <- data
  data_copy$has_integration <- FALSE
  # Reuse the same correlation the original integration used unless the caller
  # explicitly overrides it. `data$int_cor` is the resolved input correlation
  # (a user-supplied matrix, or the IPD-computed one); without this `%||%` a
  # custom `cor` from add_integration() would be silently dropped here and the
  # doubled grid regenerated under a recomputed IPD correlation, making the
  # diagnostic compare two different integration setups.
  data_doubled <- suppressMessages(suppressWarnings(
    add_integration(data_copy, n_int = n_int_double, cor = cor %||% data$int_cor,
                    cor_adjust = cor_adjust %||% data$int_cor_adjust,
                    verbose = FALSE, ...)
  ))
  X_double <- data_doubled$integration_points
  stats_double <- .int_stats(X_double, cov_names, n_agd)

  # Compare. The mean denominator includes the covariate's own SD so that a
  # near-zero target mean (e.g. a centered/standardized covariate) does not
  # produce a spuriously huge relative difference and a false "increase n_int"
  # warning; the SD provides a natural, non-degenerate scale.
  rel_diff_mean <- abs(stats_orig$mean - stats_double$mean) /
    (abs(stats_double$mean) + abs(stats_double$sd) + 1e-8)
  rel_diff_sd <- abs(stats_orig$sd - stats_double$sd) /
    (abs(stats_double$sd) + 1e-8)

  agd <- data$agd$data
  # sqrt(m * (1 - m)) is the SD of a Bernoulli margin and of nothing else. It
  # was applied to any covariate whose declared mean happened to land in [0, 1],
  # so a continuous covariate with mean 0.4 and no declared SD was compared
  # against a fabricated target and could be reported as off by a wide margin.
  # Ask the declared distributions what the margin is instead of guessing from
  # the mean.
  dtypes <- do.call(
    get_distribution_type,
    c(list(...), list(data = utils::head(agd)))
  )
  target_mean <- target_sd <- numeric(nrow(stats_orig))
  for (i in seq_len(nrow(stats_orig))) {
    cov <- stats_orig$covariate[i]
    row <- stats_orig$agd_row[i]
    target_mean[i] <- as.numeric(agd[[paste0(cov, "_mean")]][row])
    sd_col <- paste0(cov, "_sd")
    is_binary <- isTRUE(unname(dtypes[cov]) == "binary")
    target_sd[i] <- if (sd_col %in% names(agd)) {
      as.numeric(agd[[sd_col]][row])
    } else if (is_binary && isTRUE(target_mean[i] >= 0 && target_mean[i] <= 1)) {
      sqrt(target_mean[i] * (1 - target_mean[i]))
    } else {
      NA_real_
    }
  }
  target_scale <- abs(target_mean) +
    ifelse(is.finite(target_sd), abs(target_sd), 0) + 1e-8
  target_diff_mean <- abs(stats_orig$mean - target_mean) / target_scale
  target_diff_sd <- abs(stats_orig$sd - target_sd) /
    (abs(target_sd) + 1e-8)

  result <- data.frame(
    covariate = stats_orig$covariate,
    agd_row = stats_orig$agd_row,
    mean_current = round(stats_orig$mean, 6),
    mean_doubled = round(stats_double$mean, 6),
    rel_diff_mean = round(rel_diff_mean, 6),
    mean_target = round(target_mean, 6),
    rel_diff_mean_target = round(target_diff_mean, 6),
    sd_current = round(stats_orig$sd, 6),
    sd_doubled = round(stats_double$sd, 6),
    rel_diff_sd = round(rel_diff_sd, 6),
    sd_target = round(target_sd, 6),
    rel_diff_sd_target = round(target_diff_sd, 6),
    stringsAsFactors = FALSE
  )

  max_diff <- .max_finite(c(rel_diff_mean, rel_diff_sd))
  max_target_diff <- .max_finite(c(target_diff_mean, target_diff_sd))

  if (verbose) {
    cat(sprintf("Integration check: n_int = %d vs %d\n", n_int_orig, n_int_double))
    if (is.na(max_diff)) {
      cat("Resolution heuristic: not available (no finite comparison).\n")
    } else {
      cat(sprintf("Resolution heuristic, max relative difference: %.4f\n", max_diff))
      if (max_diff > 0.05) {
        cat("Warning: >5% marginal relative difference. Increase n_int.\n")
      } else if (max_diff > 0.01) {
        cat("Caution: 1-5% marginal relative difference. Consider increasing n_int.\n")
      } else {
        cat("Resolution stable within the package's 1% heuristic.\n")
      }
    }
    if (is.na(max_target_diff)) {
      cat("Declared-target fidelity: not available; the AgD declares no",
          "comparable moments for these covariates.\n")
    } else {
      cat(sprintf("Declared-target fidelity, max relative difference: %.4f\n",
                  max_target_diff))
      if (max_target_diff > 0.05) {
        cat("Warning: grid moments differ from declared AgD moments by >5%.\n")
      } else if (max_target_diff > 0.01) {
        cat("Caution: grid moments differ from declared AgD moments by 1-5%.\n")
      } else {
        cat("Grid moments agree with declared AgD moments within the package's 1% heuristic.\n")
      }
    }
  }

  # A verdict of "close" must mean something was compared. `max(x, na.rm = TRUE)`
  # on an all-NA vector returns -Inf, which passed every threshold below and
  # certified agreement computed from no data at all.
  out <- list(
    marginals = result,
    verdict = list(
      resolution = .moment_verdict(max_diff, 0.01, "stable"),
      target_moments = .moment_verdict(max_target_diff, 0.01, "close")
    )
  )

  if (isTRUE(check_joint) && length(cov_names) >= 2L) {
    # Compare like with like. When `cor` came from the IPD the default target is
    # a SPEARMAN matrix (`cor_adjust = "spearman"`), so measuring the realized
    # integration points with Pearson would compare two different estimands and
    # could warn (or reassure) for no reason. Carry the method that defined the
    # target into the diagnostic.
    # `data$int_cor` was already put in covariate order when add_integration()
    # validated it, but a matrix passed straight to check_integration() was not.
    # .int_cor_stats() indexes it positionally against `cov_names`, so a
    # caller-supplied matrix whose dimnames run in a different order was read
    # transposed and produced a verdict about the wrong pairs.
    cor_target <- if (is.null(cor)) {
      data$int_cor
    } else {
      .validate_integration_cor(cor, length(cov_names), cov_names = cov_names)
    }
    target_method <- cor_adjust %||% data$int_cor_adjust %||% "pearson"
    # `cor_adjust = "none"` means the supplied matrix IS the latent
    # Gaussian-copula correlation. The integration points realize the margins,
    # so measuring them gives an observed correlation on the covariate scale,
    # which is a different quantity: the two agree only for Gaussian margins.
    # Report the resolution comparison, which is like for like, and withhold
    # the target comparison rather than scoring one estimand against the other.
    latent_target <- identical(target_method, "none")
    if (latent_target) {
      target_method <- "pearson"
      cor_target <- NULL
    }
    cor_result <- .int_cor_stats(X_orig, X_double, cov_names, n_agd,
                                 cor_target = cor_target,
                                 cor_method = target_method)
    max_cor_diff <- .max_finite(cor_result$diff$abs_diff)
    max_target_cor_diff <- if (is.null(cor_target)) NA_real_ else
      .max_finite(cor_result$diff$abs_diff_target)
    if (verbose) {
      if (is.na(max_cor_diff)) {
        cat("Joint resolution: not available (no finite comparison).\n")
      } else {
        cat(sprintf("Joint: max |cor(current) - cor(doubled)|: %.4f\n",
                    max_cor_diff))
        if (max_cor_diff > 0.05) {
          cat("Warning: pairwise correlations differ by > 0.05 between resolutions.\n")
        } else {
          cat("Joint resolution stable within the package's 0.05 heuristic.\n")
        }
      }
      if (latent_target) {
        cat("Target correlation: not compared. `cor_adjust = \"none\"` declares",
            "a latent Gaussian-copula matrix, which the realized covariate-scale",
            "correlation does not estimate.\n")
      } else if (!is.null(cor_target) && !is.na(max_target_cor_diff)) {
        cat(sprintf("Target (%s): max |cor(doubled) - cor_target|: %.4f\n",
                    target_method, max_target_cor_diff))
      } else if (!is.null(cor_target)) {
        cat("Target correlation: not available (no finite comparison).\n")
      }
    }
    if (!is.null(cor_target)) {
      out$verdict$target_correlation <-
        .moment_verdict(max_target_cor_diff, 0.05, "close")
    }
    out$correlations <- cor_result$diff
  }

  invisible(out)
}


#' Pairwise-correlation diagnostics for integration points
#' @keywords internal
.int_cor_stats <- function(X_orig, X_double, cov_names, n_agd, cor_target = NULL,
                           cor_method = "pearson") {
  K <- length(cov_names)
  pairs <- utils::combn(seq_len(K), 2, simplify = FALSE)
  rows <- vector("list", length(pairs) * n_agd)
  idx <- 1L
  for (k in seq_len(n_agd)) {
    Xo <- X_orig[k, , , drop = FALSE]
    Xd <- X_double[k, , , drop = FALSE]
    dim(Xo) <- dim(X_orig)[2:3]
    dim(Xd) <- dim(X_double)[2:3]
    colnames(Xo) <- cov_names
    colnames(Xd) <- cov_names
    # `cor_method` matches however `cor_target` was measured; a binary margin
    # realized on the integration grid has a Spearman correlation that is not
    # its Pearson correlation, so the choice is not cosmetic.
    cor_o <- suppressWarnings(stats::cor(Xo, method = cor_method))
    cor_d <- suppressWarnings(stats::cor(Xd, method = cor_method))
    for (ij in pairs) {
      i <- ij[[1L]]
      j <- ij[[2L]]
      rho_o <- cor_o[i, j]
      rho_d <- cor_d[i, j]
      rho_t <- if (!is.null(cor_target)) cor_target[i, j] else NA_real_
      rows[[idx]] <- data.frame(
        agd_row = k,
        pair = sprintf("%s~%s", cov_names[i], cov_names[j]),
        cor_method = cor_method,
        cor_current = round(rho_o, 4),
        cor_doubled = round(rho_d, 4),
        cor_target = round(rho_t, 4),
        abs_diff = round(abs(rho_o - rho_d), 4),
        abs_diff_target = round(abs(rho_d - rho_t), 4),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  list(diff = do.call(rbind, rows))
}


#' Compute summary statistics for integration points
#' @keywords internal
.int_stats <- function(X_int, cov_names, n_agd) {
  rows <- vector("list", n_agd * length(cov_names))
  idx <- 1
  for (k in seq_len(n_agd)) {
    for (j in seq_along(cov_names)) {
      vals <- X_int[k, , j]
      rows[[idx]] <- data.frame(
        covariate = cov_names[j], agd_row = k,
        mean = mean(vals), sd = sd(vals),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  do.call(rbind, rows)
}


#' Largest finite value, or NA when there is none
#'
#' `max(x, na.rm = TRUE)` returns `-Inf` for an all-missing vector, which then
#' passes every "is it small enough" threshold. Return `NA_real_` instead so a
#' comparison that never happened cannot be reported as agreement.
#' @keywords internal
.max_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else max(x)
}


#' Turn a difference into a verdict, keeping "not measured" distinct from "close"
#' @keywords internal
.moment_verdict <- function(value, threshold, pass) {
  if (is.na(value)) {
    "unavailable"
  } else if (value <= threshold) {
    pass
  } else {
    "review"
  }
}
