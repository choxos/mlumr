#' Add numerical integration points
#'
#' Generate quasi-Monte Carlo integration points using Sobol sequences and a
#' Gaussian copula to account for correlations between covariates in the AgD.
#'
#' @param data An `mlumr_data` object from [combine_data()]
#' @param n_int Number of integration points (default 64, powers of 2 recommended)
#' @param cor Correlation matrix for covariates. If `NULL`, computed from IPD.
#' @param cor_adjust Adjustment method: `"spearman"`, `"pearson"`, or `"none"`
#' @param verbose Logical; if `FALSE`, suppresses progress messages.
#' @param ... Distribution specifications for each covariate using [distr()]
#'
#' @return An `mlumr_data` object with integration points added
#' @export
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


#' Warn when integration resolution is low for the covariate dimension
#' @keywords internal
.warn_integration_size <- function(n_int, n_cov) {
  min_recommended <- 2^(n_cov + 4)
  if (n_int < min_recommended) {
    warning(sprintf(
      paste0("n_int = %d may be insufficient for %d covariate(s). ",
             "Recommended minimum: %d (= 2^(n_cov+4)). ",
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
    copula_cor <- .adjust_integration_cor(cor, cor_adjust, dtypes)
    copula_cor <- .ensure_positive_definite_cor(copula_cor, n_cov)

    list(cor = cor, copula_cor = copula_cor, cor_adjust = cor_adjust)
  }
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
    copula_cor
  } else {
    warning("Adjusted correlation matrix not positive definite; applying nearPD correction.",
            call. = FALSE)
    if (requireNamespace("Matrix", quietly = TRUE)) {
      as.matrix(Matrix::nearPD(copula_cor, corr = TRUE)$mat)
    } else {
      copula_cor <- 0.99 * copula_cor + 0.01 * diag(n_cov)
      diag(copula_cor) <- 1
      copula_cor
    }
  }
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
#' that `n_int` should be increased.
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
#'   `correlations` -- a data frame of pairwise covariate correlations at
#'   the current and doubled `n_int` for each AgD row. Printed with a
#'   pass/warn verdict.
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
  data_doubled <- suppressMessages(suppressWarnings(
    add_integration(data_copy, n_int = n_int_double, cor = cor,
                    cor_adjust = cor_adjust %||% data$int_cor_adjust,
                    verbose = FALSE, ...)
  ))
  X_double <- data_doubled$integration_points
  stats_double <- .int_stats(X_double, cov_names, n_agd)

  # Compare
  rel_diff_mean <- abs(stats_orig$mean - stats_double$mean) /
    (abs(stats_double$mean) + 1e-10)
  rel_diff_sd <- abs(stats_orig$sd - stats_double$sd) /
    (abs(stats_double$sd) + 1e-10)

  result <- data.frame(
    covariate = stats_orig$covariate,
    agd_row = stats_orig$agd_row,
    mean_current = round(stats_orig$mean, 6),
    mean_doubled = round(stats_double$mean, 6),
    rel_diff_mean = round(rel_diff_mean, 6),
    sd_current = round(stats_orig$sd, 6),
    sd_doubled = round(stats_double$sd, 6),
    rel_diff_sd = round(rel_diff_sd, 6),
    stringsAsFactors = FALSE
  )

  max_diff <- max(c(rel_diff_mean, rel_diff_sd), na.rm = TRUE)

  if (verbose) {
    cat(sprintf("Integration check: n_int = %d vs %d\n", n_int_orig, n_int_double))
    cat(sprintf("Marginals -- max relative difference: %.4f\n", max_diff))
    if (max_diff > 0.05) {
      cat("WARN: >5%% marginal relative difference. Increase n_int.\n")
    } else if (max_diff > 0.01) {
      cat("CAUTION: 1-5%% marginal relative difference. Consider increasing n_int.\n")
    } else {
      cat("OK marginals: <1%% relative difference.\n")
    }
  }

  out <- list(marginals = result)

  if (isTRUE(check_joint) && length(cov_names) >= 2L) {
    cor_result <- .int_cor_stats(X_orig, X_double, cov_names, n_agd,
                                 cor_target = cor)
    max_cor_diff <- max(cor_result$diff$abs_diff, na.rm = TRUE)
    if (verbose) {
      cat(sprintf("Joint -- max |cor(current) - cor(doubled)|: %.4f\n",
                  max_cor_diff))
      if (max_cor_diff > 0.05) {
        cat("WARN: pairwise correlations differ by > 0.05 between resolutions.\n")
      } else {
        cat("OK joint: pairwise correlations agree within 0.05.\n")
      }
      if (!is.null(cor)) {
        cat(sprintf("Target -- max |cor(doubled) - cor_target|: %.4f\n",
                    max(cor_result$diff$abs_diff_target, na.rm = TRUE)))
      }
    }
    out$correlations <- cor_result$diff
  }

  invisible(out)
}


#' Pairwise-correlation diagnostics for integration points
#' @keywords internal
.int_cor_stats <- function(X_orig, X_double, cov_names, n_agd, cor_target = NULL) {
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
    cor_o <- stats::cor(Xo)
    cor_d <- stats::cor(Xd)
    for (ij in pairs) {
      i <- ij[[1L]]
      j <- ij[[2L]]
      rho_o <- cor_o[i, j]
      rho_d <- cor_d[i, j]
      rho_t <- if (!is.null(cor_target)) cor_target[i, j] else NA_real_
      rows[[idx]] <- data.frame(
        agd_row = k,
        pair = sprintf("%s~%s", cov_names[i], cov_names[j]),
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
