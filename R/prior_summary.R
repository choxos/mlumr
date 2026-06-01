#' Summary of priors used by a fitted ML-UMR model
#'
#' Print a human-readable summary of every prior that was used to fit an
#' [mlumr()] model, including the effective per-coefficient scales after
#' autoscaling. Mirrors the spirit of `rstanarm::prior_summary()`.
#'
#' @param object An `mlumr_fit` object.
#' @param digits Number of significant digits for numeric values (default 3).
#' @param ... Unused.
#'
#' @return Invisibly returns a list describing the priors; the side effect is
#'   printing a formatted summary.
#' @seealso [prior_sensitivity()] to quantify how much the posterior moves
#'   under alternative `prior_beta` scales; [prior_normal()],
#'   [prior_student_t()], [prior_cauchy()], [prior_exponential()] for the
#'   prior constructors themselves.
#' @export
#' @examples
#' \dontrun{
#' fit <- mlumr(dat)
#' prior_summary(fit)
#' }
prior_summary <- function(object, ...) {
  UseMethod("prior_summary")
}

#' @rdname prior_summary
#' @export
prior_summary.default <- function(object, ...) {
  stop("prior_summary() has no method for class ",
       paste(class(object), collapse = "/"),
       call. = FALSE)
}

#' @rdname prior_summary
#' @method prior_summary mlumr_fit
#' @export
prior_summary.mlumr_fit <- function(object, digits = 3, ...) {

  .validate_mlumr_fit_object(object)
  digits <- .validate_prior_summary_digits(digits)

  priors <- object$priors
  if (is.null(priors)) {
    stop("No prior information stored on this fit (was it fitted with an ",
         "older version of mlumr?).", call. = FALSE)
  }
  if (!is.null(priors$beta_resolved)) {
    priors$beta_resolved <- .validate_resolved_beta_prior(priors$beta_resolved)
  }

  cat("Priors for ML-UMR Fit\n")
  cat("=====================\n\n")

  # Intercepts
  cat("Intercepts (mu_index, mu_comparator):\n")
  cat("  ", .format_prior(priors$intercept, digits = digits), "\n", sep = "")
  .print_default_tag(priors$intercept)
  cat("\n")

  # Beta (regression coefficients)
  cat("Regression coefficients (beta):\n")
  br <- priors$beta_resolved
  if (is.null(br)) {
    # Fallback for older fits: just print the user-specified prior.
    cat(paste0("  ", .format_prior_collection(priors$beta, digits = digits)),
        sep = "\n")
    cat("\n")
  } else {
    # Detect whether the resolved per-coefficient priors are homogeneous.
    means_eq <- length(unique(round(br$mean, 12))) == 1L
    sds_eq   <- length(unique(round(br$sd,   12))) == 1L
    autos_any <- any(br$autoscale)

    family_label <- .resolved_prior_family_label(br$dist)
    broadcast_label <- .resolved_prior_broadcast_label(br, digits)

    if (means_eq && sds_eq && !autos_any) {
      # Broadcast summary
      cat(sprintf("  %s applied to all %d covariate(s)\n",
                  broadcast_label, length(br$mean)))
    } else {
      # Per-coefficient table
      tbl <- data.frame(
        coefficient = br$covariate_names,
        mean = round(br$mean, digits),
        scale = round(br$sd,  digits),
        autoscaled = br$autoscale,
        sd_x = round(br$sd_x, digits),
        stringsAsFactors = FALSE
      )
      cat(sprintf("  Family: %s%s\n", family_label,
                  if (br$dist == 1L) sprintf(" (df = %g)", br$df) else ""))
      print(tbl, row.names = FALSE)
      if (autos_any) {
        cat("  (scale = user_scale / sd_x for autoscaled rows)\n")
      }
    }
  }
  .print_default_tag(priors$beta)
  cat("\n")

  # Comparator regression coefficients (relaxed model only). Stored on the fit
  # by .mlumr_prior_metadata(); printed so a user who set prior_beta_comparator
  # can verify the comparator prior actually used.
  bc <- priors$beta_comparator_resolved
  if (!is.null(bc)) {
    bc <- .validate_resolved_beta_prior(bc)
    cat("Comparator regression coefficients (beta_comparator):\n")
    means_eq <- length(unique(round(bc$mean, 12))) == 1L
    sds_eq <- length(unique(round(bc$sd, 12))) == 1L
    autos_any <- any(bc$autoscale)
    if (means_eq && sds_eq && !autos_any) {
      cat(sprintf("  %s applied to all %d covariate(s)\n",
                  .resolved_prior_broadcast_label(bc, digits), length(bc$mean)))
    } else {
      tbl <- data.frame(
        coefficient = bc$covariate_names,
        mean = round(bc$mean, digits),
        scale = round(bc$sd, digits),
        autoscaled = bc$autoscale,
        sd_x = round(bc$sd_x, digits),
        stringsAsFactors = FALSE
      )
      cat(sprintf("  Family: %s%s\n", .resolved_prior_family_label(bc$dist),
                  if (bc$dist == 1L) sprintf(" (df = %g)", bc$df) else ""))
      print(tbl, row.names = FALSE)
      if (autos_any) {
        cat("  (scale = user_scale / sd_x for autoscaled rows)\n")
      }
    }
    if (!isTRUE(bc$user_specified)) {
      cat("  (defaults to prior_beta above; set prior_beta_comparator to override)\n")
    }
    cat("\n")
  }

  # Sigma (normal family only)
  if (!is.null(priors$sigma)) {
    cat("Residual SD (sigma, half-distribution via <lower=0>):\n")
    cat("  ", .format_prior(priors$sigma, digits = digits), "\n", sep = "")
    .print_default_tag(priors$sigma)
    cat("\n")
  }

  # Auxiliary shape parameter(s) (survival parametric distributions)
  if (!is.null(priors$aux)) {
    cat("Survival auxiliary (aux: shape or scale, half-distribution via <lower=0>):\n")
    cat("  ", .format_prior(priors$aux, digits = digits), "\n", sep = "")
    .print_default_tag(priors$aux)
    cat("\n")
  }

  # M-spline / piecewise-exponential smoothing SD (survival flexible baseline)
  if (!is.null(priors$smooth)) {
    cat("Baseline smoothing SD (sigma_smooth, half-distribution via <lower=0>):\n")
    cat("  ", .format_prior(priors$smooth, digits = digits), "\n", sep = "")
    .print_default_tag(priors$smooth)
    cat("\n")
  }

  invisible(priors)
}


#' @keywords internal
.format_prior <- function(prior, digits = 3) {
  if (is.null(prior$distribution)) {
    return("<missing prior>")
  }
  switch(prior$distribution,
    normal = sprintf("normal(%s, %s)%s",
                     format(prior$mean, digits = digits),
                     format(prior$sd,   digits = digits),
                     if (isTRUE(prior$autoscale)) " [autoscale]" else ""),
    student_t = sprintf("student_t(df = %g, %s, %s)%s",
                        prior$df,
                        format(prior$mean, digits = digits),
                        format(prior$sd,   digits = digits),
                        if (isTRUE(prior$autoscale)) " [autoscale]" else ""),
    exponential = sprintf("exponential(rate = %s)",
                          format(prior$rate, digits = digits)),
    sprintf("%s(...)", prior$distribution)
  )
}

#' @keywords internal
.format_prior_collection <- function(prior, digits = 3) {
  if (is_single_prior(prior) || is.null(prior)) {
    return(.format_prior(prior, digits = digits))
  }
  if (is.list(prior) && all(vapply(prior, is_single_prior, logical(1)))) {
    return(vapply(seq_along(prior), function(i) {
      sprintf("beta[%d]: %s", i, .format_prior(prior[[i]], digits = digits))
    }, character(1)))
  }
  "<missing prior>"
}

#' @keywords internal
.print_default_tag <- function(prior) {
  if (isTRUE(prior$default) && !is.null(prior$version)) {
    cat("  (package default, mlumr ", prior$version, ")\n", sep = "")
  }
}


#' Validate prior_summary digits
#' @keywords internal
.validate_prior_summary_digits <- function(digits) {
  valid <- is.numeric(digits) &&
    length(digits) == 1L &&
    is.finite(digits) &&
    digits >= 1 &&
    digits <= 22 &&
    digits == as.integer(digits)
  if (!valid) {
    stop("`digits` must be a single integer between 1 and 22.",
         call. = FALSE)
  }
  as.integer(digits)
}


#' Validate resolved beta-prior metadata stored on a fit
#' @keywords internal
.validate_resolved_beta_prior <- function(br) {
  required <- c("mean", "sd", "dist", "df", "autoscale", "sd_x",
                "covariate_names")
  missing <- setdiff(required, names(br))
  if (length(missing) > 0L) {
    stop(sprintf("Resolved beta prior metadata is missing: %s.",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }

  n_beta <- length(br$mean)
  valid <- is.numeric(br$mean) &&
    is.numeric(br$sd) &&
    is.logical(br$autoscale) &&
    is.numeric(br$sd_x) &&
    is.character(br$covariate_names) &&
    n_beta > 0L &&
    length(br$sd) == n_beta &&
    length(br$autoscale) == n_beta &&
    length(br$sd_x) == n_beta &&
    length(br$covariate_names) == n_beta &&
    all(is.finite(br$mean)) &&
    all(is.finite(br$sd)) &&
    all(br$sd > 0) &&
    all(is.finite(br$sd_x)) &&
    all(!is.na(br$autoscale)) &&
    all(!is.na(br$covariate_names)) &&
    all(nzchar(br$covariate_names))

  if (!valid) {
    stop("Resolved beta prior metadata is malformed.", call. = FALSE)
  }
  if (!is.numeric(br$dist) || length(br$dist) != 1L ||
        !is.finite(br$dist)) {
    stop("Resolved beta prior distribution code is malformed.", call. = FALSE)
  }
  if (!is.numeric(br$df) || length(br$df) != 1L || !is.finite(br$df)) {
    stop("Resolved beta prior degrees of freedom is malformed.",
         call. = FALSE)
  }

  br$dist <- as.integer(br$dist)
  br
}


#' Label a resolved Stan prior family code
#' @keywords internal
.resolved_prior_family_label <- function(dist) {
  switch(as.character(dist),
    "0" = "normal",
    "1" = "student_t",
    sprintf("dist=%s", dist)
  )
}


#' Format a homogeneous resolved beta prior
#' @keywords internal
.resolved_prior_broadcast_label <- function(br, digits) {
  switch(as.character(br$dist),
    "0" = sprintf("normal(%s, %s)",
                  format(br$mean[[1L]], digits = digits),
                  format(br$sd[[1L]], digits = digits)),
    "1" = sprintf("student_t(df = %g, %s, %s)",
                  br$df,
                  format(br$mean[[1L]], digits = digits),
                  format(br$sd[[1L]], digits = digits)),
    sprintf("dist=%s(...)", br$dist)
  )
}
