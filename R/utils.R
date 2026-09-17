# Null-coalescing operator (available in base R >= 4.4.0, but we support >= 4.1.0)
`%||%` <- function(x, y) if (is.null(x)) y else x

# Smallest strictly positive value the Stan models accept for an exposure or
# an aggregate standard error. Their data blocks declare `<lower=1e-12>`, so
# the R validators use the same number and reject it by column name.
.mlumr_min_positive <- 1e-12


#' Specify a marginal distribution
#'
#' Used to specify marginal distributions for covariates when adding integration
#' points. Wraps an inverse CDF (quantile) function with its parameters.
#'
#' @param qfun Inverse CDF function (e.g., `qnorm`, `qbern`)
#' @param ... Parameters of the distribution, can reference column names in AgD
#'
#' @return A list with class `"mlumr_distr"` containing the distribution specification
#' @export
#'
#' @examples
#' # Normal distribution
#' distr(qnorm, mean = 0, sd = 1)
#'
#' # Bernoulli distribution with probability 0.3
#' distr(qbern, prob = 0.3)
distr <- function(qfun, ...) {
  qfun_resolved <- match.fun(qfun)
  qfun_name <- tryCatch(deparse(substitute(qfun)), error = function(e) "user_function")
  # The bare name, so `distr(stats::qpois, ...)` still classifies as a count
  # margin in get_distribution_type().
  qfun_name <- sub("^.*:::?", "", qfun_name)

  # Capture arguments as unevaluated expressions
  args <- as.list(match.call(expand.dots = FALSE))[["..."]]
  # Name positional arguments now: everything downstream reads them by name.
  args <- .name_distr_args(args, qfun_resolved, qfun_name)

  if (!"p" %in% names(formals(qfun_resolved))) {
    stop("`qfun` should be an inverse CDF function with a formal argument `p`",
         call. = FALSE)
  }

  d <- list(
    qfun = qfun_resolved,
    args = args,
    # Where the specification was written, so its arguments can see the
    # variables in scope there as well as the aggregate row.
    envir = parent.frame(),
    qfun_name = qfun_name
  )

  class(d) <- "mlumr_distr"
  d
}

#' Evaluate a mlumr_distr object with data context
#'
#' @param d A `mlumr_distr` object
#' @param p Vector of probabilities
#' @param data A named list or data frame to evaluate expressions in
#' @return Numeric vector of quantiles
#' @keywords internal
eval_distr <- function(d, p, data = list()) {
  # By position: unnamed arguments are passed through in order.
  nms <- names(d$args)
  if (is.null(nms)) {
    nms <- rep("", length(d$args))
  }
  enc <- if (is.environment(d$envir)) d$envir else parent.frame(2)
  vals <- lapply(seq_along(d$args), function(i) {
    label <- if (nzchar(nms[[i]])) nms[[i]] else paste0("[[", i, "]]")
    tryCatch(
      eval(d$args[[i]], envir = data, enclos = enc),
      error = function(e) {
        stop(sprintf(
          "Error evaluating distribution argument '%s': %s\nAvailable data columns: %s",
          label, e$message, paste(names(data), collapse = ", ")
        ), call. = FALSE)
      }
    )
  })
  names(vals) <- nms
  do.call(d$qfun, c(list(p = p), vals))
}


#' Summarize a single draw vector into mean, sd and quantiles
#'
#' Internal helper. Centralizes the (mean, sd, quantile) triplet used by
#' [predict.mlumr_fit()], [marginal_effects()], [conditional_effects()] and
#' [conditional_predict()] so that any later change to the canonical posterior
#' summary only needs to happen in one place.
#'
#' @param x Numeric vector of posterior draws.
#' @param probs Quantile probabilities.
#' @param warn Whether to report dropped draws. Callers that summarize many
#'   vectors set this to `FALSE` and report once over the whole set instead.
#' @return Named numeric vector: `c(mean, sd, <named quantiles>)`.
#' @keywords internal
.summarize_draw_vector <- function(x, probs, warn = TRUE) {
  if (warn) {
    .warn_dropped_draws(x)
  }
  # Package names for the quantiles: R's `format()` names differ from the
  # names the lookups build for a non-round probability.
  c(mean = mean(x, na.rm = TRUE),
    sd   = stats::sd(x, na.rm = TRUE),
    stats::setNames(
      stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE),
      .quantile_names(probs)
    ))
}

#' Summarize a draws matrix column-wise into a tidy data frame
#'
#' Applies `.summarize_draw_vector()` across columns of `draws` and renames
#' the quantile columns to `qNN` form (e.g., `q2.5`, `q50`, `q97.5`).
#'
#' @param draws A numeric matrix or data frame of posterior draws (one column
#'   per quantity, one row per draw).
#' @param probs Quantile probabilities.
#' @param warn Whether to report dropped draws. Set `FALSE` where a missing
#'   draw is an expected outcome with a diagnostic of its own.
#' @return Data frame with columns `mean`, `sd` and one `qNN` column per
#'   element of `probs`.
#' @keywords internal
.summarize_draw_matrix <- function(draws, probs, warn = TRUE) {
  if (warn) {
    .warn_dropped_draws(draws)
  }
  summary_mat <- t(apply(draws, 2, .summarize_draw_vector, probs = probs,
                         warn = FALSE))
  summary_df <- as.data.frame(summary_mat)
  colnames(summary_df) <- c("mean", "sd", .quantile_names(probs))
  summary_df
}


#' Report posterior draws dropped from a summary
#'
#' `.summarize_draw_vector()` drops NA and NaN draws with `na.rm = TRUE`, so a
#' summary built on part of the chain would otherwise read like one built on
#' all of it. An infinite draw propagates into the mean and is not counted.
#'
#' @param draws Numeric vector, matrix or data frame of posterior draws.
#' @return `TRUE` if a warning was issued, `FALSE` otherwise, invisibly.
#' @keywords internal
.warn_dropped_draws <- function(draws) {
  m <- if (is.matrix(draws)) draws else as.matrix(draws)
  if (nrow(m) == 0L || ncol(m) == 0L) {
    return(invisible(FALSE))
  }
  dropped <- colSums(is.na(m))
  if (!any(dropped > 0L)) {
    return(invisible(FALSE))
  }
  n <- nrow(m)
  msg <- if (ncol(m) == 1L) {
    fmt <- paste0("%d of %d posterior draws are NA or NaN and were dropped ",
                  "from the summary, which therefore describes the remaining ",
                  "%d.")
    sprintf(fmt, dropped[[1]], n, n - dropped[[1]])
  } else {
    fmt <- paste0("%d of %d summarized quantities have NA or NaN draws, which ",
                  "were dropped from their summaries; the worst loses %d of ",
                  "%d draws. Those summaries describe the remaining draws ",
                  "only.")
    sprintf(fmt, sum(dropped > 0L), ncol(m), max(dropped), n)
  }
  warning(msg, call. = FALSE)
  invisible(TRUE)
}

#' Validate an mlumr_data object
#' @keywords internal
.validate_mlumr_data_object <- function(data) {
  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data().", call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate a single TRUE or FALSE
#' @keywords internal
.validate_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
  x
}


#' Convert a confidence level to a two-sided normal critical value
#' @keywords internal
.z_from_conf_level <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
        !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single finite number between 0 and 1.",
         call. = FALSE)
  }
  stats::qnorm(1 - (1 - conf_level) / 2)
}


#' Bound a Wald interval to a valid numerical range
#' @keywords internal
.bounded_wald_interval <- function(center, se, z,
                                   lower = -Inf, upper = Inf) {
  .validate_numeric_vector(center, "center")
  .validate_numeric_vector(se, "se")
  .validate_numeric_vector(z, "z")
  if (length(z) != 1L || !is.finite(z) || z < 0) {
    stop("`z` must be a single non-negative finite value.", call. = FALSE)
  }
  if (any(!is.finite(center)) || any(!is.finite(se)) || any(se < 0)) {
    stop("`center` and `se` must be finite, with non-negative `se`.",
         call. = FALSE)
  }
  if (!is.numeric(lower) || !is.numeric(upper) ||
        length(lower) != 1L || length(upper) != 1L ||
        is.na(lower) || is.na(upper) || lower > upper) {
    stop("`lower` and `upper` must define a valid interval.", call. = FALSE)
  }
  list(
    lower = pmax(lower, center - z * se),
    upper = pmin(upper, center + z * se)
  )
}


#' Truncate tiny negative variance estimates caused by numerical noise
#' @keywords internal
.nonnegative_variance <- function(x, name = "variance", tol = 1e-10) {
  .validate_numeric_vector(x, name)
  if (any(!is.finite(x))) {
    stop(sprintf("`%s` must be finite.", name), call. = FALSE)
  }
  if (any(x < -tol)) {
    stop(sprintf("`%s` must be non-negative.", name), call. = FALSE)
  }
  pmax(x, 0)
}


#' Square root of a variance estimate with numerical guarding
#' @keywords internal
.sqrt_variance <- function(x, name = "variance", tol = 1e-10) {
  sqrt(.nonnegative_variance(x, name, tol))
}


# -----------------------------------------------------------------------------
# Bernoulli wrappers (qbern / pbern / dbern)
#
# One-line wrappers around stats::qbinom/pbinom/dbinom with size = 1, as in
# multinma (Phillippo et al., GPL-3, R/integration.R), so `qbern` works in
# `distr()` without multinma attached.
# -----------------------------------------------------------------------------

#' Bernoulli quantile function
#'
#' @param p Vector of probabilities
#' @param prob Success probability
#' @param lower.tail Logical; if TRUE, probabilities are P(X <= x)
#' @param log.p Logical; if TRUE, probabilities are given as log(p)
#'
#' @return Integer vector of 0s and 1s
#' @export
qbern <- function(p, prob, lower.tail = TRUE, log.p = FALSE) {
  qbinom(p, size = 1, prob = prob, lower.tail = lower.tail, log.p = log.p)
}

#' Bernoulli CDF
#'
#' @param q Vector of quantiles
#' @param prob Success probability
#' @param lower.tail Logical
#' @param log.p Logical
#'
#' @return Numeric vector
#' @export
pbern <- function(q, prob, lower.tail = TRUE, log.p = FALSE) {
  pbinom(q, size = 1, prob = prob, lower.tail = lower.tail, log.p = log.p)
}

#' Bernoulli PMF
#'
#' @param x Vector of values
#' @param prob Success probability
#' @param log Logical; if TRUE, return log-density
#'
#' @return Numeric vector
#' @export
dbern <- function(x, prob, log = FALSE) {
  dbinom(x, size = 1, prob = prob, log = log)
}

#' Get distribution type (continuous, discrete, or binary)
#' @param ... distr() objects
#' @param data Sample data for evaluation
#' @return Named character vector
#' @keywords internal
get_distribution_type <- function(..., data = list()) {
  ds <- list(...)
  dnames <- names(ds)

  out <- vector("character", length = length(ds))
  names(out) <- dnames

  # A concentrated logit-normal evaluates to 1 on the probe grid below, so it
  # has to be listed rather than probed.
  known_continuous <- c("qbeta", "qcauchy", "qchisq", "qexp", "qf", "qgamma",
                        "qlnorm", "qlogitnorm", "qnorm", "qt", "qunif",
                        "qweibull")
  known_discrete <- c("qgeom", "qnbinom", "qpois")
  known_binary <- "qbern"

  for (i in seq_along(ds)) {
    di <- ds[[i]]
    if (di$qfun_name %in% known_continuous) {
      out[i] <- "continuous"
    } else if (di$qfun_name %in% known_discrete) {
      out[i] <- "discrete"
    } else if (di$qfun_name %in% known_binary) {
      out[i] <- "binary"
    } else if (di$qfun_name == "qbinom") {
      # Evaluated in the specification's own scope, like every other argument.
      size_val <- eval_distr_arg(di$args$size, data, di$envir)
      out[i] <- if (all(size_val == 1)) "binary" else "discrete"
    } else {
      # Test distribution on a grid
      ps <- 1:99 / 100
      support <- eval_distr(di, ps, data)
      is_int <- all(abs(support - round(support)) < .Machine$double.eps^0.5,
                    na.rm = TRUE)
      if (is_int) {
        out[i] <- if (all(abs(support) < 1.5, na.rm = TRUE)) "binary" else "discrete"
      } else {
        out[i] <- "continuous"
      }
    }
  }

  out
}

#' Evaluate a single mlumr_distr argument expression
#' @param expr An unevaluated expression
#' @param data Data context
#' @param enclos The environment the specification was written in, from
#'   `distr()`; anything else falls back to the caller's frame, as before.
#' @return Evaluated value
#' @keywords internal
eval_distr_arg <- function(expr, data, enclos = NULL) {
  if (!is.environment(enclos)) enclos <- parent.frame(2)
  eval(expr, envir = data, enclos = enclos)
}

#' Convert Spearman correlations to Gaussian copula correlations
#'
#' Applies theoretical relationships between Spearman's rho and the
#' Gaussian copula parameter (Kurowicka & Cooke, 2006; Lebrun & Dutfoy, 2009):
#'   - Continuous-continuous: rho_copula = 2 * sin(pi * rho_S / 6) (exact)
#'   - Binary-binary: rho_copula = sin(pi * rho_S / 2) (heuristic; the exact
#'     relationship depends on marginal prevalences, not accounted for here)
#'   - Continuous-binary: rho_copula = sqrt(2) * sin(pi * rho_S / (2*sqrt(3)))
#'     (heuristic)
#'
#' @param X Correlation matrix (Spearman)
#' @param types Character vector of distribution types
#' @return Adjusted correlation matrix for Gaussian copula
#' @keywords internal
cor_adjust_spearman <- function(X, types) {
  if (length(types) != nrow(X)) {
    stop("`types` length must match correlation matrix dimensions", call. = FALSE)
  }
  bin <- types == "binary"
  cont <- !bin

  X[cont, cont] <- 2 * sin(pi * X[cont, cont] / 6)
  X[bin, bin] <- sin(pi * X[bin, bin] / 2)
  # The continuous-binary heuristic can map strong input correlations to a
  # magnitude > 1 (|rho_S| > sqrt(3)/2); clamp to keep a valid correlation
  # entry before the positive-definite projection.
  X[cont, bin] <- .clamp_cor(sqrt(2) * sin(pi * X[cont, bin] / (2 * sqrt(3))))
  X[bin, cont] <- .clamp_cor(sqrt(2) * sin(pi * X[bin, cont] / (2 * sqrt(3))))

  diag(X) <- 1
  X
}

#' Clamp correlation entries to a valid open interval
#' @keywords internal
.clamp_cor <- function(x) {
  pmin(pmax(x, -0.999), 0.999)
}

#' Convert Pearson correlations to Gaussian copula correlations
#'
#' For continuous-continuous pairs, Pearson rho equals the Gaussian copula
#' parameter only under normality of both margins. For non-normal continuous
#' covariates, this no-adjustment assumption introduces approximation error.
#' For binary and mixed pairs:
#'   - Binary-binary: rho_copula = sin(pi * rho_P / 2)
#'   - Continuous-binary: rho_copula = sqrt(pi/2) * rho_P
#'
#' @param X Correlation matrix (Pearson)
#' @param types Character vector of distribution types
#' @return Adjusted correlation matrix for Gaussian copula
#' @keywords internal
cor_adjust_pearson <- function(X, types) {
  if (length(types) != nrow(X)) {
    stop("`types` length must match correlation matrix dimensions", call. = FALSE)
  }
  bin <- types == "binary"
  cont <- !bin

  X[bin, bin] <- sin(pi * X[bin, bin] / 2)
  # Continuous-binary heuristic can exceed |1| for |rho_P| > 1/sqrt(pi/2);
  # clamp to a valid correlation entry before the positive-definite projection.
  X[cont, bin] <- .clamp_cor(sqrt(pi / 2) * X[cont, bin])
  X[bin, cont] <- .clamp_cor(sqrt(pi / 2) * X[bin, cont])

  diag(X) <- 1
  X
}


#' Give distribution arguments the names R would match them to
#'
#' `distr()` stores its arguments unevaluated and everything downstream reads
#' them by name, so positional and abbreviated arguments are matched to their
#' formals once here, the way R would match them at call time.
#'
#' @param args The captured `...`, possibly partly named or abbreviated.
#' @param qfun The resolved quantile function.
#' @param qfun_name Its name, for error messages.
#' @return `args` with every element named in full.
#' @keywords internal
.name_distr_args <- function(args, qfun, qfun_name = "qfun") {
  if (!length(args)) {
    return(args)
  }
  nms <- names(args)
  if (is.null(nms)) {
    nms <- rep("", length(args))
  }
  # `p` is supplied by the evaluator. R matches unnamed and abbreviated
  # arguments only against the formals before `...`.
  formal_names <- names(formals(qfun))
  dots <- match("...", formal_names)
  if (!is.na(dots)) {
    formal_names <- formal_names[seq_len(dots - 1L)]
  }
  formal_names <- setdiff(formal_names, "p")
  # R's order: exact names first, then unique partial matches against the
  # formals no exact name took.
  named_idx <- which(nzchar(nms))
  exact <- intersect(nms[named_idx], formal_names)
  remaining <- setdiff(formal_names, exact)
  claimed <- list()
  for (i in named_idx) {
    nm <- nms[i]
    if (nm %in% exact) {
      next
    }
    hits <- remaining[startsWith(remaining, nm)]
    # R refuses an ambiguous abbreviation before binding anything positional.
    if (length(hits) > 1L) {
      stop(sprintf(paste0("`distr()` received argument `%s`, which matches more ",
                          "than one parameter of `%s` (%s). Name it in full."),
                   nm, qfun_name, paste(hits, collapse = ", ")), call. = FALSE)
    }
    if (length(hits) == 1L) {
      # R also refuses two abbreviations of the same formal.
      if (hits %in% names(claimed)) {
        stop(sprintf(paste0("`distr()` received arguments `%s` and `%s`, which ",
                            "both abbreviate parameter `%s` of `%s`."),
                     claimed[[hits]], nm, hits, qfun_name), call. = FALSE)
      }
      claimed[[hits]] <- nm
      nms[i] <- hits
    }
  }
  remaining <- setdiff(remaining, names(claimed))
  unnamed <- which(!nzchar(nms))
  if (length(unnamed)) {
    available <- remaining
    n_match <- min(length(unnamed), length(available))
    if (n_match) {
      nms[unnamed[seq_len(n_match)]] <- available[seq_len(n_match)]
    }
    # Anything left over belongs in `...`, if the function has one.
    leftover <- length(unnamed) - n_match
    if (leftover > 0L && !("..." %in% names(formals(qfun)))) {
      fmt <- paste0("`distr()` received %d unnamed argument(s) for `%s`, which ",
                    "has no remaining parameter to match them to and no `...`. ",
                    "Name them explicitly or drop them.")
      stop(sprintf(fmt, leftover, qfun_name), call. = FALSE)
    }
  }
  names(args) <- nms
  args
}

#' Coerce a validated count to integer without truncating
#'
#' The count validators accept values within `sqrt(.Machine$double.eps)` of a
#' whole number, and `as.integer()` truncates, so round first.
#'
#' @param x A numeric vector that has passed a whole-number count check.
#' @return An integer vector.
#' @keywords internal
.as_count_integer <- function(x) {
  as.integer(round(x))
}
