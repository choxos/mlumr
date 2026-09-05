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
  # Strip any namespace qualifier so the recorded name is the bare function
  # name. get_distribution_type() matches this against "qpois", "qbern" and the
  # rest, and a qualified call such as distr(stats::qpois, ...) would otherwise
  # miss every lookup and fall through to the support-grid heuristic. A count
  # margin with a small mean evaluates to {0, 1} on that grid and was then
  # classified "binary", which silently selects the wrong copula correction and
  # suppresses the non-binary discrete-margin warning in add_integration().
  qfun_name <- sub("^.*:::?", "", qfun_name)

  # Capture arguments as unevaluated expressions
  args <- as.list(match.call(expand.dots = FALSE))[["..."]]
  # ... and NAME any that were supplied positionally. Evaluation below iterates
  # over `names(args)`, so a fully positional spec such as `distr(qnorm, 10, 2)`
  # had no names to iterate, passed nothing to the quantile function, and
  # silently generated a standard normal: it evaluated to 0 at the median
  # instead of 10, with no error and no warning.
  args <- .name_distr_args(args, qfun_resolved, qfun_name)

  if (!"p" %in% names(formals(qfun_resolved))) {
    stop("`qfun` should be an inverse CDF function with a formal argument `p`",
         call. = FALSE)
  }

  d <- list(
    qfun = qfun_resolved,
    args = args,
    # The environment the specification was WRITTEN in. Arguments are stored
    # unevaluated so they can see a row of aggregate data at evaluation time,
    # but they must also be able to see the variables that were in scope where
    # the user wrote them. Falling back to `parent.frame()` at evaluation time
    # landed inside this package instead, so a specification built in a
    # function, or returned by one, could not resolve its own local variables.
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
  # Build the call: d$qfun(p = p, arg1 = val1, arg2 = val2, ...)
  # Iterate by POSITION, not by name. Arguments that could not be matched to a
  # formal stay unnamed and are passed through in order, which is what R does
  # with them; walking `names()` dropped them entirely.
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
#' @return Named numeric vector: `c(mean, sd, <named quantiles>)`.
#' @keywords internal
.summarize_draw_vector <- function(x, probs) {
  c(mean = mean(x, na.rm = TRUE),
    sd   = stats::sd(x, na.rm = TRUE),
    stats::quantile(x, probs = probs, na.rm = TRUE))
}

#' Summarize a draws matrix column-wise into a tidy data frame
#'
#' Applies `.summarize_draw_vector()` across columns of `draws` and renames
#' the quantile columns to `qNN` form (e.g., `q2.5`, `q50`, `q97.5`).
#'
#' @param draws A numeric matrix or data frame of posterior draws (one column
#'   per quantity, one row per draw).
#' @param probs Quantile probabilities.
#' @return Data frame with columns `mean`, `sd`, and one `qNN` column per
#'   element of `probs`.
#' @keywords internal
.summarize_draw_matrix <- function(draws, probs) {
  summary_mat <- t(apply(draws, 2, .summarize_draw_vector, probs = probs))
  summary_df <- as.data.frame(summary_mat)
  colnames(summary_df) <- c("mean", "sd", paste0("q", probs * 100))
  summary_df
}


#' Validate an mlumr_data object
#' @keywords internal
.validate_mlumr_data_object <- function(data) {
  if (!inherits(data, "mlumr_data")) {
    stop("`data` must be created with combine_data().", call. = FALSE)
  }
  invisible(TRUE)
}


#' Validate a confidence level
#' @keywords internal
.validate_conf_level <- function(conf_level) {
  valid <- is.numeric(conf_level) &&
    length(conf_level) == 1L &&
    is.finite(conf_level) &&
    conf_level > 0 &&
    conf_level < 1
  if (!valid) {
    stop("`conf_level` must be a single finite number between 0 and 1.",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Convert a confidence level to a two-sided normal critical value
#' @keywords internal
.z_from_conf_level <- function(conf_level) {
  .validate_conf_level(conf_level)
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
# These three one-line wrappers around stats::qbinom/pbinom/dbinom with
# size = 1 are identical to the corresponding functions in the multinma
# package (Phillippo et al., GPL-3,
# <https://github.com/dmphillippo/multinma>, R/integration.R). They
# are reproduced here as part of mlumr's adaptation of the ML-NMR
# workflow so that mlumr users can pass `qbern` to `distr()` without
# having multinma attached. Both packages are GPL-3-licensed.
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

  # qlogitnorm belongs here for the same reason as the rest, and its absence
  # was not harmless: the fallback below classifies by evaluating the quantile
  # function on 99 points, and a concentrated logit-normal returns values that
  # are all 1 to machine precision, so it was labeled binary and given the
  # binary copula correlation adjustment.
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
      # Through the specification's own scope, like every other argument: a
      # `size` naming a local variable of the function that built the spec
      # evaluated fine in eval_distr() and failed here with "object not
      # found", because this path still fell back to the package's frame.
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
#' `distr()` stores its arguments unevaluated, and everything downstream reads
#' them BY NAME: evaluation walks `names(args)`, and the margin classification
#' reads `args$size`. Anything supplied positionally therefore had no name, was
#' never iterated, and never reached the quantile function: `distr(qnorm, 10, 2)`
#' produced a standard normal rather than a normal with mean 10 and SD 2, with
#' nothing reported. An abbreviated name was a quieter version of the same
#' fault: `distr(qbinom, si = 5, prob = .5)` evaluated with size 5, because R
#' completes `si` at call time, but the classification looked up `args$size`,
#' found nothing, and `all(NULL == 1)` is `TRUE`, so a five-trial binomial was
#' labeled binary and given the binary copula correction. This applies R's own
#' matching once, at construction, so every stored argument carries the full
#' name of the formal it binds.
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
  # `p` is supplied by the evaluator, so it is never one of these. R matches
  # unnamed and abbreviated arguments only against formals that come BEFORE
  # `...`; anything after it can be reached by its exact name alone, and an
  # unnamed value goes into the dots. Dropping `...` from this list rather than
  # truncating at it would make `distr(qfun, 999)` bind 999 to a later formal
  # for a function declared `function(p, ..., scale = 2)`, silently replacing a
  # default and generating entirely different integration points.
  formal_names <- names(formals(qfun))
  dots <- match("...", formal_names)
  if (!is.na(dots)) {
    formal_names <- formal_names[seq_len(dots - 1L)]
  }
  formal_names <- setdiff(formal_names, "p")
  # A supplied name claims the formal R's matcher would give it, in R's order:
  # every exact name first, then each remaining name against the formals no
  # exact name took, by unique partial match. Removing only exact names left
  # `distr(qnorm, m = 10, 2)` assigning the unnamed 2 to `mean` as well, and
  # the quantile function then received both `m` and `mean` and failed with
  # "matched by multiple actual arguments". Matching partials against the
  # full list was wrong too: with formals `mean` and `method`, `m = 10` beside
  # `mean = 20` must reach `method`, not compete for a `mean` already taken.
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
    # R refuses an abbreviation that fits more than one formal, and it does so
    # BEFORE binding anything positional. Letting the positional value take one
    # of the candidates would leave the abbreviation a unique match for the
    # other at evaluation time, and a call R rejects would evaluate here with
    # both arguments bound to parameters the caller never named.
    if (length(hits) > 1L) {
      stop(sprintf(paste0("`distr()` received argument `%s`, which matches more ",
                          "than one parameter of `%s` (%s). Name it in full."),
                   nm, qfun_name, paste(hits, collapse = ", ")), call. = FALSE)
    }
    if (length(hits) == 1L) {
      # R also refuses two abbreviations of the same formal, "matched by
      # multiple actual arguments". Taking the formal for the first and
      # letting the second fall through to `...` evaluated `m = 1, me = 2`
      # with mean 1 and nothing said, for a call R rejects.
      if (hits %in% names(claimed)) {
        stop(sprintf(paste0("`distr()` received arguments `%s` and `%s`, which ",
                            "both abbreviate parameter `%s` of `%s`."),
                     claimed[[hits]], nm, hits, qfun_name), call. = FALSE)
      }
      claimed[[hits]] <- nm
      # Store the full name, so that everything reading the arguments by name
      # sees what the quantile function will see.
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
    # Anything left over belongs in `...`, if the function has one. If it does
    # not, there is nowhere for the value to go and saying so now beats a
    # confusing error from the quantile function later.
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
