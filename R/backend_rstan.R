#' Count transitions that stopped at the sampler's treedepth limit
#'
#' @param sp Per-chain sampler parameter matrices from
#'   [rstan::get_sampler_params()].
#' @param limit The `max_treedepth` the sampler actually ran under.
#' @return A single count across all chains.
#' @keywords internal
.count_treedepth_hits <- function(sp, limit) {
  sum(vapply(sp, function(x) sum(x[, "treedepth__"] >= limit), numeric(1)))
}

#' Merge a caller's rstan `control` with the settings mlumr names itself
#'
#' Tested by name, since `control = NULL` is present in `dots` and would
#' still reach `rstan::sampling()` as a second `control`.
#' @return A list with the merged `control` and `dots` with `control` removed.
#' @keywords internal
.merge_sampler_control <- function(adapt_delta, max_treedepth, dots) {
  control <- list(adapt_delta = adapt_delta, max_treedepth = max_treedepth)
  if ("control" %in% names(dots)) {
    supplied <- dots[["control"]]
    if (!is.null(supplied)) {
      if (!is.list(supplied)) {
        stop("`control` must be a list of sampler settings.", call. = FALSE)
      }
      # The caller's entries win.
      control <- utils::modifyList(control, supplied)
      # A NULL from the caller leaves mlumr's own value in place.
      for (nm in c("adapt_delta", "max_treedepth")) {
        if (is.null(control[[nm]])) {
          control[[nm]] <- if (identical(nm, "adapt_delta")) adapt_delta else
            max_treedepth
        }
      }
      # Same validators as the arguments.
      .validate_mlumr_adapt_delta(control$adapt_delta)
      .validate_mlumr_integer(control$max_treedepth, "max_treedepth",
                              lower = 1L)
    }
    dots[["control"]] <- NULL
  }
  list(control = control, dots = dots)
}

#' Fit a Stan model using rstan
#' @keywords internal
fit_rstan <- function(model_name, stan_data, chains, iter, warmup,
                      seed, adapt_delta, max_treedepth, refresh, ...) {

  dots <- list(...)
  # A caller's `control` is merged with mlumr's rather than passed beside it.
  merged <- .merge_sampler_control(adapt_delta, max_treedepth, dots)
  control <- merged$control
  dots <- merged$dots

  fit <- do.call(rstan::sampling, c(
    list(
      stanmodels[[model_name]],
      data = stan_data,
      chains = chains,
      iter = iter,
      warmup = warmup,
      seed = seed,
      control = control,
      refresh = refresh
    ),
    dots
  ))

  draws <- as.data.frame(fit)

  # Draws are chain-major; label them from the fitted chain count.
  n_chains_fit <- tryCatch(as.integer(fit@sim$chains),
                           error = function(e) NA_integer_)
  chain_ids <- if (!is.na(n_chains_fit) && n_chains_fit >= 1L &&
                     nrow(draws) %% n_chains_fit == 0L) {
    rep(seq_len(n_chains_fit), each = nrow(draws) %/% n_chains_fit)
  } else {
    NULL
  }

  summary_stats <- rstan::summary(fit)$summary
  summary_df <- as.data.frame(summary_stats)
  summary_df$variable <- rownames(summary_stats)
  summary_df <- summary_df[, c("variable", setdiff(names(summary_df), "variable"))]
  # rstan reports bulk n_eff only; add tail ESS, matched on variable name.
  summary_df$ess_tail <- unname(
    .rstan_ess_tail(draws, chain_ids)[summary_df$variable]
  )

  sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  n_divergent <- sum(vapply(sp, function(x) sum(x[, "divergent__"]), numeric(1)))
  # Count against the limit the sampler ran under, which is the merged one.
  n_max_td <- .count_treedepth_hits(sp, control$max_treedepth)

  list(
    native_fit = fit,
    draws = draws,
    chain_ids = chain_ids,
    summary_df = summary_df,
    n_divergent = n_divergent,
    n_max_td = n_max_td,
    # What the sampler ran under, quoted by the diagnostics.
    adapt_delta_used = control$adapt_delta,
    max_treedepth_used = control$max_treedepth,
    # The whole merged list, so a refit can replay it.
    control_used = control,
    n_chains_requested = as.integer(chains),
    n_chains_returned = .n_chains_returned(chain_ids, chains)
  )
}


#' Chain-aware tail ESS for rstan draws
#'
#' rstan does not report tail ESS, so compute it from the post-warmup draws with
#' `posterior`. Returns a named numeric vector over the columns of `draws`, all
#' `NA_real_` when `posterior` is unavailable or the draws cannot be laid out as
#' equal-length chains.
#' @keywords internal
.rstan_ess_tail <- function(draws, chain_ids) {
  var_names <- colnames(draws)
  out <- stats::setNames(rep(NA_real_, length(var_names)), var_names)
  if (length(var_names) == 0L || nrow(draws) == 0L ||
        !requireNamespace("posterior", quietly = TRUE)) {
    return(out)
  }
  # Unlabeled draws are not one long chain.
  if (is.null(chain_ids) || length(chain_ids) != nrow(draws)) {
    return(out)
  }
  chains <- chain_ids
  ids <- unique(chains)
  per_chain <- tabulate(match(chains, ids))
  # Unequal chain lengths cannot be reshaped into an iterations-by-chains
  # matrix; report the diagnostic as unavailable rather than guessing.
  if (length(unique(per_chain)) != 1L) {
    return(out)
  }
  n_iter <- per_chain[1L]
  mat <- as.matrix(draws)
  for (k in seq_along(var_names)) {
    col <- matrix(NA_real_, nrow = n_iter, ncol = length(ids))
    for (j in seq_along(ids)) {
      col[, j] <- mat[chains == ids[j], k]
    }
    out[k] <- suppressWarnings(posterior::ess_tail(col))
  }
  out
}
