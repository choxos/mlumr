#' Fit a Stan model using rstan
#' @keywords internal
fit_rstan <- function(model_name, stan_data, chains, iter, warmup,
                      seed, adapt_delta, max_treedepth, refresh, ...) {

  fit <- rstan::sampling(
    stanmodels[[model_name]],
    data = stan_data,
    chains = chains,
    iter = iter,
    warmup = warmup,
    seed = seed,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    refresh = refresh,
    ...
  )

  draws <- as.data.frame(fit)

  # rstan's as.data.frame(fit) returns post-warmup draws in chain-major order;
  # derive the real chain ids from the fitted object's saved chain count (not the
  # requested `chains`) so diagnostics use true chain labels.
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
  # rstan's classic summary reports bulk n_eff only. check_diagnostics() also
  # tests tail ESS, which is what speaks for the 2.5%/97.5% quantiles the
  # package reports; without this column that check quietly did nothing on the
  # default backend. Match on variable name so the two orderings cannot drift.
  summary_df$ess_tail <- unname(
    .rstan_ess_tail(draws, chain_ids)[summary_df$variable]
  )

  sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  n_divergent <- sum(vapply(sp, function(x) sum(x[, "divergent__"]), numeric(1)))
  n_max_td <- sum(vapply(sp, function(x) sum(x[, "treedepth__"] >= max_treedepth), numeric(1)))

  list(
    native_fit = fit,
    draws = draws,
    chain_ids = chain_ids,
    summary_df = summary_df,
    n_divergent = n_divergent,
    n_max_td = n_max_td,
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
  chains <- if (is.null(chain_ids)) rep(1L, nrow(draws)) else chain_ids
  if (length(chains) != nrow(draws)) {
    return(out)
  }
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
