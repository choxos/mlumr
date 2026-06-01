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
