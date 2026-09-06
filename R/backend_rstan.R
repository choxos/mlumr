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
#' Tested by name rather than by value. A caller who writes `control = NULL`
#' leaves an element that is present and NULL, so `is.null(dots$control)` is
#' true while the name is still in `dots`, and forwarding it would hand
#' `rstan::sampling()` two `control` arguments: the collision this merge
#' exists to prevent. `$` also matches partially, so an exact test on the
#' names is the one that means what it says.
#'
#' @param adapt_delta,max_treedepth The settings mlumr exposes as arguments.
#' @param dots The caller's `...`, as a list.
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
      # The caller's entries win: naming one in `control` is a more specific
      # request than the argument mlumr() offers for the same setting.
      control <- utils::modifyList(control, supplied)
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
  # A caller's `control` has to be merged rather than forwarded beside this
  # one. Passing both made `rstan::sampling()`, which has `control` as a
  # formal, stop with "formal argument \"control\" matched by multiple actual
  # arguments" before sampling began, so the documented way to reach the
  # sampler's other settings could not be used at all. The two named here are
  # the ones mlumr() exposes as its own arguments, so a caller who names them
  # in `control` is asking for something mlumr() already asked for; theirs is
  # kept, since it is the more specific request.
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
  # The limit the sampler actually ran under, which is the merged one. A
  # caller who raised `max_treedepth` through `control` got the higher limit
  # from `rstan::sampling()` while this count still used the argument, so
  # transitions that stopped below the argument were reported as hitting a
  # maximum they never reached, and an override the other way hid real ones.
  n_max_td <- .count_treedepth_hits(sp, control$max_treedepth)

  list(
    native_fit = fit,
    draws = draws,
    chain_ids = chain_ids,
    summary_df = summary_df,
    n_divergent = n_divergent,
    n_max_td = n_max_td,
    # What the sampler ran under, which is not the argument when a caller
    # overrode either through `control`. The diagnostics quote these when
    # advising a change, so that the advice names the limit in force.
    adapt_delta_used = control$adapt_delta,
    max_treedepth_used = control$max_treedepth,
    # The whole merged list, not only the two named above. A caller can set
    # anything rstan accepts here, `adapt_engaged` and `stepsize` among them,
    # and a refit that replays only two of them is not a refit of the same
    # sampler configuration.
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
  # Treating unlabeled draws as one long chain would report a tail ESS computed
  # from a layout that is not the fit's, which is worse than reporting nothing.
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
