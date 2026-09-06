#' Family metadata registry
#'
#' Internal single-source-of-truth for family-specific Stan model names,
#' AgD weighting, prediction-variable prefixes, and supported links and
#' effect measures. Every R-side call site that hard-coded a per-family
#' branch now looks up the relevant field here. This file is deliberately
#' pure-R and makes no Stan calls; keeping it a data registry makes
#' rstantools regeneration of `R/stanmodels.R` independent.
#'
#' Fields:
#' \describe{
#'   \item{`stan_prefix`}{Prefix for the Stan model name (the full name
#'     is `<stan_prefix>_{spfa,relaxed}`).}
#'   \item{`predict_prefix`}{Column prefix for generated-quantity variables
#'     in [predict.mlumr_fit()] (e.g. `"p"`, `"y"`, `"rate"`).}
#'   \item{`link_default`}{The default link when the user passes
#'     `link = NULL`.}
#'   \item{`links`}{Vector of supported links. Should match the branches
#'     in [check_link()].}
#'   \item{`effect_measures`}{Supported values of the `effect` argument in
#'     [marginal_effects()] (excluding `"all"`). Family-level, and for
#'     `"survival"` NOT the whole accepted set: the scalar contrast is
#'     distribution-specific, so a fit accepts exactly one of `"hr"`, `"tr"` or
#'     `"exp_delta_eta"` and the choice is made per fit by
#'     `.surv_scalar_effect_name()`, which cannot be expressed here because this
#'     registry is keyed by family alone. The entry lists `"hr"` as the
#'     representative scalar; treat the survival row as the RMST measures plus
#'     one fit-specific scalar.}
#'   \item{`marginal_effect_vars`}{Generated-quantity column names for each
#'     effect measure, per population. Expanded in [marginal_effects()].}
#'   \item{`comp_weight_field`}{Name of the Stan-data field used to
#'     weight the comparator-population marginal predictions. Must name
#'     the same field the family's Stan `generated quantities` block
#'     weights by, otherwise the R-side link-scale path in
#'     [predict.mlumr_fit()] would average over a different target
#'     population than the Stan-side response-scale predictions and
#'     `marginal_effects()`. Currently `n_agd` (binomial), `E_agd`
#'     (poisson), `agd_weight` (normal; required sample sizes for multiple rows,
#'     or one for a single row without `outcome_n`), and `NULL` for survival, whose
#'     comparator population is the pooled pseudo-IPD rather than a
#'     weighted mixture of aggregate rows.}
#' }
#'
#' @keywords internal
#' @name family_config
NULL

#' @keywords internal
family_config <- list(
  binomial = list(
    stan_prefix          = "mlumr_binary",
    predict_prefix       = "p",
    link_default         = "logit",
    links                = c("logit", "probit", "cloglog"),
    effect_measures      = c("lor", "rd", "rr"),
    marginal_effect_vars = list(
      lor = c("lor_index", "lor_comparator"),
      rd  = c("rd_index",  "rd_comparator"),
      rr  = c("rr_index",  "rr_comparator")
    ),
    comp_weight_field    = "n_agd"
  ),
  normal = list(
    stan_prefix          = "mlumr_normal",
    predict_prefix       = "y",
    link_default         = "identity",
    links                = c("identity", "log"),
    effect_measures      = c("md"),
    marginal_effect_vars = list(
      md = c("delta_index", "delta_comparator")
    ),
    # The normal Stan models weight the comparator-population marginal by
    # `agd_weight` (required outcome_n for multiple rows, or one for a single
    # row), so every marginal path must use the same target weights.
    comp_weight_field    = "agd_weight"
  ),
  poisson = list(
    stan_prefix          = "mlumr_poisson",
    predict_prefix       = "rate",
    link_default         = "log",
    links                = c("log"),
    effect_measures      = c("rr"),
    marginal_effect_vars = list(
      rr = c("delta_index", "delta_comparator")
    ),
    comp_weight_field    = "E_agd"
  ),
  survival = list(
    # NB stan_prefix is overridden to "mlumr_survival_mspline" in mlumr() for
    # the flexible-baseline distributions ("mspline", "pexp"); see
    # .survival_distribution_info().
    stan_prefix          = "mlumr_survival",
    predict_prefix       = "surv",
    link_default         = "log",
    links                = c("log"),
    # The raw Stan `delta_*` are log HR (PH) or log time ratios (AFT), but
    # marginal_effects() exponentiates them to natural-scale `hr` / `tr`
    # (null 1); the PH-vs-AFT label is resolved per-distribution in the
    # predict/summary layer. `rmstr` is the natural-scale RMST ratio
    # (RMST_index / RMST_comparator, null 1), derived from rmst_* draws in
    # .marginal_effects_survival(); the time-varying marginal log HR (null 0) is
    # exposed via predict(type = "loghr").
    effect_measures      = c("hr", "rmstd", "rmstr"),
    # Deliberately shorter than `effect_measures`, and this is the one family
    # where the two are not parallel. `rmstr` is a ratio of two draw columns
    # rather than a column of its own, so it has no entry here and is formed in
    # the survival branch of marginal_effects(), which dispatches before this
    # mapping is read. Adding an entry would name columns that do not exist.
    marginal_effect_vars = list(
      hr    = c("delta_index", "delta_comparator"),
      rmstd = c("rmst_diff_index", "rmst_diff_comparator")
    ),
    comp_weight_field    = NULL
  )
)

#' Lookup helper for the family registry
#'
#' Fails fast with an informative message when a caller requests an
#' unregistered family, rather than returning `NULL` and letting a
#' downstream `$` chain produce a cryptic error.
#'
#' @param family A single family string.
#' @return The corresponding entry from [family_config].
#' @keywords internal
get_family_config <- function(family) {
  if (!is.character(family) || length(family) != 1L) {
    stop("`family` must be a single string", call. = FALSE)
  }
  cfg <- family_config[[family]]
  if (is.null(cfg)) {
    stop(sprintf("Unknown family '%s'. Supported: %s.",
                 family, paste(names(family_config), collapse = ", ")),
         call. = FALSE)
  }
  cfg
}
