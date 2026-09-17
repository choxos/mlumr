#' Family metadata registry
#'
#' Family-specific Stan model names, AgD weighting, prediction-variable
#' prefixes, and supported links and effect measures, looked up by every
#' family branch in the R code.
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
#'     [marginal_effects()] (excluding `"all"`). For `"survival"` the scalar
#'     contrast is chosen per fit by `.surv_scalar_effect_name()`; `"hr"`
#'     stands for it here.}
#'   \item{`marginal_effect_vars`}{Generated-quantity column names for each
#'     effect measure, per population. Expanded in [marginal_effects()].}
#'   \item{`comp_weight_field`}{The Stan-data field the comparator-population
#'     marginal predictions are weighted by, which must match the field the
#'     family's `generated quantities` block uses: `n_agd` (binomial), `E_agd`
#'     (poisson), `agd_weight` (normal) and `NULL` for survival.}
#' }
#'
#' @noRd
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
    # `delta_*` is a log HR or log time ratio, exponentiated by
    # marginal_effects(); `rmstr` is formed there from the rmst_* draws.
    effect_measures      = c("hr", "rmstd", "rmstr"),
    # `rmstr` has no draw column of its own, so it has no entry here.
    marginal_effect_vars = list(
      hr    = c("delta_index", "delta_comparator"),
      rmstd = c("rmst_diff_index", "rmst_diff_comparator")
    ),
    comp_weight_field    = NULL
  )
)

#' Lookup helper for the family registry
#' @noRd
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
