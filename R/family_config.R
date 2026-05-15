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
#'     [marginal_effects()] (excluding `"all"`).}
#'   \item{`marginal_effect_vars`}{Generated-quantity column names for each
#'     effect measure, per population. Expanded in [marginal_effects()].}
#'   \item{`comp_weight_field`}{Name of the Stan-data field used to
#'     weight the comparator-population marginal predictions
#'     (`n_agd` for binomial, `E_agd` for poisson, `NULL` for normal
#'     = equal weights).}
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
    comp_weight_field    = NULL
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
