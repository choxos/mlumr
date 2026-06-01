# The R layer must ask the SAME question the Stan models ask before it attaches
# an evaluation time to a scalar effect, or relabels a contrast.
#
# `n_strata > 1` is not that question. `aux_by = ".study"` allocates one
# auxiliary column per study, but the exponential and exponential-AFT have no
# shape parameter to allocate: their baseline is carried entirely by the study
# intercept, which is study-specific in every unanchored fit. Stratifying them
# changes nothing, so Stan keeps the exact closed form and the R layer must not
# claim the number was evaluated on the prediction grid.
#
# mlumr_survival_{spfa,relaxed}.stan gates on `n_strata > 1 && nonexp &&
# dist <= 3` where `nonexp = (dist != 1 && dist != 4)`; the flexible models gate
# on `n_strata > 1` alone because their per-stratum simplex IS the baseline.

fake_fit <- function(distribution, n_strata, model = "spfa") {
  structure(
    list(surv_info = mlumr:::.survival_distribution_info(distribution),
         stan_data = list(n_strata = as.integer(n_strata)),
         family = "survival", model = model,
         pred_times = seq(1, 10, length.out = 10)),
    class = "mlumr_fit")
}

test_that("the gate matches the Stan gate for every distribution", {
  # (distribution, expected .aux_shapes_differ() under n_strata = 2)
  expect <- list(
    "exponential"     = FALSE,   # dist 1, nonexp = 0: no shape to stratify
    "exponential-aft" = FALSE,   # dist 4, nonexp = 0
    "weibull"         = TRUE,
    "gompertz"        = TRUE,
    "weibull-aft"     = TRUE,
    "lognormal"       = TRUE,
    "loglogistic"     = TRUE,
    "gamma"           = TRUE,
    "gengamma"        = TRUE,
    "mspline"         = TRUE,    # flexible: the simplex is the baseline
    "pexp"            = TRUE
  )
  for (d in names(expect)) {
    expect_equal(mlumr:::.aux_shapes_differ(fake_fit(d, 2L)), expect[[d]],
                 info = paste(d, "with n_strata = 2"))
    # One stratum is never "differing", whatever the distribution.
    expect_false(mlumr:::.aux_shapes_differ(fake_fit(d, 1L)), info = d)
  }
})

test_that("the two exponential forms are exactly the Stan nonexp exclusions", {
  # Guards against the gate drifting away from `nonexp` in the Stan source.
  codes <- vapply(c("exponential", "weibull", "gompertz", "exponential-aft",
                    "weibull-aft", "lognormal", "loglogistic", "gamma",
                    "gengamma"),
                  function(d) mlumr:::.survival_distribution_info(d)$dist_code,
                  integer(1))
  no_shape <- names(codes)[vapply(names(codes), function(d)
    mlumr:::.survival_distribution_info(d)$n_aux == 0L, logical(1))]
  expect_setequal(no_shape, c("exponential", "exponential-aft"))
  expect_setequal(unname(codes[no_shape]), c(1L, 4L))   # Stan's nonexp test
})

test_that("a stratified exponential is not relabeled as a stratified contrast", {
  # `.surv_contrast_name()` previously returned "exp_eta_contrast" for ANY
  # n_strata > 1, so a default exponential fit had its exact conditional hazard
  # ratio renamed and warned about.
  expect_equal(mlumr:::.surv_contrast_name(fake_fit("exponential", 2L)), "hr")
  expect_equal(mlumr:::.surv_contrast_name(fake_fit("exponential-aft", 2L)), "tr")
  # Distributions that really do have a per-study shape keep the honest name.
  expect_equal(mlumr:::.surv_contrast_name(fake_fit("weibull", 2L)),
               "exp_eta_contrast")
  expect_equal(mlumr:::.surv_contrast_name(fake_fit("lognormal", 2L)),
               "exp_eta_contrast")
  # A shared baseline is an exact HR / TR.
  expect_equal(mlumr:::.surv_contrast_name(fake_fit("weibull", 1L)), "hr")
  expect_equal(mlumr:::.surv_contrast_name(fake_fit("lognormal", 1L)), "tr")
})

# A hazard ratio and a time ratio are different estimands. `effect = "tr"` used
# to be routed to the `hr` computation and the answer labeled from the fitted
# distribution, so a PH fit answered a `tr` request with an HR and an AFT fit
# answered an `hr` request with a TR. The label disclosed the substitution but
# the request was never honored. An explicit request must now return the
# requested estimand or error.
test_that("conditional_effects() does not substitute HR for TR or TR for HR", {
  fake <- function(distribution, n_strata) {
    f <- fake_fit(distribution, n_strata)
    f$distribution <- distribution
    f
  }

  # Shared baseline (n_strata = 1): the requested measure exists for exactly
  # one of the two model classes, and asking for the other one errors.
  ph  <- fake("weibull", 1L)
  aft <- fake("lognormal", 1L)

  # PH + "tr" -> error, and the message must name the reason, not the label.
  expect_error(conditional_effects(ph, effect = "tr"),
               "only available for accelerated failure time")
  # AFT + "hr" -> error.
  expect_error(conditional_effects(aft, effect = "hr"),
               "not a scalar conditional effect")

  # The two requests that do name an existing estimand must get past the guard.
  # They fail later, on the fixture's absent draws, not on the estimand check.
  ph_hr <- tryCatch(conditional_effects(ph, effect = "hr"),
                    error = conditionMessage)
  expect_false(grepl("accelerated failure time|scalar conditional effect|not available",
                     ph_hr))
  aft_tr <- tryCatch(conditional_effects(aft, effect = "tr"),
                     error = conditionMessage)
  expect_false(grepl("accelerated failure time|scalar conditional effect|not available",
                     aft_tr))

  # A stratified baseline removes both: neither estimand exists as a scalar,
  # and the message says so under the name that was actually requested.
  expect_error(conditional_effects(fake("weibull", 2L), effect = "hr"),
               "`effect = \"hr\"` is not available")
  expect_error(conditional_effects(fake("lognormal", 2L), effect = "tr"),
               "`effect = \"tr\"` is not available")

  # The exponential is proportional-hazards in mlumr's parameterization, so its
  # contrast is a hazard ratio and its reciprocal, not itself, is the time
  # ratio. Returning it under `tr` would flip the sign of the reported effect.
  expect_error(conditional_effects(fake("exponential", 2L), effect = "tr"),
               "only available for accelerated failure time")
  expect_error(conditional_effects(fake("exponential-aft", 2L), effect = "hr"),
               "not a scalar conditional effect")
})
