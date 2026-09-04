make_predict_fit <- function() {
  draws <- data.frame(
    mu_index = c(0, 1),
    mu_comparator = c(-1, 0),
    check.names = FALSE
  )
  draws[["beta[1]"]] <- c(0.5, 0.5)

  draws[["p_index_index"]] <- c(0.60, 0.70)
  draws[["p_comparator_index"]] <- c(0.40, 0.50)
  draws[["p_index_comparator"]] <- c(0.65, 0.75)
  draws[["p_comparator_comparator"]] <- c(0.45, 0.55)

  for (nm in c("index_index", "comparator_index",
               "index_comparator", "comparator_comparator")) {
    p <- draws[[paste0("p_", nm)]]
    draws[[paste0("log_p_", nm)]] <- log(p)
    draws[[paste0("log_q_", nm)]] <- log1p(-p)
  }

  draws[["lor_index"]] <- qlogis(draws[["p_index_index"]]) -
    qlogis(draws[["p_comparator_index"]])
  draws[["lor_comparator"]] <- qlogis(draws[["p_index_comparator"]]) -
    qlogis(draws[["p_comparator_comparator"]])
  draws[["rd_index"]] <- draws[["p_index_index"]] -
    draws[["p_comparator_index"]]
  draws[["rd_comparator"]] <- draws[["p_index_comparator"]] -
    draws[["p_comparator_comparator"]]
  draws[["rr_index"]] <- draws[["p_index_index"]] /
    draws[["p_comparator_index"]]
  draws[["rr_comparator"]] <- draws[["p_index_comparator"]] /
    draws[["p_comparator_comparator"]]

  structure(
    list(
      draws = draws,
      family = "binomial",
      link = "logit",
      model = "spfa",
      data = list(
        covariates = "x",
        ipd = list(data = data.frame(x = c(0, 2))),
        integration_points = array(c(1, 3), dim = c(1, 2, 1)),
        index_treatment = "A",
        comparator_treatment = "B"
      ),
      stan_data = list(n_agd = 10L)
    ),
    class = c("mlumr_fit", "list")
  )
}


test_that("predict summarizes response-scale generated quantities", {
  fit <- make_predict_fit()

  pred <- predict(fit, population = "both", probs = 0.5)

  expect_s3_class(pred, "data.frame")
  expect_equal(nrow(pred), 4)
  expect_equal(pred$treatment, c("A", "B", "A", "B"))
  expect_equal(pred$population, c("Index", "Index", "Comparator", "Comparator"))
  expect_equal(pred$mean, c(0.65, 0.45, 0.70, 0.50))
  expect_equal(pred$q50, c(0.65, 0.45, 0.70, 0.50))
})


test_that("predict links population-standardized response means", {
  fit <- make_predict_fit()

  link_draws <- predict(fit, type = "link", summary = FALSE)

  expect_equal(link_draws[["p_index_index"]], qlogis(c(0.60, 0.70)))
  expect_equal(link_draws[["p_comparator_index"]], qlogis(c(0.40, 0.50)))
  expect_equal(link_draws[["p_index_comparator"]], qlogis(c(0.65, 0.75)))
  expect_equal(link_draws[["p_comparator_comparator"]], qlogis(c(0.45, 0.55)))
})


test_that("target link predictions transform the marginal response", {
  fit <- make_predict_fit()
  target <- data.frame(x = c(0, 2))

  response <- predict(fit, newdata = target, summary = FALSE)
  linked <- predict(fit, newdata = target, type = "link", summary = FALSE)

  # The target route names its columns after the arm and the target population,
  # matching the built-in route's p_<arm>_<population>, rather than after the
  # treatment labels.
  expect_named(response, c("p_index_target", "p_comparator_target"))
  expect_named(linked, c("p_index_target", "p_comparator_target"))
  for (nm in c("p_index_target", "p_comparator_target")) {
    expect_equal(linked[[nm]], qlogis(response[[nm]]), tolerance = 1e-12)
  }
  # Linking the marginal response is not the same as averaging the linear
  # predictors, which is what c(0.5, 1.5) would be here.
  expect_false(isTRUE(all.equal(linked$p_index_target, c(0.5, 1.5))))
})


test_that("older finite binary predictions retain a link-scale fallback", {
  fit <- make_predict_fit()
  fit$draws[grep("^log_[pq]_", names(fit$draws))] <- NULL
  linked <- predict(fit, type = "link", summary = FALSE)
  expect_equal(linked$p_index_index, qlogis(c(0.60, 0.70)))

  fit$draws$p_index_index[1] <- 0
  expect_error(predict(fit, type = "link"), "refit the model")
})


test_that("predict validates arguments and draw columns", {
  fit <- make_predict_fit()

  expect_error(predict.mlumr_fit(list()), "`object` must be an mlumr_fit object")
  expect_error(predict(fit, population = "i"), "`population` must be one of")
  expect_error(predict(fit, type = "r"), "`type` must be one of")
  expect_error(predict(fit, summary = NA), "`summary` must be TRUE or FALSE")
  expect_error(predict(fit, probs = c(0.5, 0.5)), "`probs` must be unique")

  broken <- fit
  broken$draws["p_index_index"] <- NULL
  expect_error(predict(broken), "Missing prediction draw column")
})


test_that("marginal_effects validates inputs and returns labeled effects", {
  fit <- make_predict_fit()

  me <- marginal_effects(fit, effect = "lor", summary = FALSE)
  expect_equal(names(me), c("lor_index", "lor_comparator"))

  me_summary <- marginal_effects(fit, effect = "rd", probs = 0.5)
  expect_equal(me_summary$effect, c("RD", "RD"))
  expect_equal(me_summary$population, c("Index", "Comparator"))

  expect_error(marginal_effects(fit, effect = c("lor", "rd")),
               "`effect` must be a single non-missing string")
  expect_error(marginal_effects(fit, effect = "bad"),
               "For binomial family")
})
