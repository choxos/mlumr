# Tests for prior_summary.mlumr_fit()
#
# Pure-R tests — no Stan, no sampling. The method inspects a stored
# `priors` list on the fit object, so we construct minimal fit stubs
# that exercise the three branches the method cares about:
#
#   1. Broadcast homogeneous prior (all coefficients share mean/sd,
#      no autoscaling).
#   2. Per-coefficient prior table (heterogeneous scales OR autoscaled).
#   3. Default-prior tag footer (written when $default = TRUE).

make_fit_stub <- function(priors) {
  structure(list(priors = priors), class = "mlumr_fit")
}

resolved_beta <- function(mean, sd, dist = 0L, df = 3,
                          autoscale = rep(FALSE, length(mean)),
                          sd_x = rep(1, length(mean)),
                          covariate_names = paste0("x", seq_along(mean))) {
  list(
    mean = as.numeric(mean),
    sd   = as.numeric(sd),
    dist = as.integer(dist),
    df   = df,
    autoscale = as.logical(autoscale),
    sd_x = as.numeric(sd_x),
    covariate_names = covariate_names
  )
}

tag_default <- function(prior) {
  prior$default <- TRUE
  prior$version <- "0.1.0"
  prior
}


test_that("broadcast summary is used when priors are homogeneous", {
  priors <- list(
    intercept = prior_normal(0, 10),
    beta = prior_normal(0, 2.5),
    beta_resolved = resolved_beta(mean = rep(0, 3), sd = rep(2.5, 3))
  )
  fit <- make_fit_stub(priors)

  out <- capture.output(prior_summary(fit))

  # Broadcast line names the prior and the covariate count
  expect_true(any(grepl("normal\\(0, 2\\.5\\) applied to all 3 covariate", out)))
  # Per-coefficient table should NOT be printed in the broadcast branch
  expect_false(any(grepl("coefficient\\s+mean\\s+scale", out)))
})


test_that("per-coefficient table is used when resolved scales differ", {
  priors <- list(
    intercept = prior_normal(0, 10),
    beta = list(prior_normal(0, 2.5), prior_normal(0, 1)),
    beta_resolved = resolved_beta(mean = c(0, 0), sd = c(2.5, 1),
                                  covariate_names = c("age", "sex"))
  )
  fit <- make_fit_stub(priors)

  out <- capture.output(prior_summary(fit))

  # Header identifies the family
  expect_true(any(grepl("Family: normal", out)))
  # Table header and at least one coefficient row
  expect_true(any(grepl("coefficient", out)))
  expect_true(any(grepl("\\bage\\b", out)))
  expect_true(any(grepl("\\bsex\\b", out)))
})


test_that("autoscaled coefficients are flagged and the footnote is printed", {
  # Coefficient 2 autoscaled from 2.5 by sd_x = 2 -> scale 1.25.
  priors <- list(
    intercept = prior_normal(0, 10),
    beta = list(prior_normal(0, 2.5), prior_normal(0, 2.5, autoscale = TRUE)),
    beta_resolved = resolved_beta(
      mean = c(0, 0),
      sd   = c(2.5, 1.25),
      autoscale = c(FALSE, TRUE),
      sd_x = c(1, 2),
      covariate_names = c("age", "bmi")
    )
  )
  fit <- make_fit_stub(priors)

  out <- capture.output(prior_summary(fit))

  # The autoscaled row is flagged in the table
  expect_true(any(grepl("TRUE", out)))
  # The footnote explains the scale = user_scale / sd_x convention
  expect_true(any(grepl("scale = user_scale / sd_x for autoscaled rows", out)))
})


test_that("default priors are tagged with (package default, mlumr <version>)", {
  priors <- list(
    intercept = tag_default(prior_normal(0, 10)),
    beta = tag_default(prior_normal(0, 2.5)),
    beta_resolved = resolved_beta(mean = rep(0, 2), sd = rep(2.5, 2))
  )
  fit <- make_fit_stub(priors)

  out <- capture.output(prior_summary(fit))

  # Both the intercept and beta blocks carry the default tag
  default_lines <- grep("package default, mlumr", out, value = TRUE)
  expect_gte(length(default_lines), 2L)
  expect_true(all(grepl("0\\.1\\.0", default_lines)))
})


test_that("sigma prior block is printed only when sigma is present", {
  priors_no_sigma <- list(
    intercept = prior_normal(0, 10),
    beta = prior_normal(0, 2.5),
    beta_resolved = resolved_beta(mean = 0, sd = 2.5)
  )
  out_no <- capture.output(prior_summary(make_fit_stub(priors_no_sigma)))
  expect_false(any(grepl("Residual SD", out_no)))

  priors_sigma <- priors_no_sigma
  priors_sigma$sigma <- prior_exponential(rate = 1)
  out_yes <- capture.output(prior_summary(make_fit_stub(priors_sigma)))
  expect_true(any(grepl("Residual SD", out_yes)))
  # Exponential priors are rendered by rate, not (mean, sd)
  expect_true(any(grepl("exponential\\(rate = 1\\)", out_yes)))
})


test_that("prior_summary errors on fits missing the priors slot", {
  fit <- structure(list(), class = "mlumr_fit")
  expect_error(prior_summary(fit), "No prior information stored")
})


test_that("prior_summary validates digits", {
  priors <- list(
    intercept = prior_normal(0, 10),
    beta = prior_normal(0, 2.5),
    beta_resolved = resolved_beta(mean = 0, sd = 2.5)
  )
  fit <- make_fit_stub(priors)

  expect_error(prior_summary(fit, digits = 0), "`digits`")
  expect_error(prior_summary(fit, digits = c(2, 3)), "`digits`")
  expect_error(prior_summary(fit, digits = NA_real_), "`digits`")
})


test_that("prior_summary rejects malformed resolved beta metadata", {
  priors <- list(
    intercept = prior_normal(0, 10),
    beta = prior_normal(0, 2.5),
    beta_resolved = list(mean = 0, sd = 2.5)
  )
  fit <- make_fit_stub(priors)

  expect_error(prior_summary(fit), "Resolved beta prior metadata")
})


test_that("prior_summary prints beta prior lists for older fit metadata", {
  priors <- list(
    intercept = prior_normal(0, 10),
    beta = list(prior_normal(0, 2.5), prior_normal(0, 1))
  )
  fit <- make_fit_stub(priors)

  out <- capture.output(prior_summary(fit))

  expect_true(any(grepl("beta\\[1\\]: normal\\(0, 2\\.5\\)", out)))
  expect_true(any(grepl("beta\\[2\\]: normal\\(0, 1\\)", out)))
})


test_that("prior_summary default method rejects unsupported classes", {
  expect_error(prior_summary(list()), "no method for class")
})


test_that("Student-t priors render their df", {
  priors <- list(
    intercept = prior_student_t(df = 7, 0, 10),
    beta = prior_student_t(df = 5, 0, 2.5),
    beta_resolved = resolved_beta(
      mean = rep(0, 2), sd = rep(2.5, 2), dist = 1L, df = 5
    )
  )
  out <- capture.output(prior_summary(make_fit_stub(priors)))
  expect_true(any(grepl("student_t\\(df = 7", out)))
  expect_true(any(grepl("normal|student_t", out)))  # broadcast line
})
