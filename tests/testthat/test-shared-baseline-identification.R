# A shared M-spline baseline carries one weight simplex and one intercept per
# study. Pooled column support does not tie the two studies together, and if it
# does not, the weights and the intercepts trade off exactly.

.degree0_spec <- function() {
  skip_if_not_installed("splines2")
  .build_mspline_basis(list(internal = 1, boundary = c(0, 3)), 0L)
}

# Index seen on [0, 1]; comparator seen on [2, 3] entering at 2. Disjoint
# columns of the degree-0 basis, and every column live in the pooled risk set.
.disjoint_studies <- function() {
  list(
    index = list(observed_max = 1, entry = c(0, 0, 0),
                 exit = c(0.4, 0.7, 1), event = c(0.4, 0.7)),
    comparator = list(observed_max = 3, entry = c(2, 2, 2),
                      exit = c(2.4, 2.8, 3), event = c(2.4, 2.8))
  )
}

test_that("the disjoint layout really is an exact likelihood ridge", {
  # Not a property of the guard: a property of the model the guard is about.
  # M1 = 1 on [0, 1), M2 = 1/2 on [1, 3], so with the index only ever on the
  # first column and the comparator only on the second, the likelihood sees
  # exp(mu_index) * w and exp(mu_comparator) * (1 - w) and nothing else.
  loglik <- function(w, mu_index, mu_comparator) {
    h_index <- exp(mu_index) * w
    h_comparator <- exp(mu_comparator) * (1 - w) * 0.5
    sum(log(h_index) - h_index * c(0.4, 0.7)) - h_index * 1 +
      sum(log(h_comparator) - h_comparator * (c(2.4, 2.8) - 2)) -
      h_comparator * (3 - 2)
  }
  # Rescaling the weights and absorbing it into the intercepts.
  expect_equal(loglik(0.50, 0, 0),
               loglik(0.25, log(2), log(2 / 3)),
               tolerance = 1e-14)
  # while the conditional hazard ratio moves from 1 to 3.
  expect_equal(exp(0 - 0), 1)
  expect_equal(exp(log(2) - log(2 / 3)), 3, tolerance = 1e-12)
})

test_that("pooled support alone accepts that ridge, which is why it is not enough", {
  spec <- .degree0_spec()
  studies <- .disjoint_studies()
  # Every column is live for SOMEBODY, so the support check has no complaint.
  expect_silent(
    .assert_basis_support(
      spec, observed_max = 3,
      label = "shared",
      entry = c(studies$index$entry, studies$comparator$entry),
      exit = c(studies$index$exit, studies$comparator$exit),
      event = c(studies$index$event, studies$comparator$event)
    )
  )
  # Per study, though, they touch disjoint columns.
  expect_equal(
    .live_basis_columns(spec, 1, studies$index$entry, studies$index$exit,
                        studies$index$event),
    c(TRUE, FALSE)
  )
  expect_equal(
    .live_basis_columns(spec, 3, studies$comparator$entry,
                        studies$comparator$exit, studies$comparator$event),
    c(FALSE, TRUE)
  )
})

test_that("a shared baseline over disjoint columns is refused", {
  spec <- .degree0_spec()
  expect_error(
    .assert_shared_basis_identified(spec, .disjoint_studies()),
    "unidentified"
  )
  expect_error(
    .assert_shared_basis_identified(spec, .disjoint_studies()),
    "disjoint sets of spline columns"
  )
})

test_that("studies that share a column are not refused", {
  spec <- .degree0_spec()
  studies <- .disjoint_studies()
  # Same comparator, entering early enough to be observed on both columns.
  studies$comparator <- list(observed_max = 3, entry = c(0.5, 0.5, 0.5),
                             exit = c(1.5, 2.5, 3), event = c(1.5, 2.5))
  expect_true(.assert_shared_basis_identified(spec, studies))
})

test_that("connectivity is judged transitively, not pairwise", {
  spec <- .build_mspline_basis(list(internal = c(1, 2), boundary = c(0, 3)), 0L)
  expect_equal(spec$n_scoef, 3L)
  # A on column 1, B on columns 1 and 3, C on column 3: A and C share nothing,
  # but B ties all three together, so this is one component.
  studies <- list(
    a = list(observed_max = 1, entry = 0, exit = 0.9, event = 0.9),
    b = list(observed_max = 3, entry = 0, exit = 3, event = c(0.5, 2.5)),
    c = list(observed_max = 3, entry = 2, exit = 2.9, event = 2.9)
  )
  expect_true(.assert_shared_basis_identified(spec, studies))
})

test_that("mlumr() refuses the shared baseline before it fits anything", {
  # The guard is wired into the shared-baseline branch, so this has to go
  # through the public entry point: a unit test of the helper would still pass
  # if nothing called it. Nothing is compiled or sampled here, because the
  # refusal happens while the basis is being built.
  skip_if_not_installed("splines2")
  set.seed(2026)
  n <- 30
  ipd <- data.frame(trt = "A", time = runif(n, 0.2, 1), status = 1L,
                    age = rnorm(n))
  ipd_obj <- set_ipd(ipd, "trt", covariates = "age", family = "survival",
                     time = "time", status = "status")
  # Comparator observed only on [2, 3], entering at 2.
  agd <- data.frame(trt = "B", time = runif(n, 2.2, 3), status = 1L,
                    entry = 2, age_mean = 0.1, age_sd = 1)
  agd_obj <- set_agd_surv(agd, "trt", time = "time", status = "status",
                          entry_time = "entry",
                          cov_means = "age_mean", cov_sds = "age_sd",
                          cov_types = "continuous")
  combined <- combine_data(ipd_obj, agd_obj)
  dat <- suppressWarnings(
    add_integration(combined, n_int = 8, verbose = FALSE,
                    age = distr(qnorm, mean = age_mean, sd = age_sd))
  )

  expect_error(
    mlumr(dat, distribution = "pexp", aux_by = "none",
          knots = list(internal = 1, boundary = c(0, 3)),
          chains = 1, iter = 200, warmup = 100, seed = 2026, refresh = 0),
    "shared baseline is unidentified"
  )
})
