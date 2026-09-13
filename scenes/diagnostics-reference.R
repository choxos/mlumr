# Reference values for scenes/diagnostics.ts, computed by the posterior R package that the
# TypeScript port follows (Vehtari, Gelman, Simpson, Carpenter and Bürkner 2021, "Rank-normalization,
# folding, and localization: an improved R-hat for assessing convergence of MCMC").
# Run from the lesson root: Rscript scenes/diagnostics-reference.R
library(posterior)
library(jsonlite)
set.seed(2026)

# Matrices are iterations by chains, the layout posterior's default methods take.
ar1 <- function(n, phi) as.numeric(stats::filter(rnorm(n), phi, method = "recursive"))
with_draw <- function(x, i, value) {
  x[i] <- value
  x
}
cases <- list(
  normal = cbind(rnorm(500), rnorm(500)),
  different_means = cbind(rnorm(500), rnorm(500, mean = 1)),
  different_scales = cbind(rnorm(500), rnorm(500, sd = 3)),
  heavy_tails = matrix(rt(1000, df = 2), 500),
  rounded = matrix(round(rnorm(1000), 1), 500),
  discrete = matrix(as.numeric(rpois(1000, 1)), 500),
  one_constant_chain = cbind(rnorm(500), rep(0.1, 500)),
  all_constant = matrix(0.1, 500, 2),
  three_draws = matrix(rnorm(6), 3),
  three_draws_four_chains = matrix(rnorm(12), 3),
  eleven_draws = matrix(rnorm(22), 11),
  four_chains = matrix(rnorm(4000), 1000),
  ar1 = sapply(1:4, function(i) ar1(500, 0.9)),
  alternating_ar1 = sapply(1:4, function(i) ar1(500, -0.9)),
  infinite_draw = with_draw(matrix(rnorm(1000), 500), 700, Inf),
  missing_draw = with_draw(matrix(rnorm(1000), 500), 3, NA),
  # A tie at the 5% quantile of 4000 draws where type 7 interpolation of -6.04 with itself rounds
  # below -6.04, so only R's guard for equal neighbors keeps the tied draws in the indicator.
  tied_quantile = matrix(sample(c(rep(-6.04, 400), -6.04 + abs(rnorm(3600)) + 0.01)), 1000)
)

# JSON has no Inf or NA, so non-finite draws are written as the strings R prints.
encode <- function(v) if (all(is.finite(v))) v else lapply(v, function(d) if (is.finite(d)) d else format(d))
# The alternating chains trigger posterior's warning that ESS was capped; the capped value is wanted.
out <- suppressWarnings(unname(Map(function(name, x) {
  list(
    name = name,
    chains = lapply(seq_len(ncol(x)), function(j) encode(x[, j])),
    rhat = rhat(x), ess_bulk = ess_bulk(x), ess_tail = ess_tail(x), mcse_mean = mcse_mean(x)
  )
}, names(cases), cases)))

# digits = I(17) round-trips every double; digits = NA prints 15 significant digits and does not.
writeLines(toJSON(out, digits = I(17), auto_unbox = TRUE, na = "null"), "scenes/diagnostics-reference.json")
