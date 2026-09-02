# M-spline stability evidence

- Scenario: M-spline and piecewise-exponential source compilation.
  Invocation: `Rscript` calling `rstan::stanc()` for both flexible Stan models.
  Observable: both returned `status = TRUE`; combined run printed `M-spline Stan source and numerical checks OK` and exited 0.
- Scenario: cancelling cumulative-hazard scales and zero baseline values.
  Invocation: expose `survival_mspline_functions.stan`; evaluate `H0 = exp(-720), eta = 720`, `H0 = 0, eta = 800`, an event with `h0 = 0`, and marginal hazard with `eta = c(700, 701)`.
  Observable: cumulative hazard 1, zero cumulative hazard 0, zero-hazard event `-Inf`, and marginal log hazard `log(2) + 700`; all assertions exited 0.
- Scenario: basis-support invariance under a `1e15` time scale.
  Invocation: source `R/mspline.R`, build degree 0 and degree 3 bases with all maxima below `1e-12`, then call `.assert_basis_support()`.
  Observable: both bases accepted; command printed `R support-scale checks OK` and exited 0.
- Scenario: package-level focused tests.
  Invocation: `devtools::test(filter = "mspline", reporter = "summary")`.
  Observable: all expectations passed and the process exited 0; one expected extrapolation warning was emitted by an existing stratification scenario.
- Scenario: generated source parity and whitespace.
  Invocation: `Rscript tools/check_stan_exports.R` and scoped `git diff --check`.
  Observable: parity reported every data field present; scoped whitespace check exited 0.

No commit or push was performed.
